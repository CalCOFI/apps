# prep_db.R - build optimized local database + bathymetry for ctd-viz app
#
# usage:
#   Rscript prep_db.R                # uses latest release
#   Rscript prep_db.R v2026.05.14    # uses a specific version
#   Rscript prep_db.R latest TRUE    # force re-download
#
# idempotent: skips a build when its target file already exists unless forced;
# the database and the bathymetry raster are tracked independently.
# downloads + materializes ctd_thin (~260 MB), ctd_summary (~5 GB), ctd_cast,
# measurement_type, ship. the full ctd_measurement table is a *supplemental*
# release output (not in the catalog) and is intentionally NOT materialized —
# the app runs on the adaptively-thinned ctd_thin.
#
# also crops GEBCO 2025 sub-ice bathymetry to the ctd_cast extent (+ margin) ->
# data/gebco_calcofi.tif (positive-down depth, m; land clamped to 0). this is
# an app-side stopgap — bathymetry should become a first-class released
# layer: see CalCOFI/workflows#54.

# run from app dir: `cd apps/ctd-viz && Rscript prep_db.R`
devtools::load_all("../../calcofi4r")
librarian::shelf(
  DBI, duckdb, fs, glue, terra,
  quiet = TRUE)

# parquets are 200+ MB; R's default download.file timeout of 60 s aborts
# mid-stream on typical home connections. give it plenty of room.
options(timeout = max(1800, getOption("timeout", 60)))

# locate this script's directory
cli_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cli_args[grepl("^--file=", cli_args)])
app_dir  <- if (length(file_arg) > 0) {
  dirname(normalizePath(file_arg, mustWork = FALSE))
} else getwd()

# parse user args ----
args       <- commandArgs(trailingOnly = TRUE)
db_version <- if (length(args) >= 1) args[1] else "latest"
force_pull <- if (length(args) >= 2) as.logical(args[2]) else FALSE

# crop GEBCO 2025 sub-ice bathymetry to the ctd_cast extent (+ margin) in
# `db`, negate elevation -> positive-down depth (m), clamp land to 0, write
# `out_tif`. needs the db on hand so the AOI always covers every cruise — a
# previous fixed AOI silently clipped cruises that ran past Pt. Conception.
crop_bathy <- function(db, gebco_src, out_tif) {
  if (!file.exists(gebco_src)) {
    cat("WARNING: GEBCO 2025 source not found — keeping existing bathymetry",
        "\n  ", gebco_src, "\n", sep = "")
    return(invisible())
  }
  terra::terraOptions(threads = 1)
  
  con_b <- dbConnect(duckdb::duckdb(dbdir = db, read_only = TRUE))
  # Limit DuckDB threads so it doesn't starve the Shiny server
  dbExecute(con_b, "SET threads TO 1")
  e <- dbGetQuery(con_b, "
    SELECT MIN(lon_dec) AS lon_min, MAX(lon_dec) AS lon_max,
           MIN(lat_dec) AS lat_min, MAX(lat_dec) AS lat_max
    FROM ctd_cast WHERE lon_dec IS NOT NULL AND lat_dec IS NOT NULL")
  dbDisconnect(con_b, shutdown = TRUE)
  m   <- 0.5   # degree margin around the cast extent
  aoi <- ext(e$lon_min - m, e$lon_max + m, e$lat_min - m, e$lat_max + m)
  cat("cropping GEBCO 2025 sub-ice bathymetry to cast extent [",
      paste(round(as.vector(aoi), 2), collapse = ", "), "] ...\n", sep = "")
  depth <- clamp(-crop(rast(gebco_src), aoi), lower = 0, values = TRUE)
  names(depth) <- "depth_m"
  dir_create(path_dir(out_tif))
  writeRaster(
    depth, out_tif, overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))
  cat("  wrote", out_tif,
      glue("({round(file.info(out_tif)$size / 1024^2, 1)} MB, ",
           "{ncol(depth)} x {nrow(depth)} cells, ",
           "{round(minmax(depth)[2, 1])} m max depth)"), "\n")
}

# target location ----
db_file <- if (dir.exists("/share/data")) {
  "/share/data/ctd-viz/ctd-viz.duckdb"
} else {
  path.expand("~/_big/calcofi.org/ctd-viz/ctd-viz.duckdb")
}
dir_create(path_dir(db_file))

# bathymetry source + output (cropped after the db is on hand — see
# crop_bathy, which sizes the AOI to the ctd_cast extent). TODO: move
# bathymetry into the released database — CalCOFI/workflows#54.
gebco_src <- path.expand(paste0(
  "~/_big/gebco_2025_sub_ice_topo_geotiff/",
  "gebco_2025_sub_ice_n90.0_s0.0_w-180.0_e-90.0.tif"))
bathy_tif <- file.path(app_dir, "data", "gebco_calcofi.tif")

# idempotency — the db and the bathymetry raster are tracked independently
db_needed    <- !file.exists(db_file)   || force_pull
bathy_needed <- !file.exists(bathy_tif) || force_pull

if (!db_needed && !bathy_needed) {
  cat("database + bathymetry already exist, skipping rebuild:\n  ",
      db_file, "\n  ", bathy_tif, "\n",
      "pass 'latest TRUE' as args to force rebuild.\n", sep = "")
  quit(status = 0)
}

# bathy-only path: the db is current, just (re)crop bathymetry from it
if (!db_needed && bathy_needed) {
  cat("database already exists, skipping rebuild:\n  ", db_file, "\n", sep = "")
  crop_bathy(db_file, gebco_src, bathy_tif)
  quit(status = 0)
}

# --- from here db_needed is TRUE: full database build, then bathymetry ---

# tables to include — ctd_thin is the headline CTD table (adaptively thinned;
# single cast direction, canonical measurement types, ~10 m grid + inflections).
# ctd_summary backs the cruise-stats panel; ctd_cast supplies cast metadata.
keep_tables <- c(
  "ctd_cast",          # cast metadata: (cruise, cast_key, cast_dir, datetime)
  "ctd_thin",          # thinned profiles, partitioned by cruise_key — app default
  "ctd_summary",       # station/depth aggregates, partitioned
  "measurement_type",  # reference (incl. is_canonical)
  "ship")              # reference

# stage parquets + duckdb in a temp cache alongside the final target,
# then rename into place once done.
stage_dir <- file.path(path_dir(db_file), ".stage")
dir_create(stage_dir)

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
version_rs <- info$version
cat("resolved version:", version_rs, "\n")

# only request tables actually present in this release (defensive against
# older releases that predate ctd_thin)
avail   <- intersect(keep_tables, info$tables$name)
missing <- setdiff(keep_tables, avail)
if (length(missing) > 0)
  cat("WARNING: not in release ", version_rs, ": ",
      paste(missing, collapse = ", "), "\n", sep = "")
stopifnot(
  "release lacks ctd_thin — rebuild with v2026.05.14 or later" =
    "ctd_thin" %in% avail)

# cc_get_db names the output `calcofi_{version}.duckdb` in cache_dir
staged_db <- file.path(stage_dir, glue("calcofi_{version_rs}.duckdb"))
if (file.exists(staged_db)) {
  cat("removing stale staged db:", staged_db, "\n")
  file.remove(staged_db)
}

con <- cc_get_db(
  version    = version_rs,
  local_data = TRUE,
  cache_dir  = stage_dir,
  tables     = avail,
  refresh    = force_pull)
is_server <- if (Sys.info()[["sysname"]] == "Linux")
# Limit DuckDB threads so it doesn't starve the Shiny server
if (is_server) dbExecute(con, "SET threads TO 1")

# materialize partitioned views → native local tables ----
# cc_get_db leaves partitioned tables (ctd_thin, ctd_summary) as remote S3
# views; materialize them so the app runs fully offline.
part_tbls <- info$tables$name[
  info$tables$name %in% avail & info$tables$partitioned]

for (tbl in part_tbls) {
  cat("materializing partitioned table:", tbl, "\n")
  tmp_name <- paste0(tbl, "_local")
  dbExecute(con, glue(
    "CREATE OR REPLACE TABLE \"{tmp_name}\" AS SELECT * FROM \"{tbl}\""))
  dbExecute(con, glue("DROP VIEW IF EXISTS \"{tbl}\""))
  dbExecute(con, glue("ALTER TABLE \"{tmp_name}\" RENAME TO \"{tbl}\""))
  n <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM \"{tbl}\""))$n
  cat("  ", tbl, ":", format(n, big.mark = ","), "rows\n")
}

# derive app-facing helper columns on ctd_cast ----
# cast_seq: numeric station-occupation order (drops the zero-pad on ord_occ);
#   one row with ord_occ = "001B" gets NULL via TRY_CAST and is harmless.
# dtime_pt: legible local Pacific time (America/Los_Angeles, auto DST) — used
#   in the Casts table + map hover instead of UTC. datetime_utc stays in place
#   for downstream code that wants the canonical timestamp.
cat("adding cast_seq + dtime_pt to ctd_cast...\n")
# icu provides named-timezone support (America/Los_Angeles -> auto DST)
dbExecute(con, "INSTALL icu; LOAD icu;")
# defensive: drop the geom column before any UPDATE touches ctd_cast. duckdb
# (through at least v1.5.1) hits an internal `GetChildStats not implemented
# for ColumnData of type GEOMETRY('OGC:CRS84')` on the row-group checkpoint
# that follows an UPDATE / CREATE INDEX on a table carrying a CRS-tagged
# GEOMETRY column. the app builds its own sf from lon_dec / lat_dec and
# never reads ctd_cast.geom, so dropping it costs nothing here. revisit
# once the upstream bug is fixed (issue: see CalCOFI/workflows refs).
if ("geom" %in% dbListFields(con, "ctd_cast")) {
  cat("  dropping ctd_cast.geom (unused by the app; avoids duckdb",
      "GeoColumnData::GetChildStats checkpoint bug)\n")
  dbExecute(con, "ALTER TABLE ctd_cast DROP COLUMN geom")
}
dbExecute(con, "ALTER TABLE ctd_cast ADD COLUMN IF NOT EXISTS cast_seq INTEGER")
dbExecute(con, "UPDATE ctd_cast SET cast_seq = TRY_CAST(ord_occ AS INTEGER)")
dbExecute(con, "ALTER TABLE ctd_cast ADD COLUMN IF NOT EXISTS dtime_pt TIMESTAMP")
dbExecute(con,
  "UPDATE ctd_cast SET dtime_pt =
     (datetime_utc AT TIME ZONE 'UTC') AT TIME ZONE 'America/Los_Angeles'")

# verify FK integrity ----
# ctd_thin and ctd_cast are built in the same ingest with a consistent
# cruise_key and the deterministic ctd_cast_uuid, so every thinned cast must
# resolve to a ctd_cast row (no realignment hack needed as of v2026.05.14).
n_orphan <- dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM (
    SELECT DISTINCT ctd_cast_uuid FROM ctd_thin
    EXCEPT
    SELECT DISTINCT ctd_cast_uuid FROM ctd_cast)")$n
cat("  ctd_thin casts not found in ctd_cast:", n_orphan, "\n")
stopifnot("ctd_thin.ctd_cast_uuid is not a subset of ctd_cast" = n_orphan == 0)

# indexes for fast per-cruise + per-cast lookups ----
cat("adding ART indexes...\n")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdcast_uuid   ON ctd_cast(ctd_cast_uuid)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdcast_cruise ON ctd_cast(cruise_key)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdthin_uuid   ON ctd_thin(ctd_cast_uuid)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdthin_cruise ON ctd_thin(cruise_key)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdsumm_cruise ON ctd_summary(cruise_key)")

# compact and close ----
cat("analyzing + checkpointing...\n")
dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)

# move staged duckdb into final location ----
cat("moving staged db to final path:\n  ", db_file, "\n", sep = "")
if (file.exists(db_file)) file.remove(db_file)
file.rename(staged_db, db_file)

# clean up staged parquet cache (duckdb is self-contained now) ----
pq_dir <- file.path(stage_dir, "parquet")
if (dir.exists(pq_dir)) {
  cat("removing staged parquet cache:", pq_dir, "\n")
  unlink(pq_dir, recursive = TRUE)
}
unlink(file.path(stage_dir, "latest.txt"))
unlink(list.files(stage_dir, "^catalog_.*\\.json$", full.names = TRUE))

size_gb <- round(file.info(db_file)$size / 1024^3, 2)
cat("done.\n  path:", db_file, "\n  size:", size_gb, "GB\n")

# bathymetry — crop from the freshly built db (a forced run rebuilds both)
if (bathy_needed) crop_bathy(db_file, gebco_src, bathy_tif)
