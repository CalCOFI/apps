# prep_db.R - build optimized local database + bathymetry for ctd-viz app
#
# usage:
#   Rscript prep_db.R                # uses latest release
#   Rscript prep_db.R v2026.05.14    # uses a specific version
#   Rscript prep_db.R latest TRUE    # force re-download
#
# idempotent: skips a build when its target file already exists unless forced;
# the database and the bathymetry raster are tracked independently.
# rebuilds ctd_thin (~13M rows) and ctd_cast from the core sample/obs release tables,
# measurement_type, ship. the full ctd_measurement table is a *supplemental*
# release output (not in the catalog) and is intentionally NOT materialized —
# the app runs on the adaptively-thinned ctd_thin.
#
# also crops GEBCO 2025 sub-ice bathymetry to the ctd_cast extent (+ margin) ->
# data/gebco_calcofi.tif (positive-down depth, m; land clamped to 0). this is
# an app-side stopgap — bathymetry should become a first-class released
# layer: see CalCOFI/workflows#54.
#
# THIS CROP IS ALSO PUBLISHED, at gs://calcofi-db/bathymetry/gebco_2025_calcofi.tif,
# which is what calcofi4r::cc_bathy() serves to every other consumer. If a re-crop
# here changes the extent or the source grid, push the new file up too:
#   gcloud storage cp data/gebco_calcofi.tif \
#     gs://calcofi-db/bathymetry/gebco_2025_calcofi.tif
# otherwise ctd-viz and ctd-transects draw seafloors from two different rasters,
# which is the exact drift moving the sampling into calcofi4r was meant to end.

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
  terra::terraOptions(threads = 2)
  
  con_b <- dbConnect(duckdb::duckdb(dbdir = db, read_only = TRUE))
  # Limit DuckDB threads so it doesn't starve the Shiny server
  dbExecute(con_b, "SET threads TO 2")
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

# tables to include. Since the core consolidation (calcofi4db 3.0) the release
# publishes `sample` / `obs`, not the per-dataset ctd_cast / ctd_thin /
# ctd_summary this app was written against — those became compat VIEWs that no
# release ships as parquet, which is why this script could not rebuild after
# v2026.05.14 (the server served that May database until 2026-08-25). The two
# tables the app reads are rebuilt below from the core with the legacy names,
# columns and types; ctd_summary was never referenced by the app and is gone.
keep_tables <- c(
  "sample",            # core events: the CTD casts (sample_type = 'cast')
  "obs",               # core observations: ctd_thin lives here (dataset_key = 'calcofi_ctd-cast')
  "measurement_type",  # reference (incl. is_canonical)
  "ship")              # reference

# stage parquets + duckdb in a temp cache alongside the final target,
# then rename into place once done.
stage_dir <- file.path(path_dir(db_file), ".stage")
dir_create(stage_dir)

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
# the release everything below is built from, beside the database for the app's
# header chip (calcofi4r::cc_brand_header(release=)) — "latest" resolved NOW is
# not what an already-built database holds after the next release
release_used <- info$version
version_rs <- info$version
cat("resolved version:", version_rs, "\n")
# stamp the release beside the db (app header chip) now that it is known — it
# was previously written at the top, before this line defined release_used
writeLines(release_used, file.path(dirname(db_file), "release_version.txt"))

avail   <- intersect(keep_tables, info$tables$name)
missing <- setdiff(keep_tables, avail)
if (length(missing) > 0)
  cat("WARNING: not in release ", version_rs, ": ",
      paste(missing, collapse = ", "), "\n", sep = "")
stopifnot(
  "release lacks the core sample/obs tables — rebuild with v2026.07.15 or later" =
    all(c("sample", "obs") %in% avail))

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
is_server <- Sys.info()[["sysname"]] == "Linux"
# Limit DuckDB threads so it doesn't starve the Shiny server
if (is_server) res <- dbExecute(con, "SET threads TO 2")

# rebuild the legacy app tables from the core ----
# `obs` is Hive-partitioned by dataset_key, so cc_get_db leaves it as a remote
# view; the WHERE below prunes to the one CTD partition (~13M rows) rather than
# pulling all 16 datasets. Column names and TYPES match what the app was built
# against (ord_occ/line/sta are zero-padded VARCHAR, ctd_cast_uuid is the join
# key between the two tables — the namespaced sample_key serves exactly that).
cat("building ctd_cast from sample...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE ctd_cast AS
  SELECT s.sample_key                                     AS ctd_cast_uuid,
         s.data_stage,
         split_part(s.sample_key, ':', 3)                 AS cast_key,
         upper(right(split_part(s.sample_key, ':', 3), 1)) AS cast_dir,
         lpad(CAST(s.order_occ AS VARCHAR), 3, '0')       AS ord_occ,
         s.datetime                                       AS datetime_utc,
         s.datetime                                       AS datetime_start_utc,
         s.latitude, s.longitude,
         s.latitude                                       AS lat_dec,
         s.longitude                                      AS lon_dec,
         s.site_key,
         split_part(s.site_key, ' ', 1)                   AS line,
         split_part(s.site_key, ' ', 2)                   AS sta,
         s.cruise_key, s.grid_key, s.seafloor_depth_m,
         sh.ship_key                                      -- global.R joins ship on it
  FROM sample s
  LEFT JOIN ship sh ON sh.ship_nodc = substr(s.cruise_key, 9)
  WHERE s.dataset_key = 'calcofi_ctd-cast' AND s.sample_type = 'cast'")
cat("building ctd_thin from obs (CTD partition only)...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE ctd_thin AS
  SELECT CAST(o.obs_id AS VARCHAR)  AS ctd_thin_uuid,
         o.sample_key               AS ctd_cast_uuid,
         o.depth_min_m              AS depth_m,
         o.measurement_type, o.measurement_value, o.measurement_qual,
         o.cruise_key
  FROM obs o
  WHERE o.dataset_key = 'calcofi_ctd-cast'")
# obs is not needed once ctd_thin is local; drop it to keep the app fully
# offline (a remote view would otherwise try S3 on first touch). It may be a
# remote VIEW (partitioned table, the calcofi4r >= 1.12.1 default) or a local
# TABLE (older/other layouts) — drop whichever it is rather than assume a type.
.otype <- dbGetQuery(con,
  "SELECT table_type FROM information_schema.tables WHERE table_name = 'obs'")$table_type
if (length(.otype))
  dbExecute(con, sprintf("DROP %s IF EXISTS obs",
    if (any(grepl("VIEW", .otype, ignore.case = TRUE))) "VIEW" else "TABLE"))
for (tbl in c("ctd_cast", "ctd_thin")) {
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
