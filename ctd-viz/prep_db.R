# prep_db.R - build optimized local database for ctd-viz app
#
# usage:
#   Rscript prep_db.R              # uses latest release
#   Rscript prep_db.R v2026.04.08  # uses specific version
#   Rscript prep_db.R latest TRUE  # force re-download
#
# idempotent: skips rebuild when target file exists unless forced.
# WARNING: downloads and materializes ctd_measurement locally (~15-20 GB).

# run from app dir: `cd apps/ctd-viz && Rscript prep_db.R`
devtools::load_all("../../calcofi4r")
librarian::shelf(
  DBI, duckdb, fs, glue,
  quiet = TRUE)

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

# target location ----
db_file <- if (dir.exists("/share/data")) {
  "/share/data/ctd-viz/ctd-viz.duckdb"
} else {
  path.expand("~/_big/calcofi.org/ctd-viz/ctd-viz.duckdb")
}
dir_create(path_dir(db_file))

# idempotency check
if (file.exists(db_file) && !force_pull) {
  cat("database already exists, skipping rebuild:\n  ", db_file, "\n",
      "pass 'latest TRUE' as args to force rebuild.\n", sep = "")
  quit(status = 0)
}

# tables to include (CTD subset + reference) ----
# per app usage: ctd_cast, ctd_measurement, measurement_type, ship.
# ctd_summary included for future use (station/depth aggregates).
keep_tables <- c(
  "ctd_cast",          # 6M rows
  "ctd_measurement",   # 233M rows, partitioned by cruise_key
  "ctd_summary",       # 105M rows, partitioned
  "measurement_type",  # ~100 rows, ref
  "ship")              # ref

# stage parquets + duckdb in a temp cache alongside the final target,
# then rename into place once done.
stage_dir <- file.path(path_dir(db_file), ".stage")
dir_create(stage_dir)

cat("fetching catalog for version:", db_version, "\n")
info       <- cc_db_info(version = db_version)
version_rs <- info$version
cat("resolved version:", version_rs, "\n")

# cc_get_db names the output `calcofi_{version}.duckdb` in cache_dir
staged_db  <- file.path(stage_dir, glue("calcofi_{version_rs}.duckdb"))
if (file.exists(staged_db)) {
  cat("removing stale staged db:", staged_db, "\n")
  file.remove(staged_db)
}

con <- cc_get_db(
  version    = version_rs,
  local_data = TRUE,
  cache_dir  = stage_dir,
  tables     = keep_tables,
  refresh    = force_pull)

# materialize partitioned views → native local tables ----
# cc_get_db leaves partitioned tables (ctd_measurement, ctd_summary) as
# remote S3 views; we need them physically present for an offline app.
tbl_info  <- info$tables
part_tbls <- tbl_info$name[
  tbl_info$name %in% keep_tables & tbl_info$partitioned]

for (tbl in part_tbls) {
  cat("materializing partitioned table:", tbl, "\n")
  tmp_name <- paste0(tbl, "_local")
  dbExecute(con, glue(
    "CREATE OR REPLACE TABLE \"{tmp_name}\" AS ",
    "SELECT * FROM \"{tbl}\""))
  dbExecute(con, glue("DROP VIEW IF EXISTS \"{tbl}\""))
  dbExecute(con, glue(
    "ALTER TABLE \"{tmp_name}\" RENAME TO \"{tbl}\""))
  n <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM \"{tbl}\""))$n
  cat("  ", tbl, ":", format(n, big.mark = ","), "rows\n")
}

# realign ctd_cast with ctd_measurement ----
# upstream release discrepancy: ctd_cast uses old cruise_key format
# ({YY}{MM}{ship_key}, e.g. '9802JD') and its ctd_cast_uuid is MD5'd against
# that old key. ctd_measurement uses new format (YYYY-MM-{ship_nodc}, e.g.
# '1998-02-31JD') for both cruise_key and UUID. Rebuild ctd_cast with the
# new-format cruise_key and regenerated UUID so joins work.
cat("realigning ctd_cast.cruise_key + ctd_cast_uuid with ctd_measurement...\n")
dbExecute(con, "
  CREATE OR REPLACE TABLE ctd_cast AS
  WITH aligned AS (
    SELECT
      c.* EXCLUDE (cruise_key, ctd_cast_uuid),
      (CASE WHEN CAST(substr(c.cruise_key, 1, 2) AS INT) >= 50 THEN '19' ELSE '20' END)
        || substr(c.cruise_key, 1, 2) || '-'
        || substr(c.cruise_key, 3, 2) || '-'
        || s.ship_nodc AS cruise_key
    FROM ctd_cast c
    JOIN ship s ON c.ship_key = s.ship_key)
  SELECT
    -- reconstruct UUID v?-format string (8-4-4-4-12 split of md5 hex)
    substr(_h, 1, 8)  || '-' ||
    substr(_h, 9, 4)  || '-' ||
    substr(_h, 13, 4) || '-' ||
    substr(_h, 17, 4) || '-' ||
    substr(_h, 21, 12) AS ctd_cast_uuid,
    a.* EXCLUDE (_h)
  FROM (
    SELECT
      *,
      md5(concat_ws('|',
        cruise_key,
        CAST(cast_key AS VARCHAR),
        CAST(cast_dir AS VARCHAR),
        CAST(datetime_utc AS VARCHAR))) AS _h
    FROM aligned) a
")

# verify alignment worked
n_overlap <- dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM (
    SELECT DISTINCT ctd_cast_uuid FROM ctd_cast
    INTERSECT
    SELECT DISTINCT ctd_cast_uuid FROM ctd_measurement)")$n
cat("  UUID overlap ctd_cast ∩ ctd_measurement:",
    format(n_overlap, big.mark = ","), "\n")
stopifnot("alignment failed: no UUID overlap" = n_overlap > 0)

# add index on ctd_cast.ctd_cast_uuid for transect lookups ----
cat("adding ART indexes...\n")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdcast_uuid ON ctd_cast(ctd_cast_uuid)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_ctdmeas_uuid ON ctd_measurement(ctd_cast_uuid)")

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
# leave stage_dir itself for future runs

size_gb <- round(file.info(db_file)$size / 1024^3, 2)
cat("done.\n  path:", db_file, "\n  size:", size_gb, "GB\n")
