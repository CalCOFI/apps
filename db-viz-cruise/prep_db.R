# prep_db.R — build the db-viz-cruise app database: a single cross-dataset `obs`
# table (one row per sampling event across ALL released datasets, keyed by
# cruise_key + position + time) plus cruise/ship reference tables.
#
# usage:
#   Rscript prep_db.R                # reads the frozen release core (current)
#   Rscript prep_db.R TRUE           # force rebuild
#
# Source of truth is now the frozen release CORE, not the per-dataset ingest
# parquet: the consolidated DB ships a single `sample` event dimension (one row
# per sampling event, at every grain — cast/tow/net/bottle — via an adjacency
# list) and a single `obs` measurement table. This app maps its `obs` (one row
# per EVENT, the deep-link grain) to the release `sample` filtered to ROOT events
# (`parent_sample_key IS NULL`) — i.e. the coarse station grain (cast, site, tow,
# underway, transect …), NOT the child bottles/nets. That reproduces the app's
# original per-dataset grain (casts, ichthyo sites, ctd casts, …) without
# flooding the map/table with tens of thousands of child rows, and picks up new
# datasets (zoodb, zooscan, phytoplankton) automatically.
#
# The app lets you pick a cruise and see every dataset's observations for that
# cruise on a map/table/plot, with a deep-linkable URL to reference in the
# per-dataset provider questions (metadata/{provider}/{dataset}/questions.csv).

librarian::shelf(calcofi/calcofi4r, DBI, duckdb, fs, glue, quiet = TRUE)
options(timeout = max(1800, getOption("timeout", 60)))

args       <- commandArgs(trailingOnly = TRUE)
force_pull <- length(args) >= 1 && as.logical(args[1])

db_file <- if (dir.exists("/share/data")) {
  "/share/data/db-viz-cruise/db-viz-cruise.duckdb"
} else {
  path.expand("~/_big/calcofi.org/db-viz-cruise/db-viz-cruise.duckdb")
}
dir_create(path_dir(db_file))
if (file.exists(db_file) && !force_pull) {
  cat("database exists, skipping (pass TRUE to force):\n  ", db_file, "\n"); quit(status = 0)
}
if (file.exists(db_file)) file_delete(db_file)

# resolve the promoted release (latest.txt) through its catalog, then each core
# table through calcofi4r::cc_release_sources() — the one sanctioned map from a
# table to its parquet bytes: content-addressed objects
# (ducklake/tables/{table}/{hash}/…) since the v2026.09 releases, the per-release
# releases/{version}/parquet/… path before that. never build that path by hand.
if (packageVersion("calcofi4r") < "1.11.0")
  stop("prep_db.R needs calcofi4r >= 1.11.0 (cc_release_sources); ",
       "update with remotes::install_github('calcofi/calcofi4r')")
cat_rel <- tryCatch(
  cc_catalog("latest"),
  error = function(e) stop("could not resolve the release catalog from latest.txt: ",
                           conditionMessage(e)))
rel <- cat_rel$version
cat("release:", rel, "\n")
# beside the database, for the app's header chip (cc_brand_header(release=)):
# the release the data was built from, not whatever is latest at load time
writeLines(rel, file.path(dirname(db_file), "release_version.txt"))
pq <- function(table) cc_read_parquet_sql(cc_release_sources(cat_rel, table))

con <- dbConnect(duckdb::duckdb(dbdir = db_file))
dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL spatial; LOAD spatial;")
# a legacy (pre-v2026.09) partitioned table resolves to an s3:// glob, which
# DuckDB expands through its S3 client pointed anonymously at GCS (as in
# calcofi4r:::.cc_setup_gcs_httpfs); harmless for the https reads
dbExecute(con, "
  SET s3_region = 'auto'; SET s3_endpoint = 'storage.googleapis.com';
  SET s3_url_style = 'path'; SET s3_access_key_id = ''; SET s3_secret_access_key = '';")

# obs = the release `sample` event dimension, projected to the app's schema and
# filtered to ROOT events (the coarse station grain). columns are aliased back to
# the app's names so global.R / server.R keep working unchanged:
#   dataset_key -> dataset   sample_type -> tbl   sample_key -> id / site_key
cat("building obs from the release sample table (root events)...\n")
dbExecute(con, glue("
  CREATE TABLE obs AS
  SELECT
    dataset_key AS dataset,
    sample_type AS tbl,
    sample_key  AS id,
    cruise_key,
    latitude,
    longitude,
    datetime,
    sample_key  AS site_key
  FROM {pq(\"sample\")}
  WHERE parent_sample_key IS NULL
    AND latitude  IS NOT NULL
    AND longitude IS NOT NULL"))
dbExecute(con, "CREATE INDEX idx_obs_cruise ON obs(cruise_key)")

# reference: cruise + ship (for cruise labels / ship_name join in global.R).
dbExecute(con, glue("CREATE TABLE cruise AS SELECT * FROM {pq(\"cruise\")}"))
dbExecute(con, glue("CREATE TABLE ship   AS SELECT * FROM {pq(\"ship\")}"))

n <- dbGetQuery(con, "SELECT COUNT(*) n, COUNT(DISTINCT cruise_key) c, COUNT(DISTINCT dataset) d FROM obs")
cat(glue("obs: {n$n} rows, {n$c} cruises, {n$d} datasets\n"))
print(dbGetQuery(con, "SELECT dataset, COUNT(*) n, COUNT(DISTINCT cruise_key) cruises FROM obs GROUP BY 1 ORDER BY 1"))
dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
cat("wrote", db_file, "\n")
