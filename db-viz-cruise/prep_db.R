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

librarian::shelf(DBI, duckdb, fs, glue, quiet = TRUE)
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

# resolve the current frozen release version, then point at its parquet CORE.
rel_base <- "https://storage.googleapis.com/calcofi-db/ducklake/releases"
rel <- tryCatch(
  trimws(readLines(url(glue("{rel_base}/latest.txt")), warn = FALSE)),
  error = function(e) stop("could not resolve release version from latest.txt: ",
                           conditionMessage(e)))
rel <- rel[nzchar(rel)][1]
cat("release:", rel, "\n")
pq <- function(table) glue("{rel_base}/{rel}/parquet/{table}.parquet")

con <- dbConnect(duckdb::duckdb(dbdir = db_file))
dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL spatial; LOAD spatial;")

# obs = the release `sample` event dimension, projected to the app's schema and
# filtered to ROOT events (the coarse station grain). columns are aliased back to
# the app's names so global.R / server.R keep working unchanged:
#   dataset_key -> dataset   sample_type -> tbl   sample_key -> id / site_key
cat("building obs from release sample.parquet (root events)...\n")
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
  FROM read_parquet('{pq(\"sample\")}')
  WHERE parent_sample_key IS NULL
    AND latitude  IS NOT NULL
    AND longitude IS NOT NULL"))
dbExecute(con, "CREATE INDEX idx_obs_cruise ON obs(cruise_key)")

# reference: cruise + ship (for cruise labels / ship_name join in global.R).
dbExecute(con, glue("CREATE TABLE cruise AS SELECT * FROM read_parquet('{pq(\"cruise\")}')"))
dbExecute(con, glue("CREATE TABLE ship   AS SELECT * FROM read_parquet('{pq(\"ship\")}')"))

n <- dbGetQuery(con, "SELECT COUNT(*) n, COUNT(DISTINCT cruise_key) c, COUNT(DISTINCT dataset) d FROM obs")
cat(glue("obs: {n$n} rows, {n$c} cruises, {n$d} datasets\n"))
print(dbGetQuery(con, "SELECT dataset, COUNT(*) n, COUNT(DISTINCT cruise_key) cruises FROM obs GROUP BY 1 ORDER BY 1"))
dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
cat("wrote", db_file, "\n")
