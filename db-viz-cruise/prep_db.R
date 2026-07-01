# prep_db.R — build the datacheck app database: a single cross-dataset `obs`
# table (one row per sampling event across ALL released datasets, keyed by
# cruise_key + position + time) plus cruise/ship reference tables.
#
# usage:
#   Rscript prep_db.R                # uses GCS ingest parquet (current)
#   Rscript prep_db.R TRUE           # force rebuild
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

gcs <- "https://storage.googleapis.com/calcofi-db/ingest"
pq  <- function(provider_dataset, table) glue("{gcs}/{provider_dataset}/{table}.parquet")

con <- dbConnect(duckdb::duckdb(dbdir = db_file))
dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL spatial; LOAD spatial;")

# one SELECT per dataset, projected to a common schema. `id` is the deep-link
# key used in the app URL (?dataset=&id=). datasets without a column use NULL.
sel <- function(dataset, tbl, src, id_expr, datetime = "datetime_start_utc",
                site = "site_key", extra_from = NULL) glue(
  "SELECT '{dataset}' AS dataset, '{tbl}' AS tbl, CAST({id_expr} AS VARCHAR) AS id,
          cruise_key, CAST(latitude AS DOUBLE) AS latitude, CAST(longitude AS DOUBLE) AS longitude,
          CAST({datetime} AS TIMESTAMP) AS datetime, CAST({site} AS VARCHAR) AS site_key
   FROM read_parquet('{src}') {extra_from %||% ''}")

`%||%` <- function(a, b) if (is.null(a)) b else a

parts <- c(
  sel("calcofi_bottle",            "casts",                pq("calcofi_bottle","casts"), "cast_id"),
  sel("cce-lter_euphausiids",      "euphausiids_tow",      pq("cce-lter_euphausiids","euphausiids_tow"), "tow_id"),
  sel("pic_zooplankton",           "zooplankton_tow",      pq("pic_zooplankton","zooplankton_tow"), "tow_id"),
  sel("swfsc_cufes",               "cufes_sample",         pq("swfsc_cufes","cufes_sample"), "sample_id", site = "NULL"),
  sel("calcofi_phyllosoma",        "phyllosoma_tow",       pq("calcofi_phyllosoma","phyllosoma_tow"), "tow_id"),
  sel("calcofi_bird_mammal_census","bird_mammal_transect", pq("calcofi_bird_mammal_census","bird_mammal_transect"), "gis_key", site = "NULL"))

# CTD: the ctd_cast parquet is per-SCAN (~5.5M rows; ctd_cast_uuid is per-scan
# despite its name — do NOT key on it). collapse to one row per cast (cast_key is
# globally unique) so the cross-dataset obs grain is per station-cast, not per
# depth reading — otherwise a single cruise floods the map/table with tens of
# thousands of points (the DataTables Ajax error).
ctd_sql <- glue(
  "SELECT 'calcofi_ctd-cast' AS dataset, 'ctd_cast' AS tbl,
          CAST(cast_key AS VARCHAR) AS id, cruise_key,
          CAST(AVG(latitude) AS DOUBLE) AS latitude, CAST(AVG(longitude) AS DOUBLE) AS longitude,
          CAST(MIN(datetime_start_utc) AS TIMESTAMP) AS datetime,
          CAST(ANY_VALUE(site_key) AS VARCHAR) AS site_key
   FROM read_parquet('{pq(\"calcofi_ctd-cast\",\"ctd_cast\")}')
   GROUP BY cruise_key, cast_key")

# ichthyo: the site parquet carries no time — datetime lives in tow.datetime_start_utc.
# join the earliest tow per site so ichthyo stations get a real datetime (and thus
# survive date filters in this app and the integrated app) instead of NULL.
ichthyo_sql <- glue(
  "SELECT 'swfsc_ichthyo' AS dataset, 'site' AS tbl,
          CAST(s.site_uuid AS VARCHAR) AS id, s.cruise_key,
          CAST(s.latitude AS DOUBLE) AS latitude, CAST(s.longitude AS DOUBLE) AS longitude,
          CAST(t.datetime AS TIMESTAMP) AS datetime, CAST(s.site_key AS VARCHAR) AS site_key
   FROM read_parquet('{pq(\"swfsc_ichthyo\",\"site\")}') s
   LEFT JOIN (
     SELECT site_uuid, MIN(datetime_start_utc) AS datetime
     FROM read_parquet('{pq(\"swfsc_ichthyo\",\"tow\")}') GROUP BY site_uuid
   ) t ON s.site_uuid = t.site_uuid")

# DIC: cruise_key comes from the matched cast (dic_sample.cast_id -> casts)
dic_sql <- glue(
  "SELECT 'calcofi_dic' AS dataset, 'dic_sample' AS tbl,
          CAST(d.site_key || '@' || d.datetime_start_utc AS VARCHAR) AS id,
          c.cruise_key, CAST(d.latitude AS DOUBLE) AS latitude, CAST(d.longitude AS DOUBLE) AS longitude,
          CAST(d.datetime_start_utc AS TIMESTAMP) AS datetime, CAST(d.site_key AS VARCHAR) AS site_key
   FROM read_parquet('{pq(\"calcofi_dic\",\"dic_sample\")}') d
   LEFT JOIN read_parquet('{pq(\"calcofi_bottle\",\"casts\")}') c ON d.cast_id = c.cast_id")

all_sql <- c(parts, ctd_sql, ichthyo_sql, dic_sql)
cat("building obs table from", length(all_sql), "datasets...\n")
dbExecute(con, glue("CREATE TABLE obs AS\n{paste(all_sql, collapse='\nUNION ALL BY NAME\n')}"))
# drop rows with no usable position
dbExecute(con, "DELETE FROM obs WHERE latitude IS NULL OR longitude IS NULL")
dbExecute(con, "CREATE INDEX idx_obs_cruise ON obs(cruise_key)")

# reference: cruise + ship (for cruise labels). derive a date per cruise from obs.
dbExecute(con, glue("CREATE TABLE cruise AS SELECT * FROM read_parquet('{pq(\"swfsc_ichthyo\",\"cruise\")}')"))
dbExecute(con, glue("CREATE TABLE ship   AS SELECT * FROM read_parquet('{pq(\"swfsc_ichthyo\",\"ship\")}')"))

n <- dbGetQuery(con, "SELECT COUNT(*) n, COUNT(DISTINCT cruise_key) c, COUNT(DISTINCT dataset) d FROM obs")
cat(glue("obs: {n$n} rows, {n$c} cruises, {n$d} datasets\n"))
print(dbGetQuery(con, "SELECT dataset, COUNT(*) n, COUNT(DISTINCT cruise_key) cruises FROM obs GROUP BY 1 ORDER BY 1"))
dbExecute(con, "CHECKPOINT")
dbDisconnect(con, shutdown = TRUE)
cat("wrote", db_file, "\n")
