# prep_db.R — build the local ctd-qaqc database
#
# usage:
#   Rscript prep_db.R                # latest release
#   Rscript prep_db.R v2026.07.30    # a specific version
#   Rscript prep_db.R latest TRUE    # force rebuild
#
# Idempotent: skips the build when the target exists unless forced.
#
# WHAT THIS MATERIALIZES, and why it is not just "the release":
#
#   obs / sample   the CTD slice of the core model. The release no longer publishes
#                  ctd_thin / ctd_cast / ctd_measurement as tables — v2026.07.30
#                  ships obs / sample / obs_ctd_full — so every rule targets the
#                  core model. Filtered to dataset_key = 'calcofi_ctd-cast' to keep
#                  this a CTD tool rather than a copy of the whole release.
#
#   measurement_type / measurement_qual
#                  from the WORKFLOWS REGISTRY, not from the release. The registry
#                  is the source of truth and moves ahead of the release: valid_min
#                  / valid_max exist there now but not in v2026.07.30, so sourcing
#                  them from the release would silently disable every range rule.
#
# The review ledger is NOT here. It lives in its own ctd-qaqc-review.duckdb,
# created by the app, because DuckDB cannot hold a read-only and a read-write
# handle on one file at once — and because a re-prep should replace the
# materialized copy without ever touching a reviewer's work.
#
# The full-resolution obs_ctd_full (212M rows) is deliberately NOT materialized —
# the spike and up/down rules that need it are parked (see rules.csv).

devtools::load_all("../../calcofi4r")
librarian::shelf(DBI, duckdb, fs, glue, readr, quiet = TRUE)

options(timeout = max(1800, getOption("timeout", 60)))

cli_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", cli_args[grepl("^--file=", cli_args)])
app_dir  <- if (length(file_arg) > 0) dirname(normalizePath(file_arg, mustWork = FALSE)) else getwd()

args       <- commandArgs(trailingOnly = TRUE)
db_version <- if (length(args) >= 1) args[1] else "latest"
force_pull <- if (length(args) >= 2) as.logical(args[2]) else FALSE

db_file <- if (dir.exists("/share/data")) {
  "/share/data/ctd-qaqc/ctd-qaqc.duckdb"
} else {
  path.expand("~/_big/calcofi.org/ctd-qaqc/ctd-qaqc.duckdb")
}
dir_create(path_dir(db_file))

# registries: prefer the sibling checkout, fall back to raw GitHub so the app can
# be prepped on a server that has no workflows clone
wf_local <- normalizePath(file.path(app_dir, "../../workflows"), mustWork = FALSE)
wf_raw   <- "https://raw.githubusercontent.com/CalCOFI/workflows/main"
wf_path  <- function(rel) {
  local <- file.path(wf_local, rel)
  if (file.exists(local)) local else file.path(wf_raw, rel)
}

if (file.exists(db_file) && isTRUE(force_pull)) file_delete(db_file)

if (file.exists(db_file) && !isTRUE(force_pull)) {
  cat("database already present — pass TRUE as arg 2 to force a rebuild:\n  ", db_file, "\n")
  quit(save = "no")
}

# -- pull the CTD slice of the release ----------------------------------------
con_rel <- cc_get_db(version = db_version)
version_used <- tryCatch(cc_db_version(con_rel), error = function(e) db_version)
cat("release:", version_used, "\n")

con <- dbConnect(duckdb::duckdb(dbdir = db_file, read_only = FALSE))
dbExecute(con, "SET threads TO 2")

DATASET <- "calcofi_ctd-cast"
for (tbl in c("obs", "sample")) {
  cat("materializing", tbl, "... ")
  d <- dbGetQuery(con_rel, glue(
    "SELECT * FROM {tbl} WHERE dataset_key = '{DATASET}'"))
  dbWriteTable(con, tbl, d, overwrite = TRUE)
  cat(format(nrow(d), big.mark = ","), "rows\n")
}
dbDisconnect(con_rel, shutdown = TRUE)

# -- registries (authoritative, from workflows) -------------------------------
d_mt <- read_csv(wf_path("metadata/measurement_type.csv"), show_col_types = FALSE)
dbWriteTable(con, "measurement_type", as.data.frame(d_mt), overwrite = TRUE)
cat("measurement_type:", nrow(d_mt), "rows",
    if ("valid_min" %in% names(d_mt))
      glue("({sum(!is.na(d_mt$valid_min))} with a declared range)") else
      "(WARNING: no valid_min column — range rules will error)", "\n")

d_mq <- read_csv(wf_path("metadata/measurement_qual.csv"),
                 col_types = cols(qual_code = col_character(), .default = col_guess()))
dbWriteTable(con, "measurement_qual", as.data.frame(d_mq), overwrite = TRUE)
cat("measurement_qual:", nrow(d_mq), "codes\n")

# -- QC reference tables (Phase 5) --------------------------------------------
# Mined from the CalCOFI hydrographic Access master, committed as small CSVs
# under metadata/calcofi/hydro-master/reference/. These are the QC engine's
# reference INPUTS — a climatology to compare against, station bottom depths, the
# standard-depth grid — and are deliberately NOT part of the release.
ref_tbls <- c("climatology_harmonic", "station", "standard_depth", "station_class",
              "mld_sigma", "nutclinedepth")
for (t in ref_tbls) {
  d <- read_csv(wf_path(glue("metadata/calcofi/hydro-master/reference/{t}.csv")),
                show_col_types = FALSE)
  dbWriteTable(con, t, as.data.frame(d), overwrite = TRUE)
  cat(sprintf("  %-22s %s rows\n", t, format(nrow(d), big.mark = ",")))
}
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_clim ON climatology_harmonic(site_key, measurement_type)")

# -- seafloor depth at each cast position --------------------------------------
# CTD casts carry NO reported bottom depth: `bottom_depth` exists in
# sample_measurement for 33,363 BOTTLE casts and for 0 of 14,336 CTD casts. So the
# only reference available for "is this cast in water this deep" is a bathymetry
# model. Reuse the GEBCO 2025 raster apps/ctd-viz already crops and commits rather
# than re-deriving it — same source, one maintainer, and it is only 4 MB.
#
# Positive-down depth in metres, land clamped to 0 (see crop_bathy in ctd-viz).
gebco_tif <- normalizePath(file.path(app_dir, "../ctd-viz/data/gebco_calcofi.tif"),
                           mustWork = FALSE)
if (file.exists(gebco_tif)) {
  librarian::shelf(terra, quiet = TRUE)
  pos <- dbGetQuery(con, "
    SELECT sample_key, longitude, latitude FROM sample
    WHERE sample_type = 'cast' AND longitude IS NOT NULL AND latitude IS NOT NULL")
  pos$seafloor_depth_m <- terra::extract(
    terra::rast(gebco_tif), as.matrix(pos[, c("longitude", "latitude")]))[, 1]
  dbWriteTable(con, "sample_seafloor",
               pos[, c("sample_key", "seafloor_depth_m")], overwrite = TRUE)
  cat("sample_seafloor:", nrow(pos), "casts,",
      sum(!is.na(pos$seafloor_depth_m)), "with a GEBCO depth\n")
} else {
  # an absent raster must not look like clean data — leave the table missing so the
  # two bathymetry rules ERROR visibly rather than returning zero rows
  cat("WARNING: GEBCO raster not found at", gebco_tif, "\n",
      "  the seafloor rules will error rather than silently pass.\n",
      "  fix: run `Rscript prep_db.R` in ../ctd-viz first\n")
}

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_sample ON obs(sample_key)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_obs_type   ON obs(measurement_type)")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_smp_key    ON sample(sample_key)")

writeLines(version_used, file.path(app_dir, "data", "release_version.txt"))
dbDisconnect(con, shutdown = TRUE)
cat("\ndone ->", db_file, "\n")
