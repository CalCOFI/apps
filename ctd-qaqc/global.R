# global.R — ctd-qaqc
#
# A QA/QC review tool for CalCOFI CTD data. Deliberately a Shiny app rather than a
# static DuckDB-WASM page, for three reasons a static page cannot meet:
#   * checks are RUN here, in the background, not read from a precomputed file;
#   * review verdicts are written and must persist;
#   * the CTD slice is millions of rows, with a 212M-row supplemental behind it.

# packages ----
librarian::shelf(
  bslib, bsicons, DBI, dplyr, DT, duckdb, future, glue, htmltools,
  plotly, promises, purrr, readr, sf, shiny, stringr, tibble,
  bbest / mapgl,
  quiet = T)

# background execution: rules are multi-second scans, and a QA run should never
# freeze the session it was launched from
future::plan(future::multisession)

# url bookmarking ----
enableBookmarking(store = "url")

# paths ----
app_dir <- dirname(normalizePath(
  sys.frame(1)$ofile %||% "global.R", mustWork = FALSE))
if (!dir.exists(app_dir)) app_dir <- getwd()

db_dir <- if (dir.exists("/share/data")) {
  "/share/data/ctd-qaqc"
} else {
  path.expand("~/_big/calcofi.org/ctd-qaqc")
}
db_file     <- file.path(db_dir, "ctd-qaqc.duckdb")         # read-only, rebuilt
review_file <- file.path(db_dir, "ctd-qaqc-review.duckdb")  # read-write, durable
stopifnot(
  "prepped database not found; run `Rscript prep_db.R` first" = file.exists(db_file))

# rules live in the workflows repo so they version with the pipeline that produces
# the data they check; prefer the sibling checkout, fall back to a bundled copy
rules_dir <- local({
  sibling <- normalizePath(file.path(app_dir, "../../workflows/metadata/qc_rules"),
                           mustWork = FALSE)
  if (dir.exists(sibling)) sibling else file.path(app_dir, "qc_rules")
})
stopifnot("qc_rules not found" = dir.exists(rules_dir))

source(file.path(app_dir, "R/rules.R"))

release_version <- tryCatch(
  readLines(file.path(app_dir, "data/release_version.txt"), warn = FALSE)[1],
  error = function(e) "unknown")

# database ----
# read-only for the rule engine; a second short-lived writable connection is opened
# per verdict in server.R. DuckDB permits one writer, and holding a write handle
# open for the session would block every other worker.
con <- dbConnect(duckdb::duckdb(dbdir = db_file, read_only = TRUE))
dbExecute(con, "SET threads TO 2")
onStop(function() try(dbDisconnect(con, shutdown = TRUE), silent = TRUE))

rules_all    <- qc_read_rules(rules_dir, active_only = FALSE)
rules_active <- filter(rules_all, active)
present_types <- qc_present_types(con)

n_obs <- dbGetQuery(con, "SELECT COUNT(*) n FROM obs")$n

# cruises, newest first — the scope selector for rules that read obs_ctd_full
cruises <- dbGetQuery(con, "
  SELECT cruise_key, COUNT(DISTINCT sample_key) AS n_casts
  FROM sample WHERE sample_type = 'cast' AND cruise_key IS NOT NULL
  GROUP BY 1 ORDER BY cruise_key DESC")
n_cruise_scoped <- sum((rules_active$scope %||% "all") == "cruise", na.rm = TRUE)

# a rule whose input is missing is reported as `skip`, never `pass` — surface that
# at startup too, so the header never implies more coverage than the run has
n_skip <- sum(vapply(seq_len(nrow(rules_active)), \(i) {
  req <- rules_active$requires_types[i]
  if (is.na(req) || !nzchar(str_trim(req))) return(FALSE)
  need <- str_split(req, ",")[[1]] |> str_trim() |> discard(\(x) !nzchar(x))
  length(setdiff(need, present_types)) > 0
}, logical(1)))

# -- review ledger -------------------------------------------------------------
# A SEPARATE database file, and not merely for tidiness. DuckDB cannot hold a
# read-only and a read-write handle on the same file at once, so writing verdicts
# into the materialized copy would either conflict with this session's read-only
# `con` or, if `con` were opened read-write, lock out the background workers that
# need to read it. Splitting them also means a re-prep simply replaces the
# materialized copy and never has to back up and restore a reviewer's work.
#
# Connections here are short-lived by design: shiny-server may run several R
# processes for one app, and a held-open writer would block them.
qc_review_con <- function(read_only = TRUE) {
  dbConnect(duckdb::duckdb(dbdir = review_file, read_only = read_only))
}

local({
  cw <- qc_review_con(read_only = FALSE)
  on.exit(try(dbDisconnect(cw, shutdown = TRUE), silent = TRUE), add = TRUE)
  dbExecute(cw, "
    CREATE TABLE IF NOT EXISTS qc_review (
      review_id    BIGINT,
      rule_key     VARCHAR NOT NULL,
      subject_key  VARCHAR NOT NULL,
      verdict      VARCHAR NOT NULL,
      note         VARCHAR,
      reviewer     VARCHAR,
      release      VARCHAR,
      reviewed_at  TIMESTAMP)")
  dbExecute(cw, "CREATE SEQUENCE IF NOT EXISTS seq_qc_review START 1")
})

#' Append one review verdict
qc_write_verdict <- function(rule_key, subject_key, verdict, note, reviewer, release) {
  cw <- qc_review_con(read_only = FALSE)
  on.exit(try(dbDisconnect(cw, shutdown = TRUE), silent = TRUE), add = TRUE)
  dbExecute(cw, "
    INSERT INTO qc_review
      (review_id, rule_key, subject_key, verdict, note, reviewer, release, reviewed_at)
    VALUES (nextval('seq_qc_review'), ?, ?, ?, ?, ?, ?, now())",
    params = list(rule_key, subject_key, verdict, note %||% "", reviewer, release))
  invisible(TRUE)
}

qc_read_verdicts <- function() {
  cr <- qc_review_con(read_only = TRUE)
  on.exit(try(dbDisconnect(cr, shutdown = TRUE), silent = TRUE), add = TRUE)
  dbGetQuery(cr, "
    SELECT rule_key, subject_key, verdict, note, reviewer, reviewed_at
    FROM qc_review ORDER BY reviewed_at DESC")
}

# -- profiles ------------------------------------------------------------------
# A finding is a key and a number; a reviewer cannot judge one without seeing the
# value in the profile it came from. The fetch itself lives in calcofi4db
# (qc_cast_profile), with tests, because it carries two traps: the direction
# suffix must be stripped without eating the `d` in `calcofi_ctd-cast`, and
# obs_ctd_full must be pruned to its cruise partition or a single profile scans
# 212M rows.
#
# ctd-viz answers a DIFFERENT question and is linked to rather than duplicated: it
# interpolates ACROSS stations to draw a section. This is one cast, at full
# resolution, with both directions overlaid — which is the view a QC reviewer
# needs and which nothing in the org had.

# `sample.data_stage` (final | preliminary) arrived with the v2026.08 release.
# The app is deployed independently of the release it reads, so ask the catalog
# rather than assume: against an older DB the column is simply absent and every
# cast reads NA, which renders as no badge — not as a broken Profile tab.
HAS_DATA_STAGE <- "data_stage" %in% dbGetQuery(con,
  "SELECT column_name FROM information_schema.columns
   WHERE table_name = 'sample'")$column_name

#' Casts on one cruise, for the cast selector and the map
qc_cruise_casts <- function(cruise_key) {
  if (!nzchar(cruise_key %||% "")) return(tibble())
  stage_col <- if (HAS_DATA_STAGE) "data_stage" else "NULL::VARCHAR AS data_stage"
  dbGetQuery(con, glue("
    SELECT sample_key, cruise_key, site_key, grid_key, longitude, latitude,
           datetime, order_occ, {stage_col}
    FROM sample WHERE cruise_key = ? ORDER BY sample_key"),
    params = list(cruise_key)) |> as_tibble()
}

#' `final` / `preliminary` for one cast, or NA where the release cannot say
#'
#' The source is explicit that preliminary data are "for non-publication use" and
#' that oxygen, nitrate and chlorophyll may move significantly after post-cruise
#' calibration — which is exactly the context a reviewer needs before calling a
#' value a defect.
qc_cast_stage <- function(sample_key) {
  if (!HAS_DATA_STAGE || !nzchar(sample_key %||% "")) return(NA_character_)
  d <- dbGetQuery(con, "SELECT data_stage FROM sample WHERE sample_key = ?",
                  params = list(sample_key))$data_stage
  if (length(d)) d[1] else NA_character_
}

#' "(down)" / "(up)" for a cast key, for use in a selector label
qc_dir_label <- function(sample_key) {
  d <- calcofi4db::qc_cast_direction(sample_key)
  ifelse(is.na(d), "", paste0("(", d, ")"))
}

#' The measurement types a given cast actually recorded
#'
#' Populated from the cast, not from the registry: offering a reviewer a type that
#' this cast never measured produces an empty plot with no explanation.
qc_cast_types <- function(sample_key) {
  if (!nzchar(sample_key %||% "")) return(character(0))
  base <- calcofi4db::qc_cast_base(sample_key)
  ck <- dbGetQuery(con, "SELECT cruise_key FROM sample WHERE sample_key = ?",
                   params = list(sample_key))$cruise_key
  if (!length(ck)) return(character(0))
  dbGetQuery(con, glue(
    "SELECT DISTINCT measurement_type FROM obs_ctd_full
     WHERE cruise_key = ? AND sample_key LIKE ?
     ORDER BY 1"),
    params = list(ck[1], paste0(base, "%")))$measurement_type
}

# down and up are the whole point of the overlay, so they get fixed colours rather
# than plotly's defaults — a reviewer comparing two casts should not have to check
# the legend to know which is which
DIR_COLOR <- c(down = "#1f77b4", up = "#e07b39")

# the canonical profile variables first, then everything else alphabetically —
# a reviewer opening a cast almost always wants temperature or salinity
PROFILE_TYPE_ORDER <- c(
  "temperature_ave", "salinity_ave_corr", "sigma_theta_1",
  "oxygen_ml_l_ave_sta_corr", "oxygen_umol_kg_ave_sta_corr", "fluorescence_v",
  "ph", "par", "pressure")

order_profile_types <- function(x) {
  c(intersect(PROFILE_TYPE_ORDER, x), sort(setdiff(x, PROFILE_TYPE_ORDER)))
}

# -- upload --------------------------------------------------------------------
# Registries the upload path maps through. Both live in the workflows repo so a
# reviewer can see, in a diff, which sensor became which measurement type.
d_meas_type <- calcofi4db::read_measurement_type(
  file.path(dirname(rules_dir), "measurement_type.csv"))
d_sbe_map <- readr::read_csv(
  file.path(dirname(rules_dir), "sbe_name_map.csv"), show_col_types = FALSE)

workflows_dir <- normalizePath(file.path(dirname(rules_dir), ".."), mustWork = FALSE)
gebco_tif <- normalizePath(
  file.path(app_dir, "../ctd-viz/data/gebco_calcofi.tif"), mustWork = FALSE)

UPLOAD_ACCEPT <- c(".csv", ".cnv", ".asc", ".btl", ".hex")

# deep-link a flagged cast into ctd-viz for the interpolated section view
ctd_viz_url <- function(sample_key = NULL, cruise_key = NULL) {
  base <- "https://app.calcofi.io/ctd/"
  if (is.null(cruise_key) || is.na(cruise_key) || !nzchar(cruise_key)) return(base)
  paste0(base, "?cruise=", utils::URLencode(as.character(cruise_key), reserved = TRUE))
}
