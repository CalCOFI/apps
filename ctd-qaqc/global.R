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
  promises, purrr, readr, shiny, stringr, tibble,
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

# deep-link a flagged cast into ctd-viz rather than rebuilding profile plots here
ctd_viz_url <- function(sample_key, cruise_key = NULL) {
  base <- "https://app.calcofi.io/ctd/"
  if (is.null(cruise_key) || is.na(cruise_key)) return(base)
  paste0(base, "?cruise=", utils::URLencode(as.character(cruise_key), reserved = TRUE))
}
