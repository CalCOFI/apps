# R/rules.R — load the QC rule engine
# -----------------------------------------------------------------------------
# The engine itself now lives in calcofi4db (>= 3.3.0, `R/qc.R`):
#   qc_read_rules()  qc_run_rule()  qc_run_all()  qc_summarize()
#   qc_present_types()  qc_parse_params()  qc_render_sql()  qc_stage_reference()
#
# It moved because it gained a second caller. `ingest_calcofi_ctd-cast.qmd` runs
# the same registry over the parquet it has just written, so the notebook and this
# app report the condition of the same data by the same code. Keeping a private
# copy here would have guaranteed the two drifted — the same failure the
# per-dataset core-projection `switch()` arms produced, where every divergence was
# a silent data error.
#
# Rules stay DATA in the workflows repo (metadata/qc_rules/: rules.csv +
# sql/*.sql), where they version with the pipeline that produces the data they
# check. Only the executor is packaged.
#
# This file survives as a LOADER because it is sourced in TWO places: the app
# session (global.R) and a background future worker (server.R), which starts clean
# and inherits nothing. Both define `app_dir` before sourcing it.

suppressMessages({
  library(dplyr); library(readr); library(stringr); library(glue)
  library(DBI);   library(purrr); library(tibble)
})

local({
  # a sibling checkout wins, so app work tracks package work without a reinstall;
  # the server has no checkout and uses the installed package
  sibling <- normalizePath(file.path(app_dir, "../../calcofi4db"), mustWork = FALSE)
  if (dir.exists(sibling) && requireNamespace("devtools", quietly = TRUE)) {
    suppressMessages(devtools::load_all(sibling, quiet = TRUE))
  } else {
    librarian::shelf(CalCOFI / calcofi4db, quiet = TRUE)
  }
})

stopifnot(
  "calcofi4db >= 3.3.0 is required (qc_read_rules not found)" =
    length(find("qc_read_rules")) > 0)

`%||%` <- function(x, y) if (is.null(x)) y else x
