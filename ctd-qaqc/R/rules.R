# R/rules.R — the QC rule engine for ctd-qaqc
# -----------------------------------------------------------------------------
# Rules are DATA, not code: they live in the workflows repo at
# metadata/qc_rules/ (rules.csv + sql/*.sql) so they version with the pipeline
# that produces the data they check, and so a data manager can review one in a
# diff without opening the app.
#
# SQL lives in its own file per rule rather than a `sql` column in the CSV. A
# multi-line query wedged into a CSV cell is unreviewable and un-diffable, which
# defeats the point of making the rules data in the first place.
#
# A rule's SQL MUST return at least:
#   subject_key  what is being flagged (a sample_key) — the unit of review
#   detail       one human-readable sentence naming the problem
# Any further columns are shown as-is in the findings table.
#
# Sourced by global.R; kept free of Shiny so it can be tested headlessly.

# This file is sourced in TWO places: the app session (where global.R has already
# attached everything) and a background future worker (which starts clean). It must
# therefore declare every package it uses — purrr's discard()/keep() and tibble()
# were silently inherited from global.R and only failed once a rule ran in the
# background, which is the only place it matters.
suppressMessages({
  library(dplyr); library(readr); library(stringr); library(glue)
  library(DBI);   library(purrr); library(tibble)
})

#' Parse a rule's `params` cell into a named list
#'
#' Format is `k=v;k=v` — deliberately flat. Anything needing more structure than
#' that is a sign the logic belongs in the SQL file, not the index.
qc_parse_params <- function(x) {
  if (is.na(x) || !nzchar(str_trim(x))) return(list())
  kv <- str_split(str_trim(x), ";")[[1]] |> discard(\(s) !nzchar(str_trim(s)))
  out <- lapply(kv, \(p) {
    parts <- str_split(p, "=", n = 2)[[1]]
    if (length(parts) != 2) stop("malformed param: '", p, "'", call. = FALSE)
    str_trim(parts[2])
  })
  names(out) <- vapply(kv, \(p) str_trim(str_split(p, "=", n = 2)[[1]][1]), character(1))
  out
}

#' Substitute `{{param}}` placeholders into a rule's SQL
#'
#' Errors on a placeholder with no matching param. Silently leaving `{{threshold}}`
#' in the query would produce a DuckDB parse error far from its cause, and worse, a
#' *missing* threshold could otherwise render as an empty string and change the
#' rule's meaning rather than failing.
qc_render_sql <- function(sql, params) {
  needed <- str_match_all(sql, "\\{\\{([a-z_]+)\\}\\}")[[1]][, 2] |> unique()
  missing <- setdiff(needed, names(params))
  if (length(missing)) {
    stop("rule SQL needs param(s) not supplied: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  for (nm in needed) sql <- str_replace_all(sql, glue("\\{{\\{{{nm}\\}}\\}}"), params[[nm]])
  sql
}

#' Read the rule registry, attaching SQL text and parsed params
#'
#' @param dir directory holding rules.csv and sql/
#' @param active_only drop rules parked for a later phase
qc_read_rules <- function(dir, active_only = TRUE) {
  path <- file.path(dir, "rules.csv")
  stopifnot("rules.csv not found" = file.exists(path))
  d <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) |>
    mutate(active = tolower(active) %in% c("true", "t", "yes", "1"))

  if (active_only) d <- filter(d, active)

  d$sql <- vapply(seq_len(nrow(d)), \(i) {
    f <- d$sql_file[i]
    if (is.na(f) || !nzchar(f)) return(NA_character_)
    p <- file.path(dir, "sql", f)
    if (!file.exists(p)) stop("rule '", d$rule_key[i], "' references missing SQL: ", f,
                              call. = FALSE)
    paste(readLines(p, warn = FALSE), collapse = "\n")
  }, character(1))

  # an active rule with no SQL is a registry error, not an empty result set
  bad <- d$rule_key[d$active & is.na(d$sql)]
  if (length(bad)) {
    stop("active rule(s) with no sql_file: ", paste(bad, collapse = ", "),
         "\n  park them with active=FALSE until their SQL exists", call. = FALSE)
  }

  d$params <- lapply(d$params, qc_parse_params)
  d
}

#' Which measurement types actually exist for this dataset
#'
#' Computed once and passed to every rule: one DISTINCT scan instead of one
#' presence query per rule.
qc_present_types <- function(con, dataset_key = "calcofi_ctd-cast") {
  dbGetQuery(con, glue(
    "SELECT DISTINCT measurement_type FROM obs
     WHERE dataset_key = '{dataset_key}'"))$measurement_type
}

#' Execute one rule, returning its findings
#'
#' PRECONDITIONS ARE CHECKED FIRST, and this is not a nicety. A rule whose input
#' measurement type is absent returns zero rows, which is indistinguishable from
#' "the data is clean" — a false pass. The three bottle-vs-sensor calibration
#' rules did exactly that against release v2026.07.30, which carries only
#' `btl_ammonium` because it predates the change making the other bottle-reference
#' types canonical. A QA/QC tool that reports green without having checked
#' anything is worse than no tool, so an unmet precondition is `skip`, never `pass`.
#'
#' @param present_types output of [qc_present_types()]; NULL disables the check
#' @param scope_values named list supplying scope parameters, e.g.
#'   `list(cruise_key = "2023-11-33P4")`. A rule with `scope = "cruise"` runs
#'   against obs_ctd_full (212M rows, hive-partitioned by cruise_key) and is
#'   meaningless unscoped, so it SKIPS rather than silently scanning everything.
#' @param limit cap rows returned to the UI. The COUNT is always computed over the
#'   full result, so a truncated display never understates the problem — a rule
#'   that silently showed 500 of 40,000 hits would read as "minor".
#' @return list(rule_key, n, findings, elapsed_s, error, skipped, skip_reason)
qc_run_rule <- function(con, rule, limit = 500L, present_types = NULL,
                        scope_values = list()) {
  t0 <- Sys.time()
  out <- list(rule_key = rule$rule_key, n = NA_integer_, findings = NULL,
              elapsed_s = NA_real_, error = NA_character_,
              skipped = FALSE, skip_reason = NA_character_)

  req <- rule$requires_types
  if (!is.null(present_types) && !is.na(req) && nzchar(str_trim(req))) {
    need    <- str_split(req, ",")[[1]] |> str_trim() |> discard(\(x) !nzchar(x))
    missing <- setdiff(need, present_types)
    if (length(missing)) {
      out$skipped     <- TRUE
      out$skip_reason <- paste0("input absent from obs: ", paste(missing, collapse = ", "))
      out$elapsed_s   <- 0
      return(out)
    }
  }

  scope <- rule$scope %||% NA_character_
  if (!is.na(scope) && scope == "cruise" &&
      !nzchar(scope_values$cruise_key %||% "")) {
    out$skipped     <- TRUE
    out$skip_reason <- "needs a cruise — this rule reads the 212M-row obs_ctd_full and is only run one cruise at a time"
    out$elapsed_s   <- 0
    return(out)
  }

  res <- try({
    # `rule` is a one-row tibble, so rule$params is a LIST COLUMN — a list of one
    # containing the params. Unwrap it, or every {{placeholder}} silently fails to
    # resolve and the rule errors far from its cause.
    prm <- rule$params
    if (is.list(prm) && length(prm) == 1 && is.list(prm[[1]])) prm <- prm[[1]]
    # scope values (e.g. cruise_key) are supplied at run time, not in the registry
    prm <- utils::modifyList(prm, scope_values)
    sql <- qc_render_sql(rule$sql, prm)
    n <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM ({sql})"))$n
    f <- if (n > 0) dbGetQuery(con, glue("SELECT * FROM ({sql}) LIMIT {limit}")) else
      data.frame()
    list(n = as.integer(n), findings = f)
  }, silent = TRUE)

  out$elapsed_s <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  if (inherits(res, "try-error")) {
    out$error <- trimws(as.character(res))
  } else {
    out$n <- res$n; out$findings <- res$findings
  }
  out
}

#' Run every rule in a registry, one at a time
#'
#' Sequential on purpose: these are multi-GB scans and running them concurrently
#' against one DuckDB just contends for the same buffer pool.
#'
#' @param on_progress optional callback(i, n, rule_key) for the UI
qc_run_all <- function(con, rules, limit = 500L, on_progress = NULL,
                       present_types = qc_present_types(con),
                       scope_values = list()) {
  lapply(seq_len(nrow(rules)), \(i) {
    if (!is.null(on_progress)) on_progress(i, nrow(rules), rules$rule_key[i])
    qc_run_rule(con, rules[i, ], limit = limit, present_types = present_types,
                scope_values = scope_values)
  })
}

#' Collapse rule results into one row per rule for the summary table
#'
#' `skip` is deliberately its own status rather than folded into `pass`: they mean
#' opposite things about how much you should trust the run.
qc_summarize <- function(results, rules) {
  tibble(
    rule_key    = vapply(results, \(r) r$rule_key, character(1)),
    n           = vapply(results, \(r) as.integer(r$n %||% NA_integer_), integer(1)),
    elapsed_s   = vapply(results, \(r) as.numeric(r$elapsed_s %||% NA_real_), numeric(1)),
    error       = vapply(results, \(r) r$error %||% NA_character_, character(1)),
    skipped     = vapply(results, \(r) isTRUE(r$skipped), logical(1)),
    skip_reason = vapply(results, \(r) r$skip_reason %||% NA_character_, character(1))) |>
    left_join(rules |> select(rule_key, rule_type, severity, target, description,
                              any_of("scope")),
              by = "rule_key") |>
    mutate(status = case_when(
      skipped             ~ "skip",
      !is.na(error)       ~ "ERROR",
      n == 0              ~ "pass",
      severity == "error" ~ "FAIL",
      TRUE                ~ "flag"),
      note = coalesce(error, skip_reason)) |>
    select(rule_key, status, n, severity, rule_type, target, description,
           any_of("scope"), elapsed_s, note)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
