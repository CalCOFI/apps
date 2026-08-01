# tests/test_profile_wiring.R
# -----------------------------------------------------------------------------
# Drive the Profile tab's server reactives without a browser:
#
#   Rscript tests/test_profile_wiring.R      (from the app directory)
#
# WHY THIS EXISTS. The finding -> profile path is the one piece of this app that
# is easy to get subtly wrong and hard to notice: the ring can land on the right
# depth but the wrong cast direction, the flag can persist onto an unrelated cast,
# and the plot click and the table selection can chase each other round a loop.
# None of those show up as an error — they show up as a reviewer trusting a
# highlight that is pointing at the wrong scan.
#
# shiny::testServer() reaches the server function's own reactives, so these are
# assertions about the wiring rather than about pixels. The rendering itself
# (plot, map, table) is checked by eye; this covers what an eye cannot.
#
# Requires the prepped database (Rscript prep_db.R) and a cruise that is in it.

suppressMessages(library(shiny))

app_dir <- tryCatch(dirname(dirname(normalizePath(
  sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])))),
  error = function(e) getwd())
if (!file.exists(file.path(app_dir, "global.R"))) app_dir <- getwd()

db_dir <- if (dir.exists("/share/data")) "/share/data/ctd-qaqc" else
  path.expand("~/_big/calcofi.org/ctd-qaqc")
if (!file.exists(file.path(db_dir, "ctd-qaqc.duckdb"))) {
  cat("SKIP: prepped database not found — run `Rscript prep_db.R` first\n")
  quit(save = "no", status = 0)
}

CRUISE <- "2023-11-33P4"
CAST   <- "calcofi_ctd-cast:cast:2311_041u"   # an upcast, deliberately
CAST2  <- "calcofi_ctd-cast:cast:2311_042d"
DEPTH  <- 52

ok <- function(msg) cat(sprintf("  ok  %s\n", msg))

testServer(shinyAppDir(app_dir), {

  # a finding shaped exactly as ctd_spike_temperature returns one
  rv$results <- list(list(
    rule_key = "ctd_spike_temperature", n = 1L, elapsed_s = 0.1,
    error = NA_character_, skipped = FALSE, skip_reason = NA_character_,
    findings = data.frame(
      subject_key = CAST, detail = "temperature_ave spike at 52 m",
      cruise_key = CRUISE, depth_min_m = DEPTH,
      measurement_type = "temperature_ave", stringsAsFactors = FALSE)))
  rv$summary <- data.frame(
    rule_key = "ctd_spike_temperature", status = "flag", n = 1L,
    severity = "warning", rule_type = "profile", target = "obs_ctd_full",
    description = "spike", scope = "cruise", elapsed_s = 0.1,
    note = NA_character_, stringsAsFactors = FALSE)

  session$setInputs(tbl_summary_rows_selected = 1L,
                    tbl_findings_rows_selected = 1L)
  stopifnot(identical(sel_rule(), "ctd_spike_temperature"),
            identical(sel_finding()$subject_key, CAST))
  ok("a selected summary row resolves to its findings")

  # -- the button carries the finding into profile state -----------------------
  session$setInputs(go_profile = 1L)
  stopifnot(
    "flagged depth carried"     = identical(rv$flag_depth, as.numeric(DEPTH)),
    "flagged cast carried"      = identical(rv$flag_key, CAST),
    "originating rule carried"  = identical(rv$flag_rule, "ctd_spike_temperature"),
    "cast requested"            = identical(rv$want_cast, CAST),
    "measurement type requested"= identical(rv$want_type, "temperature_ave"))
  ok("go_profile carries depth, cast, type and rule")

  # -- the profile the plot draws ---------------------------------------------
  session$setInputs(prof_cruise = CRUISE, prof_cast = CAST,
                    prof_type = "temperature_ave", prof_show_up = TRUE)
  p <- profile()
  stopifnot("both cast directions are fetched" =
              setequal(unique(p$cast_dir), c("down", "up")))
  ok(sprintf("profile() returns %d scans across both directions", nrow(p)))

  # -- the ring is matched on depth AND direction ------------------------------
  # THE REGRESSION THIS LOCKS DOWN: both directions have a scan at the flagged
  # depth, so a depth-only match rings two points and implies the other direction
  # was flagged when it was not.
  hit  <- p[abs(p$depth_m - rv$flag_depth) <= 0.5, ]
  stopifnot("a scan exists at the flagged depth" = nrow(hit) > 0)
  n_both <- nrow(hit)
  hit <- hit[hit$sample_key == rv$flag_key, ]
  stopifnot("exactly one scan is ringed" = nrow(hit) == 1)
  ok(sprintf("%d scan(s) at %.1f m, 1 after restricting to the flagged direction (%s)",
             n_both, rv$flag_depth, hit$cast_dir))

  # -- one piece of shared state for both selection directions -----------------
  session$setInputs(tbl_profile_rows_selected = 7L)
  stopifnot("a table row selects the scan" = identical(rv$sel_scan, 7L))
  ok("table row -> shared scan state (the plot reads the same value)")

  # -- moving to another cast invalidates both --------------------------------
  session$setInputs(prof_cast = CAST2)
  stopifnot(
    "a new cast clears the selected scan" = is.null(rv$sel_scan),
    "a new cast clears the flag"          = is.na(rv$flag_depth))
  ok("a new cast clears the selection and the flag (no stale red ring)")
})

cat("\nprofile wiring: all assertions passed\n")
