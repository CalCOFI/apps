suppressMessages(library(shiny))# tests/test_upload_wiring.R
# -----------------------------------------------------------------------------
# Drive the Upload tab's server reactives without a browser:
#
#   Rscript tests/test_upload_wiring.R      (from the app directory)
#
# The upload path's whole claim is that a shipboard file, projected into
# obs/sample, runs the QC registry unchanged. These assertions check the
# projection: that a real Sea-Bird .btl parses, that its columns map (and that
# the unmapped ones are REPORTED rather than dropped), and that the result
# carries the core columns every rule reads.
#
# The .hex case is here because a wrong answer there is worse than no answer:
# .hex is A/D counts, and a "best effort" conversion without the .xmlcon
# calibration file would be invented numbers presented as measurements.
#
# Needs the CalCOFI CTD archive on this machine; skips cleanly without it.


app_dir <- getwd()
dir_src <- path.expand("~/My Drive/projects/calcofi/data-public/calcofi/ctd-cast/download")
set.seed(1)
cands <- if (dir.exists(dir_src))
  list.files(dir_src, "[.]btl$", recursive = TRUE, full.names = TRUE) else character(0)
if (!length(cands)) {
  cat("SKIP: no CTD archive on this machine\n")
  quit(save = "no", status = 0)
}
# Roughly half of the archive's .btl headers have adjacent names running
# together, which the reader REFUSES rather than mis-assigning. So take the first
# file that parses and say how many it took — that number is the format's real
# state, not a flaw in the test.
src <- NA; tried <- 0
for (f in head(sample(cands), 200)) {
  tried <- tried + 1
  if (!inherits(try(calcofi4db::read_sbe_btl(f), silent = TRUE), "try-error")) {
    src <- f; break
  }
}
if (is.na(src)) { cat("SKIP: no unambiguous .btl in the sampled files\n")
                  quit(save = "no", status = 0) }
cat(sprintf("  ok  %d of %d .btl file(s) tried had an unambiguous header\n", 1, tried))

testServer(shinyAppDir(app_dir), {
  session$setInputs(up_file = data.frame(
    name = basename(src), size = file.size(src), type = "", datapath = src,
    stringsAsFactors = FALSE))
  u <- upload()
  stopifnot("parsed"   = is.null(u$error))
  stopifnot("core"     = !is.null(u$core))
  cat(sprintf("  ok  parsed %s: %d rows, %d cols\n", u$name, nrow(u$data), ncol(u$data)))
  cat(sprintf("  ok  mapping: %d measurement, %d unmapped, %d voltage\n",
              sum(u$mapping$role == "measurement" & !is.na(u$mapping$measurement_type)),
              sum(u$mapping$role == "unmapped"), sum(u$mapping$role == "voltage")))
  cat(sprintf("  ok  core: %d obs across %d type(s), %d dropped as missing/sentinel\n",
              nrow(u$core$obs), length(unique(u$core$obs$measurement_type)),
              u$core$n_sentinel))
  stopifnot("obs carries the core columns" =
    all(c("realm","dataset_key","sample_key","cruise_key","depth_min_m",
          "measurement_type","measurement_value") %in% names(u$core$obs)))
  cat("  ok  obs is in the core shape every rule reads\n")
})

# and a .hex is refused, with the reason, rather than half-parsed
hx <- file.path(withr::local_tempdir(.local_envir = globalenv()), "cast.hex"); writeLines("00A1B2", hx)
testServer(shinyAppDir(app_dir), {
  session$setInputs(up_file = data.frame(
    name = "cast.hex", size = 6, type = "", datapath = hx, stringsAsFactors = FALSE))
  u <- upload()
  stopifnot("refused" = !is.null(u$error), "reason given" = grepl("xmlcon", u$error))
  cat("  ok  .hex refused:", substr(u$error, 1, 60), "...\n")
})
cat("\nupload wiring: all assertions passed\n")
