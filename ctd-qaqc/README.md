# ctd-qaqc

A QA/QC review tool for CalCOFI CTD data: run quality rules against a release,
inspect what they flag, and record an attributable verdict on each finding.

## Why Shiny and not a static page

`db-query` serves ad-hoc SQL against the release from a static DuckDB-WASM page,
and that pattern was the obvious first candidate here. It does not fit, for three
reasons:

- **Checks are run, not read.** A static page can only query a precomputed file.
  Rules here are executed on demand, in the background, against the live database.
- **Review state is written.** Who flagged which cast, when, and why is the whole
  point of a review tool, and it has to persist.
- **Scale.** The CTD slice is millions of rows, with a 212 M-row supplemental
  (`obs_ctd_full`) behind it — hostile to WASM, routine for server-side DuckDB.

## Rules are data, not code

Rules live in the **workflows** repo at `metadata/qc_rules/`:

```
rules.csv        index: key, type, severity, params, requires_types, active, provenance
sql/*.sql        one file per rule
```

They sit there rather than in this app so they version with the pipeline that
produces the data they check, and so a data manager can review one in a diff
without opening the app. SQL is a file per rule, not a `sql` column in the CSV: a
multi-line query wedged into a CSV cell is neither reviewable nor diffable, which
would defeat the point of making rules data at all.

A rule's SQL must return at least `subject_key` (what is flagged — the unit of
review) and `detail` (one sentence naming the problem). Extra columns are shown
as-is. `{{placeholders}}` are filled from the `params` cell, and a placeholder
with no matching param is an error rather than an empty string.

### A skip is not a pass

A rule whose input measurement type is absent returns zero rows, which looks
exactly like clean data. The three bottle-vs-sensor calibration rules did that
against release `v2026.07.30`, which carries only `btl_ammonium` because it
predates the change making the other bottle-reference types canonical.

So every rule declares `requires_types`, the engine checks them first, and an
unmet precondition reports **`skip`** with the missing input named — never
`pass`. A QA/QC tool that reports green without having checked anything is worse
than no tool.

## Setup

```bash
cd apps/ctd-qaqc
Rscript prep_db.R                 # latest release
Rscript prep_db.R v2026.07.30     # a specific version
Rscript prep_db.R latest TRUE     # force rebuild
```

Builds `/share/data/ctd-qaqc/ctd-qaqc.duckdb` (or `~/_big/calcofi.org/...`
locally) holding the CTD slice of `obs` / `sample`, the measurement registries,
and the `qc_review` ledger.

Two things it does deliberately:

- **Registries come from the workflows repo, not the release.** The registry moves
  ahead of the release — `valid_min` / `valid_max` exist there now but not in
  `v2026.07.30` — so sourcing them from the release would silently disable every
  range rule.
- **The review ledger lives in a separate file** (`ctd-qaqc-review.duckdb`), so a
  rebuild replaces the materialized copy without ever touching a reviewer's work.
  That split is also required, not just tidy: DuckDB cannot hold a read-only and a
  read-write handle on one file, so verdicts written into the materialized copy
  would conflict with the session's read handle or lock out the background workers.

Then:

```r
shiny::runApp()
```

## Deploy

Same path as the other apps in this monorepo — no new hosting:

```bash
ssh calcofi
git -C /share/github/CalCOFI/apps pull --ff-only
docker exec -d rstudio bash -lc \
  'cd /share/github/CalCOFI/apps/ctd-qaqc && Rscript prep_db.R latest TRUE'
touch /share/github/CalCOFI/apps/ctd-qaqc/restart.txt
```

## Status

12 active rules: the `-99` sentinel regression guard, referential integrity,
missing positions, declared value ranges, the quality-code vocabulary,
bottle-vs-sensor calibration offsets (temperature / salinity / oxygen), density
inversions, the climatological anomaly, and two bathymetry checks.

Bathymetry deserves a note. CTD casts carry **no reported bottom depth** —
`bottom_depth` exists in `sample_measurement` for 33,363 bottle casts and for 0
of 14,336 CTD casts — so `prep_db.R` samples the GEBCO 2025 raster that
`apps/ctd-viz` already crops and commits. That supports "did this cast measure
below the seafloor" (a regression guard: a depth unit error or sign flip would
blow it up) and an adapted form of the Access master's
`TQ - BottomDepth_Vs_AvgBottomDepth`. The adaptation changes the semantics and the
rule says so: the original compared the ship's echosounder reading to the station
average, catching a bad sounder *or* a mispositioned cast; the GEBCO form tests
position plausibility only.

### Full-resolution rules run against the release in place

Four rules — temperature and salinity spikes, loop edits, and down-vs-upcast
disagreement — need `obs_ctd_full`, the 212 M-row full-resolution scans. Those are
**not materialized**: `prep_db.R` creates a view over the release (a local copy
when there is one, otherwise GCS over `httpfs`), because copying 212 M rows to
serve rules that are always run per profile would bloat this database ~30×.

`obs_ctd_full` is hive-partitioned by `cruise_key`, so one cruise prunes to ~2 M
rows and each rule returns in about 0.04 s. These rules are therefore
**cruise-scoped**: pick a cruise in the sidebar, and without one they report
`skip` — again, never `pass`.

Related: `apps/ctd-viz` (profile inspection — findings deep-link into it),
`CalCOFI/workflows` (the pipeline, the rules, and the plan under `libs/plans/`).
