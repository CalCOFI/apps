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
- **`qc_review` is preserved across rebuilds.** Losing a reviewer's verdicts to a
  routine re-prep would be unforgivable.

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

Active rules cover the sentinel regression guard, referential integrity, missing
positions, declared value ranges, the quality-code vocabulary, bottle-vs-sensor
calibration offsets, and density inversions.

Parked rules are listed in the Rules tab with the reason they cannot run yet —
bottom-depth-vs-station and the climatology anomaly need the `ctd-qaqc` reference
tables (harmonic climatology, station average depths) imported from the CalCOFI
hydrographic master; spike, pressure monotonicity and up/down disagreement need
`obs_ctd_full` at full scan resolution.

Related: `apps/ctd-viz` (profile inspection — findings deep-link into it),
`CalCOFI/workflows` (the pipeline, the rules, and the plan under `libs/plans/`).
