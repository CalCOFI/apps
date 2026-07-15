# db-viz-cruise

Cross-dataset observation explorer for CalCOFI. Pick a **cruise** and see every
released dataset's sampling stations for it on a **map + table + space-time
plot**, colored by dataset — a quick way to spot coverage gaps and mismatches
across datasets (the kind of thing worth asking a data provider about).

The whole view state round-trips through the **URL query string**, so any view
is a shareable permalink you can paste into a provider question, GitHub issue or
email. This is the feature [`ctd-viz`](../ctd-viz/) lacks.

## URL parameters

| param      | example                         | meaning                                  |
|------------|---------------------------------|------------------------------------------|
| `cruise`   | `?cruise=1998-04-31JD`          | the cruise to load (cruise_key)          |
| `datasets` | `&datasets=calcofi_ctd-cast,swfsc_ichthyo` | restrict to these datasets (omit = all) |
| `id`       | `&id=calcofi_dic:1234`          | pin one observation (`<dataset>:<id>`)   |

The **Copy link** button (and the address bar) always reflect the current view.

## Data

`prep_db.R` builds `db-viz-cruise.duckdb` with one unified `obs` table — one row
per sampling event across all released datasets, projected to a common schema
(`dataset, tbl, id, cruise_key, latitude, longitude, datetime, site_key`), read
via DuckDB `httpfs` from the frozen release **CORE**. It reads the consolidated
`sample` event dimension
(`gs://calcofi-db/ducklake/releases/{version}/parquet/sample.parquet`, version
resolved from `.../releases/latest.txt`), filtered to ROOT events
(`parent_sample_key IS NULL`) so each dataset contributes its coarse station
grain (cast, site, tow, underway, transect …), not child bottles/nets — then
aliases the core columns back to the app's names (`dataset_key → dataset`,
`sample_type → tbl`, `sample_key → id`/`site_key`). This retires the previous
per-dataset UNION of `gs://calcofi-db/ingest/*` parquet.

```bash
Rscript prep_db.R          # build (skips if present)
Rscript prep_db.R TRUE     # force rebuild
```

Output: `/share/data/db-viz-cruise/db-viz-cruise.duckdb` on the server, else
`~/_big/calcofi.org/db-viz-cruise/db-viz-cruise.duckdb` locally.

Datasets covered (root grain in parens): calcofi_bottle (cast), calcofi_ctd-cast
(cast), calcofi_dic (bottle), swfsc_ichthyo (site), cce-lter_euphausiids (tow),
cce-lter_zoodb (tow), cce-lter_zooscan (tow), pic_zooplankton (tow), swfsc_cufes
(underway), calcofi_phyllosoma (tow), calcofi_bird_mammal_census (transect),
calcofi_phytoplankton (region_pool). (calcofi_dic and calcofi_bird_mammal_census
currently carry no `cruise_key` in the release, so they appear in the obs table
but not under a cruise.)

## Run (local)

```r
shiny::runApp("apps/db-viz-cruise")
```

## Deploy on the server (`shiny-server`)

Same pattern as the other data-backed apps (e.g. `ctd-viz`): pull the repo,
build the local DuckDB with `prep_db.R`, then symlink the app into
`/srv/shiny-server` to turn it on at `https://shiny.calcofi.io/db-viz-cruise/`.

```bash
# 1. get the app
cd /share/github/apps && git pull

# 2. build /share/data/db-viz-cruise/db-viz-cruise.duckdb (~1 min).
#    reads the PUBLIC frozen release core over HTTPS (gs://calcofi-db is
#    public-read), so NO service-account credentials are needed — unlike the
#    ERDDAP parquet sync.
cd db-viz-cruise && Rscript prep_db.R          # skips if the db already exists
#   Rscript prep_db.R TRUE                  # force a rebuild AFTER EACH RELEASE
#                                           # to pick up new/updated datasets

# 3. turn it on
sudo ln -s /share/github/apps/db-viz-cruise /srv/shiny-server/db-viz-cruise
```

Re-run step 2 with `TRUE` after every `release_database.qmd` run so the cross-
dataset `obs` table reflects the latest release. The build reads the release
`sample.parquet` (plus `cruise.parquet` / `ship.parquet`) straight from the
version resolved in `gs://calcofi-db/ducklake/releases/latest.txt` via DuckDB
`httpfs`.
