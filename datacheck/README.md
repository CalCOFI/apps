# datacheck

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

`prep_db.R` builds `datacheck.duckdb` with one unified `obs` table — one row per
sampling event across all 9 released datasets, projected to a common schema
(`dataset, tbl, id, cruise_key, latitude, longitude, datetime, site_key`), read
via DuckDB `httpfs` from the GCS ingest parquet
(`gs://calcofi-db/ingest/{provider}_{dataset}/{table}.parquet`). DIC's
`cruise_key` is joined from the matched bottle cast.

```bash
Rscript prep_db.R          # build (skips if present)
Rscript prep_db.R TRUE     # force rebuild
```

Output: `/share/data/datacheck/datacheck.duckdb` on the server, else
`~/_big/calcofi.org/datacheck/datacheck.duckdb` locally.

Datasets covered: calcofi_bottle (casts), calcofi_ctd-cast (ctd_cast),
calcofi_dic (dic_sample), swfsc_ichthyo (site), cce-lter_euphausiids,
pic_zooplankton, swfsc_cufes, calcofi_phyllosoma, calcofi_bird_mammal_census.
(bird/mammal transects carry no `cruise_key`, so they appear in the obs table
but not under a cruise.)

## Run

```r
shiny::runApp("apps/datacheck")
```
