# CalCOFI Apps
Interactive Shiny web applications for filtering and visualization of CalCOFI data

## Deploy

Apps run on the **`shiny-server`** VM (Google Cloud, project `ucsd-sio-calcofi`),
served by `shiny-server` inside the `rstudio` container (infra lives in
[`CalCOFI/server`](https://github.com/CalCOFI/server)). This repo is cloned on the
host at **`/share/github/CalCOFI/apps`**, and each app directory is symlinked into
shiny-server's app root, e.g. `/srv/shiny-server/ctd → /share/github/CalCOFI/apps/ctd-viz`.
So the canonical URL `https://app.calcofi.io/ctd/` serves the `ctd-viz/` directory
(the URL path is the symlink name, not the folder name).

To deploy after pushing app changes to `main`:

```bash
ssh calcofi                                 # → ssh.calcofi.io (shiny-server VM)
cd /share/github/CalCOFI/apps
git pull                                    # pull the latest app code
touch ctd-viz/restart.txt                   # reload just that app (use the app's folder)
```

`shiny-server` starts a fresh R process for an app when its `restart.txt` mtime
changes, so `touch <app>/restart.txt` reloads only that app on the next request —
no container rebuild needed for R-code changes. Rebuild the `rstudio` image (in
`CalCOFI/server`) only when system or R **package** dependencies change. Large
data (e.g. `ctd-viz`'s prepped DuckDB) lives under `/share/data/…` on the host and
is not in git; regenerate it with the app's `prep_db.R` when the schema changes.

## Interim Apps

Interim apps are publicly visible here:

* [oceano](https://shiny.calcofi.io/oceano/)
* [oceano-dev](https://shiny.calcofi.io/oceano-dev/)
* [oceano-demo](https://shiny.calcofi.io/oceano-demo/)
* [oceano-slide](https://shiny.calcofi.io/oceano-slide/)

## TODO

1. Place w/ `cc_places` and standard + extended, nearshore + offshore
  - index ctd_casts with materialized view
1. Contour Map
    - cache contour ply
    - + button to apply filters
1. Depth

