# prep_bathy.R - build cropped GEBCO bathymetry for ctd-viz
#
# usage:
#   Rscript prep_bathy.R
#
# crops the full GEBCO_2025 sub-ice topo/bathy to the CalCOFI AOI and writes
# apps/ctd-viz/data/gebco_calcofi.tif (small, committable). re-run only when
# bathymetry source changes.
#
# full GEBCO_2025 Sub-Ice GeoTIFF (4 GB zip / 8 GB unzipped):
#   https://dap.ceda.ac.uk/bodc/gebco/global/gebco_2025/sub_ice_topography_bathymetry/geotiff/gebco_2025_sub_ice_topo_geotiff.zip?download=1
# to re-materialize gebco_src on a fresh machine, uncomment download block below.

# run from app dir: `cd apps/ctd-viz && Rscript prep_bathy.R`
librarian::shelf(
  fs, glue, terra,
  quiet = TRUE)

# locate this script's directory so paths resolve regardless of cwd
args     <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
app_dir  <- if (length(file_arg) > 0) {
  dirname(normalizePath(file_arg, mustWork = FALSE))
} else getwd()

# source raster (already present locally) ----
gebco_src <- path.expand(
  "~/_big/msens/derived/archive_2026-02-12/gebco_depth.tif")

# # fresh download (commented; uncomment to fetch ~4 GB zip) ----
# zip_url <- paste0(
#   "https://dap.ceda.ac.uk/bodc/gebco/global/gebco_2025/",
#   "sub_ice_topography_bathymetry/geotiff/",
#   "gebco_2025_sub_ice_topo_geotiff.zip?download=1")
# zip_dst <- path.expand("~/_big/gebco_2025/gebco_2025_sub_ice_topo_geotiff.zip")
# dir_create(path_dir(zip_dst))
# if (!file_exists(zip_dst)) download.file(zip_url, zip_dst, mode = "wb")
# unzip(zip_dst, exdir = path_dir(zip_dst))
# # the unzipped dir contains one global GeoTIFF tile; merge if multiple.
# gebco_src <- dir_ls(path_dir(zip_dst), glob = "*.tif")[1]

stopifnot(file.exists(gebco_src))

# CalCOFI AOI (lon_min, lon_max, lat_min, lat_max), standard -180..180 ----
# covers historical CalCOFI patterns from Baja to north of Pt. Conception,
# out to the 400 km offshore extent of the 1950 grid.
aoi <- c(xmin = -126, xmax = -115, ymin = 29, ymax = 36)

out_dir <- file.path(app_dir, "data")
out_tif <- file.path(out_dir, "gebco_calcofi.tif")
dir_create(out_dir)

cat("reading:", gebco_src, "\n")
r <- rast(gebco_src)

# source uses 0..360 longitudes; detect and shift AOI into native coords
src_ext <- ext(r)
shift_to_0_360 <- src_ext$xmin >= 0 && src_ext$xmax > 180
aoi_crop <- if (shift_to_0_360) {
  c(xmin = unname(aoi["xmin"]) + 360,
    xmax = unname(aoi["xmax"]) + 360,
    ymin = unname(aoi["ymin"]),
    ymax = unname(aoi["ymax"]))
} else aoi

cat("cropping to AOI:",
  glue("lon [{aoi_crop['xmin']}, {aoi_crop['xmax']}], ",
       "lat [{aoi_crop['ymin']}, {aoi_crop['ymax']}]"),
  "\n")
r_crop <- crop(
  r,
  ext(aoi_crop["xmin"], aoi_crop["xmax"],
      aoi_crop["ymin"], aoi_crop["ymax"]))

# shift output back to standard -180..180 convention
if (shift_to_0_360) {
  ext(r_crop) <- ext(
    xmin(r_crop) - 360, xmax(r_crop) - 360,
    ymin(r_crop),       ymax(r_crop))
}

cat("writing:", out_tif, "\n")
writeRaster(
  r_crop, out_tif,
  overwrite = TRUE,
  gdal      = c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES"))

info <- file.info(out_tif)
cat("done. output:", out_tif,
  glue("({round(info$size / 1024^2, 1)} MB,",
       " {ncol(r_crop)} x {nrow(r_crop)} cells)"), "\n")
