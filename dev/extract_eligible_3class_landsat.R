# Extract Landsat summer-to-fall change values at eligible three-class gut-check points.
#
# Run from the repository root:
#   Rscript dev/extract_eligible_3class_landsat.R
#
# The source eligible layer is preserved. A derived layer named
# `gt_sf_eda_eligible_3class_landsat` is written to the same GeoPackage.

root <- normalizePath(getwd(), mustWork = TRUE)
gpkg_path <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
raster_path <- "/mnt/mordor3/data/josh.erickson/spatial/rasters/landsat8_summer_fall_diff_R1_30m.tif"
source_layer <- "gt_sf_eda_eligible_3class"
output_layer <- "gt_sf_eda_eligible_3class_landsat"

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("terra", quietly = TRUE)) {
  stop("Packages `sf` and `terra` are required.", call. = FALSE)
}
if (!file.exists(raster_path)) {
  stop("Landsat raster was not found: ", raster_path, call. = FALSE)
}

points <- sf::st_read(gpkg_path, layer = source_layer, quiet = TRUE)
if (!all(points$eligible_3class %in% TRUE)) {
  stop("The source layer contains records that are not eligible for the three-class comparison.", call. = FALSE)
}

raster <- terra::rast(raster_path)
points_for_extraction <- sf::st_transform(points, terra::crs(raster, proj = TRUE))
values <- terra::extract(raster, terra::vect(points_for_extraction), ID = FALSE)[[1]]

points$landsat8_summer_fall_diff <- values
points$landsat_extract_status <- ifelse(
  is.na(values),
  "no_data_or_outside_raster",
  "extracted"
)

sf::st_write(
  points,
  dsn = gpkg_path,
  layer = output_layer,
  driver = "GPKG",
  delete_layer = output_layer %in% sf::st_layers(gpkg_path)$name,
  quiet = TRUE
)

summary_path <- file.path(root, "outputs", "gutcheck_disagreement", "eligible_3class_landsat_extraction_summary.csv")
summary <- data.frame(
  source_layer = source_layer,
  output_layer = output_layer,
  raster = basename(raster_path),
  raster_crs = terra::crs(raster, proj = TRUE),
  point_count = nrow(points),
  extracted_count = sum(!is.na(values)),
  missing_or_outside_count = sum(is.na(values)),
  minimum = if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE),
  median = if (all(is.na(values))) NA_real_ else stats::median(values, na.rm = TRUE),
  maximum = if (all(is.na(values))) NA_real_ else max(values, na.rm = TRUE),
  stringsAsFactors = FALSE
)
utils::write.csv(summary, summary_path, row.names = FALSE)

message("Wrote ", output_layer, " with ", nrow(points), " points to ", gpkg_path)
message("Extracted values for ", summary$extracted_count, " points; ", summary$missing_or_outside_count, " were missing or outside the raster.")
message("Wrote extraction summary to ", summary_path)
