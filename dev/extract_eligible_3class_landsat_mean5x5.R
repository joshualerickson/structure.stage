# Extract exact 5 x 5-cell Landsat neighborhood means at eligible gut-check points.
#
# Run from the repository root:
#   Rscript dev/extract_eligible_3class_landsat_mean5x5.R
#
# A 5 x 5 window is centered on the raster cell containing each point. At the
# nominal 30 m raster resolution, it represents a 150 m by 150 m neighborhood.
# The source eligible layer is preserved; this script writes
# `gt_sf_eda_eligible_3class_landsat_mean5x5` to the same GeoPackage.

root <- normalizePath(getwd(), mustWork = TRUE)
gpkg_path <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
raster_path <- "/mnt/mordor3/data/josh.erickson/spatial/rasters/landsat8_summer_fall_diff_R1_30m.tif"
source_layer <- "gt_sf_eda_eligible_3class"
output_layer <- "gt_sf_eda_eligible_3class_landsat_mean5x5"
window_width <- 5L
window_radius <- (window_width - 1L) / 2L

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
coordinates <- sf::st_coordinates(points_for_extraction)[, c("X", "Y"), drop = FALSE]
center_cells <- terra::cellFromXY(raster, coordinates)
row_col <- terra::rowColFromCell(raster, center_cells)

window_cells <- lapply(seq_len(nrow(row_col)), function(index) {
  rows <- seq.int(row_col[index, 1L] - window_radius, row_col[index, 1L] + window_radius)
  cols <- seq.int(row_col[index, 2L] - window_radius, row_col[index, 2L] + window_radius)
  valid_rows <- rows[rows >= 1L & rows <= terra::nrow(raster)]
  valid_cols <- cols[cols >= 1L & cols <= terra::ncol(raster)]
  as.vector(outer(valid_rows, valid_cols, function(row, col) terra::cellFromRowCol(raster, row, col)))
})

all_cells <- unique(unlist(window_cells, use.names = FALSE))
cell_values <- terra::extract(raster, all_cells)[[1]]
names(cell_values) <- as.character(all_cells)

window_values <- lapply(window_cells, function(cells) unname(cell_values[as.character(cells)]))
valid_cell_count <- vapply(window_values, function(values) sum(!is.na(values)), integer(1))
mean5x5 <- vapply(
  window_values,
  function(values) if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE),
  numeric(1)
)

points$landsat8_summer_fall_diff_mean_5x5 <- mean5x5
points$landsat_mean5x5_valid_cells <- valid_cell_count
points$landsat_mean5x5_status <- ifelse(
  valid_cell_count == window_width^2,
  "extracted_all_25_cells",
  ifelse(valid_cell_count > 0L, "extracted_partial_window", "no_data_or_outside_raster")
)

sf::st_write(
  points,
  dsn = gpkg_path,
  layer = output_layer,
  driver = "GPKG",
  delete_layer = output_layer %in% sf::st_layers(gpkg_path)$name,
  quiet = TRUE
)

summary_path <- file.path(root, "outputs", "gutcheck_disagreement", "eligible_3class_landsat_mean5x5_extraction_summary.csv")
summary <- data.frame(
  source_layer = source_layer,
  output_layer = output_layer,
  raster = basename(raster_path),
  window_cells = window_width,
  nominal_window_width_m = window_width * 30L,
  point_count = nrow(points),
  extracted_count = sum(!is.na(mean5x5)),
  complete_window_count = sum(valid_cell_count == window_width^2),
  partial_window_count = sum(valid_cell_count > 0L & valid_cell_count < window_width^2),
  missing_or_outside_count = sum(valid_cell_count == 0L),
  minimum = min(mean5x5, na.rm = TRUE),
  median = stats::median(mean5x5, na.rm = TRUE),
  maximum = max(mean5x5, na.rm = TRUE),
  stringsAsFactors = FALSE
)
utils::write.csv(summary, summary_path, row.names = FALSE)

message("Wrote ", output_layer, " with ", nrow(points), " points to ", gpkg_path)
message("Extracted complete 5 x 5 windows for ", summary$complete_window_count, " points; ", summary$partial_window_count, " windows were partial.")
message("Wrote extraction summary to ", summary_path)
