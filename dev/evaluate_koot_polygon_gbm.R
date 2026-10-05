# Compare polygon-level LiDAR GBM and rule-model calls with eligible field points.
#
# Run from the repository root:
#   Rscript dev/evaluate_koot_polygon_gbm.R
#
# The polygon-level LiDAR class is `winner` in koot_model_example.gpkg: the
# structural stage with the largest summarized LiDAR share in that polygon.
# Performance is reported only for points that intersect exactly one polygon.

root <- normalizePath(getwd(), mustWork = TRUE)
points_gpkg <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
polygons_gpkg <- file.path(root, "outputs", "models", "koot_model_example.gpkg")
points_layer <- "gt_sf_eda_eligible_3class"
polygons_layer <- "koot_model_example"
output_layer <- "gt_sf_eda_eligible_3class_koot_polygon"
output_dir <- file.path(root, "outputs", "gutcheck_disagreement", "koot_polygon_performance")
classes <- c("msf", "se", "si")

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package `sf` is required.", call. = FALSE)
}

map_rule_stage <- function(x) {
  mapping <- c(
    "Multi-Story" = "msf",
    "Stem Exclusion" = "se",
    "Stand Initiation" = "si"
  )
  unname(mapping[trimws(as.character(x))])
}

classification_metrics <- function(observed, predicted, model_name) {
  observed <- factor(observed, levels = classes)
  predicted <- factor(predicted, levels = classes)
  confusion <- table(observed = observed, predicted = predicted)
  total <- sum(confusion)
  true_positive <- diag(confusion)
  false_negative <- rowSums(confusion) - true_positive
  false_positive <- colSums(confusion) - true_positive
  true_negative <- total - true_positive - false_negative - false_positive
  recall <- true_positive / (true_positive + false_negative)
  specificity <- true_negative / (true_negative + false_positive)
  precision <- ifelse(true_positive + false_positive == 0, NA_real_, true_positive / (true_positive + false_positive))
  f1 <- ifelse(is.na(precision) | precision + recall == 0, NA_real_, 2 * precision * recall / (precision + recall))
  per_class <- data.frame(
    model = model_name,
    class = classes,
    support = rowSums(confusion),
    true_positive = true_positive,
    false_negative = false_negative,
    false_positive = false_positive,
    true_negative = true_negative,
    sensitivity = recall,
    specificity = specificity,
    balanced_accuracy = (recall + specificity) / 2,
    precision = precision,
    f1 = f1,
    row.names = NULL
  )
  overall <- data.frame(
    model = model_name,
    n = total,
    accuracy = sum(true_positive) / total,
    macro_recall = mean(recall, na.rm = TRUE),
    mean_balanced_accuracy = mean(per_class$balanced_accuracy, na.rm = TRUE),
    macro_f1 = mean(f1, na.rm = TRUE),
    row.names = NULL
  )
  list(confusion = confusion, per_class = per_class, overall = overall)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
points <- sf::st_read(points_gpkg, layer = points_layer, quiet = TRUE)
polygons <- sf::st_read(polygons_gpkg, layer = polygons_layer, quiet = TRUE)

if (!all(points$eligible_3class %in% TRUE)) {
  stop("Point layer must contain only eligible three-class records.", call. = FALSE)
}
if (sf::st_crs(points) != sf::st_crs(polygons)) {
  polygons <- sf::st_transform(polygons, sf::st_crs(points))
}

hits <- sf::st_intersects(points, polygons)
hit_count <- lengths(hits)
polygon_index <- vapply(hits, function(index) if (length(index) == 1L) index else NA_integer_, integer(1))
polygon_attributes <- sf::st_drop_geometry(polygons)[polygon_index, , drop = FALSE]

points$koot_polygon_join_status <- ifelse(
  hit_count == 0L,
  "no_intersecting_polygon",
  ifelse(hit_count == 1L, "matched_one_polygon", "multiple_intersecting_polygons")
)
points$koot_polygon_match_count <- hit_count
points$koot_fid_lynxha <- polygon_attributes$fid_lynxha
points$koot_forest_id <- polygon_attributes$forest_id
points$koot_rule_structural <- as.character(polygon_attributes$structural)
points$koot_rule_stage <- map_rule_stage(polygon_attributes$structural)
points$koot_gbm_winner <- as.character(polygon_attributes$winner)
points$koot_gbm_msf_share <- polygon_attributes$msf_share
points$koot_gbm_se_share <- polygon_attributes$se_share
points$koot_gbm_si_share <- polygon_attributes$si_share
points$koot_gbm_margin <- polygon_attributes$margin
points$koot_gbm_entropy_norm <- polygon_attributes$vote_entropy_norm

matched <- points$koot_polygon_join_status == "matched_one_polygon"
gbm_polygon_available <- matched & points$koot_gbm_winner %in% classes
rule_polygon_available <- matched & points$koot_rule_stage %in% classes

point_gbm <- classification_metrics(
  points$field_stage[gbm_polygon_available],
  points$lidar_stage[gbm_polygon_available],
  "LiDAR GBM point prediction (matched polygon subset)"
)
polygon_gbm <- classification_metrics(
  points$field_stage[gbm_polygon_available],
  points$koot_gbm_winner[gbm_polygon_available],
  "LiDAR GBM polygon winner"
)
polygon_rule <- classification_metrics(
  points$field_stage[rule_polygon_available],
  points$koot_rule_stage[rule_polygon_available],
  "KNF rule-model polygon stage"
)

overall <- do.call(rbind, list(point_gbm$overall, polygon_gbm$overall, polygon_rule$overall))
per_class <- do.call(rbind, list(point_gbm$per_class, polygon_gbm$per_class, polygon_rule$per_class))
confusion <- do.call(rbind, list(
  transform(as.data.frame(point_gbm$confusion), model = point_gbm$overall$model),
  transform(as.data.frame(polygon_gbm$confusion), model = polygon_gbm$overall$model),
  transform(as.data.frame(polygon_rule$confusion), model = polygon_rule$overall$model)
))
join_summary <- data.frame(
  join_status = c("matched_one_polygon", "no_intersecting_polygon", "multiple_intersecting_polygons"),
  points = c(sum(matched), sum(hit_count == 0L), sum(hit_count > 1L)),
  row.names = NULL
)

utils::write.csv(overall, file.path(output_dir, "polygon_performance_overall.csv"), row.names = FALSE)
utils::write.csv(per_class, file.path(output_dir, "polygon_performance_per_class.csv"), row.names = FALSE)
utils::write.csv(confusion, file.path(output_dir, "polygon_performance_confusion.csv"), row.names = FALSE)
utils::write.csv(join_summary, file.path(output_dir, "polygon_join_summary.csv"), row.names = FALSE)

sf::st_write(
  points,
  dsn = points_gpkg,
  layer = output_layer,
  driver = "GPKG",
  delete_layer = output_layer %in% sf::st_layers(points_gpkg)$name,
  quiet = TRUE
)

message("Wrote ", output_layer, " to ", points_gpkg)
message("Matched one polygon: ", sum(matched), "; no polygon: ", sum(hit_count == 0L), "; multiple polygons: ", sum(hit_count > 1L))
message("Wrote performance tables to ", output_dir)
