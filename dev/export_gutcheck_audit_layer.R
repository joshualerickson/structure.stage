# Add handout agreement/disagreement attributes to the gut-check GeoPackage.
#
# The original `gt_sf_eda` layer is preserved. This script writes or replaces
# only the derived `gt_sf_eda_handout_audit` layer.

root <- normalizePath(file.path(getwd()), mustWork = TRUE)
gpkg_path <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
audit_path <- file.path(
  root, "outputs", "gutcheck_disagreement", "all_eligible_point_audit.csv"
)
layer_name <- "gt_sf_eda_handout_audit"
eligible_layer_name <- "gt_sf_eda_eligible_3class"

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package `sf` is required.", call. = FALSE)
}

points <- sf::st_read(gpkg_path, layer = "gt_sf_eda", quiet = TRUE)
audit <- utils::read.csv(audit_path, stringsAsFactors = FALSE, check.names = FALSE)

if (anyDuplicated(points$GlobalID) || anyDuplicated(audit$GlobalID)) {
  stop("GlobalID must be unique in both the source layer and audit table.", call. = FALSE)
}

audit_index <- match(points$GlobalID, audit$GlobalID)
if (anyNA(match(audit$GlobalID, points$GlobalID))) {
  stop("Every eligible audit record must have a matching GlobalID in the source layer.", call. = FALSE)
}

# The audit table contains the 528 records eligible for the three-class
# comparison. Indexing expands it to all 1,207 source records; non-eligible
# records retain missing audit fields rather than being removed from the map.
audit <- audit[audit_index, , drop = FALSE]
eligible <- audit$eligible_three_class_comparison

audit_attributes <- data.frame(
  field_stage = audit$field_class,
  rule_stage = audit$rule_class,
  lidar_stage = audit$lidar_class,
  eligible_3class = eligible,
  review_status = audit$review_status,
  comparison_outcome = audit$case_type,
  models_agree = ifelse(eligible, audit$rule_class == audit$lidar_class, NA),
  rule_matches_field = ifelse(eligible, audit$rule_class == audit$field_class, NA),
  lidar_matches_field = ifelse(eligible, audit$lidar_class == audit$field_class, NA),
  rule_to_lidar = audit$prediction_direction,
  field_rule_lidar = audit$field_direction,
  lidar_confidence = audit$lidar_confidence,
  lidar_entropy = audit$lidar_entropy,
  lidar_margin = audit$lidar_margin,
  stringsAsFactors = FALSE
)

points <- cbind(points, audit_attributes)

sf::st_write(
  points,
  dsn = gpkg_path,
  layer = layer_name,
  driver = "GPKG",
  delete_layer = layer_name %in% sf::st_layers(gpkg_path)$name,
  quiet = TRUE
)

eligible_points <- points[!is.na(points$eligible_3class) & points$eligible_3class, , drop = FALSE]
sf::st_write(
  eligible_points,
  dsn = gpkg_path,
  layer = eligible_layer_name,
  driver = "GPKG",
  delete_layer = eligible_layer_name %in% sf::st_layers(gpkg_path)$name,
  quiet = TRUE
)

field_dictionary <- data.frame(
  field = names(audit_attributes),
  meaning = c(
    "Recorded three-class field stage: msf, se, or si.",
    "Existing KNF rule-model stage mapped to msf, se, or si.",
    "LiDAR GBM predicted stage: msf, se, or si.",
    "TRUE when field, rule, and LiDAR stages are all available in the three-class comparison.",
    "Source-record review provenance stratum.",
    "Handout outcome: Both correct; LiDAR correct, rule wrong; Rule correct, LiDAR wrong; Both wrong, same call; or Both wrong, different calls.",
    "TRUE when rule and LiDAR stages are identical, among eligible records.",
    "TRUE when the rule stage equals the recorded field stage, among eligible records.",
    "TRUE when the LiDAR stage equals the recorded field stage, among eligible records.",
    "Rule-to-LiDAR transition, for example Rule msf -> LiDAR se.",
    "Recorded field stage plus rule-to-LiDAR transition.",
    "Largest LiDAR class probability.",
    "Normalized Shannon entropy of the three LiDAR class probabilities; 0 means concentrated and 1 means evenly distributed.",
    "Largest LiDAR class probability minus the second-largest probability."
  ),
  stringsAsFactors = FALSE
)
dictionary_path <- file.path(
  root, "outputs", "gutcheck_disagreement", "gt_sf_eda_handout_audit_field_dictionary.csv"
)
utils::write.csv(field_dictionary, dictionary_path, row.names = FALSE)

message("Wrote layer `", layer_name, "` with ", nrow(points), " records to ", gpkg_path)
message("Wrote layer `", eligible_layer_name, "` with ", nrow(eligible_points), " eligible records to ", gpkg_path)
message("Wrote field dictionary to ", dictionary_path)
