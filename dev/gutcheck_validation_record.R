# Build a transparent diagnostic record for the KNF gut-check comparison.
#
# The comparison evaluates the two map products against recorded field-stage
# calls in a geographically held-out area. Review status is retained as an
# evidence-provenance stratum; its survey/review protocol should accompany any
# reported results.

.gutcheck_three_class <- function(x, field = FALSE) {
  x <- base::trimws(base::as.character(x))
  if (field) {
    out <- base::c(
      "Multistory" = "msf", "Stem Exclusion" = "se", "Stand Initiation" = "si"
    )
  } else {
    out <- base::c(
      "Multistory" = "msf", "Stem Exclusion" = "se", "Stand Initiation" = "si"
    )
  }
  base::unname(out[x])
}

.gutcheck_metrics <- function(observed, predicted, classes = base::c("msf", "se", "si")) {
  keep <- !base::is.na(observed) & !base::is.na(predicted)
  observed <- base::factor(observed[keep], levels = classes)
  predicted <- base::factor(predicted[keep], levels = classes)
  if (!base::length(observed)) {
    return(base::list(
      overall = base::data.frame(n = 0L, accuracy = NA_real_, macro_recall = NA_real_,
        mean_one_vs_rest_balanced_accuracy = NA_real_),
      per_class = base::data.frame()
    ))
  }
  confusion <- base::table(observed = observed, predicted = predicted)
  per_class <- base::do.call(base::rbind, base::lapply(classes, function(class_name) {
    true_positive <- confusion[class_name, class_name]
    false_negative <- base::sum(confusion[class_name, ]) - true_positive
    false_positive <- base::sum(confusion[, class_name]) - true_positive
    true_negative <- base::sum(confusion) - true_positive - false_negative - false_positive
    recall <- if (true_positive + false_negative) true_positive / (true_positive + false_negative) else NA_real_
    specificity <- if (true_negative + false_positive) true_negative / (true_negative + false_positive) else NA_real_
    precision <- if (true_positive + false_positive) true_positive / (true_positive + false_positive) else NA_real_
    f1 <- if (base::is.finite(precision + recall) && precision + recall > 0) {
      2 * precision * recall / (precision + recall)
    } else {
      NA_real_
    }
    base::data.frame(
      class = class_name, support = base::sum(confusion[class_name, ]),
      predicted_n = base::sum(confusion[, class_name]), true_positive = true_positive,
      false_negative = false_negative, false_positive = false_positive,
      recall = recall, specificity = specificity, precision = precision, f1 = f1,
      one_vs_rest_balanced_accuracy = base::mean(base::c(recall, specificity)),
      row.names = NULL
    )
  }))
  base::list(
    confusion = confusion,
    overall = base::data.frame(
      n = base::length(observed), accuracy = base::mean(observed == predicted),
      macro_recall = base::mean(per_class$recall, na.rm = TRUE),
      mean_one_vs_rest_balanced_accuracy = base::mean(per_class$one_vs_rest_balanced_accuracy, na.rm = TRUE),
      row.names = NULL
    ),
    per_class = per_class
  )
}

.write_gutcheck_metrics <- function(data, tier, model_name, prediction_column, output_dir) {
  metrics <- .gutcheck_metrics(data$field_class, data[[prediction_column]])
  overall <- base::cbind(tier = tier, model = model_name, metrics$overall, row.names = NULL)
  per_class <- base::cbind(tier = tier, model = model_name, metrics$per_class, row.names = NULL)
  confusion <- base::as.data.frame(metrics$confusion, stringsAsFactors = FALSE)
  if (base::nrow(confusion)) {
    base::names(confusion) <- base::c("observed", "predicted", "n")
    confusion$tier <- tier
    confusion$model <- model_name
  }
  base::list(overall = overall, per_class = per_class, confusion = confusion)
}

#' Build the KNF gut-check / validation record
#'
#' @param source_dir Directory containing the three supplied comparison files.
#' @param output_dir Directory for generated CSVs and the record README.
#' @return An invisible list containing the joined points, tier definitions, and
#'   comparison metrics.
#'
#' @details `review == TRUE` is retained as a separate desk-review tier. It is
#' not automatically called invalid or non-professional: the available comments
#' show that many such calls were derived from “IL's notes/photos,” rather than
#' an on-site observation. Confirm reviewer identity and protocol before making
#' a field-validation subset.
build_knf_gutcheck_record <- function(
    source_dir = "outputs/models",
    output_dir = "outputs/gutcheck_record"
) {
  survey_path <- base::file.path(source_dir, "gt_wildlife_surveys.shp")
  point_path <- base::file.path(source_dir, "gt_sf_eda.gpkg")
  polygon_path <- base::file.path(source_dir, "koot_model_example.gpkg")
  required <- base::c(survey_path, point_path, polygon_path)
  if (base::any(!base::file.exists(required))) {
    base::stop("The survey, joined-point, and polygon comparison files must be present.", call. = FALSE)
  }
  if (!base::dir.exists(output_dir)) base::dir.create(output_dir, recursive = TRUE)
  survey <- sf::st_read(survey_path, quiet = TRUE)
  points <- sf::st_read(point_path, quiet = TRUE)
  polygons <- sf::st_read(polygon_path, quiet = TRUE)
  if (!"GlobalID" %in% base::names(survey) || !"GlobalID" %in% base::names(points)) {
    base::stop("Both survey and joined-point layers must contain GlobalID.", call. = FALSE)
  }
  survey_ids <- base::as.character(survey$GlobalID)
  point_ids <- base::as.character(points$GlobalID)
  point_data <- sf::st_drop_geometry(points)
  point_data$field_class <- .gutcheck_three_class(point_data$Lynx_Field, field = TRUE)
  point_data$rule_class <- .gutcheck_three_class(point_data$Lynx_Model)
  point_data$lidar_class <- base::as.character(point_data$structure_stage)
  point_data$lidar_class[!point_data$lidar_class %in% base::c("msf", "se", "si")] <- NA_character_
  point_data$review_status <- base::ifelse(
    base::is.na(point_data$review), "not_recorded",
    base::ifelse(point_data$review, "desk_review_flagged", "not_flagged")
  )
  point_data$eligible_three_class_comparison <- !base::is.na(point_data$field_class) &
    !base::is.na(point_data$rule_class) & !base::is.na(point_data$lidar_class)

  tiers <- base::list(
    all_eligible = point_data$eligible_three_class_comparison,
    review_not_flagged = point_data$eligible_three_class_comparison & point_data$review_status == "not_flagged",
    desk_review_flagged = point_data$eligible_three_class_comparison & point_data$review_status == "desk_review_flagged",
    review_not_recorded = point_data$eligible_three_class_comparison & point_data$review_status == "not_recorded"
  )
  all_metrics <- base::lapply(base::names(tiers), function(tier) {
    subset <- point_data[tiers[[tier]], , drop = FALSE]
    base::list(
      .write_gutcheck_metrics(subset, tier, "KNF rule model", "rule_class", output_dir),
      .write_gutcheck_metrics(subset, tier, "LiDAR GBM", "lidar_class", output_dir)
    )
  })
  metric_rows <- base::unlist(all_metrics, recursive = FALSE)
  overall <- base::do.call(base::rbind, base::lapply(metric_rows, `[[`, "overall"))
  per_class <- base::do.call(base::rbind, base::lapply(metric_rows, `[[`, "per_class"))
  confusion_rows <- base::Filter(function(x) base::nrow(x) > 0L, base::lapply(metric_rows, `[[`, "confusion"))
  confusion <- if (base::length(confusion_rows)) base::do.call(base::rbind, confusion_rows) else base::data.frame()

  source_inventory <- base::data.frame(
    dataset = base::c("Survey points", "Joined comparison points", "Final comparison polygons"),
    path = required,
    rows = base::c(base::nrow(survey), base::nrow(points), base::nrow(polygons)),
    role = base::c(
      "Original survey attributes and field-stage calls.",
      "Survey attributes plus LiDAR probabilities/classes and extracted metrics.",
      "Polygon-level rule attributes plus LiDAR class-share and uncertainty summaries."
    ),
    stringsAsFactors = FALSE
  )
  id_audit <- base::data.frame(
    survey_points = base::length(survey_ids), joined_points = base::length(point_ids),
    shared_global_ids = base::length(base::intersect(survey_ids, point_ids)),
    survey_ids_missing_from_joined = base::length(base::setdiff(survey_ids, point_ids)),
    joined_ids_missing_from_survey = base::length(base::setdiff(point_ids, survey_ids)),
    stringsAsFactors = FALSE
  )
  tier_counts <- base::data.frame(
    tier = base::names(tiers), n = base::vapply(tiers, base::sum, base::integer(1)), row.names = NULL
  )
  utils::write.csv(source_inventory, base::file.path(output_dir, "source_inventory.csv"), row.names = FALSE)
  utils::write.csv(id_audit, base::file.path(output_dir, "global_id_audit.csv"), row.names = FALSE)
  utils::write.csv(tier_counts, base::file.path(output_dir, "comparison_tier_counts.csv"), row.names = FALSE)
  utils::write.csv(overall, base::file.path(output_dir, "comparison_overall_metrics.csv"), row.names = FALSE)
  utils::write.csv(per_class, base::file.path(output_dir, "comparison_per_class_metrics.csv"), row.names = FALSE)
  utils::write.csv(confusion, base::file.path(output_dir, "comparison_confusion_matrices.csv"), row.names = FALSE)
  utils::write.csv(point_data, base::file.path(output_dir, "joined_point_audit.csv"), row.names = FALSE)

  lines <- base::c(
    "# KNF gut-check comparison record",
    "",
    "This record compares the rule-model and LiDAR-GBM structural-stage calls with recorded field-stage calls at geographically held-out survey points. The project owner confirmed that these points and their broader area were excluded from LiDAR training. Review status remains an evidence-provenance stratum and its protocol should accompany reported results.",
    "",
    "## Class crosswalk used here",
    "",
    "- Field `Multistory` -> `msf`; `Stem Exclusion` -> `se`; `Stand Initiation` -> `si`.",
    "- Rule model `Multistory` -> `msf`; `Stem Exclusion` -> `se`; `Stand Initiation` -> `si`.",
    "- Field and rule values such as Non Habitat, Early Stand Initiation, Other, and Not Modeled are out of the three-class comparison.",
    "- LiDAR `structure_stage` is already `msf`, `se`, or `si`; missing LiDAR predictions are excluded.",
    "- This comparison assumes the survey/rule value `Multistory` corresponds to the LiDAR model's `msf` (Multi-Story Foraging) target. Confirm that no Multi-Story Non-foraging observations are included in that field before treating this as a final class crosswalk.",
    "",
    "## Review tiers",
    "",
    "- `all_eligible`: all records with a three-class field label and both model classes.",
    "- `review_not_flagged`: same, restricted to `review == FALSE`.",
    "- `desk_review_flagged`: same, restricted to `review == TRUE`; comments indicate review from IL notes/photos for many records.",
    "- `review_not_recorded`: same, with a missing review field.",
    "",
    "Do not call `review_not_flagged` professional field validation until reviewer identity and protocol are confirmed. The tiers make the assumption visible rather than hiding it.",
    "",
    "## Generated files",
    "",
    "- `comparison_overall_metrics.csv`: accuracy, macro recall, and mean one-vs-rest balanced accuracy for each model and tier.",
    "- `comparison_per_class_metrics.csv`: support, recall/sensitivity, specificity, precision, F1, and one-vs-rest balanced accuracy.",
    "- `comparison_confusion_matrices.csv`: every observed versus predicted class count.",
    "- `joined_point_audit.csv`: source attributes, classes, and review tier for traceability.",
    "- `global_id_audit.csv`: confirms whether point layers match by GlobalID."
  )
  base::writeLines(lines, base::file.path(output_dir, "README.md"))
  base::invisible(base::list(
    points = point_data, tiers = tiers, overall = overall, per_class = per_class,
    confusion = confusion, source_inventory = source_inventory, id_audit = id_audit
  ))
}

# Example:
# build_knf_gutcheck_record()
