# Diagnose LiDAR GBM errors by recorded field class and comparison outcome.
#
# Run from the repository root:
#   Rscript dev/stratified_lidar_error_diagnostic.R
#
# `comparison_outcome` is used as a descriptive stratum, not as a predictor of
# LiDAR correctness, because it is partially defined by LiDAR correctness.

root <- normalizePath(getwd(), mustWork = TRUE)
gpkg_path <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
layer_name <- "gt_sf_eda_eligible_3class_landsat_mean5x5"
output_dir <- file.path(root, "outputs", "gutcheck_disagreement", "stratified_lidar_error_diagnostic")
selected_gbm_predictors <- c("zmax", "n_strata_low_mid", "n_gt_6_1", "midstory_mean_degree", "understory_mean_betweenness")

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Packages `sf` and `ggplot2` are required.", call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
points <- sf::st_read(gpkg_path, layer = layer_name, quiet = TRUE)
data <- sf::st_drop_geometry(points)
data$correct <- as.integer(data$lidar_matches_field %in% TRUE)
data$field_stage <- factor(data$field_stage, levels = c("msf", "se", "si"))
data$lidar_stage <- factor(data$lidar_stage, levels = c("msf", "se", "si"))
data$comparison_outcome <- factor(data$comparison_outcome)
data$review_status <- factor(data$review_status)

excluded_numeric <- c(
  selected_gbm_predictors, "msf", "se", "si", "lidar_confidence", "lidar_entropy", "lidar_margin",
  "eligible_3class", "models_agree", "rule_matches_field", "lidar_matches_field", "correct",
  "landsat_mean5x5_valid_cells"
)
candidates <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], excluded_numeric)
candidates <- candidates[vapply(data[candidates], function(x) all(is.finite(x)) && stats::sd(x) > 0, logical(1))]

# Screen omitted metrics separately within each recorded field stage. This is
# the direct question: within a known field class, which unused measurements are
# associated with LiDAR being correct rather than wrong?
screen_one_field <- function(field) {
  subset_data <- data[data$field_stage == field, , drop = FALSE]
  subset_data$review_status <- droplevels(subset_data$review_status)
  base_formula <- if (nlevels(subset_data$review_status) > 1L) correct ~ review_status else correct ~ 1
  metric_formula <- if (nlevels(subset_data$review_status) > 1L) correct ~ review_status + metric_z else correct ~ metric_z
  field_candidates <- candidates[vapply(subset_data[candidates], function(x) all(is.finite(x)) && stats::sd(x) > 0, logical(1))]
  do.call(rbind, lapply(field_candidates, function(metric) {
    subset_data$metric_z <- as.numeric(scale(subset_data[[metric]]))
    fit <- stats::glm(metric_formula, data = subset_data, family = stats::binomial())
    coefficient <- stats::coef(summary(fit))["metric_z", ]
    data.frame(
      field_stage = field,
      n = nrow(subset_data),
      correct_n = sum(subset_data$correct == 1L),
      incorrect_n = sum(subset_data$correct == 0L),
      metric = metric,
      estimate_log_odds = coefficient[["Estimate"]],
      standard_error = coefficient[["Std. Error"]],
      p_value = coefficient[["Pr(>|z|)"]],
      odds_ratio_per_sd = exp(coefficient[["Estimate"]]),
      delta_aic_from_no_metric = stats::AIC(fit) - stats::AIC(stats::glm(base_formula, data = subset_data, family = stats::binomial())),
      stringsAsFactors = FALSE
    )
  }))
}

field_screen <- do.call(rbind, lapply(levels(data$field_stage), screen_one_field))
field_screen$q_value_bh_within_field <- ave(field_screen$p_value, field_screen$field_stage, FUN = function(p) stats::p.adjust(p, method = "BH"))
field_screen <- field_screen[order(field_screen$field_stage, field_screen$q_value_bh_within_field, field_screen$p_value), , drop = FALSE]

# Counts preserve the observed class, LiDAR error direction, and whether the
# rule model also agrees with the field record.
wrong <- data[data$correct == 0L, , drop = FALSE]
wrong_pathways <- as.data.frame(with(wrong, table(field_stage, lidar_stage, comparison_outcome)))
names(wrong_pathways) <- c("field_stage", "lidar_stage", "comparison_outcome", "n")
wrong_pathways <- wrong_pathways[wrong_pathways$n > 0L, , drop = FALSE]

# Median profiles retain the comparison-outcome stratum. These six metrics are
# selected from the pooled screen and include the Landsat neighborhood measure.
profile_metrics <- c("understory_mean_degree", "LAD_z_max", "zskew", "topo_entropy", "n_gt_12_1", "landsat8_summer_fall_diff_mean_5x5")
profile_data <- do.call(rbind, lapply(profile_metrics, function(metric) {
  groups <- split(data, interaction(data$field_stage, data$comparison_outcome, drop = TRUE))
  do.call(rbind, lapply(groups, function(group) {
    data.frame(
      field_stage = as.character(group$field_stage[[1]]),
      comparison_outcome = as.character(group$comparison_outcome[[1]]),
      metric = metric,
      n = nrow(group),
      median = stats::median(group[[metric]]),
      stringsAsFactors = FALSE
    )
  }))
}))
profile_data <- profile_data[profile_data$n >= 5L, , drop = FALSE]

# Within the largest error stratum (recorded SE), distinguish the two possible
# wrong LiDAR calls. MSF and SI observed strata are retained descriptively but
# are too small for a 44-metric direction screen.
se_wrong <- wrong[wrong$field_stage == "se", , drop = FALSE]
se_wrong$predicted_si <- as.integer(se_wrong$lidar_stage == "si")
se_wrong$review_status <- droplevels(se_wrong$review_status)
se_direction_formula <- if (nlevels(se_wrong$review_status) > 1L) predicted_si ~ review_status + metric_z else predicted_si ~ metric_z
se_candidates <- candidates[vapply(se_wrong[candidates], function(x) all(is.finite(x)) && stats::sd(x) > 0, logical(1))]
se_direction <- do.call(rbind, lapply(se_candidates, function(metric) {
  se_wrong$metric_z <- as.numeric(scale(se_wrong[[metric]]))
  fit <- stats::glm(se_direction_formula, data = se_wrong, family = stats::binomial())
  coefficient <- stats::coef(summary(fit))["metric_z", ]
  data.frame(
    observed_field_stage = "se",
    contrast = "LiDAR si versus LiDAR msf among incorrect calls",
    n = nrow(se_wrong),
    lidar_si_n = sum(se_wrong$predicted_si == 1L),
    lidar_msf_n = sum(se_wrong$predicted_si == 0L),
    metric = metric,
    estimate_log_odds = coefficient[["Estimate"]],
    standard_error = coefficient[["Std. Error"]],
    p_value = coefficient[["Pr(>|z|)"]],
    odds_ratio_per_sd = exp(coefficient[["Estimate"]]),
    stringsAsFactors = FALSE
  )
}))
se_direction$q_value_bh <- stats::p.adjust(se_direction$p_value, method = "BH")
se_direction <- se_direction[order(se_direction$q_value_bh, se_direction$p_value), , drop = FALSE]

utils::write.csv(field_screen, file.path(output_dir, "field_stratified_correctness_screen.csv"), row.names = FALSE)
utils::write.csv(wrong_pathways, file.path(output_dir, "wrong_prediction_pathways.csv"), row.names = FALSE)
utils::write.csv(profile_data, file.path(output_dir, "field_outcome_metric_medians.csv"), row.names = FALSE)
utils::write.csv(se_direction, file.path(output_dir, "se_wrong_direction_screen.csv"), row.names = FALSE)

error_plot_data <- aggregate(correct ~ field_stage + lidar_stage, data = data, FUN = length)
names(error_plot_data)[3] <- "n"
error_plot_data <- error_plot_data[error_plot_data$field_stage != error_plot_data$lidar_stage, , drop = FALSE]
error_plot <- ggplot2::ggplot(error_plot_data, ggplot2::aes(x = field_stage, y = n, fill = lidar_stage)) +
  ggplot2::geom_col(position = "stack") +
  ggplot2::labs(title = "LiDAR GBM error pathways by recorded field stage", x = "Recorded field stage", y = "Incorrect points", fill = "LiDAR predicted stage") +
  ggplot2::theme_minimal(base_size = 11)
ggplot2::ggsave(file.path(output_dir, "wrong_prediction_pathways.png"), error_plot, width = 8, height = 5, dpi = 300)

message("Screened ", length(candidates), " omitted metrics within each recorded field stage.")
message("Wrote outputs to ", output_dir)
