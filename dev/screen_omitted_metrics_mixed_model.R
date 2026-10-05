# Explore LiDAR metrics omitted from the final GBM that are associated with errors.
#
# Run from the repository root:
#   Rscript dev/screen_omitted_metrics_mixed_model.R
#
# This is a diagnostic analysis, not a replacement model and not a new measure
# of held-out predictive performance. It tests whether an unused metric is
# associated with the LiDAR GBM matching the recorded field class after
# controlling for field class and review stratum. A 7.5 km spatial-block random
# intercept accounts for broad spatial clustering in the gut-check points.

root <- normalizePath(getwd(), mustWork = TRUE)
gpkg_path <- file.path(root, "outputs", "models", "gt_sf_eda.gpkg")
layer_name <- "gt_sf_eda_eligible_3class_landsat_mean5x5"
output_dir <- file.path(root, "outputs", "gutcheck_disagreement", "omitted_metric_mixed_model")
block_size_m <- 7500
selected_gbm_predictors <- c(
  "zmax", "n_strata_low_mid", "n_gt_6_1",
  "midstory_mean_degree", "understory_mean_betweenness"
)

if (!requireNamespace("sf", quietly = TRUE) || !requireNamespace("lme4", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Packages `sf`, `lme4`, and `ggplot2` are required.", call. = FALSE)
}

tjur_r2 <- function(model, observed) {
  fitted <- stats::fitted(model)
  mean(fitted[observed == 1L]) - mean(fitted[observed == 0L])
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
points <- sf::st_read(gpkg_path, layer = layer_name, quiet = TRUE)
coordinates <- sf::st_coordinates(points)[, c("X", "Y"), drop = FALSE]
data <- sf::st_drop_geometry(points)
data$correct <- as.integer(data$lidar_matches_field %in% TRUE)
data$field_stage <- factor(data$field_stage, levels = c("se", "msf", "si"))
data$review_status <- factor(data$review_status)
data$spatial_block <- factor(paste(
  floor(coordinates[, "X"] / block_size_m),
  floor(coordinates[, "Y"] / block_size_m),
  sep = "_"
))

if (anyNA(data$correct) || anyNA(data$field_stage) || anyNA(data$review_status)) {
  stop("Outcome, field stage, and review status must be complete in the eligible layer.", call. = FALSE)
}

excluded_numeric <- c(
  selected_gbm_predictors,
  "msf", "se", "si", "lidar_confidence", "lidar_entropy", "lidar_margin",
  "eligible_3class", "models_agree", "rule_matches_field", "lidar_matches_field",
  "landsat_mean5x5_valid_cells", "correct"
)
candidate_metrics <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], excluded_numeric)
candidate_metrics <- candidate_metrics[vapply(data[candidate_metrics], function(x) {
  all(is.finite(x)) && stats::sd(x) > 0
}, logical(1))]

control <- lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
baseline_mixed <- lme4::glmer(
  correct ~ field_stage + review_status + (1 | spatial_block),
  data = data,
  family = stats::binomial(),
  control = control
)
use_mixed_model <- !lme4::isSingular(baseline_mixed, tol = 1e-4)
baseline <- if (use_mixed_model) baseline_mixed else stats::glm(
  correct ~ field_stage + review_status,
  data = data,
  family = stats::binomial()
)

fit_error_model <- function(model_data, additional_terms = character()) {
  terms <- c("field_stage", "review_status", additional_terms)
  right_hand_side <- paste(terms, collapse = " + ")
  if (use_mixed_model) right_hand_side <- paste(right_hand_side, "+ (1 | spatial_block)")
  formula <- stats::as.formula(paste("correct ~", right_hand_side))
  if (use_mixed_model) {
    lme4::glmer(formula, data = model_data, family = stats::binomial(), control = control)
  } else {
    stats::glm(formula, data = model_data, family = stats::binomial())
  }
}

screen <- lapply(candidate_metrics, function(metric) {
  model_data <- data
  model_data$metric_z <- as.numeric(scale(model_data[[metric]]))
  fit <- tryCatch(
    fit_error_model(model_data, "metric_z"),
    error = function(error) error
  )
  if (inherits(fit, "error")) {
    return(data.frame(metric = metric, converged = FALSE, error = conditionMessage(fit)))
  }
  coefficient <- stats::coef(summary(fit))["metric_z", ]
  data.frame(
    metric = metric,
    converged = TRUE,
    error = NA_character_,
    estimate_log_odds = coefficient[["Estimate"]],
    standard_error = coefficient[["Std. Error"]],
    z_value = coefficient[["z value"]],
    p_value = coefficient[["Pr(>|z|)"]],
    odds_ratio_per_sd = exp(coefficient[["Estimate"]]),
    aic = stats::AIC(fit),
    delta_aic_from_baseline = stats::AIC(fit) - stats::AIC(baseline),
    tjur_r2 = tjur_r2(fit, model_data$correct),
    singular_fit = if (use_mixed_model) lme4::isSingular(fit, tol = 1e-4) else NA,
    row.names = NULL
  )
})
screen <- do.call(rbind, screen)
screen$q_value_bh <- NA_real_
usable <- screen$converged & !is.na(screen$p_value)
screen$q_value_bh[usable] <- stats::p.adjust(screen$p_value[usable], method = "BH")
screen <- screen[order(screen$q_value_bh, screen$p_value), , drop = FALSE]

# Create an interpretable joint diagnostic from metrics that survive screening,
# pruning variables correlated above |0.70| to avoid a redundant model.
eligible_for_joint <- screen$metric[screen$converged & screen$q_value_bh <= 0.10 & screen$delta_aic_from_baseline <= -2]
joint_metrics <- character()
for (metric in eligible_for_joint) {
  if (length(joint_metrics) == 0L) {
    joint_metrics <- metric
  } else {
    correlation <- stats::cor(data[[metric]], data[joint_metrics], use = "complete.obs")
    if (all(abs(correlation) < 0.70)) joint_metrics <- c(joint_metrics, metric)
  }
}
joint_metrics <- head(joint_metrics, 6L)
utils::write.csv(
  data.frame(metric = joint_metrics, stringsAsFactors = FALSE),
  file.path(output_dir, "joint_metric_selection.csv"),
  row.names = FALSE
)

joint_summary <- data.frame()
joint_fixed_effects <- data.frame()
variance_components <- data.frame()
if (length(joint_metrics) > 0L) {
  joint_data <- data
  for (metric in joint_metrics) joint_data[[paste0(metric, "_z")]] <- as.numeric(scale(joint_data[[metric]]))
  fixed_terms <- paste(paste0("`", joint_metrics, "_z`"), collapse = " + ")
  joint_model <- fit_error_model(joint_data, paste0("`", joint_metrics, "_z`"))
  coefficients <- as.data.frame(stats::coef(summary(joint_model)))
  coefficients$term <- rownames(coefficients)
  rownames(coefficients) <- NULL
  joint_fixed_effects <- coefficients
  if (use_mixed_model) variance_components <- as.data.frame(lme4::VarCorr(joint_model))
  joint_summary <- data.frame(
    model = "joint_omitted_metric_diagnostic",
    n = nrow(joint_data),
    metrics = paste(joint_metrics, collapse = "; "),
    aic = stats::AIC(joint_model),
    delta_aic_from_baseline = stats::AIC(joint_model) - stats::AIC(baseline),
    tjur_r2 = tjur_r2(joint_model, joint_data$correct),
    singular_fit = if (use_mixed_model) lme4::isSingular(joint_model, tol = 1e-4) else NA,
    spatial_blocks = nlevels(data$spatial_block),
    row.names = NULL
  )
}

baseline_summary <- data.frame(
  model = "baseline_error_model",
  n = nrow(data),
  metrics = NA_character_,
  aic = stats::AIC(baseline),
  delta_aic_from_baseline = 0,
  tjur_r2 = tjur_r2(baseline, data$correct),
  singular_fit = if (use_mixed_model) lme4::isSingular(baseline, tol = 1e-4) else NA,
  spatial_blocks = nlevels(data$spatial_block),
  spatial_random_effect_retained = use_mixed_model,
  stringsAsFactors = FALSE
)

utils::write.csv(screen, file.path(output_dir, "univariate_omitted_metric_screen.csv"), row.names = FALSE)
if (nrow(joint_summary) > 0L) joint_summary$spatial_random_effect_retained <- use_mixed_model
utils::write.csv(rbind(baseline_summary, joint_summary), file.path(output_dir, "mixed_model_summary.csv"), row.names = FALSE)
utils::write.csv(joint_fixed_effects, file.path(output_dir, "joint_model_fixed_effects.csv"), row.names = FALSE)
utils::write.csv(variance_components, file.path(output_dir, "joint_model_variance_components.csv"), row.names = FALSE)

plot_data <- subset(screen, converged & !is.na(q_value_bh))
plot_data$metric <- factor(plot_data$metric, levels = rev(plot_data$metric[order(plot_data$estimate_log_odds)]))
plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = estimate_log_odds, y = metric, color = q_value_bh <= 0.10)) +
  ggplot2::geom_vline(xintercept = 0, color = "grey50") +
  ggplot2::geom_errorbar(ggplot2::aes(xmin = estimate_log_odds - 1.96 * standard_error, xmax = estimate_log_odds + 1.96 * standard_error), width = 0, orientation = "y") +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_color_manual(values = c("FALSE" = "grey55", "TRUE" = "#0072B2"), name = "BH q ≤ 0.10") +
  ggplot2::labs(
    title = "Unused metrics associated with LiDAR GBM correctness",
    subtitle = "One standardized metric at a time; controls for field class and review stratum, with 7.5 km spatial-block random intercepts",
    x = "Change in log-odds that LiDAR matches the recorded field call, per 1 SD",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file.path(output_dir, "univariate_omitted_metric_screen.png"), plot, width = 11, height = 12, dpi = 300)

message("Screened ", nrow(screen), " omitted metrics using ", nlevels(data$spatial_block), " spatial blocks.")
message("Wrote outputs to ", output_dir)
