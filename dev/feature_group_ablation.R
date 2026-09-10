# Held-out feature-group ablation for the structural-stage GBM.
#
# This is a practical model-utility screen: each candidate removes a whole
# feature group, refits the GBM with the full model's tuned hyperparameters,
# and evaluates on the same held-out records. Repeat spatial resampling is
# required before treating a small difference as a final performance claim.

.ablation_model <- function(model) {
  if (base::inherits(model, "structure_gbm")) {
    return(base::list(model = model$model, predictors = model$selected_predictors))
  }
  if (base::inherits(model, "train") && base::identical(model$method, "gbm")) {
    predictors <- model$coefnames
    if (base::is.null(predictors) || !base::length(predictors)) {
      predictors <- base::setdiff(base::names(model$trainingData), ".outcome")
    }
    return(base::list(model = model, predictors = predictors))
  }
  base::stop("model must be a structure_gbm bundle or caret GBM train object.", call. = FALSE)
}

.ablation_metrics <- function(observed, probabilities) {
  classes <- base::c("msf", "se", "si")
  predicted <- classes[base::max.col(probabilities[, classes, drop = FALSE], ties.method = "first")]
  recall <- base::vapply(classes, function(class_name) {
    rows <- observed == class_name
    base::mean(predicted[rows] == class_name)
  }, numeric(1L))
  precision <- base::vapply(classes, function(class_name) {
    rows <- predicted == class_name
    if (!base::any(rows)) return(0)
    base::mean(observed[rows] == class_name)
  }, numeric(1L))
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  probability_observed <- probabilities[cbind(base::seq_along(observed), base::match(observed, classes))]
  base::data.frame(
    balanced_accuracy = base::mean(recall),
    macro_f1 = base::mean(f1),
    log_loss = -base::mean(base::log(base::pmax(probability_observed, 1e-15))),
    recall_msf = recall[["msf"]], recall_se = recall[["se"]], recall_si = recall[["si"]],
    row.names = NULL
  )
}

#' Compare the full model against feature-group-drop GBMs on held-out records.
#'
#' @param model A final structure_gbm bundle or caret GBM train object.
#' @param heldout_data Records not used to fit model, including outcome_column.
#' @param training_data Training records. NULL uses model$trainingData.
#' @param outcome_column Class-label column in training_data and heldout_data.
#' @param feature_groups Named list of selected predictor groups to remove.
#' @param seed Random seed used for each refit.
#' @return List with performance and fitted candidate models.
run_feature_group_ablation <- function(
    model, heldout_data, training_data = NULL, outcome_column = ".outcome",
    feature_groups = base::list(
      understory = "understory_mean_betweenness",
      midstory_vertical = base::c("midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid"),
      understory_midstory = base::c(
        "understory_mean_betweenness", "midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid"
      )
    ),
    seed = 20260910L) {
  resolved <- .ablation_model(model)
  if (base::is.null(training_data)) training_data <- resolved$model$trainingData
  required <- base::c(resolved$predictors, outcome_column)
  if (!base::is.data.frame(training_data) || !base::is.data.frame(heldout_data) ||
      base::length(base::setdiff(required, base::names(training_data))) ||
      base::length(base::setdiff(required, base::names(heldout_data)))) {
    base::stop("training_data and heldout_data must contain the fitted predictors and outcome_column.", call. = FALSE)
  }
  classes <- base::c("msf", "se", "si")
  training_y <- base::factor(base::as.character(training_data[[outcome_column]]), levels = classes)
  heldout_y <- base::as.character(heldout_data[[outcome_column]])
  if (base::anyNA(training_y) || base::any(!heldout_y %in% classes)) {
    base::stop("outcome_column must contain only msf, se, and si.", call. = FALSE)
  }
  if (!base::is.list(feature_groups) || base::is.null(base::names(feature_groups)) ||
      base::any(!nzchar(base::names(feature_groups)))) {
    base::stop("feature_groups must be a named list.", call. = FALSE)
  }
  variants <- base::c(base::list(full = character()), feature_groups)
  models <- base::vector("list", base::length(variants))
  performance <- base::vector("list", base::length(variants))
  base::names(models) <- base::names(variants)
  for (index in base::seq_along(variants)) {
    removed <- base::intersect(resolved$predictors, variants[[index]])
    predictors <- base::setdiff(resolved$predictors, removed)
    if (!base::length(predictors)) base::stop("A feature group removed every predictor.", call. = FALSE)
    # caret's GBM interface errors for a one-predictor multiclass fit. Keep the
    # valid group-drop comparisons running and report this zmax-only candidate
    # explicitly instead of aborting the full ablation.
    if (base::length(predictors) < 2L) {
      performance[[index]] <- base::data.frame(
        candidate = base::names(variants)[[index]],
        predictors_removed = base::paste(removed, collapse = ", "),
        predictors_retained = base::length(predictors),
        status = "not_fitted_caret_gbm_requires_at_least_two_predictors",
        balanced_accuracy = NA_real_, macro_f1 = NA_real_, log_loss = NA_real_,
        recall_msf = NA_real_, recall_se = NA_real_, recall_si = NA_real_,
        row.names = NULL
      )
      models[[index]] <- NULL
      next
    }
    base::set.seed(seed)
    fitted <- caret::train(
      x = training_data[, predictors, drop = FALSE], y = training_y,
      method = "gbm", tuneGrid = resolved$model$bestTune,
      trControl = caret::trainControl(method = "none", classProbs = TRUE),
      verbose = FALSE
    )
    probabilities <- caret::predict.train(
      fitted, newdata = heldout_data[, predictors, drop = FALSE], type = "prob"
    )[, classes, drop = FALSE]
    metrics <- .ablation_metrics(heldout_y, probabilities)
    performance[[index]] <- base::data.frame(
      candidate = base::names(variants)[[index]],
      predictors_removed = base::paste(removed, collapse = ", "),
      predictors_retained = base::length(predictors),
      status = "fitted",
      metrics,
      row.names = NULL
    )
    models[[index]] <- fitted
  }
  performance <- base::do.call(base::rbind, performance)
  full <- performance[performance$candidate == "full", , drop = FALSE]
  for (metric in base::c("balanced_accuracy", "macro_f1", "log_loss", "recall_msf", "recall_se", "recall_si")) {
    performance[[base::paste0("delta_", metric)]] <- performance[[metric]] - full[[metric]]
  }
  base::list(performance = performance, models = models, assessment = "single_heldout_group_ablation")
}

#' Plot held-out feature-group ablation performance changes.
#'
#' Negative bars mean that removing the feature group reduced skill or worsened
#' log loss relative to the refitted full model.
plot_feature_group_ablation <- function(
    ablation,
    metrics = base::c(
      "balanced_accuracy", "macro_f1", "recall_msf", "recall_se", "recall_si", "log_loss"
    )) {
  if (!base::is.list(ablation) || !base::is.data.frame(ablation$performance)) {
    base::stop("ablation must come from run_feature_group_ablation().", call. = FALSE)
  }
  performance <- ablation$performance
  required <- base::c("candidate", "status", metrics, base::paste0("delta_", metrics))
  if (base::length(base::setdiff(required, base::names(performance)))) {
    base::stop("ablation performance is missing requested metrics.", call. = FALSE)
  }
  performance <- performance[performance$status == "fitted" & performance$candidate != "full", , drop = FALSE]
  if (!base::nrow(performance)) {
    base::stop("No fitted feature-group ablations are available to plot.", call. = FALSE)
  }
  labels <- base::c(
    understory = "Drop understory", midstory_vertical = "Drop midstory/vertical",
    understory_midstory = "Drop understory + midstory"
  )
  candidate_labels <- labels[performance$candidate]
  candidate_labels[base::is.na(candidate_labels)] <- performance$candidate[base::is.na(candidate_labels)]
  long <- base::data.frame(
    candidate = base::rep(candidate_labels, times = base::length(metrics)),
    metric = base::rep(metrics, each = base::nrow(performance)),
    change = base::unlist(performance[, base::paste0("delta_", metrics), drop = FALSE], use.names = FALSE),
    row.names = NULL
  )
  # Higher log loss is worse, so reverse its delta to make every negative bar
  # indicate a performance loss after dropping a feature group.
  long$change[long$metric == "log_loss"] <- -long$change[long$metric == "log_loss"]
  metric_labels <- base::c(
    balanced_accuracy = "Balanced accuracy", macro_f1 = "Macro F1",
    recall_msf = "MSF recall", recall_se = "SE recall", recall_si = "SI recall",
    log_loss = "Log loss (reversed)"
  )
  long$metric <- base::factor(long$metric, levels = metrics, labels = metric_labels[metrics])
  long$candidate <- base::factor(long$candidate, levels = base::unique(candidate_labels))
  ggplot2::ggplot(long, ggplot2::aes(x = .data$candidate, y = .data$change, fill = .data$candidate)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 3) +
    ggplot2::scale_fill_manual(values = base::c(
      "Drop understory" = "#0072B2",
      "Drop midstory/vertical" = "#D55E00",
      "Drop understory + midstory" = "#7A5195"
    )) +
    ggplot2::labs(
      x = NULL, y = "Change from refitted full model",
      title = "Held-out feature-group ablation",
      caption = "Negative values mean lower skill or higher log loss after removing the feature group."
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey45"),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}

#' Save the held-out feature-group ablation plot as a PNG.
#'
#' @inheritParams plot_feature_group_ablation
#' @param path PNG file path to create.
#' @param width,height Output dimensions in inches.
#' @param dpi Output resolution.
#' @return The normalized output path, invisibly.
save_feature_group_ablation_plot <- function(
    ablation, path, width = 11, height = 7, dpi = 400) {
  if (!base::dir.exists(base::dirname(path))) base::dir.create(base::dirname(path), recursive = TRUE)
  figure <- plot_feature_group_ablation(ablation)
  ggplot2::ggsave(path, plot = figure, width = width, height = height, dpi = dpi, bg = "white")
  base::normalizePath(path, winslash = "/", mustWork = FALSE)
}
