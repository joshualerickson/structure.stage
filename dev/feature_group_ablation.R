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
