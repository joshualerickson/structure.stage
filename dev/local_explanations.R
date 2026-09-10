# Local explanation workflow for structural-stage GBM predictions.
#
# LIME explanations are local surrogate-model diagnostics. They explain the
# fitted GBM's behavior near chosen records; they do not establish causal
# ecological effects. Use the audit to compare high-confidence correct and
# incorrect predictions before drawing conclusions.

.resolve_lime_model <- function(model) {
  if (base::is.character(model) && base::length(model) == 1L) model <- base::readRDS(model)
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
  base::stop("model must be a structure_gbm bundle, caret GBM train object, or an RDS path.", call. = FALSE)
}

#' Audit records by observed class, predicted class, confidence, and correctness.
audit_structure_predictions <- function(model, data, outcome_column = ".outcome") {
  resolved <- .resolve_lime_model(model)
  if (!base::is.data.frame(data) ||
      base::length(base::setdiff(resolved$predictors, base::names(data)))) {
    base::stop("data must contain all fitted predictors.", call. = FALSE)
  }
  probabilities <- caret::predict.train(
    resolved$model, newdata = data[, resolved$predictors, drop = FALSE], type = "prob"
  )[, base::c("msf", "se", "si"), drop = FALSE]
  predicted <- base::colnames(probabilities)[base::max.col(probabilities, ties.method = "first")]
  observed <- if (outcome_column %in% base::names(data)) base::as.character(data[[outcome_column]]) else NA_character_
  audit <- base::data.frame(
    row_id = base::seq_len(base::nrow(data)), observed_class = observed,
    predicted_class = predicted, confidence = base::apply(probabilities, 1L, base::max),
    p_msf = probabilities$msf, p_se = probabilities$se, p_si = probabilities$si,
    stringsAsFactors = FALSE
  )
  audit$correct <- ifelse(base::is.na(audit$observed_class), NA, audit$observed_class == audit$predicted_class)
  audit
}

#' Build a LIME case audit from outer held-out MSCV predictions.
#'
#' Use this audit, rather than in-sample final-model predictions, when selecting
#' records that the model got right or wrong. The final model can subsequently
#' be explained for those selected records.
audit_structure_mscv_predictions <- function(mscv, data) {
  if (!base::inherits(mscv, "structure_mscv") || !base::is.data.frame(mscv$predictions)) {
    base::stop("mscv must be a structure_mscv result with outer held-out predictions.", call. = FALSE)
  }
  required <- base::c("sample_id", "obs", "pred", "msf", "se", "si")
  if (!base::is.data.frame(data) || !"sample_id" %in% base::names(data) ||
      base::length(base::setdiff(required, base::names(mscv$predictions)))) {
    base::stop("data needs sample_id and MSCV predictions need sample_id, obs, pred, msf, se, and si.", call. = FALSE)
  }
  index <- base::match(mscv$predictions$sample_id, data$sample_id)
  if (base::anyNA(index) || base::anyDuplicated(data$sample_id)) {
    base::stop("MSCV sample_id values must match unique data$sample_id values.", call. = FALSE)
  }
  predictions <- mscv$predictions
  audit <- base::data.frame(
    row_id = index,
    observed_class = base::as.character(predictions$obs),
    predicted_class = base::as.character(predictions$pred),
    confidence = base::apply(predictions[, base::c("msf", "se", "si"), drop = FALSE], 1L, base::max),
    p_msf = predictions$msf, p_se = predictions$se, p_si = predictions$si,
    stringsAsFactors = FALSE
  )
  audit$correct <- audit$observed_class == audit$predicted_class
  audit
}

#' Select representative high-confidence correct and incorrect cases for LIME.
select_lime_cases <- function(audit, n_per_group = 2L) {
  required <- base::c("row_id", "observed_class", "predicted_class", "confidence", "correct")
  if (!base::is.data.frame(audit) || base::length(base::setdiff(required, base::names(audit)))) {
    base::stop("audit must come from audit_structure_predictions().", call. = FALSE)
  }
  if (!base::is.numeric(n_per_group) || base::length(n_per_group) != 1L ||
      base::is.na(n_per_group) || n_per_group < 1L) {
    base::stop("n_per_group must be a positive integer.", call. = FALSE)
  }
  audit <- audit[!base::is.na(audit$correct), , drop = FALSE]
  audit$case_type <- ifelse(audit$correct, "correct", "incorrect")
  audit$group <- base::paste(audit$case_type, audit$observed_class, audit$predicted_class, sep = "__")
  chosen <- base::do.call(base::rbind, base::lapply(base::split(audit, audit$group), function(part) {
    part <- part[base::order(part$confidence, decreasing = TRUE), , drop = FALSE]
    utils::head(part, n_per_group)
  }))
  base::rownames(chosen) <- NULL
  chosen[base::order(chosen$case_type, chosen$observed_class, -chosen$confidence), , drop = FALSE]
}

#' Explain selected records with LIME.
#'
#' The top predicted class is explained by default. Set labels to c("msf", "se",
#' "si") when explanations for every class are required, recognizing that this
#' creates three explanations per selected record.
explain_lime_cases <- function(
    model, data, selected_cases, background_data = NULL, n_features = 6L,
    n_permutations = 2000L, labels = NULL, seed = 20260910L) {
  if (!requireNamespace("lime", quietly = TRUE)) {
    base::stop("Install the lime package first: install.packages('lime').", call. = FALSE)
  }
  resolved <- .resolve_lime_model(model)
  if (!base::is.data.frame(data) || base::length(base::setdiff(resolved$predictors, base::names(data)))) {
    base::stop("data must contain all fitted predictors.", call. = FALSE)
  }
  if (base::is.null(background_data)) background_data <- resolved$model$trainingData
  if (!base::is.data.frame(background_data) ||
      base::length(base::setdiff(resolved$predictors, base::names(background_data)))) {
    base::stop("background_data must contain all fitted predictors; use the model training data.", call. = FALSE)
  }
  if (!base::is.data.frame(selected_cases) || !"row_id" %in% base::names(selected_cases) ||
      base::anyNA(selected_cases$row_id) || base::anyDuplicated(selected_cases$row_id)) {
    base::stop("selected_cases must contain unique row_id values from audit_structure_predictions().", call. = FALSE)
  }
  if (!base::is.numeric(n_features) || !base::is.numeric(n_permutations) ||
      base::length(n_features) != 1L || base::length(n_permutations) != 1L ||
      n_features < 1L || n_permutations < 100L) {
    base::stop("n_features must be positive and n_permutations must be at least 100.", call. = FALSE)
  }
  indices <- base::as.integer(selected_cases$row_id)
  if (base::any(indices < 1L | indices > base::nrow(data))) {
    base::stop("selected_cases$row_id is outside data.", call. = FALSE)
  }
  background <- background_data[, resolved$predictors, drop = FALSE]
  cases <- data[indices, resolved$predictors, drop = FALSE]
  case_ids <- base::paste0("row_", indices)
  base::rownames(cases) <- case_ids
  base::set.seed(seed)
  explainer <- lime::lime(background, resolved$model, bin_continuous = FALSE, use_density = TRUE)
  explanations <- lime::explain(
    cases, explainer, labels = labels,
    n_labels = if (base::is.null(labels)) 1L else NULL,
    n_features = base::as.integer(n_features), n_permutations = base::as.integer(n_permutations)
  )
  explanations$row_id <- base::as.integer(base::sub("row_", "", explanations$case, fixed = TRUE))
  explanations <- base::merge(explanations, selected_cases, by = "row_id", all.x = TRUE, sort = FALSE)
  explanations
}

#' Summarize recurring LIME feature weights by correct versus incorrect cases.
summarize_lime_features <- function(explanations) {
  required <- base::c("case_type", "feature", "feature_weight")
  if (!base::is.data.frame(explanations) || base::length(base::setdiff(required, base::names(explanations)))) {
    base::stop("explanations must come from explain_lime_cases().", call. = FALSE)
  }
  output <- stats::aggregate(
    base::abs(explanations$feature_weight),
    by = base::list(case_type = explanations$case_type, feature = explanations$feature),
    FUN = base::mean
  )
  base::names(output)[3L] <- "mean_absolute_local_weight"
  output[base::order(output$case_type, -output$mean_absolute_local_weight), , drop = FALSE]
}

#' Plot LIME explanations with observed-versus-predicted case status.
#'
#' Blue bars increase the local surrogate's probability for the class being
#' explained; red bars decrease it. Explanation fit is the local surrogate R2,
#' not model accuracy. Cases with a low fit should be interpreted cautiously.
plot_lime_prediction_audit <- function(explanations, max_cases = 12L) {
  required <- base::c(
    "case", "feature", "feature_weight", "label", "label_prob", "model_r2",
    "observed_class", "predicted_class", "confidence", "correct", "case_type"
  )
  if (!base::is.data.frame(explanations) || base::length(base::setdiff(required, base::names(explanations)))) {
    base::stop("explanations must come from explain_lime_cases().", call. = FALSE)
  }
  if (!base::is.numeric(max_cases) || base::length(max_cases) != 1L ||
      base::is.na(max_cases) || max_cases < 1L) {
    base::stop("max_cases must be a positive integer.", call. = FALSE)
  }
  case_info <- base::unique(explanations[, base::c(
    "case", "observed_class", "predicted_class", "confidence", "correct", "case_type", "model_r2"
  ), drop = FALSE])
  case_info <- case_info[base::order(case_info$case_type, case_info$observed_class,
    -case_info$confidence, case_info$case), , drop = FALSE]
  case_info <- utils::head(case_info, base::as.integer(max_cases))
  data <- explanations[explanations$case %in% case_info$case, , drop = FALSE]
  data$direction <- ifelse(data$feature_weight >= 0, "Supports predicted class", "Contradicts predicted class")
  feature_order <- stats::aggregate(
    base::abs(data$feature_weight), by = base::list(feature = data$feature), FUN = base::mean
  )
  feature_order <- feature_order$feature[base::order(feature_order$x, decreasing = TRUE)]
  data$feature <- base::factor(data$feature, levels = base::rev(feature_order))
  status <- ifelse(case_info$correct, "CORRECT", "INCORRECT")
  case_info$case_label <- base::paste0(
    "[", status, "] ", case_info$case,
    "\nObserved: ", case_info$observed_class,
    " | Predicted: ", case_info$predicted_class,
    " | P(pred): ", base::sprintf("%.2f", case_info$confidence),
    "\nLocal explanation fit (R²): ", base::sprintf("%.2f", case_info$model_r2)
  )
  data$case_label <- case_info$case_label[base::match(data$case, case_info$case)]
  data$case_label <- base::factor(data$case_label, levels = case_info$case_label)
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$feature_weight, y = .data$feature, fill = .data$direction
  )) +
    ggplot2::geom_vline(xintercept = 0, color = "grey40", linewidth = 0.35) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(~case_label, ncol = 2, scales = "free_x") +
    ggplot2::scale_fill_manual(values = base::c(
      "Supports predicted class" = "#3B7FB6",
      "Contradicts predicted class" = "#C62828"
    )) +
    ggplot2::labs(
      x = "Local LIME feature weight", y = NULL, fill = NULL,
      title = "Local explanations for correct and incorrect structural-stage predictions",
      caption = base::paste(
        "Blue increases support for the stated predicted class; red decreases it.",
        "Local explanation fit is surrogate R², not predictive accuracy."
      )
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey65"),
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}
