#' Fit a GBM with a fixed predictor set under spatial resampling
#'
#' Fits and tunes a GBM using only `selected_predictors`. This is used after a
#' predictor set has been fixed outside the resampling being run, or inside an
#' outer training partition after feature selection. It does not select
#' predictors itself.
#'
#' @param data Prepared `structure_data`.
#' @param folds Matching spatial folds from [build_spatial_folds()].
#' @param selected_predictors Nonempty predictor names from `data`'s schema.
#' @inheritParams fit_structure_gbm
#' @return A `structure_gbm` bundle. Its `assessment` field describes the role
#'   supplied by the caller; it is not an independent performance result.
#' @keywords internal
.fit_selected_structure_gbm <- function(data, folds, selected_predictors,
                                        tune_grid, tune_length, metric, seed,
                                        allow_parallel, verbose, git_commit,
                                        assessment = "selection_resampling_only") {
  .validate_folds(data, folds)
  if (!base::is.character(selected_predictors) || !base::length(selected_predictors) ||
      base::anyNA(selected_predictors) || base::anyDuplicated(selected_predictors) ||
      base::length(base::setdiff(selected_predictors, data$schema$name))) {
    base::stop("selected_predictors must be unique names in the predictor schema.", call. = FALSE)
  }
  .integer_scalar(seed, "seed", 0L)
  .integer_scalar(tune_length, "tune_length")
  if (!base::is.null(tune_grid)) {
    required <- base::c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")
    .numeric_columns(tune_grid, required)
    if (!base::nrow(tune_grid) || !base::setequal(base::names(tune_grid), required) ||
        base::any(base::as.matrix(tune_grid) <= 0)) {
      base::stop("tune_grid must have exactly the four positive GBM tuning columns.", call. = FALSE)
    }
  }
  control <- caret::trainControl(
    method = "cv", number = base::length(folds$index), allowParallel = allow_parallel,
    returnResamp = "final", verboseIter = verbose, classProbs = TRUE,
    summaryFunction = structure_summary, index = folds$index,
    indexOut = folds$indexOut, savePredictions = "final"
  )
  model <- withr::with_seed(seed, caret::train(
    x = data$records[, selected_predictors, drop = FALSE], y = data$records$structure_stage,
    method = "gbm", metric = metric, trControl = control, tuneLength = tune_length,
    tuneGrid = tune_grid, verbose = verbose
  ))
  packages <- base::c("structure.stage", "CAST", "caret", "gbm", "sf", "withr")
  versions <- stats::setNames(base::vapply(packages, function(package) {
    base::as.character(utils::packageVersion(package))
  }, base::character(1)), packages)
  base::structure(base::list(
    model = model, selected_predictors = selected_predictors, schema = data$schema,
    class_map = data$class_map, class_levels = data$class_levels, crs = data$crs,
    data_version = data$data_version, preparation = data$preparation,
    audit = data$audit, sample_key = .fold_key(data), folds = folds,
    config = base::list(method = "gbm", metric = metric, seed = seed,
      tune_grid = tune_grid, tune_length = tune_length, allow_parallel = allow_parallel,
      git_commit = git_commit), selection_results = model$results,
    best_tune = model$bestTune, assessment = assessment,
    software = base::list(versions = versions, session = utils::sessionInfo()),
    created_at = base::Sys.time()
  ), class = "structure_gbm")
}

.subset_structure_data <- function(data, rows) {
  .training_data(data)
  if (!base::is.numeric(rows) || !base::length(rows) || base::anyNA(rows) ||
      base::any(rows != base::floor(rows) | rows < 1L | rows > base::nrow(data$records))) {
    base::stop("rows must be valid, nonempty training record indices.", call. = FALSE)
  }
  out <- data
  out$records <- out$records[rows, , drop = FALSE]
  base::rownames(out$records) <- NULL
  out$audit$subset_sample_ids <- out$records$sample_id
  .training_data(out)
  out
}

.prediction_metrics <- function(predictions, class_levels) {
  .columns(predictions, base::c("obs", "pred", class_levels))
  cm <- caret::confusionMatrix(
    base::factor(predictions$pred, levels = class_levels),
    base::factor(predictions$obs, levels = class_levels)
  )
  by_class <- cm$byClass
  if (base::is.null(base::dim(by_class))) by_class <- base::t(by_class)
  base::rownames(by_class) <- class_levels
  probabilities <- base::as.matrix(predictions[, class_levels, drop = FALSE])
  observed_index <- base::match(base::as.character(predictions$obs), class_levels)
  observed_probability <- probabilities[base::cbind(base::seq_len(base::nrow(predictions)), observed_index)]
  one_hot <- base::matrix(0, nrow = base::nrow(predictions), ncol = base::length(class_levels))
  one_hot[base::cbind(base::seq_len(base::nrow(predictions)), observed_index)] <- 1
  overall <- base::data.frame(
    metric = base::c("accuracy", "kappa", "balanced_accuracy", "macro_f1", "log_loss", "brier_score"),
    value = base::c(
      base::unname(cm$overall["Accuracy"]), base::unname(cm$overall["Kappa"]),
      base::mean(by_class[, "Balanced Accuracy"], na.rm = TRUE),
      base::mean(by_class[, "F1"], na.rm = TRUE),
      -base::mean(base::log(base::pmax(observed_probability, .Machine$double.xmin))),
      base::mean(base::rowSums((probabilities - one_hot)^2))
    ), row.names = NULL
  )
  per_class <- base::data.frame(
    class = class_levels, precision = base::unname(by_class[, "Precision"]),
    recall = base::unname(by_class[, "Recall"]), f1 = base::unname(by_class[, "F1"]),
    balanced_accuracy = base::unname(by_class[, "Balanced Accuracy"]), row.names = NULL
  )
  base::list(confusion_matrix = cm$table, overall = overall, per_class = per_class)
}

#' Run nested spatial cross-validation for a structural-stage GBM
#'
#' Produces one outer held-out prediction per sampled location. With
#' `selected_predictors = NULL`, forward feature selection and GBM tuning occur
#' only within every outer training partition. This is the recommended mode for
#' a reportable performance estimate when the candidate variables have not been
#' fixed independently. With an explicit predictor set, only tuning occurs
#' within outer training partitions; use that mode only when the set was chosen
#' independently of these outer samples, and report that distinction.
#'
#' No final model is fit here. Use [fit_final_structure_gbm()] after reviewing
#' outer predictions and selecting a final predictor set.
#'
#' @param data Prepared, optionally sampled `structure_data`.
#' @param outer_folds Matching spatial folds defining held-out assessment areas.
#' @param selected_predictors NULL to perform inner CAST forward selection, or a
#'   fixed nonempty vector of schema predictor names.
#' @param inner_samples_per_cluster Positive target for the inner spatial fold
#'   construction. It is not a spatial distance.
#' @param inner_k Number of inner spatial folds.
#' @param inner_cluster_unit `"polygon"` or `"sample"`; polygon is recommended.
#' @param inner_nstart Number of inner k-means starts.
#' @param cluster_seed,fold_seed,model_seed Nonnegative seeds. The outer fold
#'   number offsets these for independent deterministic inner fits.
#' @param tune_grid,tune_length,metric,allow_parallel,verbose,git_commit Model
#'   arguments forwarded to the fitting functions.
#' @return A `structure_mscv` object with outer held-out `predictions`, aggregate
#'   and fold-level metrics, selected variables, inner models, outer folds, and
#'   configuration. Its performance estimates apply only to stand-derived sample
#'   labels under these spatial folds.
#' @export
run_mscv_gbm <- function(data, outer_folds, selected_predictors = NULL,
                         inner_samples_per_cluster, inner_k = 5L,
                         inner_cluster_unit = base::c("polygon", "sample"),
                         inner_nstart = 1L, cluster_seed = 124L, fold_seed = 1234L,
                         model_seed = 123L, tune_grid = NULL, tune_length = 3L,
                         metric = base::c("Accuracy", "Kappa", "J"),
                         allow_parallel = FALSE, verbose = FALSE,
                         git_commit = NA_character_) {
  .training_data(data)
  .validate_folds(data, outer_folds)
  .integer_scalar(inner_k, "inner_k", 2L)
  .integer_scalar(inner_nstart, "inner_nstart")
  .integer_scalar(cluster_seed, "cluster_seed", 0L)
  .integer_scalar(fold_seed, "fold_seed", 0L)
  .integer_scalar(model_seed, "model_seed", 0L)
  if (!base::is.numeric(inner_samples_per_cluster) ||
      base::length(inner_samples_per_cluster) != 1L ||
      !base::is.finite(inner_samples_per_cluster) || inner_samples_per_cluster <= 0) {
    base::stop("inner_samples_per_cluster must be positive and finite.", call. = FALSE)
  }
  metric <- base::match.arg(metric)
  inner_cluster_unit <- base::match.arg(inner_cluster_unit)
  nested_selection <- base::is.null(selected_predictors)
  if (!nested_selection && (!base::is.character(selected_predictors) ||
      !base::length(selected_predictors) || base::anyNA(selected_predictors) ||
      base::anyDuplicated(selected_predictors) ||
      base::length(base::setdiff(selected_predictors, data$schema$name)))) {
    base::stop("selected_predictors must be NULL or unique predictor schema names.", call. = FALSE)
  }
  model_rows <- base::vector("list", base::length(outer_folds$index))
  prediction_rows <- base::vector("list", base::length(outer_folds$index))
  for (i in base::seq_along(outer_folds$index)) {
    training <- .subset_structure_data(data, outer_folds$index[[i]])
    assessment <- .subset_structure_data(data, outer_folds$indexOut[[i]])
    inner_folds <- build_spatial_folds(
      training, samples_per_cluster = inner_samples_per_cluster, k = inner_k,
      cluster_unit = inner_cluster_unit, cluster_seed = cluster_seed + i,
      fold_seed = fold_seed + i, nstart = inner_nstart
    )
    if (nested_selection) {
      fitted <- fit_structure_gbm(
        training, inner_folds, tune_grid = tune_grid, tune_length = tune_length,
        metric = metric, seed = model_seed + i, allow_parallel = allow_parallel,
        verbose = verbose, git_commit = git_commit
      )
    } else {
      fitted <- .fit_selected_structure_gbm(
        training, inner_folds, selected_predictors, tune_grid, tune_length,
        metric, model_seed + i, allow_parallel, verbose, git_commit,
        assessment = "inner_tuning_for_outer_assessment"
      )
    }
    probabilities <- predict_structure_gbm(fitted, assessment$records, assessment$schema, "prob")
    classes <- base::factor(base::colnames(probabilities)[base::max.col(probabilities, ties.method = "first")],
      levels = data$class_levels)
    prediction_rows[[i]] <- base::data.frame(
      sample_id = assessment$records$sample_id, polygon_id = assessment$records$polygon_id,
      source_id = assessment$records$source_id, acquisition_id = assessment$records$acquisition_id,
      outer_fold = i, obs = assessment$records$structure_stage, pred = classes,
      probabilities, check.names = FALSE, row.names = NULL
    )
    model_rows[[i]] <- fitted
  }
  predictions <- base::do.call(base::rbind, prediction_rows)
  if (base::anyDuplicated(predictions$sample_id) ||
      !base::setequal(predictions$sample_id, data$records$sample_id)) {
    base::stop("Outer assessment did not produce exactly one prediction per sample.", call. = FALSE)
  }
  aggregate_metrics <- .prediction_metrics(predictions, data$class_levels)
  fold_metrics <- base::do.call(base::rbind, base::lapply(base::seq_along(model_rows), function(i) {
    metrics <- .prediction_metrics(predictions[predictions$outer_fold == i, , drop = FALSE], data$class_levels)
    base::cbind(outer_fold = i, metrics$overall, row.names = NULL)
  }))
  selected <- base::data.frame(
    outer_fold = base::seq_along(model_rows),
    selected_predictors = base::I(base::lapply(model_rows, function(model) model$selected_predictors)),
    char_vars = base::vapply(model_rows, function(model) {
      base::paste(model$selected_predictors, collapse = ", ")
    }, base::character(1)), row.names = NULL
  )
  base::structure(base::list(
    predictions = predictions, metrics = aggregate_metrics, fold_metrics = fold_metrics,
    selected = selected, outer_models = model_rows, outer_folds = outer_folds,
    config = base::list(nested_selection = nested_selection,
      selected_predictors = selected_predictors,
      inner_samples_per_cluster = inner_samples_per_cluster, inner_k = inner_k,
      inner_cluster_unit = inner_cluster_unit, inner_nstart = inner_nstart,
      cluster_seed = cluster_seed, fold_seed = fold_seed,
      model_seed = model_seed, tune_grid = tune_grid, tune_length = tune_length,
      metric = metric, git_commit = git_commit),
    assessment = if (nested_selection) "nested_outer_spatial_cv" else "fixed_predictors_outer_spatial_cv",
    data_version = data$data_version, class_levels = data$class_levels,
    sample_key = .fold_key(data), created_at = base::Sys.time()
  ), class = "structure_mscv")
}

#' Fit the final all-data structural-stage GBM
#'
#' Fits a deployment model using every supplied record after predictors have
#' been chosen. Its spatial resampling is used only to choose GBM tuning values;
#' report performance from [run_mscv_gbm()] instead. The returned model is fit
#' on all records by caret after tuning and is suitable for guarded prediction.
#'
#' @param data Prepared, optionally sampled `structure_data` used for final fit.
#' @param selected_predictors Final fixed predictor names. Do not derive these by
#'   selecting on outer held-out predictions.
#' @param tuning_folds Spatial folds for tuning on all final-training data.
#' @inheritParams fit_structure_gbm
#' @return A `structure_gbm` deployment bundle with `assessment` set to
#'   `"final_model_not_performance_estimate"`.
#' @export
fit_final_structure_gbm <- function(data, selected_predictors, tuning_folds,
                                    tune_grid = NULL, tune_length = 3L,
                                    metric = base::c("Accuracy", "Kappa", "J"),
                                    seed = 123L, allow_parallel = FALSE,
                                    verbose = FALSE, git_commit = NA_character_) {
  metric <- base::match.arg(metric)
  .fit_selected_structure_gbm(
    data, tuning_folds, selected_predictors, tune_grid, tune_length, metric,
    seed, allow_parallel, verbose, git_commit,
    assessment = "final_model_not_performance_estimate"
  )
}

#' Save nested spatial-CV results as an auditable result set
#'
#' Writes the complete result object plus flat CSV files for held-out
#' predictions, aggregate metrics, per-class metrics, fold metrics, the
#' confusion matrix, and selected predictors. Existing output files cause an
#' error; this avoids silently replacing a reported result set.
#'
#' @param object A `structure_mscv` from [run_mscv_gbm()].
#' @param output_dir Existing directory for output files.
#' @param prefix Nonempty file-name prefix identifying the run.
#' @return A named character vector of written paths, invisibly.
#' @export
save_mscv_results <- function(object, output_dir, prefix) {
  if (!base::inherits(object, "structure_mscv")) {
    base::stop("object must be a structure_mscv result.", call. = FALSE)
  }
  .scalar_text(output_dir, "output_dir")
  .scalar_text(prefix, "prefix")
  if (!base::dir.exists(output_dir)) {
    base::stop("output_dir must already exist.", call. = FALSE)
  }
  if (base::grepl("[/\\\\]", prefix)) {
    base::stop("prefix must be a file name, not a path.", call. = FALSE)
  }
  paths <- base::file.path(output_dir, base::paste0(prefix, base::c(
    ".rds", "_predictions.csv", "_metrics.csv", "_per_class.csv",
    "_fold_metrics.csv", "_confusion_matrix.csv", "_selected_predictors.csv"
  )))
  base::names(paths) <- base::c(
    "result", "predictions", "metrics", "per_class", "fold_metrics",
    "confusion_matrix", "selected_predictors"
  )
  if (base::any(base::file.exists(paths))) {
    base::stop("One or more result paths already exist; choose a new prefix.", call. = FALSE)
  }
  confusion <- base::as.data.frame.matrix(object$metrics$confusion_matrix)
  confusion <- base::data.frame(observed_class = base::rownames(confusion), confusion,
    row.names = NULL, check.names = FALSE)
  selected <- object$selected[, base::c("outer_fold", "char_vars"), drop = FALSE]
  base::saveRDS(object, paths[["result"]])
  utils::write.csv(object$predictions, paths[["predictions"]], row.names = FALSE)
  utils::write.csv(object$metrics$overall, paths[["metrics"]], row.names = FALSE)
  utils::write.csv(object$metrics$per_class, paths[["per_class"]], row.names = FALSE)
  utils::write.csv(object$fold_metrics, paths[["fold_metrics"]], row.names = FALSE)
  utils::write.csv(confusion, paths[["confusion_matrix"]], row.names = FALSE)
  utils::write.csv(selected, paths[["selected_predictors"]], row.names = FALSE)
  base::invisible(paths)
}
