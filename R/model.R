#' Legacy multiclass selection metrics for caret
#'
#' Calculates overall Accuracy, Kappa, and mean classwise Youden J. These are
#' selection metrics against stand-derived labels, not pixel-level accuracy.
#' Undefined classwise J values are omitted, matching the original script.
#' @param data Data frame with factor columns `obs` and `pred`.
#' @param lev Explicit class levels supplied by caret.
#' @param model Unused caret compatibility argument.
#' @return Named numeric vector: `Accuracy`, `Kappa`, `J`.
#' @export
structure_summary <- function(data, lev = base::levels(data$obs), model = NULL) {
  .columns(data, base::c("obs", "pred"))
  if (base::is.null(lev) || base::length(lev) < 2L ||
      base::anyNA(data$obs) || base::anyNA(data$pred) ||
      base::any(!data$obs %in% lev) || base::any(!data$pred %in% lev)) {
    base::stop("obs and pred must contain known, nonmissing classes.", call. = FALSE)
  }
  cm <- caret::confusionMatrix(
    base::factor(data$pred, levels = lev), base::factor(data$obs, levels = lev))
  by_class <- cm$byClass
  if (base::is.null(base::dim(by_class))) by_class <- base::t(by_class)
  j <- by_class[, "Sensitivity"] + by_class[, "Specificity"] - 1
  base::c(Accuracy = base::unname(cm$overall["Accuracy"]),
          Kappa = base::unname(cm$overall["Kappa"]), J = base::mean(j, na.rm = TRUE))
}

#' Fit a spatial forward-selected GBM
#'
#' Uses [CAST::ffs()] with caret's GBM engine and explicitly audited spatial
#' resamples. The returned GBM is refitted by caret on all supplied training
#' records. Results and saved resample predictions are used for selection and
#' tuning; they are not an independent outer evaluation. For unbiased assessment,
#' call this function only on each outer training partition and predict its
#' held-out data. No outer evaluation or automatic model comparison is performed.
#'
#' @param data Prepared (optionally sampled) `structure_data`.
#' @param folds A matching object from [build_spatial_folds()].
#' @param tune_grid NULL for caret's generated grid or a data frame containing
#'   `n.trees`, `interaction.depth`, `shrinkage`, and `n.minobsinnode`.
#' @param tune_length Positive size of caret's generated grid when no grid is given.
#' @param metric Selection metric: `"Accuracy"`, `"Kappa"`, or `"J"`.
#' @param seed Nonnegative integer model seed. The caller's RNG is restored.
#' @param allow_parallel Allow caret to use an already registered parallel
#'   backend. Default FALSE; no backend or global future plan is changed.
#' @param verbose Enable CAST feature-selection and caret progress messages.
#'   The GBM backend may also emit its own progress output.
#' @param git_commit Optional explicit source commit identifier, recorded as NA
#'   when unavailable. The function does not invoke a shell to discover it.
#' @return A `structure_gbm` model bundle containing the fitted CAST/caret model,
#'   selected predictors, schema, class mapping, folds, dataset and sample
#'   provenance, configuration, software versions, and selection results.
#'   Save with [base::saveRDS()] and restore with [base::readRDS()].
#' @export
fit_structure_gbm <- function(data, folds, tune_grid = NULL, tune_length = 3L,
                              metric = base::c("Accuracy", "Kappa", "J"),
                              seed = 123L, allow_parallel = FALSE, verbose = FALSE,
                              git_commit = NA_character_) {
  .validate_folds(data, folds)
  .integer_scalar(seed, "seed", 0L)
  .integer_scalar(tune_length, "tune_length")
  metric <- base::match.arg(metric)
  for (flag in base::list(allow_parallel, verbose)) {
    if (!base::is.logical(flag) || base::length(flag) != 1L || base::is.na(flag)) {
      base::stop("allow_parallel and verbose must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!base::is.character(git_commit) || base::length(git_commit) != 1L) {
    base::stop("git_commit must be a single string or NA_character_.", call. = FALSE)
  }
  # CAST 1.0.3 assumes a forward step after the initial two-variable search.
  if (base::nrow(data$schema) < 3L) {
    base::stop("CAST forward selection requires at least three candidate predictors here.", call. = FALSE)
  }
  if (!base::is.null(tune_grid)) {
    required <- base::c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")
    .numeric_columns(tune_grid, required)
    if (!base::nrow(tune_grid) || !base::setequal(base::names(tune_grid), required) ||
        base::any(base::as.matrix(tune_grid) <= 0)) {
      base::stop("tune_grid must have exactly the four positive GBM tuning columns.", call. = FALSE)
    }
    for (column in base::setdiff(required, "shrinkage")) {
      if (base::any(tune_grid[[column]] != base::floor(tune_grid[[column]]))) {
        base::stop("Tree counts, depths, and node sizes must be integers.", call. = FALSE)
      }
    }
  }
  control <- caret::trainControl(method = "cv", number = base::length(folds$index),
    allowParallel = allow_parallel, returnResamp = "all", verboseIter = verbose,
    classProbs = TRUE, summaryFunction = structure_summary,
    index = folds$index, indexOut = folds$indexOut, savePredictions = "all")
  model <- withr::with_seed(seed, CAST::ffs(
    predictors = data$records[, data$schema$name, drop = FALSE],
    response = data$records$structure_stage, method = "gbm", metric = metric,
    maximize = TRUE, trControl = control, tuneLength = tune_length,
    tuneGrid = tune_grid, seed = seed, cores = 1L, verbose = verbose))
  if (!base::length(model$selectedvars) ||
      !base::any(base::is.finite(model$results[[metric]]))) {
    base::stop("Feature selection produced no valid model or finite selection metric.", call. = FALSE)
  }
  packages <- base::c("structure.stage", "CAST", "caret", "gbm", "sf", "withr")
  versions <- stats::setNames(base::vapply(packages, function(package) {
    base::as.character(utils::packageVersion(package))
  }, base::character(1)), packages)
  base::structure(base::list(
    model = model, selected_predictors = model$selectedvars, schema = data$schema,
    class_map = data$class_map, class_levels = data$class_levels, crs = data$crs,
    data_version = data$data_version, preparation = data$preparation,
    audit = data$audit, sample_key = .fold_key(data), folds = folds,
    config = base::list(method = "gbm", metric = metric, seed = seed,
      tune_grid = tune_grid, tune_length = tune_length, min_variables = 2L,
      allow_parallel = allow_parallel, git_commit = git_commit),
    selection_results = model$results, best_tune = model$bestTune,
    assessment = "selection_resampling_only",
    software = base::list(versions = versions, session = utils::sessionInfo()),
    created_at = base::Sys.time()
  ), class = "structure_gbm")
}

#' Predict structural stages from a saved GBM bundle
#'
#' Predicts stand-derived classes or probabilities. Input predictor definitions
#' must match training definitions exactly for selected predictors. No automatic
#' unit conversion, imputation, resampling, or predictor derivation is performed.
#' If selected, `bt_diff` must already be computed in newdata as documented during
#' preparation. Extra metadata columns are ignored; predictors are reordered by
#' name explicitly.
#' @param object A `structure_gbm` from [fit_structure_gbm()].
#' @param newdata Data frame of finite numeric predictors.
#' @param predictor_schema Predictor definitions for newdata, with the same
#'   fields as [prepare_lynx_data()]. Source versions are also required to match;
#'   validate any changed raster products in a new training run.
#' @param type `"prob"` for class probabilities or `"raw"` for classes.
#' @return A data frame with probability columns in recorded class order, or a
#'   factor with that order, preserving input row order.
#' @export
predict_structure_gbm <- function(object, newdata, predictor_schema,
                                  type = base::c("prob", "raw")) {
  if (!base::inherits(object, "structure_gbm")) {
    base::stop("object must be a structure_gbm model bundle.", call. = FALSE)
  }
  type <- base::match.arg(type)
  schema <- .schema(predictor_schema)
  selected <- object$selected_predictors
  .numeric_columns(newdata, selected)
  fields <- base::c("name", "units", "resolution", "nodata", "interpretation", "source_version")
  expected <- object$schema[base::match(selected, object$schema$name), fields, drop = FALSE]
  actual <- schema[base::match(selected, schema$name), fields, drop = FALSE]
  base::rownames(expected) <- base::rownames(actual) <- NULL
  if (!base::isTRUE(base::all.equal(actual, expected, check.attributes = TRUE))) {
    base::stop("Prediction schema differs from the selected training predictors.", call. = FALSE)
  }
  if (!base::nrow(newdata)) {
    if (type == "raw") return(base::factor(base::character(), levels = object$class_levels))
    return(base::as.data.frame(stats::setNames(
      base::rep(base::list(base::numeric()), base::length(object$class_levels)), object$class_levels)))
  }
  result <- stats::predict(object$model, newdata = newdata[, selected, drop = FALSE], type = type)
  if (type == "prob") {
    .numeric_columns(result, object$class_levels)
    result <- result[, object$class_levels, drop = FALSE]
    if (base::nrow(result) != base::nrow(newdata) ||
        base::any(base::as.matrix(result) < 0 | base::as.matrix(result) > 1) ||
        base::any(base::abs(base::rowSums(result) - 1) > 1e-6)) {
      base::stop("Model returned invalid probability dimensions or values.", call. = FALSE)
    }
  } else {
    result <- base::factor(result, levels = object$class_levels)
    if (base::length(result) != base::nrow(newdata) || base::anyNA(result)) {
      base::stop("Model returned invalid classes or dimensions.", call. = FALSE)
    }
  }
  result
}
