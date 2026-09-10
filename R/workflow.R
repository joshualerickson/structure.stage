#' Run the lynx spatial feature-selection workflow
#'
#' Samples once and fits one GBM per cluster-size setting. Retains the prepared
#' unsampled data, sampled records, all model bundles, and a tidy results table.
#' Does not choose a winning setting or claim independent predictive performance.
#' Execution is sequential across settings; caret parallelism is opt-in and uses
#' the caller's backend. No files are written and no global plans are changed.
#'
#' The defaults retain the original class sample target, 50 cluster-size settings,
#' ten folds, two-variable-start CAST selection and accuracy objective. Explicit
#' per-stage seeds and polygon clustering improve auditability but do not promise
#' bit-for-bit reproduction of the historical implicit RNG sequence.
#' @param data Unsampled prepared data from [prepare_lynx_data()].
#' @param cluster_sizes Numeric vector of distinct positive sample-per-cluster
#'   targets. These are not spatial distances.
#' @param n_per_class,small_classes Sampling settings; see [sample_structure_data()].
#' @param k,cluster_unit,nstart Spatial settings; see [build_spatial_folds()].
#' @param sample_seed,cluster_seed,fold_seed,model_seed Explicit stage seeds.
#' @param tune_grid,tune_length,metric,allow_parallel,verbose,git_commit Model
#'   settings; see [fit_structure_gbm()].
#' @return A `lynx_workflow` list with `prepared`, `sampled`, `models`, and
#'   `results`. The latter includes `cluster_size`, selected predictors as a
#'   list column, and `char_vars` for CSV export. Results are selection resampling
#'   only. Save the whole object with [base::saveRDS()] for retraining provenance.
#' @export
run_lynx_workflow <- function(data, cluster_sizes = base::seq(300, 500, length.out = 50),
                              n_per_class = 2000L, small_classes = "error",
                              k = 10L, cluster_unit = "polygon", nstart = 1L,
                              sample_seed = 124L, cluster_seed = 124L,
                              fold_seed = 1234L, model_seed = 123L,
                              tune_grid = NULL, tune_length = 3L, metric = "Accuracy",
                              allow_parallel = FALSE, verbose = FALSE,
                              git_commit = NA_character_) {
  .training_data(data)
  if (!base::is.numeric(cluster_sizes) || !base::length(cluster_sizes) ||
      base::any(!base::is.finite(cluster_sizes) | cluster_sizes <= 0) ||
      base::anyDuplicated(cluster_sizes)) {
    base::stop("cluster_sizes must contain distinct positive finite values.", call. = FALSE)
  }
  sampled <- sample_structure_data(data, n_per_class, sample_seed, small_classes)
  # Validate every fold configuration before spending time on feature selection.
  folds <- base::lapply(cluster_sizes, function(size) {
    build_spatial_folds(sampled, size, k, cluster_unit, cluster_seed, fold_seed, nstart)
  })
  models <- base::lapply(folds, function(fold) {
    fit_structure_gbm(sampled, fold, tune_grid, tune_length, metric,
      model_seed, allow_parallel, verbose, git_commit)
  })
  base::names(models) <- base::paste0("setting_", base::seq_along(models))
  rows <- base::lapply(base::seq_along(models), function(i) {
    result <- models[[i]]$selection_results
    result$setting <- base::names(models)[i]
    result$cluster_size <- cluster_sizes[i]
    result$model <- "gbm"
    result$assessment <- "selection_resampling_only"
    result$selected_predictors <- base::rep(base::list(models[[i]]$selected_predictors), base::nrow(result))
    result$char_vars <- base::paste(models[[i]]$selected_predictors, collapse = ", ")
    result
  })
  base::structure(base::list(prepared = data, sampled = sampled, models = models,
    results = base::do.call(base::rbind, rows)), class = "lynx_workflow")
}
