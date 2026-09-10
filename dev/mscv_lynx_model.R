# Package-based nested spatial-CV entry point.
#
# The original script evaluated an external validation CSV and did not perform
# outer spatial CV. Its logic is superseded here by the package workflow: every
# sample is held out once in a spatial outer fold, and feature selection/tuning
# occur only inside the corresponding outer training partition.

#' Run reportable nested spatial CV from provider-prepared CSV files
#'
#' @param training_csv Canonical training-record CSV.
#' @param schema_csv Explicit predictor-schema CSV.
#' @param crs Projected CRS for `x` and `y`.
#' @param data_version Dataset version identifier.
#' @param outer_samples_per_cluster Outer cluster-size target.
#' @param inner_samples_per_cluster Inner cluster-size target.
#' @param output_dir Existing directory for the complete nested-CV result set.
#' @param output_prefix New filename prefix for the result set.
#' @param selected_predictors NULL to nest feature selection, or an independent
#'   fixed character vector of predictor names.
#' @param ... Explicit arguments passed to structure.stage::run_mscv_gbm().
#' @return The `structure_mscv` result, invisibly, after saving it as RDS.
run_mscv_files <- function(training_csv, schema_csv, crs, data_version,
                           outer_samples_per_cluster,
                           inner_samples_per_cluster, output_dir, output_prefix,
                           selected_predictors = NULL, ...) {
  if (!base::dir.exists(output_dir)) {
    base::stop("Create the output directory before running nested CV.", call. = FALSE)
  }
  records <- utils::read.csv(
    training_csv, stringsAsFactors = FALSE,
    colClasses = base::c(sample_id = "character", polygon_id = "character",
      source_id = "character", acquisition_id = "character"), check.names = FALSE
  )
  schema <- utils::read.csv(schema_csv, stringsAsFactors = FALSE, check.names = FALSE)
  prepared <- structure.stage::prepare_lynx_data(
    records, schema, crs, data_version, unknown_labels = "drop"
  )
  outer_folds <- structure.stage::build_spatial_folds(
    prepared, samples_per_cluster = outer_samples_per_cluster
  )
  result <- structure.stage::run_mscv_gbm(
    prepared, outer_folds = outer_folds,
    selected_predictors = selected_predictors,
    inner_samples_per_cluster = inner_samples_per_cluster, ...
  )
  structure.stage::save_mscv_results(result, output_dir, output_prefix)
  base::invisible(result)
}
