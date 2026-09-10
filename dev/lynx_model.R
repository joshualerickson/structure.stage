# Package-based file entry point. Source this file, then call train_lynx_files().
# The original interactive script is preserved in dev/legacy/lynx_model.R.
# Install structure.stage first; see README.md for the required input contract.

#' Train the lynx workflow from provider-prepared CSV files
#'
#' @param training_csv CSV with canonical metadata, labels, and predictors.
#' @param schema_csv CSV with the explicit predictor schema.
#' @param crs Projected CRS for training coordinates.
#' @param data_version Dataset version identifier.
#' @param output_rds New output file path. Existing files are never overwritten.
#' @param ... Explicit settings passed to structure.stage::run_lynx_workflow().
#' @return The workflow object, invisibly, after saving the complete RDS bundle.
train_lynx_files <- function(training_csv, schema_csv, crs, data_version,
                             output_rds, ...) {
  if (base::file.exists(output_rds)) {
    base::stop("output_rds already exists; choose a new run path.", call. = FALSE)
  }
  if (!base::dir.exists(base::dirname(output_rds))) {
    base::stop("Create the output directory before training.", call. = FALSE)
  }
  records <- utils::read.csv(training_csv, stringsAsFactors = FALSE,
                             colClasses = base::c(sample_id = "character",
                               polygon_id = "character", source_id = "character",
                               acquisition_id = "character"), check.names = FALSE)
  schema <- utils::read.csv(schema_csv, stringsAsFactors = FALSE, check.names = FALSE)
  prepared <- structure.stage::prepare_lynx_data(
    records, schema, crs, data_version, unknown_labels = "drop")
  run <- structure.stage::run_lynx_workflow(prepared, ...)
  base::saveRDS(run, output_rds)
  base::invisible(run)
}
