# Package-based entry point for three-class GBM interpretation.
#
# This script does not fit a model. It reads an existing final model bundle or
# caret GBM RDS, runs one-variable-at-a-time SD ablations, and writes a complete
# result set. Use the package functions directly when objects are already in R.

#' Run structural-stage GBM interpretation from CSV and RDS inputs
#'
#' @param model_rds Path to a final `structure_gbm` or caret GBM RDS.
#' @param newdata_csv CSV of predictor records to perturb.
#' @param reference_csv CSV defining standard deviations. NULL uses retained
#'   training data in the model.
#' @param output_dir Existing destination directory.
#' @param output_prefix New filename prefix for all output files.
#' @param variables NULL for all selected predictors or their explicit names.
#' @param sd_multiplier Positive perturbation size in standard deviations.
#' @return The `structure_gbm_ablation` result, invisibly.
run_interpretation_files <- function(model_rds, newdata_csv, reference_csv = NULL,
                                     output_dir, output_prefix, variables = NULL,
                                     sd_multiplier = 1) {
  newdata <- utils::read.csv(newdata_csv, stringsAsFactors = FALSE, check.names = FALSE)
  reference_data <- if (base::is.null(reference_csv)) {
    NULL
  } else {
    utils::read.csv(reference_csv, stringsAsFactors = FALSE, check.names = FALSE)
  }
  result <- structure.stage::ablate_structure_gbm(
    model = model_rds, newdata = newdata, reference_data = reference_data,
    variables = variables, sd_multiplier = sd_multiplier
  )
  structure.stage::save_structure_ablation(result, output_dir, output_prefix)
  base::invisible(result)
}
