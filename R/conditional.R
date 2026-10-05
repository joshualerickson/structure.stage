#' Analyze MSF versus SE response at fixed heights
#'
#' Sets `height_variable` to each requested value for every reference record,
#' then varies `betweenness_variable` across its requested quantiles. Remaining
#' selected predictors retain their observed values, and predicted probabilities
#' are averaged over those records. Thus every row evaluates the model at an
#' equal fixed height while preserving the reference distribution of its other
#' predictors. The output is a partial-dependence-style model response, not
#' observed classification performance and not a causal effect.
#'
#' `P(si)` is retained because an apparent MSF-versus-SE shift may instead be
#' driven by probability moving to or from SI.
#'
#' @param model A final `structure_gbm`, caret GBM `train` object, or an RDS
#'   path containing either.
#' @param reference_data Predictor records over which to average. NULL uses the
#'   retained training data in the model.
#' @param height_variable Selected height predictor, usually `"zmax"`.
#' @param betweenness_variable Selected predictor, usually
#'   `"understory_mean_betweenness"`.
#' @param height_quantiles Quantiles used as fixed height values.
#' @param betweenness_quantiles Quantiles at which to evaluate betweenness.
#' @return A `structure_conditional_response` object with a `response` table.
#' @export
analyze_msf_se_height_betweenness <- function(
    model, reference_data = NULL, height_variable = "zmax",
    betweenness_variable = "understory_mean_betweenness",
    height_quantiles = base::c(0.25, 0.50, 0.75),
    betweenness_quantiles = base::seq(0.05, 0.95, by = 0.05)) {
  resolved <- .interpretation_model(model)
  reference_data <- .interpretation_reference(reference_data, resolved)
  variables <- base::c(height_variable, betweenness_variable)
  if (!base::is.character(variables) || base::anyNA(variables) ||
      base::length(base::unique(variables)) != 2L ||
      base::length(base::setdiff(variables, resolved$predictors))) {
    base::stop("height_variable and betweenness_variable must be distinct selected predictors.", call. = FALSE)
  }
  for (quantiles in base::list(height_quantiles, betweenness_quantiles)) {
    if (!base::is.numeric(quantiles) || !base::length(quantiles) || base::anyNA(quantiles) ||
        base::any(!base::is.finite(quantiles) | quantiles < 0 | quantiles > 1)) {
      base::stop("Quantiles must be finite values from 0 through 1.", call. = FALSE)
    }
  }
  heights <- stats::quantile(reference_data[[height_variable]], probs = height_quantiles,
                             names = FALSE, type = 7)
  betweenness <- stats::quantile(reference_data[[betweenness_variable]], probs = betweenness_quantiles,
                                  names = FALSE, type = 7)
  if (base::anyDuplicated(heights) || base::anyDuplicated(betweenness)) {
    base::stop("Requested quantiles contain duplicate observed values; choose fewer quantiles or a variable with variation.",
               call. = FALSE)
  }
  rows <- base::vector("list", base::length(heights) * base::length(betweenness))
  index <- 0L
  for (height_index in base::seq_along(heights)) {
    for (betweenness_index in base::seq_along(betweenness)) {
      index <- index + 1L
      grid <- reference_data[, resolved$predictors, drop = FALSE]
      grid[[height_variable]] <- heights[[height_index]]
      grid[[betweenness_variable]] <- betweenness[[betweenness_index]]
      probabilities <- .interpretation_probabilities(resolved, grid)
      rows[[index]] <- base::data.frame(
        height_quantile = height_quantiles[[height_index]],
        height_value = heights[[height_index]],
        betweenness_quantile = betweenness_quantiles[[betweenness_index]],
        betweenness_value = betweenness[[betweenness_index]],
        mean_probability_msf = base::mean(probabilities$msf),
        mean_probability_se = base::mean(probabilities$se),
        mean_probability_si = base::mean(probabilities$si),
        mean_msf_minus_se = base::mean(probabilities$msf - probabilities$se),
        row.names = NULL
      )
    }
  }
  base::structure(base::list(
    response = base::do.call(base::rbind, rows),
    config = base::list(height_variable = height_variable,
      betweenness_variable = betweenness_variable,
      height_quantiles = height_quantiles,
      betweenness_quantiles = betweenness_quantiles),
    class_levels = resolved$class_levels
  ), class = "structure_conditional_response")
}

#' Summarize held-out MSF versus SE performance by observed height band
#'
#' Uses outer held-out predictions from [run_mscv_gbm()] and retains only
#' observations carrying `msf` or `se` stand-derived labels. It reports class
#' recalls and balanced accuracy within quantile bands of an observed height
#' predictor. This is an empirical performance diagnostic, unlike
#' [analyze_msf_se_height_betweenness()], which describes conditional model
#' response. Sparse bands should be combined or omitted before interpretation.
#'
#' @param mscv A `structure_mscv` result from [run_mscv_gbm()].
#' @param data The matching prepared `structure_data` used for `mscv`.
#' @param height_variable Predictor used to form observed height bands.
#' @param bins Number of quantile bands, at least two.
#' @return A data frame with held-out MSF/SE counts, recall, balanced accuracy,
#'   and mean probability contrast per height band.
#' @export
evaluate_msf_se_height_bins <- function(mscv, data, height_variable = "zmax", bins = 4L) {
  if (!base::inherits(mscv, "structure_mscv")) {
    base::stop("mscv must be a structure_mscv object with outer held-out predictions.", call. = FALSE)
  }
  .training_data(data)
  .integer_scalar(bins, "bins", 2L)
  if (!height_variable %in% data$schema$name) {
    base::stop("height_variable must be in the prepared predictor schema.", call. = FALSE)
  }
  predictions <- mscv$predictions
  .columns(predictions, base::c("sample_id", "obs", "pred", "msf", "se"))
  heights <- data$records[, base::c("sample_id", height_variable), drop = FALSE]
  index <- base::match(predictions$sample_id, heights$sample_id)
  if (base::anyNA(index) || base::anyDuplicated(predictions$sample_id)) {
    base::stop("MSCV predictions do not match unique prepared sample IDs.", call. = FALSE)
  }
  output <- predictions
  output$height <- heights[[height_variable]][index]
  output <- output[base::as.character(output$obs) %in% base::c("msf", "se"), , drop = FALSE]
  if (!base::nrow(output) || base::length(base::unique(output$obs)) != 2L) {
    base::stop("Outer predictions must contain both observed msf and se samples.", call. = FALSE)
  }
  breaks <- stats::quantile(output$height, probs = base::seq(0, 1, length.out = bins + 1L),
                            names = FALSE, type = 7)
  if (base::anyDuplicated(breaks)) {
    base::stop("Height quantile breaks are duplicated; reduce bins or use a variable with more variation.", call. = FALSE)
  }
  output$height_band <- base::cut(output$height, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
  rows <- base::lapply(base::split(output, output$height_band), function(part) {
    n_msf <- base::sum(part$obs == "msf")
    n_se <- base::sum(part$obs == "se")
    recall_msf <- if (n_msf) base::mean(part$pred[part$obs == "msf"] == "msf") else NA_real_
    recall_se <- if (n_se) base::mean(part$pred[part$obs == "se"] == "se") else NA_real_
    base::data.frame(
      height_band = base::as.character(part$height_band[1L]),
      height_min = base::min(part$height), height_max = base::max(part$height),
      n = base::nrow(part), n_msf = n_msf, n_se = n_se,
      recall_msf = recall_msf, recall_se = recall_se,
      balanced_accuracy = base::mean(base::c(recall_msf, recall_se), na.rm = TRUE),
      mean_msf_minus_se_observed_msf = base::mean((part$msf - part$se)[part$obs == "msf"]),
      mean_msf_minus_se_observed_se = base::mean((part$msf - part$se)[part$obs == "se"]),
      row.names = NULL
    )
  })
  base::do.call(base::rbind, rows)
}
