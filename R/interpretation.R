#' Resolve a structural-stage GBM for interpretation
#'
#' @param model A `structure_gbm` bundle, a caret GBM `train` object, or a path
#'   to an RDS containing either. Package model bundles are preferred because
#'   they retain the selected predictor order and the final class mapping.
#' @return An internal list containing the caret model, predictor names, class
#'   order, and reference training data.
#' @keywords internal
.interpretation_model <- function(model) {
  if (base::is.character(model) && base::length(model) == 1L && !base::is.na(model)) {
    if (!base::file.exists(model)) {
      base::stop("The model RDS path does not exist.", call. = FALSE)
    }
    model <- base::readRDS(model)
  }
  if (base::inherits(model, "structure_gbm")) {
    caret_model <- model$model
    predictors <- model$selected_predictors
    class_levels <- model$class_levels
  } else if (base::inherits(model, "train") && base::identical(model$method, "gbm")) {
    caret_model <- model
    predictors <- model$coefnames
    if (base::is.null(predictors) && base::is.data.frame(model$trainingData)) {
      predictors <- base::setdiff(base::names(model$trainingData), ".outcome")
    }
    class_levels <- base::as.character(model$levels)
  } else {
    base::stop(
      "model must be a structure_gbm bundle, a caret GBM train object, or an RDS path containing one.",
      call. = FALSE
    )
  }
  expected <- base::c("msf", "se", "si")
  if (!base::is.character(predictors) || !base::length(predictors) ||
      base::anyNA(predictors) || base::anyDuplicated(predictors) ||
      !base::setequal(class_levels, expected)) {
    base::stop("Interpretation requires exactly the three classes msf, se, and si.", call. = FALSE)
  }
  training <- caret_model$trainingData
  if (!base::is.data.frame(training)) training <- NULL
  base::list(
    caret_model = caret_model, predictors = predictors, class_levels = expected,
    training_data = training
  )
}

.interpretation_data <- function(data, predictors, name) {
  if (!base::is.data.frame(data)) {
    base::stop(name, " must be a data frame.", call. = FALSE)
  }
  .numeric_columns(data, predictors)
  base::as.data.frame(data)
}

.interpretation_reference <- function(reference_data, resolved) {
  if (base::is.null(reference_data)) reference_data <- resolved$training_data
  if (base::is.null(reference_data)) {
    base::stop(
      "reference_data is required when the supplied model does not retain trainingData.",
      call. = FALSE
    )
  }
  .interpretation_data(reference_data, resolved$predictors, "reference_data")
}

.interpretation_probabilities <- function(resolved, newdata) {
  # Use caret's exported method directly. stats::predict() can fail to discover
  # predict.train() when a train object is restored from RDS without caret being
  # attached in the caller's session.
  probabilities <- caret::predict.train(
    resolved$caret_model, newdata = newdata[, resolved$predictors, drop = FALSE], type = "prob"
  )
  .numeric_columns(probabilities, resolved$class_levels)
  probabilities <- probabilities[, resolved$class_levels, drop = FALSE]
  if (base::nrow(probabilities) != base::nrow(newdata) ||
      base::any(base::as.matrix(probabilities) < 0 | base::as.matrix(probabilities) > 1) ||
      base::any(base::abs(base::rowSums(probabilities) - 1) > 1e-6)) {
    base::stop("The GBM returned invalid class probabilities.", call. = FALSE)
  }
  probabilities
}

.prediction_points <- function(probabilities, newdata, class_levels) {
  ordered <- base::t(base::apply(probabilities, 1L, base::sort, decreasing = TRUE))
  predicted <- class_levels[base::max.col(probabilities, ties.method = "first")]
  point_id <- if ("sample_id" %in% base::names(newdata)) {
    base::as.character(newdata$sample_id)
  } else {
    base::paste0("row_", base::seq_len(base::nrow(newdata)))
  }
  if (base::anyNA(point_id) || base::anyDuplicated(point_id)) {
    base::stop("sample_id must be unique and nonmissing when supplied for interpretation.", call. = FALSE)
  }
  metadata <- base::intersect(
    base::c("sample_id", "polygon_id", "source_id", "acquisition_id", "structure_stage"),
    base::names(newdata)
  )
  base::data.frame(
    point_id = point_id, newdata[, metadata, drop = FALSE],
    predicted_class = base::factor(predicted, levels = class_levels),
    confidence = ordered[, 1L], top_two_margin = ordered[, 1L] - ordered[, 2L],
    probabilities, check.names = FALSE, row.names = NULL
  )
}

.pairwise_competition <- function(points, class_levels, close_margin) {
  pairs <- utils::combn(class_levels, 2L, simplify = FALSE)
  base::do.call(base::rbind, base::lapply(pairs, function(pair) {
    first <- points[[pair[1L]]]
    second <- points[[pair[2L]]]
    base::data.frame(
      point_id = points$point_id, class_a = pair[1L], class_b = pair[2L],
      probability_a = first, probability_b = second,
      margin_a_minus_b = first - second,
      favored_class = base::factor(base::ifelse(first >= second, pair[1L], pair[2L]), levels = class_levels),
      overall_predicted_class = points$predicted_class,
      close_competition = base::abs(first - second) <= close_margin,
      row.names = NULL, check.names = FALSE
    )
  }))
}

.pairwise_summary <- function(pairwise) {
  keys <- base::interaction(pairwise$class_a, pairwise$class_b, drop = TRUE, lex.order = TRUE)
  base::do.call(base::rbind, base::lapply(base::split(pairwise, keys), function(data) {
    base::data.frame(
      class_a = data$class_a[1L], class_b = data$class_b[1L],
      mean_absolute_margin = base::mean(base::abs(data$margin_a_minus_b)),
      close_competition_rate = base::mean(data$close_competition),
      class_a_favored_rate = base::mean(data$favored_class == data$class_a[1L]),
      row.names = NULL
    )
  }))
}

#' Describe three-class structural-stage predictions
#'
#' Produces a point-level probability table and direct pairwise competition
#' tables for `msf` versus `se`, `msf` versus `si`, and `se` versus `si`.
#' `msf` versus `se` is retained as a named table because those classes may have
#' similar LiDAR signatures. A small pairwise margin means the two classes have
#' similar model support for that point; it is not evidence of ecological truth.
#'
#' @param model A final `structure_gbm`, a caret GBM `train` object, or an RDS
#'   path containing either.
#' @param newdata Predictor records to interpret. NULL uses the model's retained
#'   training records. New data may contain additional metadata columns.
#' @param close_margin Nonnegative probability-difference threshold for calling
#'   a pairwise competition close. The default is 0.10.
#' @return A `structure_prediction_analysis` list with `points`, `pairwise`,
#'   `pairwise_summary`, and `msf_se` point-level competition.
#' @export
analyze_structure_predictions <- function(model, newdata = NULL, close_margin = 0.10) {
  resolved <- .interpretation_model(model)
  if (!base::is.numeric(close_margin) || base::length(close_margin) != 1L ||
      !base::is.finite(close_margin) || close_margin < 0 || close_margin > 1) {
    base::stop("close_margin must be a finite probability difference from 0 to 1.", call. = FALSE)
  }
  if (base::is.null(newdata)) newdata <- resolved$training_data
  newdata <- .interpretation_data(newdata, resolved$predictors, "newdata")
  probabilities <- .interpretation_probabilities(resolved, newdata)
  points <- .prediction_points(probabilities, newdata, resolved$class_levels)
  pairwise <- .pairwise_competition(points, resolved$class_levels, close_margin)
  base::structure(base::list(
    points = points, pairwise = pairwise, pairwise_summary = .pairwise_summary(pairwise),
    msf_se = pairwise[pairwise$class_a == "msf" & pairwise$class_b == "se", , drop = FALSE],
    class_levels = resolved$class_levels, close_margin = close_margin
  ), class = "structure_prediction_analysis")
}

.average_ablation_changes <- function(point_changes, class_levels) {
  groups <- base::c("all", class_levels)
  rows <- base::lapply(groups, function(group) {
    data <- if (group == "all") point_changes else point_changes[point_changes$baseline_class == group, , drop = FALSE]
    base::do.call(base::rbind, base::lapply(
      base::split(data, base::interaction(data$variable, data$direction, drop = TRUE)), function(part) {
      base::data.frame(
        baseline_group = group, variable = part$variable[1L], direction = part$direction[1L],
        standard_deviation = part$standard_deviation[1L], shift = part$shift[1L],
        n_points = base::nrow(part), class_change_rate = base::mean(part$class_changed),
        mean_delta_msf = base::mean(part$delta_msf), mean_delta_se = base::mean(part$delta_se),
        mean_delta_si = base::mean(part$delta_si),
        mean_abs_probability_change = base::mean((base::abs(part$delta_msf) +
          base::abs(part$delta_se) + base::abs(part$delta_si)) / 3),
        mean_delta_msf_minus_se = base::mean(part$delta_msf - part$delta_se),
        row.names = NULL
      )
    }))
  })
  base::do.call(base::rbind, rows)
}

.ablation_transitions <- function(point_changes) {
  groups <- base::interaction(point_changes$variable, point_changes$direction,
                              point_changes$baseline_class, point_changes$perturbed_class,
                              drop = TRUE, lex.order = TRUE)
  output <- base::do.call(base::rbind, base::lapply(base::split(point_changes, groups), function(part) {
    base::data.frame(
      variable = part$variable[1L], direction = part$direction[1L],
      baseline_class = part$baseline_class[1L], perturbed_class = part$perturbed_class[1L],
      n_points = base::nrow(part), row.names = NULL
    )
  }))
  totals <- stats::aggregate(n_points ~ variable + direction + baseline_class, output, base::sum)
  output$transition_rate <- output$n_points / totals$n_points[base::match(
    base::interaction(output$variable, output$direction, output$baseline_class, drop = TRUE),
    base::interaction(totals$variable, totals$direction, totals$baseline_class, drop = TRUE)
  )]
  output
}

#' Ablate structural-stage GBM predictors by plus or minus SD
#'
#' Holds all other predictors fixed for each point while changing one selected
#' predictor by a specified number of reference-data standard deviations. This
#' is a local counterfactual sensitivity analysis of model behavior. It does not
#' establish causal ecological effects, does not model covariate dependence, and
#' may create predictor combinations absent from the observed data.
#'
#' The output provides point-level probability changes, average changes overall
#' and by baseline predicted class, predicted-class transitions, and a focused
#' `msf` minus `se` contrast. Use [analyze_structure_predictions()] to inspect
#' the unperturbed three-class competition.
#'
#' @param model A final `structure_gbm`, a caret GBM `train` object, or an RDS
#'   path containing either.
#' @param newdata Points to perturb. NULL uses model training records.
#' @param reference_data Records used to compute predictor standard deviations.
#'   NULL uses retained model training records. This should normally be the same
#'   final-training population used to fit the model.
#' @param variables Selected predictor names to perturb. NULL uses all model
#'   predictors, in their fitted order.
#' @param sd_multiplier Positive number of standard deviations to add and subtract.
#' @param close_margin Passed to [analyze_structure_predictions()].
#' @return A `structure_gbm_ablation` list containing baseline prediction
#'   analysis, `point_changes`, `average_changes`, `class_transitions`, and
#'   `msf_se_changes`.
#' @export
ablate_structure_gbm <- function(model, newdata = NULL, reference_data = NULL,
                                 variables = NULL, sd_multiplier = 1,
                                 close_margin = 0.10) {
  resolved <- .interpretation_model(model)
  if (base::is.null(newdata)) newdata <- resolved$training_data
  newdata <- .interpretation_data(newdata, resolved$predictors, "newdata")
  reference_data <- .interpretation_reference(reference_data, resolved)
  if (base::is.null(variables)) variables <- resolved$predictors
  if (!base::is.character(variables) || !base::length(variables) || base::anyNA(variables) ||
      base::anyDuplicated(variables) || base::length(base::setdiff(variables, resolved$predictors))) {
    base::stop("variables must be unique selected predictor names.", call. = FALSE)
  }
  if (!base::is.numeric(sd_multiplier) || base::length(sd_multiplier) != 1L ||
      !base::is.finite(sd_multiplier) || sd_multiplier <= 0) {
    base::stop("sd_multiplier must be a positive finite number.", call. = FALSE)
  }
  standard_deviations <- base::vapply(variables, function(variable) {
    stats::sd(reference_data[[variable]])
  }, base::numeric(1))
  if (base::any(!base::is.finite(standard_deviations) | standard_deviations <= 0)) {
    bad <- base::names(standard_deviations)[!base::is.finite(standard_deviations) | standard_deviations <= 0]
    base::stop("Cannot perturb zero-variance or non-finite variables: ",
               base::paste(bad, collapse = ", "), call. = FALSE)
  }
  baseline <- analyze_structure_predictions(resolved$caret_model, newdata, close_margin)
  changes <- base::vector("list", base::length(variables) * 2L)
  index <- 0L
  for (variable in variables) {
    for (direction in base::c("minus_sd", "plus_sd")) {
      index <- index + 1L
      shift <- if (direction == "minus_sd") -sd_multiplier * standard_deviations[[variable]] else sd_multiplier * standard_deviations[[variable]]
      perturbed <- newdata
      original <- perturbed[[variable]]
      perturbed[[variable]] <- original + shift
      probabilities <- .interpretation_probabilities(resolved, perturbed)
      perturbed_class <- base::factor(
        resolved$class_levels[base::max.col(probabilities, ties.method = "first")],
        levels = resolved$class_levels
      )
      base_points <- baseline$points
      changes[[index]] <- base::data.frame(
        point_id = base_points$point_id, variable = variable, direction = direction,
        standard_deviation = standard_deviations[[variable]], shift = shift,
        original_value = original, perturbed_value = perturbed[[variable]],
        baseline_class = base_points$predicted_class, perturbed_class = perturbed_class,
        class_changed = base_points$predicted_class != perturbed_class,
        baseline_msf = base_points$msf, baseline_se = base_points$se, baseline_si = base_points$si,
        perturbed_msf = probabilities$msf, perturbed_se = probabilities$se, perturbed_si = probabilities$si,
        delta_msf = probabilities$msf - base_points$msf,
        delta_se = probabilities$se - base_points$se,
        delta_si = probabilities$si - base_points$si,
        row.names = NULL, check.names = FALSE
      )
    }
  }
  point_changes <- base::do.call(base::rbind, changes)
  average_changes <- .average_ablation_changes(point_changes, resolved$class_levels)
  transitions <- .ablation_transitions(point_changes)
  msf_se_changes <- average_changes[, base::c(
    "baseline_group", "variable", "direction", "standard_deviation", "shift", "n_points",
    "class_change_rate", "mean_delta_msf", "mean_delta_se", "mean_delta_msf_minus_se"
  ), drop = FALSE]
  base::structure(base::list(
    baseline = baseline, point_changes = point_changes, average_changes = average_changes,
    class_transitions = transitions, msf_se_changes = msf_se_changes,
    standard_deviations = base::data.frame(variable = base::names(standard_deviations),
      standard_deviation = base::unname(standard_deviations), row.names = NULL),
    config = base::list(variables = variables, sd_multiplier = sd_multiplier,
      close_margin = close_margin), class_levels = resolved$class_levels
  ), class = "structure_gbm_ablation")
}

#' Save structural-stage GBM interpretation outputs
#'
#' @param object A result from [ablate_structure_gbm()].
#' @param output_dir Existing output directory.
#' @param prefix New filename prefix for the complete result set.
#' @return A named character vector of written paths, invisibly.
#' @export
save_structure_ablation <- function(object, output_dir, prefix) {
  if (!base::inherits(object, "structure_gbm_ablation")) {
    base::stop("object must be a structure_gbm_ablation result.", call. = FALSE)
  }
  .scalar_text(output_dir, "output_dir")
  .scalar_text(prefix, "prefix")
  if (!base::dir.exists(output_dir)) base::stop("output_dir must already exist.", call. = FALSE)
  if (base::grepl("[/\\\\]", prefix)) base::stop("prefix must be a file name, not a path.", call. = FALSE)
  paths <- base::file.path(output_dir, base::paste0(prefix, base::c(
    ".rds", "_baseline_predictions.csv", "_pairwise_competition.csv",
    "_pairwise_summary.csv", "_msf_se_competition.csv", "_point_changes.csv",
    "_average_changes.csv", "_class_transitions.csv", "_msf_se_changes.csv",
    "_standard_deviations.csv"
  )))
  base::names(paths) <- base::c(
    "result", "baseline_predictions", "pairwise_competition", "pairwise_summary",
    "msf_se_competition", "point_changes", "average_changes", "class_transitions",
    "msf_se_changes", "standard_deviations"
  )
  if (base::any(base::file.exists(paths))) {
    base::stop("One or more interpretation result paths already exist; choose a new prefix.", call. = FALSE)
  }
  base::saveRDS(object, paths[["result"]])
  utils::write.csv(object$baseline$points, paths[["baseline_predictions"]], row.names = FALSE)
  utils::write.csv(object$baseline$pairwise, paths[["pairwise_competition"]], row.names = FALSE)
  utils::write.csv(object$baseline$pairwise_summary, paths[["pairwise_summary"]], row.names = FALSE)
  utils::write.csv(object$baseline$msf_se, paths[["msf_se_competition"]], row.names = FALSE)
  utils::write.csv(object$point_changes, paths[["point_changes"]], row.names = FALSE)
  utils::write.csv(object$average_changes, paths[["average_changes"]], row.names = FALSE)
  utils::write.csv(object$class_transitions, paths[["class_transitions"]], row.names = FALSE)
  utils::write.csv(object$msf_se_changes, paths[["msf_se_changes"]], row.names = FALSE)
  utils::write.csv(object$standard_deviations, paths[["standard_deviations"]], row.names = FALSE)
  base::invisible(paths)
}
