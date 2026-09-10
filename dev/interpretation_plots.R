# Helpers for visualizing outputs from save_structure_ablation().
#
# These functions deliberately use the fixed msf/se/si interpretation outputs,
# rather than trying to generalize beyond the known three-class problem.

#' Read one saved structural-stage ablation result set
#'
#' @param output_dir Directory passed to save_structure_ablation().
#' @param prefix Output prefix passed to save_structure_ablation().
#' @return A named list of interpretation CSV data frames.
read_ablation_outputs <- function(output_dir, prefix) {
  suffixes <- base::c(
    average_changes = "_average_changes.csv",
    transitions = "_class_transitions.csv",
    point_changes = "_point_changes.csv",
    baseline = "_baseline_predictions.csv",
    msf_se_competition = "_msf_se_competition.csv"
  )
  paths <- base::file.path(output_dir, base::paste0(prefix, suffixes))
  if (base::any(!base::file.exists(paths))) {
    base::stop("Could not find all ablation CSV files for this prefix.", call. = FALSE)
  }
  output <- base::lapply(paths, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
  base::names(output) <- base::names(suffixes)
  output
}

#' Rank variables by their msf-versus-se sensitivity
#'
#' Uses the all-point mean absolute change in P(msf) - P(se), averaging the
#' plus-SD and minus-SD perturbations. Larger values identify variables that most
#' change the model's relative support for the difficult msf/se distinction.
#'
#' @param average_changes `average_changes` from read_ablation_outputs().
#' @return A descending data frame with one row per variable.
rank_msf_se_sensitivity <- function(average_changes) {
  required <- base::c("baseline_group", "variable", "direction", "mean_delta_msf_minus_se")
  if (!base::is.data.frame(average_changes) ||
      base::length(base::setdiff(required, base::names(average_changes)))) {
    base::stop("average_changes is missing required ablation columns.", call. = FALSE)
  }
  all_points <- average_changes[average_changes$baseline_group == "all", , drop = FALSE]
  summary <- stats::aggregate(
    base::abs(all_points$mean_delta_msf_minus_se),
    by = base::list(variable = all_points$variable), FUN = base::mean
  )
  base::names(summary)[2L] <- "mean_absolute_msf_se_shift"
  summary <- summary[base::order(summary$mean_absolute_msf_se_shift, decreasing = TRUE), , drop = FALSE]
  base::rownames(summary) <- NULL
  summary
}

#' Plot paired plus/minus-SD msf-versus-se sensitivity
#'
#' @param average_changes `average_changes` from read_ablation_outputs().
#' @param baseline_group One of all, msf, se, or si.
#' @return A ggplot object.
plot_msf_se_sensitivity <- function(average_changes, baseline_group = "all") {
  required <- base::c("baseline_group", "variable", "direction", "mean_delta_msf_minus_se")
  if (!base::is.data.frame(average_changes) ||
      base::length(base::setdiff(required, base::names(average_changes)))) {
    base::stop("average_changes is missing required ablation columns.", call. = FALSE)
  }
  data <- average_changes[average_changes$baseline_group == baseline_group, , drop = FALSE]
  if (!base::nrow(data)) base::stop("No rows found for baseline_group.", call. = FALSE)
  order <- rank_msf_se_sensitivity(average_changes)$variable
  data$variable <- base::factor(data$variable, levels = base::rev(order))
  data$direction <- base::factor(data$direction, levels = base::c("minus_sd", "plus_sd"))
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$mean_delta_msf_minus_se, y = .data$variable, color = .data$direction
  )) +
    ggplot2::geom_vline(xintercept = 0, color = "grey45") +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_line(ggplot2::aes(group = .data$variable), color = "grey70") +
    ggplot2::labs(
      x = "Mean change in P(msf) - P(se)", y = NULL, color = "Perturbation",
      title = base::paste("MSF versus SE sensitivity:", baseline_group, "baseline points")
    ) +
    ggplot2::theme_minimal()
}

#' Plot class transitions after a selected variable perturbation
#'
#' Each row is conditional on the baseline predicted class. Dark off-diagonal
#' cells show perturbations that move points across a class boundary.
#'
#' @param transitions `transitions` from read_ablation_outputs().
#' @param variable Selected predictor name.
#' @param direction Either minus_sd or plus_sd.
#' @return A ggplot object.
plot_ablation_transitions <- function(transitions, variable, direction = "plus_sd") {
  required <- base::c("variable", "direction", "baseline_class", "perturbed_class", "transition_rate")
  if (!base::is.data.frame(transitions) ||
      base::length(base::setdiff(required, base::names(transitions)))) {
    base::stop("transitions is missing required ablation columns.", call. = FALSE)
  }
  data <- transitions[transitions$variable == variable & transitions$direction == direction, , drop = FALSE]
  if (!base::nrow(data)) base::stop("No transition rows found for variable and direction.", call. = FALSE)
  levels <- base::c("msf", "se", "si")
  data$baseline_class <- base::factor(data$baseline_class, levels = levels)
  data$perturbed_class <- base::factor(data$perturbed_class, levels = levels)
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$perturbed_class, y = .data$baseline_class, fill = .data$transition_rate
  )) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = base::sprintf("%.1f%%", 100 * .data$transition_rate))) +
    ggplot2::scale_fill_viridis_c(labels = scales::label_percent()) +
    ggplot2::labs(
      x = "Perturbed predicted class", y = "Baseline predicted class",
      fill = "Transition rate", title = base::paste(variable, direction)
    ) +
    ggplot2::theme_minimal()
}

#' Plot msf/se ambiguity in baseline predictions
#'
#' @param msf_se_competition `msf_se_competition` from read_ablation_outputs().
#' @return A ggplot object. Near-zero values indicate similar msf and se support.
plot_msf_se_ambiguity <- function(msf_se_competition) {
  required <- base::c("margin_a_minus_b", "overall_predicted_class")
  if (!base::is.data.frame(msf_se_competition) ||
      base::length(base::setdiff(required, base::names(msf_se_competition)))) {
    base::stop("msf_se_competition is missing required prediction columns.", call. = FALSE)
  }
  ggplot2::ggplot(msf_se_competition, ggplot2::aes(
    x = .data$overall_predicted_class, y = .data$margin_a_minus_b,
    color = .data$overall_predicted_class
  )) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45") +
    ggplot2::geom_boxplot(outlier.alpha = 0.15) +
    ggplot2::labs(
      x = "Overall predicted class", y = "P(msf) - P(se)",
      title = "MSF versus SE competition in baseline predictions"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none")
}

#' Plot MSF-versus-SE response as betweenness changes at equal fixed heights
#'
#' @param conditional_response Result from analyze_msf_se_height_betweenness().
#' @return A ggplot object. Positive values favor MSF relative to SE.
plot_fixed_height_msf_se <- function(conditional_response) {
  if (!base::inherits(conditional_response, "structure_conditional_response")) {
    base::stop("conditional_response must come from analyze_msf_se_height_betweenness().", call. = FALSE)
  }
  data <- conditional_response$response
  height_name <- conditional_response$config$height_variable
  betweenness_name <- conditional_response$config$betweenness_variable
  data$height_label <- base::paste0(
    height_name, " q", base::sprintf("%.2f", data$height_quantile),
    " = ", base::formatC(data$height_value, digits = 3, format = "fg")
  )
  ggplot2::ggplot(data, ggplot2::aes(
    x = .data$betweenness_value, y = .data$mean_msf_minus_se,
    color = .data$height_label
  )) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::labs(
      x = betweenness_name, y = "Mean P(msf) - P(se)", color = "Fixed height",
      title = "MSF versus SE response as understory betweenness changes"
    ) +
    ggplot2::theme_minimal()
}

#' Plot all three probabilities at equal fixed heights
#'
#' @param conditional_response Result from analyze_msf_se_height_betweenness().
#' @return A ggplot object showing MSF, SE, and SI probability responses.
plot_fixed_height_probabilities <- function(conditional_response) {
  if (!base::inherits(conditional_response, "structure_conditional_response")) {
    base::stop("conditional_response must come from analyze_msf_se_height_betweenness().", call. = FALSE)
  }
  data <- conditional_response$response
  height_name <- conditional_response$config$height_variable
  betweenness_name <- conditional_response$config$betweenness_variable
  long <- base::rbind(
    base::data.frame(data, outcome = "msf", probability = data$mean_probability_msf),
    base::data.frame(data, outcome = "se", probability = data$mean_probability_se),
    base::data.frame(data, outcome = "si", probability = data$mean_probability_si)
  )
  long$height_label <- base::paste0(
    height_name, " q", base::sprintf("%.2f", long$height_quantile),
    " = ", base::formatC(long$height_value, digits = 3, format = "fg")
  )
  ggplot2::ggplot(long, ggplot2::aes(
    x = .data$betweenness_value, y = .data$probability, color = .data$outcome
  )) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~height_label) +
    ggplot2::labs(
      x = betweenness_name, y = "Mean predicted probability", color = "Outcome",
      title = "Three-class model response at equal fixed heights"
    ) +
    ggplot2::theme_minimal()
}

#' Plot held-out MSF-versus-SE performance by height band
#'
#' @param height_performance Output from evaluate_msf_se_height_bins().
#' @return A ggplot object. Interpret bands with low class counts cautiously.
plot_msf_se_height_performance <- function(height_performance) {
  required <- base::c("height_band", "recall_msf", "recall_se", "balanced_accuracy", "n_msf", "n_se")
  if (!base::is.data.frame(height_performance) ||
      base::length(base::setdiff(required, base::names(height_performance)))) {
    base::stop("height_performance is missing required held-out performance columns.", call. = FALSE)
  }
  long <- base::rbind(
    base::data.frame(height_band = height_performance$height_band,
      metric = "MSF recall", value = height_performance$recall_msf),
    base::data.frame(height_band = height_performance$height_band,
      metric = "SE recall", value = height_performance$recall_se),
    base::data.frame(height_band = height_performance$height_band,
      metric = "Balanced accuracy", value = height_performance$balanced_accuracy)
  )
  ggplot2::ggplot(long, ggplot2::aes(
    x = .data$height_band, y = .data$value, group = .data$metric, color = .data$metric
  )) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(), limits = base::c(0, 1)) +
    ggplot2::labs(
      x = "Observed height band", y = "Outer held-out performance", color = NULL,
      title = "MSF versus SE performance by observed height"
    ) +
    ggplot2::theme_minimal()
}

# Joint partial dependence for the two variables driving the MSF/SE boundary.
#
# This stays in dev/ deliberately: it is an exploratory interpretation script,
# not a package interface. It uses pdp to create native `partial` objects, but
# calculates each cell exactly as the earlier manual workflow did: replace zmax
# and understory betweenness for every reference record, predict all records,
# then average the selected class probability.
#
# Install once if needed: install.packages("pdp")
pdp_zmax_betweenness <- function(
    model,
    reference_data = NULL,
    height_variable = "zmax",
    betweenness_variable = "understory_mean_betweenness",
    grid_quantiles = base::seq(0.05, 0.95, by = 0.05),
    reference_classes = NULL) {
  if (!requireNamespace("pdp", quietly = TRUE)) {
    base::stop("Install the pdp package first: install.packages('pdp').", call. = FALSE)
  }
  if (base::is.character(model) && base::length(model) == 1L) model <- base::readRDS(model)
  if (base::inherits(model, "structure_gbm")) {
    caret_model <- model$model
    predictors <- model$selected_predictors
  } else if (base::inherits(model, "train") && base::identical(model$method, "gbm")) {
    caret_model <- model
    predictors <- model$coefnames
    if (base::is.null(predictors) || !base::length(predictors)) {
      predictors <- base::setdiff(base::names(model$trainingData), ".outcome")
    }
  } else {
    base::stop("model must be a structure_gbm bundle, caret GBM train object, or an RDS path.", call. = FALSE)
  }
  variables <- base::c(height_variable, betweenness_variable)
  if (base::is.null(reference_data)) reference_data <- caret_model$trainingData
  if (!base::is.data.frame(reference_data)) {
    base::stop("reference_data is NULL but the model does not retain trainingData; supply the complete training predictor data.", call. = FALSE)
  }
  missing <- base::setdiff(base::c(predictors, variables), base::names(reference_data))
  if (base::length(missing)) {
    base::stop(
      "reference_data is missing selected predictor(s): ", base::paste(missing, collapse = ", "),
      ". Supply the complete model training data, not a zmax/betweenness-only table.", call. = FALSE
    )
  }
  if (!base::is.numeric(grid_quantiles) || base::length(grid_quantiles) < 2L ||
      base::anyNA(grid_quantiles) || base::any(!base::is.finite(grid_quantiles)) ||
      base::any(grid_quantiles < 0 | grid_quantiles > 1) || base::anyDuplicated(grid_quantiles)) {
    base::stop("grid_quantiles must contain at least two distinct values from 0 through 1.", call. = FALSE)
  }
  if (!base::is.null(reference_classes) && ".outcome" %in% base::names(reference_data)) {
    reference_data <- reference_data[
      base::as.character(reference_data$.outcome) %in% reference_classes,
      , drop = FALSE
    ]
  }
  if (!base::nrow(reference_data)) {
    base::stop("No reference rows remain after applying reference_classes.", call. = FALSE)
  }
  absent_from_model <- base::setdiff(variables, predictors)
  if (base::length(absent_from_model)) {
    base::warning(
      base::paste0(
        base::paste(absent_from_model, collapse = ", "),
        " is not among the fitted model predictors; its partial-dependence response will be flat."
      ), call. = FALSE
    )
  }
  # pdp needs the partial-dependence variables in train even if a caret model
  # records a narrower coefnames vector. caret::predict.train() ignores any
  # additional columns when it creates predictions.
  reference_data <- reference_data[, base::unique(base::c(predictors, variables)), drop = FALSE]
  height_values <- base::unique(stats::quantile(
    reference_data[[height_variable]], probs = grid_quantiles, names = FALSE, type = 7
  ))
  betweenness_values <- base::unique(stats::quantile(
    reference_data[[betweenness_variable]], probs = grid_quantiles, names = FALSE, type = 7
  ))
  if (base::length(height_values) < 2L || base::length(betweenness_values) < 2L) {
    base::stop("The requested grid has fewer than two distinct observed values for zmax or understory betweenness.", call. = FALSE)
  }
  grid <- base::expand.grid(
    height_values,
    betweenness_values,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  base::names(grid) <- variables
  probability <- function(class_name) {
    pdp::partial(
      object = caret_model,
      pred.var = variables,
      train = reference_data,
      pred.grid = grid,
      pred.fun = function(object, newdata) {
        base::mean(caret::predict.train(object, newdata = newdata, type = "prob")[[class_name]])
      },
      progress = "none"
    )
  }
  msf <- probability("msf")
  se <- probability("se")
  si <- probability("si")
  output <- msf[, variables, drop = FALSE]
  output$p_msf <- msf$yhat
  output$p_se <- se$yhat
  output$p_si <- si$yhat
  output$msf_minus_se <- output$p_msf - output$p_se
  contrast <- msf
  contrast$yhat <- output$msf_minus_se
  base::structure(
    base::list(
      surface = output,
      partials = base::list(msf = msf, se = se, si = si, msf_minus_se = contrast),
      variables = variables
    ),
    class = "zmax_betweenness_pdp"
  )
}

#' Plot the actual pdp::partial result with pdp::plotPartial().
#'
#' For msf_minus_se, positive values favor MSF. This is a partial-dependence
#' model response, not observed performance and not a causal response.
plot_pdp_zmax_betweenness <- function(
    pdp_result, response = base::c("msf_minus_se", "msf", "se", "si"),
    levelplot = TRUE, contour = TRUE, label_values = FALSE, label_every = 2L, ...) {
  response <- base::match.arg(response)
  if (!base::inherits(pdp_result, "zmax_betweenness_pdp")) {
    base::stop("pdp_result must come from pdp_zmax_betweenness(); rerun it after sourcing this script.", call. = FALSE)
  }
  if (!base::is.logical(label_values) || base::length(label_values) != 1L || base::is.na(label_values)) {
    base::stop("label_values must be TRUE or FALSE.", call. = FALSE)
  }
  # Numeric labels are unreadable when projected onto a wireframe. When
  # requested, retain this public helper but return a labeled 2-D surface.
  if (label_values) {
    surface_response <- base::switch(response,
      msf_minus_se = "msf_minus_se", msf = "p_msf", se = "p_se", si = "p_si"
    )
    return(plot_pdp_zmax_betweenness_surface(
      pdp_result, response = surface_response, labels = TRUE,
      label_every = label_every,
      transition_contour = identical(response, "msf_minus_se")
    ))
  }
  if (!requireNamespace("pdp", quietly = TRUE)) {
    base::stop("Install the pdp package first: install.packages('pdp').", call. = FALSE)
  }
  title <- if (identical(response, "msf_minus_se")) {
    "Partial dependence: P(msf) - P(se)"
  } else {
    base::paste0("Partial dependence: P(", response, ")")
  }
  pdp::plotPartial(
    object = pdp_result$partials[[response]], levelplot = levelplot, contour = contour,
    main = title, xlab = pdp_result$variables[[1L]], ylab = pdp_result$variables[[2L]], ...
  )
}

#' Plot the same joint PDP result with ggplot for reports.
#'
#' This alternative is useful when a ggplot object is needed for theming or
#' composition. For the native pdp visualization, use plot_pdp_zmax_betweenness().
plot_pdp_zmax_betweenness_surface <- function(
    pdp_result, response = base::c("msf_minus_se", "p_msf", "p_se", "p_si"),
    labels = FALSE, label_every = 2L, transition_contour = TRUE) {
  response <- base::match.arg(response)
  if (!base::inherits(pdp_result, "zmax_betweenness_pdp")) {
    base::stop("pdp_result must come from pdp_zmax_betweenness(); rerun it after sourcing this script.", call. = FALSE)
  }
  if (!base::is.logical(labels) || base::length(labels) != 1L || base::is.na(labels) ||
      !base::is.logical(transition_contour) || base::length(transition_contour) != 1L ||
      base::is.na(transition_contour) || !base::is.numeric(label_every) ||
      base::length(label_every) != 1L || base::is.na(label_every) || label_every < 1L) {
    base::stop("labels and transition_contour must be TRUE/FALSE, and label_every must be at least 1.", call. = FALSE)
  }
  pdp_surface <- pdp_result$surface
  diverging <- identical(response, "msf_minus_se")
  plot <- ggplot2::ggplot(pdp_surface, ggplot2::aes(
    x = .data$understory_mean_betweenness, y = .data$zmax, fill = .data[[response]]
  )) +
    ggplot2::geom_tile() +
    ggplot2::geom_contour(
      data = pdp_surface,
      ggplot2::aes(
        x = .data$understory_mean_betweenness, y = .data$zmax, z = .data[[response]]
      ),
      inherit.aes = FALSE, color = "grey20", linewidth = 0.35
    ) +
    ggplot2::labs(
      x = "understory_mean_betweenness", y = "zmax",
      title = "Joint partial dependence of zmax and understory betweenness"
    ) +
    ggplot2::theme_minimal()
  if (diverging && transition_contour) {
    plot <- plot + ggplot2::geom_contour(
      data = pdp_surface,
      ggplot2::aes(
        x = .data$understory_mean_betweenness, y = .data$zmax, z = .data[[response]]
      ),
      inherit.aes = FALSE, breaks = 0, color = "black", linewidth = 1
    )
  }
  if (labels) {
    x_values <- base::sort(base::unique(pdp_surface$understory_mean_betweenness))
    y_values <- base::sort(base::unique(pdp_surface$zmax))
    label_data <- pdp_surface[
      pdp_surface$understory_mean_betweenness %in% x_values[base::seq(1L, base::length(x_values), by = label_every)] &
        pdp_surface$zmax %in% y_values[base::seq(1L, base::length(y_values), by = label_every)],
      , drop = FALSE
    ]
    label_data$value_label <- base::sprintf("%.2f", label_data[[response]])
    plot <- plot + ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(
        x = .data$understory_mean_betweenness, y = .data$zmax, label = .data$value_label
      ),
      inherit.aes = FALSE, size = 3, color = "black"
    )
  }
  if (diverging) {
    plot + ggplot2::scale_fill_gradient2(
      low = "#3b4cc0", mid = "white", high = "#b40426", midpoint = 0,
      name = "P(msf) - P(se)"
    )
  } else {
    plot + ggplot2::scale_fill_viridis_c(name = response)
  }
}
