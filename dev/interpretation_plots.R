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
