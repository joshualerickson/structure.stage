#' Plot MSF-versus-SE response as betweenness changes at equal fixed heights
#'
#' @param conditional_response Result from
#'   [analyze_msf_se_height_betweenness()].
#' @return A ggplot object. Positive values favor MSF relative to SE.
#' @export
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
    x = rlang::.data$betweenness_value, y = rlang::.data$mean_msf_minus_se,
    color = rlang::.data$height_label
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
#' Keeps SI visible so a change in the MSF-versus-SE contrast is not interpreted
#' as an MSF gain when probability actually moves to SI.
#'
#' @param conditional_response Result from
#'   [analyze_msf_se_height_betweenness()].
#' @return A ggplot object showing MSF, SE, and SI probability responses.
#' @export
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
    x = rlang::.data$betweenness_value, y = rlang::.data$probability,
    color = rlang::.data$outcome
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
#' @param height_performance Output from [evaluate_msf_se_height_bins()].
#' @return A ggplot object. Interpret bands with low class counts cautiously.
#' @export
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
    x = rlang::.data$height_band, y = rlang::.data$value,
    group = rlang::.data$metric, color = rlang::.data$metric
  )) +
    ggplot2::geom_line() + ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_y_continuous(
      labels = function(value) base::paste0(base::round(100 * value), "%"),
      limits = base::c(0, 1)
    ) +
    ggplot2::labs(
      x = "Observed height band", y = "Outer held-out performance", color = NULL,
      title = "MSF versus SE performance by observed height"
    ) +
    ggplot2::theme_minimal()
}
