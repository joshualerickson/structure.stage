# Presentation-ready structural-stage interpretation figures.
#
# Source dev/interpretation_plots.R before using the 3-D PDP exporter. These
# helpers deliberately remain in dev/: they format a specific msf/se/si
# interpretation workflow rather than defining a general package interface.

#' Export one clean 3-D PDP image
#'
#' @param pdp_result Result from pdp_zmax_betweenness().
#' @param response One of msf, se, si, or msf_minus_se.
#' @param path PNG file path to create.
#' @param width,height Output dimensions in inches.
#' @param dpi Output resolution.
#' @return The normalized output path, invisibly.
save_presentation_pdp_3d <- function(
    pdp_result, response = base::c("msf", "se", "si", "msf_minus_se"),
    path, width = 10, height = 8, dpi = 400) {
  response <- base::match.arg(response)
  if (!base::exists("plot_pdp_zmax_betweenness", mode = "function")) {
    base::stop("Source dev/interpretation_plots.R before saving PDP figures.", call. = FALSE)
  }
  if (!base::is.character(path) || base::length(path) != 1L || base::is.na(path)) {
    base::stop("path must be one PNG file path.", call. = FALSE)
  }
  if (!base::dir.exists(base::dirname(path))) base::dir.create(base::dirname(path), recursive = TRUE)
  if (!base::is.numeric(width) || !base::is.numeric(height) || !base::is.numeric(dpi) ||
      width <= 0 || height <= 0 || dpi <= 0) {
    base::stop("width, height, and dpi must be positive numbers.", call. = FALSE)
  }
  palette <- if (identical(response, "msf_minus_se")) {
    grDevices::hcl.colors(101L, palette = "Blue-Red 3")
  } else {
    grDevices::hcl.colors(101L, palette = "Viridis")
  }
  response_label <- base::switch(response,
    msf = "P(MSF)", se = "P(SE)", si = "P(SI)",
    msf_minus_se = "P(MSF) - P(SE)"
  )
  grDevices::png(path, width = width, height = height, units = "in", res = dpi,
    type = "cairo", bg = "white")
  base::on.exit(grDevices::dev.off(), add = TRUE)
  figure <- plot_pdp_zmax_betweenness(
    pdp_result, response = response, levelplot = FALSE, contour = FALSE,
    drape = TRUE, colorkey = TRUE,
    col.regions = palette,
    screen = base::list(z = -55, x = -65, y = 0),
    scales = base::list(arrows = FALSE, distance = base::c(1.2, 1.2, 1.4)),
    zlab = response_label,
    par.settings = lattice::simpleTheme(col = "grey20", lwd = 0.6)
  )
  base::print(figure)
  base::normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Create a clean fixed-height probability plot
#'
#' @param fixed_height Result from analyze_msf_se_height_betweenness().
#' @return A ggplot object.
plot_presentation_fixed_height_probabilities <- function(fixed_height) {
  if (!base::inherits(fixed_height, "structure_conditional_response")) {
    base::stop("fixed_height must come from analyze_msf_se_height_betweenness().", call. = FALSE)
  }
  data <- fixed_height$response
  long <- base::rbind(
    base::data.frame(data, outcome = "MSF", probability = data$mean_probability_msf),
    base::data.frame(data, outcome = "SE", probability = data$mean_probability_se),
    base::data.frame(data, outcome = "SI", probability = data$mean_probability_si)
  )
  height_name <- fixed_height$config$height_variable
  betweenness_name <- fixed_height$config$betweenness_variable
  long$height_label <- base::paste0(
    height_name, " = ", base::formatC(long$height_value, digits = 3, format = "fg")
  )
  long$outcome <- base::factor(long$outcome, levels = base::c("MSF", "SE", "SI"))
  ggplot2::ggplot(long, ggplot2::aes(
    x = .data$betweenness_value, y = .data$probability, color = .data$outcome
  )) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::facet_wrap(~height_label, nrow = 1) +
    ggplot2::scale_color_manual(values = base::c(MSF = "#D55E00", SE = "#009E73", SI = "#0072B2")) +
    ggplot2::scale_y_continuous(limits = base::c(0, 1), breaks = base::seq(0, 1, by = 0.2)) +
    ggplot2::labs(
      x = betweenness_name, y = "Mean predicted probability", color = "Class",
      title = "Structural-stage probabilities across understory betweenness"
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Save the presentation fixed-height probability plot as a PNG.
#'
#' @param fixed_height Result from analyze_msf_se_height_betweenness().
#' @param path PNG file path to create.
#' @param width,height Output dimensions in inches.
#' @param dpi Output resolution.
#' @return The normalized output path, invisibly.
save_presentation_fixed_height_probabilities <- function(
    fixed_height, path, width = 12, height = 4.5, dpi = 400) {
  if (!base::dir.exists(base::dirname(path))) base::dir.create(base::dirname(path), recursive = TRUE)
  figure <- plot_presentation_fixed_height_probabilities(fixed_height)
  ggplot2::ggsave(path, plot = figure, width = width, height = height, dpi = dpi, bg = "white")
  base::normalizePath(path, winslash = "/", mustWork = FALSE)
}
