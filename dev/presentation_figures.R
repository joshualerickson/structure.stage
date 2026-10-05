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
    screen = base::list(z = -20, x = -65, y = 0),
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

#' Plot held-out SE predictor distributions by model outcome.
#'
#' @param audit Result from audit_structure_predictions() applied to held-out
#'   data. Its row_id values must index data.
#' @param data The held-out records used to create audit.
#' @param metrics Numeric predictors to compare.
#' @return A ggplot object.
plot_heldout_se_prediction_profiles <- function(
    audit, data,
    metrics = base::c(
      "midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid",
      "understory_mean_betweenness", "zmax"
    )) {
  required <- base::c("row_id", "observed_class", "predicted_class")
  if (!base::is.data.frame(audit) || base::length(base::setdiff(required, base::names(audit))) ||
      !base::is.data.frame(data) || base::length(base::setdiff(metrics, base::names(data)))) {
    base::stop("audit and data must contain the required prediction columns and requested metrics.", call. = FALSE)
  }
  se_audit <- audit[audit$observed_class == "se", , drop = FALSE]
  if (!base::nrow(se_audit) || base::anyNA(se_audit$row_id) ||
      base::any(se_audit$row_id < 1L | se_audit$row_id > base::nrow(data))) {
    base::stop("audit must contain valid held-out SE row_id values.", call. = FALSE)
  }
  values <- data[base::as.integer(se_audit$row_id), metrics, drop = FALSE]
  group <- ifelse(
    se_audit$predicted_class == "se", "Correct SE",
    base::paste0("SE predicted as ", base::toupper(se_audit$predicted_class))
  )
  preferred_order <- base::c("Correct SE", "SE predicted as MSF", "SE predicted as SI")
  group_counts <- base::table(group)
  group_levels <- preferred_order[preferred_order %in% base::names(group_counts)]
  group_labels <- base::paste0(group_levels, "\n(n = ", base::unname(group_counts[group_levels]), ")")
  group <- base::factor(group, levels = group_levels, labels = group_labels)
  long <- base::data.frame(
    prediction_group = base::rep(group, times = base::length(metrics)),
    feature = base::rep(metrics, each = base::nrow(values)),
    value = base::unlist(values, use.names = FALSE),
    row.names = NULL
  )
  long$feature <- base::factor(long$feature, levels = metrics)
  colors <- base::c(
    "Correct SE" = "#666666",
    "SE predicted as MSF" = "#D55E00",
    "SE predicted as SI" = "#0072B2"
  )
  color_values <- colors[group_levels]
  base::names(color_values) <- group_labels
  ggplot2::ggplot(long, ggplot2::aes(
    x = .data$prediction_group, y = .data$value, fill = .data$prediction_group
  )) +
    ggplot2::geom_boxplot(width = 0.66, outlier.alpha = 0.25, linewidth = 0.35) +
    ggplot2::facet_wrap(~feature, scales = "free_y", ncol = 3) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::labs(
      x = NULL, y = "Observed predictor value",
      title = "Held-out SE records: correct and incorrect model predictions",
      caption = "Boxes summarize held-out records; colors identify the model prediction."
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey45"),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}

#' Save the held-out SE predictor comparison as a PNG.
#'
#' @inheritParams plot_heldout_se_prediction_profiles
#' @param path PNG file path to create.
#' @param width,height Output dimensions in inches.
#' @param dpi Output resolution.
#' @return The normalized output path, invisibly.
save_heldout_se_prediction_profiles <- function(
    audit, data, path, metrics = base::c(
      "midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid",
      "understory_mean_betweenness", "zmax"
    ), width = 11, height = 8, dpi = 400) {
  if (!base::dir.exists(base::dirname(path))) base::dir.create(base::dirname(path), recursive = TRUE)
  figure <- plot_heldout_se_prediction_profiles(audit, data, metrics = metrics)
  ggplot2::ggsave(path, plot = figure, width = width, height = height, dpi = dpi, bg = "white")
  base::normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' Plot held-out predictor profiles for one observed structural-stage class.
#'
#' @param audit Result from audit_structure_predictions() applied to held-out
#'   data. Its row_id values must index data.
#' @param data The held-out records used to create audit.
#' @param observed_class One of msf, se, or si.
#' @param metrics Numeric predictors to compare.
#' @return A ggplot object.
plot_heldout_class_prediction_profiles <- function(
    audit, data, observed_class,
    metrics = base::c(
      "midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid",
      "understory_mean_betweenness", "zmax"
    )) {
  classes <- base::c("msf", "se", "si")
  if (!base::is.character(observed_class) || base::length(observed_class) != 1L ||
      !observed_class %in% classes) {
    base::stop("observed_class must be one of msf, se, or si.", call. = FALSE)
  }
  required <- base::c("row_id", "observed_class", "predicted_class")
  if (!base::is.data.frame(audit) || base::length(base::setdiff(required, base::names(audit))) ||
      !base::is.data.frame(data) || base::length(base::setdiff(metrics, base::names(data)))) {
    base::stop("audit and data must contain the required prediction columns and requested metrics.", call. = FALSE)
  }
  class_audit <- audit[audit$observed_class == observed_class, , drop = FALSE]
  if (!base::nrow(class_audit) || base::anyNA(class_audit$row_id) ||
      base::any(class_audit$row_id < 1L | class_audit$row_id > base::nrow(data))) {
    base::stop("audit must contain valid row_id values for the observed class.", call. = FALSE)
  }
  values <- data[base::as.integer(class_audit$row_id), metrics, drop = FALSE]
  class_display <- base::toupper(observed_class)
  group <- ifelse(
    class_audit$predicted_class == observed_class,
    base::paste("Correct", class_display),
    base::paste0(class_display, " predicted as ", base::toupper(class_audit$predicted_class))
  )
  prediction_order <- base::c(observed_class, classes[classes != observed_class])
  group_levels <- base::c(
    base::paste("Correct", class_display),
    base::paste0(class_display, " predicted as ", base::toupper(prediction_order[prediction_order != observed_class]))
  )
  group_counts <- base::table(group)
  group_levels <- group_levels[group_levels %in% base::names(group_counts)]
  group_labels <- base::paste0(group_levels, "\n(n = ", base::unname(group_counts[group_levels]), ")")
  group <- base::factor(group, levels = group_levels, labels = group_labels)
  long <- base::data.frame(
    prediction_group = base::rep(group, times = base::length(metrics)),
    feature = base::rep(metrics, each = base::nrow(values)),
    value = base::unlist(values, use.names = FALSE),
    row.names = NULL
  )
  long$feature <- base::factor(long$feature, levels = metrics)
  class_colors <- base::c(msf = "#D55E00", se = "#009E73", si = "#0072B2")
  color_values <- base::vapply(group_levels, function(label) {
    if (identical(label, base::paste("Correct", class_display))) return("#666666")
    predicted <- base::tolower(base::sub(base::paste0("^", class_display, " predicted as "), "", label))
    class_colors[[predicted]]
  }, character(1L))
  if (base::anyNA(color_values)) {
    base::stop("Could not assign colors to one or more predicted classes.", call. = FALSE)
  }
  base::names(color_values) <- group_labels
  ggplot2::ggplot(long, ggplot2::aes(
    x = .data$prediction_group, y = .data$value, fill = .data$prediction_group
  )) +
    ggplot2::geom_boxplot(width = 0.66, outlier.alpha = 0.25, linewidth = 0.35) +
    ggplot2::facet_wrap(~feature, scales = "free_y", ncol = 3) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::labs(
      x = NULL, y = "Observed predictor value",
      title = base::paste("Held-out", class_display, "records: correct and incorrect model predictions"),
      caption = "Boxes summarize held-out records; colors identify the model prediction."
    ) +
    ggplot2::theme_classic(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      strip.background = ggplot2::element_rect(fill = "grey95", color = "grey45"),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
}

#' Save a held-out structural-stage predictor profile comparison as a PNG.
#'
#' @inheritParams plot_heldout_class_prediction_profiles
#' @param path PNG file path to create.
#' @param width,height Output dimensions in inches.
#' @param dpi Output resolution.
#' @return The normalized output path, invisibly.
save_heldout_class_prediction_profiles <- function(
    audit, data, observed_class, path, metrics = base::c(
      "midstory_mean_degree", "n_gt_6_1", "n_strata_low_mid",
      "understory_mean_betweenness", "zmax"
    ), width = 11, height = 8, dpi = 400) {
  if (!base::dir.exists(base::dirname(path))) base::dir.create(base::dirname(path), recursive = TRUE)
  figure <- plot_heldout_class_prediction_profiles(audit, data, observed_class, metrics)
  ggplot2::ggsave(path, plot = figure, width = width, height = height, dpi = dpi, bg = "white")
  base::normalizePath(path, winslash = "/", mustWork = FALSE)
}
