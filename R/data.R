#' Legacy lynx structural-stage label mapping
#'
#' Reproduces the source-label merge in `dev/lynx_model.R`. In particular,
#' Multi-Story Non-foraging maps to `se`; this is a historical modeling choice,
#' not a newly established ecological equivalence.
#' @return A named character vector from provider labels to model labels.
#' @export
#' @examples
#' structure.stage::lynx_class_map()
lynx_class_map <- function() {
  base::c("Multi-Story Foraging" = "msf", "Multi-Story Non-foraging" = "se",
          "Stem Exclusion" = "se", "Stand Initiation" = "si")
}

#' Prepare explicit training records for the lynx GBM workflow
#'
#' Harmonizes stand-derived sample labels and optionally derives `bt_diff`.
#' No sampling, imputation, scaling, or feature selection occurs here. Metadata
#' must already be supplied by a provider-specific preparation step. Polygon
#' IDs must be globally unique across sources; no IDs or CRS are inferred.
#'
#' @param data A data frame with `sample_id`, `polygon_id`, `source_id`,
#'   `acquisition_id`, `x`, `y`, `structure_stage`, and predictor columns.
#' @param predictor_schema Data frame with one row per predictor, in explicit
#'   order: character `name`, `units`, `nodata`, `interpretation`,
#'   `source_version`, and positive numeric `resolution` in CRS units. Nodata
#'   sentinels must be resolved before calling; this function rejects NA/Inf
#'   and does not interpret the descriptive `nodata` field automatically.
#' @param crs Projected coordinate reference system accepted by [sf::st_crs()].
#' @param data_version Nonempty identifier for this input dataset version.
#' @param class_map Named character mapping of input labels to model labels.
#' @param class_levels Explicit model class order. Defaults to the legacy order.
#' @param unknown_labels Either `"error"` (default) or `"drop"`. Dropped sample
#'   IDs and original labels are recorded in the audit. `"drop"` reproduces the
#'   legacy exclusion of other source classes, including missing labels.
#' @param derive_bt_diff Derive midstory minus understory mean betweenness.
#'   If `bt_diff` exists, it must agree with the derived values.
#' @return A `structure_data` list with unsampled `records`, predictor `schema`,
#'   class mapping/order, CRS WKT, dataset version, preparation settings and audit.
#' @export
prepare_lynx_data <- function(data, predictor_schema, crs, data_version,
                              class_map = lynx_class_map(),
                              class_levels = base::c("msf", "se", "si"),
                              unknown_labels = base::c("error", "drop"),
                              derive_bt_diff = TRUE) {
  unknown_labels <- base::match.arg(unknown_labels)
  metadata <- base::c("sample_id", "polygon_id", "source_id", "acquisition_id",
                      "x", "y", "structure_stage")
  .columns(data, metadata)
  .scalar_text(data_version, "data_version")
  schema <- .schema(predictor_schema)
  if (base::any(schema$name %in% metadata)) {
    base::stop("Predictor schema must not include metadata or the response.", call. = FALSE)
  }
  if (!base::is.character(class_levels) || base::length(class_levels) < 2L ||
      base::anyNA(class_levels) || base::anyDuplicated(class_levels) ||
      base::any(!base::nzchar(class_levels)) ||
      base::any(base::make.names(class_levels) != class_levels)) {
    base::stop("class_levels must be distinct, valid R class names.", call. = FALSE)
  }
  if (!base::is.character(class_map) || base::is.null(base::names(class_map)) ||
      base::anyNA(class_map) || base::anyNA(base::names(class_map)) ||
      base::any(!base::nzchar(base::names(class_map))) ||
      base::anyDuplicated(base::names(class_map)) ||
      !base::setequal(base::unname(class_map), class_levels)) {
    base::stop("class_map must have unique source names and cover exactly class_levels.", call. = FALSE)
  }
  crs <- sf::st_crs(crs)
  if (base::is.na(crs) || !base::identical(sf::st_is_longlat(crs), FALSE)) {
    base::stop("A valid projected CRS is required for coordinate clustering.", call. = FALSE)
  }
  if (!base::is.logical(derive_bt_diff) || base::length(derive_bt_diff) != 1L ||
      base::is.na(derive_bt_diff)) {
    base::stop("derive_bt_diff must be TRUE or FALSE.", call. = FALSE)
  }
  data <- base::as.data.frame(data)
  for (column in metadata[1:4]) {
    if (!base::is.character(data[[column]]) || base::anyNA(data[[column]]) ||
        base::any(!base::nzchar(base::trimws(data[[column]])))) {
      base::stop(column, " must contain nonempty character IDs.", call. = FALSE)
    }
  }
  if (base::anyDuplicated(data$sample_id)) {
    base::stop("sample_id must be unique across inputs.", call. = FALSE)
  }
  mapped <- base::unname(class_map[base::as.character(data$structure_stage)])
  unknown <- base::is.na(mapped)
  if (base::any(unknown) && unknown_labels == "error") {
    base::stop("Unmapped structure_stage labels; provide a mapping or explicitly choose 'drop'.",
               call. = FALSE)
  }
  excluded <- data[unknown, base::c("sample_id", "structure_stage"), drop = FALSE]
  input_n <- base::nrow(data)
  data <- data[!unknown, , drop = FALSE]
  data$structure_stage <- base::factor(mapped[!unknown], levels = class_levels)
  if (derive_bt_diff) {
    .numeric_columns(data, base::c("midstory_mean_betweenness", "understory_mean_betweenness"))
    derived <- data$midstory_mean_betweenness - data$understory_mean_betweenness
    if ("bt_diff" %in% base::names(data) &&
        !base::isTRUE(base::all.equal(data$bt_diff, derived, check.attributes = FALSE))) {
      base::stop("Existing bt_diff disagrees with the derived difference.", call. = FALSE)
    }
    data$bt_diff <- derived
  }
  .numeric_columns(data, base::c("x", "y", schema$name))
  # Each globally unique polygon has one harmonized stand label and source.
  groups <- base::split(base::seq_len(base::nrow(data)), data$polygon_id)
  for (idx in groups) {
    if (base::length(base::unique(data$structure_stage[idx])) != 1L ||
        base::length(base::unique(data$source_id[idx])) != 1L) {
      base::stop("Each polygon_id must identify one source and harmonized stand label.", call. = FALSE)
    }
  }
  records <- data[, base::c(metadata, schema$name), drop = FALSE]
  base::rownames(records) <- NULL
  out <- base::structure(base::list(
    records = records, schema = schema, class_map = class_map,
    class_levels = class_levels, crs = crs$wkt, data_version = data_version,
    preparation = base::list(derive_bt_diff = derive_bt_diff, unknown_labels = unknown_labels),
    audit = base::list(input_rows = input_n, excluded = excluded,
                       class_counts = base::table(records$structure_stage))
  ), class = "structure_data")
  .training_data(out)
  out
}

#' Sample training records within structural classes
#'
#' Samples without replacement, preserving IDs and explicit class order.
#' This changes training prevalence; it does not create independent observations
#' within a stand. The caller's random-number state is restored.
#' @param data Prepared data from [prepare_lynx_data()].
#' @param n_per_class Positive integer target per class, or NULL to keep all rows.
#' @param seed Nonnegative integer sampling seed.
#' @param small_classes `"error"` if a class is too small, or `"all"` to retain
#'   all its rows (the legacy capped-count behavior).
#' @return A new `structure_data` object with sampling settings and selected IDs
#'   in its audit. The input is unchanged; retain it for future retraining.
#' @export
sample_structure_data <- function(data, n_per_class = 2000L, seed = 124L,
                                  small_classes = base::c("error", "all")) {
  .training_data(data)
  .integer_scalar(seed, "seed", 0L)
  small_classes <- base::match.arg(small_classes)
  if (!base::is.null(n_per_class)) {
    .integer_scalar(n_per_class, "n_per_class")
    counts <- base::table(data$records$structure_stage)
    if (small_classes == "error" && base::any(counts < n_per_class)) {
      base::stop("A class has fewer than n_per_class rows; explicitly choose small_classes = 'all'.",
                 call. = FALSE)
    }
    idx <- withr::with_seed(seed, base::unlist(base::lapply(data$class_levels, function(label) {
      rows <- base::which(data$records$structure_stage == label)
      rows[base::sample.int(base::length(rows), base::min(n_per_class, base::length(rows)))]
    }), use.names = FALSE))
    data$records <- data$records[idx, , drop = FALSE]
    base::rownames(data$records) <- NULL
  }
  data$audit$sampling <- base::list(n_per_class = n_per_class, seed = seed,
    small_classes = small_classes, selected_ids = data$records$sample_id,
    class_counts = base::table(data$records$structure_stage))
  data
}
