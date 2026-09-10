# Internal validators intentionally reject implicit coercion and imputation.
.scalar_text <- function(x, name) {
  if (!base::is.character(x) || base::length(x) != 1L ||
      base::is.na(x) || !base::nzchar(base::trimws(x))) {
    base::stop(name, " must be one nonempty string.", call. = FALSE)
  }
}

.integer_scalar <- function(x, name, minimum = 1L) {
  if (!base::is.numeric(x) || base::length(x) != 1L ||
      !base::is.finite(x) || x < minimum || x > .Machine$integer.max ||
      x != base::floor(x)) {
    base::stop(name, " must be an integer >= ", minimum, ".", call. = FALSE)
  }
}

.columns <- function(data, columns) {
  if (!base::is.data.frame(data) || base::anyDuplicated(base::names(data))) {
    base::stop("Expected a data frame with unique column names.", call. = FALSE)
  }
  missing <- base::setdiff(columns, base::names(data))
  if (base::length(missing)) {
    base::stop("Missing columns: ", base::paste(missing, collapse = ", "), call. = FALSE)
  }
}

.schema <- function(schema) {
  .columns(schema, base::c("name", "units", "resolution", "nodata",
                          "interpretation", "source_version"))
  if (!base::nrow(schema) || base::anyDuplicated(schema$name)) {
    base::stop("Predictor schema must contain unique predictor names.", call. = FALSE)
  }
  for (column in base::c("name", "units", "nodata", "interpretation", "source_version")) {
    x <- schema[[column]]
    if (!base::is.character(x) || base::anyNA(x) ||
        base::any(!base::nzchar(base::trimws(x)))) {
      base::stop("Schema ", column, " must contain nonempty strings.", call. = FALSE)
    }
  }
  if (!base::is.numeric(schema$resolution) ||
      base::any(!base::is.finite(schema$resolution) | schema$resolution <= 0)) {
    base::stop("Schema resolution must be positive and finite (in CRS units).", call. = FALSE)
  }
  if (base::any(base::make.names(schema$name) != schema$name)) {
    base::stop("Predictor names must be syntactically valid R names.", call. = FALSE)
  }
  base::as.data.frame(schema, stringsAsFactors = FALSE)
}

.numeric_columns <- function(data, columns) {
  .columns(data, columns)
  for (column in columns) {
    x <- data[[column]]
    if (!base::is.numeric(x) || base::any(!base::is.finite(x))) {
      base::stop("Column '", column, "' must be numeric and finite; resolve nodata explicitly.",
                 call. = FALSE)
    }
  }
}

.training_data <- function(data) {
  if (!base::inherits(data, "structure_data")) {
    base::stop("Use prepare_lynx_data() to create training data.", call. = FALSE)
  }
  .numeric_columns(data$records, base::c("x", "y", data$schema$name))
  if (base::anyDuplicated(data$records$sample_id)) {
    base::stop("sample_id must be unique.", call. = FALSE)
  }
  counts <- base::table(data$records$structure_stage)
  if (base::any(counts == 0L)) {
    base::stop("Every configured class must have samples.", call. = FALSE)
  }
  base::invisible(data)
}

.fold_key <- function(data) {
  data$records[, base::c("sample_id", "polygon_id", "source_id", "acquisition_id",
                        "x", "y", "structure_stage"), drop = FALSE]
}

.validate_folds <- function(data, folds) {
  .training_data(data)
  if (!base::inherits(folds, "structure_folds") ||
      !base::identical(folds$sample_key, .fold_key(data))) {
    base::stop("Folds do not match training sample order, labels, or metadata.", call. = FALSE)
  }
  n <- base::nrow(data$records)
  if (base::length(folds$index) < 2L ||
      base::length(folds$index) != base::length(folds$indexOut)) {
    base::stop("At least two paired training/assessment folds are required.", call. = FALSE)
  }
  for (i in base::seq_along(folds$index)) {
    train <- folds$index[[i]]
    test <- folds$indexOut[[i]]
    for (idx in base::list(train, test)) {
      if (!base::is.numeric(idx) || !base::length(idx) || base::anyNA(idx) ||
          base::any(idx != base::floor(idx) | idx < 1L | idx > n) || base::anyDuplicated(idx)) {
        base::stop("Invalid or empty fold indices.", call. = FALSE)
      }
    }
    if (!base::setequal(train, base::setdiff(base::seq_len(n), test))) {
      base::stop("Training and assessment indices must be disjoint complements.", call. = FALSE)
    }
    if (base::length(base::intersect(data$records$polygon_id[train],
                                    data$records$polygon_id[test]))) {
      base::stop("Polygon leakage: a polygon occurs in training and assessment.", call. = FALSE)
    }
    for (idx in base::list(train, test)) {
      if (base::any(base::table(data$records$structure_stage[idx]) == 0L)) {
        base::stop("Every training and assessment fold must contain every class; revise spatial folds.",
                   call. = FALSE)
      }
    }
  }
  held_out <- base::sort(base::as.integer(base::unlist(folds$indexOut)))
  if (!base::identical(held_out, base::seq_len(n))) {
    base::stop("Each sample must be assessed exactly once.", call. = FALSE)
  }
  base::invisible(folds)
}
