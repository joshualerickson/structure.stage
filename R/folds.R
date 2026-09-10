#' Construct audited spatial folds for structural-stage samples
#'
#' The default clusters polygon mean sample coordinates, then assigns all samples
#' from a polygon to its cluster. These are sample-coordinate centroids, not
#' polygon geometry centroids. This is an explicit leakage-control change from
#' the original point k-means workflow. `"sample"` uses historical point
#' clustering but rejects any resulting train/assessment polygon overlap.
#' Neither mode guarantees distance buffers or landscape independence.
#'
#' @param data Prepared (optionally sampled) `structure_data`.
#' @param samples_per_cluster Positive target controlling the cluster count:
#'   `floor(n_samples / samples_per_cluster)`. It is not a distance.
#' @param k Number of spatial folds, at least two.
#' @param cluster_unit `"polygon"` (default) or `"sample"`.
#' @param cluster_seed,fold_seed Nonnegative integer seeds for k-means and CAST.
#' @param nstart Positive number of k-means starts; one matches the legacy default.
#' @return A `structure_folds` list containing caret `index` and `indexOut`,
#'   row-aligned cluster and fold assignments, sample keys, class counts per fold,
#'   and the realized clustering configuration. All folds must contain all
#'   configured classes in training and assessment.
#' @export
build_spatial_folds <- function(data, samples_per_cluster = 400,
                                k = 10L, cluster_unit = base::c("polygon", "sample"),
                                cluster_seed = 124L, fold_seed = 1234L, nstart = 1L) {
  .training_data(data)
  cluster_unit <- base::match.arg(cluster_unit)
  .integer_scalar(k, "k", 2L)
  .integer_scalar(cluster_seed, "cluster_seed", 0L)
  .integer_scalar(fold_seed, "fold_seed", 0L)
  .integer_scalar(nstart, "nstart")
  if (!base::is.numeric(samples_per_cluster) || base::length(samples_per_cluster) != 1L ||
      !base::is.finite(samples_per_cluster) || samples_per_cluster <= 0) {
    base::stop("samples_per_cluster must be positive and finite.", call. = FALSE)
  }
  records <- data$records
  if (cluster_unit == "polygon") {
    coordinates <- stats::aggregate(records[, base::c("x", "y"), drop = FALSE],
      by = base::list(polygon_id = records$polygon_id), FUN = base::mean)
    row_map <- base::match(records$polygon_id, coordinates$polygon_id)
    coordinates <- coordinates[, base::c("x", "y"), drop = FALSE]
  } else {
    coordinates <- records[, base::c("x", "y"), drop = FALSE]
    row_map <- base::seq_len(base::nrow(records))
  }
  centers <- base::floor(base::nrow(records) / samples_per_cluster)
  # stats::kmeans requires fewer centers than rows for its default algorithm.
  if (centers < k || centers >= base::nrow(coordinates) ||
      centers > base::nrow(base::unique(coordinates))) {
    base::stop("Cluster count must be >= k, less than the number of clustering units, ",
               "and no larger than the number of distinct coordinates; revise samples_per_cluster.",
               call. = FALSE)
  }
  km <- withr::with_seed(cluster_seed, stats::kmeans(coordinates,
    centers = base::as.integer(centers), nstart = nstart))
  if (!base::is.null(km$ifault) && km$ifault != 0L) {
    base::stop("k-means did not converge; revise clustering settings.", call. = FALSE)
  }
  cluster <- km$cluster[row_map]
  indices <- withr::with_seed(fold_seed, CAST::CreateSpacetimeFolds(
    base::data.frame(cluster = cluster), spacevar = "cluster", k = k, seed = fold_seed))
  base::names(indices$index) <- base::names(indices$indexOut) <-
    base::paste0("Fold", base::seq_len(k))
  fold <- base::integer(base::nrow(records))
  for (i in base::seq_len(k)) fold[indices$indexOut[[i]]] <- i
  out <- base::structure(base::list(
    index = indices$index, indexOut = indices$indexOut,
    sample_key = .fold_key(data), assignments = base::data.frame(
      sample_id = records$sample_id, polygon_id = records$polygon_id,
      cluster = base::unname(cluster), fold = fold),
    class_counts = base::table(fold = fold, class = records$structure_stage),
    centers = km$centers,
    config = base::list(samples_per_cluster = samples_per_cluster,
      centers = centers, k = k, cluster_unit = cluster_unit,
      cluster_seed = cluster_seed, fold_seed = fold_seed, nstart = nstart)
  ), class = "structure_folds")
  .validate_folds(data, out)
  out
}
