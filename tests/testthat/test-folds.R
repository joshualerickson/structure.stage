testthat::test_that("spatial folds are deterministic and keep polygons intact", {
  data <- synthetic_training()
  base::set.seed(98)
  previous <- .Random.seed
  folds <- structure.stage::build_spatial_folds(data, 18, k = 3)
  testthat::expect_identical(.Random.seed, previous)
  testthat::expect_identical(folds, structure.stage::build_spatial_folds(data, 18, k = 3))
  testthat::expect_equal(folds$config$centers, 12)
  for (i in base::seq_along(folds$index)) {
    train <- folds$index[[i]]
    test <- folds$indexOut[[i]]
    testthat::expect_length(base::intersect(data$records$polygon_id[train],
                                           data$records$polygon_id[test]), 0)
    testthat::expect_setequal(base::c(train, test), base::seq_len(base::nrow(data$records)))
  }
  testthat::expect_true(base::all(folds$class_counts > 0))
  testthat::expect_error(structure.stage::build_spatial_folds(data, 1000, k = 3), "Cluster count")
})

testthat::test_that("fit rejects stale indices and polygon leakage before training", {
  data <- synthetic_training()
  folds <- structure.stage::build_spatial_folds(data, 18, k = 3)
  reordered <- data
  reordered$records <- reordered$records[base::rev(base::seq_len(base::nrow(data$records))), ]
  testthat::expect_error(structure.stage::fit_structure_gbm(reordered, folds), "sample order")
  bad <- folds
  test <- folds$indexOut[[1]][1]
  train <- folds$index[[1]][1]
  bad$indexOut[[1]][1] <- train
  bad$index[[1]][1] <- test
  testthat::expect_error(structure.stage::fit_structure_gbm(data, bad), "Polygon leakage")
})
