testthat::test_that("legacy multiclass summary has the expected values", {
  labels <- base::factor(base::rep(base::c("msf", "se", "si"), each = 3))
  perfect <- structure.stage::structure_summary(base::data.frame(obs = labels, pred = labels))
  testthat::expect_equal(perfect, base::c(Accuracy = 1, Kappa = 1, J = 1))
  guessed <- base::factor(base::rep("msf", 9), levels = base::levels(labels))
  constant <- structure.stage::structure_summary(base::data.frame(obs = labels, pred = guessed))
  testthat::expect_equal(constant, base::c(Accuracy = 1/3, Kappa = 0, J = 0), tolerance = 1e-10)
})

testthat::test_that("real CAST/GBM workflow yields a serializable model and validated predictions", {
  data <- synthetic_training()
  grid <- base::expand.grid(n.trees = 10L, interaction.depth = 1L,
                            shrinkage = 0.1, n.minobsinnode = 2L)
  base::set.seed(76)
  previous <- .Random.seed
  run <- structure.stage::run_lynx_workflow(data, cluster_sizes = 18,
    n_per_class = NULL, k = 3, tune_grid = grid)
  testthat::expect_identical(.Random.seed, previous)
  testthat::expect_s3_class(run, "lynx_workflow")
  model <- run$models[[1]]
  testthat::expect_s3_class(model$model, "train")
  testthat::expect_identical(model$assessment, "selection_resampling_only")
  testthat::expect_true(base::all(model$selected_predictors %in% data$schema$name))
  testthat::expect_gt(base::nrow(model$model$pred), 0)
  testthat::expect_equal(model$model$control$index, model$folds$index)
  path <- base::tempfile(fileext = ".rds")
  base::on.exit(base::unlink(path))
  base::saveRDS(model, path)
  restored <- base::readRDS(path)
  probabilities <- structure.stage::predict_structure_gbm(restored, data$records, data$schema)
  testthat::expect_identical(base::names(probabilities), data$class_levels)
  testthat::expect_equal(base::rowSums(probabilities), base::rep(1, base::nrow(data$records)), tolerance = 1e-8)
  reordered <- data$records[, base::rev(base::names(data$records)), drop = FALSE]
  testthat::expect_equal(structure.stage::predict_structure_gbm(model, reordered, data$schema), probabilities)
  classes <- structure.stage::predict_structure_gbm(model, data$records, data$schema, "raw")
  testthat::expect_identical(base::levels(classes), data$class_levels)
  testthat::expect_length(classes, base::nrow(data$records))
  bad_schema <- data$schema
  bad_schema$units[base::match(model$selected_predictors[1], bad_schema$name)] <- "wrong"
  testthat::expect_error(structure.stage::predict_structure_gbm(model, data$records, bad_schema), "schema differs")
  missing <- data$records[, base::setdiff(base::names(data$records), model$selected_predictors[1]), drop = FALSE]
  testthat::expect_error(structure.stage::predict_structure_gbm(model, missing, data$schema), "Missing columns")
  empty <- structure.stage::predict_structure_gbm(model, data$records[FALSE, ], data$schema)
  testthat::expect_equal(base::dim(empty), base::c(0L, 3L))
})

testthat::test_that("nested spatial CV supplies held-out predictions and a final all-data model", {
  data <- synthetic_training()
  grid <- base::expand.grid(n.trees = 10L, interaction.depth = 1L,
                            shrinkage = 0.1, n.minobsinnode = 2L)
  outer <- structure.stage::build_spatial_folds(data, 18, k = 3)
  result <- structure.stage::run_mscv_gbm(
    data, outer_folds = outer,
    selected_predictors = base::c("height", "cover", "bt_diff"),
    inner_samples_per_cluster = 18, inner_k = 3, tune_grid = grid
  )
  testthat::expect_s3_class(result, "structure_mscv")
  testthat::expect_identical(result$assessment, "fixed_predictors_outer_spatial_cv")
  testthat::expect_setequal(result$predictions$sample_id, data$records$sample_id)
  testthat::expect_equal(base::anyDuplicated(result$predictions$sample_id), 0L)
  testthat::expect_equal(base::rowSums(result$predictions[, data$class_levels]),
                         base::rep(1, base::nrow(data$records)), tolerance = 1e-8)
  testthat::expect_setequal(result$metrics$overall$metric,
    base::c("accuracy", "kappa", "balanced_accuracy", "macro_f1", "log_loss", "brier_score"))
  testthat::expect_equal(base::nrow(result$metrics$per_class), 3L)
  testthat::expect_equal(base::nrow(result$fold_metrics), 18L)
  height_performance <- structure.stage::evaluate_msf_se_height_bins(
    result, data, height_variable = "height", bins = 3L
  )
  testthat::expect_equal(base::nrow(height_performance), 3L)
  testthat::expect_equal(base::sum(height_performance$n), 2L * 72L)
  testthat::expect_true(base::all(height_performance$balanced_accuracy >= 0 &
                                  height_performance$balanced_accuracy <= 1))
  final <- structure.stage::fit_final_structure_gbm(
    data, base::c("height", "cover", "bt_diff"), outer, tune_grid = grid
  )
  testthat::expect_identical(final$assessment, "final_model_not_performance_estimate")
  testthat::expect_equal(base::nrow(structure.stage::predict_structure_gbm(
    final, data$records, data$schema)), base::nrow(data$records))
  output <- base::tempdir()
  paths <- structure.stage::save_mscv_results(result, output, "mscv-test")
  base::on.exit(base::unlink(paths))
  testthat::expect_true(base::all(base::file.exists(paths)))
  testthat::expect_error(structure.stage::save_mscv_results(result, output, "mscv-test"),
                         "already exist")
})

testthat::test_that("fixed-height betweenness response retains all three outcomes", {
  data <- synthetic_training()
  grid <- base::expand.grid(n.trees = 10L, interaction.depth = 1L,
                            shrinkage = 0.1, n.minobsinnode = 2L)
  folds <- structure.stage::build_spatial_folds(data, 18, k = 3)
  final <- structure.stage::fit_final_structure_gbm(
    data, base::c("height", "cover", "bt_diff"), folds, tune_grid = grid
  )
  response <- structure.stage::analyze_msf_se_height_betweenness(
    final, data$records, height_variable = "height", betweenness_variable = "bt_diff",
    height_quantiles = base::c(0.25, 0.75),
    betweenness_quantiles = base::c(0.1, 0.5, 0.9)
  )
  testthat::expect_s3_class(response, "structure_conditional_response")
  testthat::expect_equal(base::nrow(response$response), 6L)
  testthat::expect_equal(response$response$mean_probability_msf +
                            response$response$mean_probability_se +
                            response$response$mean_probability_si,
                          base::rep(1, 6L), tolerance = 1e-8)
  testthat::expect_equal(base::length(base::unique(response$response$height_value)), 2L)
  testthat::expect_error(
    structure.stage::analyze_msf_se_height_betweenness(
      final, data$records, height_variable = "height", betweenness_variable = "height"
    ), "distinct"
  )
})

testthat::test_that("nested mode selects variables inside outer training partitions", {
  data <- synthetic_training()
  grid <- base::expand.grid(n.trees = 10L, interaction.depth = 1L,
                            shrinkage = 0.1, n.minobsinnode = 2L)
  outer <- structure.stage::build_spatial_folds(data, 18, k = 3)
  result <- structure.stage::run_mscv_gbm(
    data, outer_folds = outer, inner_samples_per_cluster = 18, inner_k = 3,
    tune_grid = grid
  )
  testthat::expect_identical(result$assessment, "nested_outer_spatial_cv")
  testthat::expect_true(base::all(base::lengths(result$selected$selected_predictors) >= 2L))
  testthat::expect_setequal(result$predictions$sample_id, data$records$sample_id)
})

testthat::test_that("SD ablation records point, average, and msf versus se behavior", {
  data <- synthetic_training()
  grid <- base::expand.grid(n.trees = 10L, interaction.depth = 1L,
                            shrinkage = 0.1, n.minobsinnode = 2L)
  folds <- structure.stage::build_spatial_folds(data, 18, k = 3)
  final <- structure.stage::fit_final_structure_gbm(
    data, base::c("height", "cover", "bt_diff"), folds, tune_grid = grid
  )
  analysis <- structure.stage::analyze_structure_predictions(final, data$records)
  testthat::expect_s3_class(analysis, "structure_prediction_analysis")
  testthat::expect_equal(base::nrow(analysis$points), base::nrow(data$records))
  testthat::expect_equal(base::nrow(analysis$pairwise), 3L * base::nrow(data$records))
  testthat::expect_equal(base::nrow(analysis$msf_se), base::nrow(data$records))
  testthat::expect_setequal(analysis$pairwise_summary$class_a,
                            base::c("msf", "msf", "se"))
  result <- structure.stage::ablate_structure_gbm(
    final, data$records, variables = base::c("height", "cover")
  )
  testthat::expect_s3_class(result, "structure_gbm_ablation")
  testthat::expect_equal(base::nrow(result$point_changes), 4L * base::nrow(data$records))
  testthat::expect_equal(base::nrow(result$average_changes), 16L)
  testthat::expect_setequal(base::names(result$point_changes), base::c(
    "point_id", "variable", "direction", "standard_deviation", "shift",
    "original_value", "perturbed_value", "baseline_class", "perturbed_class",
    "class_changed", "baseline_msf", "baseline_se", "baseline_si",
    "perturbed_msf", "perturbed_se", "perturbed_si", "delta_msf", "delta_se", "delta_si"
  ))
  testthat::expect_true(base::all(base::abs(
    result$point_changes$delta_msf + result$point_changes$delta_se + result$point_changes$delta_si
  ) < 1e-8))
  testthat::expect_setequal(result$msf_se_changes$baseline_group,
                            base::c("all", "msf", "se", "si"))
  model_path <- base::tempfile(fileext = ".rds")
  base::on.exit(base::unlink(model_path), add = TRUE)
  base::saveRDS(final, model_path)
  from_rds <- structure.stage::ablate_structure_gbm(model_path, data$records, variables = "height")
  testthat::expect_equal(base::nrow(from_rds$point_changes), 2L * base::nrow(data$records))
  raw_analysis <- structure.stage::analyze_structure_predictions(final$model, data$records)
  testthat::expect_equal(raw_analysis$points[, data$class_levels], analysis$points[, data$class_levels])
  paths <- structure.stage::save_structure_ablation(result, base::tempdir(), "ablation-test")
  base::on.exit(base::unlink(paths), add = TRUE)
  testthat::expect_true(base::all(base::file.exists(paths)))
  testthat::expect_error(
    structure.stage::save_structure_ablation(result, base::tempdir(), "ablation-test"),
    "already exist"
  )
  bad <- data$records
  bad$height <- 1
  testthat::expect_error(
    structure.stage::ablate_structure_gbm(final, bad, reference_data = bad, variables = "height"),
    "zero-variance"
  )
})
