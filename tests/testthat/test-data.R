testthat::test_that("preparation maps historical labels and retains only explicit predictors", {
  input <- synthetic_inputs()
  input$data$structure_stage[1:2] <- "Multi-Story Non-foraging"
  input$data$polygon_id[1:2] <- "merged-polygon"
  input$data$unused_metadata <- "never a predictor"
  data <- structure.stage::prepare_lynx_data(input$data, input$schema, 26912, "v1")
  testthat::expect_equal(base::as.character(data$records$structure_stage[1:2]), base::c("se", "se"))
  testthat::expect_identical(base::levels(data$records$structure_stage), base::c("msf", "se", "si"))
  testthat::expect_false("unused_metadata" %in% base::names(data$records))
  testthat::expect_equal(data$records$bt_diff,
    input$data$midstory_mean_betweenness - input$data$understory_mean_betweenness)
  testthat::expect_equal(base::nrow(data$records), base::nrow(input$data))
})

testthat::test_that("ambiguous, missing, or incompatible inputs fail explicitly", {
  input <- synthetic_inputs()
  prepare <- function(x = input$data, schema = input$schema, crs = 26912, ...) {
    structure.stage::prepare_lynx_data(x, schema, crs, "v1", ...)
  }
  bad <- input$data
  bad$sample_id[2] <- bad$sample_id[1]
  testthat::expect_error(prepare(bad), "unique")
  bad <- input$data
  bad$height[1] <- NA_real_
  testthat::expect_error(prepare(bad), "nodata")
  testthat::expect_error(prepare(crs = 4326), "projected CRS")
  bad <- input$data
  bad$structure_stage[1] <- "unknown"
  testthat::expect_error(prepare(bad), "Unmapped")
  kept <- prepare(bad, unknown_labels = "drop")
  testthat::expect_equal(kept$audit$excluded$sample_id, bad$sample_id[1])
  testthat::expect_equal(base::nrow(kept$records), base::nrow(bad) - 1L)
  bad <- input$data
  bad$structure_stage[1] <- "Stem Exclusion"
  testthat::expect_error(prepare(bad), "stand label")
  bad <- input$data
  bad$bt_diff <- 100
  testthat::expect_error(prepare(bad), "disagrees")
  bad_schema <- input$schema
  bad_schema$name[1] <- "sample_id"
  testthat::expect_error(prepare(schema = bad_schema), "metadata")
})

testthat::test_that("sampling preserves identity, input and RNG state", {
  data <- synthetic_training()
  base::set.seed(88)
  previous <- .Random.seed
  sampled <- structure.stage::sample_structure_data(data, 10L)
  testthat::expect_identical(.Random.seed, previous)
  testthat::expect_equal(base::as.integer(base::table(sampled$records$structure_stage)), base::rep(10L, 3))
  testthat::expect_equal(base::nrow(data$records), 216L)
  testthat::expect_identical(sampled, structure.stage::sample_structure_data(data, 10L))
  testthat::expect_equal(base::anyDuplicated(sampled$records$sample_id), 0L)
  testthat::expect_error(structure.stage::sample_structure_data(data, 1000), "fewer")
  capped <- structure.stage::sample_structure_data(data, 1000, small_classes = "all")
  testthat::expect_setequal(capped$records$sample_id, data$records$sample_id)
  testthat::expect_identical(structure.stage::sample_structure_data(data, NULL)$records, data$records)
})
