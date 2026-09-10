synthetic_inputs <- function() {
  withr::with_seed(91, {
    data <- base::expand.grid(replicate = base::seq_len(6),
      stage = base::seq_len(3), site = base::seq_len(12))
    data$sample_id <- base::paste0("s", base::seq_len(base::nrow(data)))
    data$polygon_id <- base::paste(data$site, data$stage, sep = "_")
    data$source_id <- "synthetic"
    data$acquisition_id <- "synthetic-v1"
    data$x <- data$site * 1000 + stats::rnorm(base::nrow(data), sd = 2)
    data$y <- (data$site %% 3) * 1000 + stats::rnorm(base::nrow(data), sd = 2)
    data$structure_stage <- base::c("Multi-Story Foraging", "Stem Exclusion",
                                   "Stand Initiation")[data$stage]
    data$height <- data$stage * 5 + stats::rnorm(base::nrow(data))
    data$cover <- data$stage * 10 + stats::rnorm(base::nrow(data), sd = 5)
    data$midstory_mean_betweenness <- stats::runif(base::nrow(data))
    data$understory_mean_betweenness <- stats::runif(base::nrow(data))
    schema <- base::data.frame(name = base::c("height", "cover", "bt_diff"),
      units = base::c("m", "percent", "dimensionless"), resolution = 30,
      nodata = "NA rejected", interpretation = base::c("height", "cover", "difference"),
      source_version = "synthetic-v1")
    base::list(data = data, schema = schema)
  })
}

synthetic_training <- function() {
  input <- synthetic_inputs()
  structure.stage::prepare_lynx_data(input$data, input$schema, 26912, "synthetic-v1")
}
