# Reconstructed historical MSCV script retained before package migration.
#
# This script is not nested outer spatial CV. See dev/mscv_lynx_model.R and the
# structure.stage package for the active reportable workflow.

library(CAST)
library(caret)
library(glmnet)
library(future)
library(dplyr)
library(purrr)

gt_stuff <- read.csv("~/Downloads/gt_extract_vars.csv")
lynx_og <- read.csv("/home/josh.erickson/Downloads/lynx_df(3).csv")

set.seed(124)
lynx <- lynx_og %>%
  filter(structure_stage %in% c(
    "Multi-Story Foraging", "Stem Exclusion", "Multi-Story Non-foraging",
    "Stand Initiation"
  )) %>%
  mutate(structure_stage = factor(case_when(
    structure_stage == "Multi-Story Foraging" ~ "msf",
    structure_stage == "Multi-Story Non-foraging" ~ "se",
    structure_stage == "Stem Exclusion" ~ "se",
    structure_stage == "Stand Initiation" ~ "si"
  ))) %>%
  filter(!is.na(structure_stage)) %>%
  mutate(midstory_mean_betweenness = ifelse(
    is.na(midstory_mean_betweenness), 0, midstory_mean_betweenness
  )) %>%
  mutate(bt_diff = midstory_mean_betweenness - understory_mean_betweenness) %>%
  slice_sample(n = 2000, by = structure_stage) %>%
  na.omit()

train <- lynx
predictors <- train %>% dplyr::select(
  zmax, n_strata_low_mid, n_gt_6_1, midstory_mean_degree,
  understory_mean_betweenness
)
response <- train[, "structure_stage"]

clusters <- list()
dep <- seq(300, 500, length.out = 50)
for (i in dep) {
  cluster_val <- nrow(train) / i
  cluster <- kmeans(train[, c("x", "y")], cluster_val)$cluster
  clusters[[paste0("cluster", i)]] <- data.frame(cluster)
}
flat <- clusters %>% flatten() %>% as.data.frame()

index_list <- list()
for (i in dep) {
  set.seed(1234)
  folds <- CAST::CreateSpacetimeFolds(
    flat, spacevar = paste0("cluster.", i), k = 10
  )
  index_list[[paste0("cluster.", i)]] <- folds
}

multiFourStats <- function(data, lev = levels(data$obs), model = NULL) {
  cm <- caret::confusionMatrix(data$pred, data$obs)
  by <- cm$byClass
  if (is.null(dim(by))) {
    by <- t(by)
    rownames(by) <- lev[1]
  }
  j <- by[, "Sensitivity"] + by[, "Specificity"] - 1
  c(
    Accuracy = unname(cm$overall["Accuracy"]),
    Kappa = unname(cm$overall["Kappa"]),
    J = mean(j, na.rm = TRUE)
  )
}

plan(multisession(workers = 40))
ctrl_list <- lapply(index_list, function(folds) {
  caret::trainControl(
    method = "repeatedcv", repeats = 5, number = 10, allowParallel = TRUE,
    returnResamp = "all", verbose = FALSE, classProbs = TRUE,
    summaryFunction = multiFourStats, index = folds$index,
    savePredictions = "all"
  )
})

test <- furrr::future_map(ctrl_list, ~suppressMessages(caret::train(
  predictors, response, method = "gbm", metric = "Accuracy", trControl = .
)))

tune_results <- data.frame()
varimp_results <- data.frame()
model_results <- data.frame()
for (i in seq_along(test)) {
  best <- test[[i]]$bestTune
  metrics <- test[[i]]$results %>%
    filter(
      n.trees %in% best$n.trees,
      interaction.depth %in% best$interaction.depth,
      shrinkage %in% best$shrinkage,
      n.minobsinnode %in% best$n.minobsinnode
    ) %>%
    mutate(cluster = names(test)[i], model = "gbm")
  vimp <- summary.gbm(test[[i]]$finalModel, method = relative.influence) %>%
    mutate(cluster = names(test)[i], model = "gbm")

  mod_pred <- predict(test[[i]], gt_stuff, type = "raw")
  cm <- confusionMatrix(mod_pred, factor(gt_stuff$structure_stage,
    levels = c("msf", "se", "si")
  ))
  model_results_final <- bind_cols(
    tibble(structure_stage = rownames(cm$byClass)), as_tibble(cm$byClass)
  ) %>% mutate(cluster = names(test)[i], assessment = "validation")

  train_cm <- confusionMatrix(test[[i]], "none")
  tbl <- train_cm$table
  balanced_by_class <- sapply(colnames(tbl), function(cls) {
    tp <- tbl[cls, cls]
    fn <- sum(tbl[, cls]) - tp
    fp <- sum(tbl[cls, ]) - tp
    tn <- sum(tbl) - tp - fn - fp
    sensitivity <- tp / (tp + fn)
    specificity <- tn / (tn + fp)
    c(
      Sensitivity = sensitivity, Specificity = specificity,
      `Balanced Accuracy` = (sensitivity + specificity) / 2
    )
  })
  train_results <- bind_cols(
    tibble(structure_stage = rownames(t(balanced_by_class))),
    as_tibble(t(balanced_by_class))
  ) %>% mutate(cluster = names(test)[i], assessment = "training")

  tune_results <- plyr::rbind.fill(tune_results, metrics)
  varimp_results <- plyr::rbind.fill(varimp_results, vimp)
  model_results <- plyr::rbind.fill(model_results, model_results_final, train_results)
}

write.csv(tune_results, "~/Downloads/gbm_mscv_five_vars_focal_tune_results.csv")
write.csv(varimp_results, "~/Downloads/gbm_mscv_five_vars_focal_var_imp.csv")
write.csv(model_results, "~/Downloads/gbm_mscv_five_vars_focal_model_results.csv")
saveRDS(test[[1]], "~/Downloads/gbm_mod_five_vars_focal.rds")
plan(sequential)
