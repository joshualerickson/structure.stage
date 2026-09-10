library(CAST)
library(caret)
library(gbm)
library(future)
library(dplyr)
library(purrr)
library(sf)
lynx_og <- read.csv('/home/josh.erickson/Downloads/lynx_df.csv')

new_structure_extract <- read_sf("~/Downloads/structure_extract(1).gpkg")

gt_stuff <- read_sf("/home/josh.erickson/Downloads/gt_wildlife_surveys.shp")

glimpse(gt_stuff)
set.seed(124)
lynx <- lynx_og %>%
  filter(structure_stage %in% c('Multi-Story Foraging', 'Stem Exclusion', 'Multi-Story Non-foraging',
                                'Stand Initiation')) %>%
  mutate(structure_stage = factor(case_when(structure_stage == 'Multi-Story Foraging' ~ 'msf',
											structure_stage == 'Multi-Story Non-foraging' ~ 'se',
                                            structure_stage == 'Stem Exclusion' ~ 'se',
                                            structure_stage == 'Stand Initiation' ~'si'))) %>% 
  filter(!is.na(structure_stage)) %>% 
  mutate(bt_diff = midstory_mean_betweenness-understory_mean_betweenness) %>% 
  slice_sample(n = 2000, by = structure_stage) %>%
  select(-aws, -tmin, -sand, -clay, -psst, -pvt4, -pvt11, -def30, -slp) %>%
  select(!any_of(ends_with('mean_focal')),structure_stage, x, y) %>%
  select(!any_of(contains(c('strength', 'path_length', 'eigen_ratio', 'graph_density')))) 

glimpse(lynx)
length(lynx)

train <- lynx
remove <- c('x', 'y', 'structure_stage')
predictors <- train[, -match(remove,names(train))]
response <- train[, 'structure_stage']


clusters <- list()
dep <- seq(300, 500, length.out = 50)
for (i in dep){
  val <- i
  cluster_val <- nrow(train)/val
  
  Mycluster <- kmeans(train[,c('x','y')], cluster_val)
  
  name <- paste("cluster",val)
  
  Mycluster <- Mycluster$cluster
  
  Mycluster <- data.frame(Mycluster)
  
  colnames(Mycluster) <- name
  
  Myclusterl <- list(Mycluster)
  
  clusters <- append(clusters, Myclusterl)
}

#now take the list and create a data.frame

flat <- clusters %>% flatten() %>% as.data.frame()
index_list <- list()

for (i in dep) {
  
  set.seed(1234)
  indices <- CAST::CreateSpacetimeFolds(flat, spacevar = paste("cluster.", i, sep = ""), k = 10)
  name <- paste("cluster.", i, sep = "")
  indices <- list(indices)
  names(indices) <- name
  index_list <- append(index_list, indices)
  
}

set.seed(123)

# --- Custom multiclass summary: J and Dist, + Accuracy/Kappa (+ optional LogLoss) ---
multiFourStats <- function(data, lev = levels(data$obs), model = NULL) {
  # confusion matrix (no 'positive' arg for multiclass)
  cm <- caret::confusionMatrix(data$pred, data$obs)
  by <- cm$byClass
  
  # Coerce binary case to matrix shape if someone reuses this for 2 classes
  if (is.null(dim(by))) {
    by <- t(by)
    rownames(by) <- lev[1]
  }
  
  sens <- by[, "Sensitivity"]
  spec <- by[, "Specificity"]
  
  J    <- sens + spec - 1
  
  # aggregate metrics
  out <- c(
    Accuracy     = unname(cm$overall["Accuracy"]),
    Kappa        = unname(cm$overall["Kappa"]),
    J      = mean(J, na.rm = TRUE)
  )
  
  out
}
plan(multicore(workers = 40))

ctrl_list <- list()

for (i in seq_along(index_list)){
  ctrl <- trainControl(method="repeatedcv",
					    number = 10,
                       allowParallel = TRUE,
                       returnResamp = "all",
                       verbose = FALSE,
                       classProbs = TRUE,
                       summaryFunction = multiFourStats,
                       index = index_list[[i]][[1]],
                       savePredictions = 'all')
  
  ctrl <- list(ctrl)
  
  clusters <- dep
  
  names(ctrl) <- clusters[i]
  
  ctrl_list <- append(ctrl_list, ctrl)
}


.furrr_opts <- furrr::furrr_options(
  seed     = TRUE,                     # fixes the RNG warning
  packages = c("caret", "gbm"),# gbm if you're using method='gbm'
  globals  = list(multiFourStats = multiFourStats)
)

test <- furrr::future_map(ctrl_list, ~suppressMessages(ffs(predictors,
                                          response,
                                          method = 'gbm',
                                          metric = 'Accuracy',
                                          verbose = FALSE,
                                          trControl = .)))
ffs_results <- data.frame() 

for (i in seq_along(test)){

  vars <- test[[i]]$selectedvars
  
  metrics <- test[[i]]$results
  
  metrics <- metrics %>% mutate(vars = list(vars), cluster = paste(names(test[i])),
                                model = "gbm")
  
  
  ffs_results <- plyr::rbind.fill(ffs_results, metrics)
  
  
}

library(stringr)

#this removes `vars` as list column and makes character
ffs_results <- ffs_results %>%
  mutate(char_vars = str_remove_all(vars, c("^c" = "", "\\(|\\)" = "", "[\"]" = ""))) %>% 
  select(-vars)

write.csv(ffs_results, '/home/josh.erickson/Downloads/gbm_ffs_300_500_no_focal.csv')

plan(sequential)

d <- 'done'
?gbm::gbm
