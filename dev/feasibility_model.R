library(CAST)
library(caret)
library(gbm)
library(future)
library(dplyr)
library(purrr)
lynx_og <- read.csv('/home/josh.erickson/Downloads/lynx_df(3).csv')
set.seed(124)
glimpse(lynx_og)
lynx <- lynx_og %>%
  filter(structure_stage %in% c('Multi-Story Foraging', 'Stem Exclusion', 'Multi-Story Non-foraging',
                                'Stand Initiation')) %>%
  mutate(structure_stage = factor(case_when(structure_stage == 'Multi-Story Foraging' ~ 'msf',
                                            structure_stage == 'Stem Exclusion' ~ 'se',
                                            structure_stage == 'Stand Initiation' ~'si'))) %>% 
  filter(!is.na(structure_stage)) %>% 
  mutate(bt_diff = midstory_mean_betweenness-understory_mean_betweenness) %>% 
  slice_sample(n = 2000, by = structure_stage) %>% 
  select(zmean:zkurt,zq50:zq95, LAI, fractional_canopy_cover:n_trees, n_strata_low:topo_residual_sd, understory_mean_degree,
         understory_mean_betweenness:understory_n_components, bt_diff,midstory_mean_degree,midstory_mean_betweenness:midstory_n_components,
         aws:slp,
         structure_stage, x, y)

glimpse(lynx)
length(lynx)

train <- lynx
predictors <- train[, -c(46:48)]
response <- train[, 'structure_stage']
library(tidyverse)

library(terra)

lf20 <- read.csv('/mnt/mordor3/data/landfire/LF2020_EVT_220_CONUS/CSV_Data/LF20_EVT_220.csv')
glimpse(lf20)

lf20vals <- lf20 %>% filter(str_detect(casefold(EVT_CLASS, upper = FALSE),  "^(?!.*\\btree\\b).*")) %>% pull(VALUE)
lfrast <- rast("/mnt/mordor3/data/landfire/LF2020_EVT_220_CONUS/Tif/LC20_EVT_220_west_crop.tif")

final_model <- rast('/home/josh.erickson/Downloads/NR_lidar_structure_stage_30m_8826.tif')
library(sf)

bb <- vect(st_transform(st_as_sf(st_as_sfc(st_bbox(final_model))), st_crs(lfrast)))

lfrast <- crop(lfrast, bb)

lfrast <- project(lfrast, final_model)

lfrast_final <- ifel(lfrast %in% lf20vals, 1, 2)

writeRaster(lfrast_final, '/home/josh.erickson/Downloads/LC20_EVT_220_binary_trees_no_trees.tif')


veg_mod <- rast("/mnt/mordor3/data/RAP_biomass/vegetation-biomass-v3-2020.tif")
rap_mod <- rast("/mnt/mordor3/data/RAP_cover/RAP_cover_west_max_30m.tif")
rap_mod2 <- rast("/mnt/alpheus1/RAP_cover/vegetation-cover-v3-2024.tif")


bb <- vect(st_transform(st_as_sf(st_as_sfc(st_bbox(final_model))), st_crs(rap_mod2)))

rap_mod2 <- crop(rap_mod2, bb)

rap_mod2 <- project(rap_mod2, final_model)



envt_vars <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_envt_covariates_30m_8826.tif")


envt_vars <- crop(envt_vars, veg_mod[[1]])
names(veg_mod) <- c('annual_forb_grass_biomass', 'perennial_forb_grass_biomass')
names(rap_mod2) <- c(
  "annual_forb_grass",
  "litter",
  "bare_ground",
  "perennial_forb_grass",
  "shrub",
  "tree"
)
writeRaster(c(lfrast_final, veg_mod, rap_mod,rap_mod2, envt_vars), '/home/josh.erickson/Downloads/NR_no_hab_predictors.tif', overwrite = TRUE)


no_hab_preds <- rast('/home/josh.erickson/Downloads/NR_no_hab_predictors.tif')

structure_extract <- read_sf('/home/josh.erickson/Documents/projects/lidar/simple_features/structure_extract.gpkg')
gt_surveys <- read_sf('/home/josh.erickson/Downloads/gt_wildlife_surveys.shp')

structure_extract <- structure_extract %>% mutate(feasibility = case_when(
  structure_stage %in% c('No Habitat', 'Not Habitat') ~ 'Not Feasible',
  TRUE ~ 'Feasible'
))
unique(gt_surveys$Lynx_Field)
gt_surveys <- gt_surveys %>% filter(!is.na(Lynx_Field)) %>% mutate(feasibility = case_when(
  Lynx_Field %in% c('Non Habitat') ~ 'Not Feasible',
  TRUE ~ 'Feasible'
))


no_hab_points <- bind_rows(structure_extract %>% select(feasibility) %>% rename(geometry = 'geom'), 
                           gt_surveys %>% select(feasibility))


no_hab_points_extract <- terra::extract(no_hab_preds, vect(no_hab_points))

no_hab_points_extract <- no_hab_points %>% bind_cols(no_hab_points_extract)

library(GGally)

df <- no_hab_points_extract %>%
  st_drop_geometry() 
# %>%
#   select(!ends_with('mean_focal'))

metric_cols <- setdiff(names(df), "structure_stage")

ggpairs(
  df,
  columns = c(1, 4:21),
  aes(color = feasibility),
  upper = list(continuous = "density"),
  lower = list(continuous = "points"),
  diag  = list(
    continuous = wrap("densityDiag")
  ),
  alpha = 0.3
) + theme_bw()

df %>% ggplot(aes(LAD_z_max, n_gt_12_1)) + geom_point(aes(color = structure_stage))

write.csv(no_hab_points_extract %>%  st_as_sf() %>% st_zm() %>% 
            mutate(
  x = st_coordinates(geometry)[,1],
  y = st_coordinates(geometry)[, 2]
) %>% st_drop_geometry(), '/home/josh.erickson/Downloads/feasibility.csv')

feasibility_ffs <- read_csv('/home/josh.erickson/Downloads/feasibility_ffs.csv')


ffs_best <- feasibility_ffs %>% group_by(cluster) %>% slice_max(order_by = Accuracy)%>% ungroup()

all_models <- feasibility_ffs %>% mutate(cluster_no = str_replace_all(cluster, '[.]', '_'),
                                  cluster_no = parse_number(cluster_no))

all_models_gbm <- all_models %>% filter(model == 'gbm')
all_model_vars <- all_models_gbm %>% separate(char_vars, into = letters[1:4], sep = ',') %>% 
  pivot_longer(cols = 14:17) %>% mutate(value = str_remove_all(value, ' ') %>% factor())


#figure in paper
all_model_vars %>% count(value,model, sort = T) %>% na.omit() %>% mutate(value = fct_reorder(value, n)) %>% 
  ggplot(aes(value, n)) + 
  geom_col(alpha = 0.5) + 
  geom_point()  +
  labs(x = "Features", y = "Count") +
  coord_flip() + 
  geom_text(aes(label = n), nudge_y = 3) + 
  theme_bw() + 
  theme(axis.title = element_text(size = 14), axis.text =element_text(size = 12))

library(terra)
my_mod_f <- readRDS('/home/josh.erickson/Downloads/gbm_mod_f.rds')
train_vars <- setdiff(names(my_mod_f$trainingData), ".outcome")
no_hab_preds <- rast('~/Downloads/NR_no_hab_predictors.tif')
mod_vars_pred <- no_hab_preds[[train_vars]]

mod_vars_pred[['EVT_NAME']] = as.factor(mod_vars_pred[['EVT_NAME']])

t <- terra::predict(mod_vars_pred, my_mod_f, type = 'prob', filename = '/home/josh.erickson/Downloads/gbm_feasibility.tif', overwrite = TRUE)

writeRaster(t, '/home/josh.erickson/Downloads/new_test_msf_se.tif')

library(tidyverse)
bt <- my_mod_f$bestTune

pred <- my_mod_f$pred %>% filter(n.trees == 500 & interaction.depth == 2 & shrinkage == 0.001 & n.minobsinnode == 10)

td <- my_mod_f$trainingData %>% mutate(rowIndex = row_number())

pred <- pred %>% left_join(td, by = 'rowIndex')

tibble(pred)

glimpse(pred)

var_imp <- read.csv('/home/josh.erickson/Downloads/gbm_var_imp_msf_se.csv')
