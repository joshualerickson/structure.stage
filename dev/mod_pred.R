library(terra)
library(gbm)
library(sf)

my_mod <- readRDS('/home/josh.erickson/Downloads/gbm_mod_five_vars_no_focal.rds')
my_mod <- readRDS('/home/josh.erickson/Downloads/gbm_mod_five_vars_focal.rds')
my_mod2 <- readRDS('/home/josh.erickson/Downloads/gbm_mod_focal_my_mod.rds')
my_mod <- readRDS('/home/josh.erickson/Downloads/gbm_mod_feasibility.rds')

my_mod_si_no_si <- readRDS('/home/josh.erickson/Downloads/gbm_mod_si_no_si.rds')
my_mod_msf_se <- readRDS('/home/josh.erickson/Downloads/gbm_mod_msf_se.rds')
library(caret)
?thresholder
names(my_mod$trainingData)
confusionMatrix(my_mod_si_no_si, "none")
confusionMatrix(my_mod_msf_se, "none")
confusionMatrix(my_mod)
cm <- confusionMatrix(my_mod, "none")
cm

resample_stats <- thresholder(my_mod, threshold = seq(.2, 1, by = 0.05))


pred <- collect_predictions(my_mod)

pred <- pred |>
  mutate(
    .pred_class_035 = ifelse(.pred_f >= 0.35, "f", "nf")
  )

library(yardstick)

metrics_035 <- pred |>
  metrics(
    truth = truth,
    estimate = .pred_class_035
  )

metrics_035

ggplot(resample_stats, aes(x = prob_threshold, y = J)) +
  geom_point()

ggplot(resample_stats, aes(x = prob_threshold, y = Accuracy)) +
  geom_point()

ggplot(resample_stats, aes(x = prob_threshold, y = Sensitivity)) +
  geom_point() +
  geom_point(aes(y = Specificity), col = "red")
cm_test$table
tbl <- cm$table

classes <- colnames(tbl)

balanced_by_class <- sapply(classes, function(cls) {
  TP <- tbl[cls, cls]
  FN <- sum(tbl[, cls]) - TP
  FP <- sum(tbl[cls, ]) - TP
  TN <- sum(tbl) - TP - FN - FP

  sens <- TP / (TP + FN)
  spec <- TN / (TN + FP)
  J    <- sens + spec - 1

  c(
    sensitivity = sens,
    specificity = spec,
    balanced_accuracy = (sens + spec) / 2,
    youdens_j = J
  )
})

bind_cols(tibble(structure_stage = rownames(t(balanced_by_class))), as_tibble(t(balanced_by_class)))

balanced_by_class

gt <- read_sf("/home/josh.erickson/Downloads/gt_wildlife_surveys.shp")



mod_vars <- rast("/mnt/alpheus1/LIDAR/flathead_testing/lynx_model/model_vars_focal.tif")
mod_vars <- rast("/mnt/alpheus1/LIDAR/flathead_testing/lynx_model/model_vars_no_focal.tif")
names(mod_vars) <- paste0(names(mod_vars), '_mean_focal')
envt_vars <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_envt_covariates_30m_8826.tif")
aoi <- mapedit::drawFeatures()
envt_crop <- crop(envt_vars, mod_vars[[1]])
writeRaster(mod_vars_crop, '~/Downloads/ksanka_canopy_tree_graph_metrics.tif')
mod_vars_pred <- c(mod_vars[[c('zkurt_mean_focal', 'bt_diff_mean_focal', 'zmean_mean_focal', 'topo_residual_sd_mean_focal','zquantile_pc1_mean_focal')]],
              envt_vars[[c('tmin',
                           'sand',
                           'slp', 'def30')]])
mod_vars_pred <- c(mod_vars[[c('zkurt_mean_focal', 'bt_diff_mean_focal',
                               'zmean_mean_focal', 'topo_residual_sd_mean_focal',
                               'zquantile_pc1_mean_focal',
                               'graph_metrics_pca1_mean_focal',
                               'n_gt_12_1_mean_focal',
                               'topo_entropy_mean_focal',
                               'smoothness_score_mean_focal',
                               'rumple_index_mean_focal')]],
              envt_crop[[c(
                           'slp')]])

?crop
mod_vars_pred <- rast("~/Downloads/NR_no_hab_predictors.tif")
mod_vars_pred <- c(mod_vars, envt_crop)
mod_vars_pred[[18:19]]
mod_vars_pred <- crop(mod_vars_pred, mod_vars[[1]])
mod_vars_pred <- ifel(mod_vars_pred < 0, NA, mod_vars_pred)
mod_vars_pred_crop <- terra::crop(mod_vars, vect(st_transform(gt, st_crs(mod_vars_pred))))
glimpse(my_mod$trainingData)
train_vars <- setdiff(names(my_mod$trainingData), ".outcome")
mod_vars_pred <- mod_vars[[train_vars]]
mod_vars_pred_crop<- mod_vars_pred_crop[[train_vars]]
mod_vars_pred_crop[[1]]

library(tidyverse)
glimpse(my_mod)
plot(mod_vars_pred_crop[[1:12]])
plot(mod_vars_pred_crop[[13:19]])
library(caret)

t <- terra::predict(mod_vars_pred, my_mod, type = 'prob')

mod_vars_pred_crop_test <- mod_vars_pred_crop[[train_vars]]

t <- terra::predict(mod_vars_pred_crop, my_mod, type = 'prob')

plot(t)

t[['bunny_habitat']] <- ifel(
  t[['f']] < 0.33,
  0,   # Not Bunny Enough
  1    # Bunny Enough
)

t[['bunny_habitat']] <- as.factor(t[['bunny_habitat']])

levels(t[['bunny_habitat']]) <- data.frame(
  ID    = c(0, 1),
  label = c("Not Bunny Enough", "Bunny Enough")
)


plot(t)
writeRaster(t, '/home/josh.erickson/Downloads/ksanka_bunny.tif')

si_no_si <- rast('/home/josh.erickson/Downloads/new_test_si_no_si.tif')
msf_se <- rast('/home/josh.erickson/Downloads/new_test_msf_se.tif')

msf_se_masked <- mask(msf_se, si_no_si, maskvalues = 1)
plot(msf_se_masked)

si_only <- ifel(si_no_si == 'si', si_no_si, NA)
plot(si_only)
lev <- data.frame(value = c(1,2),
                  class = c("msf","se"))

lev2 <- data.frame(
                  value = c(1),
                  class = c("si")
                  )

levels(si_only)      <- lev2
levels(msf_se_masked) <- lev

final_map <- cover(si_only, msf_se_masked)
levels(final_map) <- lev
plot(final_map)

plot(t)

pred <- predict(
  mod,
  newdata = as.data.frame(mod_vars_pred_crop),
  n.trees = mod$n.trees,
  type = "prob"
)

head(pred)

table(pred)

unique(pred)
names(mod$trainingData)
names(mod_vars_pred)

train_vars

plot()
plot(mod_vars[['understory_mean_betweenness']])
plot(mod_vars_pred_crop[[1:9]])
bt <- mod_vars[['understory_mean_betweenness']]

bt <- ifel(bt == 0, NA, bt)

plot(bt)

tmask <- mask(t, bt)
tmask = t

K <- nlyr(tmask)
logK <- log(K)

entropy_norm <- app(tmask, function(v) {
  if (all(is.na(v))) return(NA_real_)
  s <- sum(v, na.rm = TRUE)
  if (s <= 0) return(NA_real_)
  p <- v / s                       # robust: renormalize
  p <- p[p > 0]                    # avoid log(0)
  H  <- -sum(p * log(p))
  H / logK                         # normalized [0,1]
})

conf_entropy <- 1 - entropy_norm   # 1=confident, 0=uncertain
names(entropy_norm) <- "entropy_norm"
names(conf_entropy) <- "conf_entropy"
conf_entropy
plot(conf_entropy)
# tmask: SpatRaster with 3 probability bands, e.g. names(tmask) = c("msf","se","si")

# 1) per-pixel index (1..nlyr) of the max band
win_idx <- which.max(tmask)  # or: app(tmask, which.max)

# 2) turn indexes into a categorical raster and attach the band names
win_fac <- as.factor(win_idx)
levels(win_fac)[[1]] <- data.frame(
  ID   = 1:nlyr(tmask),
  band = names(tmask)
)

names(win_fac) <- "structure_stage"  # output layer name

conf_margin <- app(tmask, function(v) {
  if (all(is.na(v))) return(NA_real_)
  s <- sum(v, na.rm = TRUE); if (s <= 0) return(NA_real_)
  p <- sort(v / s, decreasing = TRUE)
  p[1] - ifelse(length(p) >= 2, p[2], 0)
})

names(conf_margin) <- "conf_margin"
conf_margin
"/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_envt_covariates_30m_8826.tif"
writeRaster(c(tmask, win_fac, conf_entropy, conf_margin), '/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_structure_stage_30m_8826.tif')
writeRaster(c(tmask, win_fac, conf_entropy, conf_margin), '/home/josh.erickson/Downloads/NR_lidar_structure_stage_30m_8826.tif')
writeRaster(c(t), '/home/josh.erickson/Downloads/gbm_lynx_probs_focal_my_mod_crop.tif', overwrite = T)

nr_ss <- rast('/home/josh.erickson/Downloads/NR_lidar_structure_stage_30m_8826.tif')

bt <- mod_vars[['understory_mean_betweenness']]

bt <- ifel(bt == 0, NA, bt)


tmask <- mask(nr_ss, bt)

writeRaster(c(tmask), '/home/josh.erickson/Downloads/NR_lidar_structure_stage_30m_8826.tif', overwrite = T)

plot(win_fac)

all_model_vars <- glmnet_ffs %>%
  separate(char_vars, into = letters[1:15], sep = ',') %>%
  pivot_longer(cols = 12:24)


#figure in paper
all_model_vars %>% count(value,model, sort = T) %>% na.omit() %>% mutate(value = fct_reorder(value, n)) %>%
  ggplot(aes(value, n)) +
  geom_col(alpha = 0.5) +
  geom_point()  +
  labs(x = "Features", y = "Count") +
  coord_flip() +
  geom_text(aes(label = n), nudge_y = 3) +
  theme(axis.title = element_text(size = 14), axis.text =element_text(size = 12))

st_layers("~/Downloads/structure_extract.gdb")
new_structure_extract <- read_sf("~/Downloads/structure_extract.gdb")
structure_extract <- read_sf("~/Downloads/structure_etract.gpkg")
gt_stuff <- read_sf("/home/josh.erickson/Downloads/gt_wildlife_surveys.shp")

final_model <- rast('/home/josh.erickson/Downloads/gbm_lynx_probs_focal_my_mod_crop.tif')
final_model <- rast('/home/josh.erickson/Downloads/NR_lidar_structure_stage_30m_8826.tif')
writeRaster(final_model,  '/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_structure_stage_30m_8826.tif', overwrite = TRUE)

 my_model <- rast("~/Downloads/gbm_lynx_probs_wo_sand_tmin_mask.tif")

ffs_best <- read.csv("~/Downloads/gbm_var_imp_focal_my_mod.csv")

ffs_best <- read.csv("~/Downloads/gbm_ffs_200_500_focal_mean.csv")

ffs_best

gt_stuff <- gt_stuff  %>%
  filter(Lynx_Field %in% c('Multistory', 'Stem Exclusion',
                                'Stand Initiation')) %>%
  mutate(structure_stage_field = factor(case_when(Lynx_Field == 'Multistory' ~ 'msf',
                                       Lynx_Field == 'Stem Exclusion' ~ 'se',
                                       Lynx_Field == 'Stand Initiation' ~'si',
                                       TRUE ~ NA)))%>%
mutate(Lynx_Model = factor(case_when(Lynx_Model == 'Multistory' ~ 'msf',
                                     Lynx_Model == 'Stem Exclusion' ~ 'se',
                                     Lynx_Model == 'Stand Initiation' ~'si',
                                     is.na(Lynx_Model) ~ 'msf',
                                     TRUE ~ NA))) %>%
  filter(!is.na(structure_stage_field))

unique(gt_stuff$Lynx_Field)
unique(gt_stuff$Lynx_Field)

gt_extract <- terra::extract(win_fac, vect(st_transform(gt_stuff, st_crs(t)))) %>% rename(structure_stage = 'class')

gt_extract_vars <- terra::extract(mod_vars_pred, vect(st_transform(gt_stuff, st_crs(mod_vars_pred))))


gt_extract_vars <- gt_extract_vars[,-1] %>%
  bind_cols(gt_stuff %>%
              select(structure_stage = structure_stage_field)%>% st_drop_geometry())

write_csv(gt_extract_vars, '~/Downloads/gt_extract_vars.csv')

gt_extract2 <- terra::extract(final_model, vect(st_transform(gt_stuff, st_crs(final_model))))

gt_stuff_results2 <- gt_stuff %>% bind_cols(gt_extract2) %>% st_drop_geometry()
gt_stuff_results <- gt_stuff %>% bind_cols(gt_extract)
  bind_cols(gt_extract_vars[,-1]) %>%
  st_drop_geometry()

glimpse(gt)
gt_sf <- gt %>% left_join(gt_stuff_results %>% select(GlobalID, msf:review))

gt_sf %>% st_write('~/Downloads/gt_sf_eda.gpkg')

library(stringr)

gt_stuff_results2 <- gt_stuff_results2 %>% mutate(review = str_detect(Lynx_Comme, 'Review'))
table(gt_stuff_results$review)

gt_stuff_results_not_review <- gt_stuff_results2 %>% filter(review == TRUE)




confusionMatrix(gt_stuff_results$structure_stage,gt_stuff_results$structure_stage_field)
cm_test <- confusionMatrix(gt_stuff_results2$structure_stage,gt_stuff_results2$structure_stage_field)

confusionMatrix(gt_stuff_results$Lynx_Model,gt_stuff_results$structure_stage_field)
confusionMatrix(gt_stuff_results_not_review$structure_stage,gt_stuff_results_not_review$structure_stage_field)

?confusionMatrix
cm$table

glimpse(gt_stuff_results)
library(tidyverse)
ggplot(gt_stuff_results ,
       aes(bt_diff_mean_focal, n_gt_12_1_mean_focal)) +
  geom_jitter(aes(color = rumple_index_mean_focal)) +
  facet_wrap(~Lynx_Field) +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1'), limits = c(3,5))
?scale_color_gradientn

library(tidyverse)

structure_extract_df <- structure_extract %>% st_drop_geometry()
glimpse(structure_extract_df)
lynx_og <- read.csv('/home/josh.erickson/Downloads/lynx_df.csv')
lynx_og %>%
  filter(structure_stage %in% c('Multi-Story Foraging', 'Stem Exclusion', 'Multi-Story Non-foraging',
                                'Stand Initiation')) %>%
  mutate(structure_stage = factor(case_when(structure_stage == 'Multi-Story Foraging' ~ 'msf',
                                            structure_stage == 'Multi-Story Non-foraging' ~ 'se',
                                            structure_stage == 'Stem Exclusion' ~ 'se',
                                            structure_stage == 'Stand Initiation' ~'si'))) %>%
  filter(!is.na(structure_stage)) %>%
ggplot(
       aes(bt_diff, topo_entropy)) +
  geom_point(aes(color = slp), size = 2.5, alpha = 0.75) +
  theme_bw() +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1')) +
  facet_wrap(~structure_stage)


gt_stuff_results %>%
  filter(structure_stage_field == 'se', structure_stage == 'msf') %>%
  # filter(!is.na(review),
  #        bt_diff < 0 ) %>%
  # arrange(bt_diff) %>%
  # view()
  ggplot() +
  geom_point(aes(understory_mean_betweenness, topo_entropy, color = msf), size = 2.5, alpha = 0.75) +
  #geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1'))
structure_extract %>% filter(sand > 50) %>%
  filter(structure_stage %in% c('Multi-Story Foraging', 'Stem Exclusion', 'Multi-Story Non-foraging',
                                'Stand Initiation')) %>%
  mutate(structure_stage = factor(case_when(structure_stage == 'Multi-Story Foraging' ~ 'msf',
                                            structure_stage == 'Multi-Story Non-foraging' ~ 'se',
                                            structure_stage == 'Stem Exclusion' ~ 'se',
                                            structure_stage == 'Stand Initiation' ~'si')))%>% mapview::mapview()

library(GGally)

library(GGally)

df <- structure_extract %>%
  st_drop_geometry() %>%
  filter(structure_stage %in% c(
    "Multi-Story Foraging",
    "Stem Exclusion"
    #,"Stand Initiation"
  )) %>%
  select(
    structure_stage,
    bt_diff_mean_focal,
    zquantile_pc1_mean_focal,
    graph_metrics_pca1_mean_focal,
    n_gt_12_1_mean_focal,
    zmean_mean_focal,
    zkurt_mean_focal,
    topo_entropy_mean_focal,
    smoothness_score_mean_focal,
    topo_residual_sd_mean_focal,
    rumple_index_mean_focal,
    slp
  ) %>% filter(zkurt_mean_focal < 10)
# %>%
#   select(!ends_with('mean_focal'))

metric_cols <- setdiff(names(df), "structure_stage")

ggpairs(
  df,
  columns = metric_cols,
  aes(color = structure_stage),
  upper = list(continuous = "density"),
  lower = list(continuous = "points"),
  diag  = list(
    continuous = wrap("densityDiag")
  ),
  alpha = 0.3
) + theme_bw()

df %>% ggplot(aes(LAD_z_max, n_gt_12_1)) + geom_point(aes(color = structure_stage))

glimpse(structure_extract)


st_layers("~/Downloads/structure_extract.gdb")


results <- read.csv('/home/josh.erickson/Downloads/gbm_var_imp_si_no_si.csv')
results <- read.csv('/home/josh.erickson/Downloads/gbm_mscv_si_no_si.csv')

gt_units <- read_sf('/home/josh.erickson/Downloads/gt2.gpkg')

koot_model <- read_sf('/home/josh.erickson/Downloads/drive-download-20251215T220931Z-1-001/knf_lynx_hab_2024.shp')

koot_model <- koot_model %>% st_intersection(st_as_sf(st_as_sfc(st_bbox(gt))))

plot(koot_model$geometry)
install.packages('exactextractr')
library(exactextractr)

gt_units_extract <- exact_extract(mod_vars_crop, gt_units,
                                    fun = c("mean", "stdev", "min", "max", 'sum'),
                                    weights = "area")

library(exactextractr)
library(dplyr)

# vote + certainty function operating on the pixels *within one polygon*
vote_metrics <- function(values, coverage_fraction) {
  # values: data.frame with columns p_msf, p_se, p_si for intersecting pixels
  # coverage_fraction: numeric vector same length as nrow(values)

  # Keep valid rows
  ok <- is.finite(coverage_fraction) & coverage_fraction > 0
  ok <- ok & stats::complete.cases(values[, c("msf","se","si")])
  if (!any(ok)) {
    return(data.frame(
      n_pix = 0,
      msf_share = NA_real_, se_share = NA_real_, si_share = NA_real_,
      winner = NA_character_,
      winner_share = NA_real_, runnerup_share = NA_real_,
      margin = NA_real_,
      vote_entropy = NA_real_,
      vote_entropy_norm = NA_real_
    ))
  }

  v <- values[ok, c("msf","se","si")]
  w <- coverage_fraction[ok]

  # Pixel "vote" = argmax probability (ties broken deterministically)
  cls_idx <- max.col(as.matrix(v), ties.method = "first")  # 1=msf,2=se,3=si
  cls <- c("msf","se","si")[cls_idx]

  # Weighted vote totals
  tot_w <- sum(w)
  msf_w <- sum(w[cls == "msf"])
  se_w  <- sum(w[cls == "se"])
  si_w  <- sum(w[cls == "si"])

  shares <- c(msf = msf_w, se = se_w, si = si_w) / tot_w

  # Winner and margin (how close the decision is)
  ord <- order(shares, decreasing = TRUE)
  winner <- names(shares)[ord[1]]
  winner_share <- unname(shares[ord[1]])
  runnerup_share <- unname(shares[ord[2]])
  margin <- winner_share - runnerup_share

  # Entropy of vote shares (how "random" vs "sure")
  eps <- 1e-12
  p <- pmax(shares, eps)
  p <- p / sum(p)
  H <- -sum(p * log2(p))                 # range: [0, log2(3)]
  Hn <- H / log2(3)                      # normalized: [0,1]

  data.frame(
    n_pix = length(w),
    msf_share = unname(shares["msf"]),
    se_share  = unname(shares["se"]),
    si_share  = unname(shares["si"]),
    winner = winner,
    winner_share = winner_share,
    runnerup_share = runnerup_share,
    margin = margin,
    vote_entropy = H,
    vote_entropy_norm = Hn
  )
}

# Run extraction: one row per polygon
gt_units_extrtact2 <- exact_extract(final_model, gt_units,
  fun = vote_metrics,
  progress = TRUE
) %>% as_tibble()

gt_units_final <- gt_units %>% bind_cols( gt_units_extract, gt_units_extrtact2)

library(tidyverse)
gt_units_final %>% filter(mean.zkurt < 20) %>%
  ggplot(aes(mean.bt_diff, mean.graph_metrics_pca1)) +
  geom_point(aes(color = winner)) +
  geom_vline(xintercept = c(2,5,10), linetype = 2) +
  facet_wrap(~SilvRX) +
  theme_bw()

glimpse(gt_units_final)
glimpse(gt_units)

write_sf(koot_model_final, '~/Downloads/koot_model_example.gpkg')



library(tibble)

df <- tribble(
  ~assessment,  ~structure_stage,   ~Sensitivity,     ~Specificity,     ~`Balanced Accuracy`,
  "training",   "Macro Average",    "0.474 (0.025)",  "0.733 (0.119)",  "0.603 (0.059)",
  "training",   "Multistory",       "0.456 (0.024)",  "0.643 (0.038)",  "0.549 (0.023)",
  "training",   "Stand Initiation", "0.463 (0.095)",  "0.868 (0.015)",  "0.666 (0.050)",
  "training",   "Stem Exclusion",   "0.502 (0.021)",  "0.688 (0.019)",  "0.595 (0.011)",
  "validation", "Macro Average",    "0.551 (0.105)",  "0.743 (0.080)",  "0.647 (0.067)",
  "validation", "Multistory",       "0.604 (0.023)",  "0.661 (0.014)",  "0.633 (0.014)",
  "validation", "Stand Initiation", "0.618 (0.014)",  "0.821 (0.004)",  "0.719 (0.007)",
  "validation", "Stem Exclusion",   "0.430 (0.016)",  "0.746 (0.014)",  "0.588 (0.009)"
)

library(tidyverse)
write_csv(df, '/mnt/mordor3/data/lidar_download/canopy_metrics/structure_stage_results.csv')
