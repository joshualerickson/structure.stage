#### eda into misleading % of median

library(httr2)
library(jsonlite)
library(data.table)
library(tidyverse)
library(furrr)
library(future)
plan(multisession(workers = 100))
# Get station metadata
stations_url <- "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/stations"

stations <- request(stations_url) |>
  req_perform() |>
  resp_body_json()

stations_dt <- bind_rows(stations)

# Keep key fields
stations_dt <- stations_dt %>% rename(
  station_id = 'stationTriplet'
)

stations_dt <- stations_dt %>%
  filter(str_detect(station_id, "SNOW|SNTL"))

stations_dt <- stations_dt %>%
  mutate(
    beginDate = as.Date(beginDate),
    endDate   = as.Date(endDate)
  )
# now get data
get_swe_station <- function(station_id, begin_date, end_date) {

  tryCatch({
    res <- request("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/data") |>
      req_url_query(
        stationTriplets = station_id,
        elements = "WTEQ",
        duration = "daily",
        beginDate = as.character(begin_date),
        endDate = as.character(end_date)
      ) |>
      req_perform()

    js <- resp_body_json(res, simplifyVector = FALSE)


    parse_swe_response(js, station_id)


  }, error = function(e) {
    message("Failed: ", station_id, " | ", conditionMessage(e))
    NULL
  })
}

stations_sntl_dt <- stations_dt %>%
  mutate(
    beginDate = as.Date(beginDate),
    endDate   = as.Date(endDate),
    endDate   = if_else(endDate > Sys.Date(), as.Date(Sys.Date()), endDate)
  ) %>% filter(str_detect(station_id, 'SNTL')) %>% group_by(station_id) %>% slice(1) %>% ungroup()


parse_swe_response <- function(js, station_id) {

  vals <- js[[1]]$data[[1]]$values

  if (is.null(vals) || length(vals) == 0) {
    return(NULL)
  }

  tibble(
    date  = as.Date(map_chr(vals, "date")),
    value = as.numeric(map_chr(vals, "value")),
    station_id = station_id
  )
}

swe_list <- future_pmap(
  .l = list(
    station_id  = stations_sntl_dt$station_id,
    begin_date  = stations_sntl_dt$beginDate,
    end_date    = stations_sntl_dt$endDate
  ),
  .f = get_swe_station,
  .options = furrr_options(seed = TRUE)
)

plan(sequential)

swe_df <- bind_rows(swe_list)



swe_df <- swe_df %>%
  mutate(
    year = year(date),
    month = month(date),
    day = day(date),
    month_day = format(date, "%m-%d")
  ) %>%
  filter(!is.na(value))

swe_df

library(data.table)
library(lubridate)

setDT(swe_df)

swe_df[, `:=`(
  year = year(date),
  month = month(date),
  day = day(date),
  month_day = format(date, "%m-%d")
)]

swe_df <- swe_df[!(month == 2 & day == 29) & !is.na(value)]
setorder(swe_df, station_id, month_day, year)

swe_metrics <- swe_df[, {

  por_vals <- value
  por_median <- median(por_vals, na.rm = TRUE)
  por_ecdf <- ecdf(por_vals)

  max_year <- max(year, na.rm = TRUE)
  clim_start <- 1991L
  clim_end <- 2020L

  clim_vals <- value[year >= clim_start & year <= clim_end]

  if (length(clim_vals) >= 30) {

    clim_median <- median(clim_vals, na.rm = TRUE)

    clim_pct_median <- if (clim_median > 0) {
      100 * value / clim_median
    } else {
      rep(NA_real_, .N)
    }

    # IMPORTANT: compute rank relative to clim_vals, not full value vector
    clim_percentile <- sapply(value, function(v) {
      100 * (sum(clim_vals < v) + 0.5 * sum(clim_vals == v)) / length(clim_vals)
    })

  } else {
    clim_median <- NA_real_
    clim_pct_median <- rep(NA_real_, .N)
    clim_percentile <- rep(NA_real_, .N)
  }

  por_pct_median <- if (isTRUE(por_median > 0)) {
    100 * value / por_median
  } else {
    rep(NA_real_, .N)
  }

  .(
    date = date,
    year = year,
    value = value,
    por_n = length(por_vals),
    por_median = por_median,
    por_pct_median = 100 * value / por_median,
    por_percentile = 100 * (rank(value, ties.method = "average") - 0.5) / .N,
    clim_n = length(clim_vals),
    clim_start_year = clim_start,
    clim_end_year = max_year,
    clim_median = clim_median,
    clim_pct_median = clim_pct_median,
    clim_percentile = clim_percentile
  )
}, by = .(station_id, month_day)]


library(data.table)

latest_metrics <- swe_metrics %>% tibble() %>%
                  filter(por_n >= 30 & clim_n >= 30)

latest_metrics_tibble <- latest_metrics %>% tibble() %>% left_join(stations_sntl_dt)

glimpse(latest_metrics_tibble)

ggplot(latest_metrics_tibble %>% filter(stateCode %in% c('ID', 'MT'), year == 2026) %>%
         mutate(month = month(date)), aes(x = clim_pct_median, y = clim_percentile, group = station_id)) +
  geom_point(alpha = 0.75,size = 1.5,aes(color = elevation)) +
  #geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1')) +

  scale_x_continuous(breaks = seq(0, 300, by = 10)) +
  scale_y_continuous(breaks = seq(0, 300, by = 10)) +
  ylim(c(0, 100)) +
  labs(
    title = "Climate Normal (1991–2020)",
    x = "% of Median",
    y = "Percentile"
  ) +
  geom_hline(yintercept = 50) +
  geom_vline(xintercept = 100) +
  facet_wrap(month~stateCode) +
  geom_smooth(se = F, method = 'lm', color = alpha("blue", 0.3),show.legend = F) +
  theme_bw()

ggplot(latest_metrics_tibble %>% filter(station_id == '654:ID:SNTL', month_day == '04-02'),
       aes(x = year, y = value)) +
  geom_point(alpha = 0.75,size = 3, aes(color = clim_pct_median)) +
  #geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_color_gradientn(colors = hcl.colors(11, 'Zissou1')) +
  theme_bw()

ggplot(latest_metrics_tibble %>% group_by(station_id) %>%
         filter(year == max(year), stateCode %in% c('ID', 'MT')) %>% ungroup() %>%
         mutate(month = month(date)) %>% filter(month %in% c(11,12,1,2,3,4)) %>% mutate(month = factor(month)) ,
       aes(x = value, group = station_id)) +
  stat_ecdf(alpha = 0.75,size = .25, aes(color = station_id), show.legend = F) +
  facet_grid(month~stateCode) +
  theme_bw()

library(terra)
ss <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_lidar_structure_stage_30m_8826.tif")
canopy <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_canopy_metrics_mosaic_30m_8826-rename.tif")
graph <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_graph_metrics_mosaic_30m_8826-rename.tif")
std <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_std_metrics_mosaic_30m_8826-rename.tif")

std <- std[[c("zmax","zmean","zsd","zskew","zkurt","zentropy", "zq5",
              "zq10" ,"zq15","zq20" ,"zq25","zq30" ,"zq35" ,"zq40","zq45" ,"zq50",
              "zq55" ,"zq60","zq65", "zq70","zq75" ,"zq80","zq85" ,"zq90" ,"zq95" )]]

names(std) <- paste0('std_', names(std))

graph <- graph[[c("understory_mean_degree","understory_mean_betweenness","understory_mean_closeness",
         "understory_n_components","midstory_mean_degree","midstory_mean_betweenness","midstory_mean_closeness",
         "midstory_n_components")]]

names(graph) <- paste0('graph_', names(graph))

canopy <- canopy[[c("LAI", "LAD_mean", "fractional_canopy_cover", "rumple_index", "n_trees",
          "trees_per_acre", "n_strata_low", "n_strata_low_mid",
          "n_strata_mid", "n_strata_mid_upper", "n_strata_upper", "n_gt_6_1", "n_gt_12_1",
          "n_gt_24_1")]]

names(canopy) <- paste0('ct_', names(canopy))

og <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/og_stand_gbm_fits_121125_msk2.tif")

names(og[[1]]) <- 'og_probability_stand'

ss <- ss[[1:4]]

names(ss[[1:3]]) <- paste0('ss_probability_', names(ss[[1:3]]))
names(ss[[4]]) <- paste0('ss_', names(ss[[4]]), '_class')

fs <- rast("/mnt/mordor3/data/lidar_download/canopy_metrics/NR_glmnet_predictions_with_interactions_negto0.tif")
fs <- fs[[1:6]]
names(fs[[1:6]]) <- paste0('fs_', names(fs[[1:6]]))

combined <- c(std, canopy, og, fs)

combined <- crop(combined, graph[[1]])

final_combined <- c(graph, combined, ss)

writeRaster(final_combined, '/mnt/mordor3/data/lidar_download/canopy_metrics/NR_ALS_vegetation_structure_and_derived_models_2010_to_2024.tif')

library(sf)

bb <- st_bbox(ss) %>% st_transform(4326)


