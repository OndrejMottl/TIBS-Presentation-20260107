age_treshold <- 18e3
roc_treshold <- 2

time_bin <- 500


# Data from "Global acceleration in rates of vegetation change over the past 18,000 years"
# version 1.1.1
# see README for details (https://github.com/HOPE-UIB-BIO/Global_RoC/blob/v-1-1-1/README.md)

data_global_roc <-
  readr::read_rds(
    "https://github.com/HOPE-UIB-BIO/Global_RoC/raw/refs/heads/v-1-1-1/DATA/input/Dataset_20210601.rds"
  ) |>
  janitor::clean_names()


names(data_global_roc)


data_roc_with_bin <-
  data_global_roc |>
  dplyr::select(region, dataset_id, roc_main) |>
  tidyr::unnest(cols = roc_main) |>
  janitor::clean_names() |>
  dplyr::mutate(bin = ceiling(age / time_bin) * time_bin)


data_roc_sum <-
  data_roc_with_bin |>
  dplyr::group_by(region, bin) |>
  dplyr::summarise(
    .groups = "drop",
    n = n(),
    roc_mean = mean(roc, na.rm = TRUE),
    roc_median = median(roc, na.rm = TRUE),
    roc_upq = quantile(roc, 0.95, na.rm = TRUE),
    roc_sd = sd(roc, na.rm = TRUE)
  )


data_peak_sum <-
  data_roc_with_bin |>
  dplyr::group_by(region, dataset_id, bin) |>
  dplyr::summarise(
    .groups = "drop",
    peak_max = max(peak, na.rm = TRUE)
  ) |>
  dplyr::group_by(region, bin) |>
  dplyr::summarise(
    .groups = "drop",
    n = n(),
    peak_mean = mean(peak_max, na.rm = TRUE),
    peak_sd = sd(peak_max, na.rm = TRUE),
    peak_se = peak_sd / sqrt(n)
  ) |>
  dplyr::mutate(
    peak_mean = replace(peak_mean, is.na(peak_mean), 0),
    peak_sd = replace(peak_sd, is.na(peak_sd), 0)
  )

data_roc_to_fit <-
  dplyr::full_join(
    data_roc_sum,
    data_peak_sum,
    by = c("region", "bin"),
    suffix = c("_roc", "_peak")
  )


#----------------------------------------------------------#
# 2. RoC per continent -----
#----------------------------------------------------------#

plot_europe <-
  draw_region(
    sel_region = "Europe",
    data_binned = data_roc_to_fit
  )
