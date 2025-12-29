#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Make a visualisation of Mottl et al 2023
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Make a step-by-step visualisation of the global RoC data from
# "Global acceleration in rates of vegetation change over the past 18,000 years"


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

age_treshold <- 18e3
roc_treshold <- 2
time_bin <- 500


#----------------------------------------------------------#
# 2. Get data -----
#----------------------------------------------------------#

# Data from "Global acceleration in rates of vegetation change over the past 18,000 years"
# version 1.1.1
# see README for details (https://github.com/HOPE-UIB-BIO/Global_RoC/blob/v-1-1-1/README.md)

data_global_roc <-
  readr::read_rds(
    "https://github.com/HOPE-UIB-BIO/Global_RoC/raw/refs/heads/v-1-1-1/DATA/input/Dataset_20210601.rds"
  ) |>
  janitor::clean_names()


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

data_models <-
  data_roc_to_fit |>
  dplyr::group_by(region) |>
  tidyr::nest() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    mod = purrr::map(
      .x = data,
      .f = ~ fit_gam_model(
        var_y = "roc_upq",
        var_x = "bin",
        family = "tw()",
        data = .x,
        weights = "n_roc"
      )
    )
  )

data_mod_predictions <-
  data_models |>
  dplyr::mutate(
    data_pred = purrr::map(
      .x = mod,
      .f = ~ predict_gam(
        gam_model = .x,
        var_x = "bin",
        deriv = TRUE
      )
    )
  ) |>
  tidyr::unnest_wider(data_pred, names_repair = "unique⁠") |>
  rlang::set_names(
    nm = c(
      "region",
      "data",
      "mod",
      "data_pred",
      "k",
      "p_value",
      "data_first_derivative"
    )
  ) |>
  dplyr::mutate(
    data_pred_with_deriv = purrr::map2(
      .x = data_pred,
      .y = data_first_derivative,
      .f = ~ dplyr::left_join(
        x = .x,
        y = .y |>
          dplyr::mutate(
            direction_change = dplyr::case_when(
              d_lower < 0 ~ "increasing",
              d_lower > 0 ~ "decreasing",
              TRUE ~ "no_change"
            ),
          ) |>
          dplyr::select(
            bin,
            significante_change,
            direction_change
          ),
        by = "bin"
      )
    )
  )


#----------------------------------------------------------#
# 3. Visualisation -----
#----------------------------------------------------------#

palette_regions <-
  colorRampPalette(colours[-c(1:3)])(6) |>
  rlang::set_names(
    nm = c(
      "North America",
      "Europe",
      "Asia",
      "South America",
      "Africa",
      "Oceania"
    )
  )


data_to_plot <-
  data_mod_predictions |>
  dplyr::mutate(
    hemisphere = dplyr::case_when(
      .default = "Southern Hemisphere",
      region %in% c("North America", "Europe", "Asia") ~ "Northern Hemisphere"
    ),
    hemisphere = factor(
      hemisphere,
      levels = c("Northern Hemisphere", "Southern Hemisphere")
    ),
    region = dplyr::case_when(
      .default = region,
      region == "Latin America" ~ "South America"
    ),
    region = factor(
      region,
      levels = names(palette_regions)
    )
  ) |>
  dplyr::select(
    hemisphere, region, data, data_pred_with_deriv, p_value
  )


# empty facets

(
  p0 <-
    # fake data to force the plot to draw the axis
    tibble::tibble(
      region = factor(
        names(palette_regions),
        levels = names(palette_regions)
      ),
      bin = -1,
      roc_upq = -1
    ) |>
    ggplot2::ggplot() +
    ggplot2::facet_wrap(
      facets = ggplot2::vars(region),
      ncol = 3,
      nrow = 2
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      breaks = seq(0, age_treshold, by = 2e3),
      labels = seq(0, age_treshold / 1e3, by = 2)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales::comma
    ) +
    ggplot2::scale_color_manual(
      values = palette_regions
    ) +
    ggplot2::scale_fill_manual(
      values = palette_regions
    ) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Rate of vegetation change "
    ) +
    ggplot2::coord_cartesian(
      xlim = c(age_treshold, 0),
      ylim = c(0.3, 0.9),
      expand = FALSE
    ) +
    theme_presentation() +
    ggplot2::theme(
      legend.position = "none",
      strip.clip = "off",
      strip.text = ggplot2::element_text(
        margin = ggplot2::margin(
          b = -20,
          l = 8,
        ),
      ),
      strip.background = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = bin,
        y = roc_upq
      )
    ) +
    ggview::canvas(
      width = 16,
      height = 9,
      units = "cm",
      dpi = 300
    )
)

(
  p1 <-
    p0 +
    ggplot2::geom_point(
      data = data_to_plot |>
        tidyr::unnest(data),
      mapping = ggplot2::aes(
        x = bin,
        y = roc_upq,
        color = region
      ),
      size = 1,
      alpha = 1,
      shape = 15
    )
)

(
  p2 <-
    p1 +
    ggplot2::geom_ribbon(
      data = data_to_plot |>
        tidyr::unnest(data_pred_with_deriv),
      mapping = ggplot2::aes(
        x = bin,
        ymin = lower,
        ymax = upper,
        fill = region
      ),
      alpha = 0.3
    ) +
    ggplot2::geom_line(
      data = data_to_plot |>
        tidyr::unnest(data_pred_with_deriv),
      mapping = ggplot2::aes(
        x = bin,
        y = fit,
        color = region
      ),
      linewidth = 1
    )
)

(
  p3 <-
    p0 +
    ggplot2::geom_point(
      data = data_to_plot |>
        tidyr::unnest(data),
      mapping = ggplot2::aes(
        x = bin,
        y = roc_upq
      ),
      size = 1,
      alpha = 1,
      shape = 15,
      color = colours["grey"]
    ) +
    ggplot2::geom_ribbon(
      data = data_to_plot |>
        tidyr::unnest(data_pred_with_deriv),
      mapping = ggplot2::aes(
        x = bin,
        ymin = lower,
        ymax = upper
      ),
      alpha = 0.3,
      fill = colours["grey"]
    ) +
    ggplot2::geom_line(
      data = data_to_plot |>
        tidyr::unnest(data_pred_with_deriv),
      mapping = ggplot2::aes(
        x = bin,
        y = fit,
      ),
      linewidth = 0.5,
      color = colours["grey"]
    ) +
    ggplot2::geom_line(
      data = data_to_plot |>
        tidyr::unnest(data_pred_with_deriv) |>
        dplyr::filter(
          significante_change == TRUE,
          bin < 10e3,
          direction_change == "increasing"
        ),
      mapping = ggplot2::aes(
        x = bin,
        y = fit,
        color = region
      ),
      linewidth = 2
    )
)

# save all plots

c(
  p0,
  p1,
  p2,
  p3
) |>
  rlang::set_names(
    nm = c(
      "p0_empty_plot",
      "p1_data_points",
      "p2_gam_fit_with_ci",
      "p3_significant_changes"
    )
  ) |>
  purrr::iwalk(
    .progress = TRUE,
    .f = ~ ggview::save_ggplot(
      file = here::here(
        "Presentation",
        "Materials",
        "R_generated",
        "Global_RoC",
        paste0(.y, ".png")
      ),
      plot = .x
    )
  )
