#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#       Visualisation Human Impact summary Europe
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Redo a visualisation from Climate outweighs human effects on vegetation properties during the early-to-mid Holocene
# https://doi.org/10.21203/rs.3.rs-4692574/v1


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

if (
  pak::pkg_status("RUtilpol") |>
    nrow() == 0
) {
  pak::pkg_install("HOPE-UIB-BIO/R-Utilpol-package")
}

library(RUtilpol)

# add climate zones labels
data_climate_zones <-
  tibble::tibble(
    climatezone = factor(
      c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      ),
      levels = c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      )
    ),
    climatezone_label = c(
      "Polar",
      "Cold - Cold Summer",
      "Cold - Warm Summer",
      "Cold - Hot Summer",
      "Cold - Dry Winter",
      "Cold - Dry Summer",
      "Temperate",
      "Temperate - Dry Winter",
      "Temperate - Dry Summer",
      "Tropical",
      "Arid"
    )
  )


#----------------------------------------------------------#
# 2. Get  data -----
#----------------------------------------------------------#

# The original data of the project is stored in a separate repository and is accessed can be downloaded from ZENODO: https://doi.org/10.5281/zenodo.11369243

data_records_meta <-
  RUtilpol::get_latest_file(
    file_name = "data_records_meta",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  )

data_records_meta_eu <-
  data_records_meta |>
  dplyr::filter(
    region == "Europe"
  )

data_records_meta_eu_temperate <-
  data_records_meta_eu |>
  dplyr::filter(
    climatezone == "Temperate"
  )


data_impact_by_records <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_records",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  add_predictors_as_factor()

data_impact_by_record_quantiles <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_record_quantiles",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  add_predictors_as_factor()

data_impact_by_climatezone <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_climatezone",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  add_predictors_as_factor()

data_impact_by_climatezone_eu <-
  data_impact_by_climatezone |>
  dplyr::filter(
    region == "Europe"
  )

data_impact_by_region <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_region",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  add_region_as_factor() |>
  add_predictors_as_factor()


#----------------------------------------------------------#
# 3. Visualisation setup -----
#----------------------------------------------------------#

col_land <- colours["grey"]

col_common_gray <- colours["grey"]

col_ecosystem <- colours["green"]

col_crema <- colours["white"]

col_white <- colours["white"]

palette_ecozones <-
  c(
    "Polar" = "#907A8E",
    "Cold - Cold Summer" = "#8C4418",
    "Cold - Warm Summer" = "#DC702E",
    # "Cold - Hot Summer" = "#AA6133",
    # "Cold - Dry Winter" = "#CB8152",
    "Cold - Dry Summer" = "#E59463",
    "Temperate" = "#371E71",
    # "Temperate - Dry Winter" = "#562FB1",
    "Temperate - Dry Summer" = "#9A7EDD",
    "Arid" = "#DDDF78"
    # "Tropical" = "#D68FD6"
  )

palette_predictors <- c(
  human = "#c99b38",
  climate = "#1f6f6f"
)

palette_ecozones_labels <-
  palette_ecozones |>
  rlang::set_names(
    nm = get_climatezone_label(names(palette_ecozones))
  )

text_size <- 15
line_size <- 0.1
point_size <- 1

region_label_wrap <- 10


#----------------------------------------------------------#
# 4. Importance -----
#----------------------------------------------------------#

sel_range <- c(0, 1)

p_summary_0 <-
  tibble::tibble() |>
  ggplot2::ggplot() +
  ggplot2::coord_cartesian(
    ylim = sel_range
  ) +
  ggplot2::theme(
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
    panel.spacing.y = grid::unit(5, "mm"),
    legend.position = "none",
    plot.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = col_land, # [config criteria]
      colour = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    )
  ) +
  ggplot2::scale_y_continuous(
    position = "right",
    expand = c(0.05, 0.05),
    breaks = seq(
      min(sel_range),
      max(sel_range),
      by = max(sel_range) / 4
    )
  ) +
  ggplot2::labs(
    x = "",
    y = "Ratio of importance"
  ) +
  ggplot2::geom_hline(
    yintercept = seq(0, 1, 0.25),
    col = colorspace::lighten(
      col_common_gray, # [config criteria]
      amount = 0.5
    ),
    linetype = 1,
    alpha = 0.5,
    linewidth = line_size # [config criteria]
  )


fig_summary_example_record <-
  data_impact_by_records |>
  dplyr::filter(dataset_id == 215) |>
  plot_summary_regions_points()

fig_summary_eu_temperate <-
  data_impact_by_records |>
  dplyr::filter(region == "Europe") |>
  dplyr::filter(climatezone == "Temperate") |>
  plot_summary_regions_points()

fig_summary_eu_temperate_quantile <-
  data_impact_by_record_quantiles |>
  dplyr::filter(region == "Europe") |>
  dplyr::filter(climatezone == "Temperate") |>
  plot_summary_regions_quatiles(
    data_source_climatezone = data_impact_by_climatezone |>
      dplyr::filter(region == "Europe") |>
      dplyr::filter(climatezone == "Temperate"),
  )

fig_summary_eu_quantile <-
  data_impact_by_record_quantiles |>
  dplyr::filter(region == "Europe") |>
  plot_summary_regions_quatiles(
    data_source_climatezone = data_impact_by_climatezone |>
      dplyr::filter(region == "Europe"),
  )

fig_summary_density_eu <-
  data_impact_by_records |>
  dplyr::filter(region == "Europe") |>
  plot_density_summary() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )


fig_summary_eu_quantile_no_axis <-
  fig_summary_eu_quantile +
  ggplot2::theme(
    strip.text.y = ggplot2::element_blank()
  )

fig_summary_eu_with_density <-
  cowplot::plot_grid(
    add_vertical_line(
      plot = fig_summary_density_eu,
      data_source = data_impact_by_region |>
        dplyr::filter(region == "Europe"),
    ),
    add_vertical_line(
      plot = fig_summary_eu_quantile_no_axis,
      data_source = data_impact_by_region |>
        dplyr::filter(region == "Europe"),
    ),
    nrow = 1,
    align = "h",
    rel_widths = c(2, 7)
  )
