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
  dplyr::mutate(
    predictor_importance = scales::rescale(
      ratio_ind,
      to = c(-1, 1), from = c(0, 1)
    ),
    predictor_density_label = "Density"
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  add_predictors_as_factor()

data_impact_by_record_quantiles <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_record_quantiles",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  dplyr::mutate(
    predictor_importance_upr = scales::rescale(
      upr,
      to = c(-1, 1), from = c(0, 1)
    ),
    predictor_importance_lwr = scales::rescale(
      lwr,
      to = c(-1, 1), from = c(0, 1)
    )
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  add_predictors_as_factor()

data_impact_by_climatezone <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_climatezone",
    dir = here::here("Presentation/Materials/HumanImpact/Data")
  ) |>
  dplyr::mutate(
    predictor_importance = scales::rescale(
      ratio,
      to = c(-1, 1), from = c(0, 1)
    )
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
  dplyr::mutate(
    predictor_importance = scales::rescale(
      ratio,
      to = c(-1, 1), from = c(0, 1)
    )
  ) |>
  add_region_as_factor() |>
  add_predictors_as_factor()

data_impact_by_records_spatial <-
  data_impact_by_records |>
  dplyr::filter(predictor == "human") |>
  dplyr::distinct(dataset_id, predictor_importance) |>
  dplyr::inner_join(
    data_records_meta,
    by = "dataset_id"
  )


#----------------------------------------------------------#
# 3. Visualisation setup -----
#----------------------------------------------------------#

col_land <-
  colorspace::lighten(
    colours["orange"],
    amount = 0.8
  )

palette_predictors <- c(
  climate = colours["blue"],
  human = colours["green"]
)

text_size <- text_size_base
line_size <- 0.1
point_size <- 1

region_label_wrap <- 10


#----------------------------------------------------------#
# 4. Importance -----
#----------------------------------------------------------#

sel_range <- c(-1, 1)

p_summary_0 <-
  tibble::tibble() |>
  ggplot2::ggplot() +
  ggplot2::coord_cartesian(
    ylim = sel_range
  ) +
  theme_presentation() +
  ggplot2::theme(
    margins = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    panel.spacing = ggplot2::unit(0.01, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    plot.margin = grid::unit(c(5, 0, 0, 0), "mm"),
    legend.position = "none",
    plot.background = ggplot2::element_rect(
      fill = colours["white"], # [config criteria]
      colour = NA
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(
        t = 0,
        r = 0,
        b = 0,
        l = 0,
        unit = "mm"
      ),
      size = text_size, # [config criteria]
      color = colours["grey"] # [config criteria]
    ),
    panel.background = ggplot2::element_rect(
      fill = colours["white"], # [config criteria]
      colour = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = colours["white"], # [config criteria]
      colour = NA
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = colours["grey"] # [config criteria]
    ),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = colours["grey"] # [config criteria]
    )
  ) +
  ggplot2::scale_y_continuous(
    position = "right",
    expand = c(0.05, 0.05),
    breaks = seq(
      min(sel_range),
      max(sel_range),
      by = max(sel_range) / 2
    )
  ) +
  ggplot2::labs(
    x = "",
    y = "<- Human  - |Predictor importance| -  Climate ->"
  ) +
  ggplot2::geom_hline(
    yintercept = seq(-1, 1, 0.5),
    col = colorspace::lighten(
      colours["grey"], # [config criteria]
      amount = 0.5
    ),
    linetype = 1,
    alpha = 0.5,
    linewidth = line_size # [config criteria]
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    col = colours["black"], # [config criteria]
    linetype = 1,
    alpha = 1,
    linewidth = line_size * 5 # [config criteria]
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
    add_horizontal_line(
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
    rel_widths = c(3, 7)
  )


#----------------------------------------------------------#
# 5. Maps -----
#----------------------------------------------------------#


p0_map <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = long,
      y = lat
    ),
  ) +
  ggplot2::borders(
    fill = col_land,
    colour = NA
  ) +
  ggplot2::scale_fill_gradient2(
    low = palette_predictors["climate.blue"],
    high = palette_predictors["human.green"],
    na.value = colours["white"],
    midpoint = 0,
    mid = colours["grey"],
    limits = sel_range
  ) +
  ggplot2::scale_color_gradient2(
    low = palette_predictors["climate.blue"],
    high = palette_predictors["human.green"],
    na.value = colours["white"],
    midpoint = 0,
    mid = colours["grey"],
    limits = sel_range
  ) +
  ggplot2::coord_quickmap() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    panel.background = ggplot2::element_rect(
      fill = colours["white"],
      colour = colours["white"]
    ),
    plot.background = ggplot2::element_rect(
      fill = colours["white"],
      colour = colours["white"]
    ),
    panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
  ) +
  ggplot2::labs(
    x  = "Longitude",
    y  = "Latitude"
  )

fig_pollen_map_full <-
  p0_map +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial,
    mapping = ggplot2::aes(
      colour = predictor_importance
    ),
    size = 1
  )

fig_pollen_map_europe <-
  p0_map +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial |>
      dplyr::filter(region == "Europe"),
    mapping = ggplot2::aes(
      colour = predictor_importance
    ),
    size = 2
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )


fig_pollen_map_europe_temperate <-
  p0_map +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial |>
      dplyr::filter(region == "Europe") |>
      dplyr::filter(climatezone == "Temperate"),
    mapping = ggplot2::aes(
      colour = predictor_importance
    ),
    size = 3
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

fig_pollen_map_example_record <-
  p0_map +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial |>
      dplyr::filter(region == "Europe") |>
      dplyr::filter(climatezone == "Temperate"),
    colour = colours["grey"],
    size = 1
  ) +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial |>
      dplyr::filter(dataset_id == 215),
    mapping = ggplot2::aes(
      colour = predictor_importance
    ),
    size = 5
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

fig_map_example_record_bare <-
  p0_map +
  ggplot2::geom_point(
    data = data_impact_by_records_spatial |>
      dplyr::filter(dataset_id == 215),
    mapping = ggplot2::aes(
      colour = predictor_importance
    ),
    size = 5
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

#----------------------------------------------------------#
# 6. Save -----
#----------------------------------------------------------#


save_local_figure(
  plot = fig_summary_example_record,
  filename = "fig_summary_example_record.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_summary_eu_temperate,
  filename = "fig_summary_eu_temperate.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_summary_eu_temperate_quantile,
  filename = "fig_summary_eu_temperate_quantile.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_summary_eu_quantile,
  filename = "fig_summary_eu_quantile.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_summary_eu_with_density,
  filename = "fig_summary_eu_with_density.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_map_example_record_bare,
  filename = "fig_map_example_record_bare.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_pollen_map_example_record,
  filename = "fig_map_example_record.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_pollen_map_europe_temperate,
  filename = "fig_map_europe_temperate.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_pollen_map_europe,
  filename = "fig_map_europe.png",
  sel_width = 16 / 2
)

save_local_figure(
  plot = fig_pollen_map_full,
  filename = "fig_map_full.png",
  sel_width = 16
)

