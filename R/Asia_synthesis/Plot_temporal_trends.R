#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#          Visualisation Asian palynological synthesis
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Redo a visualisation from Exploring spatio-temporal patterns of palynological changes in Asia during the Holocene
# https://doi.org/10.3389/fevo.2023.1115784


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)


#----------------------------------------------------------#
# 2. Get  data -----
#----------------------------------------------------------#

# Data obtained from:
# https://doi.org/10.3389/fevo.2023.1115784

data_raw <-
  readr::read_rds(
    "https://github.com/HOPE-UIB-BIO/Asian_palynological_synthesis/raw/refs/tags/v1.0.1/Data/Processed/Data_for_temporal_plotting/Data_for_temporal_plotting-2022-10-28.rds"
  )



data_to_plot <-
  data_raw |>
  dplyr::filter(
    var_name %in% c("N0", "DCCA1", "N2 divided by N1"),
    var_type == "var"
  ) |>
  tidyr::unnest(merge_data) |>
  dplyr::mutate(
    var_name = dplyr::case_when(
      var_name == "N0" ~ "Pollen richness",
      var_name == "DCCA1" ~ "Turnover",
      var_name == "N2 divided by N1" ~ "Evenness",
      TRUE ~ var_name
    ),
    line_alpha = dplyr::case_when(
      .default = 1,
      grain == "sequence" ~ 0.05,
      grain == "climate-zone" ~ 0.5,
      grain == "continent" ~ 1
    ),
    ribbon_alpha = dplyr::case_when(
      .default = 0.5,
      grain == "sequence" ~ 0.01,
      grain == "climate-zone" ~ 0.3,
      grain == "continent" ~ 0.5
    ),
    grouping_var = dplyr::case_when(
      grain == "sequence" ~ dataset_id,
      grain == "climate-zone" ~ Climate_zone,
      grain == "continent" ~ "Continent",
    ),
    dataset_id = dplyr::case_when(
      is.na(dataset_id) ~ "different grain",
      .default = dataset_id
    ),
    Climate_zone = dplyr::case_when(
      is.na(Climate_zone) ~ "different grain",
      .default = Climate_zone
    ),
    grain = factor(
      grain,
      levels = c("sequence", "climate-zone", "continent"),
      labels = c("Site", "Climate zone", "Continent")
    ),
    var_name = factor(
      var_name,
      levels = c("Pollen richness", "Evenness", "Turnover")
    ),
    dplyr::across(
      where(is.character),
      as.factor
    )
  )

palette_climate_zones <-
  colorRampPalette(colours[-c(1:3, 9)])(5) |>
  rlang::set_names(
    nm = c(
      "Arid",
      "Cold - Dry",
      "Cold - Without dry season",
      "Temperate",
      "Polar"
    )
  )

p0 <-
  data_to_plot |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = age,
      y = var,
      color = Climate_zone,
      group = grouping_var,
      ymin = lwr,
      ymax = upr,
      fill = Climate_zone
    )
  ) +
  ggplot2::facet_grid(
    var_name ~ grain,
    scales = "free_y",
    switch = "y"
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0.05, 0.1))
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse",
    breaks = seq(0, 12e3, by = 2e3),
    labels = seq(0, 12, by = 2)
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "none",
    legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    strip.clip = "off",
    axis.title.y = ggplot2::element_blank(),
    legend.spacing = ggplot2::unit(0.001, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.key.spacing = ggplot2::unit(0.001, "cm"),
    legend.box.spacing = ggplot2::unit(0.001, "cm"),
    strip.background = ggplot2::element_rect(fill = colours[["grey"]], colour = NA)
  ) +
  ggplot2::scale_color_manual(
    values = palette_climate_zones,
    na.value = colours[["green"]],
    breaks = c(
      "Arid",
      "Cold - Dry",
      "Cold - Without dry season",
      "Temperate",
      "Polar"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = palette_climate_zones,
    na.value = colours[["green"]],
    breaks = c(
      "Arid",
      "Cold - Dry",
      "Cold - Without dry season",
      "Temperate",
      "Polar"
    )
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
    colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  ) +
  ggplot2::labs(
    x = "Age (ka BP)",
    y = "Value of variable",
    colour = "",
    fill = ""
  ) +
  ggview::canvas(
    width = 16,
    height = 7,
    units = "cm",
    dpi = 300
  )

# Site level
p1 <-
  p0 +
  ggplot2::geom_ribbon(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Site"
      ),
    alpha = 0.02,
    color = NA
  ) +
  ggplot2::geom_line(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Site"
      ),
    alpha = 0.05
  )

# Climate zone level
p2 <-
  p1 +
  ggplot2::geom_ribbon(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Climate zone"
      ),
    alpha = 0.3,
    color = NA
  ) +
  ggplot2::geom_line(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Climate zone"
      ),
    alpha = 1
  )


# Continent level
p3 <-
  p2 +
  ggplot2::geom_ribbon(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Continent"
      ),
    alpha = 0.5,
    color = NA
  ) +
  ggplot2::geom_line(
    data = data_to_plot |>
      dplyr::filter(
        grain == "Continent"
      ),
    alpha = 1
  )

c(
  p0,
  p1,
  p2,
  p3
) |>
  rlang::set_names(
    nm = c(
      "p0_empty_plot",
      "p1_site_level",
      "p2_climate_zone_level",
      "p3_continent_level"
    )
  ) |>
  purrr::iwalk(
    .progress = TRUE,
    .f = ~ ggview::save_ggplot(
      file = here::here(
        "Presentation",
        "Materials",
        "R_generated",
        "Asia",
        paste0("temporal_", .y, ".png")
      ),
      plot = .x
    )
  )
