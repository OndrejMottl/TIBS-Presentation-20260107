#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Show data distribution of Mottl et al 2023
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Make a visualization of data from
# "Global acceleration in rates of vegetation change over the past 18,000 years"


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

palette_regions <-
  colorRampPalette(colours[-c(1:3)])(6) |>
  rlang::set_names(
    nm = c(
      "North America",
      "Europe",
      "Asia",
      "Latin America",
      "Africa",
      "Oceania"
    )
  )

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

data_to_plot <-
  data_global_roc |>
  dplyr::select(region, long, lat)


#----------------------------------------------------------#
# 3. Make figure -----
#----------------------------------------------------------#

p_disr <-
  data_to_plot |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = long,
      y = lat,
      fill = region
    )
  ) +
  ggplot2::coord_quickmap(
    ylim = c(-55, 72),
    xlim = c(-160, 180)
  ) +
  ggplot2::scale_fill_manual(
    values = palette_regions,
    na.value = colours[["grey"]]
  ) +
  ggplot2::labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::theme_void() +
  # theme_presentation() +
  ggplot2::theme(
    legend.position = "none",
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = colours[["white"]],
      colour = NA
    )
  ) +
  ggplot2::borders(
    fill = colours[["grey"]],
    colour = NA,
    linewidth = line_size_thin
  ) +
  ggplot2::geom_point(
    shape = 21,
    alpha = 0.7,
    size = 2,
    color = colours[["black"]],
  ) +
  ggview::canvas(
    width = 16,
    height = 7,
    units = "cm",
    dpi = 300
  )

#----------------------------------------------------------#
# 4. Save figure -----
#----------------------------------------------------------#

ggview::save_ggplot(
  plot = p_disr,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "Global_RoC",
    "Data_distribution.png"
  )
)
