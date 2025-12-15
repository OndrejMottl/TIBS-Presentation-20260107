#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#       Visualisation Asian phylogenetic latitudinal trend
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Redo a visualisation from Latitudinal gradients in the phylogenetic assembly of angiosperms in Asia during the Holocene
# https://doi.org/10.1038/s41598-024-67650-1

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
# https://doi.org/10.1038/s41598-024-67650-1
data_raw <-
  readr::read_rds(
    "https://github.com/HOPE-UIB-BIO/Latitudinal_phylodiversity_publication/raw/refs/tags/v1.1/Outputs/Data/Overall_gam_lat_PD_201223.rds"
  )

data_mntd_predicted <-
  data_raw |>
  dplyr::filter(vars == "mntd") |>
  dplyr::select(predicted_gam) |>
  tidyr::unnest(cols = c(predicted_gam))



#----------------------------------------------------------#
# 3. Make figures -----
#----------------------------------------------------------#

p0 <-
  data_mntd_predicted |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      ymin = lwr,
      ymax = upr,
      y = var
    )
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0),
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    margins = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    panel.spacing = ggplot2::unit(0.01, "cm"),
    legend.spacing = ggplot2::unit(0.001, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.key.spacing = ggplot2::unit(0.1, "cm"),
    legend.box.spacing = ggplot2::unit(0.01, "cm")
  ) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.5)
  ) +
  ggplot2::coord_cartesian(
    ylim = c(-1.25, 1.25)
  )

p1 <-
  p0 +
  ggplot2::scale_color_gradientn(
    colors = colours[c("blue", "pink", "coral")],
    name = "Age (cal yr BP)"
  ) +
  ggplot2::scale_fill_gradientn(
    colors = colours[c("blue", "pink", "coral")],
    name = "Age (cal yr BP)"
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      color = age,
      fill = age,
      group = age,
      x = lat,
    ),
    color = NA,
    alpha = 0.01
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      color = age,
      group = age,
      x = lat
    ),
    linewidth = 0.1
  ) +
  ggplot2::labs(
    x = "Latitude (°)",
    y = "Mean Nearest Taxon Distance (MNTD)"
  )

p2 <-
  p0 +
  ggplot2::scale_color_gradientn(
    colors = colours[c("orange", "green")],
    name = "Latitude (°)"
  ) +
  ggplot2::scale_fill_gradientn(
    colors = colours[c("orange", "green")],
    name = "Latitude (°)"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 12.5e3, by = 2.5e3),
    labels = seq(0, 12.5, by = 2.5)
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      color = lat,
      fill = lat,
      group = lat,
      x = age
    ),
    color = NA,
    alpha = 0.05
  ) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(
      group = lat,
      color = lat,
      x = age
    ),
    linewidth = 0.5
  ) +
  ggplot2::labs(
    x = "Age (ka BP)",
    y = "Mean Nearest Taxon Distance (MNTD)"
  )

p3 <-
  cowplot::plot_grid(
    p1,
    p2 +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ),
    ncol = 2,
    align = "hv"
  )


#----------------------------------------------------------#
# 4. Save figures -----
#----------------------------------------------------------#

ggplot2::ggsave(
  filename = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "Phylogenetic",
    "Latitudinal_trends.png"
  ),
  plot = p1,
  width = 16 / 2,
  height = 9,
  units = "cm",
  dpi = 300
)


ggplot2::ggsave(
  filename = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "Phylogenetic",
    "Temporal_trends.png"
  ),
  plot = p2,
  width = 16 / 2,
  height = 9,
  units = "cm",
  dpi = 300
)
