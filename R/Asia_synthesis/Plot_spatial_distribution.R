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
# https://doi.org/10.3389/fevo.2023.1115784x

data_spatial_trends <-
  readr::read_rds(
    "https://github.com/HOPE-UIB-BIO/Asian_palynological_synthesis/raw/refs/tags/v1.0.1/Data/Processed/Data_for_analyses/Data_for_analyses-2022-09-29.rds"
  ) |>
  dplyr::select(
    dataset_id,
    long, lat,
    Climate_zone,
    dcca_grad_length,
    mvrt_groups_n
  ) |>
  janitor::clean_names()

beck_raster_file <-
  raster::raster(
    "https://github.com/HOPE-UIB-BIO/Asian_palynological_synthesis/raw/refs/tags/v1.0.1/Data/Input/Biomes_spatial/Beck_KG_V1_present_0p083.tif"
  )

# Read the raster value-climatic zone tranlation table
koppen_tranlation_table <-
  readr::read_csv(
    "https://raw.githubusercontent.com/HOPE-UIB-BIO/Asian_palynological_synthesis/refs/tags/v1.0.1/Data/Input/Biomes_spatial/koppen_link.csv"
  )

# Extract the required raster points
data_raster_df <-
  # Convert raster points into a dataframe
  as.data.frame(beck_raster_file, xy = TRUE) |>
  tibble::as_tibble() |>
  # Extract the rater points of only required area
  dplyr::filter(x > 20 & y > 0) |> # for required lat, long
  dplyr::rename(raster_values = Beck_KG_V1_present_0p083) |>
  dplyr::mutate(raster_values = round(raster_values, digits = 0)) |>
  # Assign the names of climate zone to the raster values
  left_join(koppen_tranlation_table, by = c("raster_values")) |>
  dplyr::filter(!raster_values == 0) |>
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome
  ) |>
  dplyr::mutate(
    Climate_zone = dplyr::case_when(
      ecozone_koppen_15 == "Arid_Desert" ~ "Arid",
      ecozone_koppen_15 == "Arid_Steppe" ~ "Arid",
      ecozone_koppen_15 == "Cold_Dry_Summer" ~ "Cold - Dry",
      ecozone_koppen_15 == "Cold_Dry_Winter" ~ "Cold - Dry",
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ "Cold - Without dry season",
      ecozone_koppen_15 == "Polar_Frost" ~ "Polar - Frost",
      ecozone_koppen_15 == "Polar_Tundra" ~ "Polar",
      ecozone_koppen_15 == "Temperate_Dry_Summer" ~ "Temperate",
      ecozone_koppen_15 == "Temperate_Dry_Winter" ~ "Temperate",
      ecozone_koppen_15 == "Temperate_Without_dry_season" ~ "Temperate",
      ecozone_koppen_15 == "Tropical_Monsoon" ~ "Tropical - Monsoon",
      ecozone_koppen_15 == "Tropical_Rainforest" ~ "Tropical - Rainforest",
      ecozone_koppen_15 == "Tropical_Savannah" ~ "Tropical - Savannah",
      TRUE ~ ecozone_koppen_15
    )
  ) |>
  janitor::clean_names()



#----------------------------------------------------------#
# 3. Make plot -----
#----------------------------------------------------------#

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

p1 <-
  data_spatial_trends |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = long,
      y = lat
    )
  ) +
  ggplot2::coord_fixed(
    ylim = c(5.00, 80.00),
    xlim = c(30.00, 173.00)
  ) +
  ggplot2::scale_fill_manual(
    values = palette_climate_zones,
    na.value = colours[["grey"]]
  ) +
  ggplot2::labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Climate zones"
  ) +
  theme_presentation() +
  ggplot2::geom_tile(
    data = data_raster_df,
    aes(x = x, y = y, fill = climate_zone),
    inherit.aes = FALSE, alpha = 0.66
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      nrow = 3,
      byrow = TRUE,
      title.position = "top"
    ),
    size = ggplot2::guide_legend(
      nrow = 1,
      byrow = TRUE,
      title.position = "left"
    )
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal"
  ) +
  ggplot2::borders(
    colour = colours[["grey"]],
    linewidth = line_size_thin
  ) +
  ggplot2::geom_point(
    data = data_spatial_trends,
    mapping = ggplot2::aes(
      x = long,
      y = lat,
      fill = climate_zone
    ),
    shape = 21,
    alpha = 0.9,
    size = 2,
    color = colours[["black"]]
  )

p1_no_legend <-
  p1 +
  ggplot2::theme(
    legend.position = "none"
  )

  
#----------------------------------------------------------#
# 4. Save plot -----
#----------------------------------------------------------#


ggplot2::ggsave(
  filename = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "Asia",
    "Asia_spatial_trends.png"
  ),
  plot = p1_no_legend,
  width = 16,
  height = 9,
  units = "cm",
  dpi = 300,
  bg = colours[["white"]]
)
