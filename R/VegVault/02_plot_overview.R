#----------------------------------------------------------#
#
#
#           VegVault - Presentation- 20251210
#
#         Make data overview for South Bohemia
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(
  "here",
  quietly = TRUE,
  warn.conflicts = FALSE,
  character.only = TRUE,
  verbose = FALSE
)

source(
  here::here("R", "___setup_project___.R")
)

coord_buffer <- 1

y_lim <- c(54.5, 58.0)
x_lim <- c(8.0, 13.0)
age_lim <- c(0, 20000)

# plan the dimensions of the output image
image_width_full <- 16
image_height_full <- 9

image_width_half <- image_width_full / 2

p1_width <- image_width_half * 0.5
p3_width <- image_width_half * 0.3
p4_width <- image_width_half * 0.2

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_denmark <-
  readr::read_rds(
    here::here(
      "Presentation/Materials/VegVault",
      "data_denmark.rds"
    )
  )


data_altitude_raw <-
  readr::read_rds(
    here::here("Presentation/Materials/VegVault/Terrain/data_altitude_raw.rds")
  ) |>
  dplyr::filter(
    long >= x_lim[1] - coord_buffer,
    long <= x_lim[2] + coord_buffer,
    lat >= y_lim[1] - coord_buffer,
    lat <= y_lim[2] + coord_buffer
  )


#----------------------------------------------------------#
# 2. spatial plot -----
#----------------------------------------------------------#

data_p1 <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id %in% c(1, 2) & age <= 200
  ) |>
  dplyr::distinct(dataset_id, sample_id, .keep_all = TRUE) |>
  dplyr::select(
    dataset_type_id,
    sample_id,
    coord_long, coord_lat
  ) |>
  dplyr::left_join(
    data_denmark |>
      dplyr::filter(
        dataset_type_id == 4 & age <= 200
      ) |>
      dplyr::distinct(
        sample_id_link,
        abiotic_value
      ),
    by = c("sample_id" = "sample_id_link")
  ) |>
  dplyr::mutate(
    dataset_type_id_name = dplyr::case_when(
      dataset_type_id == 1 ~ "Vegetation plot",
      dataset_type_id == 2 ~ "Fossil pollen archive",
      TRUE ~ NA_character_
    ),
    dataset_type_id_name = factor(
      dataset_type_id_name,
      levels = c(
        "Vegetation plot",
        "Fossil pollen archive"
      )
    )
  )

p1_with_with_legend <-
  data_p1 |>
  ggplot2::ggplot() +
  ggplot2::borders(
    fill = colours["grey"],
    col = NA,
  ) +
  # vegetation
  ggplot2::geom_point(
    ggplot2::aes(
      x = coord_long,
      y = coord_lat,
      fill = abiotic_value,
      col = dataset_type_id_name,
      shape = dataset_type_id_name,
      size = dataset_type_id_name
    ),
    alpha = 0.9
  ) +
  ggplot2::geom_point(
    data = data_p1 |>
      dplyr::distinct(dataset_type_id, coord_long, coord_lat),
    ggplot2::aes(
      x = coord_long,
      y = coord_lat
    ),
    size = 0.1,
    col = colours["black"],
    alpha = 0.9
  ) +
  ggplot2::coord_quickmap(
    xlim = x_lim,
    ylim = y_lim
  ) +
  ggplot2::scale_fill_gradient(
    low = colours["green"],
    high = colours["white"],
    breaks = scales::pretty_breaks(n = 3),
    limits = c(-5, 10),
    name = "MAT (°C)"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      colours["coral"],
      colours["blue"]
    ) |>
      rlang::set_names(
        nm = c(
          "Vegetation plot",
          "Fossil pollen archive"
        )
      ),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    )
  ) +
  ggplot2::scale_shape_manual(
    values = c(21, 22),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    )
  ) +
  ggplot2::scale_size_manual(
    values = c(2, 3),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::guides(
    shape = ggplot2::guide_legend(
      override.aes = list(
        size = 5,
        fill = c(
          colours["coral"],
          colours["blue"]
        ),
        color = NA
      ),
      nrow = 2,
      ncol = 1
    )
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "right",
    plot.margin = ggplot2::margin(0, 0, 0, 0),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggview::canvas(
    width = 4,
    height = 5,
    units = "cm",
    dpi = 300
  )

legend_points <-
  cowplot::get_legend(
    p1_with_with_legend +
      ggplot2::guides(
        fill = "none"
      )
  )

p1_without_legend <-
  p1_with_with_legend +
  ggplot2::theme(
    legend.position = "none"
  )


#----------------------------------------------------------#
# 3. temporal plot -----
#----------------------------------------------------------#

data_p2 <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id %in% c(1, 2)
  ) |>
  dplyr::distinct(dataset_id, dataset_type_id, sample_id, age) |>
  dplyr::mutate(
    dataset_type_id_name = dplyr::case_when(
      dataset_type_id == 1 ~ "Vegetation plot",
      dataset_type_id == 2 ~ "Fossil pollen archive",
      TRUE ~ NA_character_
    ),
    dataset_type_id_name = factor(
      dataset_type_id_name,
      levels = c(
        "Vegetation plot",
        "Fossil pollen archive"
      )
    )
  )

p2_with_legend <-
  data_p2 |>
  ggplot(
    ggplot2::aes(
      x = age,
      fill = dataset_type_id_name
    )
  ) +
  scale_y_continuous(
    trans = "log1p",
    breaks = c(1, 10, 100, 1e3, 1e4, 1e5, 1e6),
    labels = c("1", "10", "100", "1000", "10000", "1e+05", "1e+06")
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 5e3),
    labels = seq(0, 20, 5),
  ) +
  ggplot2::coord_cartesian(
    xlim = c(20e3, 0)
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      colours["coral"],
      colours["blue"]
    ) |>
      rlang::set_names(
        nm = c(
          "Vegetation plot",
          "Fossil pollen archive"
        )
      ),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    )
  ) +
  geom_histogram(
    binwidth = 1000,
    col = colours["black"],
    linewidth = 0.1
  ) +
  ggplot2::labs(
    x = "Age (cal ka yr BP)",
    y = "Number of samples"
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "right",
    plot.margin = ggplot2::margin(0, 0, 0, 0),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggview::canvas(
    width = 4,
    height = 5,
    units = "cm",
    dpi = 300
  )

legend_type <-
  cowplot::get_legend(
    p2_with_legend
  )

p2_without_legend <-
  p2_with_legend +
  ggplot2::theme(
    legend.position = "none"
  )

p2_without_legend

#----------------------------------------------------------#
# 4. MAT per time -----
#----------------------------------------------------------#

data_p3 <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id == 4,
  ) |>
  dplyr::distinct(dataset_id, sample_id, sample_id_link, .keep_all = TRUE) |>
  tidyr::drop_na(age, abiotic_value) |>
  dplyr::select(
    dataset_id,
    sample_id,
    sample_id_link,
    age,
    abiotic_value
  ) |>
  dplyr::left_join(
    data_denmark |>
      dplyr::filter(
        dataset_type_id %in% c(1, 2)
      ) |>
      dplyr::distinct(
        dataset_id, sample_id, dataset_type_id
      ),
    by = c("sample_id_link" = "sample_id"),
    suffix = c("_gridpoint", "_vegetation")
  ) |>
  tidyr::drop_na(dataset_type_id) |>
  dplyr::distinct(
    dataset_type_id, dataset_id_vegetation, age, abiotic_value
  ) |>
  dplyr::mutate(
    dataset_type_id_name = dplyr::case_when(
      dataset_type_id == 1 ~ "Vegetation plot",
      dataset_type_id == 2 ~ "Fossil pollen archive",
      TRUE ~ NA_character_
    ),
    dataset_type_id_name = factor(
      dataset_type_id_name,
      levels = c(
        "Vegetation plot",
        "Fossil pollen archive"
      )
    )
  )

p3_with_legend <-
  data_p3 |>
  ggplot2::ggplot() +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    breaks = seq(0, 20e3, 5e3),
    labels = seq(0, 20, 5),
  ) +
  ggplot2::coord_cartesian(
    xlim = c(20e3, 0)
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = age,
      y = abiotic_value,
      group = dataset_id_vegetation
    ),
    linewidth = 0.5,
    col = colours["grey"],
    alpha = 0.9
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = age,
      y = abiotic_value,
      fill = abiotic_value,
      col = dataset_type_id_name,
      shape = dataset_type_id_name,
      size = dataset_type_id_name
    ),
    alpha = 0.7
  ) +
  ggplot2::geom_point(
    ggplot2::aes(
      x = age,
      y = abiotic_value
    ),
    size = 0.1,
    col = colours["black"],
    alpha = 0.9
  ) +
  ggplot2::scale_fill_gradient(
    low = colours["green"],
    high = colours["white"],
    breaks = scales::pretty_breaks(n = 3),
    limits = c(-5, 10),
    name = "MAT (°C)"
  ) +
  ggplot2::scale_color_manual(
    values = c(
      colours["coral"],
      colours["blue"]
    ) |>
      rlang::set_names(
        nm = c(
          "Vegetation plot",
          "Fossil pollen archive"
        )
      ),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    )
  ) +
  ggplot2::scale_shape_manual(
    values = c(21, 22),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    )
  ) +
  ggplot2::scale_size_manual(
    values = c(2, 2),
    name = "Dataset type",
    labels = c(
      "Vegetation plot",
      "Fossil pollen archive"
    ),
    guide = "none"
  ) +
  ggplot2::labs(
    x = "Age (cal ka yr BP)",
    y = "MAT (°C)"
  ) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title.position = "top",
    plot.margin = ggplot2::margin(0, 0, 0, 0),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggview::canvas(
    width = 6,
    height = 4,
    units = "cm",
    dpi = 300
  )

legend_temperature <-
  cowplot::get_legend(
    p3_with_legend +
      ggplot2::guides(
        shape = "none",
        col = "none"
      )
  )

p3_without_legend <-
  p3_with_legend +
  ggplot2::theme(
    legend.position = "none"
  )


#----------------------------------------------------------#
# 5. traits -----
#----------------------------------------------------------#

vec_taxa_id_traits <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id == 3
  ) |>
  dplyr::distinct(taxon_id_trait) |>
  tidyr::drop_na() |>
  dplyr::pull(taxon_id_trait)

vec_taxa_id_vegetation <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id %in% c(1, 2)
  ) |>
  dplyr::distinct(taxon_id) |>
  tidyr::drop_na() |>
  dplyr::pull(taxon_id)

vec_taxa_all <-
  c(
    vec_taxa_id_traits,
    vec_taxa_id_vegetation
  ) |>
  unique()

con <-
  DBI::dbConnect(
    RSQLite::SQLite(),
    path_to_vegvault
  )

data_class_table <-
  dplyr::tbl(con, "TaxonClassification") |>
  dplyr::filter(
    taxon_id %in% vec_taxa_all
  ) |>
  dplyr::select(taxon_id, taxon_genus) |>
  dplyr::collect()


data_to_plot_traits <-
  data_denmark |>
  dplyr::filter(
    dataset_type_id == 3
  ) |>
  tidyr::drop_na(trait_domain_id, trait_name, trait_value) |>
  dplyr::distinct(
    dataset_id,
    sample_id,
    taxon_id_trait,
    .keep_all = TRUE
  ) |>
  dplyr::select(
    taxon_id_trait,
    trait_value
  ) |>
  dplyr::left_join(
    data_class_table,
    by = dplyr::join_by("taxon_id_trait" == "taxon_id")
  ) |>
  dplyr::group_by(
    taxon_genus
  ) |>
  dplyr::summarise(
    trait_value = mean(trait_value, na.rm = TRUE),
    .groups = "drop"
  )

vec_outliers <-
  data_to_plot_traits |>
  purrr::chuck("trait_value") |>
  boxplot(plot = FALSE) |>
  purrr::chuck("out")

data_to_plot_traits_filtered <-
  data_to_plot_traits |>
  dplyr::filter(
    !trait_value %in% vec_outliers
  )


p4 <-
  data_to_plot_traits_filtered |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = trait_value,
      x = 1
    ),
  ) +
  ggplot2::geom_violin(
    col = NA,
    fill = colours["orange"]
  ) +
  ggplot2::geom_boxplot(
    col = colours["black"],
    outlier.shape = NA,
    width = 0.1,
  ) +
  ggplot2::labs(
    y = "Average plant height per genus (cm)",
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 5)
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    legend.position = "none",
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  ) +
  ggview::canvas(
    width = 2,
    height = 4,
    units = "cm",
    dpi = 300
  )


#----------------------------------------------------------#
# 6. Save plots -----
#----------------------------------------------------------#

ggview::save_ggplot(
  plot = p1_without_legend,
  file = here::here(
    "Presentation/Materials/R_generated/VegVault/plot_spatial.png"
  )
)

ggview::save_ggplot(
  plot = p2_without_legend,
  file = here::here(
    "Presentation/Materials/R_generated/VegVault/plot_temporal.png"
  )
)

ggview::save_ggplot(
  plot = p3_without_legend,
  file = here::here(
    "Presentation/Materials/R_generated/VegVault/plot_mat_time.png"
  )
)

ggview::save_ggplot(
  plot = p4,
  file = here::here(
    "Presentation/Materials/R_generated/VegVault/plot_traits.png"
  )
)
