#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#                  Plot ROC input data
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Plot example data used for RRatepol estimation


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

if (
  pak::pkg_status("RRatepol") |>
    nrow() == 0
) {
  pak::pkg_install("HOPE-UIB-BIO/R-Ratepol-package")
}

library(RRatepol)

#----------------------------------------------------------#
# 2. Get example data -----
#----------------------------------------------------------#

data_example_age <-
  RRatepol::example_data$sample_age[[4]]

data_example_uncertainty <-
  RRatepol::example_data$age_uncertainty[[4]]

#----------------------------------------------------------#
# 3. Plot samples -----
#----------------------------------------------------------#


data_example_uncertainty_nested <-
  data_example_uncertainty |>
  as.data.frame() |>
  tibble::rowid_to_column("iteration") |>
  rlang::set_names(
    nm = c(
      "iteration",
      data_example_age$sample_id
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = !"iteration",
      .fns = as.numeric
    )
  ) |>
  tidyr::pivot_longer(
    names_to = "sample_id",
    values_to = "posible_ages",
    cols = !"iteration"
  ) |>
  dplyr::group_by(sample_id) |>
  tidyr::nest(posible_ages = c(iteration, posible_ages))

data_to_plot <-
  data_example_age |>
  tibble::as_tibble() |>
  dplyr::left_join(
    y = data_example_uncertainty_nested,
    by = "sample_id"
  ) |>
  tidyr::unnest(cols = c(posible_ages)) |>
  dplyr::mutate(
    age_factor = factor(
      age,
      levels = unique(sort(age, decreasing = TRUE))
    )
  )

p_depth_by_age <-
  data_to_plot |>
  dplyr::select(age_factor, posible_ages) |>
  ggplot2::ggplot(
    data = ,
    mapping = ggplot2::aes(
      x = posible_ages,
      y = age_factor,
      fill = 0.5 - abs(0.5 - ggplot2::after_stat(ecdf))
    )
  ) +
  ggridges::stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    rel_min_height = 0.03,
    color = NA
  ) +
  ggplot2::scale_x_reverse() +
  ggplot2::scale_fill_gradient(
    low = colours["blue"],
    high = colours["pink"]
  ) +
  ggplot2::coord_cartesian(
    xlim = c(8e3, 0)
  ) +
  ggplot2::labs(
    x = "Age (cal yr BP)",
    y = "Sample order",
    caption = "Shading indicates the density of possible ages for each sample",
    subtitle = "Age uncertainty"
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    margins = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    panel.spacing = ggplot2::unit(0.01, "cm")
  ) +
  ggview::canvas(
    width = 8,
    height = 4,
    dpi = 300,
    units = "cm"
  )


set.seed(900723)
selected_iterations <-
  sample(
    x = unique(data_to_plot$iteration),
    size = 100,
    replace = FALSE
  )

p_samples_density <-
  data_to_plot |>
  dplyr::filter(
    iteration %in% selected_iterations
  ) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = posible_ages,
      group = iteration
    )
  ) +
  ggplot2::stat_density(
    fill = colours["coral"],
    color = NA,
    trim = TRUE,
    alpha = 0.01
  ) +
  ggplot2::scale_x_reverse() +
  coord_cartesian(
    xlim = c(8e3, 0),
    ylim = c(0, 0.0005)
  ) +
  ggplot2::labs(
    x = "Age (cal yr BP)",
    y = "Density of samples",
    subtitle = "Sample age distributions",
    caption = "Density plots of possible ages for 100 random iterations"
  ) +
  theme_presentation() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    margins = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    panel.spacing = ggplot2::unit(0.01, "cm")
  ) +
  ggview::canvas(
    width = 8,
    height = 4,
    dpi = 300,
    units = "cm"
  )

#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

ggview::save_ggplot(
  plot = p_depth_by_age,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "example_data_age_uncertainty_ridges.png"
  )
)

ggview::save_ggplot(
  plot = p_samples_density,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "example_data_samples_density.png"
  )
)
