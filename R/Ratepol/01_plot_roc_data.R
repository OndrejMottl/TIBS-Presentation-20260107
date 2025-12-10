#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Make a visualisation of ROC data
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Obtain information about all Neotoma sequences and make plots


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

data_example_pollen <-
  RRatepol::example_data$pollen_data[[4]] |>
  janitor::clean_names()

data_example_pollen_sum <-
  data_example_pollen |>
  tibble::column_to_rownames(
    var = "sample_id"
  ) |>
  rowSums() |>
  tibble::enframe(
    name = "sample_id",
    value = "pollen_sum"
  )

min_pollen_sum <-
  data_example_pollen_sum |>
  dplyr::pull(
    pollen_sum
  ) |>
  min()

set.seed(19900723)
data_pollen_subsampled <-
  data_example_pollen |>
  tibble::column_to_rownames(
    var = "sample_id"
  ) |>
  vegan::rrarefy(
    sample = max(c(100, min_pollen_sum))
  ) |>
  as.data.frame() |>
  tibble::rownames_to_column(
    var = "sample_id"
  ) |>
  tibble::as_tibble()


data_example_age <-
  RRatepol::example_data$sample_age[[4]]

data_example_uncertainty <-
  RRatepol::example_data$age_uncertainty[[4]]

set.seed(19900723)
data_roc_levels <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "levels",
    time_standardisation = 500
  )

RRatepol::plot_roc(data_roc_levels)

set.seed(19900723)
data_roc_bins <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    rand = 1,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "bins"
  )

RRatepol::plot_roc(data_roc_bins)

set.seed(19900723)
data_roc_mw <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW"
  )

RRatepol::plot_roc(data_roc_mw)


set.seed(19900723)
data_roc_mw_standardise <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1e3,
    standardise = TRUE,
    n_individuals = 150,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW",
    use_parallel = TRUE
  )

RRatepol::plot_roc(data_roc_mw_standardise)

set.seed(19900723)
data_roc_with_uncertainty <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    age_uncertainty = data_example_uncertainty,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1e3,
    standardise = TRUE,
    n_individuals = 150,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW",
    use_parallel = TRUE
  )

RRatepol::plot_roc(data_roc_with_uncertainty)

vec_method_colors <-
  c(
    "1. levels" = colours[["green"]],
    "2. bins" = colours[["purple"]],
    "3. MW" = colours[["pink"]],
    "4. MW_standardise" = colours[["coral"]],
    "5. MW_with_uncertainty" = colours[["orange"]]
  )


p_0 <-
  tibble::tibble(
    ROC = NA_real_,
    Age = NA_real_,
    ROC_dw = NA_real_,
    ROC_up = NA_real_,
    method = factor(
      c(
        "1. levels",
        "2. bins",
        "3. MW",
        "4. MW_standardise",
        "5. MW_with_uncertainty"
      ),
      levels = c(
        "1. levels",
        "2. bins",
        "3. MW",
        "4. MW_standardise",
        "5. MW_with_uncertainty"
      )
    )
  ) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      ymin = ROC_dw,
      ymax = ROC_up,
      color = method,
      fill = method
    )
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse",
    breaks = seq(0, 10e3, by = 2e3)
  ) +
  ggplot2::scale_color_manual(
    values = vec_method_colors
  ) +
  ggplot2::scale_fill_manual(
    values = vec_method_colors
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 4),
    xlim = c(10e3, -500)
  ) +
  ggplot2::labs(
    x = "Age (cal yr BP)",
    y = "Rate of Change\n(dissimilarity per 500 years)",
    color = "Legend",
    fill = "Legend"
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "right"
  )

p_0

p_2 <-
  p_0 +
  ggplot2::geom_line(
    data = data_roc_levels |>
      dplyr::mutate(
        method = "1. levels"
      ),
    linetype = "dotted"
  )

p_2

p_3 <-
  p_2 +
  ggplot2::geom_line(
    data = data_roc_bins |>
      dplyr::mutate(
        method = "2. bins"
      ),
    linetype = "dashed"
  )

p_3

p_4 <-
  p_3 +
  ggplot2::geom_line(
    data = data_roc_mw |>
      dplyr::mutate(
        method = "3. MW"
      ),
    linetype = "solid"
  )

p_4

p_5 <-
  p_4 +
  ggplot2::geom_ribbon(
    data = data_roc_mw_standardise |>
      dplyr::mutate(
        method = "4. MW_standardise"
      ),
    alpha = 0.3
  ) +
  ggplot2::geom_line(
    data = data_roc_mw_standardise |>
      dplyr::mutate(
        method = "4. MW_standardise"
      )
  )

p_5

p_6 <-
  p_5 +
  ggplot2::geom_ribbon(
    data = data_roc_with_uncertainty |>
      dplyr::mutate(
        method = "5. MW_with_uncertainty"
      ),
    alpha = 0.3
  ) +
  ggplot2::geom_line(
    data = data_roc_with_uncertainty |>
      dplyr::mutate(
        method = "5. MW_with_uncertainty"
      )
  )

p_6

p_samples <-
  data_example_uncertainty |>
  as.vector() |>
  as.data.frame() |>
  tibble::as_tibble() |>
  rlang::set_names("age") |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age
    )
  ) +
  ggplot2::geom_density(
    fill = colours[["grey"]]
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse",
    breaks = seq(0, 10e3, by = 2e3)
  ) +
  ggplot2::coord_cartesian(
    xlim = c(10e3, -500)
  ) +
  ggplot2::labs(
    x = "Age (cal yr BP)",
    y = "Density of possible ages"
  ) +
  theme_presentation() +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  ) +
  ggplot2::geom_rug(
    data = data_example_age,
    mapping = ggplot2::aes(
      x = age
    )
  )

save_plot_with_legend <- function(sel_plot_name) {
  sel_plot <-
    get(sel_plot_name, envir = parent.frame())

  p_to_plot <-
    cowplot::plot_grid(
      p_samples +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank()
        ),
      sel_plot +
        ggplot2::theme(
          legend.position = "none"
        ) +
        ggplot2::annotation_custom(
          grob = cowplot::get_legend(p_6),
          xmin = I(0.1),
          xmax = I(0.6),
          ymin = I(0.6),
          ymax = I(0.7)
        ),
      ncol = 1,
      align = "v",
      rel_heights = c(1, 2.5)
    )

  ggplot2::ggsave(
    filename = here::here(
      "Presentation",
      "Materials",
      "R_generated",
      "RRatepol",
      stringr::str_glue("RRatepol_example_{sel_plot_name}.png")
    ),
    plot = p_to_plot,
    width = image_width,
    height = image_height,
    units = image_units,
    dpi = image_dpi
  )
}

list(
  "p_0",
  "p_2",
  "p_3",
  "p_4",
  "p_5",
  "p_6"
) |>
  purrr::walk(
    save_plot_with_legend
  )
