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

# Plot result of RRatepol estimation


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
# 2. Load data -----
#----------------------------------------------------------#

list_res <-
  readr::read_rds(
    here::here(
      "Presentation/Materials/R_generated/RRatepol/RRatepol_results.rds"
    )
  )

data_example_age <-
  RRatepol::example_data$sample_age[[4]] |>
  tibble::as_tibble()

#----------------------------------------------------------#
# 3. Visualisation -----
#----------------------------------------------------------#

vec_method_colors <-
  c(
    "1. levels" = colours[["green"]],
    "2. bins" = colours[["purple"]],
    "3. Moving Window (MW)" = colours[["pink"]],
    "4. MW + uncertainty" = colours[["orange"]]
  )

vec_method_colors_simple <-
  c(
    "Classical" = colours[["green"]],
    "RRatepol" = colours[["orange"]]
  )


p_0 <-
  tibble::tibble(
    ROC = NA_real_,
    Age = NA_real_,
    ROC_dw = NA_real_,
    ROC_up = NA_real_,
    method = factor(
      names(vec_method_colors),
      levels = names(vec_method_colors)
    )
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_rug(
    data = data_example_age,
    mapping = ggplot2::aes(
      x = age
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
    ylim = c(0, 3),
    xlim = c(10e3, -500)
  ) +
  ggplot2::guides(
    col = ggplot2::guide_legend(
      ncol = 1,
      override.aes = list(
        linewidth = 1
      )
    ),
    fill = "none"
  ) +
  ggplot2::labs(
    x = "Age (cal yr BP)",
    y = "Rate of Change\n(dissimilarity per 500 years)",
    color = NA,
    fill = NA
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    legend.margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0),
    legend.background = ggplot2::element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.text = ggplot2::element_text(
      family = presentation_base_font,
      colour = colours["black"],
      size = ggplot2::rel(2)
    ),
    legend.box.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    margins = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.spacing = ggplot2::unit(0.001, "cm"),
    panel.grid.minor = ggplot2::element_blank(),
    legend.key.spacing = ggplot2::unit(0.1, "cm"),
    legend.box.spacing = ggplot2::unit(0.01, "cm")
  ) +
  ggview::canvas(
    width = 12.8,
    height = 8,
    dpi = 300,
    units = "cm"
  )

#----------------------------------------------------#
## 3.1. Detailed -----
#----------------------------------------------------#

p_levels <-
  p_0 +
  ggplot2::geom_line(
    data = list_res$levels |>
      dplyr::mutate(
        method = "1. levels"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linetype = "dotted"
  )

p_levels_legend_inset <-
  p_levels +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_levels),
    xmin = I(0.2),
    xmax = I(0.3),
    ymin = I(0.6),
    ymax = I(0.7)
  )


p_bins <-
  p_levels +
  ggplot2::geom_line(
    data = list_res$bins |>
      dplyr::mutate(
        method = "2. bins"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linetype = "dashed"
  )

p_bins_legend_inset <-
  p_bins +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_bins),
    xmin = I(0.2),
    xmax = I(0.3),
    ymin = I(0.6),
    ymax = I(0.7)
  )


p_mw <-
  p_bins +
  ggplot2::geom_line(
    data = list_res$mw |>
      dplyr::mutate(
        method = "3. Moving Window (MW)"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linetype = "solid"
  )

p_mw_legend_inset <-
  p_mw +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_mw),
    xmin = I(0.3),
    xmax = I(0.5),
    ymin = I(0.6),
    ymax = I(0.7)
  )

p_uncertaint <-
  p_mw +
  ggplot2::geom_ribbon(
    data = list_res$mw_with_age_uncertainty |>
      dplyr::mutate(
        method = "4. MW + uncertainty"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      ymin = ROC_dw,
      ymax = ROC_up,
      fill = method
    ),
    alpha = 0.3,
    color = NA
  ) +
  ggplot2::geom_line(
    data = list_res$mw_with_age_uncertainty |>
      dplyr::mutate(
        method = "4. MW + uncertainty"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linetype = "solid"
  )

p_uncertaint_legend_inset <-
  p_uncertaint +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_uncertaint),
    xmin = I(0.3),
    xmax = I(0.5),
    ymin = I(0.6),
    ymax = I(0.7)
  )

#----------------------------------------------------#
## 3.2. Simple -----
#----------------------------------------------------#


p_clasical <-
  p_0 +
  ggplot2::scale_color_manual(
    values = vec_method_colors_simple
  ) +
  ggplot2::scale_fill_manual(
    values = vec_method_colors_simple
  ) +
  ggplot2::geom_line(
    data = list_res$levels |>
      dplyr::mutate(
        method = "Classical"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linewidth = 0.3,
    linetype = "dashed"
  )


p_clasical_legend_inset <-
  p_clasical +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_clasical),
    xmin = I(0.2),
    xmax = I(0.65),
    ymin = I(0.6),
    ymax = I(0.7)
  )

p_clasical_legend_inset

p_rratepol <-
  p_clasical +
  ggplot2::geom_ribbon(
    data = list_res$mw_with_age_uncertainty |>
      dplyr::mutate(
        method = "RRatepol"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      ymin = ROC_dw,
      ymax = ROC_up,
      fill = method
    ),
    alpha = 0.3,
    color = NA
  ) +
  ggplot2::geom_line(
    data = list_res$mw_with_age_uncertainty |>
      dplyr::mutate(
        method = "RRatepol"
      ),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC,
      col = method
    ),
    linetype = "solid",
    linewidth = 1
  )

p_rratepol_highlight <-
  p_rratepol +
  ggplot2::geom_line(
    data = list_res$mw_with_age_uncertainty |>
      RRatepol::detect_peak_points(
        sel_method = "trend_non_linear"
      ) |>
      dplyr::filter(Peak),
    mapping = ggplot2::aes(
      x = Age,
      y = ROC
    ),
    linetype = "solid",
    linewidth = 4,
    col = colours[["purple"]]
  )

p_rratepol_legend_inset <-
  p_rratepol +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_rratepol),
    xmin = I(0.2),
    xmax = I(0.65),
    ymin = I(0.6),
    ymax = I(0.7)
  )


p_rratepol_highlight_legend_inset <-
  p_rratepol_highlight +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_rratepol),
    xmin = I(0.2),
    xmax = I(0.65),
    ymin = I(0.6),
    ymax = I(0.7)
  )


#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#


ggview::save_ggplot(
  plot = p_0,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "empty_plot.png"
  )
)

ggview::save_ggplot(
  plot = p_levels_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "levels.png"
  )
)

ggview::save_ggplot(
  plot = p_bins_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "bins.png"
  )
)

ggview::save_ggplot(
  plot = p_mw_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "moving_window.png"
  )
)

ggview::save_ggplot(
  plot = p_uncertaint_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "moving_window_with_uncertainty.png"
  )
)

ggview::save_ggplot(
  plot = p_clasical_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "simple_classical.png"
  )
)

ggview::save_ggplot(
  plot = p_rratepol_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "simple_rratepol.png"
  )
)


ggview::save_ggplot(
  plot = p_rratepol_highlight_legend_inset,
  file = here::here(
    "Presentation",
    "Materials",
    "R_generated",
    "RRatepol",
    "simple_rratepol_highlight.png"
  )
)
