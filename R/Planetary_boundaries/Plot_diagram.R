#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Plot planetary boundaries diagram
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Make a visualization simimar to Figure 2 in
#   "Exploring the Interface Between Planetary Boundaries and Palaeoecology"
#   https://doi.org/10.1111/gcb.70017

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)


#----------------------------------------------------------#
# 2. Generate data -----
#----------------------------------------------------------#

set.seed(900723)
data_wiggly_line <-
  generate_wiggly_line(
    y_min = 0,
    y_max = 1,
    x_max = 14e3,
    x_min = 0
  ) |>
  dplyr::mutate(
    y_flipped = y * (-1) + 1,
    x_flipped = x * (-1) + 12e3
  )

data_point_present <-
  data_wiggly_line |>
  dplyr::filter(min(abs(x_flipped)) == x_flipped)

data_point_future_bad <-
  data_wiggly_line |>
  dplyr::filter(min(x_flipped) == x_flipped)

data_point_addapt_good <-
  tibble::tibble(
    x_flipped = -2e3,
    y_flipped = 0.4
  )

data_point_return_to_safe <-
  tibble::tibble(
    x_flipped = -2e3,
    y_flipped = 0.7
  )

data_point_future <-
  dplyr::bind_rows(
    data_point_addapt_good,
    data_point_return_to_safe,
    data_point_future_bad
  ) |>
  dplyr::mutate(
    type = c(
      "Adapt",
      "Return",
      "Continue"
    ),
    type = factor(
      type,
      levels = c(
        "Return",
        "Adapt",
        "Continue"
      )
    )
  )

data_arrows <-
  tibble::tribble(
    ~x_flipped_start, ~x_flipped_end, ~y_flipped_start, ~y_flipped_end,
    0, -2e3, data_point_present$y_flipped, 0.4,
    0, -2e3, data_point_present$y_flipped, 0.7,
    0, -2e3, data_point_present$y_flipped, data_point_future_bad$y_flipped,
  )

data_polygon_posibility <-
  tibble::tribble(
    ~x_flipped, ~y_flipped,
    0, 0.5,
    -4e3, 0.2,
    -4e3, 1.3,
    0, 1
  )

data_polygon_posibility_top <-
  tibble::tribble(
    ~x_flipped, ~y_flipped,
    0, 1,
    -4e3, 1.3,
    -8e3, 1.3,
    -8e3, 1,
    -4e3, 1
  )

data_polygon_posibility_bottom <-
  tibble::tribble(
    ~x_flipped, ~y_flipped,
    0, 0.5,
    -4e3, 0.2,
    -8e3, 0.2,
    -8e3, 0.5,
    -4e3, 0.5
  )


#----------------------------------------------------------#
# 3. Make Figure -----
#----------------------------------------------------------#

p_0 <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = x_flipped,
      y = y_flipped
    )
  ) +
  ggplot2::geom_rect(
    mapping = ggplot2::aes(
      x = 0,
      y = 0,
      xmin = -Inf,
      xmax = Inf,
      ymin = 0.5,
      ymax = 1.0
    ),
    fill = colours["green"],
    col = NA,
    alpha = 0.75,
  ) +
  ggplot2::geom_polygon(
    data = data_polygon_posibility_top,
    fill = colours["green"],
    alpha = 0.5,
    col = NA
  ) +
  ggplot2::geom_polygon(
    data = data_polygon_posibility_bottom,
    fill = colours["green"],
    alpha = 0.5,
    col = NA
  ) +
  ggplot2::geom_line(
    data = data_wiggly_line,
    linewidth = 1,
    col = colours["blue"]
  ) +
  ggplot2::geom_point(
    data = data_point_present,
    size = 5,
    shape = 21,
    fill = colours["black"],
    col = colours["black"]
  ) +
  theme_presentation() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(
      fill = "transparent",
      color = NA
    ),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      size = 25,
      color = colours["black"]
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Return" = colours["purple"],
      "Adapt" = colours["pink"],
      "Continue" = colours["orange"]
    ) |>
      rlang::set_names(
        nm = c(
          "Return",
          "Adapt",
          "Continue"
        )
      )
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse",
    breaks = seq(-2e3, 12e3, by = 2e3),
    labels = c("future", "present", seq(2e3, 12e3, by = 2e3))
  ) +
  ggplot2::scale_y_continuous() +
  ggplot2::coord_cartesian(
    ylim = c(0, 1.5),
    xlim = c(-4e3, 12e3)
  ) +
  ggplot2::labs(
    x = "Time (years BP)"
  ) +
  ggview::canvas(
    width = 16,
    height = 8,
    units = "cm",
    dpi = 300
  )

p_1 <-
  p_0 +
  # arrow pointing to present point
  ggplot2::geom_curve(
    mapping = ggplot2::aes(
      xend = 0,
      yend = 0.4,
      x = 2e3,
      y = 1.2
    ),
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.2, "cm"),
      type = "closed"
    ),
    linewidth = 0.5,
    color = colours["grey"]
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 4.8e3,
    y = 1.2,
    label = "We are here",
    size = 12,
    color = colours["black"],
    hjust = 0
  )

p_2 <-
  p_0 +
  ggplot2::geom_point(
    data = data_point_future,
    mapping = ggplot2::aes(
      fill = type
    ),
    size = 5,
    shape = 21,
    col = colours["black"]
  ) +
  ggplot2::geom_segment(
    data = data_arrows,
    mapping = ggplot2::aes(
      x = x_flipped_start,
      y = y_flipped_start,
      xend = x_flipped_end,
      yend = y_flipped_end
    ),
    arrow = ggplot2::arrow(
      length = ggplot2::unit(0.2, "cm"),
      type = "closed"
    ),
    linewidth = 0.5,
    color = colours["grey"],
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    data = data_point_present,
    size = 5,
    shape = 21,
    fill = colours["black"],
    col = colours["black"]
  )

p_2_legend_inset <-
  p_2 +
  ggplot2::theme(
    legend.position = "none"
  ) +
  ggplot2::annotation_custom(
    grob = cowplot::get_legend(p_2),
    xmin = 6e3,
    xmax = 12e3,
    ymin = 0,
    ymax = 0.5
  )

p_2_legend_anotate <-
  p_2 +
  ggplot2::theme(
    legend.position = "none"
  ) +
  # return
  ggplot2::geom_label(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.7,
      label = "Return"
    ),
    size = 10,
    color = colours["black"],
    fill = colours["purple"],
    alpha = 0.8
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.7,
      label = "Return"
    ),
    size = 10,
    color = colours["black"]
  ) +
  # adapt
  ggplot2::geom_label(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.42,
      label = "Adapt"
    ),
    size = 10,
    color = colours["black"],
    fill = colours["pink"],
    alpha = 0.8
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.42,
      label = "Adapt"
    ),
    size = 10,
    color = colours["black"]
  ) +
  # continue
  ggplot2::geom_label(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.15,
      label = "Continue"
    ),
    size = 10,
    color = colours["black"],
    fill = colours["orange"],
    alpha = 0.8
  ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(
      x = -3.5e3,
      y = 0.15,
      label = "Continue"
    ),
    size = 10,
    color = colours["black"]
  )

#----------------------------------------------------------#
# 4. Save Figure -----
#----------------------------------------------------------#

ggview::save_ggplot(
  plot = p_1,
  file = here::here(
    "Presentation/Materials/R_generated/Planetary_boundaries",
    "planetary_boundaries_diagram_1.png"
  )
)

ggview::save_ggplot(
  plot = p_2_legend_anotate,
  file = here::here(
    "Presentation/Materials/R_generated/Planetary_boundaries",
    "planetary_boundaries_diagram_2.png"
  )
)
