plot_density_summary <- function(data_source, sel_var = "human") {
  require(colorspace)
  tibble::tibble() |>
    ggplot2::ggplot() +
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
      axis.title = ggplot2::element_text(
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
      line = ggplot2::element_line(
        linewidth = line_size, # [config criteria]
        color = colours["grey"] # [config criteria]
      ),
      strip.text = ggplot2::element_text(
        size = text_size, # [config criteria]
        color = colours["grey"] # [config criteria]
      ),
      strip.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
    ) +
    ggplot2::scale_x_continuous(
      position = "top",
      expand = c(0.05, 0.05),
      breaks = seq(
        min(sel_range),
        max(sel_range),
        by = max(sel_range) / 2
      )
    ) +
    ggplot2::labs(
      y = "",
      x = "<- Human  - |Predictor importance| -  Climate ->"
    ) +
    ggplot2::geom_vline(
      xintercept = seq(-1, 1, 0.5),
      col = colorspace::lighten(
        colours["grey"], # [config criteria]
        amount = 0.5
      ),
      linetype = 1,
      alpha = 0.5,
      linewidth = line_size # [config criteria]
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      col = colours["grey"], # [config criteria]
      linetype = 1,
      alpha = 1,
      linewidth = line_size * 5 # [config criteria]
    ) +
    ggplot2::facet_grid(
      region ~ predictor_density_label,
      switch = "both",
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(region_label_wrap) # [config criteria]
      )
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
    ggplot2::geom_vline(
      xintercept = seq(-1, 1, 0.5),
      col = colorspace::lighten(
        colours["grey"], # [config criteria]
        amount = 0.5
      ),
      linetype = 1,
      alpha = 0.5,
      size = line_size # [config criteria]
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      col = colours["black"], # [config criteria]
      linetype = 1,
      alpha = 1,
      linewidth = line_size * 5 # [config criteria]
    ) +
    ggridges::geom_density_ridges_gradient(
      data = data_source |>
        dplyr::filter(
          predictor == sel_var
        ),
      mapping = ggplot2::aes(
        x = predictor_importance,
        y = predictor_density_label,
        fill = ggplot2::after_stat(x),
      ),
      col = NA
    ) +
    ggplot2::coord_flip(
      ylim = c(1, 3),
      xlim = sel_range
    )
}
