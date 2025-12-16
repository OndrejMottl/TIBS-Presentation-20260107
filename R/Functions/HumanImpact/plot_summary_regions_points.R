plot_summary_regions_points <- function(data_source, sel_var = "human", sel_var_label = "Human importance") {
  set.seed(1234)

  p_summary_0 +
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
    ggplot2::facet_grid(
      region ~ climatezone_label,
      switch = "both",
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(region_label_wrap) # [config criteria]
      )
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_jitter(
      data = data_source %>%
        dplyr::filter(
          predictor == sel_var
        ),
      mapping = ggplot2::aes(
        x = predictor,
        y = predictor_importance,
        col = predictor_importance
      ),
      position = ggplot2::position_jitter(height = 0),
      alpha = 0.8
    )
}
