plot_summary_regions_quatiles <- function(
  data_source_quantiles,
  data_source_climatezone,
  sel_var = "human",
  sel_var_label = "Human importance"
) {
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
    purrr::map(
      .x = c("95", "75", "50"),
      .f = ~ ggforce::geom_link(
        data = data_source_quantiles %>%
          dplyr::filter(quantile_degree == .x) %>%
          dplyr::filter(
            predictor == sel_var
          ),
        mapping = ggplot2::aes(
          x = predictor,
          xend = predictor,
          y = predictor_importance_upr,
          yend = predictor_importance_lwr,
          col = ggplot2::after_stat(y)
        ),
        n = 100,
        alpha = 0.8,
        linewidth = (0.1 + (1 - (as.numeric(.x) / 100))) * 5
      )
    ) +
    ggplot2::geom_point(
      data = data_source_climatezone %>%
        dplyr::filter(
          predictor == sel_var
        ) %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      mapping = ggplot2::aes(
        x = predictor,
        y = predictor_importance,
        fill = predictor_importance
      ),
      shape = 21,
      col = colours["grey"], # [config criteria]
      size = point_size * 3, # [config criteria]
    )
}
