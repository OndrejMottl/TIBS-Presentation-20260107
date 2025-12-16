plot_summary_regions_quatiles <- function(
    data_source_quantiles,
    data_source_climatezone,
    sel_var = "human",
    sel_var_label = "Human importance") {
  p_summary_0 +
    ggplot2::scale_fill_manual(
      values = palette_ecozones_labels
    ) +
    ggplot2::scale_color_manual(
      values = palette_ecozones_labels
    ) +
    ggplot2::labs(
      x = "",
      y = sel_var_label
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
      axis.text = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    purrr::map(
      .x = c("95", "75", "50"),
      .f = ~ ggplot2::geom_segment(
        data = data_source_quantiles %>%
          dplyr::filter(quantile_degree == .x) %>%
          dplyr::filter(
            predictor == sel_var
          ),
        mapping = ggplot2::aes(
          x = predictor,
          xend = predictor,
          y = upr,
          yend = lwr,
          col = climatezone_label
        ),
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
        y = ratio,
        fill = climatezone_label
      ),
      shape = 21,
      col = col_common_gray, # [config criteria]
      size = point_size * 3, # [config criteria]
    )
}
