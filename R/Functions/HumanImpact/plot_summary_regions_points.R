plot_summary_regions_points <- function(data_source, sel_var = "human", sel_var_label = "Human importance") {
  set.seed(1234)

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
    ggplot2::geom_jitter(
      data = data_source %>%
        dplyr::filter(
          predictor == sel_var
        ),
      mapping = ggplot2::aes(
        x = predictor,
        y = ratio_ind,
        col = climatezone_label
      ),
      alpha = 0.8
    )
}
