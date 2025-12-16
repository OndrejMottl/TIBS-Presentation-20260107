plot_density_summary <- function(data_source, sel_var = "human") {
  require(colorspace)
  p_summary_0 +
    ggplot2::facet_grid(
      region ~ predictor,
      switch = "both",
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(region_label_wrap) # [config criteria]
      )
    ) +
    ggplot2::scale_x_continuous(
      trans = "reverse"
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::geom_hline(
      yintercept = seq(0, 1, 0.25),
      col = colorspace::lighten(
        col_common_gray, # [config criteria]
        amount = 0.5
      ),
      linetype = 1,
      alpha = 0.5,
      size = line_size # [config criteria]
    ) +
    ggplot2::geom_density(
      data = data_source %>%
        dplyr::filter(predictor == sel_var),
      mapping = ggplot2::aes(
        y = ratio_ind
      ),
      # width = .5,
      # .width = 0,
      trim = FALSE,
      fill = palette_predictors[sel_var], # [config criteria]
      col = NA
    )
}