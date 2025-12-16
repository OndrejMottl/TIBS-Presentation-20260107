add_vertical_line <- function(plot, data_source, sel_var = "human") {
  require(colorspace)
  plot +
    ggplot2::geom_segment(
      data = data_source %>%
        dplyr::filter(
          predictor == sel_var
        ) %>%
        dplyr::filter(
          importance_type == "ratio_ind_wmean"
        ),
      mapping = ggplot2::aes(
        x = Inf,
        xend = -Inf,
        y = ratio,
        yend = ratio,
      ),
      lty = 1,
      linewidth = line_size * 10, # [config criteria]
      color = colorspace::darken(
        palette_predictors[sel_var], # [config criteria]
        amount = 0.3
      )
    )
}