add_horizontal_line <- function(plot, data_source, sel_var = "human") {
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
        y = Inf,
        yend = -Inf,
        x = predictor_importance,
        xend = predictor_importance,
        color = predictor_importance
      ),
      lty = 1,
      linewidth = line_size * 10
    )
}