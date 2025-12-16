add_vertical_line <- function(plot, data_source, sel_var = "human") {
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
        y = predictor_importance,
        yend = predictor_importance,
        color = predictor_importance
      ),
      lty = 1,
      linewidth = line_size * 10
    )
}