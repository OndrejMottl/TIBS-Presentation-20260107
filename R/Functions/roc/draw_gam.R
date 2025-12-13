# This is a updated function of 'draw.gam.custom' from:
# https://github.dev/HOPE-UIB-BIO/Global_RoC/blob/v-1-1-1/R/functions/draw.region.R

draw_gam <- function(data,
                     pred_gam_up,
                     sel_region,
                     siluete = FALSE,
                     plot_maxima = FALSE,
                     palette_x = NULL,
                     age_treshold = 18e3,
                     deriv = FALSE,
                     y_cut = 1.8,
                     y_start = 0,
                     axis_ratio = 2) {
  region_coord <-
    tibble::tibble(
      region = c(
        "North America",
        "Latin America",
        "Europe",
        "Africa",
        "Asia",
        "Oceania"
      ),
      long_min = c(-165, -100, -8, -18, 40, 95),
      long_max = c(-55, -37, 40, 50, 182, 175),
      lat_min = c(25, -54, 38, -32, 11, -45),
      lat_max = c(68, 25, 70, 35, 75, 10)
    )


  p0 <-
    ggplot2::ggplot() +
    ggplot2::scale_x_continuous(
      trans = "reverse",
      breaks = seq(0, age_treshold, 2e3),
      labels = seq(0, age_treshold / 1e3, 2)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(age_treshold, 0)
    ) +
    theme_presentation() +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_rect(
        fill = NA,
        colour = "gray30",
        linewidth = 0.1
      )
    ) +
    ggplot2::labs(x = "age (cal yr BP)")

  if (
    all(plot_maxima != FALSE)
  ) {
    p0 <-
      p0 +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = plot_maxima[1],
          xend = plot_maxima[1],
          y = plot_maxima[2] + 0.12,
          yend = plot_maxima[2] + 0.05
        ),
        col = "gray30",
        size = 0.2,
        arrow = ggplot2::arrow(length = grid::unit(0.05, "npc"))
      ) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = plot_maxima[3],
          xend = plot_maxima[3],
          y = plot_maxima[4] + 0.12,
          yend = plot_maxima[4] + 0.05
        ),
        col = "gray30",
        size = 0.2,
        arrow = ggplot2::arrow(length = grid::unit(0.05, "npc"))
      )
  }


  if (
    isTRUE(deriv)
  ) {
    # ROC

    p3 <-
      p0 +
      ggplot2::geom_vline(
        xintercept = seq(0, 18e3, 2e3),
        color = "gray90",
        size = 0.1
      ) +
      ggplot2::geom_ribbon(
        data = pred_gam_up$data,
        mapping = ggplot2::aes(
          x = bin,
          y = fit,
          ymin = lower,
          ymax = upper
        ),
        fill = "gray80",
        linewidth = 0.1,
        alpha = ifelse(pred_gam_up$p < 0.05, 0.5, 0)
      ) +
      ggplot2::geom_line(
        data = pred_gam_up$data,
        mapping = ggplot2::aes(x = bin, y = fit),
        lty = ifelse(pred_gam_up$p < 0.05, 1, 2),
        linewidth = 0.1
      ) +
      ggplot2::scale_fill_manual(values = palette_x) +
      ggplot2::scale_color_manual(values = palette_x) +
      ggplot2::labs(y = "Rate of vegetation change") +
      ggplot2::scale_y_continuous(
        limits = c(y_start, y_cut),
        breaks = seq(0, y_cut, 0.1)
      )


    if (
      all(is.na(pred_gam_up$first_deriv) == FALSE)
    ) {
      rect_df_upq <-
        dplyr::left_join(
          pred_gam_up$first_deriv,
          pred_gam_up$data,
          by = "bin"
        ) |>
        dplyr::filter(significante_change == TRUE)

      if (
        nrow(rect_df_upq) > 0
      ) {
        p3 <-
          p3 +
          ggplot2::geom_point(
            data = rect_df_upq,
            mapping = ggplot2::aes(x = bin, y = fit),
            size = 0.1,
            color = "gray30",
            shape = 8
          )
      }
    }

    p3 <-
      p3 +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          x = bin,
          y = roc_upq,
          color = region
        ),
        shape = 15,
        size = 1,
        alpha = 1 / 2
      )
  } else {
    p3 <-
      p0 +
      ggplot2::geom_ribbon(
        data = pred_gam_up$data,
        mapping = ggplot2::aes(
          x = bin,
          y = fit,
          ymin = lower,
          ymax = upper
        ),
        fill = "gray80",
        alpha = ifelse(pred_gam_up$p < 0.05, 0.5, 0)
      ) +
      ggplot2::geom_point(
        data = data,
        mapping = ggplot2::aes(
          x = bin,
          y = roc_upq, color = region
        ),
        shape = 15
      ) +
      ggplot2::geom_line(
        data = pred_gam_up$data,
        mapping = ggplot2::aes(
          x = bin,
          y = fit
        ),
        lty = ifelse(pred_gam_up$p < 0.05, 1, 2)
      ) +
      ggplot2::scale_color_manual(values = palette_x) +
      ggplot2::labs(y = "Rate of vegetation change") +
      ggplot2::scale_y_continuous(
        limits = c(y_start, y_cut),
        breaks = seq(0, y_cut, 0.1),
        sec.axis = ggplot2::sec_axis(
          name = ,
          ~ (. - y_start) / axis_ratio,
          breaks = seq(0, y_cut, 0.1)
        )
      )
  }

  if (
    siluete == TRUE
  ) {
    region_coord_w <-
      region_coord |>
      dplyr::filter(region == sel_region)

    p2 <-
      ggplot2::ggplot() +
      ggplot2::borders(
        fill = palette_x[names(palette_x) == sel_region],
        colour = NA,
        alpha = .3
      ) +
      ggplot2::coord_quickmap(
        xlim = c(
          region_coord_w$long_min,
          region_coord_w$long_max
        ),
        ylim = c(
          region_coord_w$lat_min,
          region_coord_w$lat_max
        )
      ) +
      ggplot2::theme_void()

    p2_g <- ggplot2::ggplotGrob(p2)

    p3_a <-
      p3 +
      ggpubr::rremove("xylab") +
      ggplot2::annotation_custom(
        grob = p2_g,
        xmin = -1.5e3,
        xmax = -5e3,
        ymin = 0.4,
        ymax = y_cut
      )
  } else {
    p3_a <-
      p3 +
      ggpubr::rremove("xylab")
  }

  return(p3_a)
}
