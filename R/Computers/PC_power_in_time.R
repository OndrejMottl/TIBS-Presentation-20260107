#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Visualisation PC power in time
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Plot a timeline of computer power change

#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)



#----------------------------------------------------------#
# 2. Get  data -----
#----------------------------------------------------------#

data_pc <-
  readr::read_csv(
    here::here(
      "Presentation/Materials/Computers/pc_power.csv"
    )
  ) |>
  janitor::clean_names()


#----------------------------------------------------------#
# 3. Make a plot -----
#----------------------------------------------------------#

library(ggview)

p_0 <-
  data_pc |>
  dplyr::mutate(
    mips_avg = (low_mips + high_mips) / 2
  ) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = year
    )
  ) +
  theme_presentation() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    x = "Year",
    y = "Computing Power\n(Million Instructions Per Second)"
  ) +
  ggplot2::scale_color_gradient(
    low = colours["purple"],
    high = colours["orange"],
    na.value = colours["white"],
    limits = c(0, max(data_pc$high_mips))
  ) +
  ggplot2::scale_y_continuous(
    transform = "log",
    breaks = c(1, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8),
    labels = scales::comma_format()
  ) +
  ggplot2::geom_smooth(
    mapping = ggplot2::aes(
      y = mips_avg
    ),
    linewidth = 0.1,
    color = colours["green"],
    method = "loess",
    se = FALSE,
    formula = y ~ x
  ) +
  ggforce::geom_link(
    mapping = ggplot2::aes(
      xend = year,
      y = low_mips,
      yend = high_mips,
      col = ggplot2::after_stat(y)
    ),
    n = 100,
    alpha = 0.8,
    linewidth = 2
  ) +
  ggplot2::geom_label(
    mapping = ggplot2::aes(
      label = cpu,
      y = mips_avg
    ),
    alpha = 0.8,
    size = 4
  ) +
  ggview::canvas(
    width = 16,
    height = 9,
    dpi = 300,
    units = "cm",
    scale = 1
  )

#----------------------------------------------------------#
# 4. Save the plot -----
#----------------------------------------------------------#

ggview::save_ggplot(
  plot = p_0,
  file = here::here(
    "Presentation/Materials/R_generated/Computers/PC_power_in_time.png"
  )
)
