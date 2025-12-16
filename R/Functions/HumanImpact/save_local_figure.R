save_local_figure <- function(
  plot, filename,
  sel_width = 16, # [config]
  sel_height = 9 # [config]
) {
  ggplot2::ggsave(
    filename = here::here("Presentation/Materials/R_generated/HumanImpact/", filename),
    plot = plot,
    width = sel_width,
    height = sel_height,
    dpi = 300,
    units = "cm",
    bg = colours["white"] # [config]
  )
}
