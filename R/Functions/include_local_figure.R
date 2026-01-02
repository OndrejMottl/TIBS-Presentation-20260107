include_local_figure <- function(data_source) {
  knitr::include_graphics(
    path = here::here(
      "Presentation",
      "Materials",
      data_source
    ),
    error = TRUE
  )
}
