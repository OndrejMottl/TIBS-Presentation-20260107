#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#             Get the data for all Neotoma sequences
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Obtain information about all Neotoma sequences and make plots


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

if (
  pak::pkg_status("RUtilpol") |>
    nrow() == 0
) {
  pak::pkg_install("HOPE-UIB-BIO/R-Utilpol-package")
}
if (
  pak::pkg_status("RFossilpol") |>
    nrow() == 0
) {
  pak::pkg_install("HOPE-UIB-BIO/R-Fossilpol-package")
}

library(httr)
library(RFossilpol)


#----------------------------------------------------------#
# 2. Get Neotoma data -----
#----------------------------------------------------------#


# request all data of selected type from Neotoma 2.0
pollends <-
  httr::GET(
    "https://api.neotomadb.org/v2.0/data/datasets/",
    query = list(
      datasettype = "pollen", # [config_criteria]
      limit = 99999,
      offset = 0
    )
  )

# Extract all data
datasets <- httr::content(pollends)$data

# Create a tibble with dataset_id, and coordinates
dataset_geo <-
  RFossilpol:::proc_neo_get_coord(datasets)

datasets_age <-
  datasets |>
  purrr::map(
    "site"
  ) |>
  purrr::map(
    "datasets"
  ) |>
  purrr::map_dfr(
    .f = ~ {
      sel_datase_id <-
        .x[[1]]$datasetid

      dataset_info <-
        .x[[1]]$agerange

      purrr::map_dfr(
        dataset_info,
        .f = ~ tibble::tibble(
          dsid = sel_datase_id,
          ageold = .x$ageold,
          ageyoung = .x$ageyoung
        )
      )
    }
  ) |>
  dplyr::distinct(dsid, .keep_all = TRUE)

datasets_full <-
  dplyr::inner_join(
    dataset_geo,
    datasets_age,
    by = "dsid"
  ) |>
  dplyr::filter(
    lat <= 90 & lat >= -90
  ) |>
  dplyr::filter(
    long <= 180 & long >= -180
  ) |>
  dplyr::filter(ageold < 50e3) |>
  dplyr::filter(ageyoung > -75) |>
  dplyr::mutate(
    agemean = (ageold + ageyoung) / 2,
    age_dif = ageold - ageyoung
  ) |>
  dplyr::filter(age_dif >= 100) |>
  dplyr::arrange(agemean) |>
  dplyr::mutate(
    row_n = dplyr::row_number()
  )


#----------------------------------------------------------#
# 3. geographical distribution -----
#----------------------------------------------------------#

lat_bin_seq <-
  with(datasets_full, seq(from = min(lat), to = max(lat), by = 3))

long_bin_seq <-
  with(datasets_full, seq(from = min(long), to = max(long), by = 3))

lat_seq <-
  seq(-90, 90, 30)

long_seq <-
  seq(-180, 180, 30)

n_seq <-
  c(1, 5, 10, 20, 50, 150, 250)

seq_labels <-
  paste(n_seq[-length(n_seq)], n_seq[-1], sep = "-")

bin_palette_geo <-
  colorRampPalette(c(colours[["purple"]], colours[["orange"]]))(length(n_seq) - 1) |>
  purrr::set_names(as.character(n_seq[-length(n_seq)]))

dat_geo <-
  datasets_full |>
  dplyr::mutate(
    lat_bin = lat_bin_seq[findInterval(lat, lat_bin_seq, all.inside = TRUE)],
    long_bin = long_bin_seq[findInterval(long, long_bin_seq, all.inside = TRUE)]
  ) |>
  dplyr::group_by(lat_bin, long_bin) |>
  dplyr::summarise(
    .groups = "drop",
    N = dplyr::n()
  ) |>
  dplyr::mutate(
    N_bin = n_seq[findInterval(N, n_seq, all.inside = TRUE)]
  )


#----------------------------------------------------------#
# 4. Plot -----
#----------------------------------------------------------#

p1 <-
  dat_geo |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = long_bin,
      y = lat_bin
    )
  ) +
  ggplot2::borders(
    fill = colours[["grey"]],
    colour = colours[["grey"]],
    linewidth = 0.01
  ) +
  ggplot2::geom_tile(
    ggplot2::aes(
      fill = as.factor(N_bin)
    ),
    colour = NA,
    alpha = 1
  ) +
  ggplot2::coord_quickmap(
    xlim = range(long_bin_seq),
    ylim = range(lat_bin_seq)
  ) +
  ggplot2::scale_y_continuous(breaks = lat_seq) +
  ggplot2::scale_x_continuous(breaks = long_seq) +
  ggplot2::scale_fill_manual(
    values = bin_palette_geo
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_void() +
  ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    legend.position = "note",
    text = ggplot2::element_blank(),
    line = ggplot2::element_line(linewidth = line_size),
    plot.margin = ggplot2::margin(0, 0, 0, 0)
  )


ggplot2::ggsave(
  plot = p1,
  filename = stringr::str_glue(
    here::here(
      "Presentation/Materials/R_generated/Fossil_pollen/"
    ),
    "/Neotoma_spatial_data_full-", as.character(Sys.Date()), ".png"
  ),
  width = 800,
  height = 320,
  units = "px",
  dpi = 600
)
