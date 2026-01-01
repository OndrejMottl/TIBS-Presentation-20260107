#----------------------------------------------------------#
#
#
#              TIBS - Presentation- 20260107
#
#                Get data for Denmark
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(
  "here",
  quietly = TRUE,
  warn.conflicts = FALSE,
  character.only = TRUE,
  verbose = FALSE
)

source(
  here::here("R", "___setup_project___.R")
)

if (
  pak::pkg_status("vaultkeepr") |>
    nrow() == 0
) {
  pak::pkg_install("OndrejMottl/vaultkeepr")
}

library(vaultkeepr)

rerun <- FALSE

y_lim <- c(54.5, 58.0)
x_lim <- c(8.0, 13.0)
age_lim <- c(0, 20000)

#----------------------------------------------------------#
# 1. Get data from VegVault -----
#----------------------------------------------------------#

is_denmark_data_present <-
  file.exists(
    here::here(
      "Presentation/Materials/VegVault",
      "data_denmark.rds"
    )
  )

if (
  isFALSE(is_denmark_data_present) || isTRUE(rerun)
) {
  data_denmark <-
    vaultkeepr::open_vault(
      path = path_to_vegvault
    ) |>
    vaultkeepr::get_datasets() |>
    vaultkeepr::select_dataset_by_geo(
      lat_lim = y_lim,
      long_lim = x_lim,
      sel_dataset_type = c(
        "vegetation_plot",
        "fossil_pollen_archive",
        "gridpoints",
        "traits"
      )
    ) |>
    vaultkeepr::get_samples() |>
    vaultkeepr::select_samples_by_age(
      age_lim = age_lim,
      verbose = FALSE
    ) |>
    vaultkeepr::get_taxa() |>
    vaultkeepr::get_abiotic_data(verbose = FALSE) |>
    vaultkeepr::select_abiotic_var_by_name(sel_var_name = "bio1") |>
    vaultkeepr::get_traits(
      verbose = FALSE
    ) |>
    vaultkeepr::select_traits_by_domain_name(
      sel_domain = "Plant heigh"
    ) |>
    vaultkeepr::extract_data(
      return_raw_data = TRUE,
      verbose = FALSE
    )


  if (
    isTRUE(rerun)
  ) {
    readr::write_rds(
      data_denmark,
      here::here(
        "Presentation/Materials/VegVault",
        "data_denmark.rds"
      ),
      compress = "gz"
    )
  }
}
