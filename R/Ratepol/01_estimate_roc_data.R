#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#                   Esimate ROC data
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Estimate various variants of RRatepol estimation using example data


#----------------------------------------------------------#
# 1. Set up -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

if (
  pak::pkg_status("RRatepol") |>
    nrow() == 0
) {
  pak::pkg_install("HOPE-UIB-BIO/R-Ratepol-package")
}

library(RRatepol)

#----------------------------------------------------------#
# 2. Get example data -----
#----------------------------------------------------------#

data_example_pollen <-
  RRatepol::example_data$pollen_data[[4]] |>
  janitor::clean_names()

data_example_pollen_sum <-
  data_example_pollen |>
  tibble::column_to_rownames(
    var = "sample_id"
  ) |>
  rowSums() |>
  tibble::enframe(
    name = "sample_id",
    value = "pollen_sum"
  )

min_pollen_sum <-
  data_example_pollen_sum |>
  dplyr::pull(
    pollen_sum
  ) |>
  min()

set.seed(19900723)
data_pollen_subsampled <-
  data_example_pollen |>
  tibble::column_to_rownames(
    var = "sample_id"
  ) |>
  vegan::rrarefy(
    sample = max(c(100, min_pollen_sum))
  ) |>
  as.data.frame() |>
  tibble::rownames_to_column(
    var = "sample_id"
  ) |>
  tibble::as_tibble()


data_example_age <-
  RRatepol::example_data$sample_age[[4]]

data_example_uncertainty <-
  RRatepol::example_data$age_uncertainty[[4]]


#----------------------------------------------------------#
# 3. Rate of change estimation -----
#----------------------------------------------------------#

set.seed(19900723)
data_roc_levels <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "levels",
    time_standardisation = 500
  )

set.seed(19900723)
data_roc_bins <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    rand = 1,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "bins"
  )

set.seed(19900723)
data_roc_mw <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW"
  )

set.seed(19900723)
data_roc_mw_standardise <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1e3,
    standardise = TRUE,
    n_individuals = 150,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW",
    use_parallel = TRUE
  )

set.seed(19900723)
data_roc_with_uncertainty <-
  RRatepol::estimate_roc(
    data_source_community = data_pollen_subsampled,
    data_source_age = data_example_age,
    age_uncertainty = data_example_uncertainty,
    bin_size = 500,
    time_standardisation = 500,
    number_of_shifts = 5,
    rand = 1e3,
    standardise = TRUE,
    n_individuals = 150,
    smooth_method = "shep",
    dissimilarity_coefficient = "chisq",
    working_units = "MW",
    use_parallel = TRUE
  )


#----------------------------------------------------------#
# 4. Save -----
#----------------------------------------------------------#

list(
  "levels" = data_roc_levels,
  "bins" = data_roc_bins,
  "mw" = data_roc_mw,
  "mw_standardise" = data_roc_mw_standardise,
  "mw_with_age_uncertainty" = data_roc_with_uncertainty
) |>
  readr::write_rds(
    file = here::here(
      "Presentation/Materials/R_generated/RRatepol/RRatepol_results.rds"
    )
  )
