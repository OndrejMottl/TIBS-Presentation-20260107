add_predictor_names_as_factors <- function(data_source) {
  data_source %>%
    dplyr::mutate(
      predictor = dplyr::case_when(
        .default = predictor,
        predictor == "Density diversity" ~ "Change points - Diversity",
        predictor == "Density turnover" ~ "Change points - Turnover"
      ),
      predictor = factor(
        predictor,
        levels = c(
          "N0", "N1", "N2",
          "N2 divided by N1", "N1 divided by N0",
          "DCCA axis 1", "ROC",
          "Change points - Diversity", "Change points - Turnover",
          "MAT", "MTCO",
          "MAPS", "MAPW",
          "SPD"
        )
      )
    ) %>%
    return()
}