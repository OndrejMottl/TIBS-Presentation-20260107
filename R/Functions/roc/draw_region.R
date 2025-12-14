# Function adjusted from:
# https://github.dev/HOPE-UIB-BIO/Global_RoC/blob/v-1-1-1/R/functions/draw.region.R

draw_region <- function(sel_region,
                        data_binned,
                        blue_samples = FALSE,
                        roc_max_value = 1.3,
                        axis_ratio = 2) {
  data_region_selected <-
    data_binned |>
    dplyr::filter(region == sel_region) |>
    dplyr::ungroup()

  # 95% quantile ROC GAM
  mod_roc <-
    fit_gam_model(
      var_y = "roc_upq",
      var_x = "bin",
      family = "tw()",
      data = data_region_selected,
      weights = "n_roc"
    )

  data_pred <-
    predict_gam(
      gam_model = mod_roc,
      var_x = "bin",
      deriv = TRUE
    )

  palette_regions <-
    colorRampPalette(colours[-c(1:3)])(6) |>
    rlang::set_names(
      nm = c(
        "Africa",
        "Asia",
        "Europe",
        "North America",
        "Oceania",
        "South America"
      )
    )

  p_fin <-
    draw_gam(
      data = data_region_selected,
      pred_gam_up = data_pred,
      sel_region = sel_region,
      siluete = TRUE,
      palette_x = palette_regions,
      deriv = TRUE,
      y_cut = roc_max_value,
      axis_ratio = axis_ratio
    )

  return(
    list(
      plot = p_fin,
      mod = mod_roc,
      data_pred = data_pred
    )
  )
}
