predict_gam <- function(
  gam_model, var_x, deriv = FALSE
) {
  data_to_predict <-
    tibble::tibble(
      var = seq(
        gam_model$var.summary[[1]][1],
        gam_model$var.summary[[1]][3],
        length.out = 100
      )
    ) |>
    rlang::set_names(var_x)

  data_pred <-
    cbind(
      data_to_predict,
      data.frame(
        predict(
          gam_model,
          se.fit = TRUE,
          newdata = data_to_predict,
          type = "response"
        )
      )
    ) |>
    tibble::as_tibble() |>
    janitor::clean_names()

  crit_t <- qt(0.975, df = df.residual(gam_model))

  data_pred <-
    data_pred |>
    dplyr::mutate(
      upper = fit + (crit_t * se_fit),
      lower = fit - (crit_t * se_fit)
    )

  k_value <-
    gam_model$formula |>
    as.character() |>
    _[3] |>
    stringr::str_extract("k = [:digit:]*") |>
    stringr::str_replace("k = ", "") |>
    as.double()

  gam_model_summary <-
    gam_model |>
    summary()

  p_value <- gam_model_summary$s.table[4]

  if (
    deriv == TRUE && p_value < 0.05
  ) {
    gam_model_deriv <-
      gratia::fderiv(
        gam_model,
        newdata = data_to_predict, n = 1000
      )

    gam_model_deriv_sint <-
      with(
        data_to_predict,
        cbind(
          stats::confint(
            gam_model_deriv,
            nsim = 1000,
            type = "simultaneous",
            transform = T
          ),
          bin = bin
        )
      ) |>
      tibble::as_tibble()

    deriv <-
      gam_model_deriv_sint |>
      dplyr::mutate(
        significante_change = lower > 0 | upper < 0
      ) |>
      dplyr::select(-term) |>
      dplyr::rename(
        d_lower = lower,
        d_est = est,
        d_upper = upper
      )
  } else {
    deriv <- NA
  }

  return(
    list(
      data = data_pred,
      k = k_value,
      p = p_value,
      first_deriv = deriv
    )
  )
}
