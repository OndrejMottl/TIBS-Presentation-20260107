# This is a updated version of 'select.model' avauilable at:
# https://github.com/HOPE-UIB-BIO/Global_RoC/blob/v-1-1-1/R/functions/select.model.R
fit_gam_model <- function(
  var_y,
  var_x,
  family,
  data,
  weights = NA_character_
) {
  require(mgcv)
  require(stringr)
  require(dplyr)

  n_row_data <-
    nrow(data)

  gam_w_success <- NULL

  for (i in seq(
    from = 10,
    to = min(c(n_row_data - 1, 100)),
    by = 5
  )) {
    print(paste("trying k=", i))

    formula_w <-
      stringr::str_glue("{var_y} ~ s({var_x}, k={i}, bs='tp')")

    if (
      isFALSE(is.na(weights))
    ) {
      data <-
        data |>
        dplyr::mutate(
          weight = with(data, get(weights)),
          weight = weight / mean(weight, na.rm = TRUE) + 1
        )
    } else {
      data <-
        data |>
        dplyr::mutate(
          weight = 1
        )
    }

    try(
      suppressWarnings(
        gam_w <-
          mgcv::gam(
            formula = as.formula(formula_w),
            data = data,
            family = noquote(family),
            weights = weight,
            method = "REML",
            niterPQL = 50
          )
      ),
      silent = TRUE
    )

    if (
      !exists("gam_w")
    ) {
      break
    }

    # save the result from the k.check fc
    check_k <- function(mod,
                        k_sample = 10e3,
                        k_rep = 1e3) {
      mgcv:::k.check(mod, subsample = k_sample, n.rep = k_rep)
    }

    suppressWarnings(
      mod_basis <- check_k(gam_w)
    )

    gam_w_success <- gam_w
    rm(gam_w)

    res_basis <- mod_basis[4]

    if (
      is.na(res_basis) == FALSE
    ) {
      if (
        res_basis > 0.05
      ) {
        break
      }
    }
  }

  return(gam_w_success)
}
