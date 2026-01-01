#----------------------------------------------------------#
#
#
#               TIBS - Presentation- 20260107
#
#                 Download all elevation data
#
#
#                       O. Mottl
#                         2025
#----------------------------------------------------------#

# Download the altitude data using `raster::getData`

#----------------------------------------------------------#
# 1. Set up -----
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

download_de_novo <- FALSE


#----------------------------------------------------------#
# 2. Download data  -----
#----------------------------------------------------------#

# get the name codes of countries
View(geodata::country_codes())

# make a list of countires to download
countires_vec <-
  c(
    "DNK", # Denmark
    "DEU", # Germany
    "POL", # Poland
    "SWE", # Sweden
    "NOR" # Norway
  )


# get raster data for all countries and save them
# !!! This takes time once !!!
purrr::walk(
  .progress = "getting raster data",
  .x = countires_vec,
  .f = ~ {
    sel_county <- .x

    # skip if alredy downloaded
    if (
      isFALSE(download_de_novo) &&
        list.files(
          here::here(
            "Presentation/Materials/VegVault/Terrain/Terrain_rasters"
          )
        ) |>
          stringr::str_detect(sel_county) |>
          any()
    ) {
      return()
    }

    # get the altitude data
    ras <-
      geodata::elevation_30s(
        country = sel_county,
        path = here::here("Presentation/Materials/VegVault/Terrain/Terrain_rasters/"),
        mask = FALSE
      )

    # some countries has more than have more than 1 raster files.
    #   Loop through the list and save each individualy
    if (is.list(ras)) {
      purrr::walk(
        .x = ras,
        .f = ~ try(
          terra::writeRaster(
            .x,
            filename = here::here(
              paste0(
                "Presentation/Materials/VegVault/Terrain/Terrain_rasters/raster_",
                names(.x) |>
                  stringr::str_extract(paste0(eval(sel_county), ".*")) |>
                  stringr::str_replace(".grd", ""),
                ".tif"
              )
            ),
            overwrite = FALSE
          ),
          silent = TRUE
        )
      )
    }

    try(
      terra::writeRaster(
        ras,
        filename = here::here(
          paste0(
            "Presentation/Materials/VegVault/Terrain/Terrain_rasters/raster_",
            names(ras) |>
              stringr::str_extract(paste0(eval(sel_county), ".*")) |>
              stringr::str_replace(".grd", ""),
            ".tif"
          )
        ),
        overwrite = FALSE
      ),
      silent = TRUE
    )
  }
)

#----------------------------------------------------------#
# 3. Merge and lower quality  -----
#----------------------------------------------------------#

# Create list with all exported raster files
ras_lst <-
  list.files(
    here::here("Presentation/Materials/VegVault/Terrain/Terrain_rasters"),
    full.names = TRUE,
    pattern = ".tif"
  ) |>
  stringr::str_subset(pattern = "aux", negate = TRUE)


assertthat::assert_that(
  length(countires_vec) <= length(ras_lst),
  msg = "There are less rasters than the expected countries"
)

# Load each raster and merged them
# !!! This takes time !!!
all_rasters <-
  purrr::map(
    .x = ras_lst,
    .f = ~ terra::rast(.x)
  )

alt_raster <-
  purrr::reduce(
    .x = all_rasters,
    .f = ~ terra::merge(.x, .y)
  )

# turn into data.frame
data_altitude_raw <-
  terra::crds(alt_raster) |>
  data.frame() |>
  tibble::as_tibble() |>
  dplyr::bind_cols(
    terra::values(alt_raster, na.rm = TRUE)
  ) |>
  purrr::set_names(nm = c("long", "lat", "alt_raw"))


#----------------------------------------------------------#
# 3. Save it  -----
#----------------------------------------------------------#

readr::write_rds(
  x = data_altitude_raw,
  file = here::here(
    "Presentation/Materials/VegVault/Terrain/data_altitude_raw.rds"
  )
)
