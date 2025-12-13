#----------------------------------------------------------#
#
#
#             TIBS - Presentation- 20260107
#
#                   Configuration script
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#

# Configuration script with the variables that should be consistent throughout
#   the whole repo. It loads packages, defines important variables,
#   authorises the user, and saves config file.

# Set the current environment
current_env <- environment()


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#

library(
  "here",
  quietly = TRUE,
  warn.conflicts = FALSE,
  character.only = TRUE,
  verbose = FALSE
)

if (
  isFALSE(
    exists("already_synch", envir = current_env)
  )
) {
  already_synch <- FALSE
}

if (
  isFALSE(already_synch)
) {
  library(here)
  # Synchronise the package versions
  renv::restore(
    lockfile = here::here("renv.lock")
  )
  already_synch <- TRUE

  # Save snapshot of package versions
  # renv::snapshot(lockfile =  here::here("renv.lock"))  # do only for update
}

# Define packages
package_list <-
  c(
    "assertthat",
    "geodata",
    "ggnewscale",
    "here",
    "janitor",
    "jsonlite",
    "knitr",
    "languageserver",
    "renv",
    "pak",
    "rlang",
    "terra",
    "tidyverse",
    "usethis",
    "utils"
  )

# Attach all packages
sapply(
  package_list,
  function(x) {
    library(x,
      quietly = TRUE,
      warn.conflicts = FALSE,
      character.only = TRUE,
      verbose = FALSE
    )
  }
)


#----------------------------------------------------------#
# 2. Load functions -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <-
  list.files(
    path = here::here("R/Functions/"),
    pattern = "*.R",
    recursive = TRUE
  )

# source them
if (
  length(fun_list) > 0
) {
  sapply(
    paste0(here::here("R/Functions"), "/", fun_list, sep = ""),
    source
  )
}


#----------------------------------------------------------#
# 3. Get path to VegVault -----
#----------------------------------------------------------#

# !!!  IMPORTANT  !!!

# This solution was created due to VegVault data being a large file

# Please download the data from the VegVault repository and place the path to it
#  in the '.secrets/path.yaml' file.

if (
  file.exists(
    here::here(".secrets/path.yaml")
  )
) {
  path_to_vegvault <-
    yaml::read_yaml(
      here::here(".secrets/path.yaml")
    ) |>
    purrr::chuck(Sys.info()["user"])
} else {
  warning(
    paste(
      "The path to the VegVault data is not specified.",
      " Please, create a 'path.yaml' file."
    )
  )
}


#----------------------------------------------------------#
# 4. Graphical options -----
#----------------------------------------------------------#

source(
  here::here("R/generate_theme.R")
)

source(
  here::here("R/set_r_theme.R")
)
