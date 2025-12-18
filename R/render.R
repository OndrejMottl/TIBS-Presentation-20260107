#----------------------------------------------------------#
#
#
#             TIBS - Presentation- 20260107
#
#                  Render presentation
#
#
#                       O.Mottl
#                         2025
#
#----------------------------------------------------------#

# The QUARTO is curently unable to render into other directory.
# GitHub pages require the presentation to be in the `docs` directory.
# This is a workaround to render the presentation into the `docs`` directory

# Setup -----

library(here)

source(
  here::here("R/___setup_project___.R")
)

# Render -----
quarto::quarto_render(
  input = here::here("Presentation/presentation.qmd")
)

# Move the rendered file to the `docs` directory. -----

fs::file_copy(
  path = here::here("Presentation/index.html"),
  new_path = here::here("docs/index.html"),
  overwrite = TRUE
)


# Make PDF version -----

# decktape needs to be installed separately.
# See https://github.com/astefanutti/decktape
system2(
  command = "decktape.cmd",
  args = c(
    "reveal", "--fragments=true",
    here::here("Presentation/index.html"),
    here::here("Presentation/presentation.pdf")
  )
)
