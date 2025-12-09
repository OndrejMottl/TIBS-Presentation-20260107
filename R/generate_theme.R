#----------------------------------------------------------#
#
#
#             TIBS - Presentation- 20260107
#
#                Theme Generation Script
#
#
#                       O. Mottl
#                         2025
#
#----------------------------------------------------------#
# This script automatically generates SCSS files from JSON configuration
# It runs before every Quarto render via the pre-render hook


#----------------------------------------------------------#
# 1. Setup -----
#----------------------------------------------------------#

library(here)
library(purrr)

#----------------------------------------------------------#
# 2. Source all function files -----
#----------------------------------------------------------#

message("Loading theme generation functions...\n")

c(
  # Helper functions
  "calculate_contrast_color.R",
  "resolve_color_name.R",
  # Main SCSS generation functions
  "generate_colors_scss.R",
  "generate_fonts_scss.R",
  "generate_custom_theme_scss.R",
  "generate_r_theme.R",
  # Custom theme helper functions
  "generate_custom_theme_defaults.R",
  "generate_custom_theme_font_overrides.R",
  "generate_custom_theme_body_styles.R",
  "generate_custom_theme_code_styles.R",
  "generate_custom_theme_scrollbar_styles.R",
  "generate_custom_theme_quarto_fixes.R",
  "generate_custom_theme_link_styles.R",
  "generate_custom_theme_heading_styles.R",
  "generate_custom_theme_list_styles.R",
  "generate_custom_theme_utility_classes.R",
  "generate_custom_theme_slide_layout.R",
  "generate_custom_theme_image_styles.R",
  "generate_custom_theme_specialized_sections.R",
  "generate_custom_theme_blockquote_styles.R",
  "generate_custom_theme_code_block_styles.R",
  "generate_custom_theme_table_styles.R",
  "generate_custom_theme_exercise_overrides.R",
  "generate_custom_theme_title_slide_overrides.R",
  # Color and HTML helper functions
  "generate_color_variables.R",
  "generate_html_body_styles.R",
  "generate_html_heading_styles.R",
  "generate_html_link_styles.R",
  "generate_html_blockquote_styles.R",
  "generate_html_table_styles.R",
  "generate_html_utility_classes.R",
  "generate_html_responsive_styles.R",
  # R theme and fonts HTML generation
  "generate_r_theme.R",
  "generate_fonts_html.R"
) |>
  purrr::walk(
    .f = ~ source(here::here("R/Functions/Theme_generation", .x))
  )


message("All functions loaded successfully\n\n")

#----------------------------------------------------------#
# 3. Main execution -----
#----------------------------------------------------------#
message("Theme Generation\n")
message("Starting theme generation process...\n\n")

tryCatch(
  {
    # Generate all theme files
    # Generate colors and fonts SCSS for Presentation folder
    generate_colors_scss()
    generate_fonts_scss()

    # Generate other theme files
    generate_fonts_html()
    generate_custom_theme_scss()
    generate_r_theme()

    message("\nTheme generation completed successfully!\n")
    message("Generated files:\n")
    message("  - _colors.scss\n")
    message("  - _fonts.scss\n")
    message("  - fonts-include.html\n")
    message("  - custom_theme.scss\n")
    message("  - R/set_r_theme.R\n")
  },
  error = function(e) {
    message("\nError during theme generation:\n")
    message(paste("Error:", e$message, "\n"))
    quit(status = 1)
  }
)
