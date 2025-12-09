generate_custom_theme_scss <- function(
  colors_file = here::here("Presentation/colors.json"),
  fonts_file = here::here("Presentation/fonts.json"),
  custom_theme_file = here::here("Presentation/custom_theme.json"),
  output_file = here::here("Presentation/custom_theme.scss")
) {
  message("Generating custom_theme.scss...\n")

  # Check if input files exist
  if (
    !file.exists(colors_file)
  ) {
    stop("colors.json not found. Please create this file first.")
  }
  if (
    !file.exists(fonts_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }
  if (
    !file.exists(custom_theme_file)
  ) {
    stop("custom_theme.json not found. Please create this file first.")
  }

  # Read configuration from JSON files
  colors <- jsonlite::fromJSON(colors_file)
  fonts <- jsonlite::fromJSON(fonts_file)
  custom_theme <- jsonlite::fromJSON(custom_theme_file)

  # Generate custom theme SCSS content
  custom_theme_scss_content <-
    c(
      "// This file is auto-generated from colors.json, fonts.json, and custom_theme.json. Do not edit directly.",
      "// Custom Theme for Reveal.js Presentations",
      "",
      "/*-- scss:defaults --*/",
      "",
      generate_custom_theme_defaults(custom_theme, fonts),
      "",
      "// Colors are now loaded from _colors.scss, which is auto-generated from colors.json.",
      "// Do not define color variables here. Edit colors.json and regenerate _colors.scss if needed.",
      '@import "_colors";',
      "",
      "// Background of the presentation",
      "// All color assignments use direct color names from colors.json",
      "",
      "// Typography is now loaded from _fonts.scss, which is auto-generated from fonts.json.",
      "// Do not define font variables here. Edit fonts.json and regenerate _fonts.scss if needed.",
      '@import "_fonts";',
      "",
      generate_custom_theme_font_overrides(fonts, custom_theme),
      "",
      "",
      "/*-- scss:rules --*/",
      "",
      generate_custom_theme_body_styles(custom_theme),
      "",
      generate_custom_theme_code_styles(custom_theme, colors),
      "",
      generate_custom_theme_scrollbar_styles(),
      "",
      generate_custom_theme_quarto_fixes(),
      "",
      generate_custom_theme_link_styles(),
      "",
      generate_custom_theme_heading_styles(custom_theme),
      "",
      generate_custom_theme_list_styles(custom_theme),
      "",
      generate_custom_theme_utility_classes(custom_theme),
      "",
      generate_custom_theme_slide_layout(custom_theme),
      "",
      generate_custom_theme_image_styles(),
      "",
      generate_custom_theme_specialized_sections(),
      "",
      generate_custom_theme_blockquote_styles(custom_theme, colors),
      "",
      generate_custom_theme_code_block_styles(custom_theme),
      "",
      generate_custom_theme_table_styles(custom_theme),
      "",
      generate_custom_theme_exercise_overrides(),
      "",
      generate_custom_theme_title_slide_overrides()
    )

  # Write to file
  writeLines(
    text = custom_theme_scss_content,
    con = output_file
  )

  message("custom_theme.scss generated successfully\n")
}
