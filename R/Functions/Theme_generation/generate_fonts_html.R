generate_fonts_html <- function(
  input_file = here::here("Presentation/fonts.json"),
  output_file = here::here("Presentation/fonts-include.html")
) {
  message("Generating fonts-include.html...\n")

  # Check if fonts.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Generate Google Fonts links
  font_links <-
    c(
      "<!-- Auto-generated Google Fonts links from fonts.json -->"
    )

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Generate Google Fonts links
  font_links <-
    c(
      "<!-- Auto-generated Google Fonts links from fonts.json -->"
    )

  # Add specific font links for Google Fonts
  if (
    !is.null(fonts$body) && fonts$body != ""
  ) {
    body_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$body), ":wght@", fonts$googleFonts$bodyWeights, '&display=swap" rel="stylesheet">')
    font_links <-
      c(font_links, body_font_url)
  }

  if (
    !is.null(fonts$heading) && fonts$heading != ""
  ) {
    heading_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$heading), ":wght@", fonts$googleFonts$headingWeights, '&display=swap" rel="stylesheet">')

    font_links <-
      c(font_links, heading_font_url)
  }

  if (
    !is.null(fonts$monospace) && fonts$monospace != ""
  ) {
    mono_font_url <-
      paste0('<link href="https://fonts.googleapis.com/css2?family=', gsub(" ", "+", fonts$monospace), ":wght@", fonts$googleFonts$monospaceWeights, '&display=swap" rel="stylesheet">')
    font_links <-
      c(font_links, mono_font_url)
  }

  # Write to fonts-include.html
  writeLines(
    text = c(
      font_links
    ),
    con = output_file
  )

  message("fonts-include.html generated successfully\n")
}
