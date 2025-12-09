generate_fonts_scss <- function(
  input_file = here::here("Presentation/fonts.json"),
  output_file = here::here("Presentation/_fonts.scss"),
  use_html_sizes = FALSE
) {
  message("Generating _fonts.scss...\n")

  # Check if fonts.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("fonts.json not found. Please create this file first.")
  }

  # Read fonts from JSON
  fonts <- jsonlite::fromJSON(input_file)

  # Choose which size configuration to use
  if (
    use_html_sizes && !is.null(fonts$htmlSizes)
  ) {
    size_config <- fonts$htmlSizes
    size_comment <- "// Font sizes optimized for HTML documents"
  } else {
    size_config <- fonts$sizes
    size_comment <- "// Font sizes from fonts.json"
  }

  # Generate SCSS font definitions
  fonts_definition <-
    c(
      "// This file is auto-generated from fonts.json. Do not edit directly.\n",
      paste0('$mainFont: "', fonts$body, '", "', paste(fonts$fallbacks$body, collapse = '", "'), '" !default;\n'),
      paste0('$headingFont: "', fonts$heading, '", "', paste(fonts$fallbacks$heading, collapse = '", "'), '" !default;\n'),
      paste0('$monospaceFont: "', fonts$monospace, '", "', paste(fonts$fallbacks$monospace, collapse = '", "'), '" !default;\n'),
      "\n",
      size_comment,
      paste0("$mainFontSize: ", size_config$mainFontSize, " !default;"),
      paste0("$heading1Size: ", size_config$heading1Size, " !default;"),
      paste0("$heading2Size: ", size_config$heading2Size, " !default;"),
      paste0("$heading3Size: ", size_config$heading3Size, " !default;"),
      paste0("$heading4Size: ", size_config$heading4Size, " !default;"),
      paste0("$body-line-height: ", size_config$bodyLineHeight, " !default;"),
      paste0("$headingLineHeight: ", size_config$headingLineHeight, " !default;"),
      "\n",
      "// Font weights from fonts.json",
      paste0("$headingFontWeight: ", fonts$weights$headingFontWeight, " !default;"),
      paste0("$bodyFontWeight: ", fonts$weights$bodyFontWeight, " !default;"),
      paste0("$boldFontWeight: ", fonts$weights$boldFontWeight, " !default;"),
      "\n",
      "// Font spacing from fonts.json",
      paste0("$headingLetterSpacing: ", fonts$spacing$headingLetterSpacing, " !default;"),
      paste0("$bodyLetterSpacing: ", fonts$spacing$bodyLetterSpacing, " !default;"),
      "\n",
      "// HTML-specific font variables for compatibility",
      "$font-family-sans-serif: $mainFont !default;",
      "$headings-font-family: $headingFont !default;",
      "$font-family-monospace: $monospaceFont !default;",
      "$font-size-base: $mainFontSize !default;",
      "$line-height-base: $body-line-height !default;",
      "$headings-line-height: $headingLineHeight !default;",
      "$headings-font-weight: $headingFontWeight !default;",
      "$headings-margin-bottom: 1rem !default;",
      if (use_html_sizes && !is.null(size_config$blockMargin)) {
        paste0("$block-margin: ", size_config$blockMargin, " !default;")
      } else {
        "$block-margin: 1.5rem !default;"
      },
      "\n",
      "// Spacing variables used in utility classes",
      if (use_html_sizes && !is.null(size_config$smallMargin)) {
        paste0("$smallMargin: ", size_config$smallMargin, " !default;")
      } else {
        "$smallMargin: 5px !default;"
      },
      "$largeMargin: 20px !default;",
      "\n",
      "// Utility classes for font families",
      ".text-font-body { font-family: $mainFont; }",
      ".text-font-heading { font-family: $headingFont; }",
      ".text-font-monospace { font-family: $monospaceFont; }",
      "\n",
      "// Utility classes for font sizes",
      ".text-size-main { font-size: $mainFontSize !important; }",
      ".text-size-heading1 { font-size: $heading1Size !important; }",
      ".text-size-heading2 { font-size: $heading2Size !important; }",
      ".text-size-heading3 { font-size: $heading3Size !important; }",
      ".text-size-heading4 { font-size: $heading4Size !important; }",
      ".text-size-body { font-size: $mainFontSize !important; }",
      if (!use_html_sizes) {
        c(
          paste0(".text-smaller { font-size: calc($mainFontSize * ", fonts$sizes$textSizeSmall, ") !important; }"),
          paste0(".text-tiny { font-size: calc($mainFontSize * ", fonts$sizes$textSizeTiny, ") !important; }"),
          paste0(".text-larger { font-size: calc($mainFontSize * ", fonts$sizes$textSizeLarge, ") !important; }")
        )
      } else {
        c(
          ".text-smaller { font-size: calc($mainFontSize * 0.8) !important; }",
          ".text-tiny { font-size: calc($mainFontSize * 0.7) !important; }",
          ".text-larger { font-size: calc($mainFontSize * 1.2) !important; }"
        )
      },
      "\n",
      "/* Debug font loading - this will help us see if fonts are loaded */",
      paste0('@supports (font-family: "', fonts$heading, '") {'),
      "  .debug-font-heading::before {",
      paste0('    content: "', fonts$heading, ' font is supported";'),
      "    display: block;",
      "    font-size: 12px;",
      "    color: green;",
      "  }",
      "}",
      paste0('@supports (font-family: "', fonts$monospace, '") {'),
      "  .debug-font-mono::before {",
      paste0('    content: "', fonts$monospace, ' font is supported";'),
      "    display: block;",
      "    font-size: 12px;",
      "    color: green;",
      "  }",
      "}"
    )

  # Write to _fonts.scss
  writeLines(
    text = c(
      fonts_definition
    ),
    con = output_file
  )

  message("_fonts.scss generated successfully\n")
}
