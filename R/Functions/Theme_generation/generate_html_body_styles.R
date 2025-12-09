generate_html_body_styles <- function() {
  c(
    "// Main content styling",
    "body {",
    "  font-family: $font-family-sans-serif;",
    "  font-size: $font-size-base;",
    "  line-height: $line-height-base;",
    "  color: $bodyColor;",
    "  background-color: $backgroundColor;",
    "}",
    ""
  )
}
