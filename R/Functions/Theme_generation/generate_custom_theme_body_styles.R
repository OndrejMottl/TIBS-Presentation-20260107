generate_custom_theme_body_styles <- function(custom_theme) {
  c(
    "/* General text styling */",
    "body {",
    "  font-family: $mainFont;",
    "  font-size: $mainFontSize;",
    "  line-height: $body-line-height;",
    "  color: $bodyColor;",
    "  background-color: $backgroundColor !important;",
    "}"
  )
}
