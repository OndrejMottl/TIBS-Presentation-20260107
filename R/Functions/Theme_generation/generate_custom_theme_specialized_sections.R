generate_custom_theme_specialized_sections <- function() {
  c(
    "/* Specialized sections */",
    ".reveal .title {",
    "  background-color: $headingColor;",
    "  color: $backgroundColor;",
    "  text-align: center;",
    "}",
    "",
    ".reveal .subtitle {",
    "  background-color: $subtitleBackground;",
    "  color: $backgroundColor;",
    "  text-align: center;",
    "}",
    "",
    ".reveal .inverse {",
    "  color: $backgroundColor;",
    "  background-color: $headingColor;",
    "}",
    "",
    ".reveal .exercise {",
    "  background-color: $linkColor;",
    "  color: $backgroundColor;",
    "}"
  )
}
