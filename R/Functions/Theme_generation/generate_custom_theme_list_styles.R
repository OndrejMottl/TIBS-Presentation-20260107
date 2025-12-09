generate_custom_theme_list_styles <- function(custom_theme) {
  c(
    "/* List styling */",
    "ul {",
    "  padding-left: 0;",
    "  margin-left: $blockMargin;",
    "}",
    "",
    "ul ul {",
    "  margin-left: $blockMargin;",
    "}"
  )
}
