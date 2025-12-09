generate_html_link_styles <- function() {
  c(
    "// Link styling",
    "a {",
    "  color: $linkColor;",
    "  text-decoration: none;",
    "  transition: color 0.2s ease;",
    "",
    "  &:hover {",
    "    color: darken($linkColor, 15%);",
    "    text-decoration: underline;",
    "  }",
    "}"
  )
}
