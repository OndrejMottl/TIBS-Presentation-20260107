generate_html_responsive_styles <- function() {
  c(
    "// Responsive adjustments",
    "@media (max-width: 768px) {",
    "  .quarto-container {",
    "    max-width: 100%;",
    "    padding: 0 1rem;",
    "  }",
    "",
    "  h1, .h1 { font-size: 1.75rem; }",
    "  h2, .h2 { font-size: 1.5rem; }",
    "  h3, .h3 { font-size: 1.25rem; }",
    "}"
  )
}
