generate_custom_theme_quarto_fixes <- function() {
  c(
    "/* Quarto-specific code elements */",
    ".sourceCode code {",
    "  background-color: transparent !important;",
    "  color: inherit !important;",
    "}",
    "",
    "/* Fix for code blocks in HTML output */",
    ".sourceCode {",
    "  margin-left: 0 !important;",
    "  margin-right: 0 !important;",
    "}"
  )
}
