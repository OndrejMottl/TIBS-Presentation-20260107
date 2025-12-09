generate_custom_theme_code_block_styles <- function(custom_theme) {
  c(
    "/* Code block */",
    "code {",
    "  background-color: $backgroundColor;",
    "  padding: $smallMargin;",
    paste0("  border-radius: ", custom_theme$code$codeBorderRadius, ";"),
    "  font-family: $monospaceFont;",
    "  border: 1px solid $codeBorderColor;",
    "}"
  )
}
