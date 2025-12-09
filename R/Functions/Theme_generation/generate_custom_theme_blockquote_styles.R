generate_custom_theme_blockquote_styles <- function(custom_theme, colors) {
  c(
    "/* Blockquote */",
    ".reveal .blockquote,",
    "blockquote {",
    paste0("  padding: ", custom_theme$blockquote$blockquotePadding, "; /* Following brand guidelines */"),
    paste0("  border-radius: ", custom_theme$blockquote$blockquoteBorderRadius, "; /* 2xl rounded corners */"),
    "  background-color: darken($white, 20%);",
    paste0("  border-left: ", custom_theme$blockquote$blockquoteBorderWidth, " solid $blockquoteBorderColor;"),
    "  border-top: 2px solid $blockquoteBorderColor;",
    "  border-bottom: 2px solid $blockquoteBorderColor;",
    "  border-right: 2px solid $blockquoteBorderColor;",
    paste0("  box-shadow: ", custom_theme$shadows$blockquoteShadow, "; /* Soft shadow with dark Green */"),
    paste0("  transition: background-color ", custom_theme$transitions$defaultTransition, ", border-color ", custom_theme$transitions$defaultTransition, ";"),
    paste0("  margin: ", custom_theme$blockquote$blockquoteMargin, ";"),
    paste0("  color: ", colors$semantic$blockquoteTextColor, ";"),
    "  font-style: italic;",
    "}"
  )
}
