generate_custom_theme_defaults <- function(custom_theme, fonts) {
  c(
    "// Vertical spacing between blocks of text",
    paste0("$smallMargin: ", custom_theme$margins$smallMargin, ";"),
    paste0("$blockMargin: ", custom_theme$margins$blockMargin, ";"),
    paste0("$largeMargin: ", custom_theme$margins$largeMargin, ";")
  )
}
