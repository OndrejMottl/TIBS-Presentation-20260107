generate_custom_theme_footer_styles <- function(fonts) {
  # Extract footer font size from fonts config
  footer_font_size <- fonts$sizes$footerFontSize
  
  c(
    "/* Footer styles */",
    ".reveal .footer {",
    paste0("  font-size: ", footer_font_size, " !important;"),
    "  text-align: center;",
    "  padding: 0.5em;",
    "}"
  )
}
