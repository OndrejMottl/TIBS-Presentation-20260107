generate_custom_theme_font_overrides <- function(fonts) {
  c(
    paste0("$mainFontSize: ", fonts$sizes$mainFontSize, ";"),
    paste0("$body-line-height: ", fonts$sizes$bodyLineHeight, ";"),
    "",
    paste0("$headingMargin: 0;"),
    paste0("$headingLineHeight: ", fonts$sizes$headingLineHeight, ";"),
    paste0("$headingLetterSpacing: ", fonts$spacing$headingLetterSpacing, "; /* -2% tracking as per guidelines */"),
    "$headingTextTransform: none; /* SPROuT uses title case, not uppercase */",
    "$headingTextShadow: none;",
    paste0("$headingFontWeight: ", fonts$weights$headingFontWeight, "; /* Semibold as per guidelines */"),
    "",
    paste0("$heading1Size: ", fonts$sizes$heading1Size, "; /* 36-44px equivalent */"),
    "$heading1TextShadow: none;",
    paste0("$heading2Size: ", fonts$sizes$heading2Size, "; /* 28-32px equivalent */"),
    paste0("$heading3Size: ", fonts$sizes$heading3Size, "; /* 22-24px equivalent */"),
    paste0("$heading4Size: ", fonts$sizes$heading4Size, ";")
  )
}
