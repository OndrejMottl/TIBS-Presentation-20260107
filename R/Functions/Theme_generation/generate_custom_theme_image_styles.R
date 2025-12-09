generate_custom_theme_image_styles <- function() {
  c(
    "/* Constrain figures and other large block elements */",
    ".reveal .slides img,",
    ".reveal .slides figure,",
    ".reveal .slides video,",
    ".reveal .slides .reveal-image {",
    "  max-width: 100%;",
    "  width: auto;",
    "  height: auto;",
    "  display: block;",
    "  margin: 0 auto;",
    "  box-sizing: border-box;",
    "}"
  )
}
