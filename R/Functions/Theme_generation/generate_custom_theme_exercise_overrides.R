generate_custom_theme_exercise_overrides <- function() {
  c(
    "/* EXERCISE SECTION LINK OVERRIDES - Must be last to ensure precedence */",
    ".reveal .exercise a,",
    ".reveal .exercise * a,",
    ".reveal .slides .exercise a,",
    ".reveal .slides .exercise * a,",
    ".reveal .slides section.exercise a,",
    ".reveal .slides section.exercise * a {",
    "  color: $backgroundColor !important;",
    "  text-decoration: underline !important;",
    "  text-decoration-color: $backgroundColor !important;",
    "  font-weight: 600 !important;",
    "  background-color: transparent !important;",
    "",
    "  &:hover,",
    "  &:focus,",
    "  &:active {",
    "    color: $headingColor !important;",
    "    background-color: $backgroundColor !important;",
    "    border-radius: 3px !important;",
    "    padding: 2px 4px !important;",
    "    text-decoration: underline !important;",
    "    text-decoration-color: $headingColor !important;",
    "  }",
    "}"
  )
}
