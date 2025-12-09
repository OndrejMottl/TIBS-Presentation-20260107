generate_html_blockquote_styles <- function() {
  c(
    "// Blockquote styling",
    "blockquote {",
    "  border-left: 4px solid $blockquoteBorderColor;",
    "  background-color: rgba($blockquoteBackgroundTint, 0.1);",
    "  padding: 1rem;",
    "  margin: 1rem 0;",
    "  margin-bottom: $block-margin;",
    "  font-style: italic;",
    "  border-radius: 0.375rem;",
    "}"
  )
}
