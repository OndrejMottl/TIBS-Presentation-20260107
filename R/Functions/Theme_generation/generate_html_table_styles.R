generate_html_table_styles <- function() {
  c(
    "// Table styling",
    "table {",
    "  border-collapse: collapse;",
    "  margin-bottom: $block-margin;",
    "  width: 100%;",
    "",
    "  th, td {",
    "    border: 1px solid rgba($tableBorderColor, 0.2);",
    "    padding: 0.75rem;",
    "    text-align: left;",
    "  }",
    "",
    "  th {",
    "    background-color: $tableHeaderBackground !important;",
    "    color: $tableHeaderColor !important;",
    "    font-weight: 600;",
    "  }",
    "",
    "  tr:nth-child(odd) {",
    "    background-color: rgba($tableStripeAlternate, 0.5) !important;",
    "  }",
    "}"
  )
}
