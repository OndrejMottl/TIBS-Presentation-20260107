generate_html_utility_classes <- function() {
  c(
    "// Utility classes for brand colors",
    ".text-dark-green { color: $utilityDarkGreen !important; }",
    ".text-teal { color: $utilityTeal !important; }",
    ".text-pistachio { color: $utilityPistachio !important; }",
    ".text-seal-brown { color: $utilitySealBrown !important; }",
    ".text-fulvous { color: $utilityFulvous !important; }",
    "",
    ".bg-dark-green { background-color: $utilityDarkGreen !important; }",
    ".bg-teal { background-color: $utilityTeal !important; }",
    ".bg-pistachio { background-color: $utilityPistachio !important; }",
    ".bg-seal-brown { background-color: $utilitySealBrown !important; }",
    ".bg-fulvous { background-color: $utilityFulvous !important; }"
  )
}
