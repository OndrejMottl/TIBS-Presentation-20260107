# Helper function to calculate luminance and determine contrast color
calculate_contrast_color <- function(hex_color) {
  # Remove # if present
  hex_color <- gsub("#", "", hex_color)

  # Convert hex to RGB
  r <- as.numeric(paste0("0x", substr(hex_color, 1, 2))) / 255
  g <- as.numeric(paste0("0x", substr(hex_color, 3, 4))) / 255
  b <- as.numeric(paste0("0x", substr(hex_color, 5, 6))) / 255

  # Calculate relative luminance using sRGB formula
  luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b

  # Return semantic contrast colors for light/dark backgrounds
  if (luminance > 0.5) {
    return("$contrastColorLight") # Use dark text on light backgrounds
  } else {
    return("$contrastColorDark") # Use light text on dark backgrounds
  }
}
