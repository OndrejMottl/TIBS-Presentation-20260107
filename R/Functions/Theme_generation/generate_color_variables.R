generate_color_variables <- function(colors_data) {
  # Handle both new structured format and old format (backward compatibility)
  if ("primary" %in% names(colors_data)) {
    primary_colors <- colors_data$primary

    # Handle new structured format with ui, components, utilities
    if ("ui" %in% names(colors_data)) {
      ui_colors <- colors_data$ui
      component_colors <- colors_data$components
      utility_colors <- colors_data$utilities
      all_semantic_colors <- c(ui_colors, component_colors, utility_colors)
    } else if ("semantic" %in% names(colors_data)) {
      # Old semantic format (backward compatibility)
      all_semantic_colors <- colors_data$semantic
    } else {
      all_semantic_colors <- list()
    }

    primary_vars <- c(
      "// SPROuT Brand Colors",
      purrr::imap(primary_colors, ~ paste0("$", .y, ": ", .x, ";"))
    )

    # Generate semantic color variables, resolving references to primary colors
    semantic_vars <- c("", "// Semantic Colors")
    if (length(all_semantic_colors) > 0) {
      for (semantic_name in names(all_semantic_colors)) {
        semantic_value <- all_semantic_colors[[semantic_name]]

        # If the semantic color references a primary color, use the primary color
        if (semantic_value %in% names(primary_colors)) {
          semantic_vars <- c(semantic_vars, paste0("$", semantic_name, ": $", semantic_value, ";"))
        } else {
          # If it's a hex value, use it directly
          semantic_vars <- c(semantic_vars, paste0("$", semantic_name, ": ", semantic_value, ";"))
        }
      }
    }

    return(c(primary_vars, semantic_vars))
  } else {
    # Very old flat format (backward compatibility)
    # Generate variables dynamically from whatever colors are present
    color_vars <- purrr::imap_chr(colors_data, ~ paste0("$", .y, ": ", .x, ";"))
    return(c(
      "// Colors (legacy flat format)",
      color_vars
    ))
  }
}
