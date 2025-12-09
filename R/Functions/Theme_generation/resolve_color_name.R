resolve_color_name <- function(color_name, colors_data) {
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

    all_colors <- c(primary_colors, all_semantic_colors)
  } else {
    # Very old flat format (backward compatibility)
    all_colors <- colors_data
    primary_colors <- colors_data
    all_semantic_colors <- list()
  }

  # If it's a hex color (starts with #), return as is
  if (grepl("^#", color_name)) {
    return(color_name)
  }

  # If it's a semantic color that references a primary color
  if (color_name %in% names(all_semantic_colors)) {
    referenced_color <- all_semantic_colors[[color_name]]
    # If the referenced color is a primary color name, resolve it
    if (referenced_color %in% names(primary_colors)) {
      return(paste0("$", referenced_color))
    } else {
      # If it's already a hex value, return as SCSS variable
      return(paste0("$", color_name))
    }
  }

  # If it's a known primary color name, return as SCSS variable
  if (color_name %in% names(primary_colors)) {
    return(paste0("$", color_name))
  }

  # If it's a known color name, return as SCSS variable
  if (color_name %in% names(all_colors)) {
    return(paste0("$", color_name))
  }

  # If not found, return as is (might be a SCSS variable already)
  return(color_name)
}
