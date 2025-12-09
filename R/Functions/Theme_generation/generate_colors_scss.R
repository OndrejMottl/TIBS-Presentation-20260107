# Function to generate colors SCSS from JSON
generate_colors_scss <- function(
  input_file = here::here("Presentation/colors.json"),
  output_file = here::here("Presentation/_colors.scss")
) {
  message("Generating _colors.scss...\n")

  # Check if colors.json exists
  if (
    !file.exists(input_file)
  ) {
    stop("colors.json not found. Please create this file first.")
  }

  # Read colors from JSON
  colors_data <- jsonlite::fromJSON(input_file)

  # Handle both new structured format and old format (backward compatibility)
  if ("primary" %in% names(colors_data)) {
    primary_colors <- colors_data$primary

    # Handle new structured format with ui, components, utilities
    if ("ui" %in% names(colors_data)) {
      ui_colors <- colosr_data$ui
      component_colors <- colors_data$components
      utility_colors <- colors_data$utilities

      # Combine all semantic colors for processing
      all_semantic_colors <- c(ui_colors, component_colors, utility_colors)
    } else if ("semantic" %in% names(colors_data)) {
      # Old semantic format (backward compatibility)
      all_semantic_colors <- colors_data$semantic
      ui_colors <- list()
      component_colors <- list()
      utility_colors <- list()
    } else {
      all_semantic_colors <- list()
      ui_colors <- list()
      component_colors <- list()
      utility_colors <- list()
    }
  } else {
    # Very old flat format (backward compatibility)
    primary_colors <- colors_data
    all_semantic_colors <- list()
    ui_colors <- list()
    component_colors <- list()
    utility_colors <- list()
  }

  # Helper function to resolve color references
  resolve_color_reference <- function(color_value, primary_colors) {
    if (color_value %in% names(primary_colors)) {
      return(primary_colors[[color_value]])
    } else {
      return(color_value)
    }
  }

  # Generate primary color variables
  primary_section <- c(
    "// ==============================================================================",
    "// PRIMARY COLORS - Core brand palette",
    "// ==============================================================================",
    unlist(purrr::imap(primary_colors, ~ paste0("$", .y, ": ", .x, ";"))),
    ""
  )

  # Generate semantic color variables
  semantic_section <- if (length(all_semantic_colors) > 0) {
    c(
      "// ==============================================================================",
      "// SEMANTIC COLORS - Functional color assignments",
      "// ==============================================================================",
      unlist(purrr::imap(all_semantic_colors, ~ {
        resolved_value <- resolve_color_reference(.x, primary_colors)
        if (.x %in% names(primary_colors)) {
          paste0("$", .y, ": $", .x, ";")
        } else {
          paste0("$", .y, ": ", resolved_value, ";")
        }
      })),
      ""
    )
  } else {
    c("")
  }

  # Generate contrast color variables
  contrast_section <- c(
    "// ==============================================================================",
    "// CONTRAST COLORS - For text contrast calculations",
    "// ==============================================================================",
    if ("contrastColorLight" %in% names(all_semantic_colors)) {
      c(
        paste0("$contrastColorLight: $", all_semantic_colors[["contrastColorLight"]], ";"),
        paste0("$contrastColorDark: $", all_semantic_colors[["contrastColorDark"]], ";")
      )
    } else {
      c("$contrastColorLight: $black;", "$contrastColorDark: $white;")
    },
    ""
  )

  # Generate UI color section
  ui_section <- if (length(ui_colors) > 0) {
    c(
      "// ==============================================================================",
      "// UI COLORS - Basic user interface elements",
      "// ==============================================================================",
      unlist(purrr::imap(ui_colors, ~ {
        resolved_value <- resolve_color_reference(.x, primary_colors)
        if (.x %in% names(primary_colors)) {
          paste0("$", .y, ": $", .x, ";")
        } else {
          paste0("$", .y, ": ", resolved_value, ";")
        }
      })),
      ""
    )
  } else {
    c("")
  }

  # Generate component color section
  component_section <- if (length(component_colors) > 0) {
    c(
      "// ==============================================================================",
      "// COMPONENT COLORS - Specific component styling",
      "// ==============================================================================",
      unlist(purrr::imap(component_colors, ~ {
        resolved_value <- resolve_color_reference(.x, primary_colors)
        if (.x %in% names(primary_colors)) {
          paste0("$", .y, ": $", .x, ";")
        } else {
          paste0("$", .y, ": ", resolved_value, ";")
        }
      })),
      "",
      "// Component-specific compatibility variables",
      "$code-color: $codeColor;",
      ""
    )
  } else {
    c("")
  }

  # Generate utility color classes for primary colors
  primary_utility_classes <- purrr::imap(primary_colors, ~ {
    contrast_color <- calculate_contrast_color(.x)
    paste0(
      "// Primary color: ", .y, "\n",
      ".reveal .bg-", .y, " { background-color: ", .x, "; }\n",
      ".text-color-", .y, " { color: ", .x, " !important; }\n",
      ".text-background-", .y, " {\n",
      "  background-color: ", .x, ";\n",
      "  padding: $smallMargin;\n",
      "  border-radius: 5px;\n",
      "}\n",
      ".text-highlight-", .y, " {\n",
      "  background-color: ", .x, ";\n",
      "  color: ", contrast_color, ";\n",
      "  padding: 2px 4px;\n",
      "  border-radius: 3px;\n",
      "}\n"
    )
  }) |>
    unlist() |>
    paste(collapse = "\n")

  # Generate utility classes section
  utility_classes_section <- c(
    "// ==============================================================================",
    "// UTILITY CLASSES - CSS classes for primary colors",
    "// ==============================================================================",
    primary_utility_classes,
    "",
    "// Semantic utility classes",
    ".text-color-body { color: $bodyColor !important; }",
    ".text-color-background { color: $backgroundColor !important; }",
    ".text-color-heading { color: $headingColor !important; }",
    ".text-color-link { color: $linkColor !important; }",
    ".text-color-accent { color: $accentColor !important; }",
    ""
  )

  # Generate utility color variables section (for utility classes)
  utility_section <- if (length(utility_colors) > 0) {
    c(
      "// ==============================================================================",
      "// UTILITY COLOR VARIABLES - Additional semantic color mappings",
      "// ==============================================================================",
      unlist(purrr::imap(utility_colors, ~ {
        resolved_value <- resolve_color_reference(.x, primary_colors)
        if (.x %in% names(primary_colors)) {
          paste0("$", .y, ": $", .x, ";")
        } else {
          paste0("$", .y, ": ", resolved_value, ";")
        }
      })),
      ""
    )
  } else {
    c("")
  }

  # Generate specialized color classes for semantic colors (at the end)
  specialized_classes <- if (length(all_semantic_colors) > 0) {
    resolved_semantic <- purrr::imap(all_semantic_colors, ~ {
      resolved_value <- resolve_color_reference(.x, primary_colors)
      list(name = .y, hex = resolved_value)
    })

    semantic_utility_classes <- purrr::map(resolved_semantic, ~ {
      contrast_color <- calculate_contrast_color(.x$hex)
      paste0(
        "// Semantic color: ", .x$name, "\n",
        ".reveal .bg-", .x$name, " { background-color: ", .x$hex, "; }\n",
        ".text-color-", .x$name, " { color: ", .x$hex, " !important; }\n",
        ".text-background-", .x$name, " {\n",
        "  background-color: ", .x$hex, ";\n",
        "  padding: $smallMargin;\n",
        "  border-radius: 5px;\n",
        "}\n",
        ".text-highlight-", .x$name, " {\n",
        "  background-color: ", .x$hex, ";\n",
        "  color: ", contrast_color, ";\n",
        "  padding: 2px 4px;\n",
        "  border-radius: 3px;\n",
        "}\n"
      )
    }) |>
      unlist() |>
      paste(collapse = "\n")

    c(
      "// ==============================================================================",
      "// SPECIALIZED COLOR CLASSES - CSS classes for semantic colors",
      "// ==============================================================================",
      semantic_utility_classes
    )
  } else {
    c("")
  }

  # Combine all sections
  scss_content <- c(
    "// This file is auto-generated from colors.json. Do not edit directly.",
    "",
    primary_section,
    semantic_section,
    contrast_section,
    ui_section,
    component_section,
    utility_classes_section,
    utility_section,
    specialized_classes
  )

  # Write to _colors.scss
  writeLines(
    text = scss_content,
    con = output_file
  )

  message("_colors.scss generated successfully\n")
}
