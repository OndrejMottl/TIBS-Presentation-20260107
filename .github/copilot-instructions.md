# Copilot Instructions: Quarto Presentation Projects

## Project Architecture

This workspace contains **academic presentation projects** built with Quarto + Reveal.js + R. Each project is a standalone presentation with a date-stamped naming convention (e.g., `TIBS-Presentation-20260107`). Projects share architectural patterns but are independent.

### Core Components

- **Quarto Presentation Layer**: `.qmd` files with YAML frontmatter → rendered to `index.html` via Reveal.js
- **R Analysis Layer**: Data wrangling, visualization, and figure generation in `R/` directory
- **Theme System**: JSON-driven theme generation that compiles to SCSS automatically
- **Package Management**: `renv` for reproducible R environments

## Essential Workflows

### Rendering a Presentation

**Primary command** (run from R or R script):
```r
source(here::here("R/render.R"))
```

This script:
1. Renders `presentation.qmd` (or `Presentation/presentation.qmd`) via `quarto::quarto_render()`
2. Copies `index.html` to `docs/` for GitHub Pages deployment
3. Optionally generates PDF via `decktape.cmd` (requires separate installation)

**Direct Quarto render** (if not using custom render script):
```r
quarto::quarto_render("presentation.qmd")  # or "Presentation/presentation.qmd"
```

### Initial Setup

1. **Restore R environment**:
   ```r
   renv::restore(lockfile = "renv.lock")
   ```
   This happens automatically via `R/___setup_project___.R` but can be manually triggered.

2. **Configure VegVault data path** (if project uses external VegVault data):
   - Create `.secrets/path.yaml` with:
     ```yaml
     YOUR_USERNAME: "path/to/vegvault/data"
     ```
   - Script reads `Sys.info()["user"]` to match username
   - See `R/___setup_project___.R` lines 118-142 for implementation

### Theme Generation System

**Complete Theme Build Workflow** (auto-triggered on every render):

```
1. R/___setup_project___.R
   └─> sources R/generate_theme.R

2. R/generate_theme.R
   ├─> loads functions from R/Functions/Theme_generation/
   └─> executes in order:
       ├─> generate_colors_scss()
       │   └─> reads Presentation/colors.json
       │       └─> writes Presentation/_colors.scss
       │
       ├─> generate_fonts_scss()
       │   └─> reads Presentation/fonts.json
       │       └─> writes Presentation/_fonts.scss
       │
       ├─> generate_fonts_html()
       │   └─> reads Presentation/fonts.json
       │       └─> writes Presentation/fonts-include.html
       │
       ├─> generate_custom_theme_scss()
       │   └─> reads Presentation/custom_theme.json
       │       └─> writes Presentation/custom_theme.scss
       │
       └─> generate_r_theme()
           └─> writes R/set_r_theme.R (for ggplot2)

3. Presentation/presentation.qmd (YAML frontmatter)
   ├─> theme: [default, custom_theme.scss]  ← uses generated SCSS
   └─> include-in-header: "fonts-include.html"  ← uses generated HTML
```

**JSON Configuration Files** (human-editable source of truth):

- **`colors.json`**: Two-tier color system
  ```json
  {
    "primary": {
      "white": "#ffffff",
      "blue": "#415280",
      "purple": "#8a5697"
    },
    "semantic": {
      "backgroundColor": "white",
      "linkColor": "green",
      "headingColor": "black"
    }
  }
  ```
  - `primary`: Palette colors (hex codes)
  - `semantic`: UI mappings (reference primary colors by name)

- **`fonts.json`**: Font families + sizing
  ```json
  {
    "body": "Inconsolata",
    "heading": "Poppins",
    "sizes": {
      "mainFontSize": "30px",
      "heading1Size": "2.2em"
    }
  }
  ```

- **`custom_theme.json`**: Additional Reveal.js theme overrides (optional)

**Modifying themes**:
1. Edit JSON files (`colors.json`, `fonts.json`, `custom_theme.json`)
2. Run `source(here::here("R/generate_theme.R"))` OR just render (auto-triggers)
3. **NEVER** edit `_colors.scss`, `_fonts.scss`, `fonts-include.html` directly - they're auto-generated

**Theme generation functions** in `R/Functions/Theme_generation/`:
- `generate_colors_scss.R`, `generate_fonts_scss.R` - core SCSS generators
- `generate_fonts_html.R` - HTML `<link>` tags for Google Fonts
- `generate_custom_theme_*.R` - modular component generators (25+ functions)
- Pattern: Each UI element (headings, links, tables, etc.) has dedicated generator
- Helper functions: `resolve_color_name.R`, `calculate_contrast_color.R`

## Project-Specific Conventions

### File Organization

```
Project-Name-YYYYMMDD/
├── presentation.qmd           # Main source (or Presentation/presentation.qmd)
├── _quarto.yml                # Minimal project config
├── custom_theme.scss          # Compiled theme (auto-generated)
├── colors.json, fonts.json    # Theme definitions
├── R/
│   ├── ___setup_project___.R  # Central config - source this first
│   ├── render.R               # Render + deploy script
│   ├── generate_theme.R       # Theme compilation orchestrator
│   └── Functions/             # Helper functions (auto-sourced)
├── Data/                      # Analysis input data (.rds files)
├── Materials/                 # Static assets (logos, figures, QR codes)
│   ├── About/, AI/, Logos/, QR/, R_generated/
├── renv/                      # Package environment (managed by renv)
└── docs/                      # GitHub Pages output (index.html)
```

### R Script Patterns

**Standard script structure**:
```r
#----------------------------------------------------------#
# Project Name - Script Purpose
# Author, Year
#----------------------------------------------------------#

# 0. Setup -----
library(here)
source(here::here("R/___setup_project___.R"))

# 1. Load data -----
data <- readr::read_rds(here::here("Data/file.rds"))

# 2. Process -----
# ... analysis code ...

# 3. Save output -----
# Save to Materials/R_generated/ for inclusion in presentation
```

**Key conventions**:
- Always use `here::here()` for paths - projects use multi-root workspaces
- Source `R/___setup_project___.R` for package loading + function sourcing
- Functions in `R/Functions/` auto-source via setup script
- Use quiet library loading: `library(pkg, quietly = TRUE, warn.conflicts = FALSE)`

### Quarto Chunk Options

Standard setup chunk in all `.qmd` files:
```r
#| label: setup
#| include: false
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(
  fig.align = "center",
  out.width = "100%",
  dpi = 300
)

# Install + restore environment
if (!require("renv")) install.packages("renv")
library(renv)
renv::restore(prompt = FALSE)

# Load project configuration
source(here::here("R/___setup_project___.R"))
```

### Custom Helper Functions

**Include local figures** (pattern used across projects):
```r
include_local_figure <- function(data_source) {
  knitr::include_graphics(
    path = here::here("Materials", data_source),
    error = TRUE
  )
}
```

**Dynamic project names** (auto-derived from directory):
```r
project_name <- basename(here::here())
github_url <- paste0("https://github.com/OndrejMottl/", project_name)
```

## Data Management

### VegVault External Data

**Problem**: VegVault database is too large for git
**Solution**: Local path configuration via `.secrets/path.yaml`

```r
# From R/___setup_project___.R
if (file.exists(here::here(".secrets/path.yaml"))) {
  path_to_vegvault <- yaml::read_yaml(here::here(".secrets/path.yaml")) %>%
    purrr::chuck(Sys.info()["user"])
} else {
  warning("VegVault data path not specified. Create .secrets/path.yaml")
}
```

**Usage in analysis scripts**:
```r
data <- readr::read_rds(paste0(path_to_vegvault, "/data_file.rds"))
```

### Data Files

- **Format**: Primarily `.rds` (R data serialization)
- **Location**: `Data/` directory, organized by region/topic
- **Spatial data**: `Data/Spatial/Terrain/` for geographic layers
- **Naming**: Descriptive with date stamps when versioned

## Testing and Debugging

### Common Issues

1. **"Path to VegVault not specified"**: Create `.secrets/path.yaml` with your username and data path
2. **Font rendering issues**: Check `Fonts/` directory and `fonts-include.html` generation
3. **Theme not updating**: Delete `_colors.scss`, `_fonts.scss` and re-run `generate_theme.R`
4. **Package conflicts**: Run `renv::restore()` to sync with `renv.lock`

### Verification Steps

```r
# Check environment sync
renv::status()

# Test theme generation
source(here::here("R/generate_theme.R"))

# Preview presentation (opens in browser)
quarto::quarto_preview("presentation.qmd")
```

## Dependencies

### External Tools

- **Quarto CLI**: Required for rendering (install from quarto.org)
- **decktape** (optional): For PDF generation via `render.R`
  - Install: `npm install -g decktape`
  - Called via `decktape.cmd` on Windows

### R Package Ecosystem

Core packages (from `___setup_project___.R`):
- `here` - path management (critical for multi-root workspace)
- `renv` - package version management
- `tidyverse` - data wrangling and viz
- `knitr`, `quarto` - rendering
- `terra`, `geodata` - spatial analysis (in some projects)
- `jsonlite` - JSON theme config parsing

## Integration Points

### GitHub Pages Deployment

- **Source**: `docs/index.html` (Quarto renders to `Presentation/`, script copies to `docs/`)
- **Config**: Set GitHub Pages to serve from `docs/` directory on main branch
- **URL pattern**: `https://ondrejmottl.github.io/Project-Name-YYYYMMDD/`

### Cross-Project Patterns

All projects in this workspace follow identical patterns:
- Same `___setup_project___.R` structure (copy-paste with project name updated)
- Consistent `render.R` workflow
- Shared theme generation architecture
- Parallel folder structures (`R/Functions/`, `Materials/`, `Data/`)

When working across projects, patterns from one directly apply to others.

## Development Philosophy

- **Reproducibility first**: `renv`, explicit versions, locked environments
- **Automation**: JSON → SCSS generation, auto-sourcing functions, dynamic content
- **Separation of concerns**: Data prep (R scripts) → Results (saved to `Materials/R_generated/`) → Presentation (`.qmd`)
- **Self-documenting**: Consistent header comments, numbered script sections with `# -----`
