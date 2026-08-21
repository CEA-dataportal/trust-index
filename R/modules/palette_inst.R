##########################################################
#. COMMUNITY TRUST INDEX - INSTITUTIONAL COLOUR PALETTE
##########################################################

# ============================================================
# CTI Report - palette_inst.R
# Colour system for the Institutional module
#
# Purpose:
# - Define the module colour palette
# - Define shared colours expected by charts and custom_theme()
# - Define Institutional semantic colours
# ============================================================

# ---- 1. Module colours ----

color_primary_100   <- "#17284B"   # Competencies / main Institutional colour
color_primary_10    <- "#D1D4DB"

color_secondary_100 <- "#DF4747"   # Values / secondary Institutional colour
color_secondary_10  <- "#F9DADA"

color_tertiary_100  <- "#728BB5"
color_tertiary_10   <- "#E3E8F0"

color_quaternary_100 <- "#8893A5"
color_quaternary_10  <- "#E7E9ED"


# ---- 2. Neutral colours ----

color_grey        <- "#CFCFCF"
color_bg          <- "#EFEFEF"
color_label_bl    <- "#262626"
color_label_grey  <- "#666666"
color_label_White <- "#FFFFFF"


# ---- 3. Generic chart palettes ----

# Diverging scale used for Likert / response charts
color_scale <- c(
  color_primary_100,
  color_grey,
  color_secondary_100
)

# Two-colour gradients
color_gradient_1 <- c(
  color_primary_100,
  color_primary_10
)

color_gradient_2 <- c(
  color_secondary_100,
  color_secondary_10
)

# Four-colour module palette
palette_module <- c(
  color_primary_100,
  color_secondary_100,
  color_tertiary_100,
  color_quaternary_100
)


# ---- 4. Institutional semantic colours ----

color_competencies <- color_primary_100
color_values       <- color_secondary_100

palette_dimensions <- c(
  "Competency"   = color_competencies,
  "Competencies" = color_competencies,
  "Value"        = color_values,
  "Values"       = color_values
)


# ---- 5. Score / response palette ----

pal_score <- grDevices::colorRampPalette(color_scale)(5)


message("✓ Institutional colour palette loaded")
