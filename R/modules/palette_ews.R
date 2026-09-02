##########################################################
#. COMMUNITY TRUST INDEX - EWS COLOUR PALETTE
##########################################################

# ============================================================
# CTI Report - palette_ews.R
# Colour system for the Early Warning System module
#
# Purpose:
# - Define the module colour palette
# - Define shared colours expected by charts and custom_theme()
# - Define semantic colours for the four EWS pillars
# ============================================================

# ---- 1. Module colours ----

color_primary_100   <- "#105CAA"   # Pillar 1
color_primary_10    <- "#D6E4F2"

color_secondary_100 <- "#0EB5D5"   # Pillar 2
color_secondary_10  <- "#D3F2F8"

color_tertiary_100  <- "#FAA61B"   # Pillar 3
color_tertiary_10   <- "#FDEBCB"

color_quaternary_100 <- "#529B3A"  # Pillar 4
color_quaternary_10  <- "#DFECD9"


# ---- 2. Neutral colours ----

color_grey        <- "#CFCFCF"
color_bg          <- "#EFEFEF"
color_label_bl    <- "#262626"
color_label_grey  <- "#666666"
color_label_White <- "#FFFFFF"


# ---- 3. Generic chart palettes ----

# Diverging scale used for Likert / response charts
# Kept consistent with the CTI response logic:
# positive -> neutral -> negative
color_scale <- c(
  color_primary_100,
  color_grey,
  color_tertiary_100
)

# Two-colour gradients
color_gradient_1 <- c(
  color_primary_100,
  color_primary_10
)

color_gradient_2 <- c(
  color_tertiary_100,
  color_tertiary_10
)

# Four-colour EWS palette
palette_module <- c(
  color_primary_100,
  color_secondary_100,
  color_tertiary_100,
  color_quaternary_100
)

# Gender EWS palette
color_female_100  <- color_tertiary_100
color_female_10 <- color_tertiary_10
color_male_100  <- color_primary_100
color_male_10 <- color_primary_10

# ---- 4. EWS semantic colours ----

color_pillar1 <- color_primary_100
color_pillar2 <- color_secondary_100
color_pillar3 <- color_tertiary_100
color_pillar4 <- color_quaternary_100

palette_pillars <- c(
  "Pillar 1" = color_pillar1,
  "Pillar 2" = color_pillar2,
  "Pillar 3" = color_pillar3,
  "Pillar 4" = color_pillar4
)


# ---- 5. Score / response palette ----

pal_score <- grDevices::colorRampPalette(color_scale)(5)


message("✓ EWS colour palette loaded")
