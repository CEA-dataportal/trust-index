##########################################################
#. COMMUNITY TRUST INDEX - EWS MODULE CONFIG
##########################################################

# ============================================================
# CTI Report - config_ews.R
# Module-specific configuration for the Early Warning System module
#
# Load this script before R/base/read_config.R.
# ============================================================

cti_module_config <- list(
  code       = "EWS",
  name       = "Early Warning",
  index_name = "EWS INDEX",

  score_categories = c(
    "disaster",
    "detection",
    "dissemination",
    "response"
  ),

  score_dimensions = c(
    "Disaster",
    "Detection",
    "Dissemination",
    "Response"
  ),

  prefixes = list(
    # Kept for compatibility with the current EWS Rmd
    prefix_comp          = "COMP",
    prefix_val           = "VAL",

    # EWS score pillars
    prefix_disaster      = "DISASTER",
    prefix_detection     = "DETECTION",
    prefix_dissemination = "WARNING",
    prefix_response      = "ACTION",

    # EWS contextual questions
    prefix_ews           = "EWS",
    prefix_exp           = "BEFORE",
    prefix_hazard        = "hazard"
  )
)

message("✓ EWS module configuration loaded")
