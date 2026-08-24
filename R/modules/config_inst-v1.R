##########################################################
#. COMMUNITY TRUST INDEX - INSTITUTIONAL MODULE CONFIG
##########################################################

# ============================================================
# CTI Report - config_inst.R
# Module-specific configuration for the Institutional module
#
# Load this script before R/base/read_config.R.
# ============================================================

cti_module_config <- list(
  code       = "INST",
  name       = "Institutional",
  index_name = "INST INDEX",

  score_categories = c(
    "competency",
    "value"
  ),

  score_dimensions = c(
    "Competency",
    "Value"
  ),

  prefixes = list(
    prefix_comp = "COMP",
    prefix_val  = "VALUES"
  )
)

message("✓ Institutional module configuration loaded")
