
##########################################################
#.  COMMUNITY TRUST INDEX - SETUP
##########################################################

# ============================================================
# CTI Report - setup.R
#
# Purpose:
# - Load required packages
# - Set global R / knitr options
# - Define the shared ggplot theme
# - Define small reusable helper functions

# ============================================================

# ---- 1. Global options ----

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  encoding = "UTF-8"
)

if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(
    echo = FALSE,
    warning = FALSE,
    message = FALSE,
    results = "hide"
  )
}

# ---- 2. Package management ----

load_packages <- function(packages) {
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman")
  }
  
  pacman::p_load(
    char = packages,
    install = TRUE,
    character.only = TRUE
  )
}

required_packages <- c(
  "devtools",
  "conflicted",
  "httr",
  "sp",
  "gridExtra",
  "grid",
  "ggstance",
  "RSQLite",
  "cowplot",
  "reshape2",
  "ggplot2",
  "weights",
  "readxl",
  "broom",
  "boot",
  "HH",
  "jsonlite",
  "patchwork",
  "formattable",
  "lubridate",
  "lemon",
  "rlang",
  "forcats",
  "tidyverse",
  "kableExtra",
  "knitr",
  "stringr",
  "sf",
  "plotly",
  "spdep",
  "extrafont",
  "XML",
  "tibble",
  "tmap",
  "dplyr",
  "tidyr",
  "tidyselect",
  "purrr",
  "corrplot",
  "FactoMineR",
  "tinytex",
  "rmarkdown"
)

load_packages(required_packages)

# ---- 3. Optional fonts ----

if (requireNamespace("extrafont", quietly = TRUE)) {
  suppressMessages(
    try(extrafont::loadfonts(quiet = TRUE), silent = TRUE)
  )
}

# ---- 4. Conflict management ----

if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("mutate", "dplyr")
  conflicted::conflict_prefer("rename", "dplyr")
  conflicted::conflict_prefer("summarise", "dplyr")
  conflicted::conflict_prefer("arrange", "dplyr")
}

# ---- 5. Shared ggplot theme ----

custom_theme <- function() {
  ggplot2::theme(
    text = ggplot2::element_text(size = 16, family = "sans"),
    legend.position = "bottom",
    legend.box.margin = ggplot2::margin(5, 5, 5, 5),
    legend.text = ggplot2::element_text(size = 14),
    legend.background = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.spacing.x = grid::unit(1, "pt"),
    strip.background = ggplot2::element_rect(colour = color_bg),
    
    axis.ticks.y = ggplot2::element_line(
      color = "transparent"
    ),
    axis.ticks.x = ggplot2::element_line(
      color = "#CFCFCF"
    ),
    axis.title.x = ggplot2::element_text(color = color_label_bl, size = 16, margin = ggplot2::margin(t = 15, unit = "pt")),
    axis.title.y = ggplot2::element_text(color = color_label_bl, size = 16),
    axis.text.y = ggplot2::element_text(color = color_label_bl, size = 14),
    axis.text.x = ggplot2::element_text(
      color = color_label_bl,
      size = 14
    ),
    strip.text = ggplot2::element_text(color = color_label_bl, size = 16),
    
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16,
      vjust = 2,
      family = "sans"
    ),
    plot.subtitle = ggplot2::element_text(size = 12),
    plot.margin = ggplot2::margin(t = 10, r = 5, b = 10, l = 5),
    plot.caption = ggtext::element_markdown(
      size = 10, 
      hjust = 0,
      lineheight = 1.15,
      margin = margin(t = 12)
    ),
    plot.caption.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F8F8F8")
  )
}

# ---- 6. Standard exclusions used in charts ----

# These values are excluded from some sampling and demographic charts.
display_no <- c(
  "Don't know",
  "No data",
  "Prefer not to answer"
)

gender_no <- c(
  "Other",
  "No data",
  "Prefer not to say"
)

excluded_regions <- c(
  "Don't know",
  "No data",
  "Prefer not to answer"
)

# ---- 7. Helper functions ----

make_mapping <- function(df, object_name) {
  x <- df |>
    dplyr::filter(.data$map == object_name)
  
  if (all(is.na(x$to) | x$to == "")) {
    return(x$from)
  }
  
  stats::setNames(x$to, x$from)
}

clean_name_safe <- function(x) {
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove_all("^_|_$")
}

capitalize_first <- function(x) {
  vapply(
    x,
    function(s) {
      if (is.na(s) || nchar(s) == 0) return(s)
      paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
    },
    character(1),
    USE.NAMES = FALSE
  )
}

clean_dimension <- function(x) {
  dplyr::recode(
    x,
    "Competency" = "Competencies",
    "Value" = "Values",
    .default = x
  )
}

safe_read_xlsx <- function(path, sheet = 1, ...) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  
  readxl::read_xlsx(path, sheet = sheet, ...)
}

make_subset_postfix <- function(subset_value) {
  if (is.null(subset_value) || is.na(subset_value) || subset_value == "") {
    return("")
  }
  
  paste0("_", gsub("[^A-Za-z0-9]", "_", subset_value))
}

message_info <- function(text) {
  message("ℹ ", text)
}

message_success <- function(text) {
  message("✓ ", text)
}

message_success("setup.R loaded")
