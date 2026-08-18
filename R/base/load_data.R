##########################################################
#.  COMMUNITY TRUST INDEX - DATA LOADING
##########################################################

# ============================================================
# CTI Report - load_data.R
# Load survey data, codebook, population tables and shapefiles
# Keeps current Rmd object names for compatibility
# ============================================================

message("Loading data...")

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

is_empty_tbl <- function(x) {
  is.null(x) ||
    nrow(x) == 0 ||
    ncol(x) == 0 ||
    all(is.na(x))
}

safe_read_excel <- function(file, sheet, required_cols = NULL) {

  if (is.null(file) || is.na(file) || !file.exists(file)) {
    return(NULL)
  }

  if (is.null(sheet) || is.na(sheet) || sheet == "") {
    sheet_names <- readxl::excel_sheets(file)
    sheet <- sheet_names[1]
    
    message(
      "No sheet specified for '", basename(file),
      "'. Loading the first worksheet: '", sheet, "'."
    )
  }

  out <- tryCatch(
    readxl::read_xlsx(file, sheet = sheet),
    error = function(e) NULL
  )

  if (is_empty_tbl(out)) {
    return(NULL)
  }

  if (!is.null(required_cols) &&
      !all(required_cols %in% colnames(out))) {
    return(NULL)
  }

  out
}

# ------------------------------------------------------------
# Default objects
# ------------------------------------------------------------

pop_demo <- NULL
population_geo <- NULL
pop_strata1 <- NULL
pop_strata2 <- NULL
pop_strata3 <- NULL
country_shp <- NULL
regions_shp <- NULL
districts_shp <- NULL

age_breaks <- c(18, 30, 40, 50, 60, 100)
age_labels <- c("18-29", "30-39", "40-49", "50-59", "60+")

# ------------------------------------------------------------
# Main survey data
# ------------------------------------------------------------

raw_data <- readxl::read_xlsx(
  file.path(path, data_file),
  sheet = data_tab
)

# Optional subset before analysis
if (exists("subset_field") &&
    exists("subset_value") &&
    !is.null(subset_field) &&
    !is.null(subset_value) &&
    !is.na(subset_field) &&
    !is.na(subset_value) &&
    subset_field != "" &&
    subset_value != "") {

  if (!(subset_field %in% names(raw_data))) {
    stop(paste0("Column '", subset_field, "' not found in dataset"))
  }

  if (!(subset_value %in% raw_data[[subset_field]])) {
    stop(paste0(
      "Value '", subset_value, "' not found in column '", subset_field, "'"
    ))
  }

  raw_data <- raw_data |>
    dplyr::filter(.data[[subset_field]] == subset_value)
}

# ------------------------------------------------------------
# Codebook / question bank
# ------------------------------------------------------------

question_code <- readxl::read_xlsx(
  file.path(path, code_file),
  sheet = code_tab
) |>
  dplyr::filter(!is.na(category) & category != "") |>
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "admin1" ~ adm1,
      variable == "admin2" ~ adm2,
      TRUE ~ variable
    )
  )

# ------------------------------------------------------------
# Population data
# ------------------------------------------------------------

pop_file_path <- file.path(path, pop_file)

# Demography
pop_demo <- safe_read_excel(
  file = pop_file_path,
  sheet = demo_tab
)

if (!is.null(pop_demo) && "AGEgroup" %in% names(pop_demo)) {

  AGECut <- unique(pop_demo$AGEgroup)

  lower_bounds <- sapply(AGECut, function(label) {
    if (grepl("\\+|>", label)) {
      as.numeric(gsub("[^0-9]", "", label))
    } else {
      as.numeric(strsplit(label, "-")[[1]][1])
    }
  })

  ord <- order(lower_bounds)

  AGECut <- AGECut[ord]
  lower_bounds <- lower_bounds[ord]

  has_open_ended <- any(grepl("\\+|>", AGECut))
  upper_bound <- if (has_open_ended) 100 else max(lower_bounds) + 10

  age_breaks <- c(lower_bounds, upper_bound)
  age_labels <- AGECut
}

# Age group in survey data
if (!"Age_group" %in% names(raw_data)) {
  raw_data <- raw_data |>
    dplyr::mutate(
      Age_group = cut(
        !!rlang::sym(age_col),
        breaks = age_breaks,
        labels = age_labels,
        include.lowest = TRUE,
        right = FALSE
      )
    )
}

demo_age <- pop_demo

# Geography population table
population_geo <- safe_read_excel(
  file = pop_file_path,
  sheet = geo_tab
)

# Strata 1
if (exists("strata1_tab") &&
    !is.null(strata1_tab) &&
    !is.na(strata1_tab)) {
  pop_strata1 <- safe_read_excel(
    file = pop_file_path,
    sheet = strata1_tab
  )
}

# Strata 2
if (
  exists("strata2_tab") &&
  !is.null(strata2_tab) &&
  length(strata2_tab) == 1 &&
  !is.na(strata2_tab) &&
  nzchar(trimws(as.character(strata2_tab)))
  )  {
  pop_strata2 <- safe_read_excel(
    file = pop_file_path,
    sheet = strata2_tab,
    required_cols = c("Level", "Percentage")
  )
}

# Strata 3
if (exists("strata3_tab") &&
    !is.null(strata3_tab) && 
    !is.na(strata3_tab)) {
  pop_strata3 <- safe_read_excel(
    file = pop_file_path,
    sheet = strata3_tab,
    required_cols = c("Level", "Percentage")
  )
}

# ------------------------------------------------------------
# Shapefiles
# ------------------------------------------------------------

if (exists("shp_folder") &&
    exists("adm0_file") &&
    !is.na(shp_folder) &&
    !is.na(adm0_file) &&
    file.exists(file.path(path, shp_folder, adm0_file))) {

  country_shp <- sf::st_read(
    file.path(path, shp_folder, adm0_file),
    quiet = TRUE
  )
}

if (exists("shp_folder") &&
    exists("adm1_file") &&
    !is.na(shp_folder) &&
    !is.na(adm1_file) &&
    file.exists(file.path(path, shp_folder, adm1_file))) {

  regions_shp <- sf::st_read(
    file.path(path, shp_folder, adm1_file),
    quiet = TRUE
  )
}

if (exists("shp_folder") &&
    exists("adm2_file") &&
    !is.na(shp_folder) &&
    !is.na(adm2_file) &&
    file.exists(file.path(path, shp_folder, adm2_file))) {

  districts_shp <- sf::st_read(
    file.path(path, shp_folder, adm2_file),
    quiet = TRUE
  )
}



# ------------------------------------------------------------
# Analysis dataset
# ------------------------------------------------------------

has_num_age <- "age" %in% names(raw_data) && is.numeric(raw_data[["age"]])

# Apply optional filter first
if (exists("column_name") &&
    exists("filter_value") &&
    !is.null(column_name) &&
    column_name != "" &&
    !is.null(filter_value) &&
    filter_value != "") {
  
  data <- raw_data |>
    dplyr::filter(!!rlang::sym(column_name) == filter_value)
  
} else {
  
  data <- raw_data
  
}

# Then apply age / Age_group filtering
if (has_num_age) {
  
  data <- data |>
    dplyr::filter(
      !is.na(Age_group),
      !is.na(age),
      age >= 18
    )
  
} else {
  
  data <- data |>
    dplyr::filter(
      !is.na(Age_group)
    )
  
}

# Geographic variables
if (all(c("admin1", "admin2") %in% names(data))) {
  data <- data |>
    dplyr::rename(
      !!adm1 := admin1,
      !!adm2 := admin2
    )
}

# Clean typographic apostrophes
data <- data |>
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      ~ gsub("’", "'", .)
    )
  )

# ------------------------------------------------------------
# Add loaded objects to config for future use
# ------------------------------------------------------------

if (exists("config", envir = .GlobalEnv, inherits = FALSE)) {
  
  config$age_breaks <- age_breaks
  config$age_labels <- age_labels
  config$path_data_file <- file.path(path, data_file)
  
  assign("config", config, envir = .GlobalEnv)
}

message("✓ Data loaded")
