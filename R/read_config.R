##########################################################
#.  COMMUNITY TRUST INDEX - READ CONFIGURATION FILE
##########################################################


# ============================================================
# CTI Report - read_config.R
# Read configuration workbook and initialise parameters
# ============================================================

message("read_config.R loaded")


read_cti_config <- function(config_file, country_name) {
  
  # ---- Paths ----
  path <- file.path("..", country_name)
  
  # ---- Parameters sheet ----
  params_df <- readxl::read_xlsx(
    file.path(path, config_file),
    sheet = "Parameters"
  ) |>
    dplyr::filter(
      !is.na(parameters),
      !grepl("^#", parameters)
    )
  
  params <- stats::setNames(
    lapply(params_df$values, function(x) {
      
      if (
        length(x) == 0 ||
        is.na(x) ||
        trimws(as.character(x)) == "" ||
        toupper(trimws(as.character(x))) %in% c("NA", "NULL")
      ) {
        return(NULL)
      }
      
      x
    }),
    params_df$parameters
  )
  
  # Optional geoname lookup
  geoname_survey <- NULL
  
  if ("geoname_survey" %in% names(params)) {
    geoname_survey <- params$geoname_survey
  }
  
  # Remove previous parameter variables
  rm(
    list = intersect(names(params), ls(envir = .GlobalEnv)),
    envir = .GlobalEnv
  )
  
  # Export fresh parameters
  list2env(
    params,
    envir = .GlobalEnv
  )
  
  # ---- Global report settings ----
  date <- format(Sys.Date(), "%d%m%y")
  
  prefix_comp <- "COMP"
  prefix_val  <- "VALUES"
  
  path_data_file <- file.path(path, data_file)
  
  path_archives <- "Archives"
  
  if (!dir.exists(path_archives)) {
    dir.create(path_archives, recursive = TRUE)
  }
  
  subset_postfix <- ""
  
  if (exists("subset_value") &&
      !is.null(subset_value) &&
      !is.na(subset_value) &&
      subset_value != "") {
    
    subset_postfix <- paste0(
      "_",
      gsub("[^A-Za-z0-9]", "_", subset_value)
    )
  }
  
  html_output <- file.path(
    path,
    paste0(
      country_iso,
      subset_postfix,
      "_report_INST.html"
    )
  )
  
  export_file <- file.path(
    path,
    paste0(
      country_iso,
      subset_postfix,
      "_INST_export.xlsx"
    )
  )
  
  # ---- Answer mappings ----
  
  mapping_df <- readxl::read_xlsx(
    file.path(path, config_file),
    sheet = "Answer_mapping"
  )
  
  make_mapping <- function(df, object_name) {
    
    x <- dplyr::filter(df, map == object_name)
    
    if (all(is.na(x$to) | x$to == "")) {
      return(x$from)
    }
    
    setNames(x$to, x$from)
  }
  
  score_map           <- make_mapping(mapping_df, "score_map")
  answer_likertscale  <- make_mapping(mapping_df, "answer_likertscale")
  answer_extra        <- make_mapping(mapping_df, "answer_extra")
  answer_exp          <- make_mapping(mapping_df, "answer_exp")
  answer_behaviours   <- make_mapping(mapping_df, "answer_behaviours")
  answer_intention    <- make_mapping(mapping_df, "answer_intention")
  answer_yn_map       <- make_mapping(mapping_df, "answer_yn_map")
  gender_map          <- make_mapping(mapping_df, "gender_map")
  
  # ---- Standard exclusions ----
  
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
  
  list(
    path = path,
    config_file = config_file,
    country_name = country_name,
    params = params,
    geoname_survey = geoname_survey,
    date = date,
    prefix_comp = prefix_comp,
    prefix_val = prefix_val,
    path_data_file = path_data_file,
    html_output = html_output,
    export_file = export_file,
    score_map = score_map,
    answer_likertscale = answer_likertscale,
    answer_extra = answer_extra,
    answer_exp = answer_exp,
    answer_behaviours = answer_behaviours,
    answer_intention = answer_intention,
    answer_yn_map = answer_yn_map,
    gender_map = gender_map,
    display_no = display_no,
    gender_no = gender_no,
    excluded_regions = excluded_regions
  )
}
