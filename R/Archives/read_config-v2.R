##########################################################
#. COMMUNITY TRUST INDEX - READ CONFIGURATION FILE
##########################################################

# ============================================================
# CTI Report - read_config.R
# Read configuration workbook and initialise report parameters
#
# Module-specific settings are provided by:
#   R/modules/config_inst.R
#   R/modules/config_ews.R
#
# The selected module script must be sourced before calling
# read_cti_config().
# ============================================================

message("read_config.R loaded")


# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

.is_blank_config_value <- function(x) {
  length(x) == 0L ||
    is.null(x) ||
    is.na(x[1L]) ||
    !nzchar(trimws(as.character(x[1L]))) ||
    toupper(trimws(as.character(x[1L]))) %in% c("NA", "NULL")
}


.validate_module_config <- function(module_config) {

  if (is.null(module_config) || !is.list(module_config)) {
    stop(
      "No valid CTI module configuration found. ",
      "Source R/modules/config_inst.R or R/modules/config_ews.R ",
      "before calling read_cti_config().",
      call. = FALSE
    )
  }

  required <- c(
    "code",
    "name",
    "index_name",
    "score_categories",
    "score_dimensions",
    "prefixes"
  )

  missing <- setdiff(required, names(module_config))

  if (length(missing) > 0L) {
    stop(
      "The module configuration is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.list(module_config$prefixes)) {
    stop("module_config$prefixes must be a named list.", call. = FALSE)
  }

  if (is.null(names(module_config$prefixes)) ||
      any(!nzchar(names(module_config$prefixes)))) {
    stop("All module prefixes must have names.", call. = FALSE)
  }

  invisible(TRUE)
}


# ------------------------------------------------------------
# Main configuration reader
# ------------------------------------------------------------

read_cti_config <- function(
    config_file,
    country_name,
    module_config = get0(
      "cti_module_config",
      envir = .GlobalEnv,
      inherits = TRUE,
      ifnotfound = NULL
    )
) {

  .validate_module_config(module_config)

  # ---- Paths ----

  path <- file.path("..", country_name)

  config_path <- file.path(path, config_file)

  if (!file.exists(config_path)) {
    stop(
      "Configuration file not found: ",
      normalizePath(config_path, mustWork = FALSE),
      call. = FALSE
    )
  }


  # ---- Parameters sheet ----

  params_df <- readxl::read_xlsx(
    config_path,
    sheet = "Parameters"
  ) |>
    dplyr::filter(
      !is.na(parameters),
      !grepl("^#", parameters)
    )

  params <- stats::setNames(
    lapply(
      params_df$values,
      function(x) {
        if (.is_blank_config_value(x)) {
          return(NULL)
        }

        x
      }
    ),
    params_df$parameters
  )


  # ---- Required parameters ----

  required_params <- c(
    "data_file",
    "country_iso"
  )

  missing_params <- required_params[
    !required_params %in% names(params) |
      vapply(
        required_params,
        function(x) .is_blank_config_value(params[[x]]),
        logical(1)
      )
  ]

  if (length(missing_params) > 0L) {
    stop(
      "Missing required parameter(s) in the Parameters sheet: ",
      paste(missing_params, collapse = ", "),
      call. = FALSE
    )
  }


  # ---- Optional geoname lookup ----

  geoname_survey <- params[["geoname_survey"]]


  # ---- Refresh parameters in global environment ----
  #
  # Existing variables with the same names are removed first so that
  # parameters set to blank/NULL in a new configuration do not retain
  # stale values from a previous report run.

  previous_parameter_names <- intersect(
    names(params_df$parameters),
    ls(envir = .GlobalEnv)
  )

  if (length(previous_parameter_names) > 0L) {
    rm(
      list = previous_parameter_names,
      envir = .GlobalEnv
    )
  }

  list2env(
    params,
    envir = .GlobalEnv
  )


  # ---- Module settings ----

  module_code       <- module_config$code
  module_name       <- module_config$name
  index_name        <- module_config$index_name
  score_categories  <- module_config$score_categories
  score_dimensions  <- module_config$score_dimensions
  module_prefixes   <- module_config$prefixes


  # ---- Global report settings ----

  date <- format(Sys.Date(), "%d%m%y")

  path_data_file <- file.path(
    path,
    params$data_file
  )

  path_archives <- "Archives"

  if (!dir.exists(path_archives)) {
    dir.create(
      path_archives,
      recursive = TRUE
    )
  }


  # ---- Optional subset postfix ----

  subset_postfix <- ""

  subset_value <- params[["subset_value"]]

  if (!.is_blank_config_value(subset_value)) {
    subset_postfix <- paste0(
      "_",
      gsub(
        "[^A-Za-z0-9]",
        "_",
        as.character(subset_value)
      )
    )
  }


  # ---- Module-specific output names ----

  html_output <- file.path(
    path,
    paste0(
      params$country_iso,
      subset_postfix,
      "_report_",
      module_code,
      ".html"
    )
  )

  export_file <- file.path(
    path,
    paste0(
      params$country_iso,
      subset_postfix,
      "_",
      module_code,
      "_export.xlsx"
    )
  )


  # ---- Answer mappings ----

  mapping_df <- readxl::read_xlsx(
    config_path,
    sheet = "Answer_mapping"
  )

  make_mapping_local <- function(df, object_name) {

    x <- dplyr::filter(
      df,
      map == object_name
    )

    if (nrow(x) == 0L) {
      return(character(0))
    }

    if (all(is.na(x$to) | x$to == "")) {
      return(x$from)
    }

    stats::setNames(
      x$to,
      x$from
    )
  }

  score_map          <- make_mapping_local(mapping_df, "score_map")
  answer_likertscale <- make_mapping_local(mapping_df, "answer_likertscale")
  answer_extra       <- make_mapping_local(mapping_df, "answer_extra")
  answer_exp         <- make_mapping_local(mapping_df, "answer_exp")
  answer_behaviours  <- make_mapping_local(mapping_df, "answer_behaviours")
  answer_intention   <- make_mapping_local(mapping_df, "answer_intention")
  answer_impact      <- make_mapping_local(mapping_df, "answer_impact")
  answer_yn_map      <- make_mapping_local(mapping_df, "answer_yn_map")
  gender_map         <- make_mapping_local(mapping_df, "gender_map")


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


  # ---- Return configuration ----

  base_config <- list(
    path = path,
    config_file = config_file,
    country_name = country_name,

    params = params,
    geoname_survey = geoname_survey,

    module_code = module_code,
    module_name = module_name,
    index_name = index_name,
    score_categories = score_categories,
    score_dimensions = score_dimensions,

    date = date,
    path_data_file = path_data_file,
    path_archives = path_archives,
    subset_postfix = subset_postfix,
    html_output = html_output,
    export_file = export_file,

    score_map = score_map,
    answer_likertscale = answer_likertscale,
    answer_extra = answer_extra,
    answer_exp = answer_exp,
    answer_behaviours = answer_behaviours,
    answer_impact = answer_impact,
    answer_intention = answer_intention,
    answer_yn_map = answer_yn_map,
    gender_map = gender_map,

    display_no = display_no,
    gender_no = gender_no,
    excluded_regions = excluded_regions
  )

  # Prefix variables are returned at the top level for compatibility
  # with the current analysis and chart scripts:
  #
  # INST: prefix_comp, prefix_val
  # EWS : prefix_disaster, prefix_detection, prefix_dissemination, etc.

  c(
    base_config,
    module_prefixes
  )
}
