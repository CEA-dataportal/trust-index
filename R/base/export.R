##########################################################
#.  COMMUNITY TRUST INDEX - EXPORT
##########################################################

# ============================================================
# CTI Report - export.R
# Database-ready export + microdata export
# Shared by all CTI modules; output is controlled by config.
# ============================================================

message("Exporting report data...")

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

is_valid_tbl <- function(x) {
  !is.null(x) &&
    (is.data.frame(x) || tibble::is_tibble(x)) &&
    nrow(x) > 0
}

cfg <- function(name, default = NULL) {
  
  # Configuration values must come from objects created in the report
  # environment, not from attached packages (e.g. lubridate::year).
  if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    return(default)
  }
  
  x <- get(name, envir = .GlobalEnv, inherits = FALSE)
  
  # Reject unusable values and functions before subsetting.
  if (is.null(x) || is.function(x) || length(x) == 0L) {
    return(default)
  }
  
  # Use the first configured value when vectors are supplied.
  x <- x[[1]]
  
  if (is.na(x)) {
    return(default)
  }
  
  if (is.character(x) && !nzchar(trimws(x))) {
    return(default)
  }
  
  x
}

subset_postfix <- as.character(cfg("subset_postfix", ""))

# These should be defined in the Excel configuration.
# Examples:
# module_export_name  = Institutional / Early Warning
# overall_dimension   = INST INDEX / EWS INDEX
# overall_export_name = Institutional / Early Warning
# report_year         = 2025
module_export_name <- as.character(cfg("module_export_name", cfg("module", "")))
overall_dimension  <- as.character(cfg("overall_dimension", NA_character_))
overall_export_name <- as.character(cfg("overall_export_name", module_export_name))
report_year_export <- suppressWarnings(as.numeric(cfg("report_year", cfg("year", format(Sys.Date(), "%Y")))))
if (is.na(report_year_export)) report_year_export <- as.numeric(format(Sys.Date(), "%Y"))

# Build a generic dimension-code -> dimension-label map when available.
dimension_map <- character(0)
if (exists("score_prefixes", inherits = TRUE) &&
    exists("score_dimensions", inherits = TRUE)) {
  sp <- get("score_prefixes", inherits = TRUE)
  sd <- get("score_dimensions", inherits = TRUE)
  if (!is.null(names(sp)) && length(sp) == length(sd)) {
    dimension_map <- stats::setNames(as.character(sd), names(sp))
  }
}

clean_dimension <- function(x) {
  x <- as.character(x)
  if (length(dimension_map) > 0) {
    mapped <- unname(dimension_map[x])
    x <- ifelse(!is.na(mapped), mapped, x)
  }
  dplyr::recode(
    x,
    "Competency" = "Competencies",
    "Value" = "Values",
    .default = x
  )
}


# ------------------------------------------------------------
# Supabase helper
# ------------------------------------------------------------

export_to_supabase <- function(
    data,
    table,
    conflict_cols = c("country", "module", "year", "data_group", "name", "label", "series"),
    chunk_size = 500
) {
  
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for Supabase export.")
  }
  
  supabase_url <- Sys.getenv("SUPABASE_URL")
  supabase_key <- Sys.getenv("SUPABASE_SECRET_KEY")
  
  if (!nzchar(supabase_url)) {
    stop("SUPABASE_URL is not defined in .Renviron / environment variables.")
  }
  
  if (!nzchar(supabase_key)) {
    stop("SUPABASE_SECRET_KEY is not defined in .Renviron / environment variables.")
  }
  
  if (is.null(table) || !nzchar(trimws(table))) {
    stop("Supabase table name is missing. Set 'supabase_table' in the Excel configuration.")
  }
  
  if (!is_valid_tbl(data)) {
    message("No rows to send to Supabase.")
    return(invisible(FALSE))
  }
  
  # Supabase/Postgres columns are lowercase.
  # Keep `Group` in the Google/CSV export, but rename it to
  # `data_group` only for the Supabase payload.
  payload <- data %>%
    dplyr::rename(data_group = Group) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character)
    )
  
  conflict_cols <- tolower(trimws(conflict_cols))
  conflict_cols <- conflict_cols[nzchar(conflict_cols)]
  
  missing_conflict_cols <- setdiff(conflict_cols, names(payload))
  if (length(missing_conflict_cols) > 0) {
    stop(
      "Supabase conflict columns missing from export: ",
      paste(missing_conflict_cols, collapse = ", ")
    )
  }
  
  
  # Remove fully identical rows first.
  payload <- payload %>%
    dplyr::distinct()
  
  # Ensure the columns used by ON CONFLICT uniquely identify each row
  # within the same Supabase request.
  duplicate_keys <- payload %>%
    dplyr::count(
      dplyr::across(dplyr::all_of(conflict_cols)),
      name = "n"
    ) %>%
    dplyr::filter(n > 1)
  
  if (nrow(duplicate_keys) > 0) {
    
    duplicate_rows <- payload %>%
      dplyr::semi_join(
        duplicate_keys,
        by = conflict_cols
      ) %>%
      dplyr::arrange(
        dplyr::across(dplyr::all_of(conflict_cols))
      )
    
    print(duplicate_rows, n = 30)
    
    stop(
      "Supabase export contains ",
      nrow(duplicate_keys),
      " duplicated conflict key(s). ",
      "See the rows printed above."
    )
  }
  
  endpoint <- paste0(
    sub("/+$", "", supabase_url),
    "/rest/v1/",
    table
  )
  
  row_groups <- split(
    seq_len(nrow(payload)),
    ceiling(seq_len(nrow(payload)) / chunk_size)
  )
  
  for (i in seq_along(row_groups)) {
    
    payload_chunk <- payload[row_groups[[i]], , drop = FALSE]
    
    req <- httr2::request(endpoint) %>%
      httr2::req_url_query(
        on_conflict = paste(conflict_cols, collapse = ",")
      ) %>%
      httr2::req_headers(
        apikey = supabase_key,
        Authorization = paste("Bearer", supabase_key),
        Prefer = "resolution=merge-duplicates,return=minimal"
      ) %>%
      httr2::req_body_json(
        payload_chunk,
        auto_unbox = TRUE,
        null = "null"
      ) %>%
      httr2::req_method("POST")
    # Clean non-finite numeric values before JSON serialisation.
    payload_chunk <- payload_chunk %>%
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ dplyr::if_else(is.finite(.x), .x, NA_real_)
        )
      )
    
    message(
      "Supabase chunk ", i, "/", length(row_groups),
      ": ", nrow(payload_chunk), " rows"
    )
    
    # Do not let httr2 stop automatically on HTTP errors.
    # We inspect the Supabase/PostgREST response body ourselves so that
    # database errors are visible in the report log.
    resp <- tryCatch(
      req %>%
        httr2::req_error(is_error = function(resp) FALSE) %>%
        httr2::req_perform(),
      error = function(e) {
        stop(
          "Supabase request failed before receiving a response for chunk ",
          i, "/", length(row_groups), ": ",
          conditionMessage(e)
        )
      }
    )
    
    status <- httr2::resp_status(resp)
    
    if (status < 200 || status >= 300) {
      
      response_body <- tryCatch(
        httr2::resp_body_string(resp),
        error = function(e) "<unable to read response body>"
      )
      
      stop(
        "Supabase export failed for chunk ", i,
        "/", length(row_groups),
        " - HTTP ", status, "
",
"Response: ", response_body
      )
    }
  }
  
  message(
    "✓ Supabase upsert: ",
    nrow(payload),
    " rows -> ",
    table
  )
  
  invisible(TRUE)
}

# ============================================================
# 1. DATABASE-READY EXPORT
#    Long table suitable for Google Sheets / Supabase import
# ============================================================

db_parts <- list()

# ---- Overall, dimensions and drivers ------------------------
if (exists("summary_2", inherits = TRUE) &&
    is_valid_tbl(get("summary_2", inherits = TRUE))) {
  
  scores <- get("summary_2", inherits = TRUE)
  
  # Default empty object so downstream profile export is always safe
  # even when no overall/index row is available.
  overall_row <- tibble::tibble()
  
  # Overall score: configuration tells the script which Dimension is the index.
  if (!is.na(overall_dimension) &&
      all(c("Dimension", "Drivers", "Overall") %in% names(scores))) {
    
    overall_row <- scores %>%
      dplyr::filter(Dimension == overall_dimension) %>%
      dplyr::slice(1)
    
    if (nrow(overall_row) > 0) {
      db_parts[["overall"]] <- overall_row %>%
        dplyr::transmute(
          Country = country_name,
          Module = module_export_name,
          Year = report_year_export,
          Group = "Overall",
          Name = overall_export_name,
          Label = overall_export_name,
          Series = "",
          Value = as.numeric(Overall)
        )
    }
  }
  
  # Dimensions
  dimension_rows <- scores %>%
    dplyr::filter(Drivers == "Overall")
  
  if (!is.na(overall_dimension)) {
    dimension_rows <- dimension_rows %>%
      dplyr::filter(Dimension != overall_dimension)
  }
  
  if (nrow(dimension_rows) > 0 && "Overall" %in% names(dimension_rows)) {
    db_parts[["dimensions"]] <- dimension_rows %>%
      dplyr::transmute(
        Country = country_name,
        Module = module_export_name,
        Year = report_year_export,
        Group = "Dimension",
        Name = clean_dimension(Dimension),
        Label = clean_dimension(Dimension),
        Series = "",
        Value = as.numeric(Overall)
      )
  }
  
  # Profiles + Drivers
  # Profile columns are expected to follow the existing score naming pattern,
  # e.g. "Grp1: Volunteers", "Grp2: People receiving support".
  if (all(c("Dimension", "Drivers") %in% names(scores))) {
    
    value_cols <- setdiff(names(scores), c("Dimension", "Drivers"))
    profile_cols <- setdiff(value_cols, "Overall")
    
    # Profiles: use the overall module/index row.
    # Name = Grp1 / Grp2 / ... ; Label = human-readable profile name.
    if (nrow(overall_row) > 0 && length(profile_cols) > 0) {
      
      profile_long <- overall_row %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(profile_cols),
          names_to = "Profile",
          values_to = "Value"
        ) %>%
        dplyr::mutate(
          Value = suppressWarnings(as.numeric(Value)),
          profile_name = trimws(sub(":.*$", "", Profile)),
          profile_label = dplyr::if_else(
            grepl(":", Profile),
            trimws(sub("^[^:]*:", "", Profile)),
            profile_name
          )
        ) %>%
        dplyr::filter(!is.na(Value))
      
      if (nrow(profile_long) > 0) {
        db_parts[["profiles"]] <- profile_long %>%
          dplyr::transmute(
            Country = country_name,
            Module = module_export_name,
            Year = report_year_export,
            Group = "Profile",
            Name = profile_name,
            Label = profile_label,
            Series = "",
            Value = Value
          )
      }
    }
    
    # Drivers: Series must be the readable profile label, not Grp1/Grp2.
    drivers_long <- scores %>%
      dplyr::filter(Drivers != "Overall")
    
    if (!is.na(overall_dimension)) {
      drivers_long <- drivers_long %>%
        dplyr::filter(Dimension != overall_dimension)
    }
    
    drivers_long <- drivers_long %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(value_cols),
        names_to = "SerieRaw",
        values_to = "Value"
      ) %>%
      dplyr::mutate(
        Value = suppressWarnings(as.numeric(Value)),
        Serie = dplyr::case_when(
          SerieRaw == "Overall" ~ "Overall",
          grepl(":", SerieRaw) ~ trimws(sub(":.*$", "", SerieRaw)),
          TRUE ~ as.character(SerieRaw)
        )
      ) %>%
      dplyr::filter(!is.na(Value))
    
    if (nrow(drivers_long) > 0) {
      db_parts[["drivers"]] <- drivers_long %>%
        dplyr::transmute(
          Country = country_name,
          Module = module_export_name,
          Year = report_year_export,
          Group = "Drivers",
          Name = clean_dimension(Dimension),
          Label = as.character(Drivers),
          Series = Serie,
          Value = Value
        )
    }
  }
  
}

# ---- Factors and geographic scores -------------------------
if (exists("means_df", inherits = TRUE) &&
    is_valid_tbl(get("means_df", inherits = TRUE))) {
  
  geo_short_labels <- character(0)
  
  if (exists("question_code", inherits = TRUE) &&
      is_valid_tbl(get("question_code", inherits = TRUE))) {
    geo_short_labels <- get("question_code", inherits = TRUE) %>%
      dplyr::filter(tolower(category) == "geographic") %>%
      dplyr::pull(short_label) %>%
      gsub("\\n", " ", .) %>%
      tools::toTitleCase() %>%
      tolower()
  }
  
  means_export <- get("means_df", inherits = TRUE) %>%
    dplyr::filter(
      !is.na(variable_value),
      variable_value != 0
    ) %>%
    dplyr::mutate(
      clean_variable = tools::toTitleCase(gsub("\\n", " ", variable)),
      is_geographic = tolower(clean_variable) %in% geo_short_labels
    )
  
  # Keep only ADM1 for geographic results
  means_export <- means_export %>%
    dplyr::filter(
      !is_geographic |
        tolower(variable) == "adm1"
    )
  
  db_parts[["factors"]] <- means_export %>%
    dplyr::transmute(
      Country = country_name,
      Module = module_export_name,
      Year = report_year_export,
      Group = dplyr::if_else(is_geographic, "Geographic", "Factor"),
      Name = clean_variable,
      Label = as.character(variable_value),
      Series = clean_dimension(dimension),
      Value = as.numeric(mean)
    )
}

# ---- Sampling: age ------------------------------------------
if (exists("tab_age_group", inherits = TRUE) &&
    is_valid_tbl(get("tab_age_group", inherits = TRUE)) &&
    all(c("Age Group", "Total Respondents") %in% names(tab_age_group))) {
  
  db_parts[["sampling_age"]] <- tab_age_group %>%
    dplyr::transmute(
      Country = country_name,
      Module = module_export_name,
      Year = report_year_export,
      Group = "Sampling",
      Name = "Age group",
      Label = as.character(`Age Group`),
      Series = "",
      Value = as.numeric(`Total Respondents`)
    )
}

# ---- Sampling: gender ---------------------------------------
if (exists("tab_gender", inherits = TRUE) &&
    is_valid_tbl(get("tab_gender", inherits = TRUE)) &&
    all(c("Gender", "Total Respondents") %in% names(tab_gender))) {
  
  db_parts[["sampling_gender"]] <- tab_gender %>%
    dplyr::filter(!is.na(Gender), Gender != "") %>%
    dplyr::transmute(
      Country = country_name,
      Module = module_export_name,
      Year = report_year_export,
      Group = "Sampling",
      Name = "Demographic",
      Label = as.character(Gender),
      Series = "",
      Value = as.numeric(`Total Respondents`)
    )
}

# ---- Sampling: geography ------------------------------------
if (exists("tab_geo", inherits = TRUE) &&
    is_valid_tbl(get("tab_geo", inherits = TRUE)) &&
    "Total Respondents" %in% names(tab_geo)) {
  
  # `tab_geo` is expected to have been prepared upstream using the
  # geographic level selected by the Excel configuration.
  geo_label_col <- names(tab_geo)[1]
  
  db_parts[["sampling_geo"]] <- tab_geo %>%
    dplyr::filter(
      !is.na(.data[[geo_label_col]]),
      trimws(as.character(.data[[geo_label_col]])) != "",
      toupper(as.character(.data[[geo_label_col]])) != "TOTAL"
    ) %>%
    dplyr::transmute(
      Country = country_name,
      Module = module_export_name,
      Year = report_year_export,
      Group = "Sampling",
      Name = "Geographic",
      Label = as.character(.data[[geo_label_col]]),
      Series = "",
      Value = as.numeric(`Total Respondents`)
    )
}

# ---- Write database export ----------------------------------
if (length(db_parts) > 0) {
  output_database <- dplyr::bind_rows(db_parts) %>%
    dplyr::select(Country, Module, Year, Group, Name, Label, Series, Value) %>%
    dplyr::filter(!is.na(Value)) %>%
    dplyr::arrange(Country, Module, Year, Group, Name, Label, Series)
  
  database_export_file <- as.character(
    cfg(
      "database_export_file",
      paste0(country_iso, subset_postfix, "_database_export.csv")
    )
  )
  
  readr::write_csv(
    output_database,
    file.path(path, database_export_file),
    na = ""
  )
  
  message("✓ Database export: ", database_export_file)
  
  # Optional direct export to Supabase.
  # Controlled by the Excel configuration; credentials remain in .Renviron.
  export_supabase <- isTRUE(as.logical(cfg("export_supabase", FALSE)))
  
  if (export_supabase) {
    
    supabase_table <- as.character(
      cfg("supabase_table", "cti_results")
    )
    
    supabase_conflict_cols <- strsplit(
      as.character(
        cfg(
          "supabase_conflict_cols",
          "country,module,year,data_group,name,label,series"
        )
      ),
      ",",
      fixed = TRUE
    )[[1]]
    
    supabase_chunk_size <- suppressWarnings(
      as.integer(cfg("supabase_chunk_size", 500))
    )
    
    if (is.na(supabase_chunk_size) || supabase_chunk_size < 1) {
      supabase_chunk_size <- 500L
    }
    
    export_to_supabase(
      data = output_database,
      table = supabase_table,
      conflict_cols = supabase_conflict_cols,
      chunk_size = supabase_chunk_size
    )
  }
  
} else {
  warning("No database-ready data were available for export.")
}

# ============================================================
# 2. MICRODATA EXCEL EXPORT
# ============================================================

if (exists("data", inherits = TRUE) &&
    is_valid_tbl(get("data", inherits = TRUE))) {
  
  microdata <- get("data", inherits = TRUE)
  
  # Add report metadata without overwriting existing fields.
  if (!"Country" %in% names(microdata)) {
    microdata <- microdata %>%
      dplyr::mutate(Country = country_name, .before = 1)
  }
  
  if (!"Module" %in% names(microdata)) {
    microdata <- microdata %>%
      dplyr::mutate(Module = module_export_name, .after = Country)
  }
  
  if (!"Year" %in% names(microdata)) {
    microdata <- microdata %>%
      dplyr::mutate(Year = report_year_export, .after = Module)
  }
  
  microdata_sheets <- list(Microdata = microdata)
  
  # Keep the codebook with the microdata when available.
  if (exists("question_code", inherits = TRUE) &&
      is_valid_tbl(get("question_code", inherits = TRUE))) {
    microdata_sheets[["Codebook"]] <- get("question_code", inherits = TRUE)
  }
  
  microdata_export_file <- as.character(
    cfg(
      "microdata_export_file",
      paste0(country_iso, subset_postfix, "_microdata.xlsx")
    )
  )
  
  writexl::write_xlsx(
    microdata_sheets,
    path = file.path(path, microdata_export_file)
  )
  
  message("✓ Microdata export: ", microdata_export_file)
} else {
  warning("Analysis dataset `data` is empty or unavailable; microdata export not created.")
}

message("✓ Report data exported")
