##########################################################
# COMMUNITY TRUST INDEX - TABLES
# Multilingual version
#
# Requires translation_v2.R to be sourced beforehand:
#   tr()             -> template/common labels
#   tr_variable()    -> variable/question/dimension labels
#   tr_data()        -> categorical values / answers
#   tr_group_label() -> profile labels
##########################################################


# ------------------------------------------------------------
# Small table helpers
# ------------------------------------------------------------

# Translate a template key, but keep a readable fallback if the key
# has not yet been added to translation_v2.csv.
tr_table <- function(key, fallback) {
  value <- tr(key, warn_missing = FALSE)
  if (identical(value, key)) fallback else value
}

# Normalise categorical values to English for calculations.
# This allows source data to be EN / FR / ES while keeping the
# calculation logic independent from the report language.
to_canonical <- function(x) {
  tr_data(x, lang = "EN")
}


# ============================================================
# Gender table
# ============================================================

gender_canonical <- to_canonical(data$gender)

tab_gender <- data.frame(
  Gender = c("Female", "Male", "Other or did not answer"),
  Total = c(
    sum(gender_canonical == "Female", na.rm = TRUE),
    sum(gender_canonical == "Male", na.rm = TRUE),
    sum(
      !(gender_canonical %in% c("Female", "Male")) |
        is.na(gender_canonical)
    )
  )
)

tab_gender <- tab_gender %>%
  mutate(
    Percentage = round(Total / sum(Total) * 100, 1),
    Gender = tr_data(Gender)
  )

tab_gender <- bind_rows(
  tab_gender,
  data.frame(
    Gender = tr_table("common.total", "Total"),
    Total = sum(tab_gender$Total),
    Percentage = 100
  )
)

colnames(tab_gender) <- c(
  tr_variable("gender", "short", fallback = "Gender"),
  tr_table("common.total_respondents", "Total Respondents"),
  tr_table("common.percentage", "Percentage (%)")
)


# ============================================================
# Age group table
# ============================================================

tab_age_group <- data %>%
  group_by(Age_group) %>%
  summarise(Total = n(), .groups = "drop") %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1)) %>%
  arrange(Age_group)

tab_age_group <- bind_rows(
  tab_age_group,
  data.frame(
    Age_group = tr_table("common.total", "Total"),
    Total = sum(tab_age_group$Total),
    Percentage = 100
  )
)

colnames(tab_age_group) <- c(
  tr_table("common.age_group", "Age Group"),
  tr_table("common.total_respondents", "Total Respondents"),
  tr_table("common.percentage", "Percentage (%)")
)


# ============================================================
# Profile table
# ============================================================

profiles_core <- group_map %>%
  arrange(group_id) %>%
  pull(group_label)

# Technical object: keep stable names for downstream calculations.
tab_group <- group_map %>%
  rowwise() %>%
  mutate(
    `Total Respondents` = sum(
      to_canonical(data[[group_col]]) ==
        to_canonical(group_value),
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  transmute(
    Profile = group_label,
    `Total Respondents`
  )

# Display-only translated object.
tab_group_display <- tab_group %>%
  mutate(
    Profile = vapply(Profile, tr_group_label, character(1))
  )

colnames(tab_group_display) <- c(
  tr_table("common.profile", "Profile"),
  tr_table("common.total_respondents", "Total Respondents")
)


# ============================================================
# Geographic table
# ============================================================

# ------------------------------------------------------------
# Validate geographic variables
# ------------------------------------------------------------

valid_data_column <- function(column, dataset) {
  length(column) == 1 &&
    !is.na(column) &&
    nzchar(column) &&
    column %in% names(dataset)
}

has_adm1 <- valid_data_column(adm1, data)
has_adm2 <- valid_data_column(adm2, data)

if (has_adm2) {
  has_adm2 <- any(
    !is.na(data[[adm2]]) &
      trimws(as.character(data[[adm2]])) != ""
  )
}

has_locality <- valid_data_column(locality, data)

if (has_locality) {
  has_locality <- any(
    !is.na(data[[locality]]) &
      trimws(as.character(data[[locality]])) != ""
  )
}

if (!has_adm1) {
  stop(
    paste0(
      "The adm1 variable is missing, empty, or does not correspond ",
      "to a column in the dataset."
    )
  )
}


# ------------------------------------------------------------
# Prepare geographic data
# ------------------------------------------------------------

geo_data <- data %>%
  filter(
    !is.na(.data[[adm1]]),
    trimws(as.character(.data[[adm1]])) != ""
  )

if (has_adm2 && has_locality) {
  
  table_locality <- geo_data %>%
    filter(
      !is.na(.data[[adm2]]),
      trimws(as.character(.data[[adm2]])) != "",
      !is.na(.data[[locality]]),
      trimws(as.character(.data[[locality]])) != ""
    ) %>%
    group_by(
      .data[[adm1]],
      .data[[adm2]],
      .data[[locality]]
    ) %>%
    summarise(
      Total = n(),
      .groups = "drop"
    )
  
} else if (has_adm2) {
  
  table_locality <- geo_data %>%
    filter(
      !is.na(.data[[adm2]]),
      trimws(as.character(.data[[adm2]])) != ""
    ) %>%
    group_by(
      .data[[adm1]],
      .data[[adm2]]
    ) %>%
    summarise(
      Total = n(),
      .groups = "drop"
    )
  
} else if (has_locality) {
  
  table_locality <- geo_data %>%
    filter(
      !is.na(.data[[locality]]),
      trimws(as.character(.data[[locality]])) != ""
    ) %>%
    group_by(
      .data[[adm1]],
      .data[[locality]]
    ) %>%
    summarise(
      Total = n(),
      .groups = "drop"
    )
  
} else {
  
  table_locality <- geo_data %>%
    group_by(.data[[adm1]]) %>%
    summarise(
      Total = n(),
      .groups = "drop"
    )
}

table_locality <- table_locality %>%
  arrange(
    .data[[adm1]],
    desc(Total)
  )


# ------------------------------------------------------------
# Calculate percentages within adm1
# ------------------------------------------------------------

region_totals <- table_locality %>%
  group_by(.data[[adm1]]) %>%
  summarise(
    region_total = sum(Total),
    .groups = "drop"
  )

table_locality <- table_locality %>%
  left_join(
    region_totals,
    by = adm1
  ) %>%
  mutate(
    Percentage = round(Total / region_total * 100, 1)
  ) %>%
  select(-region_total)


# ------------------------------------------------------------
# Create total row for each adm1
# Keep a technical marker until sorting is complete.
# ------------------------------------------------------------

if (has_adm2 && has_locality) {
  
  region_totals_rows <- table_locality %>%
    group_by(.data[[adm1]]) %>%
    summarise(
      !!adm2 := "TOTAL",
      !!locality := "TOTAL",
      Total = sum(Total),
      Percentage = 100,
      .groups = "drop"
    )
  
} else if (has_adm2) {
  
  region_totals_rows <- table_locality %>%
    group_by(.data[[adm1]]) %>%
    summarise(
      !!adm2 := "TOTAL",
      Total = sum(Total),
      Percentage = 100,
      .groups = "drop"
    )
  
} else if (has_locality) {
  
  region_totals_rows <- table_locality %>%
    group_by(.data[[adm1]]) %>%
    summarise(
      !!locality := "TOTAL",
      Total = sum(Total),
      Percentage = 100,
      .groups = "drop"
    )
  
} else {
  
  region_totals_rows <- table_locality %>%
    group_by(.data[[adm1]]) %>%
    summarise(
      Total = sum(Total),
      Percentage = 100,
      .groups = "drop"
    )
}


# ------------------------------------------------------------
# Combine detail rows and totals
# ------------------------------------------------------------

tab_geo <- bind_rows(
  table_locality,
  region_totals_rows
)

if (has_locality) {
  
  tab_geo <- tab_geo %>%
    mutate(
      .is_total = .data[[locality]] == "TOTAL"
    ) %>%
    arrange(
      .data[[adm1]],
      desc(.is_total),
      desc(Total)
    ) %>%
    select(-.is_total)
  
} else if (has_adm2) {
  
  tab_geo <- tab_geo %>%
    mutate(
      .is_total = .data[[adm2]] == "TOTAL"
    ) %>%
    arrange(
      .data[[adm1]],
      desc(.is_total),
      desc(Total)
    ) %>%
    select(-.is_total)
  
} else {
  
  tab_geo <- tab_geo %>%
    arrange(.data[[adm1]])
}


# ------------------------------------------------------------
# Translate TOTAL display values after sorting
# ------------------------------------------------------------

total_label <- tr_table("common.total", "Total")

if (has_adm2) {
  tab_geo[[adm2]] <- ifelse(
    tab_geo[[adm2]] == "TOTAL",
    total_label,
    as.character(tab_geo[[adm2]])
  )
}

if (has_locality) {
  tab_geo[[locality]] <- ifelse(
    tab_geo[[locality]] == "TOTAL",
    total_label,
    as.character(tab_geo[[locality]])
  )
}


# ------------------------------------------------------------
# Geographic variable labels
# ------------------------------------------------------------

geo_label <- function(variable) {
  translated <- tr_variable(
    variable,
    "short",
    fallback = variable
  )
  
  if (
    length(translated) == 0L ||
    is.na(translated) ||
    !nzchar(trimws(translated))
  ) {
    return(variable)
  }
  
  translated
}


# ------------------------------------------------------------
# Format column names
# ------------------------------------------------------------

column_names <- c(
  geo_label(adm1),
  if (has_adm2) geo_label(adm2) else NULL,
  if (has_locality) geo_label(locality) else NULL,
  tr_table("common.total_respondents", "Total Respondents"),
  tr_table("common.percentage", "Percentage (%)")
)

colnames(tab_geo) <- column_names


# ------------------------------------------------------------
# Create translated caption
# ------------------------------------------------------------

geo_labels <- c(
  if (has_locality) geo_label(locality) else NULL,
  if (has_adm2) geo_label(adm2) else NULL,
  geo_label(adm1)
)

if (length(geo_labels) == 1L) {
  
  caption_text <- paste(
    tr_table("table.respondents_by", "Respondents by"),
    geo_labels
  )
  
} else {
  
  last_label <- tail(geo_labels, 1)
  first_labels <- head(geo_labels, -1)
  
  caption_text <- paste0(
    tr_table("table.respondents_by", "Respondents by"),
    " ",
    paste(first_labels, collapse = ", "),
    " ",
    tr_table("common.and", "and"),
    " ",
    last_label
  )
}