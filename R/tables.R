




## Gender table

tab_gender <- data.frame(
  Gender = c("Female", "Male", "Other or did not answer"),
  Total = c(
    sum(data$gender == "Female", na.rm = TRUE),
    sum(data$gender == "Male", na.rm = TRUE),
    sum(!(data$gender %in% c("Female", "Male")) | is.na(data$gender))
  )
)

tab_gender <- tab_gender %>%
  mutate(
    Percentage = round(Total / sum(Total) * 100, 1)
  )

tab_gender <- rbind(
  tab_gender,
  data.frame(
    Gender = "Total",
    Total = sum(tab_gender$Total),
    Percentage = 100
  )
)

colnames(tab_gender) <- c("Gender", "Total Respondents", "Percentage (%)")


## Age group table

tab_age_group <- data %>%
  group_by(Age_group) %>%
  summarise(Total = n(), .groups = "drop") %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1)) %>%
  arrange(Age_group)

tab_age_group <- bind_rows(
  tab_age_group,
  data.frame(
    Age_group = "Total",
    Total = sum(tab_age_group$Total),
    Percentage = 100
  )
)

colnames(tab_age_group) <- c("Age Group", "Total Respondents", "Percentage (%)")


## Profile table

profiles_core <- group_map %>%
  arrange(group_id) %>%          # optional: enforce order by group_id
  pull(group_label) 


tab_group <- group_map %>%
  rowwise() %>% 
  mutate(
    `Total Répondants` = sum(data[[group_col]] == group_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    Profile = group_label,
    `Total Répondants`
  )

# ---- Original chunk: table_localities ----
## Geographic table

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

# adm1 is required for the geographic table
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
    group_by(
      .data[[adm1]]
    ) %>%
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
# Format column names
# ------------------------------------------------------------

capitalize_first <- function(x) {
  if (
    length(x) != 1 ||
    is.na(x) ||
    !nzchar(x)
  ) {
    return("")
  }
  
  paste0(
    toupper(substr(x, 1, 1)),
    tolower(substr(x, 2, nchar(x)))
  )
}

column_names <- c(
  capitalize_first(adm1),
  if (has_adm2) capitalize_first(adm2) else NULL,
  if (has_locality) capitalize_first(locality) else NULL,
  "Total Respondents",
  "Percentage (%)"
)

colnames(tab_geo) <- column_names


# ------------------------------------------------------------
# Create caption
# ------------------------------------------------------------

caption_text <- if (has_adm2 && has_locality) {
  
  paste0(
    "Respondents by ",
    capitalize_first(locality),
    ", ",
    capitalize_first(adm2),
    " and ",
    capitalize_first(adm1)
  )
  
} else if (has_adm2) {
  
  paste0(
    "Respondents by ",
    capitalize_first(adm2),
    " and ",
    capitalize_first(adm1)
  )
  
} else if (has_locality) {
  
  paste0(
    "Respondents by ",
    capitalize_first(locality),
    " and ",
    capitalize_first(adm1)
  )
  
} else {
  
  paste0(
    "Respondents by ",
    capitalize_first(adm1)
  )
}


