##########################################################
#.  COMMUNITY TRUST INDEX - CHARTS
##########################################################


# ============================================================
# CTI Report - charts.R
# All chart, visual table and chart-preparation chunks
# Extracted from Data-Report-INST.Rmd
# ============================================================

message("Loading charts and visual outputs...")

# This script assumes the following scripts have already run:
# source('R/setup.R')
# source('R/read_config.R')
# source('R/load_data.R')
# source('R/prepare_data.R')
# source('R/compute_score.R')


# ------------------------------------------------------------
# Report tables
# ------------------------------------------------------------

# ---- Original chunk: table_gender ----
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
knitr::kable(tab_gender, caption = "Respondents by Gender")

# ---- Original chunk: table_age ----
## Age group table

# Summary table by AGEgroup
tab_age_group <- data %>%
  group_by(Age_group) %>%
  summarise(Total = n(), .groups = "drop") %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1)) %>%
  arrange(Age_group)

# Display as table
colnames(tab_age_group) <- c("Age Group", "Total Respondents", "Percentage (%)")
knitr::kable(tab_age_group, caption = "Respondents by Age Group")

# ---- Original chunk: table_localities ----
## Geographic table

has_locality <- locality %in% names(data) && any(!is.na(data[[locality]]))
has_adm2 <- !is.null(adm2) && adm2 %in% names(data) && any(!is.na(data[[adm2]]))

table_locality <- data %>%
  filter(!is.na(.data[[adm1]])) %>%
  {
    if (has_adm2 && !all(is.na(.[[adm2]]))) {
      if (has_locality) {
        filter(., !is.na(.data[[adm2]]), !is.na(.data[[locality]])) %>%
          group_by(.data[[adm1]], .data[[adm2]], .data[[locality]])
      } else {
        filter(., !is.na(.data[[adm2]])) %>%
          group_by(.data[[adm1]], .data[[adm2]])
      }
    } else {
      if (has_locality) {
        filter(., !is.na(.data[[locality]])) %>%
          group_by(.data[[adm1]], .data[[locality]])
      } else {
        group_by(., .data[[adm1]])
      }
    }
  } %>%
  summarise(Total = n(), .groups = "drop") %>%
  arrange(.data[[adm1]], desc(Total))


region_totals <- table_locality %>%
  group_by(.data[[adm1]]) %>%
  summarise(region_total = sum(Total), .groups = "drop")


table_locality <- table_locality %>%
  left_join(region_totals, by = adm1) %>%
  mutate(Percentage = round(Total / region_total * 100, 1)) %>%
  select(-region_total)

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


tab_geo <- bind_rows(table_locality, region_totals_rows) %>%
  arrange(.data[[adm1]], desc(if (has_locality) .data[[locality]] == "TOTAL" else TRUE))


capitalize_first <- function(x) {
  sapply(x, function(s) {
    paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
  }, USE.NAMES = FALSE)
}

column_names <- c(
  capitalize_first(adm1),
  if (has_adm2) capitalize_first(adm2) else NULL,
  if (has_locality) capitalize_first(locality) else NULL,
  "Total Respondents",
  "Percentage (%)"
)
colnames(tab_geo) <- column_names

caption_text <- paste0(
  "Respondents by ",
  if (has_locality) capitalize_first(locality) else if (has_adm2) capitalize_first(adm2) else capitalize_first(adm1),
  if (has_adm2 && has_locality) paste0(", ", capitalize_first(adm2)) else "",
  " and ", capitalize_first(adm1)
)

knitr::kable(tab_geo, caption = caption_text)

# ---- Original chunk: tab_type ----
## Profile table

profiles_core <- group_map %>%
  arrange(group_id) %>%          # optional: enforce order by group_id
  pull(group_label) 


tab_group <- group_map %>%
  rowwise() %>% 
  mutate(
    `Total Respondents` = sum(data[[group_col]] == group_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  transmute(
    Profile = group_label,
    `Total Respondents`
  )


kable(tab_group, caption = "Respondents by relationship with RC")

# ---- Original chunk: weighting_new ----
## Weighting comparison chart

df_long <- df3 %>%
  reshape2::melt(
    id.vars      = c("Dimension", "Drivers"),  # keep both as identifiers
    variable.name = "variable",
    value.name    = "value"
  )

ggplot(df_long, aes(x = Drivers, y = value, group = variable, color = variable)) +
  geom_point() +
  geom_line() +
  geom_label(
    aes(label = round(value, 2), fill = variable),  # same color scale for bg
    color        = "white",                         # text color
    label.size   = 0,                               # no border line
    label.r      = unit(0.2, "lines"),              # rounded corners
    label.padding = unit(2, "pt"),                  # small padding
    size         = 4
  ) +
  facet_wrap(~ Dimension, nrow = 1, scales = "free_x") +
  theme(
    axis.text.x = element_text(
      color = "black", size = 12,
      angle = 90, vjust = 0.5, hjust = 1
    ),
    axis.title.x = element_text(
      margin = margin(t = 15),     # top margin
      size = 16
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),     # right margin
      size = 16
    ),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank()
  ) +
  custom_theme() +
  labs(
    x = "Drivers",
    y = "Score variation"
  )


# ------------------------------------------------------------
# Correlation chart
# ------------------------------------------------------------

# ---- Original chunk: correlation ----
vars_comp  <- paste0("COMP_", names(drivers_map$Competency))
vars_value <- paste0("VALUES_", names(drivers_map$Value))

driver_columns <- c(vars_comp, vars_value)

score_map_full <- score_map
score_map_full["Don't know"] <- 5

driver_rename_map <- question_code %>%
  filter(variable %in% driver_columns) %>%
  select(variable, short_label) %>%
  deframe() 

survey_drivers <- data %>%
  select(all_of(c(driver_columns, "weight"))) %>%
  mutate(`weight` = as.numeric(`weight`)) %>%
  rename(weight = `weight`) %>%
  rename_with(~ driver_rename_map[.x], .cols = all_of(driver_columns))

survey_unweighted <- survey_drivers

survey_weighted <- survey_drivers %>%
  mutate(across(-weight, ~ . * weight))

driver_unweighted <- survey_unweighted %>%
  select(-weight)
driver_weighted <- survey_weighted %>%
  select(-weight)

testRes = cor.mtest(driver_unweighted, conf.level = 0.95)
cor_matrix_uw <- cor(driver_unweighted, use = "pairwise.complete.obs", method = "pearson")

cor_matrix_w <- cor(driver_weighted, use = "pairwise.complete.obs", method = "pearson")

corrplot(cor_matrix_w, p.mat = testRes$p, sig.level = 0.10, method = 'circle', type = 'lower', order = 'hclust', addrect = 2 , col = COL2('PuOr', 10),insig='blank',
         tl.col = color_label_grey, tl.cex = 0.9,
         addCoef.col = "white", number.cex = 0.7, diag = FALSE, addgrid.col = 'white')

message("✓ Charts and visual outputs loaded")
