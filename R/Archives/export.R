##########################################################
#.  COMMUNITY TRUST INDEX - EXPORT
##########################################################

# ============================================================
# CTI Report - export.R
# Export report data and archive report source
# Extracted from Data-Report-INST.Rmd
# ============================================================

message("Exporting report data...")

# This script assumes the following scripts have already run:
# source('R/setup.R')
# source('R/read_config.R')
# source('R/load_data.R')
# source('R/prepare_data.R')
# source('R/compute_score.R')
# source('R/charts.R') # if exported objects are produced there

# ------------------------------------------------------------
# Original export chunk
# ------------------------------------------------------------

## Excel file Export

tables <- list()
tables[["Competencies"]] <- summary_comp
tables[["Values"]] <-  summary_val
if (isTRUE(check_extra)) {tables[["Extra_questions"]] <- summary_exp}

tables[["Trust Index"]] <- summary_2

tables[["Sampling_age"]] <- tab_age_group
tables[["Sampling_Gender"]] <-  tab_gender
if (isTRUE(check_adm)) {tables[["Sampling_Geo"]] <- tab_geo
}
tables[["Sampling_type"]] <- tab_group

writexl::write_xlsx(tables, path = file.path(path, export_file))



## Excel file for Google / Database

summary_export <- summary_2 %>%
  filter(Dimension != "INST INDEX") %>%
  pivot_longer(
    cols = -c(Dimension, Drivers),
    names_to = "Serie",
    values_to = "Value"
  ) %>%
  arrange(Serie, Dimension, Drivers) %>%
  select(Dimension, Drivers, Serie, Value)

current_year <- as.numeric(format(Sys.Date(), "%Y"))

clean_dimension <- function(x) {
  recode(
    x,
    "Competency" = "Competencies",
    "Value" = "Values",
    .default = x
  )
}

geo_short_labels <- question_code %>%
  filter(category == "geographic") %>%
  pull(short_label) %>%
  gsub("\\n", " ", .) %>%
  tools::toTitleCase() %>%
  tolower()

output_gg <- bind_rows(

  summary_2 %>%
    filter(Dimension == "INST INDEX") %>%
    transmute(
      Country = country_name,
      Module = "Institutional",
      Year = current_year,
      Group = "Overall",
      Name = "Institutional",
      Label = "Institutional",
      Series = "",
      Value = Overall,
      `Label::FR` = "",
      `Label::ES` = "",
      `Label::PT` = ""
    ),

  summary_2 %>%
    filter(Drivers == "Overall", Dimension != "INST INDEX") %>%
    transmute(
      Country = country_name,
      Module = "Institutional",
      Year = current_year,
      Group = "Dimension",
      Name = clean_dimension(Dimension),
      Label = clean_dimension(Dimension),
      Series = "",
      Value = Overall,
      `Label::FR` = "",
      `Label::ES` = "",
      `Label::PT` = ""
    ),

  summary_export %>%
    filter(Drivers != "Overall") %>%
    transmute(
      Country = country_name,
      Module = "Institutional",
      Year = current_year,
      Group = "Drivers",
      Name = clean_dimension(Dimension),
      Label = Drivers,
      Series = sub(":.*", "", Serie),
      Value = Value,
      `Label::FR` = "",
      `Label::ES` = "",
      `Label::PT` = ""
    ),
  

  means_df %>%
    filter(
      !is.na(variable_value),
      variable_value != 0
    ) %>%
    mutate(
      clean_variable = tools::toTitleCase(gsub("\\n", " ", variable)),
      is_geographic = tolower(clean_variable) %in% geo_short_labels
    ) %>%
    transmute(
      Country = country_name,
      Module = "Institutional",
      Year = current_year,
      Group = if_else(is_geographic, "Geographic", "Factor"),
      Name = clean_variable,
      Label = variable_value,
      Series = recode(
        dimension,
        "comp" = "Competencies",
        "values" = "Values",
        .default = dimension
      ),
      Value = mean,
      `Label::FR` = "",
      `Label::ES` = "",
      `Label::PT` = ""
    ),
  
  tab_age_group %>%
  transmute(
    Country = country_name,
    Module = "Institutional",
    Year = current_year,
    Group = "Sampling",
    Name = "Age group",
    Label = `Age Group`,
    Series = "",
    Value = `Total Respondents`,
    `Label::FR` = "",
    `Label::ES` = "",
    `Label::PT` = ""
  ),
  
  tab_gender %>%
  filter(Gender %in% c("Female", "Male")) %>%
  transmute(
    Country = country_name,
    Module = "Institutional",
    Year = current_year,
    Group = "Sampling",
    Name = "Gender",
    Label = Gender,
    Series = "",
    Value = `Total Respondents`,
    `Label::FR` = "",
    `Label::ES` = "",
    `Label::PT` = ""
  ),
  
   tab_geo %>%
    filter(
      !has_adm2 | .[[2]] == "TOTAL"
    ) %>%
    transmute(
      Country = country_name,
      Module = "Institutional",
      Year = current_year,
      Group = "Sampling",
      Name = "Geographic",
      Label = .[[1]],
      Series = "",
      Value = `Total Respondents`,
      `Label::FR` = "",
      `Label::ES` = "",
      `Label::PT` = ""
    )
  
)


subset_postfix <- if (exists("subset_postfix") &&
                      !is.null(subset_postfix) &&
                      !is.na(subset_postfix) &&
                      nzchar(subset_postfix)) {
  subset_postfix
} else {
  ""
}

export_gg <- paste0(country_iso, subset_postfix, "_gg_export.xlsx") 

writexl::write_xlsx(output_gg, path = file.path(path, export_gg))

message("✓ Report data exported")
