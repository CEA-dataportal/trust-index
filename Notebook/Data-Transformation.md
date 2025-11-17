Data Transformation
================

### Introduction

This notebook documents the workflow used to prepare survey data from
the **Community Trust Index (CTI)** and other related survey instruments
for analysis. The goal is to take raw survey exports (e.g. CSV, Excel)
and transform them into a clean, standardized dataset with consistent
variable names, formats, and coding schemes.

The code that follows will:

- Import raw survey files from their original sources

- Extract fields from raw survey that we will use for Community Trust
  Index

- Standardize fields to a common schema

- Output an analysis-ready dataset that can be reused across projects

All steps are implemented in **R**. Each transformation is performed
programmatically (rather than manually) and is documented in the
notebook so that the process can be re-run when new data become
available or when additional surveys are added. This ensures that
downstream analyses of community trust are based on consistent,
well-documented data preparation procedures.

### Load packages

Load required libraries for data import, cleaning, transformation, and
export

``` r
library(readxl)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
library(writexl)
```

### File paths and sheet configuration

Define your working directory if it doesn’t exist. All files will be
saved in this directory.

``` r
country <- "../Mozambique"

# Create the folder if it doesn't exist
if (!dir.exists(country)) {
  dir.create(country)
} else 
print ("Folder already created")
```

    ## [1] "Folder already created"

1\. Specify the location of the original raw data file exported from the
survey.

2\. Define the path and filename for the standardized output dataset.

3\. Indicate which Excel sheet contains the actual survey data to be
processed.

``` r
path        <- "../Mozambique/"
file        <- "MOZ_CTI_rawdata.xlsx" # Raw file (original version)
output      <- "MOZ_Data.xlsx" # Output standardized file
data_sheet  <- "DATA" # Sheet name with data
```

### Import data

``` r
original_df   <- read_excel(paste0(path, file), sheet = data_sheet)

# View a few rows
#dplyr::glimpse(original_df)
```

### Creating column mapping

This step defines a correspondence between standardized column names
(`new_name`) and the original raw column names (`old_name`) to guide the
selection and renaming process.

*Note: You can temporarily remove a column from the mapping by
adding `#` before its line.*

``` r
col_map <- tribble(
 ~new_name, ~old_name,
   "admin1", "Provincia",
  "admin2", "Distrito",
  "admin3", "Posto_Admin",
  "locality", "Localidade",
  "sampling_area", "Povoado_Bairro",
  "gender", "gender_1M2F",
  "age", "Idade",
  "education", "nivel_escolaridade",
  "employment", "main_socioeconomicactivity",
  "hazard_main", "main_climatichazard",
  "hazard_frequency", "freq_events",
  "hazard_exposure", "RISK_exposure_change",
  "hazard_vulnerability", "area_venerabilitylevel",
  "hazard_concerns", "riskandhazerd_cocernlevel",
  "hazard_impact", "hazardimpact_hhldslivelihoods",
  "hazard_damage", "hazard_propertydestruction",
  "hazard_riskarea", "vulnerability_hholdperception",
  "hazard_adaptation", "hazardadjustment_aretheremschangemade",
  "EWS_availability", "cbews_isthemechanismavailable",
  "EWS_importance", "cbews_isthemechanismimportant",
  "EWS_trust_mechanism", "cbews_doutrustthemechanism",
  "EWS_trust_community", "cbews_wholecommunitytrust",
  "EWS_trust_actors", "cbews_trustontheactor",
  "EWS_usefulness", "cbews_inadequatemessage",
  "BEFORE_EWS_awareness", "cbews_instaledequipmentoperational",
  "BEFORE_EWS_actors_awareness", "cbews_awarenessclgrdexistence",
  "BEFORE_EWS_actors_interaction", "cbews_hadinteractionwithclgrd",
  "BEFORE_EWS_warning", "cbews_receptionofanAAPmessage",
  "EXTRA_CHANNEL_SMS", "cbews_usedchannelSMS",
  "EXTRA_CHANNEL_SOCIALNETWORK", "cbews_usedchannelRedes Sociais",
  "EXTRA_CHANNEL_RADIO", "cbews_usedchannelRadio",
  "EXTRA_CHANNEL_TV", "cbews_usedchannelTV",
  "EXTRA_CHANNEL_SOCIALMOBILIZATION", "cbews_usedchannelmobilebrigadeclgrdmegafone", 
  "EXTRA_CHANNEL_FAMILY", "cbews_usedchannelfriedorparent",
  "EXTRA_CHANNEL_LOCALAUTHORITIES", "cbews_usedchannellocalauthorities",
  "WARNING_AWARENESS", "cbews_islocalknowldegeintegratedinAAPmechanism",
  "WARNING_RESPONSIVENESS", "cbews_responsivenessintimemessaging",
  "WARNING_EFFECTIVENESS", "cbews_aapmessageseffectiveness",
  "WARNING_INCLUSIVENESS", "cbews_inclusiveness",
  "WARNING_PARTICIPATION", "cbews_partcipation",
  "WARNING_FEEDBACK", "cbews_opnessandsafedialog",
  "WARNING_TRANSPARENCY", "cbews_transparency",
  "ACTION_AWARENESS", "preparednessrci_knowledge",
  "ACTION_RESPONSIVENESS", "preparednessrci_responsiveness",
  "ACTION_PARTICIPATION", "preparednessrci_participation",
  "ACTION_EFFECTIVENESS", "preparednessrci_effectiveness",
  "ACTION_FEEDBACK", "preparednessrci_opensess",
  "ACTION_INCLUSIVENESS", "preparednessrci_inclusiveness",
  "ACTION_TRANSPARENCY", "preparednessrci_transparency",
  "Date", "Data",
  "_index", "_index"
 )
```

### Selection

This block validates your mapping, reports any `old_name` columns
missing in the raw data, and selects only existing raw columns. It then
builds a clean selection (`selected_df`) and prepares a “Code” sheet
(`code_df`) listing standardized variable names with placeholder fields
for short/long labels and category.

``` r
old <- col_map$old_name
new <- col_map$new_name

# Check which raw columns are missing
missing_old <- setdiff(old, names(original_df))
if (length(missing_old)) {
  message("These old_name columns were not found in the data:\n- ",
          paste(missing_old, collapse = "\n- "))
}
# Use only the old_name columns that exist
selected_col <- old %in% names(original_df)
old_exist <- old[selected_col]
selected_df <- original_df %>% select(all_of(old_exist))

# Prepare code table for export
code_df <- col_map %>%
  transmute(
    variable     = new_name,
    short_label  = NA_character_,
    longer_label = NA_character_,
    category     = NA_character_
  )
```

### Rename columns

The code below selects columns from the original dataset based on their
original names, then renames them using standardized English names. It
keeps only existing columns, ensures the old and new names align, and
produces a clean dataset ready for analysis.

``` r
new_exist <- new[selected_col]
new_df <- selected_df %>%
  rename(!!!setNames(old_exist, new_exist))

#dplyr::glimpse(new_df)
```

### Export

``` r
write_xlsx(list(data = new_df, Code = code_df), path = paste0(path, output))
message("Standardized file written to: ", paste0(path, output))
```

    ## Standardized file written to: ../Mozambique/MOZ_Data.xlsx
