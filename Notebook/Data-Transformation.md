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

[Download Test
package](https://cea-dataportal.github.io/trust-index/Notebook/Test.zip)

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
library(tibble)
```

### File paths and sheet configuration

Define your working directory if it doesn’t exist. All files will be
saved in this directory.

``` r
country <- "../Test"
#country <- "../Slovakia"

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
## ------ FOR TRAINING ------ ###
path        <- "../Test/"
file        <- "TEST_rawdata.xlsx" # Raw file (original version)
output      <- "TEST_Data.xlsx" # Output standardized file
code_in     <- "TEST_Questions.xlsx" # Question mapping
code_out    <- "TEST_Code.xlsx" # Code table template
data_sheet  <- "DATA" # Sheet name with data
code_sheet  <- 1 # If many sheet, write the name with column names and labels (e.g. "Code")

### ------ FOR SLOVAKIA ------ ###
# path        <- "../Slovakia/"
# file        <- "SLK_Data_original.xlsx" # Raw file (original version)
# output      <- "SLK_Data.xlsx" # Output standardized file
# code_in     <- "SLK_Questions.xlsx" # Question mapping
# code_out    <- "SLK_Code.xlsx" # Code table template
# data_sheet  <- "Data" # Sheet name with data
# code_sheet  <- 1 # If many sheet, write the name with column names and labels (e.g. "Code")
```

### Import data

``` r
original_file <- file.path(path, file)
if (file.exists(original_file)) {
original_df   <- read_excel(original_file, sheet = data_sheet)
} else {
  print("File not found - Please check the path and your folder")
}
# View a few rows
# dplyr::glimpse(original_df)
```

### Creating column mapping

This step defines a correspondence between standardized column names
(`new_name`) and the original raw column names (`old_name`) to guide the
selection and renaming process.

You could prepare an excel file for mapping questions and rename it.
Please use TEST_Questions.xlsx as template. In your original data file,
copy-paste (transposed) the first row (column header) into the column
“old_name”. Select only columns you want to keep and rename it.

In order to organize the report, we add prefixes for grouping fields
into section and charts. Here is the prefixes to use:

<table>
<colgroup>
<col style="width: 57%" />
<col style="width: 42%" />
</colgroup>
<thead>
<tr>
<th><strong>Institutional Trust</strong></th>
<th><strong>Early warning &amp; Anticipation</strong></th>
</tr>
</thead>
<tbody>
<tr>
<td><strong>COMP</strong>: All questions related to competencies<br />
<strong>VALUES</strong>: All questions related to values/ethics<br />
<strong>BEHAVIOUR</strong>: Question related to behaviours<br />
<strong>BEFORE</strong>: Experiences (with RC or services)<br />
<strong>INTENTION</strong>: Intention related to RC or service
(future)<br />
<strong>EXTRA</strong>: Other questions related to competencies, values
or channel used.</td>
<td><strong>DISASTER</strong>: Disaster Risk Knowledge<br />
<strong>DETECTION</strong>: Detection, Monitoring and Forecasting<br />
<strong>WARNING</strong>: Dissemination, Warning and Communication<br />
<strong>ACTION</strong> : Response and Preparedness Capabilities</td>
</tr>
</tbody>
</table>

*Note: You can temporarily remove a column from the mapping by
adding `#` before its line.*

``` r
code_file <- file.path(path, code_in)

if (file.exists(code_file)) {
  code_df   <- read_excel(code_file, sheet = code_sheet)
   result="hide"# Build col_map from the two columns in the file
  col_map <- code_df %>%
    select(new_name, old_name) %>%        # keep only these two
    filter(!is.na(old_name), old_name != "") %>%  # drop rows with empty old_name
    distinct()                            # optional: drop duplicates if any
  
} else {
print("Code file not found - Please create your own col_map")  
col_map <- tribble(
 ~new_name, ~old_name,
 
  #Example from Mozambique EWS Community Trust Index
  "admin1", "Provincia",
  "admin2", "Distrito",
  "admin3", "Posto_Admin",
  "locality", "Localidade",
  "sampling_area", "Povoado_Bairro",
  "gender", "gender_1M2F",
  "age", "Idade",
  "education", "nivel_escolaridade",
  "employment", "main_socioeconomicactivity",
  "native","Are you currently living in the country where you were born?",
  "migrant","What is your country of origin?",
 
 # EWS Specific columns
  "HAZARD_main", "main_climatichazard",
  "HAZARD_frequency", "freq_events",
  "HAZARD_exposure", "RISK_exposure_change",
  "HAZARD_vulnerability", "area_venerabilitylevel",
  "HAZARD_concerns", "riskandhazerd_cocernlevel",
  "HAZARD_impact", "hazardimpact_hhldslivelihoods",
  "HAZARD_damage", "hazard_propertydestruction",
  "HAZARD_riskarea", "vulnerability_hholdperception",
  "HAZARD_adaptation", "hazardadjustment_aretheremschangemade",
  "EWS_availability", "cbews_isthemechanismavailable",
  "EWS_importance", "cbews_isthemechanismimportant",
  "EWS_trust_mechanism", "cbews_doutrustthemechanism",
  "EWS_trust_community", "cbews_wholecommunitytrust",
  "EWS_trust_actors", "cbews_trustontheactor",
  "EWS_usefulness", "cbews_inadequatemessage",
 
  #Behavioural Questions
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
 
   # EWS Trust Dimension columns
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
}
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
```

    ## These old_name columns were not found in the data:
    ## - Do you agree to participate in this survey?
    ## - Are you at least 18 years old?
    ## - Before this interaction, had you heard of the Slovak Red Cross?
    ## - Branch
    ## - What main language do you speak at home?
    ## - How old are you?
    ## - How would you describe your gender?
    ## - Are you currently living in the country where you were born?
    ## - What is your country of origin?
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty seeing, even when wearing glasses.
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty hearing, even if using a hearing aid.
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty walking or climbing stairs.
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty remembering or concentrating.
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty with self-care, such as washing all over or dressing.
    ## - Is any of the following statements true for you? Please select all that apply./I have difficulty communicating, for example, understanding or being understood [this is not related to a language barrier].
    ## - Do you know what the Slovak Red Cross does in this country?/Work with migrants (first aid and humanitarian aid)
    ## - Do you know what the Slovak Red Cross does in this country?/Blood donation (campaigns to promote non-contributory blood donation, co-organisation of mobile blood donation, recognition and award events for blood donors)
    ## - Do you know what the Slovak Red Cross does in this country?/First Aid (first aid courses, first aid demonstrations, campaigns)
    ## - Do you know what the Slovak Red Cross does in this country?/Social services - (specialised social counselling, social rehabilitation, nursing care in facilities, dormitories for the homeless, shelters, day care centres, transport service, lending of medical aids, canteen, laundry, etc.)
    ## - Do you know what the Slovak Red Cross does in this country?/Restoring family links
    ## - Do you know what the Slovak Red Cross does in this country?/Crisis management
    ## - Do you know what the Slovak Red Cross does in this country?/Water Rescue Service
    ## - Do you know what the Slovak Red Cross does in this country?/Provision of food and material aid
    ## - Do you know what the Slovak Red Cross does in this country?/Other
    ## - Do you know what the Slovak Red Cross does in this country?/I don't know
    ## - How / Through which channel/s did you receive the information?/Over the phone
    ## - How / Through which channel/s did you receive the information?/SMS
    ## - How / Through which channel/s did you receive the information?/E-mail
    ## - How / Through which channel/s did you receive the information?/Social media
    ## - How / Through which channel/s did you receive the information?/Community meetings
    ## - How / Through which channel/s did you receive the information?/In-person from the Slovak Red Cross staff or a volunteer
    ## - How / Through which channel/s did you receive the information?/Word of mouth
    ## - How / Through which channel/s did you receive the information?/Flyer / Poster
    ## - How / Through which channel/s did you receive the information?/Other
    ## - Do you know how to provide feedback or make a complaint to the Slovak Red Cross?
    ## - Would you feel comfortable providing feedback or making a complaint to the Slovak Red Cross staff or volunteer?
    ## - Have you ever submitted feedback or a complaint to the Slovak Red Cross?
    ## - Did you receive a response?
    ## - If many people complained about a service offered by the Slovak Red Cross that is working badly, do you think that it would be improved?
    ## - If a decision affecting your community is to be made by the Slovak Red Cross, do you think that you would have an opportunity to voice your views?
    ## - Have you had opportunities to participate in discussions about the support available in your community?
    ## - Have you had opportunities to participate in any activities organised by the Slovak Red Cross aiming to strengthen dialogue and understanding between different groups and communities (e.g. migrants and host community members)?
    ## - Have the engagement activities or discussions facilitated by the Slovak Red Cross helped you feel more connected to the community you consider yourself a part of?
    ## - Have the engagement activities or discussions facilitated by the Slovak Red Cross helped you feel more connected to other groups of people?
    ## - Do you feel that being involved in discussions about services and support has helped improve the services?
    ## - Do you feel that your opinions and needs were valued during these activities or discussions?
    ## - Do you feel the Slovak Red Cross provided you with enough information to navigate essential aspects of life (e.g., housing, work, services, legal matters)?
    ## - Has the way information was shared been useful for improving your situation?
    ## - Has the information provided by the Slovak Red Cross helped you make better decisions about your daily life?
    ## - Has access to information, participation, and trust in services contributed to improving your ability to plan for the future?
    ## - In the past 3 years, have you received aid or support from the Slovak Red Cross?
    ## - Have you received any information about available support, services, or opportunities from the Slovak Red Cross?
    ## - Has engagement with the Slovak Red Cross helped improve your overall life situation?
    ## - Have the recent activities related to supporting migrants affected your trust in the Slovak Red Cross?
    ## - Have the recent activities related to supporting migrants affected your perception of the Slovak Red Cross’ values and competences?
    ## - Do you think the Slovak Red Cross is capable of helping people?
    ## - Do you think the Slovak Red Cross provides support to people in a timely manner?
    ## - Do you think the Slovak Red Cross understands the needs of the people it supports?
    ## - Do you think it is easy to talk to a staff or volunteer from the Slovak Red Cross?
    ## - Do you think the Slovak Red Cross provides useful information on what they do?
    ## - Do you think the Slovak Red Cross provides the right kinds of assistance to the people it supports?
    ## - Do you think the feedback received by the Slovak Red Cross influences the way they work and is used by them to improve their services?
    ## - Do you think the Slovak Red Cross puts the people it supports and their needs first, above anything else?
    ## - Do you think the Slovak Red Cross provides support to the people who need it the most?
    ## - Do you think the Slovak Red Cross provides support to all people without discrimination?
    ## - Do you think the Slovak Red Cross respects people’s cultures and personal beliefs?
    ## - Do you think the Slovak Red Cross asks local communities what support they need?
    ## - Do you think the Slovak Red Cross is responsible in how its funds are spent?
    ## - Do you think the Slovak Red Cross is independent of the government?
    ## - If the Slovak Red Cross made a big mistake in how they provide support to people, do you think it will share it publicly?
    ## - start
    ## - _index_

``` r
# Use only the old_name columns that exist
selected_col <- old %in% names(original_df)
old_exist <- old[selected_col]
selected_df <- original_df %>% select(all_of(old_exist))

# Prepare code table for export
if (file.exists(code_file)) {
  code_df <- code_df %>%
    select(-old_name) %>%        
    rename(variable = new_name) %>%
    rename_with(tolower)

  missing_cols <- setdiff(c("short_label","long_label","category", "analysis", "breakdown"), names(code_df))

  if (length(missing_cols) > 0) {
    code_df <- code_df %>%
      mutate(!!!setNames(rep(list(NA_character_), length(missing_cols)), missing_cols))
  }
  
} else {
  code_df <- col_map %>%
  transmute(
    variable     = new_name,
    short_label  = NA_character_,
    long_label   = NA_character_,
    category     = NA_character_,
    analysis     = NA_character_, #field used for factor disaggregation
    breakdown    = NA_character_#field used for creating profile of respondent
  )
}
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
```

### Export

``` r
write_xlsx(list(data = new_df), path = paste0(path, output))
write_xlsx(list(Code = code_df), path = paste0(path, code_out))
message("Standardized file written to: ", paste0(path, output))
```

    ## Standardized file written to: ../Test/TEST_Data.xlsx
