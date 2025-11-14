Data Recoding
================

This section describes how we **recode and harmonize survey response
options** to a common CTI schema so the **Community Trust Index (CTI)
score** can be computed consistently across sources. Starting from raw
survey exports (CSV/Excel), we align differing scales (e.g., 1–5
vs. 0–10), standardize directionality (ensuring higher values = higher
trust), map text labels to numeric codes, and resolve “Don’t
know/Refuse” categories. The output is a set of uniformly coded CTI
items ready for reliable index calculation and cross-survey comparison.

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

``` r
path        <- "Mozambique/"
file        <- "MOZ_Data.xlsx" # Standardized file (selected columns only)
output      <- "MOZ_Data_recoded.xlsx" # Output recoded file
data_sheet  <- "data" # Sheet name with data
```

### Import data

``` r
data_df   <- read_excel(paste0(path, file), sheet = data_sheet)

# View a few rows
#dplyr::glimpse(original_df)
```

### Recode function

``` r
###############################################
## 5. Helper: generic recode using named vector
###############################################
recode_with_map <- function(x, map, default = NA_character_) {
  y <- as.character(x)
  out <- unname(map[y])
  out[is.na(out)] <- default
  out
}
```

### Create your recode map

``` r
####################################
#       Answers  mapping           #
####################################

# Template: "Answer option from Excel"  = "LABEL DISPLAY",

gender_map <- c(
  "2"  = "Female",
  "1" = "Male"
  #"Other" = "Other"
)

education_map <- c(
  "1" = "None",
  "2" = "Primary",
  "3" = "Secondary",
  "5" = "University",
  "4" = "Professional"
  #"Don't know" = "Don't know",
  #"Prefer not to answer" = "Prefer not to answer"
)
employment_map <- c(
"1" = "Farming",
"2" = "Informal/Seasonal",
"3" = "Employee",
"4" = "Self-employed"
)

score_map_1 <- c(
  "1"    = "Yes, completely",
  "2"    = "Mostly yes",
  "4"    = "Not so much",
  "5"    = "Not at all",
  "3"    = "Don’t know"
)

answer_extra_map <- c(
"Yes completely"    = "Yes",
"Yes, completely"   = "Yes",
"Yes"               = "Yes",
"Very likely"       = "Yes",
"Fully confident"       = "Yes",


"Mostly yes"        = "Mostly yes",
"Somehow confident" = "Mostly yes",
"Likely"            = "Mostly yes",

"Unlikely"          = "Not so much",
"Not so much"       = "Not so much",

"Not at all"        = "Not at all",
"Very unlikely"     = "Not at all",
"Not confident"     = "Not at all",
"No"                = "Not at all",

"Don’t know"        = "Don’t know",
"Don't know"        = "Don’t know"
)


likely_map <- c(
  "1" = "Very likely",
  "2" = "Likely",
  "4" = "Unlikely",
  "5" = "Very unlikely",
  "3" = "Don’t know"
)

yn_map <- c(
  "1" = "Yes",
  "2"  = "No",
  "3" = "Don't know",
  "0" = "No data"
)

change_map <- c(
  "1" = "Increased",
  "2" = "Decreased",
  "3" = "Same/No change",
  "4" = "Don’t know"
)

level_map <- c(
  "1" = "High",
  "2" = "Medium",
  "3" = "Low"
)

hazard_map <- c(
  "1" = "Floods",
  "2" = "Cyclones",
  "3" = "Drought",
  "4" = "Public Health Emergency – Cholera and Diarrhea",
  "5" = "Strong Winds"
)
```

Recode

``` r
std_df <- data_df %>%
  mutate(
    # cast first to numeric/character as needed
    gender          = recode_with_map(gender, gender_map),
    education       =   recode_with_map(education, education_map),
    employment      = recode_with_map(employment, employment_map),
    hazard_main     = recode_with_map(hazard_main, hazard_map),
    hazard_frequency = recode_with_map(hazard_frequency, change_map),
    hazard_exposure = recode_with_map(hazard_exposure, change_map),
    hazard_vulnerability = recode_with_map(hazard_vulnerability, yn_map),
    hazard_concerns = recode_with_map(hazard_concerns, change_map),
    hazard_impact = recode_with_map(hazard_impact, change_map),
    hazard_riskarea = recode_with_map(hazard_riskarea, level_map),
    hazard_damage = recode_with_map(hazard_damage, change_map),
    hazard_adaptation = recode_with_map(hazard_adaptation, yn_map),
    EWS_availability = recode_with_map(EWS_availability, yn_map),
    EWS_importance = recode_with_map(EWS_importance, yn_map),
    EWS_trust_mechanism = recode_with_map(EWS_trust_mechanism, likely_map),
    EWS_trust_community = recode_with_map(EWS_trust_community, likely_map),
    EWS_trust_actors = recode_with_map(EWS_trust_actors, likely_map),
    EWS_usefulness = recode_with_map(EWS_usefulness, likely_map),
    BEFORE_EWS_awareness = recode_with_map(BEFORE_EWS_awareness, yn_map),
    BEFORE_EWS_actors_awareness = recode_with_map(BEFORE_EWS_actors_awareness, yn_map),
    BEFORE_EWS_actors_interaction = recode_with_map(BEFORE_EWS_actors_interaction, yn_map),
    BEFORE_EWS_warning = recode_with_map(BEFORE_EWS_warning, yn_map),
    EXTRA_CHANNEL_SMS = recode_with_map(EXTRA_CHANNEL_SMS, yn_map),
    EXTRA_CHANNEL_SOCIALNETWORK = recode_with_map(EXTRA_CHANNEL_SOCIALNETWORK, yn_map),
    EXTRA_CHANNEL_RADIO = recode_with_map(EXTRA_CHANNEL_RADIO, yn_map),
    EXTRA_CHANNEL_TV = recode_with_map(EXTRA_CHANNEL_TV, yn_map),
    EXTRA_CHANNEL_SOCIALMOBILIZATION = recode_with_map(EXTRA_CHANNEL_SOCIALMOBILIZATION, yn_map),
    EXTRA_CHANNEL_FAMILY = recode_with_map(EXTRA_CHANNEL_FAMILY, yn_map),
    EXTRA_CHANNEL_LOCALAUTHORITIES = recode_with_map(EXTRA_CHANNEL_LOCALAUTHORITIES, yn_map),
    WARNING_AWARENESS = recode_with_map(employment, score_map_1),
    WARNING_RESPONSIVENESS = recode_with_map(employment, score_map_1),
    WARNING_EFFECTIVENESS = recode_with_map(employment, score_map_1),
    WARNING_INCLUSIVENESS = recode_with_map(employment, score_map_1),
    WARNING_PARTICIPATION = recode_with_map(employment, score_map_1),
    WARNING_FEEDBACK = recode_with_map(employment, score_map_1),
    WARNING_TRANSPARENCY = recode_with_map(employment, score_map_1),
    ACTION_AWARENESS = recode_with_map(employment, score_map_1),
    ACTION_RESPONSIVENESS = recode_with_map(employment, score_map_1),
    ACTION_PARTICIPATION = recode_with_map(employment, score_map_1),
    ACTION_EFFECTIVENESS = recode_with_map(employment, score_map_1),
    ACTION_FEEDBACK = recode_with_map(employment, score_map_1),
    ACTION_INCLUSIVENESS = recode_with_map(employment, score_map_1),
    ACTION_TRANSPARENCY = recode_with_map(employment, score_map_1),
    
    # ensure numeric columns really are numeric
    age           = as.numeric(age)
  )
```

Export

``` r
write_xlsx(std_df, path = output)  
```
