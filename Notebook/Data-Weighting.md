Data Weighting
================

This section describes how we **recode and harmonize survey response
options** to a common CTI schema so the **Community Trust Index (CTI)
score** can be computed consistently across sources. Starting from raw
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
library(stringr)
library(writexl)
```

### File paths

``` r
### ------ FOR TRAINING ------ ###
path        <- "../Test"

data_file   <- "TEST_Data_recoded.xlsx" #
output      <- "TEST_Data_weighted.xlsx" # Output recoded file
data_sheet  <- "data" # Sheet name with data

pop_file <- "TEST_population_data.xlsx"
demo_tab <- "age"        # age group x gender
geo_tab  <- "geo"        # geographic area
edu_tab  <- "education"  # education level
employment_tab  <- "employment"  # employment category

# ---- Level of geographic level use for weighting
geo_var1 = "admin2" # from survey data
geo_var2 = "Admin2" # from population data

### ------ FOR TRAINING ------ ###
# path        <- "../Slovakia"
# 
# data_file   <- "SVK_Data.xlsx" #
# output      <- "SVK_Data_weighted.xlsx" # Output recoded file
# data_sheet  <- "data" # Sheet name with data
# 
# pop_file <- "SVK_population_data.xlsx"
# demo_tab <- "age"        # age group x gender
# geo_tab  <- "geo"        # geographic area
# edu_tab  <- "education"  # education level
# employment_tab  <- "employment"  # employment category
# 
# ---- Level of geographic level use for weighting
# geo_var1 = "admin1" # from survey data
# geo_var2 = "Admin1" # from population data
```

### Import data

``` r
data_df   <- read_excel(file.path(path, data_file), sheet = data_sheet)
# View a few rows
#dplyr::glimpse(data_df)
```

Import data population

``` r
is_empty_sheet <- function(df) {
  is.null(df) ||
    nrow(df) == 0 ||
    ncol(df) == 0 ||
    all(is.na(df))
}

full_path <- file.path(path, pop_file)

if (file.exists(full_path)) {
  population_demo <- read_excel(full_path, sheet = demo_tab)
  population_geo  <- read_excel(full_path, sheet = geo_tab)
  population_edu  <- read_excel(full_path, sheet = edu_tab)
  population_employment  <- read_excel(full_path, sheet = employment_tab)

  # Check
  if (is_empty_sheet(population_demo)) {
    population_demo <- NULL
    message("Sheet '", demo_tab, "' is empty or has only NA values.")
  }

  if (is_empty_sheet(population_geo)) {
    population_geo <- NULL
    message("Sheet '", geo_tab, "' is empty or has only NA values..")
  }

  if (is_empty_sheet(population_edu)) {
    population_edu <- NULL
    message("Sheet '", edu_tab, "' is empty or has only NA values.")
  }
  
  if (is_empty_sheet(population_employment)) {
    population_employment <- NULL
    message("Sheet '", employment_tab, "' is empty or has only NA values.")
  }


} else {
  message("File not found : ", full_path)
  population_demo <- population_geo <- population_edu <- population_employment <- NULL
}
```

### Population Data

Function that creates a new column and calculates each row’s value
divided by the sum of all rows.

``` r
# Generic helper to compute proportions on a given numeric column
calc_proportion <- function(df, value_col, prop_col = "pop_prop") {
  # If the data frame is missing (NULL), just return NULL
  if (is.null(df)) return(NULL)
  
  # Compute total of the reference column
  total <- sum(df[[value_col]], na.rm = TRUE)
  
  if (is.na(total) || total == 0) {
    warning("Total of ", value_col, " is zero or NA; cannot compute proportions.")
    df[[prop_col]] <- NA_real_
  } else {
    df[[prop_col]] <- df[[value_col]] / total
  }
  
  return(df)
}
```

Calculate population proportions for demographic table (AGEgroup x
Gender)

``` r
if (!is.null(population_demo)) {
  pop_demo_tbl <- population_demo |>
    mutate(
      strata = paste(AGEgroup, Gender, sep = "_")
    ) |>
    group_by(strata) |>
    summarise(
      Freq = sum(Freq, na.rm = TRUE),
      .groups = "drop"
    )
  
  pop_demo_tbl <- calc_proportion(
    df        = pop_demo_tbl,
    value_col = "Freq",
    prop_col  = "pop_prop"
  ) |>
    mutate(
      strata_type = "demo"
    ) |>
    select(
      strata_type,
      strata,
      pop_prop
    )
  
} else {
  pop_demo_tbl <- NULL
}
```

Geographic distribution

``` r
if (!is.null(population_geo)) {
  if (is.character(population_geo$Pop)) {
    population_geo$Pop <- as.numeric(gsub(",", ".", population_geo$Pop))
  }

  pop_geo_tbl <- population_geo |>
    mutate(
      strata = .data[[geo_var2]]
    ) |>
    group_by(strata) |>
    summarise(
      Pop = sum(Pop, na.rm = TRUE),
      .groups = "drop"
    )

  pop_geo_tbl <- calc_proportion(
    df        = pop_geo_tbl,
    value_col = "Pop",
    prop_col  = "pop_prop"
  ) |>
    mutate(
      strata_type = "geo"
    ) |>
    select(
      strata_type,
      strata,
      pop_prop
    )
} else {
  pop_geo_tbl <- NULL
}
```

Education level

``` r
if (!is.null(population_edu)) {
  if (is.character(population_edu$Percentage)) {
    population_edu$Percentage <- as.numeric(gsub(",", ".", population_edu$Percentage))
  }

  pop_edu_tbl <- population_edu |>
    mutate(
      strata = Level
    ) |>
    group_by(strata) |>
    summarise(
      Percentage = sum(Percentage, na.rm = TRUE),
      .groups = "drop"
    )
  
  pop_edu_tbl <- calc_proportion(
    df        = pop_edu_tbl,
    value_col = "Percentage",
    prop_col  = "pop_prop"
  ) |>
    mutate(
      strata_type = "education"
    ) |>
    select(
      strata_type,
      strata,
      pop_prop
    )
} else {
  pop_edu_tbl <- NULL
}
```

Employment category

``` r
if (exists("population_employment") && !is.null(population_employment)) {
  if (is.character(population_employment$Percentage)) {
    population_employment$Percentage <- as.numeric(gsub(",", ".", population_employment$Percentage))
  }

  pop_emp_tbl <- population_employment |>
    mutate(
      strata = Level
    ) |>
    group_by(strata) |>
    summarise(
      Percentage = sum(Percentage, na.rm = TRUE),
      .groups = "drop"
    )

  pop_emp_tbl <- calc_proportion(
    df        = pop_emp_tbl,
    value_col = "Percentage",
    prop_col  = "pop_prop"
  ) |>
    mutate(
      strata_type = "employment"
    ) |>
    select(
      strata_type,
      strata,
      pop_prop
    )
} else {
  pop_emp_tbl <- NULL
}
```

    ## Warning in calc_proportion(df = pop_emp_tbl, value_col = "Percentage", prop_col
    ## = "pop_prop"): Total of Percentage is zero or NA; cannot compute proportions.

Summary (optional)

``` r
population_summary <- bind_rows(
  pop_demo_tbl,   # from your previous demo pipeline
  pop_geo_tbl,
  pop_edu_tbl,
  pop_emp_tbl
)
```

### Sampling Data

**Demography** : age and gender

We first standardize the age variable: if it is numeric, we convert it
into predefined age groups; if it is already categorical, we simply
rename it. Next, we create a demographic stratum by combining age group
and gender for respondents with valid information. For these strata, we
count the sample cases and compute their sample proportions, stored in
sample_demo_tbl. If no valid strata exist, this table is set to NULL.

``` r
if (is.numeric(data_df$age)) {
  data_df <- data_df |>
    mutate(
      # Create age groups consistent with population_demo$Age_Group
      Age_group = case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 39 ~ "30-39",
        age >= 40 & age <= 49 ~ "40-49",
        age >= 50 & age <= 59 ~ "50-59",
        age >= 60             ~ "60+",
        TRUE                  ~ NA_character_   # ages outside range or missing
      )
    )
} else {
  # If age is already categorical with the right labels, you could just rename
  data_df <- data_df |> rename(Age_group = age)
}

## 2) Create demo strata = Age_group + Gender
data_df <- data_df |>
  mutate(
    # Create demo strata only when both Age_group and Gender are not missing
    demo = if_else(
      !is.na(Age_group) & !is.na(gender),
      paste(Age_group, gender, sep = "_"),
      NA_character_
    )
  )

if (any(!is.na(data_df$demo))) {
  sample_demo_tbl <- data_df |>
    filter(!is.na(demo)) |>
    # Count number of observations per demo strata
    count(strata = demo, name = "Count") |>
    # Compute sample proportions
    calc_proportion(
      df        = _,
      value_col = "Count",
      prop_col  = "sample_prop"
    ) |>
    mutate(
      strata_type = "demo"
    ) |>
    select(
      strata_type,
      strata,
      sample_prop,
      Count
    )
} else {
  sample_demo_tbl <- NULL
}
```

**Geographic**

We first create a geographic variable geo based on the chosen input
variable (geo_var1). If any respondents have non-missing values on geo,
we form geographic strata, count the number of cases in each, and
compute their sample proportions, storing the results in sample_geo_tbl
with strata_type = “geo”. If all geo values are missing, sample_geo_tbl
is set to NULL.

``` r
data_df <- data_df |>
  mutate(
    geo = .data[[geo_var1]] 
  )

if (any(!is.na(data_df$geo))) {
  sample_geo_tbl <- data_df |>
    filter(!is.na(geo)) |>
    count(strata = geo, name = "Count") |>
    calc_proportion(
      df        = _,
      value_col = "Count",
      prop_col  = "sample_prop"
    ) |>
    mutate(
      strata_type = "geo"
    ) |>
    select(
      strata_type,
      strata,
      sample_prop,
      Count
    )
} else {
  sample_geo_tbl <- NULL
}
```

**Education**

If an education variable exists and has valid values, we define
education strata, count respondents in each, and compute their sample
proportions in sample_edu_tbl (with strata_type = “edu”). If not,
sample_edu_tbl is set to NULL.

``` r
if ("education" %in% names(data_df) && any(!is.na(data_df$education))) {
  sample_edu_tbl <- data_df |>
    filter(!is.na(education)) |>
    count(strata = education, name = "Count") |>
    calc_proportion(
      df        = _,
      value_col = "Count",
      prop_col  = "sample_prop"
    ) |>
    mutate(
      strata_type = "edu"
    ) |>
    select(
      strata_type,
      strata,
      sample_prop,
      Count
    )
} else {
  sample_edu_tbl <- NULL
}
```

**Employment**

If an employment variable exists and has valid values, we create
employment strata, count respondents in each, and compute their sample
proportions in sample_employment_tbl (with strata_type = “employment”);
otherwise, sample_employment_tbl is set to NULL.

``` r
if ("employment" %in% names(data_df) && any(!is.na(data_df$employment))) {
  sample_employment_tbl <- data_df |>
    filter(!is.na(employment)) |>
    count(strata = employment, name = "Count") |>
    calc_proportion(
      df        = _,
      value_col = "Count",
      prop_col  = "sample_prop"
    ) |>
    mutate(
      strata_type = "employment"
    ) |>
    select(
      strata_type,
      strata,
      sample_prop,
      Count
    )
} else {
  sample_employment_tbl <- NULL
}
```

We then combine all available strata tables (demographic, geographic,
education, and employment) into a single sample_summary table, which
contains the sample proportions and counts for all defined strata types.

``` r
sample_summary <- bind_rows(
  sample_demo_tbl,
  sample_geo_tbl,
  sample_edu_tbl,
  sample_employment_tbl
)
```

### Weight calculation

**Stratum-Level Weight Calculation**

For each stratum, we calculate the ratio between the population
proportion (target) and the sample proportion (survey).

In some strata, this ratio can become very large when the sample share
is much smaller than the population share. Such extreme weights can
distort estimates and inflate variance.

Therefore, if any raw weight exceeds 4, we trim all weights at this
threshold and rescale them. This limits the influence of extreme cases
while preserving overall representativeness.

``` r
weights_tbl <- population_summary |>
  inner_join(sample_summary, by = c("strata_type", "strata")) |>
  mutate(weight_raw = pop_prop / sample_prop)

# Only trim & rescale if any weight_raw > 4
if (max(weights_tbl$weight_raw, na.rm = TRUE) > 4) {
  weights_tbl <- weights_tbl |>
    mutate(weight_trimmed = pmin(weight_raw, 4))
  
  # rescale so the sum of weights stays the same as for weight_raw
  scale_factor <- sum(weights_tbl$weight_raw, na.rm = TRUE) /
                  sum(weights_tbl$weight_trimmed, na.rm = TRUE)
  weights_tbl <- weights_tbl |>
        mutate(weight = weight_trimmed * scale_factor)
  neutral_coefficient <- 1 * scale_factor

  } else {
  # no trimming needed, just use raw weights
  weights_tbl <- weights_tbl |>
    mutate(weight = weight_raw)
  neutral_coefficient <- 1
}
```

  
**Assigning Weights to Survey Data by Stratum**

We will assign weights to the survey data according to their
corresponding strata.

``` r
demo_weights <- weights_tbl |>
  filter(strata_type == "demo") |>
  select(strata, demo_weight = weight)

geo_weights <- weights_tbl |>
  filter(strata_type == "geo") |>
  select(strata, geo_weight = weight)

edu_weights <- weights_tbl |>
  filter(strata_type == "education") |>
  select(strata, edu_weight = weight)

emp_weights <- weights_tbl |>
  filter(strata_type == "employment") |>
  select(strata, emp_weight = weight)

if (!is.null(demo_weights) && any(!is.na(demo_weights$strata))) {
  data_df <- data_df |>
    dplyr::left_join(demo_weights, by = c("demo" = "strata")) # Join demo weight on demo strata
  } else {
  data_df <- data_df |>
    dplyr::mutate(demo_weight = neutral_coefficient)
  }
 
if (!is.null(geo_weights) && any(!is.na(geo_weights$strata))) { 
  data_df <- data_df |>
      left_join(geo_weights,  by = c("geo"  = "strata")) # Join geo weight on geo strata
  } else {
  data_df <- data_df |>
    dplyr::mutate(geo_weight = neutral_coefficient)
  }

if (!is.null(edu_weights) && any(!is.na(edu_weights$strata))) { 
  left_join(edu_weights,  by = c("education"  = "strata")) # Join education weight on edu strata
  } else {
  data_df <- data_df |>
    dplyr::mutate(edu_weight = neutral_coefficient)
  }

if (!is.null(emp_weights) && any(!is.na(emp_weights$strata))) { 
  left_join(emp_weights,  by = c("employment"  = "strata"))  # Join employment weight on employment strata
  } else {
  data_df <- data_df |>
    dplyr::mutate(emp_weight = neutral_coefficient)
  }
```

**Overall Weight Calculation**

We calculate an overall weight based on the correspondence between
strata in the survey and strata in the population data.

``` r
data_df <- data_df |>
  mutate(
    # Replace missing weights with 1 (no adjustment)
    demo_weight = ifelse(is.na(demo_weight), neutral_coefficient, demo_weight),
    geo_weight  = ifelse(is.na(geo_weight),  neutral_coefficient, geo_weight),
    edu_weight  = ifelse(is.na(edu_weight),  neutral_coefficient, edu_weight),
    emp_weight  = ifelse(is.na(emp_weight),  neutral_coefficient, emp_weight),

  
    weight = demo_weight * geo_weight * edu_weight * emp_weight
  )
```

### Export

``` r
# Remove temporary column
data_df <- data_df |>
  select(
    -demo,
    -geo,
    -demo_weight,
    -geo_weight,
    -edu_weight,
    -emp_weight
  )


write_xlsx(list(data = data_df), path = file.path(path, output))  
```
