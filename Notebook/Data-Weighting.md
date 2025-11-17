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

### File paths

``` r
path        <- "../Mozambique"

data_file   <- "MOZ_Data_recoded.xlsx" # 
output      <- "MOZ_Data_weighted.xlsx" # Output recoded file
data_sheet  <- "data" # Sheet name with data
code_sheet  <- "Code" 

pop_file <- "MOZ_population_data.xlsx"
demo_tab <- "age"        # age group x gender
geo_tab  <- "geo"        # geographic area
edu_tab  <- "education"  # education level
employment_tab  <- "employment"  # employment category
```

### Import data

``` r
data_df   <- read_excel(file.path(path, data_file), sheet = data_sheet)
code_df   <- read_excel(file.path(path, data_file), sheet = code_sheet)
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
      strata = Admin2
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

print(population_summary)
```

    ## # A tibble: 20 × 3
    ##    strata_type strata                  pop_prop
    ##    <chr>       <chr>                      <dbl>
    ##  1 demo        18-29_Female              0.233 
    ##  2 demo        18-29_Male                0.191 
    ##  3 demo        30-39_Female              0.114 
    ##  4 demo        30-39_Male                0.0846
    ##  5 demo        40-49_Female              0.0870
    ##  6 demo        40-49_Male                0.0673
    ##  7 demo        50-59_Female              0.0563
    ##  8 demo        50-59_Male                0.0441
    ##  9 demo        60+_Female                0.0692
    ## 10 demo        60+_Male                  0.0543
    ## 11 geo         Buzi                      0.889 
    ## 12 geo         Chubugo                   0.111 
    ## 13 education   No formal education       0.0638
    ## 14 education   Primary School            0.276 
    ## 15 education   Secondary school          0.434 
    ## 16 education   University                0.227 
    ## 17 employment  Active but unemployed    NA     
    ## 18 employment  Employed                 NA     
    ## 19 employment  Not economically active  NA     
    ## 20 employment  Not usually active       NA

### Sampling Data

**Demography** : age and gender

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
  # data_df <- data_df |> rename(Age_group = age)
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
      sample_prop
    )
} else {
  sample_demo_tbl <- NULL
}
```

**Geographic**

``` r
## 3) Use Admin2 as geo strata
data_df <- data_df |>
  mutate(
    geo = admin2    # direct copy, but makes the intention explicit
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
      sample_prop
    )
} else {
  sample_geo_tbl <- NULL
}
```

**Education**

``` r
if (any(!is.na(data_df$education))) {
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
      sample_prop
    )
} else {
  sample_edu_tbl <- NULL
}
```

**Employment**

``` r
if (any(!is.na(data_df$employment))) {
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
      sample_prop
    )
} else {
  sample_employment_tbl <- NULL
}
```

``` r
sample_summary <- bind_rows(
  sample_demo_tbl,
  sample_geo_tbl,
  sample_edu_tbl,
  sample_employment_tbl
)

print (sample_summary)
```

    ## # A tibble: 21 × 3
    ##    strata_type strata       sample_prop
    ##    <chr>       <chr>              <dbl>
    ##  1 demo        18-29_Female      0.196 
    ##  2 demo        18-29_Male        0.0893
    ##  3 demo        30-39_Female      0.153 
    ##  4 demo        30-39_Male        0.0721
    ##  5 demo        40-49_Female      0.112 
    ##  6 demo        40-49_Male        0.0619
    ##  7 demo        50-59_Female      0.0852
    ##  8 demo        50-59_Male        0.0471
    ##  9 demo        60+_Female        0.0959
    ## 10 demo        60+_Male          0.0876
    ## # ℹ 11 more rows

### Weight calculation

**Stratum-Level Weight Calculation**

For each stratum, we calculate the ratio between the population
proportion (target) and the sample proportion (survey).

``` r
weights_tbl <- population_summary |>
  inner_join(sample_summary, by = c("strata_type", "strata")) |>
  mutate(weight = pop_prop / sample_prop)
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


data_df <- data_df |>
  # Join demo weight on demo strata
  left_join(demo_weights, by = c("demo" = "strata")) |>
  # Join geo weight on geo strata
  left_join(geo_weights,  by = c("geo"  = "strata")) |>
  # Join education weight on edu strata
  left_join(edu_weights,  by = c("education"  = "strata")) |>
  # Join employment weight on employment strata
  left_join(emp_weights,  by = c("employment"  = "strata"))
```

**Overall Weight Calculation**

We calculate an overall weight based on the correspondence between
strata in the survey and strata in the population data.

``` r
data_df <- data_df |>
  mutate(
    # Replace missing weights with 1 (no adjustment)
    demo_weight = ifelse(is.na(demo_weight), 1, demo_weight),
    geo_weight  = ifelse(is.na(geo_weight),  1, geo_weight),
    edu_weight  = ifelse(is.na(edu_weight),  1, edu_weight),
    emp_weight  = ifelse(is.na(emp_weight),  1, emp_weight),
    
  
    weight = demo_weight * geo_weight * edu_weight * emp_weight
  )
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

data_df <- data_df |>
  relocate(Age_group, .after = age) |>
  relocate(weight, .before = Date)
```

### 

### Export

``` r
write_xlsx(list(data = data_df, Code = code_df), path = file.path(path, output))  
```
