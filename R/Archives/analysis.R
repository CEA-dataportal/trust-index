##########################################################
#.  COMMUNITY TRUST INDEX - ANALYSIS
##########################################################

# ============================================================
# CTI Report - analysis.R
# Data preparation and score computation
# Merged from prepare_data.R and compute_score.R
# ============================================================

message("Running data preparation and score computation...")

# This script assumes the following scripts have already run:
# source("R/setup.R")
# source("R/read_config.R")
# source("R/load_data.R")

# ============================================================
# 1. Data preparation
# ============================================================

# ============================================================
# CTI Report - prepare_data.R
# Prepare analysis parameters, groups and drivers mapping
# Extracted from Data-Report-INST.Rmd
# ============================================================

message("Preparing analysis metadata...")

#Factors for data analysis

if ("analysis" %in% names(question_code)) {
  disaggregation_levels <- question_code |>
    dplyr::filter(tolower(analysis) == "yes")  |>
    dplyr::mutate(
      variable = if_else(variable == "age", "Age_group", variable)
    ) |>
    dplyr::select(variable, short_label, long_label)
} else {
  disaggregation_levels <- NULL
}

# Questions used to build respondent profiles,
# such as identifying people affected by floods or those who have recently received an alert.

if ("breakdown" %in% names(question_code)) {
  group_map <- question_code %>%
    dplyr::filter(!is.na(breakdown) & breakdown != "") |>
    separate_rows(breakdown, sep = ",") |>
    dplyr::mutate(
      term        = str_to_title(str_trim(breakdown)),         
      group_col   = variable,
      group_value = term,
      group_label = paste0(short_label, ": ", term)             
    ) |>
    
    dplyr::mutate(group_id = paste0("grp_", row_number())) |>
    
    dplyr::select(group_id, group_col, group_value, group_label)
} else {
  group_map <- NULL
}

# This code prepares helper objects to link survey questions to their corresponding
# “drivers” and dimensions based on variable names and metadata from question_code.

prefixes <- unique(str_extract(question_code$variable, "^[^_]+"))
prefix_pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")_")

#Build the drivers_map
drivers_map <- question_code %>%
  rename(
    Variables = variable,
    Drivers   = short_label,
    Dimension = category
  ) %>%
  mutate(
    Dimension = str_to_title(Dimension),
    Variables = str_replace(Variables, prefix_pattern, "")  # Remove detected prefix
  ) %>%
  filter(Dimension %in% c("Competency", "Value")) %>%
  group_by(Dimension) %>%
  summarise(
    mapping = list(setNames(Drivers, Variables)),
    .groups = "drop"
  ) %>%
  deframe()


drivers_lookup <- question_code %>%
  filter(category %in% c("competency", "value")) %>%
  mutate(
    Dimension = stringr::str_to_title(category)
  ) %>%
  select(variable, Dimension, short_label) %>%
  rename(
    Variables = variable,
    Drivers = short_label
  )

message("✓ Analysis metadata prepared")


# ============================================================
# 2. Score computation
# ============================================================

# ============================================================
# CTI Report - compute_score.R
# Compute CTI scores, weighted summaries, profile groups and tests
# Extracted from Data-Report-INST.Rmd
# ============================================================

message("Computing scores...")

# This chunk builds and applies a dynamic 0–10 scoring scale, computes pillar means,
# and summarises mean scores by disaggregation groups into means_df.

values <- colnames(data)[grepl(paste0("^", prefix_val),colnames(data))]
comp <- colnames(data)[grepl(paste0("^", prefix_comp),colnames(data))]

# Save data with answer option as survey_data
survey_data <- data[, c(comp, values)]

# Build the dynamic score scale from answer_likertscale

score_map_0 <- score_map
score_map <- gsub("’", "'", score_map)
if (Neutral == "No") {
  scoring_levels <- score_map[!score_map %in% c("I don't know","Don't know","No answer")]
} else {
  scoring_levels <- score_map[!score_map %in% c("No answer")]
}
ordered_levels <- unique(scoring_levels)

n_levels <- length(ordered_levels)
score_values <- seq(from = 10, to = 0, length.out = n_levels)
score_map <- setNames(score_values, ordered_levels)



# Convert to score
data[, c(comp, values)] <- data[, c(comp, values)] %>% 
  dplyr::mutate(
    across( 
      everything(),
      ~ as.numeric(recode( as.character(.), !!!score_map, .default = NA_real_ # or some other numeric default 
      )) ))

data$comp_mean <- data %>% select(comp) %>% rowMeans(na.rm=TRUE)
data$values_mean <- data %>% select(values) %>% rowMeans(na.rm=TRUE)


means_df <- data.frame(
  variable = character(),
  variable_value = character(),
  dimension = character(),
  mean = numeric(),
  stringsAsFactors = FALSE
)

# Loop through defined variables
for (i in disaggregation_levels$variable) {
  
  # Find matching column name (starts with i)
  col_name <- grep(paste0("^", i), colnames(data), value = TRUE)[1]
  
  if (!is.na(col_name)) {
    # comp
    df_comp <- data %>%
      select(all_of(c(col_name, "comp_mean"))) %>%
      group_by(across(all_of(col_name))) %>%
      summarise(mean = mean(comp_mean, na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(variable = i, dimension = "comp") %>%
      rename(variable_value = 1)
    
    means_df <- bind_rows(means_df, df_comp)
    
    # VALUES
    df_values <- data %>%
      select(all_of(c(col_name, "values_mean"))) %>%
      group_by(across(all_of(col_name))) %>%
      summarise(mean = mean(values_mean,na.rm = TRUE), n = n(), .groups = "drop") %>%
      mutate(variable = i, dimension = "values") %>%
      rename(variable_value = 1)
    
    means_df <- bind_rows(means_df, df_values)
  } else {
    warning(paste("No column found starting with", i))
  }
}

# Clean up
means_df <- means_df %>%
  filter(!is.na(mean)) %>%
  filter(!variable_value %in% c("Prefer not to answer", "Don't know")) %>%
  # Add labels using your disaggregation_levels
  left_join(disaggregation_levels, by = c("variable" = "variable")) %>%
  mutate(variable = stringr::str_wrap(short_label, 10)) %>%
  select(-short_label)

# Optional: Reorder levels
means_df <- means_df %>%
  mutate(variable = factor(variable, levels = stringr::str_wrap(disaggregation_levels$short_label, 10)))

# This chunk standardises the weight variable (or sets it to 1 if missing),
# then computes unweighted and weighted mean scores per question across pillars.

if ("weight" %in% names(data)) {
  data$weight <- as.numeric(data$weight)
  
} else if ("_weight" %in% names(data)) {
  data$weight <- as.numeric(data$`_weight`)
  
} else {
  data$weight <- 1
}


df<-data %>% dplyr::select(c(values,comp,weight))


#NO WEIGHT

df2 <-df %>%
  pivot_longer(!weight, names_to = "question", values_to = "value")%>%
  na.omit() %>%
  dplyr::group_by(question) %>%
  dplyr::summarise(unweighted=mean(value))


#WEIGHT

df3 <-df%>%
  pivot_longer(!weight, names_to = "question", values_to = "value")%>%
  na.omit()%>%
  dplyr::group_by(question)%>%
  dplyr::summarise(count=n(),weighted=mean(value*weight/sum(weight))*count)%>%
  left_join(df2) %>% select(-count)

df3  <- df3 %>%
  left_join(drivers_lookup, by = c("question" = "Variables")) %>%
  select(-question) %>%
  select(Dimension, Drivers, everything())

# This chunk splits data into defined groups, computes weighted/unweighted
# scores by question and group, and builds overall, dimension- and index-level
# score summaries for reporting.

# Defining groups for scoring

if (!is.null(group_map) && nrow(group_map) > 0) {
  
  group_map_valid <- group_map |>
    dplyr::filter(!is.na(group_col), group_col != "")
  
  if (nrow(group_map_valid) > 0) {
    
    group_cols <- unique(as.character(group_map_valid$group_col)) # Check unique value from group_cols
    group_cols <- group_cols[nzchar(group_cols)] # Check if group_cols is not empty
    group_cols <- intersect(group_cols, names(data))  # Intersection between group_cols and data columns name
    
    if (length(group_cols) == 0) {
      warning("No valid group_col columns found in data; skipping grouping.")
    } else {
      
      # ALL columns must be non-missing and non-empty
      valid_all <- Reduce(`|`, lapply(group_cols, function(col) {
        !is.na(data[[col]]) & data[[col]] != ""
      }))
      
      in_any_group <- rep(FALSE, nrow(data))
      
      for (i in seq_len(nrow(group_map_valid))) {
        row <- group_map_valid[i, , drop = FALSE]
        
        grp_col <- as.character(row$group_col)
        grp_val <- as.character(row$group_value)
        
        if (!nzchar(grp_col) || !(grp_col %in% names(data))) {
          warning(sprintf(
            "Skipping group_id=%s: column '%s' not in data",
            as.character(row$group_id), grp_col
          ))
          next
        }
        
        idx_group <- !is.na(data[[grp_col]]) & (data[[grp_col]] == grp_val)
        
        subset_data <- data[idx_group & valid_all, , drop = FALSE]
        assign(paste0("data_", row$group_id), subset_data, envir = .GlobalEnv)
        
        in_any_group <- in_any_group | idx_group
      }
      
      subset_others <- data[valid_all & !in_any_group, , drop = FALSE]
      assign("data_grp_other", subset_others, envir = .GlobalEnv)
    }
  }
}


# Scoring by group

compute_weighted <- function(data, group_label) {
  data %>%
    select(all_of(c(values, comp, "weight"))) %>%
    pivot_longer(!weight, names_to = "question", values_to = "value") %>%
    na.omit() %>%
    group_by(question) %>%
    summarise(
      !!group_label := sum(value * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .groups = "drop"
    )
}

compute_unweighted <- function(data, group_label) {
  data %>%
    select(all_of(c(values, comp))) %>%  # No weight column
    pivot_longer(everything(), names_to = "question", values_to = "value") %>%
    na.omit() %>%
    group_by(question) %>%
    summarise(
      !!group_label := mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}


# Apply weight for each subgroup
overall_weight   <- compute_weighted(df, "weighted")
overall_unweight <- compute_unweighted(df, "Unweighted")

# Only loop over groups if group_map is not NULL / empty
if (!is.null(group_map) && nrow(group_map) > 0) {
  for (i in seq_len(nrow(group_map))) {
    row <- group_map[i, ]
    df_name  <- paste0("data_", row$group_id)
    out_name <- paste0(row$group_id, "_df") 
    
    if (!exists(df_name)) next
    
    df_group <- get(df_name)
    
    assign(out_name, compute_weighted(df_group, row$group_label))
  }
}

if (exists("data_grp_other")) {
  grp_other_df        <- compute_weighted(data_grp_other, "Others")
}


# Merge all into one wide dataframe by question
df_list <- list(overall_weight)

if (!is.null(group_map) && nrow(group_map) > 0) {
  for (i in seq_len(nrow(group_map))) {
    row    <- group_map[i, ]
    df_var <- paste0(row$group_id, "_df") 
    
    if (exists(df_var)) {
      df_list[[length(df_list) + 1]] <- get(df_var)
    }
  }
}

if (exists("grp_other_df")) {
  df_list[[length(df_list) + 1]] <- grp_other_df
}

df_list[[length(df_list) + 1]] <- overall_unweight
df_list <- df_list[!vapply(df_list, is.null, logical(1))]


df_group <- reduce(df_list, full_join, by = "question")%>%
  left_join(drivers_lookup, by = c("question" = "Variables"))


# Generating the Score summary

score_cols <- c("weighted", group_map$group_label, "Others")
summary <- df_group %>% select(Dimension, Drivers, any_of(score_cols))

metric_cols <- setdiff(names(summary), c("Dimension", "Drivers"))

dim_scores <- summary %>%
  group_by(Dimension) %>%
  summarise(
    across(
      all_of(metric_cols),
      ~ mean(.x, na.rm = TRUE),     # mean = sum / n, same intent as before
      .names = "{.col}"
    ),
    .groups = "drop"
  )

dimension_scores <- dim_scores %>%
  mutate(Drivers = "Overall") %>%
  relocate(Drivers, .after = Dimension)

index_overall <- dim_scores %>%
  summarise(across(all_of(metric_cols), ~ mean(.x, na.rm = TRUE))) %>%
  mutate(
    Dimension = "INST INDEX",
    Drivers   = "Overall"
  ) %>%
  relocate(Dimension, Drivers)

index_drivers <- summary %>%
  group_by(Drivers) %>%
  summarise(
    across(
      all_of(metric_cols),
      ~ mean(.x, na.rm = TRUE),           # average per driver
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  mutate(Dimension = "INST INDEX") %>%     
  relocate(Dimension, .before = Drivers)   

summary_2 <- bind_rows(
  summary,          
  dimension_scores,
  index_overall 
)
summary_2 <- summary_2 %>%
  rename(Overall = weighted)

df4 <- df_group %>%
  select(Dimension, question = Drivers, any_of(score_cols))

## Significant test

list_dim <- c(comp, values)

p_values_df <- data.frame()

for (i in list_dim) {
  
  if (!(exists("data_grp_1") && exists("data_grp_2") && exists("data_grp_other"))) {
    next
  }
  
  group1_data <- data_grp_1[[i]]
  group2_data <- data_grp_2[[i]]
  group3_data <- data_grp_other[[i]]
  
  group1_data <- group1_data[!is.na(group1_data)]
  group2_data <- group2_data[!is.na(group2_data)]
  group3_data <- group3_data[!is.na(group3_data)]
  
  # If any group has fewer than 2 observations, skip this variable
  if (
    length(group1_data) < 2 ||
    length(group2_data) < 2 ||
    length(group3_data) < 2
  ) {
    next
  }
  
  # Perform t-tests
  t_test_result12 <- t.test(group1_data, group2_data)
  t_test_result13 <- t.test(group1_data, group3_data)
  t_test_result23 <- t.test(group3_data, group2_data)
  
  # Column name
  column1 <- i
  
  # Add results to the data frame
  p_values_df <- rbind(
    p_values_df,
    data.frame(
      Column1 = column1,
      P12 = t_test_result12$p.value,
      P13 = t_test_result13$p.value,
      P23 = t_test_result23$p.value,
      mean12 = round(
        abs(
          t_test_result12$estimate[["mean of x"]] -
            t_test_result12$estimate[["mean of y"]]
        ),
        2
      ),
      mean13 = round(
        abs(
          t_test_result13$estimate[["mean of x"]] -
            t_test_result13$estimate[["mean of y"]]
        ),
        2
      ),
      mean23 = round(
        abs(
          t_test_result23$estimate[["mean of x"]] -
            t_test_result23$estimate[["mean of y"]]
        ),
        2
      )
    )
  )
}


# ------------------------------------------------------------
# Prepare significance results
# ------------------------------------------------------------

p_values_df <- pivot_longer(
  p_values_df,
  !Column1
)

p_values_df <- p_values_df %>%
  mutate(
    number = gsub("\\D", "", name)
  )

meansP <- p_values_df %>%
  dplyr::filter(grepl("mean", name)) %>%
  dplyr::rename(mean = value) %>%
  select(-name)


# ------------------------------------------------------------
# Translate respondent group labels
# ------------------------------------------------------------

labels <- group_map$group_label
names(labels) <- group_map$group_id

labels <- vapply(
  labels,
  tr_group_label,
  character(1)
)

# "Others" is a data/profile label, not a technical variable
others_label <- tr_data("Others")

pair_labels <- c(
  paste0(labels["grp_1"], " - ", labels["grp_2"]),
  paste0(labels["grp_1"], " - ", others_label),
  paste0(labels["grp_2"], " - ", others_label)
)

names(pair_labels) <- c(
  "P12",
  "P13",
  "P23"
)


# ------------------------------------------------------------
# Reshape and translate significance table
# ------------------------------------------------------------

sig_reshaped <- p_values_df %>%
  dplyr::filter(grepl("^P", name)) %>%
  mutate(
    name = recode(name, !!!pair_labels)
  ) %>%
  left_join(
    meansP,
    by = c("Column1", "number")
  ) %>%
  mutate(
    value = p.adjust(
      value,
      method = "BH",
      n = length(value)
    ),
    sig = ifelse(
      value < 0.05,
      "Yes",
      "No"
    ),
    name = as.factor(name)
  ) %>%
  select(
    Column1,
    sig,
    name
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = sig
  ) %>%
  left_join(
    drivers_lookup,
    by = c("Column1" = "Variables")
  ) %>%
  select(
    Dimension,
    Drivers,
    everything(),
    -Column1
  ) %>%
  mutate(
    Dimension = tr_variable(Dimension),
    Drivers = tr_variable(Drivers),
    across(
      -c(Dimension, Drivers),
      ~ tr_data(.x)
    )
  )


# ------------------------------------------------------------
# Formatted significance table
# ------------------------------------------------------------

tab_sig_reshaped <- formattable(
  sig_reshaped,
  align = rep("c", ncol(sig_reshaped))
)

message("✓ Scores computed")


message("✓ Data preparation and score computation completed")