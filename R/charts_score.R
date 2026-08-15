##########################################################
#.  COMMUNITY TRUST INDEX - CHARTS
##########################################################


# ============================================================
# CTI Report - charts.R
# All chart, visual table and chart-preparation chunks
# Extracted from Data-Report-INST.Rmd
# ============================================================

message(tr("runtime.loading_charts"))

# This script assumes the following scripts have already run:
# source('R/setup.R')
# source('R/read_config.R')
# source('R/load_data.R')
# source('R/prepare_data.R')
# source('R/compute_score.R')


# ------------------------------------------------------------
# Chart parameters
# ------------------------------------------------------------

# ---- Original chunk: chart_parameters ----
# This chunk checks which groups of questions are available in the codebook
# (Relationship with RC, behaviours, intention and other contextual modules) and creates logical
# flags. These flags are later used to decide which charts, maps and sections
# should be displayed in the report based on the data that actually exists.


# Caption
# Preparing a caption with all excluded terms
caption_no_str <- switch(
  length(display_no),
  paste0("'", display_no[1], "'"),
  paste0("'", display_no[1], "' and '", display_no[2], "'"),
  paste0(
    paste0("'", display_no[-length(display_no)], "'", collapse = ", "),
    ", and '", display_no[length(display_no)], "'"
  ) 
)



# ------------------------------------------------------------
# Score charts
# ------------------------------------------------------------

# ---- Original chunk: overall_score ----
# Data preparation
## Formatting

comp_df <- df4 %>% filter(Dimension == "Competency") %>% select(question, weighted)
values_df <- df4 %>% filter(Dimension == "Value") %>% select(question, weighted)

# Calculate means
tmean <- mean(comp_df$weighted, na.rm = TRUE)
vmean <- mean(values_df$weighted, na.rm = TRUE)

# Harmonize number of subdimensions
n_comp <- nrow(comp_df)
n_values <- nrow(values_df)

if (n_comp < n_values) {
  comp_df <- comp_df %>%
    add_row(question = rep("-", n_values - n_comp), weighted = NA, ,.before = 1)
} else if (n_values < n_comp) {
  values_df <- values_df %>%
    add_row(question = rep("-", n_comp - n_values), weighted = NA, .before = 1)
}

# Step 2: Add a separator row
comp_df  <- comp_df %>% add_row(question = " ", weighted = NA)
values_df <- values_df %>% add_row(question = "  ", weighted = NA)

# Step 3: Add "Overall" rows
comp_df <- comp_df %>% add_row(question = "Overall Competencies", weighted = tmean)
values_df <- values_df %>% add_row(question = "Overall Values", weighted = vmean)


## Sorting logic
# For Competency
comp_df <- comp_df %>%
  mutate(
    question = as.character(question),  # Ensure it's character first
    question = factor(
      question,
      levels = c(
        "-" ,
        sort(setdiff(question, c("Overall Competencies", " ", "-")), decreasing  = TRUE),
        " ",  # separators,
        "Overall Competencies"
        
        
      )
    )
  )

# For Value
values_df <- values_df %>%
  mutate(
    question = as.character(question),
    question = factor(
      question,
      levels = c(
        "-" ,
        sort(setdiff(question, c("Overall Values", "  ", "-")), decreasing  = TRUE),
        "  ",  # separator,
        "Overall Values"
      )
    )
  )

#########################
# Plots
#########################


comp_plot <- ggplot(comp_df,aes(x=question,y=weighted)) +
  geom_bar(stat="identity", fill = color_competencies) +
  geom_text(aes(label=round(weighted,2),y=round(weighted,2)-1),color="white",size=6) +
  coord_flip() +
  custom_theme() +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  labs(y = NULL, x = NULL, title = tr("score.competencies")) + 
  scale_y_continuous(limits = c(0, 10))



values_plot <- ggplot(values_df,aes(x=question,y=weighted)) +
  geom_bar(stat="identity", fill = color_values) +
  geom_text(aes(label=round(weighted,2),y=round(weighted,2)-1),color="white",size=6) +
  coord_flip() +
  custom_theme() +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  labs(y = NULL, x = NULL, title = tr("score.values")) +
  scale_y_continuous(limits = c(0, 10))




# ---- Original chunk: scoring_factors ----
# Charts

# filtering dataset
factor_data <- means_df %>% 
  filter(
    n > 10,
    !is.na(variable_value)
  )

n_strip_rows <- nrow(factor_data)
height_strip_plot <- n_strip_rows * 0.3 + 2

# row headers
strip <- ggplot(
  data = factor_data,
  aes(
    x = mean,
    y = stringr::str_wrap(tr_data(variable_value), 20)
  )
) +
  geom_bar(stat = "identity", fill = color_bg) +
  geom_text(
    aes(label = round(mean, 2), x = mean - 1),
    color = color_bg,
    size = 7
  ) +
  facet_grid(
    variable ~ .,
    scales = "free",
    space = "free",
    labeller = labeller(variable = function(x) {
      stringr::str_wrap(
        tr_variable(x),
        width = 10
      )
    })
  ) +
  custom_theme() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "white"),
    panel.grid = element_blank(),
    panel.spacing.x = unit(1, "pt"),
    panel.spacing.y = unit(0.5, "lines"),
    panel.background = element_blank(),
    strip.text.y = element_text(
      size = 14,
      angle = 0,
      margin = margin(t = 10, r = 10, b = 10, l = 10)
    )
  ) +
  labs(x = NULL, y = NULL, title = " ")

# Competencies
co <- ggplot(
  data = factor_data %>% filter(dimension == "comp"),
  aes(x = mean, y = stringr::str_wrap(tr_data(variable_value), 20))
) +
  geom_bar(stat = "identity", fill = color_competencies) +  # Set the fill color for bars
  geom_text(aes(label = round(mean, 2), x = mean - 1), color = "white", size = 6) +
  facet_grid(variable ~ ., scales = "free", space = "free") +
  custom_theme() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "pt"),
        panel.spacing.y = unit(0.5, "lines"),
        strip.text.y = element_blank(),
        strip.background = element_blank()) +
  scale_x_continuous(limits=c(0,10))+
  labs(x = NULL, y = NULL, title = tr("score.competencies"))

# Values
va <- ggplot(
  data = factor_data %>% filter(dimension == "values"),
  aes(x = mean, y = stringr::str_wrap(tr_data(variable_value), 20))
) +
  geom_bar(stat = "identity", fill = color_values) +  # Set the fill color for bars
  geom_text(aes(label = round(mean, 2), x = mean - 1), color = "white", size = 6) +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
  custom_theme() +
  theme(legend.position = "none",
        panel.spacing.x = unit(1, "pt"),
        panel.spacing.y = unit(0.5, "lines"),
        strip.text.y = element_blank() , 
        strip.background = element_blank()) +
  scale_x_continuous(limits=c(0,10))+
  labs(x = NULL, y = NULL, title = tr("score.values"))



# ---- Original chunk: scoring_profile ----
# Data preparation

cols_to_pivot <- intersect(group_map$group_label, names(summary_2))
cols_to_pivot <- c(cols_to_pivot, "Others")

score_data <- summary_2 %>%
  filter(!is.na(Drivers)) %>%
  pivot_longer(
    cols      = all_of(cols_to_pivot),  # <-- dynamic columns from group_map
    names_to  = "Group",
    values_to = "Mean"
  ) %>%
  mutate(
    Group    = factor(Group, levels = cols_to_pivot),
    Question = Drivers
  )


comp_plot_data <- score_data %>% filter(Dimension == "Competency")
val_plot_data  <- score_data %>% filter(Dimension == "Value")

comp_plot_data$Question <- factor(comp_plot_data$Question, levels = unique(comp_plot_data$Question))
val_plot_data$Question  <- factor(val_plot_data$Question, levels = unique(val_plot_data$Question))


# Palette
pal_val_score <- grDevices::colorRampPalette(color_gradient_1)(length(unique(comp_plot_data$Group)))
pal_comp_score <- grDevices::colorRampPalette(color_gradient_2)(length(unique(val_plot_data$Group)))

# Plots
# Competencies chart
c_plot <- ggplot(comp_plot_data, aes(x = Question, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width =0.9), width = 0.9) +
  geom_text(aes(label = round(Mean, 2), y = Mean), 
            position = position_dodge(width = 0.9), 
            hjust = -0.3, size = 4, color = "#262626") +
  coord_flip() +
  scale_fill_manual(
    values = pal_comp_score,
    labels = tr_group_label
  ) +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  custom_theme() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = NULL, y = NULL, title = tr("score.competencies")) +
  theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_blank())

# Values chart
v_plot <- ggplot(val_plot_data, aes(x = Question, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.9) +
  geom_text(aes(label = round(Mean, 2), y = Mean), 
            position = position_dodge(width = 0.9), 
            hjust = -0.3, size = 4, color = "#262626") +
  coord_flip() +
  scale_fill_manual(
    values = pal_val_score,
    labels = tr_group_label
  ) +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  custom_theme() +
  scale_y_continuous(limits = c(0, 10)) +
  labs(x = NULL, y = NULL, title = tr("score.values")) +
  theme(legend.position = "bottom", legend.direction = "vertical", legend.title = element_blank())

# Display side-by-side




message("✓ ", tr("runtime.charts_loaded"))