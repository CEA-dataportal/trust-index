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

# ---- Original chunk: weighting_new ----
## Weighting comparison chart

df_long <- df3 %>%
  reshape2::melt(
    id.vars      = c("Dimension", "Drivers"),  # keep both as identifiers
    variable.name = "variable",
    value.name    = "value"
  )

weighting_plot <- ggplot(df_long, aes(x = Drivers, y = value, group = variable, color = variable)) +
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



message("✓ Charts and visual outputs loaded")
