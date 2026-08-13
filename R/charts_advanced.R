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

# ---- Original chunk: weighting_new ----
## Weighting comparison chart

df_long <- df3 %>%
  reshape2::melt(
    id.vars       = c("Dimension", "Drivers"),
    variable.name = "variable",
    value.name    = "value"
  ) %>%
  mutate(
    # Keep original values for calculations/grouping
    Drivers_display = tr_variable(as.character(Drivers)),
    Dimension_display = tr_variable(as.character(Dimension)),
    variable_display = tr_variable(as.character(variable))
  )

weighting_plot <- ggplot(
  df_long,
  aes(
    x = Drivers_display,
    y = value,
    group = variable_display,
    color = variable_display
  )
) +
  geom_point() +
  geom_line() +
  geom_label(
    aes(
      label = round(value, 2),
      fill = variable_display
    ),
    color = "white",
    label.size = 0,
    label.r = unit(0.2, "lines"),
    label.padding = unit(2, "pt"),
    size = 4
  ) +
  facet_wrap(
    ~ Dimension_display,
    nrow = 1,
    scales = "free_x"
  ) +
  theme(
    axis.text.x = element_text(
      color = "black",
      size = 12,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = element_text(
      margin = margin(t = 15),
      size = 16
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),
      size = 16
    ),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank()
  ) +
  custom_theme() +
  labs(
    x = tr("score.drivers"),
    y = tr("score.score_variation")
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
  mutate(
    short_label_display = tr_variable(
      variable,
      label = "short",
      fallback = short_label
    )
  ) %>%
  select(variable, short_label_display) %>%
  deframe()


# Convert response values to numeric scores before correlation/weighting
score_driver_response <- function(x) {
  
  x_chr <- as.character(x)
  
  mapped <- unname(score_map_full[x_chr])
  
  numeric_value <- suppressWarnings(
    as.numeric(x_chr)
  )
  
  dplyr::if_else(
    !is.na(mapped),
    as.numeric(mapped),
    numeric_value
  )
}


survey_drivers <- data %>%
  select(all_of(c(driver_columns, "weight"))) %>%
  mutate(
    weight = as.numeric(weight),
    across(
      all_of(driver_columns),
      score_driver_response
    )
  ) %>%
  rename_with(
    ~ driver_rename_map[.x],
    .cols = all_of(driver_columns)
  )

survey_unweighted <- survey_drivers

survey_weighted <- survey_drivers %>%
  mutate(
    across(
      -weight,
      ~ .x * weight
    )
  )

driver_unweighted <- survey_unweighted %>%
  select(-weight)
driver_weighted <- survey_weighted %>%
  select(-weight)

testRes = cor.mtest(driver_unweighted, conf.level = 0.95)
cor_matrix_uw <- cor(driver_unweighted, use = "pairwise.complete.obs", method = "pearson")

cor_matrix_w <- cor(driver_weighted, use = "pairwise.complete.obs", method = "pearson")



message("✓ ", tr("runtime.charts_loaded"))