##########################################################
#. COMMUNITY TRUST INDEX - SCORE CHARTS
##########################################################

message("Loading score charts...")

# Shared score chart engine for Institutional and EWS.
# Requires: module config + palette, setup.R, read_config.R,
# load_data.R, translation_v2.R and analysis.R.

# ============================================================
# 1. Module helpers
# ============================================================

score_keys <- names(score_prefixes)

score_title_map <- if (toupper(module_code) == "INST") {
  c(
    comp   = tr("score.competencies"),
    values = tr("score.values")
  )
} else {
  c(
    disaster      = "Disaster Risk Knowledge",
    detection     = "Detection, Monitoring & Forecasting",
    dissemination = "Warning Dissemination & Communication",
    response      = "Preparedness & Response Capabilities"
  )
}

for (i in seq_along(score_keys)) {
  key <- score_keys[i]
  if (!(key %in% names(score_title_map))) {
    score_title_map[key] <- score_dimensions[i]
  }
}

score_color_map <- if (toupper(module_code) == "INST") {
  c(
    comp   = color_competencies,
    values = color_values
  )
} else {
  stats::setNames(
    palette_module[seq_along(score_keys)],
    score_keys
  )
}

score_light_map <- if (toupper(module_code) == "INST") {
  c(
    comp   = color_primary_10,
    values = color_secondary_10
  )
} else {
  c(
    disaster      = color_primary_10,
    detection     = color_secondary_10,
    dissemination = color_tertiary_10,
    response      = color_quaternary_10
  )
}

score_title <- function(key) {
  label <- unname(score_title_map[key])
  if (length(label) == 0L || is.na(label) || !nzchar(label)) key else label
}


# ============================================================
# 2. Overall score charts
# ============================================================

make_overall_score_data <- function(dimension_label) {
  out <- df4 %>%
    dplyr::filter(tolower(Dimension) == tolower(dimension_label)) %>%
    dplyr::select(question, weighted)

  if (nrow(out) == 0L) return(NULL)

  dimension_mean <- mean(out$weighted, na.rm = TRUE)

  out <- out %>%
    dplyr::mutate(question = as.character(question)) %>%
    dplyr::add_row(question = " ", weighted = NA_real_) %>%
    dplyr::add_row(question = "Overall", weighted = dimension_mean)

  driver_levels <- sort(
    setdiff(as.character(out$question), c("Overall", " ")),
    decreasing = TRUE
  )

  out %>%
    dplyr::mutate(
      question = factor(
        question,
        levels = c(driver_levels, " ", "Overall")
      )
    )
}

make_overall_score_plot <- function(plot_df, key) {
  if (is.null(plot_df) || nrow(plot_df) == 0L) return(NULL)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = question, y = weighted)) +
    ggplot2::geom_col(fill = unname(score_color_map[key])) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(is.na(weighted), "", round(weighted, 2)),
        y = weighted - 1
      ),
      colour = "white",
      size = 6,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip() +
    custom_theme() +
    ggplot2::scale_y_continuous(limits = c(0, 10)) +
    ggplot2::scale_x_discrete(
      labels = function(x) {
        z <- x
        idx <- !(z %in% c(" ", "Overall"))
        z[idx] <- tr_variable(z[idx])
        z[z == "Overall"] <- tr("common.overall")
        z
      }
    ) +
    ggplot2::labs(x = NULL, y = NULL, title = score_title(key))
}

overall_score_data  <- list()
overall_score_plots <- list()

for (i in seq_along(score_keys)) {
  key <- score_keys[i]
  dimension_label <- score_dimensions[i]

  plot_df <- make_overall_score_data(dimension_label)
  plot_obj <- make_overall_score_plot(plot_df, key)

  overall_score_data[[key]]  <- plot_df
  overall_score_plots[[key]] <- plot_obj

  assign(paste0(key, "_df"), plot_df, envir = .GlobalEnv)
  assign(paste0(key, "_plot"), plot_obj, envir = .GlobalEnv)
}

# Current Institutional Rmd aliases
if (toupper(module_code) == "INST") {
  comp_df     <- overall_score_data$comp
  values_df   <- overall_score_data$values
  comp_plot   <- overall_score_plots$comp
  values_plot <- overall_score_plots$values
}

# Current EWS naming
if (toupper(module_code) == "EWS") {
  ews_keys <- c("disaster", "detection", "dissemination", "response")
  for (i in seq_along(ews_keys)) {
    key <- ews_keys[i]
    if (key %in% names(overall_score_plots) && !is.null(overall_score_plots[[key]])) {
      assign(paste0("pillar", i, "_df"), overall_score_data[[key]], envir = .GlobalEnv)
      assign(paste0("pillar", i, "_plot"), overall_score_plots[[key]], envir = .GlobalEnv)
    }
  }
}



# ============================================================
# 3. Combined pillar score - EWS only
# ============================================================

# This chart is specific to the EWS module.
# It shows the mean score for each driver across the pillars in which
# that driver is assessed, while the stacked segments show each pillar's
# contribution to that mean.

combined_pillar_plot <- NULL
p_summary_stack <- NULL
all_pillars_df <- NULL
allpillars_plot_df <- NULL
allpillars_totals_lab <- NULL

if (toupper(module_code) == "EWS") {

  ews_pillar_keys <- c(
    "disaster",
    "detection",
    "dissemination",
    "response"
  )

  ews_pillar_labels <- c(
    disaster      = "Pillar 1",
    detection     = "Pillar 2",
    dissemination = "Pillar 3",
    response      = "Pillar 4"
  )

  ews_pillar_dimensions <- c(
    disaster      = "Disaster",
    detection     = "Detection",
    dissemination = "Dissemination",
    response      = "Response"
  )

  # Keep only pillars actually available in the current survey.
  available_pillars <- ews_pillar_keys[
    ews_pillar_dimensions[ews_pillar_keys] %in% unique(df4$Dimension)
  ]

  if (length(available_pillars) > 0L) {

    pillar_dfs <- lapply(
      available_pillars,
      function(key) {

        df4 %>%
          dplyr::filter(
            Dimension == ews_pillar_dimensions[[key]],
            !is.na(question),
            !is.na(weighted)
          ) %>%
          dplyr::transmute(
            question = question,
            weighted = weighted,
            dimension = ews_pillar_labels[[key]]
          )
      }
    )

    all_pillars_df <- dplyr::bind_rows(pillar_dfs)

    if (nrow(all_pillars_df) > 0L) {

      # Mean driver score across the pillars in which the driver exists.
      # The stacked segments are rescaled so the full bar equals that mean.
      allpillars_plot_df <- all_pillars_df %>%
        dplyr::group_by(question) %>%
        dplyr::mutate(
          mean_total = mean(weighted, na.rm = TRUE),
          total = sum(weighted, na.rm = TRUE),
          weighted_share = dplyr::if_else(
            total > 0,
            weighted / total,
            0
          ),
          weighted_for_stack = mean_total * weighted_share
        ) %>%
        dplyr::ungroup()

      allpillars_totals_lab <- allpillars_plot_df %>%
        dplyr::distinct(question, mean_total) %>%
        dplyr::mutate(
          label_y = mean_total,
          label_text = sprintf("Avg.: %.2f", mean_total)
        )

      y_limit <- max(
        allpillars_totals_lab$mean_total,
        na.rm = TRUE
      ) * 1.15

      # Use the module palette and keep the pillar order stable.
      combined_colors <- stats::setNames(
        palette_module[seq_along(ews_pillar_keys)],
        ews_pillar_labels[ews_pillar_keys]
      )

      combined_pillar_plot <- ggplot2::ggplot(
        allpillars_plot_df,
        ggplot2::aes(
          x = question,
          y = weighted_for_stack,
          fill = dimension
        )
      ) +
        ggplot2::geom_col(
          width = 0.7,
          colour = "white"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = round(weighted, 2)
          ),
          position = ggplot2::position_stack(vjust = 0.5),
          colour = "white",
          size = 4
        ) +
        ggplot2::geom_label(
          data = allpillars_totals_lab,
          ggplot2::aes(
            x = question,
            y = label_y,
            label = label_text
          ),
          inherit.aes = FALSE,
          size = 5,
          fill = "white",
          alpha = 0.8,
          colour = color_label_bl,
          fontface = "bold",
          linewidth = 0,
          label.padding = grid::unit(4, "pt"),
          label.r = grid::unit(3, "pt")
        ) +
        ggplot2::scale_fill_manual(
          values = combined_colors,
          breaks = unname(
            ews_pillar_labels[available_pillars]
          ),
          name = NULL
        ) +
        ggplot2::scale_x_discrete(
          labels = function(x) tr_variable(x)
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, y_limit),
          expand = ggplot2::expansion(
            mult = c(0, 0.05)
          )
        ) +
        custom_theme() +
        ggplot2::theme(
          axis.ticks = ggplot2::element_blank(),
          plot.margin = ggplot2::margin(
            10,
            20,
            10,
            10
          )
        ) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = "Combined Scores by Drivers",
          subtitle = "Mean score by driver, disaggregated by pillar contribution"
        )

      # Legacy alias used by the previous EWS Rmd.
      p_summary_stack <- combined_pillar_plot
    }
  }
}


# ============================================================
# 4. Score by factors
# ============================================================

factor_data <- means_df %>%
  dplyr::filter(n > 10, !is.na(variable_value))

n_strip_rows <- nrow(factor_data)
height_strip_plot <- n_strip_rows * 0.3 + 2

strip <- ggplot2::ggplot(
  factor_data,
  ggplot2::aes(x = mean, y = stringr::str_wrap(variable_value, 20))
) +
  ggplot2::geom_col(fill = color_bg) +
  ggplot2::geom_text(
    ggplot2::aes(label = round(mean, 2), x = mean - 1),
    colour = color_bg,
    size = 7
  ) +
  ggplot2::facet_grid(variable ~ ., scales = "free", space = "free") +
  custom_theme() +
  ggplot2::theme(
    legend.position = "none",
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(color = "white"),
    panel.grid = ggplot2::element_blank(),
    panel.spacing.x = grid::unit(1, "pt"),
    panel.spacing.y = grid::unit(0.5, "lines"),
    panel.background = ggplot2::element_blank(),
    strip.text.y = ggplot2::element_text(
      size = 14,
      angle = 0,
      margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
    )
  ) +
  ggplot2::labs(x = NULL, y = NULL, title = " ")

make_factor_plot <- function(key) {
  df_key <- factor_data %>% dplyr::filter(dimension == key)
  if (nrow(df_key) == 0L) return(NULL)

  ggplot2::ggplot(
    df_key,
    ggplot2::aes(x = mean, y = stringr::str_wrap(variable_value, 20))
  ) +
    ggplot2::geom_col(fill = unname(score_color_map[key])) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(mean, 2), x = mean - 1),
      colour = "white",
      size = 6
    ) +
    ggplot2::facet_grid(variable ~ ., scales = "free_y", space = "free_y") +
    custom_theme() +
    ggplot2::theme(
      legend.position = "none",
      panel.spacing.x = grid::unit(1, "pt"),
      panel.spacing.y = grid::unit(0.5, "lines"),
      strip.text.y = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 10)) +
    ggplot2::labs(x = NULL, y = NULL, title = score_title(key))
}

factor_score_plots <- purrr::map(score_keys, make_factor_plot)
names(factor_score_plots) <- score_keys

for (key in score_keys) {
  assign(paste0("factor_", key, "_plot"), factor_score_plots[[key]], envir = .GlobalEnv)
}

# Institutional aliases used by current Rmd
if (toupper(module_code) == "INST") {
  co <- factor_score_plots$comp
  va <- factor_score_plots$values
}

# EWS aliases
if (toupper(module_code) == "EWS") {
  p1_factor_plot <- factor_score_plots$disaster
  p2_factor_plot <- factor_score_plots$detection
  p3_factor_plot <- factor_score_plots$dissemination
  p4_factor_plot <- factor_score_plots$response
}


# ============================================================
# 5. Score by respondent profile
# ============================================================

profile_labels <- if (!is.null(group_map) && nrow(group_map) > 0L) {
  group_map$group_label
} else {
  character(0)
}

cols_to_pivot <- intersect(profile_labels, names(summary_2))
if ("Others" %in% names(summary_2)) cols_to_pivot <- c(cols_to_pivot, "Others")
cols_to_pivot <- unique(cols_to_pivot)

score_data <- NULL
profile_score_plots <- list()

if (length(cols_to_pivot) > 0L) {
  score_data <- summary_2 %>%
    dplyr::filter(!is.na(Drivers), Dimension != index_name) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_to_pivot),
      names_to = "Group",
      values_to = "Mean"
    ) %>%
    dplyr::mutate(
      Group = factor(Group, levels = cols_to_pivot),
      Question = Drivers
    )

  make_profile_plot <- function(key, dimension_label) {
    plot_data <- score_data %>%
      dplyr::filter(tolower(Dimension) == tolower(dimension_label))

    if (nrow(plot_data) == 0L) return(NULL)

    plot_data <- plot_data %>%
      dplyr::mutate(
        Question = factor(Question, levels = unique(Question))
      )

    n_groups <- length(unique(plot_data$Group))
    base_color <- unname(score_color_map[key])
    light_color <- unname(score_light_map[key])
    if (length(light_color) == 0L || is.na(light_color)) light_color <- "#E5E5E5"

    profile_palette <- grDevices::colorRampPalette(
      c(base_color, light_color)
    )(max(1L, n_groups))

    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Question, y = Mean, fill = Group)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.9),
        width = 0.9
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = round(Mean, 2), y = Mean),
        position = ggplot2::position_dodge(width = 0.9),
        hjust = -0.3,
        size = 4,
        colour = color_label_bl
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = profile_palette,
        labels = function(x) tr_group_label(x)
      ) +
      ggplot2::scale_x_discrete(labels = function(x) tr_variable(x)) +
      custom_theme() +
      ggplot2::scale_y_continuous(limits = c(0, 10)) +
      ggplot2::labs(x = NULL, y = NULL, title = score_title(key)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.title = ggplot2::element_blank()
      )
  }

  for (i in seq_along(score_keys)) {
    key <- score_keys[i]
    profile_score_plots[[key]] <- make_profile_plot(key, score_dimensions[i])
    assign(
      paste0("profile_", key, "_plot"),
      profile_score_plots[[key]],
      envir = .GlobalEnv
    )
  }
}

# Current Institutional Rmd aliases
if (toupper(module_code) == "INST" && !is.null(score_data)) {
  comp_plot_data <- score_data %>% dplyr::filter(Dimension == "Competency")
  val_plot_data  <- score_data %>% dplyr::filter(Dimension == "Value")
  c_plot <- profile_score_plots$comp
  v_plot <- profile_score_plots$values
}

# EWS profile aliases
if (toupper(module_code) == "EWS" && !is.null(score_data)) {
  p1_score <- score_data %>% dplyr::filter(Dimension == "Disaster")
  p2_score <- score_data %>% dplyr::filter(Dimension == "Detection")
  p3_score <- score_data %>% dplyr::filter(Dimension == "Dissemination")
  p4_score <- score_data %>% dplyr::filter(Dimension == "Response")

  p1_profile_plot <- profile_score_plots$disaster
  p2_profile_plot <- profile_score_plots$detection
  p3_profile_plot <- profile_score_plots$dissemination
  p4_profile_plot <- profile_score_plots$response
}


# ============================================================
# 5. Availability flags for the Rmd
# ============================================================

has_score_dimension <- function(key) {
  key %in% names(overall_score_plots) && !is.null(overall_score_plots[[key]])
}

check_comp   <- has_score_dimension("comp")
check_values <- has_score_dimension("values")

check_pillar1 <- has_score_dimension("disaster")
check_pillar2 <- has_score_dimension("detection")
check_pillar3 <- has_score_dimension("dissemination")
check_pillar4 <- has_score_dimension("response")

check_pillar12 <- check_pillar1 || check_pillar2
check_pillar34 <- check_pillar3 || check_pillar4

message("✓ Score charts loaded")
