##########################################################
#.  COMMUNITY TRUST INDEX - CHARTS
##########################################################


# ============================================================
# CTI Report - charts_drivers.R
# Driver and contextual-question charts
# Shared by Institutional and EWS modules
# ============================================================

message(tr("runtime.loading_charts"))

# This script assumes the following scripts have already run:
# - module palette and configuration
# - R/base/setup.R
# - R/base/read_config.R
# - R/base/load_data.R
# - R/base/translation_v2.R
# - R/analysis.R


# ------------------------------------------------------------
# Chart parameters
# ------------------------------------------------------------

# Reinitialise calculated plot heights
height_exp_plot        <- 1
height_behaviours_plot <- 1
height_impact_plot     <- 1
height_intention_plot  <- 1
height_knowledge_plot  <- 1
height_extra_plot      <- 1
height_risk_plot       <- 1
height_ews_plot        <- 1
height_strip_plot      <- 1

# Normalised category list
chart_categories <- tolower(as.character(question_code$category))

# Contextual-question flags
check_extra      <- "extra"      %in% chart_categories
check_experience <- "experience" %in% chart_categories
check_behaviours <- "behaviours" %in% chart_categories
check_intention  <- "intention"  %in% chart_categories
check_impact     <- "impact"     %in% chart_categories
check_knowledge  <- "knowledge"  %in% chart_categories
check_channel    <- "channel"    %in% chart_categories
check_risk       <- "risk"       %in% chart_categories
check_ews        <- "ews"        %in% chart_categories

check_rc <- any(
  chart_categories %in% c(
    "experience",
    "behaviours",
    "impact",
    "intention",
    "knowledge",
    "channel"
  )
)

# EWS pillar flags retained for compatibility with the current Rmd.
check_pillar1 <- "disaster"      %in% names(score_columns)
check_pillar2 <- "detection"     %in% names(score_columns)
check_pillar3 <- "dissemination" %in% names(score_columns)
check_pillar4 <- "response"      %in% names(score_columns)

# Caption used by contextual charts
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
# Score-driver summaries
# ------------------------------------------------------------

# Build the response distribution for one score dimension / EWS pillar.
build_driver_summary <- function(dimension_key, dimension_label) {
  
  dimension_lookup <- drivers_lookup %>%
    dplyr::filter(tolower(Dimension) == tolower(dimension_label)) %>%
    dplyr::select(Variables, Drivers)
  
  dimension_vars <- intersect(
    dimension_lookup$Variables,
    names(survey_data)
  )
  
  if (length(dimension_vars) == 0L) {
    return(NULL)
  }
  
  recode_map <- stats::setNames(
    dimension_lookup$Drivers,
    dimension_lookup$Variables
  )
  
  survey_data %>%
    dplyr::select(dplyr::all_of(dimension_vars)) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "Question",
      values_to = "Response"
    ) %>%
    dplyr::filter(!is.na(Response), Response != "") %>%
    dplyr::mutate(
      Question = dplyr::recode(Question, !!!recode_map),
      Response = dplyr::recode(
        as.character(Response),
        !!!answer_likertscale
      ),
      Response = factor(
        Response,
        levels = unique(answer_likertscale)
      )
    ) %>%
    dplyr::group_by(Question, Response) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(Question) %>%
    dplyr::mutate(percent = n / sum(n) * 100) %>%
    dplyr::ungroup()
}


# Create summary_<dimension> objects for the active module.
for (dimension_key in names(score_columns)) {
  
  dimension_position <- match(
    dimension_key,
    names(score_prefixes)
  )
  
  if (is.na(dimension_position)) next
  
  dimension_label <- score_dimensions[dimension_position]
  
  summary_dimension <- build_driver_summary(
    dimension_key,
    dimension_label
  )
  
  assign(
    paste0("summary_", dimension_key),
    summary_dimension,
    envir = .GlobalEnv
  )
}

# Backward-compatible Institutional aliases.
if (exists("summary_values", inherits = FALSE)) {
  summary_val <- summary_values
}
if (exists("summary_comp", inherits = FALSE)) {
  summary_comp <- summary_comp
}


# ------------------------------------------------------------
# Score-driver plots
# ------------------------------------------------------------

# pal_score is normally defined by palette_inst.R / palette_ews.R.
if (!exists("pal_score", inherits = TRUE)) {
  pal_score <- grDevices::colorRampPalette(color_scale)(5)
}

make_driver_score_plot <- function(summary_df, dimension_key, dimension_label) {
  
  if (is.null(summary_df) || nrow(summary_df) == 0L) {
    return(NULL)
  }
  
  is_ews <- exists("module_code", inherits = TRUE) &&
    identical(toupper(as.character(module_code)), "EWS")
  
  ews_dimension_titles <- c(
    disaster      = tr("score.disaster_risk_knowledge"),
    detection     = tr("score.detection_monitoring_forecasting"),
    dissemination = tr("score.warning_dissemination_communication"),
    response      = tr("score.preparedness_response_capabilities")
  )
  
  plot_title <- if (is_ews) {
    translated_title <- unname(ews_dimension_titles[dimension_key])
    
    if (length(translated_title) == 0L || is.na(translated_title)) {
      tr_variable(dimension_label)
    } else {
      translated_title
    }
  } else {
    tr("drivers.responses_by_subdimension")
  }
  
  plot_subtitle <- if (is_ews) {
    tr("drivers.responses_by_subdimension")
  } else {
    NULL
  }
  
  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = Question,
      y = percent,
      fill = Response
    )
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_stack(reverse = TRUE),
      colour = "white",
      linewidth = 0.35,
      width = 0.50,
      show.legend = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          percent > 2,
          paste0(round(percent, 1), "%"),
          ""
        )
      ),
      colour = color_label_White,
      position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
      size = 4,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = pal_score,
      name = NULL,
      labels = function(x) tr_data(x)
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) tr_variable(x)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(reverse = FALSE)
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = NULL,
      y = NULL
    ) +
    ggplot2::coord_flip() +
    custom_theme()
}


# Create drivers_<dimension>_plot objects dynamically.
for (dimension_key in names(score_columns)) {
  
  summary_name <- paste0("summary_", dimension_key)
  if (!exists(summary_name, inherits = TRUE)) next
  
  dimension_position <- match(
    dimension_key,
    names(score_prefixes)
  )
  
  if (is.na(dimension_position)) next
  
  dimension_label <- score_dimensions[dimension_position]
  
  plot_object <- make_driver_score_plot(
    get(summary_name, inherits = TRUE),
    dimension_key,
    dimension_label
  )
  
  assign(
    paste0("drivers_", dimension_key, "_plot"),
    plot_object,
    envir = .GlobalEnv
  )
}

# Backward-compatible Institutional alias expected by the current report.
if (exists("drivers_values_plot", inherits = FALSE)) {
  drivers_val_plot <- drivers_values_plot
}


# ------------------------------------------------------------
# EWS contextual-question chart
# ------------------------------------------------------------

# Stacked distribution of questions whose category is "ews".
# Entirely conditional: Institutional reports are unaffected.
ews_plot <- NULL

if (isTRUE(check_ews)) {
  
  select_ews <- question_code %>%
    dplyr::filter(tolower(category) == "ews") %>%
    dplyr::transmute(
      variable,
      ShortLabel      = short_label,
      LongLabel       = long_label,
      ShortLabel_wrap = stringr::str_wrap(short_label, width = 20)
    )
  
  ews_variables <- intersect(
    select_ews$variable,
    names(data)
  )
  
  if (length(ews_variables) > 0L) {
    
    select_ews <- select_ews %>%
      dplyr::filter(variable %in% ews_variables)
    
    # Preserve the order defined in question_code.
    long_levels <- unique(select_ews$LongLabel)
    
    ews_data <- data %>%
      dplyr::select(dplyr::all_of(ews_variables)) %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "variable",
        values_to = "Response"
      ) %>%
      dplyr::left_join(select_ews, by = "variable") %>%
      dplyr::filter(!is.na(Response), Response != "") %>%
      dplyr::mutate(
        Response = dplyr::recode(
          as.character(Response),
          !!!answer_ews
        ),
        Response = tidyr::replace_na(Response, "No answer"),
        Question = factor(
          LongLabel,
          levels = long_levels
        )
      )
    
    summary_ews <- ews_data %>%
      dplyr::count(Question, Response, name = "n") %>%
      dplyr::group_by(Question) %>%
      dplyr::mutate(
        percent = 100 * n / sum(n)
      ) %>%
      dplyr::ungroup()
    
    # Keep the global response order from answer_extra, while retaining
    # any unexpected response values at the end rather than dropping them.
    ews_response_levels <- unique(as.character(answer_ews))
    present_ews_levels <- unique(as.character(summary_ews$Response))
    
    ews_response_levels <- c(
      ews_response_levels[ews_response_levels %in% present_ews_levels],
      setdiff(present_ews_levels, ews_response_levels)
    )
    
    summary_ews <- summary_ews %>%
      dplyr::mutate(
        Response = factor(
          Response,
          levels = ews_response_levels
        )
      )
    
    n_ews_levels <- max(1L, length(ews_response_levels))
    pal_ews_questions <- if (exists("palette_1", inherits = TRUE)) {
      palette_candidate <- get("palette_1", inherits = TRUE)
      
      if (length(palette_candidate) < n_ews_levels) {
        grDevices::colorRampPalette(palette_candidate)(n_ews_levels)
      } else {
        palette_candidate[seq_len(n_ews_levels)]
      }
    } else {
      grDevices::colorRampPalette(color_scale)(n_ews_levels)
    }
    
    n_ews_questions <- dplyr::n_distinct(summary_ews$Question)
    height_ews_plot <- max(
      6,
      min(14, n_ews_questions * 1.10 + 2)
    )
    
    ews_plot <- ggplot2::ggplot(
      summary_ews,
      ggplot2::aes(
        x = Question,
        y = percent,
        fill = Response
      )
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_stack(reverse = TRUE),
        colour = "white",
        width = 0.50,
        linewidth = 0.35
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          label = ifelse(
            percent > 2,
            paste0(round(percent, 0), "%"),
            ""
          )
        ),
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
        colour = color_label_White,
        size = 4.3,
        show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(
        values = pal_ews_questions,
        name = NULL,
        labels = function(x) tr_data(x)
      ) +
      ggplot2::scale_x_discrete(
        labels = function(x) stringr::str_wrap(
          tr_variable(as.character(x)),
          width = 30
        )
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(reverse = FALSE)
      ) +
      ggplot2::labs(
        title = "Perceptions and Trust in Early Warning Systems",
        x = NULL,
        y = NULL
      ) +
      ggplot2::coord_flip() +
      custom_theme()
  }
}


# ------------------------------------------------------------
# EWS risk-perception chart
# ------------------------------------------------------------

# This is the only additional driver/context chart required by EWS.
# It is entirely conditional: Institutional reports are unaffected.
if (isTRUE(check_risk)) {
  
  select_risk <- question_code %>%
    dplyr::filter(tolower(category) == "risk") %>%
    dplyr::select(variable, short_label, long_label)
  
  if (nrow(select_risk) > 0L) {
    
    questions_risk <- stats::setNames(
      select_risk$short_label,
      select_risk$variable
    )
    
    risk_labels <- stats::setNames(
      select_risk$long_label,
      select_risk$short_label
    )
    
    risk_variables <- intersect(
      select_risk$variable,
      names(data)
    )
    
    survey_risk <- data %>%
      dplyr::select(dplyr::all_of(risk_variables)) %>%
      dplyr::rename_with(
        ~ questions_risk[.x],
        .cols = dplyr::everything()
      )
    
    data_risk <- survey_risk %>%
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "Question_short",
        values_to = "Response"
      ) %>%
      dplyr::mutate(
        Question_long = risk_labels[Question_short],
        Response = dplyr::recode(
          as.character(Response),
          !!!answer_risk,
          .default = as.character(Response)
        )
      )%>%
      dplyr::filter(!is.na(Response), Response != "")
    
    summary_risk <- data_risk %>%
      dplyr::group_by(
        Question_short,
        Question_long,
        Response
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(
        Question_short,
        Question_long
      ) %>%
      dplyr::mutate(
        percent = n / sum(n) * 100
      ) %>%
      dplyr::ungroup()
    
    make_risk_barchart <- function(df_q) {
      
      q_name <- as.character(
        unique(df_q$Question_short)[1]
      )
      
      # Use the EWS question-specific ordering when available.
      desired_levels <- if (
        exists("answer_risk", inherits = TRUE) &&
        length(answer_risk) > 0
      ) {
        unique(unname(answer_risk))
      } else {
        NULL
      }
      
      present <- unique(as.character(df_q$Response))
      
      if (!is.null(desired_levels)) {
        levels_in_use <- desired_levels[
          desired_levels %in% present
        ]
        levels_in_use <- c(
          levels_in_use,
          setdiff(present, levels_in_use)
        )
      } else {
        levels_in_use <- present
      }
      
      # # Keep "Don't know" at the end when present.
      # dont_know <- grepl(
      #   "^Don.?t know$",
      #   levels_in_use,
      #   ignore.case = TRUE
      # )
      # 
      # levels_in_use <- c(
      #   levels_in_use[!dont_know],
      #   levels_in_use[dont_know]
      # )
      # 
      df_q <- df_q %>%
        dplyr::mutate(
          Response = factor(
            Response,
            levels = levels_in_use
          )
        )
      
      # Use question-specific EWS palettes when available; otherwise
      # construct a palette from the active module colours.
      base_pal <- NULL
      
      if (exists("question_palettes", inherits = TRUE)) {
        base_pal <- get(
          "question_palettes",
          inherits = TRUE
        )[[q_name]]
      }
      
      if (is.null(base_pal)) {
        palette_base <- if (exists("palette_module", inherits = TRUE)) {
          get("palette_module", inherits = TRUE)
        } else {
          c(color_primary_100, color_secondary_100, color_grey)
        }
        
        base_pal <- grDevices::colorRampPalette(
          c(palette_base, color_grey)
        )(max(1L, length(levels_in_use)))
      }
      
      if (length(base_pal) < length(levels_in_use)) {
        base_pal <- grDevices::colorRampPalette(base_pal)(
          length(levels_in_use)
        )
      }
      
      palette_q <- base_pal[seq_along(levels_in_use)]
      names(palette_q) <- levels_in_use
      
      ggplot2::ggplot(
        df_q,
        ggplot2::aes(
          x = Response,
          y = percent,
          fill = Response
        )
      ) +
        ggplot2::geom_col(
          width = 0.7,
          colour = "white",
          show.legend = FALSE
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = paste0(round(percent), "%")
          ),
          vjust = -0.5,
          size = 4
        ) +
        ggplot2::scale_y_continuous(
          labels = function(x) paste0(x, "%"),
          expand = ggplot2::expansion(mult = c(0, 0.1))
        ) +
        ggplot2::scale_fill_manual(
          values = palette_q,
          breaks = levels_in_use,
          name = NULL
        ) +
        ggplot2::scale_x_discrete(
          labels = function(x) tr_data(x)
        ) +
        ggplot2::labs(
          title = tr_variable(q_name),
          subtitle = stringr::str_wrap(
            tr_variable(unique(df_q$Question_long)[1]),
            width = 40
          ),
          x = NULL,
          y = NULL
        ) +
        custom_theme() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            vjust = 1
          )
        )
    }
    
    risk_plots <- summary_risk %>%
      dplyr::group_split(Question_short) %>%
      purrr::map(make_risk_barchart)
    
    n_risk_questions <- length(risk_plots)
    height_risk_plot <- max(
      5,
      ceiling(n_risk_questions / 2) * 4
    )
    
    risk_grid <- patchwork::wrap_plots(
      risk_plots,
      ncol = 2
    ) &
      ggplot2::theme(
        plot.background = ggplot2::element_rect(
          fill = "#F8F8F8",
          colour = NA
        ),
        panel.background = ggplot2::element_rect(
          fill = "#F8F8F8",
          colour = NA
        ),
        plot.margin = ggplot2::margin(
          10,
          20,
          10,
          20
        )
      )
  }
}

# ---- Original chunk: extra_question ----
if ("extra" %in% question_code$category) {
  
  select_extra <- question_code %>%
    filter(category == "extra") %>%
    select(variable, short_label)
  
  question_map_extra <- question_code %>%
    filter(category == "extra") %>%
    mutate(
      Drivers = str_split(variable, "_", simplify = TRUE)[, 2],
      Question = short_label,
      long_label = long_label
    ) %>%
    select(variable, Question, Drivers, long_label)
  
  questions_extra <- deframe(question_map_extra %>% select(variable, Question))
  
  survey_extra <- data %>%
    select(all_of(names(questions_extra))) %>%
    rename_with(~ questions_extra[.x], .cols = names(questions_extra))
  
  extra_data <- survey_extra %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
    # drop empty responses (NA or blank string)
    filter(!is.na(Response) & Response != "") %>%
    left_join(question_map_extra %>% select(Question, Drivers, long_label), by = "Question")
  
  summary_extra <- extra_data %>%
    filter(!(is.na(extra_data$Response))) %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_extra)
    ) %>%
    group_by(Drivers, Question, Response,long_label) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Drivers, Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  # Dynamically calculate the maximum number of characters for the label to be limited to three lines.
  label_lengths <- summary_extra |>
    distinct(long_label) |>                               
    mutate(
      n_chars = nchar(as.character(long_label), type = "chars")
    ) |>
    arrange(desc(n_chars))
  
  wrap <- as.integer((max(label_lengths$n_chars, na.rm = TRUE) / 4)+10)
  
  
  make_barchart <- function(df_driver) {
    
    # Order questions within the driver (largest "Yes" or largest total, pick one)
    # Here: order by total percent descending (i.e. most common categories first)
    q_order <- df_driver %>%
      group_by(long_label) %>%
      summarise(total = sum(percent, na.rm = TRUE), .groups = "drop") %>%
      arrange(total) %>%                       # ascending so biggest ends up on top after flip
      pull(long_label)
    
    extra_order <- unique(as.character(answer_extra))
    
    df_driver <- df_driver %>%
      mutate(
        Question = factor(long_label, levels = q_order),
        Response = factor(Response, levels = extra_order)            # keep natural / existing order
      )
    
    
    # Make driver-specific palette sized to number of response levels
    pal_extra <- colorRampPalette(color_scale)(nlevels(df_driver$Response))
    
    ggplot(
      df_driver,
      aes(
        x = stringr::str_wrap(tr_variable(as.character(Question)), wrap),
        y = percent,
        fill = Response
      )
    ) +
      geom_col(color = "white", width = 0.55) +
      geom_text(
        aes(label = ifelse(percent >= 5, paste0(round(percent, 1), "%"), "")),
        position = position_stack(vjust = 0.5, reverse = TRUE),
        color = "white",
        size = 4,
        show.legend = FALSE
      ) +
      scale_fill_manual(
        values = pal_extra,
        name = NULL,
        labels = function(x) tr_data(x)
      ) +
      coord_flip() +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
      scale_x_discrete(expand = expansion(add = 0.35)) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = tr_variable(unique(df_driver$Drivers)),
        subtitle = NULL,
        x = NULL,
        y = NULL
      ) +
      guides(fill = guide_legend(reverse = FALSE)) +
      custom_theme() +
      theme(
        axis.text.y = element_text(size=12)
      )
  }
  
  extra_plots <- summary_extra %>%
    group_split(Drivers) %>%
    map(make_barchart)
  
}

# ---- Original chunk: experience_question ----
if ("experience" %in% question_code$category) {
  # Data preparation
  select_experience <- question_code %>%
    filter(category == "experience") %>%
    select(variable, short_label)
  
  questions_experience <- deframe(select_experience)
  
  survey_exp <- data %>%
    select(all_of(names(questions_experience))) %>%
    rename_with(~ questions_experience[.x], .cols = names(questions_experience))
  
  experience_labels <- question_code %>%
    filter(category == "experience") %>%
    select(short_label, long_label) %>%
    mutate(long_label = stringr::str_wrap(long_label, width = 20)) %>%
    deframe()
  
  exp_data <- survey_exp %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response")
  exp_data$Question <- str_wrap(experience_labels[exp_data$Question], width = 20)
  
  
  
  # exp_levels <- c("Very positively", "Yes", "Somewhat positively", "Yes, mostly", "Don't know", "No, mostly", "Somewhat negatively", "No", "Not at all", "Very negatively")
  
  summary_exp <- exp_data %>%
    filter(!(is.na(exp_data$Response))) %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_exp)
    ) %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(Response = factor(Response, levels = unique(answer_exp)))
  
  
  # Palette
  n_exp_levels <- nlevels(summary_exp$Response)
  pal_exp <- grDevices::colorRampPalette(color_scale)(n_exp_levels)
  
  n_questions_exp <- length(unique(summary_exp$Question))
  height_exp_plot <- max(
    6,
    min(12, n_questions_exp * 1.25 + 2)
  )
  
  # Plot
  experience_plot <- ggplot(summary_exp, aes(x = Question, y = percent, fill = Response)) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      stat = "identity",
      lwd = 0.35,
      color = "white",
      width = 0.50,
      show.legend = TRUE
    ) +
    geom_text(
      aes(
        label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
      color = color_label_White,
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 4,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = pal_exp,
      name = NULL,
      labels = function(x) tr_data(x)
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(tr_variable(x), 20)
    ) +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(
      title = tr("drivers.experience_title"),
      subtitle = tr("drivers.last_12_months"),
      x = NULL,
      y = NULL
    ) +
    coord_flip() +
    custom_theme()
  
}

# ---- Original chunk: behaviours_questions ----
if ("behaviours" %in% question_code$category) {
  # Data preparation
  select_behaviours <- question_code %>%
    filter(category == "behaviours") %>%
    select(variable, short_label)
  
  questions_behaviours <- deframe(select_behaviours)
  
  survey_behaviours <- data %>%
    select(all_of(names(questions_behaviours))) %>%
    rename_with(~ questions_behaviours[.x], .cols = names(questions_behaviours))
  
  behaviour_labels <- question_code %>%
    filter(category == "behaviours") %>%
    select(short_label, long_label) %>%
    mutate(long_label = stringr::str_wrap(long_label, width = 20)) %>%
    deframe()
  
  behaviours_data <- survey_behaviours %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response")
  behaviours_data$Question <- str_wrap(behaviour_labels[behaviours_data$Question], width = 20)
  
  summary_behaviours <- behaviours_data %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_behaviours)
    ) %>%
    filter(!is.na(Response), Response != "") %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Response = factor(Response, levels = unique(answer_behaviours))) %>%
    group_by(Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  
  # Palette
  behaviours_levels <- nlevels(summary_behaviours$Response)
  pal_behaviours <- grDevices::colorRampPalette(color_scale)(behaviours_levels)
  
  n_questions_behaviours <- length(unique(summary_behaviours$Question))
  height_behaviours_plot <- max(
    6,
    min(12, n_questions_behaviours * 1.25 + 2)
  )
  
  # Plot
  behaviours_plot <- ggplot(summary_behaviours, aes(x = Question, y = percent, fill = Response)) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      stat = "identity",
      lwd = 0.35,
      color = "white",
      width = 0.50,
      show.legend = TRUE
    ) +
    geom_text(
      aes(
        label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
      color = color_label_White,
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 4,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = pal_behaviours,
      name = NULL,
      labels = function(x) tr_data(x)
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(tr_variable(x), 20)
    ) +
    guides(fill = guide_legend(reverse = FALSE))+
    labs(
      title = tr("drivers.behaviours_title"),
      subtitle = tr("drivers.last_12_months"),
      x = NULL,
      y = NULL
    ) +
    coord_flip() +
    custom_theme()
  
}

# ---- Original chunk: intention_questions ----
if ("intention" %in% question_code$category) {
  
  # Data preparation
  select_intention <- question_code %>%
    filter(category == "intention") %>%
    select(variable, short_label)
  
  questions_intention <- deframe(select_intention)
  
  survey_intention <- data %>%
    select(all_of(names(questions_intention))) %>%
    rename_with(~ questions_intention[.x], .cols = names(questions_intention))
  
  intention_labels <- question_code %>%
    filter(category == "intention") %>%
    select(short_label, long_label) %>%
    mutate(long_label = stringr::str_wrap(long_label, width = 20)) %>%
    deframe()
  
  intention_data <- survey_intention %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response")
  intention_data$Question <- str_wrap(intention_labels[intention_data$Question], width = 20)
  
  
  summary_intention <- intention_data %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_intention)
    ) %>%
    filter(!is.na(Response), Response != "") %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Response = factor(Response, levels = unique(answer_intention))) %>%
    group_by(Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  
  # Palette
  intention_levels <- nlevels(summary_intention$Response)
  pal_intention <- grDevices::colorRampPalette(color_scale)(intention_levels)
  
  n_questions_intention <- length(unique(summary_intention$Question))
  height_intention_plot <- max(
    6,
    min(12, n_questions_intention * 1.25 + 2)
  )
  
  # Plot
  
  intention_plot <- ggplot(summary_intention, aes(x = Question, y = percent, fill = Response)) +
    geom_col(
      position = position_stack(reverse = TRUE),
      color = "white",
      width = 0.50,
      linewidth = 0.35
    ) +
    geom_text(
      aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color = color_label_White,
      size = 4,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = pal_intention,
      name = NULL,
      labels = function(x) tr_data(x)
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(tr_variable(x), 20)
    ) +
    guides(fill = guide_legend(reverse = FALSE))+
    labs(
      title = tr("drivers.intention_title"),
      subtitle = tr("drivers.next_12_months"),
      x = NULL,
      y = NULL
    ) +
    coord_flip() +
    custom_theme()
  
}


# ---- Original chunk: impact_question ----
if ("impact" %in% question_code$category) {
  # Data preparation
  select_impact <- question_code %>%
    filter(category == "impact") %>%
    select(variable, short_label)
  
  questions_impact <- deframe(select_impact)
  
  survey_impact <- data %>%
    select(all_of(names(questions_impact))) %>%
    rename_with(~ questions_impact[.x], .cols = names(questions_impact))
  
  impact_labels <- question_code %>%
    filter(category == "impact") %>%
    select(short_label, long_label) %>%
    mutate(long_label = stringr::str_wrap(long_label, width = 20)) %>%
    deframe()
  
  impact_data <- survey_impact %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response")
  impact_data$Question <- str_wrap(impact_labels[impact_data$Question], width = 20)
  
  summary_impact <- impact_data %>%
    filter(!(is.na(impact_data$Response))) %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_impact)
    ) %>%
    group_by(Question, Response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup() %>%
    mutate(Response = factor(Response, levels = unique(answer_impact)))
  
  
  # Palette
  n_impact_levels <- nlevels(summary_impact$Response)
  pal_impact <- grDevices::colorRampPalette(color_scale)(n_impact_levels)
  
  n_questions_impact <- length(unique(summary_impact$Question))
  height_impact_plot <- max(
    6,
    min(12, n_questions_impact * 1.25 + 2)
  )
  
  # Plot
  impact_plot <- ggplot(summary_impact, aes(x = Question, y = percent, fill = Response)) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      stat = "identity",
      lwd = 0.35,
      color = "white",
      width = 0.50,
      show.legend = TRUE
    ) +
    geom_text(
      aes(
        label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
      color = color_label_White,
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 4,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = pal_impact,
      name = NULL,
      labels = function(x) tr_data(x)
    ) +
    scale_x_discrete(
      labels = function(x) stringr::str_wrap(tr_variable(x), 20)
    ) +
    guides(fill = guide_legend(reverse = FALSE)) +
    labs(
      title = tr("drivers.impact_title"),
      subtitle = tr("drivers.impact_subtitle"),
      x = NULL,
      y = NULL
    ) +
    coord_flip() +
    custom_theme()
  
}
# ---- Channel questions ----
# Distribution of respondents who received information through each channel.

if ("channel" %in% tolower(question_code$category)) {
  
  questions_channel <- question_code %>%
    dplyr::filter(tolower(category) == "channel") %>%
    dplyr::select(variable, short_label) %>%
    tibble::deframe()
  
  question_map_channel <- question_code %>%
    dplyr::filter(tolower(category) == "channel") %>%
    dplyr::mutate(
      Drivers = stringr::str_split(variable, "_", simplify = TRUE)[, 2],
      Question = short_label
    ) %>%
    dplyr::select(variable, Question, Drivers, long_label)
  
  channel_variables <- intersect(
    names(questions_channel),
    names(data)
  )
  
  if (length(channel_variables) > 0L) {
    
    survey_channel <- data %>%
      dplyr::select(dplyr::all_of(channel_variables)) %>%
      dplyr::rename_with(
        ~ questions_channel[.x],
        .cols = dplyr::everything()
      )
    
    channel_data <- survey_channel %>%
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "Question",
        values_to = "Response"
      ) %>%
      dplyr::filter(!is.na(Response), Response != "") %>%
      dplyr::left_join(
        question_map_channel %>%
          dplyr::select(Question, Drivers, long_label),
        by = "Question"
      )
    
    # Use the specific channel mapping when available;
    # otherwise, fall back to the generic Yes / No mapping.
    channel_answer_map <- if (
      exists("answer_channel", inherits = TRUE) &&
      length(answer_channel) > 0
    ) {
      answer_channel
    } else {
      answer_yn_map
    }
    
    summary_channel <- channel_data %>%
      dplyr::mutate(
        Response = dplyr::recode(
          as.character(Response),
          !!!channel_answer_map
        )
      ) %>%
      dplyr::filter(Response %in% c("Yes", "No")) %>%
      dplyr::group_by(
        Drivers,
        Question,
        Response,
        long_label
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(
        Drivers,
        long_label,
        Question
      ) %>%
      dplyr::mutate(
        percent = n / sum(n) * 100
      ) %>%
      dplyr::ungroup() %>%
      # Keep only the proportion of respondents who answered Yes.
      dplyr::filter(Response == "Yes") %>%
      dplyr::mutate(
        Response = factor(
          Response,
          levels = "Yes"
        )
      )
    
    # Yes = blue / No = orange.
    # The No value is retained in the palette for consistency with other charts.
    channel_palette <- c(
      "Yes" = color_primary_100,
      "No" = color_tertiary_100
    )
    
    make_channel_plot <- function(df_channel) {
      
      # Retrieve the full question wording for the chart subtitle.
      channel_subtitle <- unique(df_channel$long_label)
      channel_subtitle <- channel_subtitle[
        !is.na(channel_subtitle) & nzchar(channel_subtitle)
      ][1]
      
      # Order channels from the highest to the lowest share of Yes responses.
      channel_order <- df_channel %>%
        dplyr::arrange(percent) %>%
        dplyr::pull(Question) %>%
        as.character()
      
      df_channel <- df_channel %>%
        dplyr::mutate(
          Question = factor(
            Question,
            levels = channel_order
          )
        )
      
      ggplot2::ggplot(
        df_channel,
        ggplot2::aes(
          x = Question,
          y = percent,
          fill = Response
        )
      ) +
        ggplot2::geom_col(
          colour = "white",
          linewidth = 0.35,
          width = 0.50
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            y = percent -2,
            label = dplyr::if_else(
              percent >= 3,
              paste0(round(percent, 0), "%"),
              ""
            )
          ),
          colour = color_label_White,
          size = 4.2,
          show.legend = FALSE
        ) +
        ggplot2::scale_fill_manual(
          values = channel_palette,
          breaks = "Yes",
          labels = function(x) tr_data(x),
          name = NULL,
          drop = FALSE
        ) +
        ggplot2::scale_x_discrete(
          labels = function(x) stringr::str_wrap(
            tr_variable(x),
            width = 28
          ),
          expand = ggplot2::expansion(add = 0.35)
        ) +
        ggplot2::scale_y_continuous(
          limits = c(0, 100),
          breaks = c(0, 25, 50, 75, 100),
          labels = function(x) x,
          expand = ggplot2::expansion(mult = c(0, 0))
        ) +
        ggplot2::labs(
          title = toupper(tr("drivers.channel_title")),
          subtitle = tr_variable(channel_subtitle),
          x = NULL,
          y = NULL
        ) +
        ggplot2::coord_flip() +
        custom_theme() +
        ggplot2::theme(
          legend.position = "none",
          axis.text.y = ggplot2::element_text(size = 12)
        )
    }
    
    plots_by_channel <- summary_channel %>%
      split(.$long_label) %>%
      purrr::map(make_channel_plot)
    
    n_channel_questions <- dplyr::n_distinct(summary_channel$Question)
    
    height_channel_plot <- max(
      6,
      min(14, n_channel_questions * 0.90 + 2)
    )
  }
}

# ---- Original chunk: knowledge_questions ----
# Charts for knowledge questions (e.g.)
# Long Label -> Subtitle
# Question -> X axis

if ("knowledge" %in% question_code$category) {
  questions_knowledge <- question_code %>%
    filter(category == "knowledge") %>%
    select(variable, short_label) %>%
    tibble::deframe()   # named char vector
  
  question_map_knowledge <- question_code %>%
    filter(category == "knowledge") %>%
    mutate(
      Drivers    = str_split(variable, "_", simplify = TRUE)[, 2],
      Question   = short_label
    ) %>%
    select(variable, Question, Drivers, long_label)
  
  survey_knowledge <- data %>%
    select(all_of(names(questions_knowledge))) %>%
    rename_with(~ questions_knowledge[.x], .cols = names(questions_knowledge))  # columns now are short_label
  
  knowledge_data <- survey_knowledge %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response) & Response != "") %>%          # drop empty
    left_join(
      question_map_knowledge %>% select(Question, Drivers, long_label),
      by = "Question"
    )
  
  knowledge_answer_map <- if (
    exists("answer_knowledge", inherits = TRUE) &&
    length(answer_knowledge) > 0
  ) {
    answer_knowledge
  } else {
    answer_yn_map
  }
  
  summary_knowledge <- knowledge_data %>%
    mutate(
      Response = recode(as.character(Response), !!!knowledge_answer_map)
    ) %>%
    group_by(Drivers, Question, Response, long_label) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Response = factor(Response, levels = unique(knowledge_answer_map))) %>%
    group_by(Drivers, Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  plots_knowledge <- summary_knowledge %>%
    split(.$Drivers) %>%
    map(~ .x %>%
          dplyr::filter(Response == "Yes") %>%
          ggplot(
            aes(
              x = forcats::fct_reorder(
                stringr::str_wrap(tr_variable(Question), 20),
                percent
              ),
              y = percent,
              fill = Response
            )
          ) +
          geom_col(color = "white", width = 0.65) +
          geom_text(
            aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, show.legend = FALSE
          ) +
          scale_fill_manual(
            values = color_primary_100,
            name = NULL,
            labels = function(x) tr_data(x)
          ) +
          coord_flip() + 
          scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
          scale_x_discrete(expand = expansion(add = 0.35)) +
          labs(
            title = tr("drivers.knowledge_title"),
            subtitle = tr("drivers.knowledge_subtitle"),
            x = NULL,
            y = NULL
          ) +
          custom_theme() +
          theme(
            legend.position = "none",
            axis.text.y = element_text(size=12)
          )
    )
  
}



message(tr("runtime.charts_loaded"))
