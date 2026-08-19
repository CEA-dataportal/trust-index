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

#Reinitiate calculated plot height
height_exp_plot <- 1
height_behaviours_plot <- 1
height_impact_plot <- 1
height_intention_plot <-1
height_knowledge_plot <-1
height_extra_plot <-1

#Sampling tabs

if ("extra" %in% question_code$category){
  check_extra <- TRUE
} else {
  check_extra <- FALSE
}

#Contextual questions
check_rc <- any(question_code$category %in% c("experience", "behaviours", "impact", "intention", "knowledge","channel"))

if ("experience" %in% question_code$category){
  check_experience <- TRUE } else {
    check_experience <- FALSE
  }

if ("behaviours" %in% question_code$category){
  check_behaviours <- TRUE } else {
    check_behaviours <- FALSE
  }

if ("intention" %in% question_code$category){
  check_intention <- TRUE } else {
    check_intention <- FALSE
  }
if ("impact" %in% question_code$category){
  check_impact <- TRUE
} else {
  check_impact <- FALSE
}
if ("knowledge" %in% question_code$category){
  check_knowledge <- TRUE
} else {
  check_knowledge <- FALSE
}
if ("channel" %in% question_code$category){
  check_channel <- TRUE
} else {
  check_channel <- FALSE
}



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
# Question summaries and charts
# ------------------------------------------------------------

# ---- Original chunk: question_calculation ----
# COMPETENCIES QUESTIONS

survey_comp <- survey_data %>%
  select(matches(paste0("^", prefix_comp)))

comp_data <- survey_comp %>%
  pivot_longer(everything(), names_to = "Question", values_to = "Response")

comp_recode_map <- setNames(
  drivers_map$Competency,
  paste0("COMP_", names(drivers_map$Competency))
)

comp_data <- comp_data %>%
  filter(!(is.na(Response))) %>%
  mutate(
    Question = recode(Question, !!!comp_recode_map),
    Response = recode(Response, !!!answer_likertscale)
  )

summary_comp <- comp_data %>%
  group_by(Question, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Response = factor(Response, levels = unique(answer_likertscale))
  ) %>%
  group_by(Question) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()



# VALUES QUESTIONS

survey_val <- survey_data %>%
  select(matches(paste0("^", prefix_val)))


val_data <- survey_val %>%
  pivot_longer(everything(), names_to = "Question", values_to = "Response")


val_recode_map <- setNames(
  drivers_map$Value,
  paste0("VALUES_", names(drivers_map$Value))
)

val_data <- val_data %>%
  filter(!(is.na(Response))) %>%
  mutate(
    Question = recode(Question, !!!val_recode_map),
    Response = recode(Response, !!!answer_likertscale)
  )

summary_val <- val_data %>%
  group_by(Question, Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Response = factor(Response, levels = unique(answer_likertscale))
  ) %>%
  group_by(Question) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup()


# Palette
pal_score <- grDevices::colorRampPalette(color_scale)(5)

# ---- Original chunk: question_competencies ----
# Plot
drivers_comp_plot <- ggplot(summary_comp, aes(x = Question, y = percent, fill = Response)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    lwd = 0.35,
    color = "white",
    width = 0.45,
    show.legend = TRUE
  ) +
  geom_text(
    aes(
      label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
    color = color_label_grey,
    position = position_stack(vjust = 0.5, reverse = FALSE),
    size = 4,
    vjust = -2.6,
    show.legend = FALSE
  ) +
  labs(
    title = tr("drivers.responses_by_subdimension"), 
    x = NULL,
    y = NULL
  ) +
  coord_flip() +
  scale_fill_manual(
    values = pal_score,
    name = NULL,
    labels = function(x) tr_data(x)
  ) +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  custom_theme()

# ---- Original chunk: question_values ----
# Plot
drivers_val_plot <- ggplot(summary_val, aes(x = Question, y = percent, fill = Response)) +
  geom_bar(
    position = "stack",
    stat = "identity",
    lwd = 0.35,
    color = "white",
    width = 0.50,
    show.legend = TRUE
  ) +
  geom_text(
    aes(
      label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
    color = color_label_grey,
    position = position_stack(vjust = 0.5, reverse = FALSE),
    size = 4,
    vjust = -2.6,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = pal_score,
    name = NULL,
    labels = function(x) tr_data(x)
  ) +
  scale_x_discrete(
    labels = function(x) tr_variable(x)
  ) +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(
    title = tr("drivers.responses_by_subdimension"), 
    x = NULL,
    y = NULL
  ) +
  coord_flip() +
  custom_theme()

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
    
    extra_order <- c("Mostly agree","Strongly agree","Don't know","Strongly disagree","Mostly disagree")
    
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
        x = stringr::str_wrap(tr_variable(as.character(Question)), 20),
        y = percent,
        fill = Response
      )
    ) +
      geom_col(color = "white", width = 0.55) +
      geom_text(
        aes(label = ifelse(percent >= 5, paste0(round(percent, 1), "%"), "")),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 3.5,
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
        subtitle = paste0(
          tr("drivers.opinions_subtitle"),
          tolower(tr_variable(unique(df_driver$Drivers)))
        ),
        x = NULL,
        y = NULL
      ) +
      guides(fill = guide_legend(reverse = TRUE)) +
      custom_theme() +
      theme(
        axis.text.y = element_text(size=10)
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
      position = "stack",
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
      position = position_stack(vjust = 0.5),
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
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(
      title = tr("drivers.exp_title"),
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
      position = "stack",
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
      position = position_stack(vjust = 0.5),
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
    guides(fill = guide_legend(reverse = TRUE))+
    labs(
      title = tr("drivers.behaviours_title"),
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
      position = "stack",
      color = "white",
      width = 0.50,
      linewidth = 0.35
    ) +
    geom_text(
      aes(label = ifelse(percent > 2, paste0(round(percent, 1), "%"), "")),
      position = position_stack(vjust = 0.5),
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
    guides(fill = guide_legend(reverse = TRUE))+
    labs(title = "", x = NULL, y = NULL) +
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
      position = "stack",
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
      position = position_stack(vjust = 0.5),
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
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(
      title = tr("drivers.impact_title"),
      x = NULL,
      y = NULL
    ) +
    coord_flip() +
    custom_theme()
  
}

# ---- Original chunk: channel_questions ----
# Charts for channel question
# Long Label -> Subtitle
# Question -> X axis

if ("channel" %in% question_code$category) {
  questions_channel <- question_code %>%
    filter(category == "channel") %>%
    select(variable, short_label) %>%
    tibble::deframe()   # named char vector
  
  question_map_channel <- question_code %>%
    filter(category == "channel") %>%
    mutate(
      Drivers    = str_split(variable, "_", simplify = TRUE)[, 2],
      Question   = short_label
    ) %>%
    select(variable, Question, Drivers, long_label)
  
  survey_channel <- data %>%
    select(all_of(names(questions_channel))) %>%
    rename_with(~ questions_channel[.x], .cols = names(questions_channel))  # columns now are short_label
  
  channel_data <- survey_channel %>%
    pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
    filter(!is.na(Response) & Response != "") %>%          # drop empty
    left_join(
      question_map_channel %>% select(Question, Drivers, long_label),
      by = "Question"
    )
  
  summary_channel <- channel_data %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_yn_map)
    ) %>%
    group_by(Drivers, Question, Response, long_label) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Response = factor(Response, levels = unique(answer_yn_map))) %>%
    group_by(long_label, Question) %>%
    mutate(percent = n / sum(n) * 100) %>%
    ungroup()
  
  plots_by_channel <- summary_channel %>%
    split(.$long_label) %>%
    map(~ .x %>%
          dplyr::filter(Response == "Yes") %>% 
          ggplot(
            aes(
              x = forcats::fct_reorder(
                stringr::str_wrap(tr_variable(Question), 40),
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
          guides(fill = guide_legend(reverse = TRUE))+
          coord_flip() + 
          scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
          scale_x_discrete(expand = expansion(add = 0.35)) +
          labs(
            title = tr_variable(unique(.x$Drivers)),
            subtitle = tr_variable(unique(.x$long_label)),
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
  
  summary_knowledge <- knowledge_data %>%
    mutate(
      Response = recode(as.character(Response), !!!answer_yn_map)
    ) %>%
    group_by(Drivers, Question, Response, long_label) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Response = factor(Response, levels = unique(answer_yn_map))) %>%
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
                stringr::str_wrap(tr_variable(Question), 40),
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
            title = tr_variable(unique(.x$Drivers)),
            subtitle = tr_variable(unique(.x$long_label)),
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
