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
# source('R/translation.R')
# source('R/load_data.R')
# source('R/analysis.R')


# ------------------------------------------------------------
# Chart parameters
# ------------------------------------------------------------

# ---- Original chunk: chart_parameters ----
# This chunk checks which groups of questions are available in the codebook
# (Relationship with RC, behaviours, intention and other contextual modules) and creates logical
# flags. These flags are later used to decide which charts, maps and sections
# should be displayed in the report based on the data that actually exists.

#Sampling tabs


check_strata1 <-
  (exists("pop_strata1") && !is.null(pop_strata1)) ||
  (!is.null(strata1_tab) && strata1_tab %in% names(data))

check_strata2 <-
  (exists("pop_strata2") && !is.null(pop_strata2)) ||
  (!is.null(strata2_tab) && strata2_tab %in% names(data))

check_strata3 <-
  (exists("pop_strata3") && !is.null(pop_strata3)) ||
  (!is.null(strata3_tab) && strata3_tab %in% names(data))



#geographic questions
if ((!is.null(adm1) && adm1 != "") ||
    (!is.null(adm2) && adm2 != "") ||
    (!is.null(locality) && locality != "")) {
  message(tr("sampling.admin_levels_available"))
  check_adm <- TRUE
} else { check_adm <- FALSE}



# Helper used to standardise chart source captions
chart_source <- function(...) {
  sources <- unlist(list(...), use.names = FALSE)
  sources <- as.character(sources)
  sources <- sources[!is.na(sources) & nzchar(trimws(sources))]
  
  paste0(
    "<b>", tr("common.source"), ":</b> ",
    paste(sources, collapse = ", ")
  )
  
}


# ------------------------------------------------------------
# Geographic charts and maps
# ------------------------------------------------------------

# ---- Original chunk: geographical distribution ----
# Data preparation
geo_label1 <- adm1 # Label for admin 1
geo_label2 <- adm2 # Label for admin 2

geo_shp1   <- adm1 # shapefile adm1 name
geo_shp2   <- adm2 # shapefile adm2 name


# admin1
data_region <- data %>% group_by(.data[[adm1]]) %>% dplyr::summarize(n=n()) %>% ungroup()
regions_select <- regions_shp %>% 
  left_join(data_region, by = setNames(adm1, adm1_shp)) %>%  # shp_col = data_col
  filter(!is.na(n))

# admin2
data_district <- data %>% group_by(.data[[adm2]]) %>% dplyr::summarize(n=n()) %>% ungroup()
districts_select <- districts_shp %>%
  left_join(data_district, by = setNames(adm2, adm2_shp)) %>%  # shp_col = data_col
  filter(!is.na(n))


# Common geographical levels

#Comparison geographic data between survey and population data
geoname_survey <-  adm1    #  from survey data (e.g. adm1 or adm2 without "")
geoname_pop    <- "Admin1" # from population (a changer dans EWS)

other_regions_label <- tr("common.others")


# Population Data: Get top 9 regions by population
if (!is.null(population_geo) && geoname_pop %in% names(population_geo)) {
  
  pop_region_summary <- population_geo %>%
    group_by(.data[[geoname_pop]]) %>%
    summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(Pop))
  
  total_region_count <- nrow(pop_region_summary)
  
  if (total_region_count > 10) {
    top9_regions <- pop_region_summary %>%
      slice_head(n = 9) %>%
      pull(geoname_pop)
    
    all_plot_levels <- rev(c(sort(top9_regions), other_regions_label))
  } else {
    top9_regions <- pop_region_summary[[geoname_pop]]
    all_plot_levels <- rev(sort(top9_regions))  # No "Other regions"
  }
  
} else {
  top9_regions <- character(0)
  all_plot_levels <- NULL
}


# Plot

# Survey Data

if (!is.null(all_plot_levels) && geoname_survey %in% names(data)) {
  
  geo_survey_counts <- data %>%
    filter(!.data[[geoname_survey]] %in% excluded_regions) %>%
    mutate(
      District = if (length(top9_regions) < total_region_count) {
        if_else(.data[[geoname_survey]] %in% top9_regions, .data[[geoname_survey]], other_regions_label)
      } else {
        .data[[geoname_survey]]
      }
    ) %>%
    count(District, name = "Count")
  
  region_template <- data.frame(
    District = all_plot_levels,
    stringsAsFactors = FALSE
  )
  
  geo_survey_raw <- region_template %>%
    left_join(geo_survey_counts, by = "District") %>%
    mutate(
      Count = replace_na(Count, 0),
      Percentage = Count / sum(Count) * 100
    )
  
  geo_survey <- geo_survey_raw %>%
    mutate(
      Applicable = Percentage != 0,
      District = factor(District, levels = all_plot_levels)
    )
  
  geo_plot_survey <- ggplot(geo_survey, aes(x = District, y = Percentage, fill = District)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.3, size = 4) +
    coord_flip() +
    custom_theme() +
    scale_fill_manual(values = rep(color_primary_100, nrow(geo_survey))) +
    scale_y_continuous(limits = c(0, max(geo_survey$Percentage) + 10)) +
    labs(
      title = tr("sampling.title"),
      subtitle = tr("sampling.from_survey"),
      x = NULL,
      y = tr("common.percentage"),
      caption = chart_source(ns_name)
    ) +
    theme(legend.position = "none")
  
} else {
  geo_plot_survey <- 
    textGrob(
      tr("sampling.region_column_missing"),
      gp = gpar(fontsize = 16, fontface = "italic", col = "gray30")
    )
}

# Population Data aligned with top9
if (!is.null(population_geo) && length(top9_regions) > 0 && !is.null(all_plot_levels)) {
  
  region_population <- population_geo %>%
    mutate(
      District = if (length(top9_regions) < total_region_count) {
        if_else(.data[[geoname_pop]] %in% top9_regions, .data[[geoname_pop]], other_regions_label)
      } else {
        .data[[geoname_pop]]
      }
    ) %>%
    group_by(District) %>%
    summarise(Pop = sum(Pop, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Percentage = Pop / sum(Pop, na.rm = TRUE) * 100,
      Applicable = Percentage != 0,
      Percentage = ifelse(Applicable, Percentage, NA),
      District = factor(District, levels = all_plot_levels)
    )
  
  geo_plot_population <- ggplot(region_population, aes(x = District, y = Percentage, fill = District)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    geom_text(
      data = subset(region_population, Applicable),
      aes(label = paste0(round(Percentage, 1), "%")),
      hjust = -0.3,
      size = 4
    ) +
    geom_text(
      data = subset(region_population, !Applicable),
      aes(y = 5, label = tr("common.no_data")),
      color = "red",
      hjust = 0,
      size = 4
    ) +
    coord_flip() +
    custom_theme() +
    scale_fill_manual(values = rep(color_tertiary_100, nrow(region_population))) +
    scale_y_continuous(limits = c(0, max(region_population$Percentage, na.rm = TRUE) + 10)) +
    labs(
      title = tr("sampling.population"),
      subtitle = geo_org,
      x = NULL,
      y = tr("common.percentage"),
      caption = chart_source(geo_source)
    ) +
    theme(legend.position = "none")
  
} else {
  geo_plot_population <- 
    textGrob(
      tr("sampling.geo_population_missing"),
      gp = gpar(fontsize = 14, fontface = "italic", col = "red")
    )
}


# ---- Original chunk: maps ----
regions_lbl <- regions_select %>%
  mutate(geometry = sf::st_point_on_surface(geometry))

if (geoname_survey == adm1) {
  
  map_region <- tm_basemap("CartoDB.Positron")  +
    tm_shape(country_shp) +
    tm_polygons(fill = "grey90", fill_alpha = 0.5, col = "grey50", lwd = 0) +
    
    tm_shape(regions_select) +
    tm_polygons(
      fill       = "n",
      fill_alpha = 0.5,
      fill.scale = tm_scale_intervals(values = "matplotlib.blues",  n = 4),
      fill.legend = tm_legend(title = tr("sampling.total_respondents_map"), direction = "horizontal"),
      col        = "white",
      lwd        = 0.8,
      id         = adm1_shp,
      popup.vars = setNames(
        c(adm1_shp, "n"),
        c(adm1, tr("common.respondents"))
      )
    ) +
    
    tm_shape(regions_lbl) +
    tm_text(
      adm1_shp,
      col = "white",
      size = 1,
      options = opt_tm_text(just = "center")
    ) +
    
    tm_shape(country_shp) +
    tm_borders(lwd = 2, col = "grey50")
  
} else if (geoname_survey == adm2) {
  
  map_region <- tm_basemap("CartoDB.Positron") +
    tm_shape(country_shp) +
    tm_polygons(fill = "grey90", fill_alpha = 0.5, col = "grey50", lwd = 0) +
    
    tm_shape(districts_select) +
    tm_polygons(
      fill       = "n",
      fill_alpha = 0.5,
      fill.scale = tm_scale_intervals(values = "matplotlib.blues", n = 4),
      fill.legend = tm_legend(title = tr("sampling.total_respondents_map"), direction = "horizontal"),
      col        = "white",
      lwd        = 0.8,
      id         = adm2_shp,
      popup.vars = setNames(
        c(adm2_shp, "n"),
        c(adm2, tr("common.respondents"))
      )
    ) +
    
    tm_shape(regions_shp) +
    tm_borders(col = "white", lwd = 2.5) +
    
    tm_labels(
      adm2_shp,
      col = "white",
      size = 1,
      options = opt_tm_text(just = "center")
    ) +
    
    tm_shape(country_shp) +
    tm_borders(lwd = 2, col = "grey50")
  
} else {
  stop(tr("sampling.no_matching_geographies"))
}



# ------------------------------------------------------------
# Demographic charts
# ------------------------------------------------------------

# ---- Original chunk: demog ----
# Data preparation

# Calculate tables 
ages <- data.frame(
  table(data$Age_group, data[[gender_col]])
)
names(ages) <- c("AGEgroup", "Gender", "Freq")

pop_age <- demo_age
combined <- bind_rows(pop_age, ages)

ages$Freq <- ages$Freq/sum(ages$Freq)*100
pop_age$Freq <- pop_age$Freq/sum(pop_age$Freq)*100


ages$origin <- tr("sampling.survey")
pop_age$origin <- tr("sampling.population")

gender_source_caption <- chart_source(
  ns_name,
  paste0(demo_source, " (", demo_org, ")")
)

# Combining Survey and Population dataset
combined <- rbind(pop_age,ages)
combined <- combined %>% mutate(Freq = ifelse(Gender == gender_map[1], Freq *-1 , Freq)) %>% mutate(Age=AGEgroup)

gender_1_survey <- unique(paste0("",gender_map[1]," ",ages$origin,""))
gender_1_population <-  unique(paste0("",gender_map[1]," ",pop_age$origin,""))
gender_2_survey <- unique(paste0("",gender_map[2]," ",ages$origin,""))
gender_2_population <- unique(paste0("",gender_map[2]," ",pop_age$origin,""))

dodge <- position_dodge(width = 0.9)

legend_order <- c(gender_1_survey, gender_1_population, gender_2_survey, gender_2_population)

combined <- combined %>% filter(!Gender %in% gender_no)

# Plot

pyramid_plot <- ggplot(combined) +
  geom_col(aes(fill = interaction(Gender, origin, sep = " "),y = Freq, x = Age), position = dodge) +
  geom_text(aes(label = paste(round(abs(Freq), 1), " %"), 
                y = ifelse(Freq >= 0, Freq + 1, Freq - 2),     x = Age,group = interaction(origin, Gender)),position = dodge, vjust = 0.5, hjust = 0.5, size = 4) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c(color_secondary_100, color_secondary_10, color_primary_100, color_primary_10),name = "",limits = legend_order)+
  coord_flip() +
  facet_wrap(.~ Gender, scale = "free_x", strip.position = "bottom") +
  custom_theme() +
  labs(x = NULL, y = NULL, 
       title = tr("sampling.survey_vs_population"),
       caption = gender_source_caption
  )




# ------------------------------------------------------------
# Sampling comparison charts
# ------------------------------------------------------------

# ---- Sampling 1 ----

if (isTRUE(check_strata1)) {
  
  ####################################
  #          Strata 1 Charts         #
  ####################################
  
  ## 1. Survey data preparation ----
  
  strata1_counts <- table(
    data[[strata1_tab]],
    useNA = "no"
  )
  
  strata1_survey <- data.frame(
    lev = names(strata1_counts),
    Percentage = as.vector(strata1_counts) /
      sum(strata1_counts) * 100
  ) %>%
    dplyr::filter(
      !is.na(lev),
      !lev %in% display_no
    ) %>%
    dplyr::arrange(Percentage)
  
  # Survey levels
  strata1_levels_survey <- as.character(
    strata1_survey$lev
  )
  
  ## 2. Check population data availability ----
  
  has_pop_strata1 <- (
    exists("pop_strata1") &&
      !is.null(pop_strata1) &&
      is.data.frame(pop_strata1) &&
      all(
        c("Level", "Percentage") %in%
          names(pop_strata1)
      ) &&
      any(!is.na(pop_strata1$Percentage))
  )
  
  
  ## 3. Align survey and population levels ----
  
  if (has_pop_strata1) {
    
    strata1_levels_population <- pop_strata1$Level %>%
      as.character() %>%
      (\(x) x[!is.na(x)])() %>%
      setdiff(display_no) %>%
      unique()
    
    extra_levels1 <- setdiff(
      strata1_levels_survey,
      strata1_levels_population
    )
    
    strata1_levels <- c(
      strata1_levels_population,
      extra_levels1
    )
    
  } else {
    
    strata1_levels <- strata1_levels_survey
  }
  
  strata1_survey <- strata1_survey %>%
    dplyr::mutate(
      lev = factor(
        lev,
        levels = strata1_levels
      ),
      Applicable = !is.na(Percentage)
    )
  
  
  ## 4. Common y-axis maximum ----
  
  if (has_pop_strata1) {
    
    strata1_population_values <- suppressWarnings(
      as.numeric(pop_strata1$Percentage)
    )
    
    y_max1 <- max(
      c(
        strata1_survey$Percentage,
        strata1_population_values
      ),
      na.rm = TRUE
    ) + 10
    
  } else {
    
    y_max1 <- max(
      strata1_survey$Percentage,
      na.rm = TRUE
    ) + 10
  }
  
  
  ## 5. Survey plot ----
  
  strata1_plot_survey <- ggplot(
    strata1_survey,
    aes(
      x = lev,
      y = Percentage,
      fill = lev
    )
  ) +
    geom_col() +
    geom_text(
      aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    coord_flip() +
    custom_theme() +
    scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata1_survey$lev)
      )
    ) +
    scale_x_discrete(
      drop = FALSE,
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    scale_y_continuous(
      limits = c(
        0,
        y_max1
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0.02)
      )
    ) +
    labs(
      title = tr("sampling.title"),
      subtitle = tr("sampling.from_survey"),
      x = NULL,
      y = tr("common.percentage"),
      caption = chart_source(ns_name)
    ) +
    theme(
      legend.position = "none"
    )
  
  
  ## 6. Population plot or placeholder ----
  
  if (has_pop_strata1) {
    
    strata1_population <- pop_strata1 %>%
      dplyr::mutate(
        Percentage = suppressWarnings(
          as.numeric(Percentage)
        ),
        Applicable = !is.na(Percentage),
        Level = factor(
          Level,
          levels = strata1_levels
        )
      )
    
    strata1_plot_population <- ggplot(
      strata1_population,
      aes(
        x = Level,
        y = Percentage,
        fill = Level
      )
    ) +
      geom_col(
        na.rm = TRUE
      ) +
      geom_text(
        data = dplyr::filter(
          strata1_population,
          Applicable
        ),
        aes(
          label = paste0(
            round(Percentage, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      geom_text(
        data = dplyr::filter(
          strata1_population,
          !Applicable
        ),
        aes(
          x = Level,
          y = y_max1 * 0.05,
          label = tr("common.no_data")
        ),
        inherit.aes = FALSE,
        color = color_label_grey,
        hjust = 0,
        size = 4
      ) +
      coord_flip() +
      custom_theme() +
      scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata1_population$Level)
        )
      ) +
      scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      scale_y_continuous(
        limits = c(
          0,
          y_max1
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0.02)
        )
      ) +
      labs(
        title = tr("sampling.population"),
        subtitle = strata1_org,
        x = NULL,
        y = tr("common.percentage"),
        caption = chart_source(strata1_source)
      ) +
      theme(
        legend.position = "none"
      )
    
  } else {
    
    placeholder_plot1 <- grid::textGrob(
      tr(
        "sampling.category_data_missing",
        strata1_tab
      ),
      gp = grid::gpar(
        fontsize = 14,
        fontface = "italic",
        col = "red"
      ),
      just = "center"
    )
  }
  
  
  
  # End of Strata 1
}

# ---- Original chunk: sampling_2 ----
if (isTRUE(check_strata2)) {
  
  ####################################
  #       Strata 2 Charts           #
  ####################################
  ## 1. Data preparation ----
  
  # Survey frequencies
  strata2_counts <- table(
    data[[strata2_tab]],
    useNA = "no"
  )
  
  strata2 <- data.frame(
    Category = names(strata2_counts),
    Percentage = as.vector(strata2_counts) /
      sum(strata2_counts) * 100
  ) %>%
    dplyr::filter(
      !Category %in% display_no
    ) %>%
    dplyr::arrange(Percentage) %>%
    dplyr::mutate(
      Applicable = Percentage > 0
    )
  
  # Store survey order
  strata2_levels <- as.character(strata2$Category)
  
  # Apply factor order
  strata2 <- strata2 %>%
    dplyr::mutate(
      Category = factor(
        Category,
        levels = strata2_levels
      )
    )
  
  ## 2. Survey plot ----
  
  survey_y_max <- max(
    strata2$Percentage,
    na.rm = TRUE
  ) + 10
  
  strata2_plot_survey <- ggplot(
    strata2,
    aes(
      x = Category,
      y = Percentage,
      fill = Category
    )
  ) +
    geom_col() +
    geom_text(
      aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    coord_flip() +
    custom_theme() +
    scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata2$Category)
      )
    ) +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    scale_y_continuous(
      limits = c(0, survey_y_max),
      expand = expansion(
        mult = c(0, 0.02)
      )
    ) +
    labs(
      title = tr("sampling.title"),
      subtitle = tr("sampling.from_survey"),
      x = NULL,
      y = tr("common.percentage"),
      caption = chart_source(ns_name)
    ) +
    theme(
      legend.position = "none"
    )
  
  
  ## 3. Population plot or placeholder ----
  
  has_pop_strata2 <- (
    exists("pop_strata2") &&
      !is.null(pop_strata2) &&
      is.data.frame(pop_strata2) &&
      all(c("Level", "Percentage") %in% names(pop_strata2)) &&
      any(!is.na(pop_strata2$Percentage))
  )
  
  if (has_pop_strata2) {
    
    strata2_population <- pop_strata2 %>%
      dplyr::mutate(
        Applicable = !is.na(Percentage) & Percentage != 0,
        Percentage_plot = dplyr::if_else(
          Applicable,
          as.numeric(Percentage),
          NA_real_
        ),
        Level = factor(
          Level,
          levels = strata2_levels
        )
      )
    
    population_y_max <- max(
      strata2_population$Percentage_plot,
      na.rm = TRUE
    ) + 10
    
    y_max <- max(
      survey_y_max,
      population_y_max,
      na.rm = TRUE
    )
    
    strata2_plot_population <- ggplot(
      strata2_population,
      aes(
        x = Level,
        y = Percentage_plot,
        fill = Level
      )
    ) +
      geom_col(
        na.rm = TRUE
      ) +
      geom_text(
        data = dplyr::filter(
          strata2_population,
          Applicable
        ),
        aes(
          label = paste0(
            round(Percentage_plot, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      geom_text(
        data = dplyr::filter(
          strata2_population,
          !Applicable
        ),
        mapping = aes(
          x = Level,
          y = y_max * 0.05,
          label = tr("common.no_data")
        ),
        inherit.aes = FALSE,
        color = "red",
        hjust = 0,
        size = 4
      ) +
      coord_flip() +
      custom_theme() +
      scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata2_population$Level)
        )
      ) +
      scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      scale_y_continuous(
        limits = c(0, y_max),
        expand = expansion(
          mult = c(0, 0.02)
        )
      ) +
      labs(
        title = tr("sampling.population"),
        subtitle = strata2_org,
        x = NULL,
        y = tr("common.percentage"),
        caption = chart_source(strata2_source)
      ) +
      theme(
        legend.position = "none"
      )
    
    
    
  } else {
    
    placeholder_plot2 <- grid::textGrob(
      tr(
        "sampling.category_data_missing",
        strata2_tab
      ),
      gp = grid::gpar(
        fontsize = 14,
        fontface = "italic",
        col = "red"
      ),
      just = "center"
    )
    
  }
  
  
  #End of Strata 2
}



# ---- Sampling 3 ----

if (isTRUE(check_strata3)) {
  
  ####################################
  #          Strata 3 Charts         #
  ####################################
  
  ## 1. Survey data preparation ----
  
  strata3_counts <- table(
    data[[strata3_tab]],
    useNA = "no"
  )
  
  strata3 <- data.frame(
    Category = names(strata3_counts),
    Percentage = as.vector(strata3_counts) /
      sum(strata3_counts) * 100
  ) %>%
    dplyr::filter(
      !Category %in% display_no
    ) %>%
    dplyr::arrange(Percentage) %>%
    dplyr::mutate(
      Applicable = Percentage > 0
    )
  
  # Store survey order
  strata3_levels <- as.character(strata3$Category)
  
  # Apply factor order
  strata3 <- strata3 %>%
    dplyr::mutate(
      Category = factor(
        Category,
        levels = strata3_levels
      )
    )
  
  strata3_category <- setdiff(
    as.character(strata3$Category),
    display_no
  )
  
  ## 2. Survey plot ----
  
  survey_y_max3 <- max(
    strata3$Percentage,
    na.rm = TRUE
  ) + 10
  
  strata3_plot_survey <- ggplot(
    strata3,
    aes(
      x = Category,
      y = Percentage,
      fill = Category
    )
  ) +
    geom_col() +
    geom_text(
      aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    coord_flip() +
    custom_theme() +
    scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata3$Category)
      )
    ) +
    scale_x_discrete(
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    scale_y_continuous(
      limits = c(
        0,
        survey_y_max3
      ),
      expand = expansion(
        mult = c(0, 0.02)
      )
    ) +
    labs(
      title = tr("sampling.title"),
      subtitle = tr("sampling.from_survey"),
      x = NULL,
      y = tr("common.percentage"),
      caption = chart_source(ns_name)
    ) +
    theme(
      legend.position = "none"
    )
  
  
  ## 3. Check population data availability ----
  
  has_pop_strata3 <- (
    exists("pop_strata3") &&
      !is.null(pop_strata3) &&
      is.data.frame(pop_strata3) &&
      all(
        c("Level", "Percentage") %in%
          names(pop_strata3)
      ) &&
      any(!is.na(pop_strata3$Percentage))
  )
  
  
  ## 4. Population plot or placeholder ----
  
  if (has_pop_strata3) {
    
    strata3_population <- pop_strata3 %>%
      dplyr::mutate(
        Percentage = as.numeric(Percentage),
        Applicable = !is.na(Percentage),
        Level = factor(
          Level,
          levels = strata3_levels
        )
      )
    
    y_max3 <- max(
      c(
        strata3$Percentage,
        strata3_population$Percentage
      ),
      na.rm = TRUE
    ) + 10
    
    
    strata3_plot_population <- ggplot(
      strata3_population,
      aes(
        x = Level,
        y = Percentage,
        fill = Level
      )
    ) +
      geom_col(
        na.rm = TRUE
      ) +
      geom_text(
        data = dplyr::filter(
          strata3_population,
          Applicable
        ),
        aes(
          label = paste0(
            round(Percentage, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      geom_text(
        data = dplyr::filter(
          strata3_population,
          !Applicable
        ),
        aes(
          x = Level,
          y = y_max3 * 0.05,
          label = tr("common.no_data")
        ),
        inherit.aes = FALSE,
        color = "red",
        hjust = 0,
        size = 4
      ) +
      coord_flip() +
      custom_theme() +
      scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata3_population$Level)
        )
      ) +
      scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      scale_y_continuous(
        limits = c(
          0,
          y_max3
        ),
        expand = expansion(
          mult = c(0, 0.02)
        )
      ) +
      labs(
        title = tr("sampling.population"),
        subtitle = strata3_org,
        x = NULL,
        y = tr("common.percentage"),
        caption = chart_source(strata3_source)
      ) +
      theme(
        legend.position = "none"
      )
    
    strata3_plot <- gridExtra::grid.arrange(
      strata3_plot_survey,
      strata3_plot_population,
      nrow = 1
    )
    
  } else {
    
    placeholder_plot3 <- grid::textGrob(
      tr(
        "sampling.category_data_missing",
        strata3_tab
      ),
      gp = grid::gpar(
        fontsize = 14,
        fontface = "italic",
        col = "red"
      ),
      just = "center"
    )
    
    strata3_plot <- gridExtra::grid.arrange(
      strata3_plot_survey,
      placeholder_plot3,
      nrow = 1
    )
  }
  
  # End of Strata 3
}


message("✓ ", tr("runtime.sampling_charts_loaded"))
