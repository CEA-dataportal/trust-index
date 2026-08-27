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


check_strata3 <-
  (exists("pop_strata3") && !is.null(pop_strata3)) ||
  (!is.null(strata3_tab) && strata3_tab %in% names(data))



#geographic questions
if ((!is.null(adm1) && adm1 != "") ||
    (!is.null(adm2) && adm2 != "") ||
    (!is.null(locality) && locality != "")) {
message("All administrative levels are available. Proceeding...")
check_adm <- TRUE
} else { check_adm <- FALSE}



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
geoname_survey <-  get(geoname_survey)    #  from survey data (e.g. adm1 or adm2 without "") (a changer dans EWS)
#geoname_pop    <- "Admin1" # from population (a changer dans EWS)


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
    
    all_plot_levels <- rev(c(sort(top9_regions), "Others"))
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
#
# IMPORTANT:
# Build the survey chart from survey data itself. The previous version
# depended on `all_plot_levels`, which is derived from population_geo.
# If population geography is absent or region labels do not match exactly,
# valid survey values in `data_region` can be dropped and the chart can
# appear empty.

if (geoname_survey %in% names(data)) {

  geo_survey_counts <- data %>%
    dplyr::filter(
      !is.na(.data[[geoname_survey]]),
      trimws(as.character(.data[[geoname_survey]])) != "",
      !.data[[geoname_survey]] %in% excluded_regions
    ) %>%
    dplyr::count(
      District = .data[[geoname_survey]],
      name = "Count"
    )

  # If population geography is available, retain the same top-9 / Others
  # grouping for comparability. Otherwise show all survey regions.
  if (
    exists("total_region_count") &&
    length(top9_regions) > 0 &&
    total_region_count > 10
  ) {

    geo_survey_counts <- geo_survey_counts %>%
      dplyr::mutate(
        District = dplyr::if_else(
          District %in% top9_regions,
          District,
          "Others"
        )
      ) %>%
      dplyr::group_by(District) %>%
      dplyr::summarise(
        Count = sum(Count),
        .groups = "drop"
      )

    survey_plot_levels <- rev(
      c(
        sort(intersect(top9_regions, geo_survey_counts$District)),
        if ("Others" %in% geo_survey_counts$District) "Others" else NULL
      )
    )

  } else {

    survey_plot_levels <- rev(
      sort(unique(geo_survey_counts$District))
    )
  }

  geo_survey <- geo_survey_counts %>%
    dplyr::mutate(
      Percentage = Count / sum(Count) * 100,
      Applicable = Count > 0,
      District = factor(
        District,
        levels = survey_plot_levels
      )
    )

  perc_noregion <- mean(
    is.na(data[[geoname_survey]]) |
      trimws(as.character(data[[geoname_survey]])) == "" |
      data[[geoname_survey]] %in% excluded_regions,
    na.rm = TRUE
  ) * 100

  if (nrow(geo_survey) > 0 && sum(geo_survey$Count, na.rm = TRUE) > 0) {

    geo_plot_survey <- ggplot2::ggplot(
      geo_survey,
      ggplot2::aes(
        x = District,
        y = Percentage,
        fill = District
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(round(Percentage, 1), "%")
        ),
        hjust = -0.3,
        size = 4
      ) +
      ggplot2::coord_flip() +
      custom_theme() +
      ggplot2::scale_fill_manual(
        values = rep(
          color_primary_100,
          nrow(geo_survey)
        )
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          0,
          max(geo_survey$Percentage, na.rm = TRUE) + 10
        )
      ) +
      ggplot2::labs(
        title = "Sampling",
        subtitle = "From Survey",
        x = NULL,
        caption = paste0(
          "Note: missing, excluded or non-response geographic values total ",
          round(perc_noregion, 1),
          "%.
Source: ",
          ns_name
        )
      ) +
      ggplot2::theme(
        legend.position = "none"
      )

  } else {

    geo_plot_survey <- grid::textGrob(
      "No valid geographic survey data available",
      gp = grid::gpar(
        fontsize = 16,
        fontface = "italic",
        col = "gray30"
      )
    )
  }

} else {

  geo_plot_survey <- grid::textGrob(
    "Region column missing from survey data",
    gp = grid::gpar(
      fontsize = 16,
      fontface = "italic",
      col = "gray30"
    )
  )
}

# Population Data aligned with top9
if (!is.null(population_geo) && length(top9_regions) > 0 && !is.null(all_plot_levels)) {
  
  region_population <- population_geo %>%
    mutate(
      District = if (length(top9_regions) < total_region_count) {
        if_else(.data[[geoname_pop]] %in% top9_regions, .data[[geoname_pop]], "Others")
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
  
  geo_plot_population <- ggplot2::ggplot(
    region_population,
    ggplot2::aes(
      x = District,
      y = Percentage,
      fill = District
    )
  ) +
    ggplot2::geom_col(na.rm = TRUE) +
    ggplot2::geom_text(
      data = subset(region_population, Applicable),
      ggplot2::aes(label = paste0(round(Percentage, 1), "%")),
      hjust = -0.3,
      size = 4
    ) +
    ggplot2::geom_text(
      data = subset(region_population, !Applicable),
      ggplot2::aes(y = 5, label = "no data"),
      color = "red",
      hjust = 0,
      size = 4
    ) +
    ggplot2::coord_flip() +
    custom_theme() +
    ggplot2::scale_fill_manual(
      values = rep(color_tertiary_100, nrow(region_population))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        max(region_population$Percentage, na.rm = TRUE) + 10
      )
    ) +
    ggplot2::labs(
      title = "Population",
      subtitle = geo_org,
      x = NULL,
      caption = paste0(
        "Note: 'no data' indicates unreported or unavailable values.\nSource: ",
        geo_source, "."
      )
    ) +
    ggplot2::theme(legend.position = "none")
  
} else {
  geo_plot_population <- 
    grid::textGrob(
      "Geographic population data not available",
      gp = grid::gpar(fontsize = 14, fontface = "italic", col = "red")
    )
}


# ---- Original chunk: maps ----
# Compute label points in a projected CRS to avoid longitude/latitude
# warnings from st_point_on_surface(), then transform them back.
regions_lbl <- regions_select %>%
  sf::st_transform(3857) %>%
  dplyr::mutate(
    geometry = sf::st_point_on_surface(geometry)
  ) %>%
  sf::st_transform(sf::st_crs(regions_select))

if (geoname_survey == adm1) {

  map_region <- tm_basemap("CartoDB.Positron")  +
    tm_shape(country_shp) +
    tm_polygons(fill = "grey90", fill_alpha = 0.5, col = "grey50", lwd = 0) +
    
    tm_shape(regions_select) +
    tm_polygons(
      fill       = "n",
      fill_alpha = 0.5,
      fill.scale = tm_scale_intervals(values = "matplotlib.blues",  n = 4),
      fill.legend = tm_legend(title = "Total of respondents", direction = "horizontal"),
      col        = "white",
      lwd        = 0.8,
      id         = adm1_shp,
      id          = adm2_shp,
      popup.vars  = c(
        "Respondents: " = "n"
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
      fill.legend = tm_legend(title = "Total of respondents", direction = "horizontal"),
      col        = "white",
      lwd        = 0.8,
      id         = adm2_shp,
      popup.vars  = c(
        "Respondents: " = "n"
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
  stop("No matching geographies after join. Check join keys / spelling.")
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


ages$origin="Sampling"
pop_age$origin="Population"

perc_nogender <- mean(data$gender %in% gender_no, na.rm = TRUE) * 100

# Caption
if (!is.na(perc_nogender)) {
   gender_caption_text <- paste0(
   "Note: Others or did not answer total ", round(perc_nogender, 1), "%.\nSource:\nSurvey: ",ns_name," - Population data: ", demo_org," (",demo_source,")")
   } else {
  gender_caption_text <- paste0("Source:\nSurvey: ",ns_name," - Population data: ", demo_org," (",demo_source,")")
}

# Combining Survey and Population dataset
combined <- rbind(pop_age,ages)
combined <- combined %>% mutate(Freq = ifelse(Gender == gender_map[1], Freq *-1 , Freq)) %>% mutate(Age=AGEgroup)

gender_1_survey <- unique(paste0("",gender_map[1]," ",ages$origin,""))
gender_1_population <-  unique(paste0("",gender_map[1]," ",pop_age$origin,""))
gender_2_survey <- unique(paste0("",gender_map[2]," ",ages$origin,""))
gender_2_population <- unique(paste0("",gender_map[2]," ",pop_age$origin,""))

dodge <- ggplot2::position_dodge(width = 0.9)

legend_order <- c(gender_1_survey, gender_1_population, gender_2_survey, gender_2_population)

combined <- combined %>% filter(!Gender %in% gender_no)

# Plot

pyramid_plot <- ggplot2::ggplot(combined) +
  ggplot2::geom_col(ggplot2::aes(fill = interaction(Gender, origin, sep = " "),y = Freq, x = Age), position = dodge) +
  ggplot2::geom_text(ggplot2::aes(label = paste(round(abs(Freq), 1), " %"), 
    y = ifelse(Freq >= 0, Freq + 1, Freq - 2),     x = Age,group = interaction(origin, Gender)),position = dodge, vjust = 0.5, hjust = 0.5, size = 4) +
  ggplot2::scale_y_continuous(labels = abs) +
  ggplot2::scale_fill_manual(values = c(color_secondary_100, color_secondary_10, color_primary_100, color_primary_10),name = "",limits = legend_order)+
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(~ Gender, scales = "free_x", strip.position = "bottom") +
  custom_theme() +
  ggplot2::labs(x = NULL, y = NULL, 
       title="Survey vs. Population data",
       caption = gender_caption_text
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
  
  # Percentage of excluded responses
  perc_nocategory1 <- mean(
    data[[strata1_tab]] %in% display_no,
    na.rm = TRUE
  ) * 100
  
  strata1_caption_text <- paste0(
    "Note: ",
    caption_no_str,
    " responses total ",
    round(perc_nocategory1, 1),
    "%.\nSource: ",
    ns_name
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
  
  strata1_plot_survey <- ggplot2::ggplot(
    strata1_survey,
    ggplot2::aes(
      x = lev,
      y = Percentage,
      fill = lev
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    ggplot2::coord_flip() +
    custom_theme() +
    ggplot2::scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata1_survey$lev)
      )
    ) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        y_max1
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0.02)
      )
    ) +
    ggplot2::labs(
      title = "Sampling",
      subtitle = "From Survey",
      x = NULL,
      y = NULL,
      caption = strata1_caption_text
    ) +
    ggplot2::theme(
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
    
    strata1_plot_population <- ggplot2::ggplot(
      strata1_population,
      ggplot2::aes(
        x = Level,
        y = Percentage,
        fill = Level
      )
    ) +
      ggplot2::geom_col(
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata1_population,
          Applicable
        ),
        ggplot2::aes(
          label = paste0(
            round(Percentage, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata1_population,
          !Applicable
        ),
        ggplot2::aes(
          x = Level,
          y = y_max1 * 0.05,
          label = "no data"
        ),
        inherit.aes = FALSE,
        color = color_label_grey,
        hjust = 0,
        size = 4
      ) +
      ggplot2::coord_flip() +
      custom_theme() +
      ggplot2::scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata1_population$Level)
        )
      ) +
      ggplot2::scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          0,
          y_max1
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0.02)
        )
      ) +
      ggplot2::labs(
        title = "Population",
        subtitle = strata1_org,
        x = NULL,
        y = NULL,
        caption = paste0(
          "Note: 'no data' indicates unreported or unavailable values.\n",
          "Source: ",
          strata1_source,
          "."
        )
      ) +
      ggplot2::theme(
        legend.position = "none"
      )
    
  } else {
    
    placeholder_plot1 <- grid::textGrob(
      paste0(
        strata1_tab,
        " data not available"
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
  
  # Percentage of excluded responses
  perc_nocategory <- mean(
    data[[strata2_tab]] %in% display_no,
    na.rm = TRUE
  ) * 100
  
  
  ## 2. Survey plot ----
  
  survey_y_max <- max(
    strata2$Percentage,
    na.rm = TRUE
  ) + 10
  
  strata2_plot_survey <- ggplot2::ggplot(
    strata2,
    ggplot2::aes(
      x = Category,
      y = Percentage,
      fill = Category
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    ggplot2::coord_flip() +
    custom_theme() +
    ggplot2::scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata2$Category)
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, survey_y_max),
      expand = ggplot2::expansion(
        mult = c(0, 0.02)
      )
    ) +
    ggplot2::labs(
      title = "Sampling",
      subtitle = "From Survey",
      x = NULL,
      y = NULL,
      caption = paste0(
        "Note: 'Prefer not to answer' and 'Don't know' responses total ",
        round(perc_nocategory, 1),
        "%.\nSource: ",
        ns_name
      )
    ) +
    ggplot2::theme(
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
    
    strata2_plot_population <- ggplot2::ggplot(
      strata2_population,
      ggplot2::aes(
        x = Level,
        y = Percentage_plot,
        fill = Level
      )
    ) +
      ggplot2::geom_col(
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata2_population,
          Applicable
        ),
        ggplot2::aes(
          label = paste0(
            round(Percentage_plot, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata2_population,
          !Applicable
        ),
        mapping = ggplot2::aes(
          x = Level,
          y = y_max * 0.05,
          label = "no data"
        ),
        inherit.aes = FALSE,
        color = "red",
        hjust = 0,
        size = 4
      ) +
      ggplot2::coord_flip() +
      custom_theme() +
      ggplot2::scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata2_population$Level)
        )
      ) +
      ggplot2::scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_max),
        expand = ggplot2::expansion(
          mult = c(0, 0.02)
        )
      ) +
      ggplot2::labs(
        title = "Population",
        subtitle = strata2_org,
        x = NULL,
        y = NULL,
        caption = paste0(
          "Note: 'no data' indicates unreported or unavailable values.\n",
          "Source: ",
          strata2_source,
          "."
        )
      ) +
      ggplot2::theme(
        legend.position = "none"
      )
    

    
  } else {
    
    placeholder_plot2 <- grid::textGrob(
      paste0(
      strata2_tab,
      " data not available"
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
  
  # Percentage of excluded responses
  perc_nocategory3 <- mean(
    data[[strata3_tab]] %in% display_no,
    na.rm = TRUE
  ) * 100
  
  
  ## 2. Survey plot ----
  
  survey_y_max3 <- max(
    strata3$Percentage,
    na.rm = TRUE
  ) + 10
  
  strata3_plot_survey <- ggplot2::ggplot(
    strata3,
    ggplot2::aes(
      x = Category,
      y = Percentage,
      fill = Category
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0(
          round(Percentage, 1),
          "%"
        )
      ),
      hjust = -0.3,
      size = 4
    ) +
    ggplot2::coord_flip() +
    custom_theme() +
    ggplot2::scale_fill_manual(
      values = rep(
        color_primary_100,
        nlevels(strata3$Category)
      )
    ) +
    ggplot2::scale_x_discrete(
      labels = function(x) {
        stringr::str_wrap(x, 30)
      }
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        survey_y_max3
      ),
      expand = ggplot2::expansion(
        mult = c(0, 0.02)
      )
    ) +
    ggplot2::labs(
      title = "Sampling",
      subtitle = "From Survey",
      x = NULL,
      y = NULL,
      caption = paste0(
        "Note: 'Prefer not to answer' and 'Don't know' responses total ",
        round(perc_nocategory3, 1),
        "%.\nSource: ",
        ns_name
      )
    ) +
    ggplot2::theme(
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
    
    
    strata3_plot_population <- ggplot2::ggplot(
      strata3_population,
      ggplot2::aes(
        x = Level,
        y = Percentage,
        fill = Level
      )
    ) +
      ggplot2::geom_col(
        na.rm = TRUE
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata3_population,
          Applicable
        ),
        ggplot2::aes(
          label = paste0(
            round(Percentage, 1),
            "%"
          )
        ),
        hjust = -0.3,
        size = 4
      ) +
      ggplot2::geom_text(
        data = dplyr::filter(
          strata3_population,
          !Applicable
        ),
        ggplot2::aes(
          x = Level,
          y = y_max3 * 0.05,
          label = "no data"
        ),
        inherit.aes = FALSE,
        color = "red",
        hjust = 0,
        size = 4
      ) +
      ggplot2::coord_flip() +
      custom_theme() +
      ggplot2::scale_fill_manual(
        values = rep(
          color_tertiary_100,
          nlevels(strata3_population$Level)
        )
      ) +
      ggplot2::scale_x_discrete(
        drop = FALSE,
        labels = function(x) {
          stringr::str_wrap(x, 30)
        }
      ) +
      ggplot2::scale_y_continuous(
        limits = c(
          0,
          y_max3
        ),
        expand = ggplot2::expansion(
          mult = c(0, 0.02)
        )
      ) +
      ggplot2::labs(
        title = "Population",
        subtitle = strata3_org,
        x = NULL,
        y = NULL,
        caption = paste0(
          "Note: 'no data' indicates unreported or unavailable values.\n",
          "Source: ",
          strata3_source,
          "."
        )
      ) +
      ggplot2::theme(
        legend.position = "none"
      )
    
    strata3_plot <- gridExtra::grid.arrange(
      strata3_plot_survey,
      strata3_plot_population,
      nrow = 1
    )
    
  } else {
    
    placeholder_plot3 <- grid::textGrob(
      paste0(
        strata3_tab,
        " data not available"
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


message("✓ Sampling Charts and visual outputs loaded")
