##########################################################
#.  COMMUNITY TRUST INDEX - ADVANCED CHARTS
##########################################################

# ============================================================
# CTI Report - charts_advanced.R
# Weighting comparison and driver correlation preparation
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


# ============================================================
# 1. Weighting comparison chart
# ============================================================

if (
  !exists("score_prefixes", inherits = TRUE) ||
  length(score_prefixes) == 0L
) {
  stop(
    "score_prefixes is missing. Load the module configuration before charts_advanced.R.",
    call. = FALSE
  )
}

weighting_score_keys <- names(score_prefixes)

weighting_title_map <- if (toupper(module_code) == "INST") {
  c(
    comp = tr("score.competencies"),
    values = tr("score.values")
  )
} else {
  c(
    disaster = tr("score.disaster_risk_knowledge"),
    detection = tr("score.detection_monitoring_forecasting"),
    dissemination = tr("score.warning_dissemination_communication"),
    response = tr("score.preparedness_response_capabilities")
  )
}

for (i in seq_along(weighting_score_keys)) {
  key <- weighting_score_keys[i]
  if (!(key %in% names(weighting_title_map))) {
    weighting_title_map[key] <- score_dimensions[i]
  }
}

translate_short_label <- function(variable, short_label) {
  variable <- as.character(variable)
  short_label <- as.character(short_label)
  
  vapply(
    seq_along(variable),
    function(i) {
      if (is.na(variable[[i]])) {
        return(tr_variable(short_label[[i]])[[1L]])
      }
      
      # Prefer translations configured against the question code. If there is
      # no code-specific entry, match the visible short label in the shared or
      # custom translation dictionary.
      label_from_code <- tr_variable(
        variable[[i]],
        label = "short",
        fallback = NA_character_
      )
      
      if (!is.na(label_from_code) &&
          !identical(label_from_code, variable[[i]])) {
        return(label_from_code)
      }
      
      tr_variable(short_label[[i]])[[1L]]
    },
    character(1)
  )
}

weighting_facet_label <- function(x) {
  x_chr <- as.character(x)
  dimension_index <- match(
    tolower(x_chr),
    tolower(as.character(score_dimensions))
  )
  translated_titles <- unname(
    weighting_title_map[weighting_score_keys]
  )
  
  out <- x_chr
  matched <- !is.na(dimension_index)
  out[matched] <- translated_titles[dimension_index[matched]]
  
  vapply(
    out,
    function(label) {
      wrapped <- stringr::str_wrap(label, width = 26)
      lines <- strsplit(wrapped, "\n", fixed = TRUE)[[1L]]
      
      if (length(lines) <= 2L) {
        return(wrapped)
      }
      
      paste0(
        lines[1L],
        "\n",
        stringr::str_trunc(lines[2L], width = 24, side = "right")
      )
    },
    character(1)
  )
}

df_long <- df3 %>%
  reshape2::melt(
    id.vars       = c("Dimension", "Drivers"),
    variable.name = "variable",
    value.name    = "value"
  ) %>%
  dplyr::mutate(
    Drivers_display   = tr_variable(as.character(Drivers)),
    variable_display  = tr_variable(as.character(variable))
  )

# Use this value in the report chunk: fig.height = weighting_plot_height.
# Four EWS facets need more vertical space than the two Institutional ones.
weighting_plot_height <- if (
  dplyr::n_distinct(df_long$Dimension) > 2L
) 12 else 7

weighting_plot <- ggplot2::ggplot(
  df_long,
  ggplot2::aes(
    x = Drivers_display,
    y = value,
    group = variable_display,
    color = variable_display
  )
) +
  ggplot2::geom_line(
    position = ggplot2::position_dodge(width = 0.45),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    position = ggplot2::position_dodge(width = 0.45),
    size = 2.4
  ) +
  ggplot2::geom_label(
    ggplot2::aes(
      label = sprintf("%.2f", value)
    ),
    position = ggplot2::position_dodge(width = 0.45),
    vjust = -0.75,
    fill = "white",
    alpha = 0.92,
    label.size = 0,
    label.padding = grid::unit(1.5, "pt"),
    size = 3.2,
    show.legend = FALSE
  ) +
  ggplot2::facet_wrap(
    ~ Dimension,
    nrow = 2,
    ncol = 2,
    scales = "free_x",
    labeller = ggplot2::as_labeller(weighting_facet_label)
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2),
    expand = ggplot2::expansion(mult = c(0.02, 0.14))
  ) +
  custom_theme() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = 9,
      angle = 45,
      vjust = 1,
      hjust = 1
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 10),
      size = 12
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.spacing = grid::unit(1.6, "lines"),
    strip.background = ggplot2::element_rect(
      fill = "#F1F3F5",
      color = NA
    ),
    strip.text = ggplot2::element_text(
      size = 11,
      face = "bold",
      lineheight = 0.9,
      margin = ggplot2::margin(6, 10, 6, 10)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 10),
    plot.margin = ggplot2::margin(14, 20, 14, 14)
  ) +
  ggplot2::labs(
    x = NULL,
    y = tr("score.score_variation")
  )


# ============================================================
# 2. Driver correlation preparation
# ============================================================

score_map_full <- score_map
score_map_full["Don't know"] <- 5
score_map_full["Don’t know"] <- 5

score_driver_response <- function(x) {
  x_chr <- as.character(x)
  mapped <- unname(score_map_full[x_chr])
  numeric_value <- suppressWarnings(as.numeric(x_chr))
  
  dplyr::if_else(
    !is.na(mapped),
    as.numeric(mapped),
    numeric_value
  )
}

move_overall_last <- function(mat) {
  if (is.null(mat) || !"Overall" %in% colnames(mat)) {
    return(mat)
  }
  
  ord <- setdiff(colnames(mat), "Overall")
  
  mat[
    c(ord, "Overall"),
    c(ord, "Overall"),
    drop = FALSE
  ]
}

get_driver_source <- function() {
  if (exists("survey_data", inherits = TRUE)) {
    return(get("survey_data", inherits = TRUE))
  }
  
  data
}

# ------------------------------------------------------------
# Safe pairwise correlation significance test
# Shared by dimension-level and Institutional combined correlations
# ------------------------------------------------------------

# Safe pairwise significance matrix.
# Some pairs may still have fewer than 3 overlapping finite values,
# even if each variable individually has enough observations.
safe_cor_mtest <- function(mat, conf.level = 0.95) {
  
  mat <- as.data.frame(mat)
  n <- ncol(mat)
  
  p.mat <- matrix(
    NA_real_,
    nrow = n,
    ncol = n,
    dimnames = list(names(mat), names(mat))
  )
  
  lowCI.mat <- p.mat
  uppCI.mat <- p.mat
  
  diag(p.mat) <- 0
  diag(lowCI.mat) <- 1
  diag(uppCI.mat) <- 1
  
  if (n < 2L) {
    return(
      list(
        p = p.mat,
        lowCI = lowCI.mat,
        uppCI = uppCI.mat
      )
    )
  }
  
  for (i in seq_len(n - 1L)) {
    for (j in (i + 1L):n) {
      
      ok <- is.finite(mat[[i]]) &
        is.finite(mat[[j]])
      
      if (sum(ok) < 3L) {
        next
      }
      
      test <- tryCatch(
        stats::cor.test(
          mat[[i]][ok],
          mat[[j]][ok],
          conf.level = conf.level,
          method = "pearson"
        ),
        error = function(e) NULL
      )
      
      if (is.null(test)) {
        next
      }
      
      p.mat[i, j] <- p.mat[j, i] <- test$p.value
      
      if (!is.null(test$conf.int) &&
          length(test$conf.int) == 2L) {
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- test$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- test$conf.int[2]
      }
    }
  }
  
  list(
    p = p.mat,
    lowCI = lowCI.mat,
    uppCI = uppCI.mat
  )
}


build_dimension_correlation <- function(
    dimension_key,
    dimension_label,
    driver_vars
) {
  
  source_data <- get_driver_source()
  
  driver_vars <- intersect(
    driver_vars,
    names(source_data)
  )
  
  if (length(driver_vars) < 2L) {
    warning(
      paste0(
        "Not enough driver columns for ",
        dimension_label,
        " to compute a correlation matrix."
      ),
      call. = FALSE
    )
    return(NULL)
  }
  
  driver_rename_map <- question_code %>%
    dplyr::filter(variable %in% driver_vars) %>%
    dplyr::mutate(
      short_label_display = translate_short_label(
        variable,
        short_label
      )
    ) %>%
    dplyr::select(
      variable,
      short_label_display
    ) %>%
    tibble::deframe()
  
  survey_dimension <- source_data %>%
    dplyr::select(
      dplyr::all_of(driver_vars)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        score_driver_response
      )
    ) %>%
    dplyr::rename_with(
      ~ driver_rename_map[.x],
      .cols = dplyr::all_of(driver_vars)
    ) %>%
    dplyr::mutate(
      Overall = rowMeans(
        dplyr::across(
          dplyr::everything()
        ),
        na.rm = TRUE
      )
    )
  
  survey_dimension$Overall[
    is.nan(survey_dimension$Overall)
  ] <- NA_real_
  
  # Remove variables with too few finite observations.
  # cor.test() requires enough finite paired values and otherwise errors.
  finite_n <- vapply(
    survey_dimension,
    function(x) sum(is.finite(x)),
    integer(1)
  )
  
  survey_dimension <- survey_dimension[
    ,
    finite_n >= 3L,
    drop = FALSE
  ]
  
  if (ncol(survey_dimension) < 2L) {
    warning(
      paste0(
        "Not enough usable variables for ",
        dimension_label,
        " to compute a correlation matrix."
      ),
      call. = FALSE
    )
    return(NULL)
  }
  
  test_unweighted <- safe_cor_mtest(
    survey_dimension,
    conf.level = 0.95
  )
  
  cor_unweighted <- stats::cor(
    survey_dimension,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  if ("weight" %in% names(data)) {
    weight_vector <- as.numeric(data$weight)
  } else if ("_weight" %in% names(data)) {
    weight_vector <- as.numeric(data$`_weight`)
  } else {
    weight_vector <- rep(1, nrow(survey_dimension))
  }
  
  if (length(weight_vector) != nrow(survey_dimension)) {
    weight_vector <- rep(1, nrow(survey_dimension))
  }
  
  survey_dimension_weighted <- survey_dimension %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ .x * weight_vector
      )
    )
  
  cor_weighted <- stats::cor(
    survey_dimension_weighted,
    use = "pairwise.complete.obs",
    method = "pearson"
  )
  
  cor_unweighted <- move_overall_last(cor_unweighted)
  cor_weighted <- move_overall_last(cor_weighted)
  test_unweighted$p <- move_overall_last(test_unweighted$p)
  
  if (!is.null(test_unweighted$lowCI)) {
    test_unweighted$lowCI <- move_overall_last(
      test_unweighted$lowCI
    )
  }
  
  if (!is.null(test_unweighted$uppCI)) {
    test_unweighted$uppCI <- move_overall_last(
      test_unweighted$uppCI
    )
  }
  
  list(
    key = dimension_key,
    dimension = dimension_label,
    variables = driver_vars,
    data = survey_dimension,
    data_weighted = survey_dimension_weighted,
    test = test_unweighted,
    cor_unweighted = cor_unweighted,
    cor_weighted = cor_weighted
  )
}


# ============================================================
# 3. Compute correlations for active dimensions
# ============================================================

correlation_results <- list()

for (i in seq_along(score_prefixes)) {
  
  dimension_key <- names(score_prefixes)[i]
  dimension_label <- tr_variable(
    score_dimensions[i],
    fallback = score_dimensions[i]
  )
  
  driver_vars <- if (
    exists("score_columns", inherits = TRUE) &&
    dimension_key %in% names(score_columns)
  ) {
    score_columns[[dimension_key]]
  } else {
    prefix <- score_prefixes[[dimension_key]]
    names(data)[
      grepl(
        paste0("^", prefix, "_"),
        names(data)
      )
    ]
  }
  
  result <- build_dimension_correlation(
    dimension_key = dimension_key,
    dimension_label = dimension_label,
    driver_vars = driver_vars
  )
  
  correlation_results[[dimension_key]] <- result
  
  if (is.null(result)) {
    next
  }
  
  assign(
    paste0("drivers_", dimension_key),
    result$data,
    envir = .GlobalEnv
  )
  
  assign(
    paste0("test_", dimension_key),
    result$test,
    envir = .GlobalEnv
  )
  
  assign(
    paste0("cor_", dimension_key),
    result$cor_unweighted,
    envir = .GlobalEnv
  )
  
  assign(
    paste0("cor_", dimension_key, "_weighted"),
    result$cor_weighted,
    envir = .GlobalEnv
  )
}


# ============================================================
# 4. Backward compatibility
# ============================================================


corr_plot_height <- 12

if (toupper(module_code) == "INST") {
  
  available_inst <- correlation_results[
    !vapply(
      correlation_results,
      is.null,
      logical(1)
    )
  ]
  
  if (length(available_inst) > 0L) {
    
    institutional_driver_data <- lapply(
      available_inst,
      function(x) {
        x$data %>%
          dplyr::select(
            -dplyr::any_of("Overall")
          )
      }
    )
    
    driver_unweighted <- dplyr::bind_cols(
      institutional_driver_data
    )
    
    if (anyDuplicated(names(driver_unweighted))) {
      names(driver_unweighted) <- make.unique(
        names(driver_unweighted),
        sep = "_"
      )
    }
    
    cor_matrix_uw <- stats::cor(
      driver_unweighted,
      use = "pairwise.complete.obs",
      method = "pearson"
    )
    
    if ("weight" %in% names(data)) {
      institutional_weight <- as.numeric(data$weight)
    } else if ("_weight" %in% names(data)) {
      institutional_weight <- as.numeric(data$`_weight`)
    } else {
      institutional_weight <- rep(
        1,
        nrow(driver_unweighted)
      )
    }
    
    driver_weighted <- driver_unweighted %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ .x * institutional_weight
        )
      )
    
    cor_matrix_w <- stats::cor(
      driver_weighted,
      use = "pairwise.complete.obs",
      method = "pearson"
    )
    
    testRes <- safe_cor_mtest(
      driver_unweighted,
      conf.level = 0.95
    )
  }
}



# ============================================================
# 5. Correlation plots - EWS only
# ============================================================

# Build the four EWS correlation plots here so the Rmd only needs to
# call one prepared function.


correlation_ews_plot <- NULL

if (toupper(module_code) == "EWS") {
  
  n_corr_plot_row <- length(unique(all_pillars_df$question))
  n_dim <- length (ews_pillar_dimensions)/2
  corr_plot_height <- n_corr_plot_row*n_dim*0.7
  
  correlation_ews_plot <- function() {
    
    graphics::par(mfrow = c(2, 2))
    
    if (exists("cor_disaster", inherits = TRUE)) {
      corrplot::corrplot(
        cor_disaster,
        p.mat       = test_disaster$p,
        sig.level   = 0.10,
        method      = "circle",
        type        = "lower",
        order       = "original",
        addrect     = 2,
        col         = corrplot::COL2("PuOr", 10),
        insig       = "blank",
        tl.col      = color_label_grey,
        tl.cex      = 0.8,
        addCoef.col = "white",
        number.cex  = 0.65,
        diag        = FALSE,
        addgrid.col = "white",
        title       = tr("score.pillar1.long_label"),
        mar         = c(0, 0, 2, 0)
      )
    }
    
    if (exists("cor_detection", inherits = TRUE)) {
      corrplot::corrplot(
        cor_detection,
        p.mat       = test_detection$p,
        sig.level   = 0.10,
        method      = "circle",
        type        = "lower",
        order       = "original",
        addrect     = 2,
        col         = corrplot::COL2("PuOr", 10),
        insig       = "blank",
        tl.col      = color_label_grey,
        tl.cex      = 0.8,
        addCoef.col = "white",
        number.cex  = 0.65,
        diag        = FALSE,
        addgrid.col = "white",
        title       = tr("score.pillar2.long_label"),
        mar         = c(0, 0, 2, 0)
      )
    }
    
    if (exists("cor_dissemination", inherits = TRUE)) {
      corrplot::corrplot(
        cor_dissemination,
        p.mat       = test_dissemination$p,
        sig.level   = 0.10,
        method      = "circle",
        type        = "lower",
        order       = "original",
        addrect     = 2,
        col         = corrplot::COL2("PuOr", 10),
        insig       = "blank",
        tl.col      = color_label_grey,
        tl.cex      = 0.8,
        addCoef.col = "white",
        number.cex  = 0.65,
        diag        = FALSE,
        addgrid.col = "white",
        title       = tr("score.pillar3.long_label"),
        mar         = c(0, 0, 2, 0)
      )
    }
    
    if (exists("cor_response", inherits = TRUE)) {
      corrplot::corrplot(
        cor_response,
        p.mat       = test_response$p,
        sig.level   = 0.10,
        method      = "circle",
        type        = "lower",
        order       = "original",
        addrect     = 2,
        col         = corrplot::COL2("PuOr", 10),
        insig       = "blank",
        tl.col      = color_label_grey,
        tl.cex      = 0.8,
        addCoef.col = "white",
        number.cex  = 0.65,
        diag        = FALSE,
        addgrid.col = "white",
        title       = tr("score.pillar4.long_label"),
        mar         = c(0, 0, 2, 0)
      )
    }
    
    graphics::par(mfrow = c(1, 1))
  }
}


message("✓ ", tr("runtime.charts_loaded"))
