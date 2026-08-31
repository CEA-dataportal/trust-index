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

df_long <- df3 %>%
  reshape2::melt(
    id.vars       = c("Dimension", "Drivers"),
    variable.name = "variable",
    value.name    = "value"
  ) %>%
  dplyr::mutate(
    Drivers_display   = tr_variable(as.character(Drivers)),
    Dimension_display = tr_variable(as.character(Dimension)),
    variable_display  = tr_variable(as.character(variable))
  )

weighting_plot <- ggplot2::ggplot(
  df_long,
  ggplot2::aes(
    x = Drivers_display,
    y = value,
    group = variable_display,
    color = variable_display
  )
) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::geom_label(
    ggplot2::aes(
      label = round(value, 2),
      fill = variable_display
    ),
    color = "white",
    linewidth = 0,
    label.r = grid::unit(0.2, "lines"),
    label.padding = grid::unit(2, "pt"),
    size = 4
  ) +
  ggplot2::facet_wrap(
    ~ Dimension_display,
    nrow = 1,
    scales = "free_x"
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      color = "black",
      size = 12,
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 15),
      size = 16
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 15),
      size = 16
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank()
  ) +
  custom_theme() +
  ggplot2::labs(
    x = tr("score.drivers"),
    y = tr("score.score_variation")
  )


# ============================================================
# 2. Driver correlation preparation
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
      short_label_display = tr_variable(
        variable,
        label = "short",
        fallback = short_label
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
  dimension_label <- score_dimensions[i]
  
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
        title       = "Disaster Risk Knowledge",
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
        title       = "Detection, Monitoring & Forecasting",
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
        title       = "Warning Dissemination & Communication",
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
        title       = "Preparedness & Response Capabilities",
        mar         = c(0, 0, 2, 0)
      )
    }
    
    graphics::par(mfrow = c(1, 1))
  }
}


message("✓ ", tr("runtime.charts_loaded"))