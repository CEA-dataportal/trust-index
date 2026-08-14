##########################################################
# COMMUNITY TRUST INDEX - TRANSLATIONS
##########################################################

# Translation dictionary structure:
#   type, key, EN, FR, ES, source_files, usage
#
# Three translation types:
#   template : report/template/chart/table/export/runtime labels
#   variable : question_code labels, dimensions and drivers
#   answer   : response options and categorical data values
#
# Public helpers:
#   tr("report.sampling")
#   tr_variable("gender", "short")
#   tr_variable("Capability")
#   tr_answer(data$gender)
#   tr_data(data$gender)   # backward-compatible alias

# ------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------

.is_blank_translation <- function(x) {
  length(x) == 0L ||
    is.null(x) ||
    is.na(x[1L]) ||
    !nzchar(trimws(as.character(x[1L])))
}

.normalise_language <- function(
    lang,
    supported = c("EN", "FR", "ES"),
    fallback = "EN",
    warn = TRUE
) {
  if (length(lang) == 0L || is.null(lang) || is.na(lang[1L])) {
    return(fallback)
  }
  
  lang <- toupper(trimws(as.character(lang[1L])))
  
  if (!lang %in% supported) {
    if (isTRUE(warn)) {
      warning(
        "Unsupported translation language '", lang,
        "'. Using '", fallback, "'.",
        call. = FALSE
      )
    }
    return(fallback)
  }
  
  lang
}

.normalise_translation_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub("[[:space:]]+", " ", x)
  tolower(x)
}

repo <- "https://raw.githubusercontent.com/CEA-dataportal/trust-index/main"

repo_path <- function(repo, ...) {
  parts <- c(...)
  if (grepl("^https?://", repo)) {
    paste0(sub("/+$", "", repo), "/", paste(parts, collapse = "/"))
  } else {
    file.path(repo, ...)
  }
}

# ------------------------------------------------------------
# Load and validate dictionary
# ------------------------------------------------------------

load_translation_dictionary <- function(
    file = repo_path(repo, "R", "base", "translation_v2.csv"),
    languages = c("EN", "FR", "ES"),
    fallback_language = "EN"
) {
  if (length(file) != 1L || is.na(file) || !nzchar(trimws(as.character(file)))) {
    stop("A valid translation CSV path or URL must be provided.", call. = FALSE)
  }
  
  file <- as.character(file)
  is_url <- grepl("^https?://", file)
  
  if (!is_url && !file.exists(file)) {
    stop(
      "Translation file not found: ",
      normalizePath(file, mustWork = FALSE),
      call. = FALSE
    )
  }
  
  dictionary <- tryCatch(
    {
      if (requireNamespace("readr", quietly = TRUE)) {
        readr::read_csv(
          file,
          locale = readr::locale(encoding = "UTF-8"),
          show_col_types = FALSE,
          na = c("", "NA"),
          trim_ws = TRUE
        )
      } else {
        if (is_url) {
          con <- url(file, open = "r", encoding = "UTF-8")
          on.exit(close(con), add = TRUE)
          utils::read.csv(
            con,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            na.strings = c("", "NA")
          )
        } else {
          utils::read.csv(
            file,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            fileEncoding = "UTF-8",
            na.strings = c("", "NA")
          )
        }
      }
    },
    error = function(e) {
      stop(
        "Could not load translation dictionary from:\n",
        file,
        "\n\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  
  dictionary[] <- lapply(dictionary, function(x) {
    if (is.character(x)) x <- enc2utf8(x)
    x
  })
  
  required_columns <- c("type", "key", languages)
  missing_columns <- setdiff(required_columns, names(dictionary))
  
  if (length(missing_columns) > 0L) {
    stop(
      "The translation dictionary is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  
  dictionary$type <- tolower(trimws(as.character(dictionary$type)))
  dictionary$key <- trimws(as.character(dictionary$key))
  
  allowed_types <- c("template", "variable", "answer")
  invalid_types <- unique(dictionary$type[!dictionary$type %in% allowed_types])
  
  if (length(invalid_types) > 0L) {
    stop(
      "Unsupported translation type(s): ",
      paste(invalid_types, collapse = ", "),
      ". Expected: template, variable, answer.",
      call. = FALSE
    )
  }
  
  if (any(is.na(dictionary$key) | !nzchar(dictionary$key))) {
    stop("The translation dictionary contains blank key(s).", call. = FALSE)
  }
  
  duplicated_pairs <- paste(dictionary$type, dictionary$key, sep = "::")
  if (anyDuplicated(duplicated_pairs)) {
    dup <- unique(duplicated_pairs[duplicated(duplicated_pairs)])
    stop(
      "Duplicated type/key pair(s): ",
      paste(dup, collapse = ", "),
      call. = FALSE
    )
  }
  
  for (lang in languages) {
    values <- as.character(dictionary[[lang]])
    invalid <- !is.na(values) & !validUTF8(values)
    if (any(invalid)) {
      stop(
        "Invalid UTF-8 text found in language '", lang, "'.",
        call. = FALSE
      )
    }
  }
  
  fallback_language <- .normalise_language(
    fallback_language,
    supported = languages,
    fallback = languages[1L],
    warn = FALSE
  )
  
  attr(dictionary, "languages") <- languages
  attr(dictionary, "fallback_language") <- fallback_language
  attr(dictionary, "translation_file") <- if (is_url) {
    file
  } else {
    normalizePath(file, mustWork = FALSE)
  }
  
  dictionary
}

# ------------------------------------------------------------
# Default dictionary and language
# ------------------------------------------------------------

if (
  !exists("translation_file", inherits = TRUE) ||
  is.null(translation_file) ||
  length(translation_file) == 0L ||
  is.na(translation_file[1L]) ||
  !nzchar(trimws(as.character(translation_file[1L])))
) {
  translation_file <- repo_path(
    repo,
    "R",
    "base",
    "translation_v2.csv"
  )
}

translation_dictionary <- load_translation_dictionary(
  file = translation_file,
  languages = c("EN", "FR", "ES"),
  fallback_language = "EN"
)

if (
  !exists("translation", inherits = TRUE) ||
  is.null(translation) ||
  length(translation) == 0L ||
  is.na(translation[1L]) ||
  !nzchar(trimws(as.character(translation[1L])))
) {
  translation <- "EN"
}

translation <- .normalise_language(
  translation,
  supported = attr(translation_dictionary, "languages"),
  fallback = attr(translation_dictionary, "fallback_language")
)

# ------------------------------------------------------------
# Core lookup helpers
# ------------------------------------------------------------

.translate_key <- function(
    key,
    type,
    lang = translation,
    dictionary = translation_dictionary,
    fallback_language = attr(dictionary, "fallback_language"),
    warn_missing = TRUE
) {
  languages <- attr(dictionary, "languages")
  if (is.null(languages)) {
    languages <- intersect(c("EN", "FR", "ES"), names(dictionary))
  }
  
  lang <- .normalise_language(
    lang,
    supported = languages,
    fallback = fallback_language,
    warn = warn_missing
  )
  
  idx <- which(
    dictionary$type == type &
      dictionary$key == key
  )
  
  if (length(idx) == 0L) {
    if (isTRUE(warn_missing)) {
      warning(
        "Missing ", type, " translation key: ", key,
        call. = FALSE
      )
    }
    return(NULL)
  }
  
  idx <- idx[1L]
  value <- dictionary[[lang]][idx]
  
  if (.is_blank_translation(value)) {
    value <- dictionary[[fallback_language]][idx]
  }
  
  if (.is_blank_translation(value)) {
    return(NULL)
  }
  
  enc2utf8(as.character(value[1L]))
}

.translate_value <- function(
    x,
    type,
    lang = translation,
    dictionary = translation_dictionary,
    fallback_original = TRUE
) {
  if (length(x) == 0L) {
    return(character(0))
  }
  
  languages <- attr(dictionary, "languages")
  if (is.null(languages)) {
    languages <- intersect(c("EN", "FR", "ES"), names(dictionary))
  }
  
  target_lang <- .normalise_language(
    lang,
    supported = languages,
    fallback = attr(dictionary, "fallback_language"),
    warn = FALSE
  )
  
  pool <- dictionary[dictionary$type == type, , drop = FALSE]
  
  out <- vapply(
    seq_along(x),
    function(i) {
      value <- x[[i]]
      
      if (is.na(value)) {
        return(NA_character_)
      }
      
      value_chr <- enc2utf8(as.character(value))
      value_norm <- .normalise_translation_text(value_chr)
      
      matches <- integer(0)
      
      for (source_lang in languages) {
        source_values <- pool[[source_lang]]
        source_norm <- .normalise_translation_text(source_values)
        matches <- union(
          matches,
          which(!is.na(source_values) & source_norm == value_norm)
        )
      }
      
      if (length(matches) == 0L) {
        return(if (isTRUE(fallback_original)) value_chr else NA_character_)
      }
      
      translated <- pool[[target_lang]][matches[1L]]
      
      if (.is_blank_translation(translated)) {
        translated <- pool[[attr(dictionary, "fallback_language")]][matches[1L]]
      }
      
      if (.is_blank_translation(translated)) {
        return(if (isTRUE(fallback_original)) value_chr else NA_character_)
      }
      
      enc2utf8(as.character(translated[1L]))
    },
    character(1)
  )
  
  if (is.factor(x)) {
    return(out)
  }
  
  out
}

# ------------------------------------------------------------
# 1. TEMPLATE
# ------------------------------------------------------------

tr <- function(
    key,
    ...,
    lang = translation,
    dictionary = translation_dictionary,
    warn_missing = TRUE
) {
  if (length(key) != 1L || is.na(key) || !nzchar(trimws(as.character(key)))) {
    stop("tr() requires one non-empty template key.", call. = FALSE)
  }
  
  key <- trimws(as.character(key))
  
  translated_text <- .translate_key(
    key = key,
    type = "template",
    lang = lang,
    dictionary = dictionary,
    warn_missing = warn_missing
  )
  
  if (is.null(translated_text)) {
    return(key)
  }
  
  arguments <- list(...)
  
  if (length(arguments) > 0L) {
    translated_text <- tryCatch(
      do.call(sprintf, c(list(fmt = translated_text), arguments)),
      error = function(e) {
        stop(
          "Could not format translation key '", key, "': ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  
  translated_text
}

# ------------------------------------------------------------
# 2. VARIABLES / QUESTION CODE / DIMENSIONS
# ------------------------------------------------------------

tr_variable <- function(
    variable,
    label = NULL,
    fallback = NULL,
    lang = translation,
    dictionary = translation_dictionary,
    warn_missing = FALSE
) {
  if (length(variable) == 0L) {
    return(character(0))
  }
  
  if (!is.null(label)) {
    if (length(label) != 1L || !label %in% c("short", "long")) {
      stop("label must be NULL, 'short' or 'long'.", call. = FALSE)
    }
    
    out <- vapply(
      seq_along(variable),
      function(i) {
        var <- as.character(variable[[i]])
        
        if (is.na(var) || !nzchar(trimws(var))) {
          return(NA_character_)
        }
        
        key <- paste0(var, ".", label)
        
        translated <- .translate_key(
          key = key,
          type = "variable",
          lang = lang,
          dictionary = dictionary,
          warn_missing = warn_missing
        )
        
        if (!is.null(translated)) {
          return(translated)
        }
        
        if (!is.null(fallback)) {
          fb <- if (length(fallback) == 1L) fallback[[1L]] else fallback[[i]]
          if (!is.na(fb)) return(as.character(fb))
        }
        
        var
      },
      character(1)
    )
    
    return(out)
  }
  
  # No label supplied:
  # 1) try variable key (dimension.competencies, etc.)
  # 2) otherwise translate by matching the visible value in EN/FR/ES.
  vapply(
    seq_along(variable),
    function(i) {
      value <- variable[[i]]
      
      if (is.na(value)) {
        return(NA_character_)
      }
      
      value_chr <- as.character(value)
      
      direct <- .translate_key(
        key = value_chr,
        type = "variable",
        lang = lang,
        dictionary = dictionary,
        warn_missing = FALSE
      )
      
      if (!is.null(direct)) {
        return(direct)
      }
      
      .translate_value(
        value_chr,
        type = "variable",
        lang = lang,
        dictionary = dictionary,
        fallback_original = TRUE
      )[[1L]]
    },
    character(1)
  )
}

# ------------------------------------------------------------
# 3. ANSWERS / DATA VALUES
# ------------------------------------------------------------

tr_answer <- function(
    x,
    lang = translation,
    dictionary = translation_dictionary
) {
  .translate_value(
    x,
    type = "answer",
    lang = lang,
    dictionary = dictionary,
    fallback_original = TRUE
  )
}

# Backward-compatible alias for chart code already using tr_data()
tr_data <- tr_answer

# ggplot convenience helper
tr_data_labels <- function(
    lang = translation,
    dictionary = translation_dictionary
) {
  function(x) {
    tr_answer(
      x,
      lang = lang,
      dictionary = dictionary
    )
  }
}


# for group label (profile)

tr_group_label <- function(
    x,
    lang = translation,
    dictionary = translation_dictionary
) {
  
  vapply(
    x,
    function(label) {
      
      if (is.na(label)) {
        return(NA_character_)
      }
      
      label <- trimws(as.character(label))
      
      # Special generic group
      if (tolower(label) == "others") {
        return(
          tr(
            "common.others",
            lang = lang,
            dictionary = dictionary
          )
        )
      }
      
      # Group built as "Variable: Answer"
      if (grepl(":", label, fixed = TRUE)) {
        
        parts <- strsplit(
          label,
          ":",
          fixed = TRUE
        )[[1]]
        
        variable_part <- trimws(parts[1])
        
        answer_part <- trimws(
          paste(
            parts[-1],
            collapse = ":"
          )
        )
        
        variable_translated <- tr_variable(
          variable_part,
          lang = lang,
          dictionary = dictionary
        )
        
        answer_translated <- tr_data(
          answer_part,
          lang = lang,
          dictionary = dictionary
        )
        
        return(
          paste0(
            variable_translated,
            ": ",
            answer_translated
          )
        )
      }
      
      # If there is no ":" try variable first
      variable_translated <- tr_variable(
        label,
        lang = lang,
        dictionary = dictionary
      )
      
      if (!identical(variable_translated, label)) {
        return(variable_translated)
      }
      
      # Last fallback: try as data value
      tr_data(
        label,
        lang = lang,
        dictionary = dictionary
      )
    },
    character(1)
  )
}

# ------------------------------------------------------------
# 5. LONG TRANSLATED TEXT
# ------------------------------------------------------------

include_text <- function(
    name,
    lang = translation,
    repo = repo
) {
  
  relative_path <- file.path(
    "R",
    "text",
    paste0(name, "_", lang, ".md")
  )
  
  # Local repository
  if (!grepl("^https?://", repo)) {
    
    file <- file.path(
      repo,
      relative_path
    )
    
    if (!file.exists(file)) {
      stop(
        "Translated text file not found: ",
        file,
        call. = FALSE
      )
    }
    
    lines <- readLines(
      file,
      encoding = "UTF-8",
      warn = FALSE
    )
    
  } else {
    
    # GitHub / remote repository
    file <- repo_path(
      repo,
      "R",
      "text",
      paste0(name, "_", lang, ".md")
    )
    
    con <- url(
      file,
      open = "r",
      encoding = "UTF-8"
    )
    
    on.exit(
      close(con),
      add = TRUE
    )
    
    lines <- readLines(
      con,
      encoding = "UTF-8",
      warn = FALSE
    )
  }
  
  cat(
    lines,
    sep = "\n"
  )
  
  invisible(NULL)
}

# ------------------------------------------------------------
# Convenience helpers
# ------------------------------------------------------------

translation_keys <- function(
    type = NULL,
    dictionary = translation_dictionary
) {
  if (is.null(type)) {
    return(dictionary$key)
  }
  
  type <- tolower(type)
  
  if (!type %in% c("template", "variable", "answer")) {
    stop("type must be template, variable or answer.", call. = FALSE)
  }
  
  dictionary$key[dictionary$type == type]
}

missing_translations <- function(
    lang = translation,
    type = NULL,
    dictionary = translation_dictionary
) {
  lang <- .normalise_language(
    lang,
    supported = attr(dictionary, "languages"),
    fallback = attr(dictionary, "fallback_language"),
    warn = FALSE
  )
  
  subset <- dictionary
  
  if (!is.null(type)) {
    subset <- subset[subset$type == tolower(type), , drop = FALSE]
  }
  
  missing <- vapply(
    subset[[lang]],
    .is_blank_translation,
    logical(1)
  )
  
  subset[missing, c("type", "key", lang), drop = FALSE]
}

validate_translation_dictionary <- function(
    dictionary = translation_dictionary,
    stop_on_error = FALSE
) {
  issues <- character(0)
  
  allowed_types <- c("template", "variable", "answer")
  
  if (any(!dictionary$type %in% allowed_types)) {
    issues <- c(issues, "Invalid translation types")
  }
  
  pair <- paste(dictionary$type, dictionary$key, sep = "::")
  
  if (anyDuplicated(pair)) {
    issues <- c(issues, "Duplicated type/key pairs")
  }
  
  if (any(is.na(dictionary$key) | !nzchar(trimws(dictionary$key)))) {
    issues <- c(issues, "Blank translation keys")
  }
  
  languages <- attr(dictionary, "languages")
  
  for (lang in languages) {
    missing_count <- sum(
      vapply(dictionary[[lang]], .is_blank_translation, logical(1))
    )
    
    if (missing_count > 0L) {
      issues <- c(
        issues,
        paste0(missing_count, " missing ", lang, " translation(s)")
      )
    }
  }
  
  result <- list(
    valid = length(issues) == 0L,
    issues = issues,
    languages = languages,
    rows = nrow(dictionary),
    counts = table(dictionary$type),
    file = attr(dictionary, "translation_file")
  )
  
  if (isTRUE(stop_on_error) && !result$valid) {
    stop(paste(result$issues, collapse = "; "), call. = FALSE)
  }
  
  result
}

set_translation_language <- function(lang) {
  lang <- .normalise_language(
    lang,
    supported = attr(translation_dictionary, "languages"),
    fallback = attr(translation_dictionary, "fallback_language")
  )
  
  assign("translation", lang, envir = .GlobalEnv)
  invisible(lang)
}

message(
  "✓ Translation dictionary loaded: ",
  nrow(translation_dictionary),
  " keys | template: ",
  sum(translation_dictionary$type == "template"),
  " | variable: ",
  sum(translation_dictionary$type == "variable"),
  " | answer: ",
  sum(translation_dictionary$type == "answer"),
  " | language: ",
  translation
)
