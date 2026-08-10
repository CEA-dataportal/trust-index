##########################################################
# COMMUNITY TRUST INDEX - TRANSLATIONS
##########################################################

# This script loads translation.csv and provides helpers for
# multilingual chart, table, caption and export labels.
#
# Expected CSV columns:
#   key, section, EN, FR, ES
# Optional metadata columns are allowed and ignored by the helpers.

# ------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------

.is_blank_translation <- function(x) {
  length(x) == 0L ||
    is.null(x) ||
    is.na(x[1L]) ||
    !nzchar(trimws(as.character(x[1L])))
}

.normalise_language <- function(lang, supported = c("EN", "FR", "ES"),
                                fallback = "EN", warn = TRUE) {
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

# ------------------------------------------------------------
# Load and validate dictionary
# ------------------------------------------------------------

load_translation_dictionary <- function(
  file = file.path("R", "translation.csv"),
  languages = c("EN", "FR", "ES"),
  fallback_language = "EN"
) {
  if (length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("A valid translation CSV path must be provided.", call. = FALSE)
  }

  if (!file.exists(file)) {
    stop(
      "Translation file not found: ", normalizePath(file, mustWork = FALSE),
      call. = FALSE
    )
  }

  dictionary <- if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(
      file,
      show_col_types = FALSE,
      na = c("", "NA")
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

  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)

  required_columns <- c("key", "section", languages)
  missing_columns <- setdiff(required_columns, names(dictionary))

  if (length(missing_columns) > 0L) {
    stop(
      "The translation dictionary is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  dictionary$key <- trimws(as.character(dictionary$key))
  dictionary$section <- trimws(as.character(dictionary$section))

  blank_keys <- is.na(dictionary$key) | !nzchar(dictionary$key)
  if (any(blank_keys)) {
    stop(
      "The translation dictionary contains ", sum(blank_keys),
      " blank translation key(s).",
      call. = FALSE
    )
  }

  duplicated_keys <- unique(dictionary$key[duplicated(dictionary$key)])
  if (length(duplicated_keys) > 0L) {
    stop(
      "Duplicated translation key(s): ",
      paste(duplicated_keys, collapse = ", "),
      call. = FALSE
    )
  }

  fallback_language <- .normalise_language(
    fallback_language,
    supported = languages,
    fallback = languages[1L],
    warn = FALSE
  )

  if (any(vapply(dictionary[[fallback_language]], .is_blank_translation, logical(1)))) {
    missing_fallback <- dictionary$key[
      vapply(dictionary[[fallback_language]], .is_blank_translation, logical(1))
    ]

    warning(
      "Missing ", fallback_language, " fallback translation(s) for: ",
      paste(missing_fallback, collapse = ", "),
      call. = FALSE
    )
  }

  attr(dictionary, "languages") <- languages
  attr(dictionary, "fallback_language") <- fallback_language
  attr(dictionary, "translation_file") <- normalizePath(file, mustWork = FALSE)

  dictionary
}

# ------------------------------------------------------------
# Default dictionary and language
# ------------------------------------------------------------

# Override translation_file before sourcing this script when needed.
if (!exists("translation_file", inherits = TRUE) ||
    is.null(translation_file) ||
    is.na(translation_file) ||
    !nzchar(as.character(translation_file))) {
  translation_file <- file.path("R", "translation.csv")
}

translation_dictionary <- load_translation_dictionary(
  file = translation_file,
  languages = c("EN", "FR", "ES"),
  fallback_language = "EN"
)

# The configuration file can define translation as EN, FR or ES.
if (!exists("translation", inherits = TRUE) ||
    is.null(translation) ||
    length(translation) == 0L ||
    is.na(translation[1L]) ||
    !nzchar(trimws(as.character(translation[1L])))) {
  translation <- "EN"
}

translation <- .normalise_language(
  translation,
  supported = attr(translation_dictionary, "languages"),
  fallback = attr(translation_dictionary, "fallback_language")
)

# ------------------------------------------------------------
# Main translation helper
# ------------------------------------------------------------

tr <- function(
  key,
  ...,
  lang = translation,
  dictionary = translation_dictionary,
  fallback_language = attr(dictionary, "fallback_language"),
  warn_missing = TRUE
) {
  if (length(key) != 1L || is.na(key) || !nzchar(trimws(as.character(key)))) {
    stop("tr() requires one non-empty translation key.", call. = FALSE)
  }

  key <- trimws(as.character(key))
  languages <- attr(dictionary, "languages")

  if (is.null(languages)) {
    languages <- intersect(c("EN", "FR", "ES"), names(dictionary))
  }

  fallback_language <- .normalise_language(
    fallback_language,
    supported = languages,
    fallback = languages[1L],
    warn = FALSE
  )

  lang <- .normalise_language(
    lang,
    supported = languages,
    fallback = fallback_language,
    warn = warn_missing
  )

  row_index <- match(key, dictionary$key)

  if (is.na(row_index)) {
    if (isTRUE(warn_missing)) {
      warning("Missing translation key: ", key, call. = FALSE)
    }
    return(key)
  }

  translated_text <- dictionary[[lang]][row_index]

  # Fall back to English (or configured fallback language).
  if (.is_blank_translation(translated_text)) {
    translated_text <- dictionary[[fallback_language]][row_index]

    if (isTRUE(warn_missing) && lang != fallback_language) {
      warning(
        "Missing '", lang, "' translation for key '", key,
        "'. Using '", fallback_language, "'.",
        call. = FALSE
      )
    }
  }

  # Last fallback: return the key itself.
  if (.is_blank_translation(translated_text)) {
    if (isTRUE(warn_missing)) {
      warning("No usable translation found for key: ", key, call. = FALSE)
    }
    return(key)
  }

  translated_text <- as.character(translated_text[1L])
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
# Vector and lookup helpers
# ------------------------------------------------------------

tr_vec <- function(
  keys,
  lang = translation,
  dictionary = translation_dictionary,
  warn_missing = TRUE,
  use_names = TRUE
) {
  values <- vapply(
    keys,
    function(key) {
      tr(
        key,
        lang = lang,
        dictionary = dictionary,
        warn_missing = warn_missing
      )
    },
    character(1)
  )

  if (isTRUE(use_names)) {
    names(values) <- keys
  }

  values
}

translation_exists <- function(
  key,
  lang = translation,
  dictionary = translation_dictionary
) {
  lang <- .normalise_language(
    lang,
    supported = attr(dictionary, "languages"),
    fallback = attr(dictionary, "fallback_language"),
    warn = FALSE
  )

  row_index <- match(key, dictionary$key)

  !is.na(row_index) && !.is_blank_translation(dictionary[[lang]][row_index])
}

translation_keys <- function(
  section = NULL,
  dictionary = translation_dictionary
) {
  if (is.null(section)) {
    return(dictionary$key)
  }

  dictionary$key[dictionary$section %in% section]
}

missing_translations <- function(
  lang = translation,
  dictionary = translation_dictionary
) {
  lang <- .normalise_language(
    lang,
    supported = attr(dictionary, "languages"),
    fallback = attr(dictionary, "fallback_language"),
    warn = FALSE
  )

  missing <- vapply(dictionary[[lang]], .is_blank_translation, logical(1))

  dictionary[missing, c("key", "section", lang), drop = FALSE]
}

validate_translation_dictionary <- function(
  dictionary = translation_dictionary,
  stop_on_error = FALSE
) {
  languages <- attr(dictionary, "languages")
  if (is.null(languages)) {
    languages <- intersect(c("EN", "FR", "ES"), names(dictionary))
  }

  issues <- character(0)

  if (anyDuplicated(dictionary$key)) {
    issues <- c(issues, "Duplicated translation keys")
  }

  if (any(is.na(dictionary$key) | !nzchar(trimws(dictionary$key)))) {
    issues <- c(issues, "Blank translation keys")
  }

  for (lang in languages) {
    missing_count <- sum(vapply(dictionary[[lang]], .is_blank_translation, logical(1)))

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
    file = attr(dictionary, "translation_file")
  )

  if (isTRUE(stop_on_error) && !result$valid) {
    stop(paste(result$issues, collapse = "; "), call. = FALSE)
  }

  result
}

# ------------------------------------------------------------
# Optional language setter
# ------------------------------------------------------------

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
  " keys | language: ",
  translation
)
