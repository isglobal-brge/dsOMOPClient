# Module: Recipe Builder
# Functions for building extraction recipes with concept blocks, feature specs,
# population filters, and output configuration.

# --- Naming engine ---

#' Sanitize a concept name into a valid R variable name
#'
#' Converts a human-readable concept name into a safe R variable name by
#' lowercasing, replacing non-alphanumeric characters with underscores, and
#' truncating to 50 characters.
#'
#' @param x Character; the name to sanitize.
#' @return Character; a valid R variable name.
#' @keywords internal
.sanitize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  if (nchar(x) == 0) return("var")
  if (grepl("^[0-9]", x)) x <- paste0("v_", x)
  if (nchar(x) > 50) x <- substr(x, 1, 50)
  x
}

# Reserved R names to avoid
.r_reserved <- c("if", "else", "repeat", "while", "function", "for", "in",
                  "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
                  "NA", "NA_integer_", "NA_real_", "NA_complex_",
                  "NA_character_", "T", "F")

#' Ensure a variable name is unique within existing names
#'
#' Appends numeric suffixes (\_2, \_3, etc.) to resolve collisions with
#' existing names. Also escapes R reserved words by appending "\_var".
#'
#' @param name Character; proposed name.
#' @param existing Character vector; existing names to check against.
#' @return Character; a unique name (with \_2, \_3 suffixes if needed).
#' @keywords internal
.ensure_unique_name <- function(name, existing) {
  if (name %in% .r_reserved) name <- paste0(name, "_var")
  if (!name %in% existing) return(name)
  i <- 2L
  while (paste0(name, "_", i) %in% existing) i <- i + 1L
  paste0(name, "_", i)
}

#' Generate suffixed names for multi-column variables
#'
#' Produces a vector of column names from a base name by appending index,
#' range, or label suffixes. Used when a single variable expands into
#' multiple output columns.
#'
#' @param base_name Character; base variable name.
#' @param n Integer; number of columns to generate.
#' @param mode Character; suffix mode ("index", "range", or "label").
#' @param labels Character vector; labels for "label" mode (optional).
#' @param ranges Numeric matrix; start/end for "range" mode (optional).
#' @return Character vector of suffixed names.
#' @keywords internal
.suffix_names <- function(base_name, n,
                          mode = c("index", "range", "label"),
                          labels = NULL, ranges = NULL) {
  mode <- match.arg(mode)
  if (n <= 1) return(base_name)
  switch(mode,
    index = paste0(base_name, "_", seq_len(n)),
    range = {
      if (is.null(ranges) || nrow(ranges) < n) {
        paste0(base_name, "_", seq_len(n))
      } else {
        paste0(base_name, "_d", ranges[, 1], "_", ranges[, 2])
      }
    },
    label = {
      if (is.null(labels) || length(labels) < n) {
        paste0(base_name, "_", seq_len(n))
      } else {
        paste0(base_name, "_", vapply(labels[seq_len(n)], .sanitize_name,
                                       character(1)))
      }
    }
  )
}

# --- omop_variable: What to extract ---

#' Create a variable specification
#'
#' Describes a single variable to extract from the CDM. Variables reference
#' a source table and column, and may include concept-level filtering and
#' output formatting options. Variables are the atomic units added to an
#' \code{omop_recipe} either individually via \code{\link{recipe_add_variable}}
#' or in bulk via \code{\link{recipe_add_block}}.
#'
#' @param name Character; output column name (auto-generated from
#'   \code{concept_name}, \code{concept_id}, or \code{column} if \code{NULL}).
#' @param table Character; source OMOP CDM table (e.g. \code{"condition_occurrence"}).
#' @param column Character or \code{NULL}; source column to extract.
#' @param concept_id Integer or \code{NULL}; concept ID filter (for concept columns).
#' @param concept_name Character or \code{NULL}; human-readable concept name.
#' @param type Character; variable type hint. One of \code{"auto"}, \code{"numeric"},
#'   \code{"categorical"}, \code{"date"}, \code{"boolean"}, \code{"integer"},
#'   \code{"character"}.
#' @param format Character; output format. One of \code{"raw"}, \code{"binary"},
#'   \code{"count"}, \code{"first_value"}, \code{"last_value"}, \code{"mean"},
#'   \code{"min"}, \code{"max"}, \code{"time_since"}, \code{"binned"}.
#' @param value_source Character or \code{NULL}; column to extract value from
#'   (e.g. \code{"value_as_number"} for measurements).
#' @param time_window Named list with \code{start}/\code{end} offsets relative
#'   to index date, or \code{NULL} for no window constraint.
#' @param suffix_mode Character; how to name multi-column expansions
#'   (\code{"index"}, \code{"range"}, or \code{"label"}).
#' @param filters List of \code{\link{omop_filter}} objects to apply to this variable.
#' @return An \code{omop_variable} object (a named list with class
#'   \code{"omop_variable"}).
#' @examples
#' \dontrun{
#' v <- omop_variable(
#'   table = "condition_occurrence",
#'   concept_id = 201820,
#'   concept_name = "Type 2 diabetes",
#'   format = "binary"
#' )
#' }
#' @seealso \code{\link{recipe_add_variable}}, \code{\link{omop_variable_block}}
#' @export
omop_variable <- function(name = NULL,
                          table,
                          column = NULL,
                          concept_id = NULL,
                          concept_name = NULL,
                          type = c("auto", "numeric", "categorical",
                                   "date", "boolean", "integer",
                                   "character"),
                          format = c("raw", "binary", "count",
                                     "first_value", "last_value",
                                     "mean", "min", "max", "time_since",
                                     "binned", "age", "sex_mf",
                                     "obs_duration", "drug_duration",
                                     "sum", "n_distinct"),
                          value_source = NULL,
                          time_window = NULL,
                          suffix_mode = c("index", "range", "label"),
                          filters = list()) {
  type <- match.arg(type)
  format <- match.arg(format)
  suffix_mode <- match.arg(suffix_mode)

  # Auto-generate name from concept or column
  if (is.null(name)) {
    if (!is.null(concept_name)) {
      name <- .sanitize_name(concept_name)
    } else if (!is.null(concept_id)) {
      name <- paste0(table, "_c", concept_id)
    } else if (!is.null(column)) {
      name <- paste0(table, "_", column)
    } else {
      name <- paste0(table, "_var")
    }
  }

  obj <- list(
    name         = name,
    table        = table,
    column       = column,
    concept_id   = if (!is.null(concept_id)) as.integer(concept_id) else NULL,
    concept_name = concept_name,
    type         = type,
    format       = format,
    value_source = value_source,
    time_window  = time_window,
    suffix_mode  = suffix_mode,
    filters      = filters
  )
  class(obj) <- c("omop_variable", "list")
  obj
}

#' Print an omop_variable
#'
#' @param x An \code{omop_variable} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_variable
print.omop_variable <- function(x, ...) {
  cat("omop_variable:", x$name, "\n")
  cat("  Table:   ", x$table, "\n")
  if (!is.null(x$concept_id))
    cat("  Concept: ", x$concept_id,
        if (!is.null(x$concept_name)) paste0(" (", x$concept_name, ")") else "",
        "\n")
  if (!is.null(x$column))
    cat("  Column:  ", x$column, "\n")
  cat("  Format:  ", x$format, "\n")
  if (!is.null(x$time_window))
    cat("  Window:  ", x$time_window$start, "to", x$time_window$end, "days\n")
  if (length(x$filters) > 0)
    cat("  Filters: ", length(x$filters), "\n")
  invisible(x)
}

# --- Convenience derived variable constructors ---

#' Create an age variable
#'
#' Produces a derived variable that computes age from \code{year_of_birth}.
#' With \code{reference = "today"}, age is \code{current_year - year_of_birth}.
#' With \code{reference = "index"}, age is computed relative to the cohort
#' start date.
#'
#' @param name Character; output column name (default \code{"age"}).
#' @param reference Character; \code{"today"} or \code{"index"}.
#' @param reference_date Date or \code{NULL}; explicit reference date
#'   (overrides \code{reference}).
#' @return An \code{omop_variable} object with \code{format = "age"} and
#'   a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe, omop_variable_age())
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop_variable_sex}}
#' @export
omop_variable_age <- function(name = "age",
                               reference = c("today", "index"),
                               reference_date = NULL) {
  reference <- match.arg(reference)
  v <- omop_variable(
    name = name, table = "person", format = "age"
  )
  v$derived <- list(
    kind = "age",
    reference = reference,
    reference_date = reference_date
  )
  v
}

#' Create a sex (M/F) variable
#'
#' Produces a derived variable that maps \code{gender_concept_id} to
#' \code{"M"} (8507) or \code{"F"} (8532).
#'
#' @param name Character; output column name (default \code{"sex"}).
#' @return An \code{omop_variable} object with \code{format = "sex_mf"} and
#'   a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe, omop_variable_sex())
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop_variable_age}}
#' @export
omop_variable_sex <- function(name = "sex") {
  v <- omop_variable(
    name = name, table = "person", format = "sex_mf"
  )
  v$derived <- list(kind = "sex_mf")
  v
}

#' Create an observation duration variable
#'
#' Produces a derived variable that computes the number of days between
#' \code{observation_period_start_date} and
#' \code{observation_period_end_date}.
#'
#' @param name Character; output column name (default \code{"obs_duration"}).
#' @return An \code{omop_variable} object with \code{format = "obs_duration"}
#'   and a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe, omop_variable_obs_duration())
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop_variable_age}}
#' @export
omop_variable_obs_duration <- function(name = "obs_duration") {
  v <- omop_variable(
    name = name, table = "observation_period", format = "obs_duration"
  )
  v$derived <- list(kind = "obs_duration")
  v
}

#' Create a drug duration variable
#'
#' Produces a feature variable that computes the duration of drug exposures
#' (\code{drug_exposure_end_date - drug_exposure_start_date}) and aggregates
#' per person using the specified function.
#'
#' @param concept_id Integer; drug concept ID.
#' @param concept_name Character or \code{NULL}; human-readable name.
#' @param name Character or \code{NULL}; output column name (auto-generated
#'   if \code{NULL}).
#' @param agg Character; aggregation function — \code{"mean"}, \code{"sum"},
#'   or \code{"max"}.
#' @return An \code{omop_variable} object with \code{format = "drug_duration"}
#'   and a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe,
#'   omop_variable_drug_duration(1124300, concept_name = "Metformin"))
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop.feature.drug_duration}}
#' @export
omop_variable_drug_duration <- function(concept_id,
                                         concept_name = NULL,
                                         name = NULL,
                                         agg = c("mean", "sum", "max")) {
  agg <- match.arg(agg)
  if (is.null(name)) {
    base <- if (!is.null(concept_name)) .sanitize_name(concept_name)
            else paste0("drug_c", concept_id)
    name <- paste0(base, "_duration_", agg)
  }
  v <- omop_variable(
    name = name, table = "drug_exposure",
    concept_id = concept_id, concept_name = concept_name,
    format = "drug_duration"
  )
  v$derived <- list(kind = "drug_duration", agg = agg)
  v
}

#' Create a sum variable
#'
#' Produces a feature variable that sums a numeric column per person for
#' records matching the concept set.
#'
#' @param table Character; source OMOP CDM table.
#' @param column Character; numeric column to sum (e.g.
#'   \code{"days_supply"}, \code{"quantity"}).
#' @param concept_id Integer or \code{NULL}; concept ID filter.
#' @param concept_name Character or \code{NULL}; human-readable name.
#' @param name Character or \code{NULL}; output column name (auto-generated
#'   if \code{NULL}).
#' @return An \code{omop_variable} object with \code{format = "sum"} and
#'   a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe,
#'   omop_variable_sum("drug_exposure", "days_supply",
#'                     concept_id = 1124300))
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop.feature.sum_value}}
#' @export
omop_variable_sum <- function(table, column,
                               concept_id = NULL,
                               concept_name = NULL,
                               name = NULL) {
  if (is.null(name)) {
    base <- if (!is.null(concept_name)) .sanitize_name(concept_name)
            else if (!is.null(concept_id)) paste0(table, "_c", concept_id)
            else table
    name <- paste0(base, "_sum_", column)
  }
  v <- omop_variable(
    name = name, table = table,
    concept_id = concept_id, concept_name = concept_name,
    format = "sum", value_source = column
  )
  v$derived <- list(kind = "sum", column = column)
  v
}

#' Create a distinct-concept-count variable
#'
#' Produces a feature variable that counts the number of distinct concept
#' IDs per person in the specified table.
#'
#' @param table Character; source OMOP CDM table (e.g.
#'   \code{"condition_occurrence"}).
#' @param name Character or \code{NULL}; output column name (auto-generated
#'   as \code{"n_distinct_<table>"} if \code{NULL}).
#' @return An \code{omop_variable} object with \code{format = "n_distinct"}
#'   and a \code{$derived} metadata field.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe,
#'   omop_variable_n_distinct("condition_occurrence"))
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop.feature.n_distinct}}
#' @export
omop_variable_n_distinct <- function(table, name = NULL) {
  if (is.null(name)) name <- paste0("n_distinct_", table)
  v <- omop_variable(
    name = name, table = table, format = "n_distinct"
  )
  v$derived <- list(kind = "n_distinct")
  v
}

# --- omop_filter + omop_filter_group: Conditions & chaining ---

#' Create a filter specification
#'
#' Filters restrict the population or events included in the extraction.
#' There are three levels: \code{"population"} (person-level inclusion criteria),
#' \code{"row"} (event-level restrictions), and \code{"output"} (post-extraction
#' transformations). Filters are added to a recipe via
#' \code{\link{recipe_add_filter}} and can be nested into groups with
#' \code{\link{omop_filter_group}}.
#'
#' Convenience constructors are provided for common filter types:
#' \code{\link{omop_filter_sex}}, \code{\link{omop_filter_age}},
#' \code{\link{omop_filter_age_group}}, \code{\link{omop_filter_has_concept}},
#' \code{\link{omop_filter_date_range}}, \code{\link{omop_filter_value}}.
#'
#' @param type Character; filter type. One of \code{"sex"}, \code{"age_range"},
#'   \code{"age_group"}, \code{"cohort"}, \code{"has_concept"},
#'   \code{"date_range"}, \code{"value_threshold"}, \code{"concept_set"},
#'   \code{"min_count"}, \code{"top_n"}, \code{"dedup"}, \code{"custom"}.
#' @param level Character; \code{"population"}, \code{"row"}, or \code{"output"}.
#' @param params Named list; filter-specific parameters (varies by type).
#' @param label Character or \code{NULL}; human-readable description
#'   (auto-generated from type and params if \code{NULL}).
#' @return An \code{omop_filter} object (a named list with class
#'   \code{"omop_filter"}).
#' @examples
#' \dontrun{
#' f <- omop_filter(type = "sex", level = "population",
#'                  params = list(value = "F"))
#' }
#' @seealso \code{\link{recipe_add_filter}}, \code{\link{omop_filter_group}}
#' @export
omop_filter <- function(type = c("sex", "age_range", "age_group", "cohort",
                                  "has_concept", "date_range",
                                  "value_threshold", "concept_set",
                                  "min_count", "top_n", "dedup", "custom"),
                        level = c("population", "row", "output"),
                        params = list(),
                        label = NULL) {
  type <- match.arg(type)
  level <- match.arg(level)

  if (is.null(label)) {
    label <- switch(type,
      sex = paste0("Sex = ", params$value %||% "?"),
      age_range = paste0("Age ", params$min %||% "?", "-", params$max %||% "?"),
      age_group = paste0("Age groups: ",
                         paste(params$groups %||% "?", collapse = ", ")),
      cohort = paste0("Cohort #", params$cohort_definition_id %||% "?"),
      has_concept = paste0("Has concept ", params$concept_id %||% "?",
                           " in ", params$table %||% "?"),
      date_range = paste0("Dates ", params$start %||% "?",
                          " to ", params$end %||% "?"),
      value_threshold = paste0("Value ", params$op %||% "?",
                               " ", params$value %||% "?"),
      concept_set = paste0(length(params$concept_ids %||% integer(0)),
                           " concepts"),
      min_count = paste0("Min ", params$min_count %||% 1, " occurrences"),
      top_n = paste0("Top ", params$n %||% "?"),
      dedup = "Deduplicate",
      custom = params$description %||% "Custom filter"
    )
  }

  obj <- list(type = type, level = level, params = params, label = label)
  class(obj) <- c("omop_filter", "list")
  obj
}

#' Print an omop_filter
#'
#' @param x An \code{omop_filter} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_filter
print.omop_filter <- function(x, ...) {
  cat("omop_filter [", x$level, "]:", x$label, "\n")
  invisible(x)
}

#' Create an AND/OR group of filters
#'
#' Combines multiple filters (or nested groups) using a Boolean operator.
#' Groups can contain \code{\link{omop_filter}} objects or other
#' \code{omop_filter_group} objects, allowing arbitrarily nested condition
#' trees. During plan compilation, these are translated to the server's
#' filter DSL.
#'
#' @param ... \code{omop_filter} or \code{omop_filter_group} objects to combine.
#' @param operator Character; \code{"AND"} or \code{"OR"}.
#' @param label Character or \code{NULL}; human-readable description
#'   (auto-generated from children if \code{NULL}).
#' @return An \code{omop_filter_group} object.
#' @examples
#' \dontrun{
#' grp <- omop_filter_group(
#'   omop_filter_sex("F"),
#'   omop_filter_age(min = 18, max = 65),
#'   operator = "AND"
#' )
#' }
#' @seealso \code{\link{omop_filter}}, \code{\link{recipe_add_filter}}
#' @export
omop_filter_group <- function(..., operator = c("AND", "OR"), label = NULL) {
  operator <- match.arg(operator)
  children <- list(...)
  # Validate children
  for (ch in children) {
    if (!inherits(ch, "omop_filter") && !inherits(ch, "omop_filter_group"))
      stop("All arguments must be omop_filter or omop_filter_group objects",
           call. = FALSE)
  }

  if (is.null(label)) {
    child_labels <- vapply(children, function(ch) {
      if (inherits(ch, "omop_filter_group")) {
        paste0("(", ch$label, ")")
      } else {
        ch$label
      }
    }, character(1))
    label <- paste(child_labels, collapse = paste0(" ", operator, " "))
  }

  obj <- list(
    operator = operator,
    children = children,
    label    = label
  )
  class(obj) <- c("omop_filter_group", "list")
  obj
}

#' Print an omop_filter_group
#'
#' @param x An \code{omop_filter_group} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_filter_group
print.omop_filter_group <- function(x, ...) {
  cat("omop_filter_group [", x$operator, "]:", x$label, "\n")
  for (i in seq_along(x$children)) {
    cat("  ", i, ". ")
    if (inherits(x$children[[i]], "omop_filter_group")) {
      cat("GROUP [", x$children[[i]]$operator, "]:",
          x$children[[i]]$label, "\n")
    } else {
      cat("[", x$children[[i]]$level, "]:",
          x$children[[i]]$label, "\n")
    }
  }
  invisible(x)
}

# --- Convenience filter constructors ---

#' @rdname omop_filter
#' @param value Character; sex value. Accepts "F", "f", "female", "Female",
#'   "FEMALE", "M", "m", "male", "Male", "MALE" — normalized internally to
#'   "F" or "M".
#' @export
omop_filter_sex <- function(value) {
  normalized <- switch(toupper(trimws(value)),
    "F" =, "FEMALE" = "F",
    "M" =, "MALE" = "M",
    stop("Invalid sex value '", value,
         "'. Use 'F'/'female' or 'M'/'male'.", call. = FALSE)
  )
  omop_filter(
    type = "sex", level = "population",
    params = list(value = normalized),
    label = paste("Sex =", normalized)
  )
}

#' @rdname omop_filter
#' @param min Numeric; minimum age (inclusive)
#' @param max Numeric; maximum age (inclusive)
#' @export
omop_filter_age <- function(min = 0, max = 150) {
  omop_filter(
    type = "age_range", level = "population",
    params = list(min = min, max = max),
    label = paste0("Age ", min, "-", max)
  )
}

#' @rdname omop_filter
#' @param groups Character vector; age group labels (e.g. c("18-24", "25-34"))
#' @export
omop_filter_age_group <- function(groups) {
  omop_filter(
    type = "age_group", level = "population",
    params = list(groups = groups),
    label = paste0("Age groups: ", paste(groups, collapse = ", "))
  )
}

#' @rdname omop_filter
#' @param concept_id Integer; the concept to check for
#' @param table Character; which OMOP table to check
#' @param concept_name Character or NULL; human-readable name
#' @param window Named list with start/end offsets, or NULL
#' @param min_count Integer; minimum number of occurrences (default 1)
#' @export
omop_filter_has_concept <- function(concept_id, table, concept_name = NULL,
                                     window = NULL, min_count = 1L) {
  label <- paste0("Has ",
                  if (!is.null(concept_name)) concept_name
                  else paste("concept", concept_id),
                  " in ", table)
  if (min_count > 1L) label <- paste0(label, " (>=", min_count, "x)")
  omop_filter(
    type = "has_concept", level = "population",
    params = list(
      concept_id = as.integer(concept_id),
      table = table,
      concept_name = concept_name,
      window = window,
      min_count = as.integer(min_count)
    ),
    label = label
  )
}

#' @rdname omop_filter
#' @param start Character; start date (YYYY-MM-DD) or NULL
#' @param end Character; end date (YYYY-MM-DD) or NULL
#' @export
omop_filter_date_range <- function(start = NULL, end = NULL) {
  omop_filter(
    type = "date_range", level = "row",
    params = list(start = start, end = end)
  )
}

#' @rdname omop_filter
#' @param column Character; column to compare
#' @param threshold Numeric; threshold value
#' @param direction Character; "above" or "below"
#' @param safe_bins List with $breaks numeric vector from ds.omop.safe.cutpoints()
#' @export
omop_filter_value <- function(column = "value_as_number", threshold,
                               direction = c("above", "below"),
                               safe_bins = NULL) {
  direction <- match.arg(direction)
  stopifnot(!is.null(safe_bins), length(safe_bins$breaks) >= 2)
  breaks <- safe_bins$breaks
  bin_idx <- max(1L, min(
    findInterval(threshold, breaks, rightmost.closed = TRUE),
    length(breaks) - 1L
  ))
  if (direction == "above") {
    lower <- breaks[bin_idx]
    upper <- breaks[length(breaks)]
  } else {
    lower <- breaks[1]
    upper <- breaks[bin_idx + 1L]
  }
  omop_filter(
    type = "value_bin", level = "row",
    params = list(var = column, op = "value_bin",
                  value = list(lower = lower, upper = upper)),
    label = paste0(column, " ", direction, " ~", threshold,
                   " [bin: ", lower, "-", upper, ")")
  )
}

#' Client-side filter safety classification (informational only)
#'
#' Mirrors the server-side classification for UI display purposes.
#' The server performs the authoritative check.
#'
#' @param filter_type Character; filter type
#' @param params List; filter parameters (unused for now)
#' @return Character; "allowed", "constrained", or "blocked"
#' @keywords internal
.classifyFilterClient <- function(filter_type, params = list()) {
  always_allowed <- c("sex", "age_group", "cohort", "concept_set", "value_bin")
  constrained <- c("age_range", "has_concept", "date_range", "min_count")
  blocked <- c("value_threshold", "custom")

  if (filter_type %in% always_allowed) return("allowed")
  if (filter_type %in% blocked) return("blocked")
  "constrained"
}

# --- omop_population: DAG of subpopulations ---

#' Create a population node
#'
#' Populations form a directed acyclic graph (DAG): a base cohort at the root,
#' with derived subpopulations created by applying filter chains. Each recipe
#' starts with a \code{"base"} population representing all persons. Additional
#' populations can be created as children of existing ones via
#' \code{\link{recipe_add_population}}.
#'
#' @param id Character; population ID (must be unique within the recipe).
#' @param label Character; human-readable label.
#' @param parent_id Character or \code{NULL}; parent population ID (\code{NULL}
#'   for root).
#' @param filters List of \code{\link{omop_filter}} or
#'   \code{\link{omop_filter_group}} objects.
#' @param cohort_definition_id Integer or \code{NULL}; base cohort definition ID
#'   (if the population is defined by a pre-existing cohort).
#' @return An \code{omop_population} object.
#' @examples
#' \dontrun{
#' pop <- omop_population(id = "females",
#'                        label = "Female patients",
#'                        parent_id = "base",
#'                        filters = list(omop_filter_sex("F")))
#' }
#' @seealso \code{\link{recipe_add_population}}, \code{\link{omop_recipe}}
#' @export
omop_population <- function(id = "base",
                            label = "Base Population",
                            parent_id = NULL,
                            filters = list(),
                            cohort_definition_id = NULL) {
  obj <- list(
    id                   = id,
    label                = label,
    parent_id            = parent_id,
    filters              = filters,
    cohort_definition_id = if (!is.null(cohort_definition_id))
      as.integer(cohort_definition_id) else NULL
  )
  class(obj) <- c("omop_population", "list")
  obj
}

#' Print an omop_population
#'
#' @param x An \code{omop_population} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_population
print.omop_population <- function(x, ...) {
  parent_txt <- if (!is.null(x$parent_id)) paste("->", x$parent_id) else "(root)"
  cat("omop_population:", x$id, parent_txt, "-", x$label, "\n")
  if (!is.null(x$cohort_definition_id))
    cat("  Cohort ID:", x$cohort_definition_id, "\n")
  if (length(x$filters) > 0)
    cat("  Filters:", length(x$filters), "\n")
  invisible(x)
}

# --- omop_variable_block: Grouped variables ---

#' Create a variable block
#'
#' A variable block groups variables that share a source table, time window,
#' and row-level filters. When added to a recipe via
#' \code{\link{recipe_add_block}}, the block's \code{concept_ids} are expanded
#' into individual \code{\link{omop_variable}} objects that inherit the block's
#' defaults. This is the primary way the Builder UI adds concepts to a recipe.
#'
#' @param id Character or \code{NULL}; block ID (auto-generated from table and
#'   concept count if \code{NULL}).
#' @param table Character; shared source OMOP CDM table
#'   (e.g. \code{"condition_occurrence"}).
#' @param concept_ids Integer vector; concept IDs for all variables in the block.
#' @param concept_names Character vector or \code{NULL}; human-readable names
#'   matching \code{concept_ids} positionally.
#' @param time_window Named list with \code{start}/\code{end} offsets, or
#'   \code{NULL} for no window.
#' @param format Character; default output format for variables in this block
#'   (e.g. \code{"binary"}, \code{"count"}).
#' @param value_source Character or \code{NULL}; default value source column
#'   (e.g. \code{"value_as_number"}).
#' @param suffix_mode Character; naming mode for multi-column expansion
#'   (\code{"index"}, \code{"range"}, or \code{"label"}).
#' @param filters List of \code{\link{omop_filter}} objects; row-level filters
#'   applied to all variables in the block.
#' @param population_id Character; which population this block belongs to
#'   (default \code{"base"}).
#' @return An \code{omop_variable_block} object.
#' @examples
#' \dontrun{
#' block <- omop_variable_block(
#'   table = "condition_occurrence",
#'   concept_ids = c(201820, 320128),
#'   concept_names = c("Type 2 diabetes", "Essential hypertension"),
#'   format = "binary"
#' )
#' recipe <- recipe_add_block(recipe, block)
#' }
#' @seealso \code{\link{recipe_add_block}}, \code{\link{omop_variable}}
#' @export
omop_variable_block <- function(id = NULL,
                                table,
                                concept_ids = integer(0),
                                concept_names = NULL,
                                time_window = NULL,
                                format = "raw",
                                value_source = NULL,
                                suffix_mode = "index",
                                filters = list(),
                                population_id = "base") {
  if (is.null(id)) {
    id <- paste0("block_", table, "_", length(concept_ids))
  }

  obj <- list(
    id             = id,
    table          = table,
    concept_ids    = as.integer(concept_ids),
    concept_names  = concept_names,
    time_window    = time_window,
    format         = format,
    value_source   = value_source,
    suffix_mode    = suffix_mode,
    filters        = filters,
    population_id  = population_id
  )
  class(obj) <- c("omop_variable_block", "list")
  obj
}

#' Print an omop_variable_block
#'
#' @param x An \code{omop_variable_block} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_variable_block
print.omop_variable_block <- function(x, ...) {
  cat("omop_variable_block:", x$id, "\n")
  cat("  Table:     ", x$table, "\n")
  cat("  Concepts:  ", length(x$concept_ids), "\n")
  cat("  Format:    ", x$format, "\n")
  cat("  Population:", x$population_id, "\n")
  if (!is.null(x$time_window))
    cat("  Window:    ", x$time_window$start, "to", x$time_window$end, "\n")
  if (length(x$filters) > 0)
    cat("  Filters:   ", length(x$filters), "\n")
  invisible(x)
}

# --- omop_output: How to shape the result ---

#' Create an output specification
#'
#' Defines how to shape the extracted data into a result table. Each output
#' selects a subset of variables from the recipe, targets a population, and
#' specifies a layout type (e.g. wide person-level, long event-level, or
#' feature matrix). Outputs are added to a recipe via
#' \code{\link{recipe_add_output}} and determine the server-side plan structure
#' produced by \code{\link{recipe_to_plan}}.
#'
#' @param name Character; output table name (used as key in the recipe).
#' @param type Character; output layout type. One of \code{"wide"},
#'   \code{"long"}, \code{"features"}, \code{"survival"}, \code{"intervals"},
#'   \code{"baseline"}, \code{"joined_long"}, \code{"covariates_sparse"}.
#' @param variables Character vector or \code{NULL}; variable names to include
#'   (\code{NULL} means all variables in the recipe).
#' @param population_id Character; which population to use (default
#'   \code{"base"}).
#' @param options Named list; type-specific options (e.g. \code{tar} for
#'   survival outputs).
#' @param result_symbol Character or \code{NULL}; R symbol name for the result
#'   on the server (auto-generated as \code{D_<name>} if \code{NULL}).
#' @return An \code{omop_output} object.
#' @examples
#' \dontrun{
#' out <- omop_output(name = "features_wide", type = "wide",
#'                    population_id = "base")
#' recipe <- recipe_add_output(recipe, out)
#' }
#' @seealso \code{\link{recipe_add_output}}, \code{\link{recipe_to_plan}}
#' @export
omop_output <- function(name = "output_1",
                        type = c("wide", "long", "features",
                                 "survival", "intervals",
                                 "baseline", "joined_long",
                                 "covariates_sparse"),
                        variables = NULL,
                        population_id = "base",
                        options = list(),
                        result_symbol = NULL) {
  type <- match.arg(type)
  if (is.null(result_symbol)) {
    result_symbol <- paste0("D_", name)
  }
  obj <- list(
    name          = name,
    type          = type,
    variables     = variables,
    population_id = population_id,
    options       = options,
    result_symbol = result_symbol
  )
  class(obj) <- c("omop_output", "list")
  obj
}

#' Print an omop_output
#'
#' @param x An \code{omop_output} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_output
print.omop_output <- function(x, ...) {
  cat("omop_output:", x$name, "[", x$type, "]",
      "pop:", x$population_id,
      "-> ", x$result_symbol %||% paste0("D_", x$name), "\n")
  if (!is.null(x$variables))
    cat("  Variables:", paste(x$variables, collapse = ", "), "\n")
  invisible(x)
}

# --- omop_recipe: The single source of truth ---

#' Create an empty extraction recipe
#'
#' The recipe is the central data structure for the Recipe Builder workflow.
#' It holds all selections for an OMOP data extraction: populations (who),
#' variable blocks (what, grouped), individual variables, filters (constraints),
#' and output specifications (how to shape the result). A recipe is built
#' incrementally using \code{recipe_add_*} / \code{recipe_remove_*} functions,
#' then compiled to a server-side plan via \code{\link{recipe_to_plan}} and
#' executed via \code{\link{recipe_execute}}.
#'
#' @return An \code{omop_recipe} object containing empty populations, blocks,
#'   variables, filters, outputs, and metadata.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe()
#' recipe <- recipe_add_block(recipe, omop_variable_block(
#'   table = "condition_occurrence",
#'   concept_ids = c(201820),
#'   format = "binary"
#' ))
#' recipe <- recipe_add_output(recipe, omop_output(type = "wide"))
#' plan <- recipe_to_plan(recipe)
#' }
#' @seealso \code{\link{recipe_add_block}}, \code{\link{recipe_add_filter}},
#'   \code{\link{recipe_to_plan}}, \code{\link{recipe_execute}}
#' @export
omop_recipe <- function() {
  obj <- list(
    populations = list(
      base = omop_population(id = "base", label = "All Persons")
    ),
    blocks    = list(),     # Named list of omop_variable_block
    variables = list(),     # Named list of omop_variable
    filters   = list(),     # Named list of omop_filter / omop_filter_group
    outputs   = list(),     # Named list of omop_output
    meta      = list(
      created   = Sys.time(),
      modified  = Sys.time()
    )
  )
  class(obj) <- c("omop_recipe", "list")
  obj
}

#' Add a population to the recipe
#'
#' Registers a new population node in the recipe's population DAG. The
#' population's parent (if any) must already exist in the recipe. The base
#' population is created automatically by \code{\link{omop_recipe}}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param population An \code{\link{omop_population}} object to add.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe()
#' recipe <- recipe_add_population(recipe,
#'   omop_population(id = "adults", label = "Adults 18+",
#'                   parent_id = "base",
#'                   filters = list(omop_filter_age(min = 18))))
#' }
#' @seealso \code{\link{recipe_remove_population}}, \code{\link{omop_population}}
#' @export
recipe_add_population <- function(recipe, population) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (!inherits(population, "omop_population"))
    stop("population must be an omop_population object", call. = FALSE)
  # Validate parent exists
  if (!is.null(population$parent_id) &&
      !population$parent_id %in% names(recipe$populations)) {
    stop("Parent population '", population$parent_id, "' not found in recipe",
         call. = FALSE)
  }
  recipe$populations[[population$id]] <- population
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Remove a population from the recipe
#'
#' Removes a population node from the recipe's population DAG. The base
#' population cannot be removed.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param id Character; population ID to remove (must not be \code{"base"}).
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_remove_population(recipe, "adults")
#' }
#' @seealso \code{\link{recipe_add_population}}
#' @export
recipe_remove_population <- function(recipe, id) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (id == "base") stop("Cannot remove base population", call. = FALSE)
  recipe$populations[[id]] <- NULL
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Add a variable block to the recipe
#'
#' Registers the block and expands its \code{concept_ids} into individual
#' \code{\link{omop_variable}} objects using the block's default settings
#' (table, format, time window, filters). Variable names are auto-generated
#' from concept names or IDs with uniqueness enforcement.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param block An \code{\link{omop_variable_block}} object.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe()
#' recipe <- recipe_add_block(recipe, omop_variable_block(
#'   table = "condition_occurrence",
#'   concept_ids = c(201820, 320128),
#'   format = "binary"
#' ))
#' }
#' @seealso \code{\link{omop_variable_block}}, \code{\link{recipe_add_variable}}
#' @export
recipe_add_block <- function(recipe, block) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (!inherits(block, "omop_variable_block"))
    stop("block must be an omop_variable_block object", call. = FALSE)

  recipe$blocks[[block$id]] <- block

  # Expand concepts into variables
  existing_names <- names(recipe$variables)
  for (i in seq_along(block$concept_ids)) {
    cid <- block$concept_ids[i]
    cname <- if (!is.null(block$concept_names) && i <= length(block$concept_names))
      block$concept_names[i] else NULL

    base_name <- if (!is.null(cname)) .sanitize_name(cname)
                 else paste0(block$table, "_c", cid)
    var_name <- .ensure_unique_name(base_name, existing_names)

    v <- omop_variable(
      name = var_name, table = block$table,
      concept_id = cid, concept_name = cname,
      format = block$format, value_source = block$value_source,
      time_window = block$time_window,
      suffix_mode = block$suffix_mode,
      filters = block$filters
    )
    recipe$variables[[var_name]] <- v
    existing_names <- c(existing_names, var_name)
  }

  recipe$meta$modified <- Sys.time()
  recipe
}

#' Add a variable to the recipe
#'
#' Adds a single variable to the recipe. Either pass a pre-built
#' \code{\link{omop_variable}} object, or pass arguments directly which
#' will be forwarded to \code{omop_variable()}. The variable name is
#' deduplicated if it conflicts with existing names.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param variable An \code{\link{omop_variable}} object, or \code{NULL} to
#'   construct from \code{...}.
#' @param ... If \code{variable} is \code{NULL}, arguments passed to
#'   \code{\link{omop_variable}()}.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_variable(recipe,
#'   table = "measurement", concept_id = 3004249,
#'   concept_name = "Systolic BP", format = "mean")
#' }
#' @seealso \code{\link{recipe_remove_variable}}, \code{\link{recipe_add_block}}
#' @export
recipe_add_variable <- function(recipe, variable = NULL, ...) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  if (is.null(variable)) {
    variable <- omop_variable(...)
  } else if (!inherits(variable, "omop_variable")) {
    stop("variable must be an omop_variable object", call. = FALSE)
  }

  # Ensure unique name
  variable$name <- .ensure_unique_name(variable$name, names(recipe$variables))

  recipe$variables[[variable$name]] <- variable
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Remove a variable from the recipe
#'
#' Removes a variable by name from the recipe's variable list.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param name Character; variable name to remove.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_remove_variable(recipe, "condition_occurrence_c201820")
#' }
#' @seealso \code{\link{recipe_add_variable}}
#' @export
recipe_remove_variable <- function(recipe, name) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  recipe$variables[[name]] <- NULL
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Add a filter to the recipe
#'
#' Registers a filter or filter group in the recipe. Filters are applied during
#' plan compilation: population-level filters restrict the cohort, row-level
#' filters restrict events. The filter ID is auto-generated from the type if
#' not provided.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param filter An \code{\link{omop_filter}} or \code{\link{omop_filter_group}}
#'   object.
#' @param id Character or \code{NULL}; filter ID (auto-generated from type and
#'   sequence number if \code{NULL}).
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_filter(recipe, omop_filter_sex("F"))
#' recipe <- recipe_add_filter(recipe,
#'   omop_filter_age(min = 18, max = 65), id = "adults_only")
#' }
#' @seealso \code{\link{recipe_remove_filter}}, \code{\link{omop_filter}}
#' @export
recipe_add_filter <- function(recipe, filter, id = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (!inherits(filter, "omop_filter") && !inherits(filter, "omop_filter_group"))
    stop("filter must be an omop_filter or omop_filter_group object",
         call. = FALSE)

  if (is.null(id)) {
    ftype <- if (inherits(filter, "omop_filter_group")) filter$operator
             else filter$type
    id <- paste0("f", length(recipe$filters) + 1L, "_", ftype)
  }
  recipe$filters[[id]] <- filter
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Remove a filter from the recipe
#'
#' Removes a filter by its ID from the recipe's filter list.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param id Character; filter ID to remove.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_remove_filter(recipe, "f1_sex")
#' }
#' @seealso \code{\link{recipe_add_filter}}
#' @export
recipe_remove_filter <- function(recipe, id) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  recipe$filters[[id]] <- NULL
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Add an output specification to the recipe
#'
#' Registers an output specification that defines how extracted data should be
#' shaped. Multiple outputs can target different subsets of variables and
#' populations, each producing a separate server-side dataset.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param output An \code{\link{omop_output}} object.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_add_output(recipe,
#'   omop_output(name = "wide_data", type = "wide"))
#' }
#' @seealso \code{\link{recipe_remove_output}}, \code{\link{omop_output}}
#' @export
recipe_add_output <- function(recipe, output) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (!inherits(output, "omop_output"))
    stop("output must be an omop_output object", call. = FALSE)

  recipe$outputs[[output$name]] <- output
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Remove an output specification from the recipe
#'
#' Removes an output specification by name from the recipe.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param name Character; output name to remove.
#' @return The modified \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_remove_output(recipe, "wide_data")
#' }
#' @seealso \code{\link{recipe_add_output}}
#' @export
recipe_remove_output <- function(recipe, name) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  recipe$outputs[[name]] <- NULL
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Clear the entire recipe
#'
#' Discards all populations, blocks, variables, filters, and outputs,
#' returning a fresh empty recipe identical to \code{\link{omop_recipe}()}.
#'
#' @param recipe An \code{omop_recipe} object (used only for type validation).
#' @return A new empty \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_clear(recipe)
#' }
#' @seealso \code{\link{omop_recipe}}
#' @export
recipe_clear <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  omop_recipe()
}

#' Print an omop_recipe
#'
#' @param x An \code{omop_recipe} object.
#' @param ... Additional arguments (ignored).
#' @return \code{x}, invisibly.
#' @export
#' @method print omop_recipe
print.omop_recipe <- function(x, ...) {
  cat("=== omop_recipe ===\n")

  cat("Populations (", length(x$populations), "):\n")
  for (pid in names(x$populations)) {
    p <- x$populations[[pid]]
    parent_txt <- if (!is.null(p$parent_id)) paste("->", p$parent_id) else ""
    cat("  ", pid, parent_txt, ": ", p$label, "\n")
  }

  if (length(x$blocks) > 0) {
    cat("Variable Blocks (", length(x$blocks), "):\n")
    for (bid in names(x$blocks)) {
      b <- x$blocks[[bid]]
      cat("  ", bid, ": ", b$table, " [", length(b$concept_ids),
          " concepts] -> ", b$format, "\n")
    }
  }

  cat("Variables (", length(x$variables), "):\n")
  for (nm in names(x$variables)) {
    v <- x$variables[[nm]]
    concept_info <- if (!is.null(v$concept_id))
      paste0(" [", v$concept_id,
             if (!is.null(v$concept_name)) paste0(": ", v$concept_name) else "",
             "]") else ""
    cat("  ", nm, ": ", v$table, concept_info, " -> ", v$format, "\n")
  }

  cat("Filters (", length(x$filters), "):\n")
  for (id in names(x$filters)) {
    f <- x$filters[[id]]
    if (inherits(f, "omop_filter_group")) {
      cat("  [", f$operator, "] ", f$label, "\n")
    } else {
      cat("  [", f$level, "] ", f$label, "\n")
    }
  }

  cat("Outputs (", length(x$outputs), "):\n")
  for (nm in names(x$outputs)) {
    o <- x$outputs[[nm]]
    cat("  ", nm, " [", o$type, "] pop:", o$population_id, "\n")
  }
  invisible(x)
}

# --- Recipe -> Plan conversion ---

#' Convert a recipe to an extraction plan
#'
#' Compiles the recipe into an \code{omop_plan} suitable for server-side
#' execution via \code{\link{ds.omop.plan.execute}}. The conversion maps
#' population-level filters to cohort specifications, groups variables by
#' output and table, selects the appropriate plan builder (person_level,
#' features, events, survival, intervals) for each output type, and attaches
#' row-level filter trees.
#'
#' @param recipe An \code{omop_recipe} object.
#' @return An \code{omop_plan} object ready for execution.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe()
#' recipe <- recipe_add_block(recipe, omop_variable_block(
#'   table = "condition_occurrence",
#'   concept_ids = c(201820), format = "binary"))
#' recipe <- recipe_add_output(recipe, omop_output(type = "wide"))
#' plan <- recipe_to_plan(recipe)
#' }
#' @seealso \code{\link{recipe_execute}}, \code{\link{omop_recipe}},
#'   \code{\link{ds.omop.plan}}
#' @export
recipe_to_plan <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  plan <- ds.omop.plan()

  # Build cohort from base population + population-level filters
  base_pop <- recipe$populations[["base"]]
  if (!is.null(base_pop$cohort_definition_id)) {
    plan <- ds.omop.plan.cohort(plan,
      cohort_definition_id = base_pop$cohort_definition_id)
  }

  # Collect all population-level filters
  pop_filters <- .collect_pop_filters(recipe)
  if (length(pop_filters) > 0) {
    spec <- lapply(pop_filters, function(f) {
      list(type = f$type, params = f$params)
    })
    if (is.null(plan$cohort)) {
      plan$cohort <- list(type = "spec", spec = spec)
    } else {
      plan$cohort$spec <- spec
    }
  }

  # Group variables by output
  for (out_name in names(recipe$outputs)) {
    out <- recipe$outputs[[out_name]]
    var_names <- out$variables %||% names(recipe$variables)
    vars <- recipe$variables[var_names]
    vars <- Filter(Negate(is.null), vars)

    if (length(vars) == 0) next

    # Group by table
    by_table <- list()
    for (v in vars) {
      tbl <- v$table
      if (is.null(by_table[[tbl]])) by_table[[tbl]] <- list()
      by_table[[tbl]] <- c(by_table[[tbl]], list(v))
    }

    if (out$type %in% c("wide", "baseline")) {
      if (out$type == "baseline") {
        tables_spec <- lapply(by_table, function(vs) {
          cols <- unique(unlist(lapply(vs, function(v) {
            c(v$column, v$value_source)
          })))
          cols[!is.null(cols)]
        })
        plan <- ds.omop.plan.baseline(plan,
          columns = tables_spec[["person"]] %||% c("gender_concept_id",
            "year_of_birth", "race_concept_id"),
          name = out_name)
      } else {
        # Separate person-derived variables from event/raw variables
        person_derived_fmts <- c("age", "sex_mf", "obs_duration")
        derived_vars <- Filter(function(v) {
          v$format %in% person_derived_fmts
        }, vars)
        event_vars <- Filter(function(v) {
          !v$format %in% person_derived_fmts
        }, vars)

        # Build derived_columns specs for plan
        derived_specs <- NULL
        if (length(derived_vars) > 0) {
          derived_specs <- lapply(derived_vars, function(v) {
            spec <- list(kind = v$format, name = v$name)
            if (!is.null(v$derived)) {
              spec <- c(spec, v$derived[setdiff(names(v$derived), "kind")])
            }
            spec
          })
        }

        # "wide" with explicit formats → use features pipeline
        has_formats <- any(vapply(event_vars, function(v) {
          !is.null(v$format) && v$format != "raw"
        }, logical(1)))

        if (length(event_vars) == 0) {
          # ALL variables are person-derived — create person_level output
          # with empty tables but with derived_columns
          plan$outputs[[out_name]] <- list(
            type = "person_level",
            tables = list(),
            representation = "features",
            derived_columns = derived_specs
          )
        } else if (has_formats) {
          # Re-group event_vars by table
          ev_by_table <- list()
          for (v in event_vars) {
            tbl <- v$table
            if (is.null(ev_by_table[[tbl]])) ev_by_table[[tbl]] <- list()
            ev_by_table[[tbl]] <- c(ev_by_table[[tbl]], list(v))
          }

          if (length(ev_by_table) == 1 && is.null(derived_specs)) {
            # Single table, no derived: use event_level features directly
            tbl <- names(ev_by_table)[1]
            vs <- ev_by_table[[tbl]]
            specs <- .build_feature_specs(vs)
            plan <- ds.omop.plan.features(plan, name = out_name, table = tbl,
                                           specs = specs)
          } else {
            # Multi-table or has derived: use person_level with features
            tables_spec <- list()
            for (tbl in names(ev_by_table)) {
              vs <- ev_by_table[[tbl]]
              has_feature_fmts <- any(vapply(vs, function(v) {
                !is.null(v$format) && v$format != "raw"
              }, logical(1)))

              if (has_feature_fmts) {
                feat_vs <- Filter(function(v) {
                  !is.null(v$format) && v$format != "raw"
                }, vs)
                specs <- .build_feature_specs(feat_vs)
                concept_ids <- unique(unlist(lapply(feat_vs, function(v) v$concept_id)))
                concept_ids <- concept_ids[!is.null(concept_ids)]
                tables_spec[[tbl]] <- list(
                  concept_set = if (length(concept_ids) > 0) as.integer(concept_ids) else NULL,
                  features = specs
                )
              } else {
                cols <- unique(unlist(lapply(vs, function(v) {
                  c(v$column, v$value_source)
                })))
                cols <- cols[!is.null(cols)]
                tables_spec[[tbl]] <- cols
              }
            }
            plan$outputs[[out_name]] <- list(
              type = "person_level",
              tables = tables_spec,
              representation = "features",
              derived_columns = derived_specs
            )
          }
        } else {
          # Raw format: use person_level join
          ev_by_table <- list()
          for (v in event_vars) {
            tbl <- v$table
            if (is.null(ev_by_table[[tbl]])) ev_by_table[[tbl]] <- list()
            ev_by_table[[tbl]] <- c(ev_by_table[[tbl]], list(v))
          }
          tables_spec <- lapply(ev_by_table, function(vs) {
            cols <- unique(unlist(lapply(vs, function(v) {
              c(v$column, v$value_source)
            })))
            cols[!is.null(cols)]
          })
          if (!is.null(derived_specs)) {
            plan$outputs[[out_name]] <- list(
              type = "person_level",
              tables = tables_spec,
              derived_columns = derived_specs
            )
          } else {
            plan <- ds.omop.plan.person_level(plan, tables = tables_spec,
                                               name = out_name)
          }
        }
      }
    } else if (out$type %in% c("long", "joined_long")) {
      for (tbl in names(by_table)) {
        vs <- by_table[[tbl]]
        concept_ids <- unique(unlist(lapply(vs, function(v) v$concept_id)))
        concept_ids <- concept_ids[!is.null(concept_ids)]
        columns <- unique(unlist(lapply(vs, function(v) {
          c(v$column, v$value_source)
        })))
        columns <- columns[!is.null(columns)]

        nm <- if (length(by_table) > 1) paste0(out_name, "_", tbl)
              else out_name
        plan <- ds.omop.plan.events(
          plan, name = nm, table = tbl,
          columns = if (length(columns) > 0) columns else NULL,
          concept_set = if (length(concept_ids) > 0) concept_ids else NULL
        )
      }
    } else if (out$type %in% c("features", "covariates_sparse")) {
      for (tbl in names(by_table)) {
        vs <- by_table[[tbl]]
        specs <- .build_feature_specs(vs)

        nm <- if (length(by_table) > 1) paste0(out_name, "_", tbl)
              else out_name
        plan <- ds.omop.plan.features(plan, name = nm, table = tbl,
                                       specs = specs)
      }
    } else if (out$type == "survival") {
      concept_ids <- unique(unlist(lapply(vars, function(v) v$concept_id)))
      concept_ids <- concept_ids[!is.null(concept_ids)]
      tbl <- names(by_table)[1] %||% "condition_occurrence"
      tar <- out$options$tar %||% list(start_offset = 0, end_offset = 730)
      plan <- ds.omop.plan.survival(
        plan, outcome_table = tbl,
        outcome_concepts = concept_ids,
        tar = tar, name = out_name
      )
    } else if (out$type == "intervals") {
      tables <- names(by_table)
      plan <- ds.omop.plan.intervals(plan, tables = tables,
                                      name = out_name)
    }
  }

  # Apply row-level filters to relevant outputs (preserving AND/OR tree)
  row_filter_items <- .extract_filters_by_level(recipe$filters, "row")
  if (length(row_filter_items) > 0) {
    filter_tree <- .compile_filter_tree(row_filter_items)
    for (out_name in names(plan$outputs)) {
      plan$outputs[[out_name]]$filter <- filter_tree
    }
  }

  plan
}

#' Build feature specifications from a list of omop_variable objects
#'
#' Maps each variable's format to the corresponding \code{omop.feature.*}
#' constructor and returns a named list of feature spec objects.
#'
#' @param vs List of \code{omop_variable} objects.
#' @return Named list of feature spec objects.
#' @keywords internal
.build_feature_specs <- function(vs) {
  specs <- lapply(vs, function(v) {
    fmt <- v$format %||% "binary"
    feat_fn <- switch(fmt,
      binary        = omop.feature.boolean,
      count         = omop.feature.count,
      first_value   = omop.feature.first_value,
      last_value    = omop.feature.latest_value,
      mean          = omop.feature.mean_value,
      min           = omop.feature.min_value,
      max           = omop.feature.max_value,
      time_since    = omop.feature.time_since,
      drug_duration = omop.feature.drug_duration,
      sum           = omop.feature.sum_value,
      n_distinct    = omop.feature.n_distinct,
      omop.feature.boolean
    )
    args <- list(
      name = v$name,
      concept_set = if (!is.null(v$concept_id)) v$concept_id else integer(0)
    )
    # Pass value_column for feature types that support it
    if (!is.null(v$value_source) && fmt %in% c("mean", "min", "max",
        "first_value", "last_value", "sum")) {
      args$value_column <- v$value_source
    }
    # Pass agg for drug_duration
    if (fmt == "drug_duration" && !is.null(v$derived$agg)) {
      args$agg <- v$derived$agg
    }
    do.call(feat_fn, args)
  })
  names(specs) <- vapply(vs, function(v) v$name, character(1))
  specs
}

#' Collect all population-level filters from a recipe, flattening groups
#'
#' @param recipe An \code{omop_recipe} object.
#' @return List of \code{omop_filter} objects at the \code{"population"} level.
#' @keywords internal
.collect_pop_filters <- function(recipe) {
  .flatten_filters(recipe$filters, level = "population")
}

#' Collect all row-level filters from a recipe, flattening groups
#'
#' @param recipe An \code{omop_recipe} object.
#' @return List of \code{omop_filter} objects at the \code{"row"} level.
#' @keywords internal
.collect_row_filters <- function(recipe) {
  .flatten_filters(recipe$filters, level = "row")
}

#' Recursively flatten filter groups into individual filters at a given level
#'
#' @param filters List of \code{omop_filter} and/or \code{omop_filter_group}
#'   objects.
#' @param level Character or \code{NULL}; filter level to keep (\code{NULL}
#'   keeps all).
#' @return Flat list of \code{omop_filter} objects.
#' @keywords internal
.flatten_filters <- function(filters, level = NULL) {
  result <- list()
  for (f in filters) {
    if (inherits(f, "omop_filter_group")) {
      result <- c(result, .flatten_filters(f$children, level))
    } else if (inherits(f, "omop_filter")) {
      if (is.null(level) || f$level == level) {
        result <- c(result, list(f))
      }
    }
  }
  result
}

# --- Filter tree compilation (preserves AND/OR structure) ---

#' Compile a list of filters into an AND/OR tree for the server filter DSL
#'
#' @param filters List of \code{omop_filter} or \code{omop_filter_group}
#'   objects.
#' @param default_operator Character; default Boolean operator (\code{"and"}).
#' @return A nested list structure representing the filter tree, or \code{NULL}
#'   if empty.
#' @keywords internal
.compile_filter_tree <- function(filters, default_operator = "and") {
  if (length(filters) == 0) return(NULL)
  children <- lapply(filters, .compile_filter_node)
  children <- Filter(Negate(is.null), children)
  if (length(children) == 0) return(NULL)
  if (length(children) == 1) return(children[[1]])
  setNames(list(children), default_operator)
}

#' Compile a single filter or filter group into the server filter DSL
#'
#' @param f An \code{omop_filter} or \code{omop_filter_group} object.
#' @return A nested list node for the filter tree, or \code{NULL}.
#' @keywords internal
.compile_filter_node <- function(f) {
  if (inherits(f, "omop_filter_group")) {
    op <- tolower(f$operator)
    children <- lapply(f$children, .compile_filter_node)
    children <- Filter(Negate(is.null), children)
    if (length(children) == 0) return(NULL)
    if (length(children) == 1) return(children[[1]])
    return(setNames(list(children), op))
  }
  if (inherits(f, "omop_filter")) return(.filter_to_leaf(f))
  NULL
}

#' Convert an omop_filter to a leaf node matching the server's .compileFilter()
#'
#' @param f An \code{omop_filter} object.
#' @return A list with \code{var}, \code{op}, and \code{value} fields, or
#'   \code{NULL} if the filter cannot be converted.
#' @keywords internal
.filter_to_leaf <- function(f) {
  switch(f$type,
    "sex" = list(var = "gender_concept_id", op = "==",
                 value = if (toupper(f$params$value) %in% c("M", "MALE")) 8507L else 8532L),
    "age_range" = list(and = list(
      list(var = "age_at_index", op = ">=", value = f$params$min),
      list(var = "age_at_index", op = "<=", value = f$params$max))),
    "date_range" = list(and = list(
      list(var = f$params$date_column %||% "start_date", op = ">=", value = f$params$start),
      list(var = f$params$date_column %||% "start_date", op = "<=", value = f$params$end))),
    "has_concept" =, "concept_set" = list(
      var = f$params$concept_col %||% "concept_id",
      op = "in", value = f$params$concept_ids),
    "value_bin" = list(var = f$params$var, op = "value_bin", value = f$params$value),
    {
      if (!is.null(f$params$var))
        list(var = f$params$var, op = f$params$op, value = f$params$value)
      else
        NULL
    }
  )
}

#' Extract filters (or groups containing filters) at a given level
#'
#' @param filters List of \code{omop_filter} and/or \code{omop_filter_group}
#'   objects.
#' @param level Character; the filter level to extract (\code{"population"},
#'   \code{"row"}, or \code{"output"}).
#' @return List of matching \code{omop_filter} or \code{omop_filter_group}
#'   objects.
#' @keywords internal
.extract_filters_by_level <- function(filters, level) {
  result <- list()
  for (f in filters) {
    if (inherits(f, "omop_filter_group") && .group_has_level(f, level))
      result <- c(result, list(f))
    else if (inherits(f, "omop_filter") && f$level == level)
      result <- c(result, list(f))
  }
  result
}

#' Check if a filter group contains any filter at a given level
#'
#' @param group An \code{omop_filter_group} object.
#' @param level Character; the filter level to check for.
#' @return Logical; \code{TRUE} if any descendant filter matches the level.
#' @keywords internal
.group_has_level <- function(group, level) {
  for (ch in group$children) {
    if (inherits(ch, "omop_filter_group") && .group_has_level(ch, level)) return(TRUE)
    if (inherits(ch, "omop_filter") && ch$level == level) return(TRUE)
  }
  FALSE
}

# --- Recipe -> Code generation ---

#' Generate reproducible R code from a recipe
#'
#' Produces a minimal R script that, when executed, recreates the recipe from
#' scratch. The output contains calls to \code{\link{omop_recipe}},
#' \code{recipe_add_*} functions, and the relevant constructors. Does not
#' include \code{library()} calls or header comments.
#'
#' @param recipe An \code{omop_recipe} object.
#' @return Character string containing executable R code.
#' @examples
#' \dontrun{
#' code <- recipe_to_code(recipe)
#' cat(code)
#' }
#' @seealso \code{\link{recipe_export_json}}, \code{\link{omop_recipe}}
#' @export
recipe_to_code <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  lines <- c("recipe <- omop_recipe()")

  # Populations (skip base)
  for (pid in names(recipe$populations)) {
    if (pid == "base") next
    p <- recipe$populations[[pid]]
    lines <- c(lines, paste0(
      "recipe <- recipe_add_population(recipe, ",
      .build_code("omop_population",
        id = p$id, label = p$label, parent_id = p$parent_id,
        cohort_definition_id = p$cohort_definition_id),
      ")"
    ))
  }

  # Blocks
  for (bid in names(recipe$blocks)) {
    b <- recipe$blocks[[bid]]
    lines <- c(lines, paste0(
      "recipe <- recipe_add_block(recipe, ",
      .build_code("omop_variable_block",
        id = b$id, table = b$table,
        concept_ids = b$concept_ids,
        format = b$format, population_id = b$population_id),
      ")"
    ))
  }

  # Individual variables (not from blocks)
  block_vars <- unlist(lapply(recipe$blocks, function(b) {
    vapply(seq_along(b$concept_ids), function(i) {
      cname <- if (!is.null(b$concept_names) && i <= length(b$concept_names))
        b$concept_names[i] else NULL
      if (!is.null(cname)) .sanitize_name(cname)
      else paste0(b$table, "_c", b$concept_ids[i])
    }, character(1))
  }))
  for (nm in names(recipe$variables)) {
    if (nm %in% block_vars) next
    v <- recipe$variables[[nm]]

    # Use convenience constructors for derived variables
    if (!is.null(v$derived)) {
      code <- switch(v$derived$kind,
        "age" = .build_code("omop_variable_age",
          name = v$name,
          reference = v$derived$reference %||% "today",
          reference_date = v$derived$reference_date),
        "sex_mf" = .build_code("omop_variable_sex", name = v$name),
        "obs_duration" = .build_code("omop_variable_obs_duration",
          name = v$name),
        "drug_duration" = .build_code("omop_variable_drug_duration",
          concept_id = v$concept_id,
          concept_name = v$concept_name,
          name = v$name, agg = v$derived$agg %||% "mean"),
        "sum" = .build_code("omop_variable_sum",
          table = v$table, column = v$derived$column,
          concept_id = v$concept_id, concept_name = v$concept_name,
          name = v$name),
        "n_distinct" = .build_code("omop_variable_n_distinct",
          table = v$table, name = v$name),
        NULL
      )
      if (!is.null(code)) {
        lines <- c(lines, paste0(
          "recipe <- recipe_add_variable(recipe, ", code, ")"))
        next
      }
    }

    lines <- c(lines, paste0(
      "recipe <- recipe_add_variable(recipe, ",
      .build_code("omop_variable",
        name = v$name, table = v$table, column = v$column,
        concept_id = v$concept_id, concept_name = v$concept_name,
        format = v$format, value_source = v$value_source),
      ")"
    ))
  }

  # Filters
  for (id in names(recipe$filters)) {
    f <- recipe$filters[[id]]
    if (inherits(f, "omop_filter_group")) {
      lines <- c(lines, paste0(
        "recipe <- recipe_add_filter(recipe, ",
        .codegen_filter_group(f), ', id = "', id, '")'
      ))
    } else {
      lines <- c(lines, paste0(
        "recipe <- recipe_add_filter(recipe, ",
        .codegen_filter(f), ', id = "', id, '")'
      ))
    }
  }

  # Outputs
  for (nm in names(recipe$outputs)) {
    o <- recipe$outputs[[nm]]
    lines <- c(lines, paste0(
      "recipe <- recipe_add_output(recipe, ",
      .build_code("omop_output",
        name = o$name, type = o$type, population_id = o$population_id,
        result_symbol = o$result_symbol),
      ")"
    ))
  }

  paste(lines, collapse = "\n")
}

#' Generate R code string for a single omop_filter
#'
#' @param f An \code{omop_filter} object.
#' @return Character; R code that recreates the filter.
#' @keywords internal
.codegen_filter <- function(f) {
  if (f$type == "sex") {
    .build_code("omop_filter_sex", value = f$params$value)
  } else if (f$type == "age_group") {
    .build_code("omop_filter_age_group", groups = f$params$groups)
  } else if (f$type == "age_range") {
    .build_code("omop_filter_age", min = f$params$min, max = f$params$max)
  } else if (f$type == "has_concept") {
    .build_code("omop_filter_has_concept",
      concept_id = f$params$concept_id,
      table = f$params$table,
      concept_name = f$params$concept_name)
  } else if (f$type == "date_range") {
    .build_code("omop_filter_date_range",
      start = f$params$start, end = f$params$end)
  } else if (f$type == "value_threshold") {
    .build_code("omop_filter_value",
      op = f$params$op, value = f$params$value)
  } else {
    .build_code("omop_filter", type = f$type, level = f$level)
  }
}

#' Generate R code string for a filter group
#'
#' @param fg An \code{omop_filter_group} object.
#' @return Character; R code that recreates the filter group.
#' @keywords internal
.codegen_filter_group <- function(fg) {
  child_code <- vapply(fg$children, function(ch) {
    if (inherits(ch, "omop_filter_group")) .codegen_filter_group(ch)
    else .codegen_filter(ch)
  }, character(1))
  paste0("omop_filter_group(",
         paste(child_code, collapse = ", "),
         ', operator = "', fg$operator, '")')
}

# --- JSON import/export ---

#' Export a recipe to JSON
#'
#' Serializes the recipe to a JSON string or file. All object classes are
#' stripped for clean serialization. The JSON format includes a version tag
#' (\code{"2.0"}) and can be re-imported with \code{\link{recipe_import_json}}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param file Character or \code{NULL}; file path to write. If \code{NULL},
#'   returns the JSON string directly.
#' @return If \code{file} is \code{NULL}, returns a JSON string (invisibly).
#'   Otherwise writes to \code{file} and returns the file path invisibly.
#' @examples
#' \dontrun{
#' json <- recipe_export_json(recipe)
#' recipe_export_json(recipe, file = "my_recipe.json")
#' }
#' @seealso \code{\link{recipe_import_json}}, \code{\link{recipe_to_code}}
#' @export
recipe_export_json <- function(recipe, file = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  # Strip classes for clean JSON serialization
  strip <- function(x) {
    if (is.list(x) && !is.data.frame(x)) {
      x <- lapply(x, strip)
      class(x) <- "list"
    }
    x
  }

  export <- list(
    version     = "2.0",
    populations = strip(recipe$populations),
    blocks      = strip(recipe$blocks),
    variables   = strip(recipe$variables),
    filters     = strip(recipe$filters),
    outputs     = strip(recipe$outputs)
  )

  json <- jsonlite::toJSON(export, auto_unbox = TRUE, pretty = TRUE,
                            null = "null")
  if (is.null(file)) return(as.character(json))
  writeLines(as.character(json), file)
  invisible(file)
}

#' Import a recipe from JSON
#'
#' Reconstructs an \code{omop_recipe} from a JSON string or file previously
#' created by \code{\link{recipe_export_json}}. Automatically detects whether
#' the input is a file path or a raw JSON string. Filter groups are currently
#' skipped during import.
#'
#' @param json Character; a JSON string, or a file path to a JSON file.
#' @return An \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_import_json("my_recipe.json")
#' recipe <- recipe_import_json('{"version":"2.0","populations":{...}}')
#' }
#' @seealso \code{\link{recipe_export_json}}
#' @export
recipe_import_json <- function(json) {
  # Detect if it's a file path
  if (length(json) == 1 && file.exists(json)) {
    json <- paste(readLines(json, warn = FALSE), collapse = "\n")
  }

  data <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  recipe <- omop_recipe()

  # Restore populations
  if (!is.null(data$populations)) {
    for (pid in names(data$populations)) {
      p <- data$populations[[pid]]
      pop <- omop_population(
        id = p$id %||% pid,
        label = p$label %||% pid,
        parent_id = p$parent_id,
        cohort_definition_id = p$cohort_definition_id
      )
      recipe$populations[[pid]] <- pop
    }
  }

  # Restore variables
  if (!is.null(data$variables)) {
    for (nm in names(data$variables)) {
      v <- data$variables[[nm]]
      var <- omop_variable(
        name = v$name %||% nm,
        table = v$table,
        column = v$column,
        concept_id = v$concept_id,
        concept_name = v$concept_name,
        type = v$type %||% "auto",
        format = v$format %||% "raw",
        value_source = v$value_source
      )
      # Restore derived metadata if present
      if (!is.null(v$derived)) {
        var$derived <- v$derived
      }
      recipe$variables[[nm]] <- var
    }
  }

  # Restore filters (simple filters only for now)
  if (!is.null(data$filters)) {
    for (fid in names(data$filters)) {
      f <- data$filters[[fid]]
      if (!is.null(f$operator)) {
        # Skip filter groups in JSON import for now
        next
      }
      filt <- omop_filter(
        type = f$type %||% "custom",
        level = f$level %||% "population",
        params = f$params %||% list(),
        label = f$label
      )
      recipe$filters[[fid]] <- filt
    }
  }

  # Restore outputs
  if (!is.null(data$outputs)) {
    for (nm in names(data$outputs)) {
      o <- data$outputs[[nm]]
      out <- omop_output(
        name = o$name %||% nm,
        type = o$type %||% "wide",
        variables = o$variables,
        population_id = o$population_id %||% "base",
        result_symbol = o$result_symbol
      )
      recipe$outputs[[nm]] <- out
    }
  }

  recipe
}

# --- Preview schema ---

#' Preview the output schema for a recipe
#'
#' Returns the projected columns, join keys, source tables, and output shape
#' for each output defined in the recipe, without executing anything on the
#' server. Useful for verifying the recipe structure before running it.
#'
#' @param recipe An \code{omop_recipe} object.
#' @return A named list of \code{data.frame}s (one per output), each with
#'   columns \code{output}, \code{column}, \code{source}, \code{concept},
#'   \code{type}, and \code{format}. Attributes \code{"join_key"},
#'   \code{"tables"}, \code{"output_type"}, and \code{"population_id"} are
#'   attached to each data.frame.
#' @examples
#' \dontrun{
#' schemas <- recipe_preview_schema(recipe)
#' print(schemas[["output_1"]])
#' }
#' @seealso \code{\link{recipe_to_plan}}, \code{\link{recipe_preview_stats}}
#' @export
recipe_preview_schema <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  schemas <- list()
  for (out_name in names(recipe$outputs)) {
    out <- recipe$outputs[[out_name]]
    var_names <- out$variables %||% names(recipe$variables)
    vars <- recipe$variables[var_names]
    vars <- Filter(Negate(is.null), vars)

    rows <- lapply(vars, function(v) {
      data.frame(
        output    = out_name,
        column    = v$name,
        source    = paste0(v$table, ".", v$column %||% "*"),
        concept   = if (!is.null(v$concept_id)) as.character(v$concept_id) else "",
        type      = v$type,
        format    = v$format,
        stringsAsFactors = FALSE
      )
    })

    if (length(rows) > 0) {
      schema <- do.call(rbind, rows)
    } else {
      schema <- data.frame(
        output = character(0), column = character(0),
        source = character(0), concept = character(0),
        type = character(0), format = character(0),
        stringsAsFactors = FALSE
      )
    }

    tables_used <- unique(vapply(vars, function(v) v$table, character(1)))
    attr(schema, "join_key") <- "person_id"
    attr(schema, "tables") <- tables_used
    attr(schema, "output_type") <- out$type
    attr(schema, "population_id") <- out$population_id

    schemas[[out_name]] <- schema
  }

  schemas
}

# --- Recipe execution helpers ---

#' Preview aggregate stats for a recipe (without materializing)
#'
#' Runs aggregate-only queries via \code{\link{ds.omop.table.stats}} to show
#' safe row and person counts for the tables referenced by the recipe's
#' variables. This provides a quick sanity check without materializing any
#' server-side datasets.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param scope Character; \code{"per_site"}, \code{"pooled"}, or
#'   \code{"both"}.
#' @param symbol Character; OMOP session symbol on the server (default
#'   \code{"omop"}).
#' @param conns DSI connections or \code{NULL} (uses active connections).
#' @return A \code{dsomop_result} with aggregate stats per table.
#' @examples
#' \dontrun{
#' stats <- recipe_preview_stats(recipe, scope = "pooled")
#' }
#' @seealso \code{\link{recipe_execute}}, \code{\link{recipe_preview_schema}}
#' @export
recipe_preview_stats <- function(recipe,
                               scope = c("per_site", "pooled", "both"),
                               symbol = "omop", conns = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  scope <- match.arg(scope)

  # Collect tables referenced
  tables <- unique(vapply(recipe$variables, function(v) v$table, character(1)))

  results <- list()
  for (tbl in tables) {
    tryCatch({
      stats_res <- ds.omop.table.stats(tbl, stats = c("rows", "persons"),
                                        scope = if (scope == "both") "pooled"
                                                else scope,
                                        symbol = symbol)
      results[[tbl]] <- stats_res
    }, error = function(e) {
      results[[tbl]] <<- list(error = conditionMessage(e))
    })
  }

  dsomop_result(
    per_site = results,
    meta = list(
      call_code = .build_code("recipe_preview_stats",
        scope = scope, symbol = symbol),
      scope = scope
    )
  )
}

#' Execute a recipe: compile to plan and run
#'
#' Convenience function that compiles the recipe to an execution plan via
#' \code{\link{recipe_to_plan}} and immediately executes it via
#' \code{\link{ds.omop.plan.execute}}. Symbol names for server-side datasets
#' are derived from the recipe's output \code{result_symbol} fields, or
#' auto-generated as \code{D_<name>}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param out Named character vector; \code{output_name -> symbol_name}
#'   mapping. If \code{NULL}, auto-generates symbol names from the recipe's
#'   output specifications.
#' @param symbol Character; OMOP session symbol on the server (default
#'   \code{"omop"}).
#' @param conns DSI connections or \code{NULL} (uses active connections).
#' @return Invisibly, the output symbol mapping (a named character vector).
#' @examples
#' \dontrun{
#' recipe_execute(recipe)
#' # Or with explicit symbol mapping:
#' recipe_execute(recipe, out = c(features_wide = "D_features"))
#' }
#' @seealso \code{\link{recipe_to_plan}}, \code{\link{ds.omop.plan.execute}}
#' @export
recipe_execute <- function(recipe, out = NULL, symbol = "omop", conns = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  plan <- recipe_to_plan(recipe)

  # Detect split outputs: plan output names that don't match recipe output names
  # e.g., recipe output "my_data" → plan outputs "my_data_condition", "my_data_measurement"
  recipe_out_names <- names(recipe$outputs)
  plan_out_names <- names(plan$outputs)

  # Map plan outputs back to recipe outputs for symbol generation
  if (is.null(out)) {
    symbols <- vapply(plan_out_names, function(nm) {
      # Direct match
      o <- recipe$outputs[[nm]]
      if (!is.null(o) && !is.null(o$result_symbol)) return(o$result_symbol)
      if (!is.null(o)) return(paste0("D_", nm))
      # Check if this is a split output (e.g., "mydata_condition" from recipe "mydata")
      for (recipe_nm in recipe_out_names) {
        if (startsWith(nm, paste0(recipe_nm, "_"))) {
          o <- recipe$outputs[[recipe_nm]]
          sym_base <- if (!is.null(o$result_symbol)) o$result_symbol
                      else paste0("D_", recipe_nm)
          suffix <- sub(paste0("^", recipe_nm, "_"), "", nm)
          return(paste0(sym_base, "_", suffix))
        }
      }
      paste0("D_", nm)
    }, character(1))
    out <- stats::setNames(symbols, plan_out_names)
  }

  ds.omop.plan.execute(plan, out = out, symbol = symbol, conns = conns)
}
