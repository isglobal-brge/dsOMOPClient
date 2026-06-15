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
#' output formatting options. Variables are the atomic building blocks passed to
#' \code{\link{omop_recipe}} via its \code{variables} argument (or grouped with
#' \code{\link{omop_variable_block}} and passed via \code{blocks}).
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
#' @param visit_filter Named list \code{list(concept_ids = ...)} or \code{NULL};
#'   restrict this variable's events to visits of those \code{visit_concept_id}
#'   values (via the \code{visit_occurrence_id} link).
#' @param concept_col Character or \code{NULL}; override the concept column the
#'   \code{concept_id}/concept set scopes (default: the table's domain concept),
#'   e.g. \code{"unit_concept_id"} to extract a single unit for harmonization.
#' @param expand Logical; if \code{TRUE}, expand the concept to include
#'   vocabulary descendants server-side (default \code{FALSE}).
#' @return An \code{omop_variable} object (a named list with class
#'   \code{"omop_variable"}).
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(
#'   variables = omop_variable(
#'     table = "condition_occurrence",
#'     concept_id = 201820,
#'     concept_name = "Type 2 diabetes",
#'     format = "binary"
#'   ),
#'   outputs = omop_output(type = "wide")
#' )
#' }
#' @seealso \code{\link{omop_recipe}}, \code{\link{omop_variable_block}}
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
                                     "sum", "n_distinct",
                                     "sd", "cv", "slope",
                                     "abnormal_high", "abnormal_low",
                                     "gap_max", "gap_mean",
                                     "duration_sum",
                                     "prior_obs", "followup",
                                     "demo_missingness",
                                     "charlson", "chads2", "chadsvasc",
                                     "dcsi", "hfrs"),
                          value_source = NULL,
                          time_window = NULL,
                          suffix_mode = c("index", "range", "label"),
                          filters = list(),
                          visit_filter = NULL,
                          concept_col = NULL,
                          expand = FALSE) {
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
  # Only set when supplied so the plain (exported) form stays stable.
  if (!is.null(visit_filter)) obj$visit_filter <- visit_filter
  if (!is.null(concept_col)) obj$concept_col <- concept_col
  if (isTRUE(expand)) obj$expand <- TRUE
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
#' recipe <- omop_recipe(variables = omop_variable_age(),
#'                       outputs = omop_output(type = "wide"))
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
#' recipe <- omop_recipe(variables = omop_variable_sex(),
#'                       outputs = omop_output(type = "wide"))
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
#' recipe <- omop_recipe(variables = omop_variable_obs_duration(),
#'                       outputs = omop_output(type = "wide"))
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
#' recipe <- omop_recipe(
#'   variables = omop_variable_drug_duration(1124300, concept_name = "Metformin"),
#'   outputs = omop_output(type = "wide"))
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
#' recipe <- omop_recipe(
#'   variables = omop_variable_sum("drug_exposure", "days_supply",
#'                                 concept_id = 1124300),
#'   outputs = omop_output(type = "wide"))
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
#' recipe <- omop_recipe(
#'   variables = omop_variable_n_distinct("condition_occurrence"),
#'   outputs = omop_output(type = "wide"))
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

#' Create a prior observation duration variable
#'
#' Produces a derived variable computing days from observation start to a
#' reference date (default today).
#'
#' @param name Character; output column name (default \code{"prior_obs"}).
#' @param reference_date Date or \code{NULL}; explicit reference date.
#' @return An \code{omop_variable} with \code{format = "prior_obs"}.
#' @export
omop_variable_prior_obs <- function(name = "prior_obs",
                                     reference_date = NULL) {
  v <- omop_variable(
    name = name, table = "observation_period", format = "prior_obs"
  )
  v$derived <- list(kind = "prior_obs", reference_date = reference_date)
  v
}

#' Create a followup duration variable
#'
#' Produces a derived variable computing days from a reference date (default
#' today) to observation end.
#'
#' @param name Character; output column name (default \code{"followup"}).
#' @param reference_date Date or \code{NULL}; explicit reference date.
#' @return An \code{omop_variable} with \code{format = "followup"}.
#' @export
omop_variable_followup <- function(name = "followup",
                                    reference_date = NULL) {
  v <- omop_variable(
    name = name, table = "observation_period", format = "followup"
  )
  v$derived <- list(kind = "followup", reference_date = reference_date)
  v
}

#' Create a demographics missingness variable
#'
#' Produces a derived variable counting the number of missing or zero-valued
#' demographic fields per person (0-6 range).
#'
#' @param name Character; output column name (default \code{"demo_missingness"}).
#' @return An \code{omop_variable} with \code{format = "demo_missingness"}.
#' @export
omop_variable_demo_missingness <- function(name = "demo_missingness") {
  v <- omop_variable(
    name = name, table = "person", format = "demo_missingness"
  )
  v$derived <- list(kind = "demo_missingness")
  v
}

#' Create a standard deviation variable
#'
#' Produces a feature variable computing the standard deviation of a numeric
#' column per person for records matching the concept.
#'
#' @param table Character; source OMOP CDM table.
#' @param concept_id Integer; concept ID filter.
#' @param concept_name Character or \code{NULL}; human-readable name.
#' @param name Character or \code{NULL}; output column name.
#' @param value_source Character; value column (default
#'   \code{"value_as_number"}).
#' @return An \code{omop_variable} with \code{format = "sd"}.
#' @export
omop_variable_sd <- function(table, concept_id,
                              concept_name = NULL,
                              name = NULL,
                              value_source = "value_as_number") {
  if (is.null(name)) {
    base <- if (!is.null(concept_name)) .sanitize_name(concept_name)
            else paste0(table, "_c", concept_id)
    name <- paste0(base, "_sd")
  }
  v <- omop_variable(
    name = name, table = table,
    concept_id = concept_id, concept_name = concept_name,
    format = "sd", value_source = value_source
  )
  v$derived <- list(kind = "sd")
  v
}

#' Create a coefficient of variation variable
#'
#' Produces a feature variable computing \code{sd / mean * 100} per person.
#'
#' @param table Character; source OMOP CDM table.
#' @param concept_id Integer; concept ID filter.
#' @param concept_name Character or \code{NULL}; human-readable name.
#' @param name Character or \code{NULL}; output column name.
#' @param value_source Character; value column (default
#'   \code{"value_as_number"}).
#' @return An \code{omop_variable} with \code{format = "cv"}.
#' @export
omop_variable_cv <- function(table, concept_id,
                              concept_name = NULL,
                              name = NULL,
                              value_source = "value_as_number") {
  if (is.null(name)) {
    base <- if (!is.null(concept_name)) .sanitize_name(concept_name)
            else paste0(table, "_c", concept_id)
    name <- paste0(base, "_cv")
  }
  v <- omop_variable(
    name = name, table = table,
    concept_id = concept_id, concept_name = concept_name,
    format = "cv", value_source = value_source
  )
  v$derived <- list(kind = "cv")
  v
}

#' Create a slope (linear trend) variable
#'
#' Produces a feature variable fitting a linear model of value over time per
#' person and extracting the slope.
#'
#' @param table Character; source OMOP CDM table.
#' @param concept_id Integer; concept ID filter.
#' @param concept_name Character or \code{NULL}; human-readable name.
#' @param name Character or \code{NULL}; output column name.
#' @param value_source Character; value column (default
#'   \code{"value_as_number"}).
#' @return An \code{omop_variable} with \code{format = "slope"}.
#' @export
omop_variable_slope <- function(table, concept_id,
                                 concept_name = NULL,
                                 name = NULL,
                                 value_source = "value_as_number") {
  if (is.null(name)) {
    base <- if (!is.null(concept_name)) .sanitize_name(concept_name)
            else paste0(table, "_c", concept_id)
    name <- paste0(base, "_slope")
  }
  v <- omop_variable(
    name = name, table = table,
    concept_id = concept_id, concept_name = concept_name,
    format = "slope", value_source = value_source
  )
  v$derived <- list(kind = "slope")
  v
}

#' Create a Charlson Comorbidity Index variable
#'
#' Produces a derived variable computing the Charlson Comorbidity Index
#' (17 categories, standard weights 1-6).
#'
#' @param name Character; output column name (default \code{"charlson"}).
#' @return An \code{omop_variable} with \code{format = "charlson"}.
#' @export
omop_variable_charlson <- function(name = "charlson") {
  v <- omop_variable(
    name = name, table = "condition_occurrence", format = "charlson"
  )
  v$derived <- list(kind = "charlson")
  v
}

#' Create a CHA2DS2-VASc score variable
#'
#' Produces a derived variable computing the CHA2DS2-VASc stroke risk score
#' (7 categories for atrial fibrillation).
#'
#' @param name Character; output column name (default \code{"chadsvasc"}).
#' @return An \code{omop_variable} with \code{format = "chadsvasc"}.
#' @export
omop_variable_chadsvasc <- function(name = "chadsvasc") {
  v <- omop_variable(
    name = name, table = "condition_occurrence", format = "chadsvasc"
  )
  v$derived <- list(kind = "chadsvasc")
  v
}

#' Create a CHADS2 score variable
#'
#' Produces a derived variable computing the CHADS2 stroke risk score
#' (analysis_id 903 in FeatureExtraction).
#' Components: CHF, Hypertension, Age >= 75, Diabetes, Stroke/TIA (x2).
#'
#' @param name Character; output column name (default \code{"chads2"}).
#' @return An \code{omop_variable} with \code{format = "chads2"}.
#' @export
omop_variable_chads2 <- function(name = "chads2") {
  v <- omop_variable(
    name = name, table = "condition_occurrence", format = "chads2"
  )
  v$derived <- list(kind = "chads2")
  v
}

#' Create a DCSI score variable
#'
#' The Diabetes Complications Severity Index (analysis_id 902) uses ICD9CM
#' source codes mapped via concept_relationship to SNOMED targets (tiered
#' scoring: MAX tier per category, SUM across 7 categories, max total 13).
#' Requires ICD9CM vocabulary loaded in the CDM. Returns 0 for all persons
#' if concept_relationship mappings are not available.
#'
#' @param name Character; output column name (default \code{"dcsi"}).
#' @return An \code{omop_variable} with \code{format = "dcsi"}.
#' @export
omop_variable_dcsi <- function(name = "dcsi") {
  v <- omop_variable(
    name = name, table = "condition_occurrence", format = "dcsi"
  )
  v$derived <- list(kind = "dcsi")
  v
}

#' Create an HFRS score variable
#'
#' The Hospital Frailty Risk Score (analysis_id 926) uses ICD-10 source codes
#' mapped via concept_relationship to SNOMED targets (109 weighted categories,
#' decimal weights 0.1-7.1, binary presence x weight). Supports both ICD10CM
#' and ICD10 vocabularies. Returns 0 for all persons if concept_relationship
#' mappings are not available.
#'
#' @param name Character; output column name (default \code{"hfrs"}).
#' @return An \code{omop_variable} with \code{format = "hfrs"}.
#' @export
omop_variable_hfrs <- function(name = "hfrs") {
  v <- omop_variable(
    name = name, table = "condition_occurrence", format = "hfrs"
  )
  v$derived <- list(kind = "hfrs")
  v
}

# --- omop_filter + omop_filter_group: Conditions & chaining ---

#' Create a filter specification
#'
#' Filters restrict the population or events included in the extraction.
#' There are three levels: \code{"population"} (person-level inclusion criteria),
#' \code{"row"} (event-level restrictions), and \code{"output"} (post-extraction
#' transformations). Filters are passed to \code{\link{omop_recipe}} via its
#' \code{filters} argument and can be nested into groups with
#' \code{\link{omop_filter_group}}.
#'
#' Convenience constructors are provided for common filter types:
#' \code{\link{omop_filter_sex}}, \code{\link{omop_filter_age}},
#' \code{\link{omop_filter_age_group}}, \code{\link{omop_filter_has_concept}},
#' \code{\link{omop_filter_date_range}}, \code{\link{omop_filter_value}}.
#'
#' @param type Character; filter type. One of \code{"sex"}, \code{"age_range"},
#'   \code{"age_group"}, \code{"cohort"}, \code{"has_concept"},
#'   \code{"date_range"}, \code{"concept_set"},
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
#' @seealso \code{\link{omop_recipe}}, \code{\link{omop_filter_group}}
#' @export
omop_filter <- function(type = c("sex", "age_range", "age_group", "cohort",
                                  "has_concept", "date_range",
                                  "concept_set",
                                  "min_count", "top_n", "dedup", "custom",
                                  "not_has_concept", "concept_count",
                                  "prior_observation", "followup",
                                  "visit_count", "has_measurement",
                                  "missing_measurement", "value_bin",
                                  "value_concept"),
                        level = c("population", "row", "output"),
                        params = list(),
                        label = NULL) {
  type <- match.arg(type)
  level <- match.arg(level)

  if (is.null(label)) {
    cid <- paste(unlist(params$concept_id) %||% "?", collapse = ", ")
    label <- switch(type,
      sex = paste0("Sex = ", params$value %||% "?"),
      age_range = paste0("Age ", params$min %||% "?", "-", params$max %||% "?"),
      age_group = paste0("Age groups: ",
                         paste(params$groups %||% "?", collapse = ", ")),
      cohort = paste0("Cohort #", params$cohort_definition_id %||% "?"),
      has_concept = paste0("Has concept ", cid,
                           " in ", params$table %||% "?"),
      date_range = paste0("Dates ", params$start %||% "?",
                          " to ", params$end %||% "?"),
      concept_set = paste0(length(params$concept_ids %||% integer(0)),
                           " concepts"),
      min_count = paste0("Min ", params$min_count %||% 1, " occurrences"),
      top_n = paste0("Top ", params$n %||% "?"),
      dedup = "Deduplicate",
      custom = params$description %||% "Custom filter",
      not_has_concept = paste0("Not has concept ", cid,
                               " in ", params$table %||% "?"),
      concept_count = paste0("Concept ", cid,
                             " count >= ", params$min_count %||% 1),
      prior_observation = paste0("Prior obs >= ",
                                 params$min_days %||% "?", " days"),
      followup = paste0("Followup >= ", params$min_days %||% "?", " days"),
      visit_count = paste0("Visits >= ", params$min_count %||% "?"),
      has_measurement = paste0("Has measurement ", cid,
                               " in range"),
      missing_measurement = paste0("Missing measurement ",
                                   cid),
      value_bin = if (is.list(params$value))
        paste0(params$var %||% "value", " bin [",
               params$value$lower %||% "?", ", ",
               params$value$upper %||% "?", ")")
      else
        paste0(params$var %||% "value", " = ",
               paste(params$value %||% "?", collapse = ", "))
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
#' @seealso \code{\link{omop_filter}}, \code{\link{omop_recipe}}
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
#' @param cohort_definition_id Integer; existing OMOP cohort_definition_id to
#'   require for membership.
#' @export
omop_filter_cohort <- function(cohort_definition_id) {
  omop_filter(
    type = "cohort", level = "population",
    params = list(cohort_definition_id = as.integer(cohort_definition_id)),
    label = paste0("Cohort #", as.integer(cohort_definition_id))
  )
}

#' Format a concept label for filter display (vector-safe)
#'
#' @param concept_id Integer scalar or vector of concept IDs
#' @param concept_name Character or NULL; human-readable name(s)
#' @return Character scalar suitable for a filter label
#' @keywords internal
.concept_label <- function(concept_id, concept_name = NULL) {
  if (!is.null(concept_name)) return(paste(concept_name, collapse = ", "))
  ids <- as.integer(concept_id)
  if (length(ids) <= 1L) paste("concept", ids) else
    paste("concepts", paste(ids, collapse = ", "))
}

#' @rdname omop_filter
#' @param concept_id Integer scalar or vector; the concept(s) to check for
#'   (a vector matches if any concept is present)
#' @param table Character; which OMOP table to check
#' @param concept_name Character or NULL; human-readable name
#' @param window Named list with start/end offsets, or NULL
#' @param min_count Integer; minimum number of occurrences (default 1)
#' @export
omop_filter_has_concept <- function(concept_id, table, concept_name = NULL,
                                     window = NULL, min_count = 1L) {
  label <- paste0("Has ", .concept_label(concept_id, concept_name),
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
  if (is.null(safe_bins) || is.null(safe_bins$breaks)) {
    stop("omop_filter_value() needs disclosure-safe bin edges. Either call ",
         "ds.omop.safe.filter.value(table, column, threshold, direction) which ",
         "fetches them, or pass safe_bins = list(breaks = <edges>) from ",
         "ds.omop.safe.cutpoints(table, column).", call. = FALSE)
  }
  if (length(safe_bins$breaks) < 2) {
    stop("omop_filter_value() needs at least 2 bin edges, but the server ",
         "returned fewer (it may have merged bins for disclosure). Widen ",
         "n_bins or pick a higher-spread column when calling ",
         "ds.omop.safe.cutpoints().", call. = FALSE)
  }
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
                  direction = direction,
                  value = list(lower = lower, upper = upper)),
    label = paste0(column, " ", direction, " ~", threshold,
                   " [bin: ", lower, "-", upper, ")")
  )
}

#' @rdname omop_filter
#' @param concept_ids Integer scalar or vector; the value concept(s) to keep
#'   (a record matches if its value concept is any of them)
#' @param column Character; the value-concept column (default
#'   \code{"value_as_concept_id"})
#' @param concept_name Character or NULL; human-readable name(s)
#' @export
omop_filter_value_concept <- function(concept_ids,
                                       column = "value_as_concept_id",
                                       concept_name = NULL) {
  ids <- as.integer(concept_ids)
  label <- paste0(column, " in ", .concept_label(ids, concept_name))
  omop_filter(
    type = "value_concept", level = "row",
    params = list(var = column, op = "in", value = ids,
                  concept_name = concept_name),
    label = label
  )
}

#' @rdname omop_filter
#' @param concept_id Integer scalar or vector; the concept(s) to exclude
#'   (a vector excludes persons having any of them)
#' @param table Character; which OMOP table to check
#' @param concept_name Character or NULL; human-readable name
#' @param window Named list with start/end index-relative day offsets, or NULL;
#'   restricts absence to that window (e.g. "no drug X in the prior year")
#' @export
omop_filter_not_has_concept <- function(concept_id, table,
                                         concept_name = NULL,
                                         window = NULL) {
  label <- paste0("Not has ", .concept_label(concept_id, concept_name),
                  " in ", table)
  omop_filter(
    type = "not_has_concept", level = "population",
    params = list(
      concept_id = as.integer(concept_id),
      table = table,
      concept_name = concept_name,
      window = window
    ),
    label = label
  )
}

#' @rdname omop_filter
#' @param concept_id Integer scalar or vector; the concept(s) to count
#'   (a vector counts records matching any of them)
#' @param table Character; which OMOP table to check
#' @param min_count Integer; minimum number of records required
#' @param concept_name Character or NULL; human-readable name
#' @export
omop_filter_concept_count <- function(concept_id, table,
                                       min_count = 2L,
                                       concept_name = NULL) {
  label <- paste0(.concept_label(concept_id, concept_name),
                  " count >= ", min_count, " in ", table)
  omop_filter(
    type = "concept_count", level = "population",
    params = list(
      concept_id = as.integer(concept_id),
      table = table,
      min_count = as.integer(min_count),
      concept_name = concept_name
    ),
    label = label
  )
}

#' @rdname omop_filter
#' @param min_days Integer; minimum days of prior observation
#' @export
omop_filter_prior_observation <- function(min_days = 365L) {
  omop_filter(
    type = "prior_observation", level = "population",
    params = list(min_days = as.integer(min_days)),
    label = paste0("Prior obs >= ", min_days, " days")
  )
}

#' @rdname omop_filter
#' @param min_days Integer; minimum days of followup
#' @export
omop_filter_followup <- function(min_days = 30L) {
  omop_filter(
    type = "followup", level = "population",
    params = list(min_days = as.integer(min_days)),
    label = paste0("Followup >= ", min_days, " days")
  )
}

#' @rdname omop_filter
#' @param min_count Integer; minimum number of visits
#' @param visit_concept_id Integer scalar or vector, or NULL; visit type
#'   filter (a vector counts visits of any of the given types)
#' @export
omop_filter_visit_count <- function(min_count = 1L,
                                     visit_concept_id = NULL) {
  omop_filter(
    type = "visit_count", level = "population",
    params = list(
      min_count = as.integer(min_count),
      visit_concept_id = if (!is.null(visit_concept_id))
        as.integer(visit_concept_id) else NULL
    ),
    label = paste0("Visits >= ", min_count)
  )
}

#' @rdname omop_filter
#' @param concept_id Integer scalar or vector; measurement concept ID(s)
#'   (a vector matches if any measurement is present)
#' @param min_value Numeric or NULL; minimum value
#' @param max_value Numeric or NULL; maximum value
#' @export
omop_filter_has_measurement <- function(concept_id,
                                         min_value = NULL,
                                         max_value = NULL) {
  label <- paste0("Has measurement ",
                  paste(as.integer(concept_id), collapse = ", "))
  if (!is.null(min_value) || !is.null(max_value)) {
    label <- paste0(label, " [",
                    if (!is.null(min_value)) min_value else "",
                    "-",
                    if (!is.null(max_value)) max_value else "",
                    "]")
  }
  omop_filter(
    type = "has_measurement", level = "population",
    params = list(
      concept_id = as.integer(concept_id),
      min_value = min_value,
      max_value = max_value
    ),
    label = label
  )
}

#' @rdname omop_filter
#' @param concept_id Integer scalar or vector; measurement concept ID(s) to
#'   check absence of (a vector requires all of them to be absent)
#' @param window Named list with start/end index-relative day offsets, or NULL;
#'   restricts absence to that window (e.g. "no HbA1c in the prior year")
#' @export
omop_filter_missing_measurement <- function(concept_id, window = NULL) {
  omop_filter(
    type = "missing_measurement", level = "population",
    params = list(concept_id = as.integer(concept_id), window = window),
    label = paste0("Missing measurement ",
                   paste(as.integer(concept_id), collapse = ", "))
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
  always_allowed <- c("sex", "age_group", "cohort", "concept_set", "value_bin",
                      "value_concept")
  constrained <- c("age_range", "has_concept", "date_range", "min_count",
                    "not_has_concept", "concept_count", "prior_observation",
                    "followup", "visit_count", "has_measurement",
                    "missing_measurement")
  blocked <- c("custom")

  if (filter_type %in% always_allowed) return("allowed")
  if (filter_type %in% blocked) return("blocked")
  "constrained"
}

# --- omop_population: DAG of subpopulations ---

#' Create a population node
#'
#' A recipe defines one or more populations and every output targets one (via
#' \code{omop_output(..., population_id=)}). Each recipe starts with an implicit
#' \code{"base"} population representing all persons; additional populations are
#' passed to \code{\link{omop_recipe}} via its \code{populations} argument.
#'
#' A population is one of two kinds, which are mutually exclusive:
#' \itemize{
#'   \item \strong{criteria-defined} — a person-level inclusion tree given in
#'     \code{filters} (any mix of \code{\link{omop_filter}} /
#'     \code{\link{omop_filter_group}} at the \code{"population"} level, e.g.
#'     sex + \code{has_concept} + \code{has_measurement}). It compiles to the
#'     same cohort filter tree the server builds the base cohort from.
#'   \item \strong{set-op derived} — built from \emph{other} populations by a
#'     set operation on the person key. Supply exactly one of \code{union},
#'     \code{intersect}, or \code{setdiff} as a character vector of two or more
#'     population IDs; the server folds the named members with the matching
#'     algebra (\code{\link{ds.omop.cohort.combine}}'s \code{.cohortCombine}).
#'     \code{setdiff} keeps persons in the first member and not in the rest.
#' }
#'
#' @param id Character; population ID (must be unique within the recipe).
#' @param label Character; human-readable label.
#' @param parent_id Character or \code{NULL}; parent population ID (\code{NULL}
#'   for root). Informational provenance only; set-op membership is the
#'   executable dependency.
#' @param filters List of \code{\link{omop_filter}} or
#'   \code{\link{omop_filter_group}} objects (criteria populations only).
#' @param cohort_definition_id Integer or \code{NULL}; base cohort definition ID
#'   (if the population is defined by a pre-existing cohort).
#' @param union,intersect,setdiff Character vector of two or more population IDs
#'   to derive this population from by the named set operation on the person key.
#'   Exactly one may be supplied, and only for a set-op population (mutually
#'   exclusive with \code{filters} / \code{cohort_definition_id}).
#' @return An \code{omop_population} object. A set-op population carries a
#'   \code{$setop = list(op, members)} field; a criteria population carries
#'   \code{$filters} (and optionally \code{$cohort_definition_id}).
#' @examples
#' \dontrun{
#' # Criteria population.
#' females <- omop_population(id = "females", label = "Female patients",
#'                            filters = list(omop_filter_sex("F")))
#'
#' # Set-op population: persons in EITHER of two criteria subgroups.
#' either <- omop_population(id = "either", label = "diabetic or hypertensive",
#'                           union = c("diabetic", "hypertensive"))
#' }
#' @seealso \code{\link{omop_recipe}}, \code{\link{omop_filter}},
#'   \code{\link{ds.omop.cohort.combine}}
#' @export
omop_population <- function(id = "base",
                            label = "Base Population",
                            parent_id = NULL,
                            filters = list(),
                            cohort_definition_id = NULL,
                            union = NULL,
                            intersect = NULL,
                            setdiff = NULL) {
  # A population is EITHER criteria-defined (filters / cohort id) OR derived from
  # other populations by ONE set operation; the two shapes never mix.
  setops <- list(union = union, intersect = intersect, setdiff = setdiff)
  setops <- setops[!vapply(setops, is.null, logical(1))]
  setop <- NULL
  if (length(setops) > 0) {
    if (length(setops) > 1) {
      stop("omop_population(): supply only one of union / intersect / setdiff.",
           call. = FALSE)
    }
    if (length(filters) > 0 || !is.null(cohort_definition_id)) {
      stop("omop_population(): a set-op population (union / intersect / ",
           "setdiff) cannot also take filters or cohort_definition_id.",
           call. = FALSE)
    }
    op <- names(setops)[1]
    members <- as.character(setops[[1]])
    members <- members[nzchar(members)]
    if (length(members) < 2) {
      stop("omop_population(): ", op, " needs at least two member population ",
           "IDs.", call. = FALSE)
    }
    if (anyDuplicated(members) > 0) {
      dups <- unique(members[duplicated(members)])
      stop("omop_population(): ", op, " has duplicate member(s): '",
           paste(dups, collapse = "', '"),
           "'. A set operation over a population and itself is degenerate; ",
           "list each member once.", call. = FALSE)
    }
    setop <- list(op = op, members = members)
  }

  obj <- list(
    id                   = id,
    label                = label,
    parent_id            = parent_id,
    filters              = filters,
    cohort_definition_id = if (!is.null(cohort_definition_id))
      as.integer(cohort_definition_id) else NULL
  )
  # Only set when supplied so the plain (exported) form of a criteria
  # population stays byte-stable for the JSON/YAML round-trip.
  if (!is.null(setop)) obj$setop <- setop
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
  if (!is.null(x$setop))
    cat("  Set-op:", x$setop$op, "(",
        paste(x$setop$members, collapse = ", "), ")\n")
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
#' and row-level filters. When passed to \code{\link{omop_recipe}} via its
#' \code{blocks} argument, the block's \code{concept_ids} are expanded into
#' individual \code{\link{omop_variable}} objects that inherit the block's
#' defaults. This is the compact way to add many concepts from one table.
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
#' @param expand Logical; if \code{TRUE}, expand the block's concepts to
#'   include vocabulary descendants server-side (default \code{FALSE}).
#' @return An \code{omop_variable_block} object.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(
#'   blocks = omop_variable_block(
#'     table = "condition_occurrence",
#'     concept_ids = c(201820, 320128),
#'     concept_names = c("Type 2 diabetes", "Essential hypertension"),
#'     format = "binary"
#'   ),
#'   outputs = omop_output(type = "wide")
#' )
#' }
#' @seealso \code{\link{omop_recipe}}, \code{\link{omop_variable}}
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
                                population_id = "base",
                                expand = FALSE) {
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
  # Only set when TRUE so the plain (exported) form stays stable.
  if (isTRUE(expand)) obj$expand <- TRUE
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
#' feature matrix). Outputs are passed to \code{\link{omop_recipe}} via its
#' \code{outputs} argument and determine the server-side plan structure
#' produced by \code{\link{recipe_to_plan}}.
#'
#' @param name Character; output table name (used as key in the recipe).
#' @param type Character; output layout type. One of \code{"wide"},
#'   \code{"long"}, \code{"features"}, \code{"survival"}, \code{"intervals"},
#'   \code{"baseline"}, \code{"covariates_sparse"}. A \code{"long"} output that
#'   spans multiple source tables always splits into one per-table output
#'   (named \code{<name>_<table>}); there is no single cross-table joined frame.
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
#' recipe <- omop_recipe(
#'   variables = omop_variable(table = "condition_occurrence",
#'                             concept_id = 201820, format = "binary"),
#'   outputs = omop_output(name = "features_wide", type = "wide")
#' )
#' }
#' @seealso \code{\link{omop_recipe}}, \code{\link{recipe_to_plan}}
#' @export
omop_output <- function(name = "output_1",
                        type = c("wide", "long", "features",
                                 "survival", "intervals",
                                 "baseline",
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
    # as.character(unlist(...)) so JSON-imported lists of names behave the
    # same as character vectors in recipe_to_plan()'s subsetting.
    variables     = if (!is.null(variables))
      as.character(unlist(variables)) else NULL,
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

#' Build the empty recipe skeleton
#'
#' Internal base used by \code{\link{omop_recipe}}: a fresh recipe with the
#' implicit \code{"base"} population, empty slots, and the default plan options.
#' The declarative \code{omop_recipe(...)} starts from this skeleton and
#' delegates each supplied argument to the internal slot-filling setters, so the
#' declarative form is identical by construction to incremental building.
#'
#' @return An empty \code{omop_recipe} object.
#' @keywords internal
.omop_recipe_empty <- function() {
  obj <- list(
    populations = list(
      base = omop_population(id = "base", label = "All Persons")
    ),
    blocks    = list(),     # Named list of omop_variable_block
    variables = list(),     # Named list of omop_variable
    filters   = list(),     # Named list of omop_filter / omop_filter_group
    outputs   = list(),     # Named list of omop_output
    options   = list(       # Plan-wide options applied at recipe_to_plan()
      translate_concepts = TRUE,
      block_sensitive    = TRUE,
      factor_concepts    = TRUE
    ),
    meta      = list(
      created   = Sys.time(),
      modified  = Sys.time()
    )
  )
  class(obj) <- c("omop_recipe", "list")
  obj
}

#' Normalize a declarative slot argument to a list of items
#'
#' Each \code{omop_recipe()} slot accepts a single building-block object (e.g.
#' one \code{omop_output}) or a list of them. This coerces both forms to a list
#' so the delegation loop can iterate uniformly. \code{NULL} and the leaf-object
#' classes are wrapped into a one-element list; an already-supplied list is
#' returned unchanged (preserving any element names, e.g. filter IDs).
#'
#' @param x A single building-block object, a list of them, or \code{NULL}.
#' @return A list (possibly empty).
#' @keywords internal
.recipe_arg_list <- function(x) {
  if (is.null(x)) return(list())
  leaf_classes <- c("omop_population", "omop_variable_block", "omop_variable",
                    "omop_filter", "omop_filter_group", "omop_output")
  if (inherits(x, leaf_classes)) return(list(x))
  if (is.list(x)) return(x)
  list(x)
}

#' Create an extraction recipe declaratively
#'
#' The recipe is the central user-facing data structure for an OMOP data
#' extraction. It holds every selection in one place: populations (who),
#' variable blocks (what, grouped), individual variables, filters (constraints),
#' outputs (how to shape the result), the base cohort, and plan-wide options.
#'
#' This is the single recipe-authoring entry point: pass the complete extraction
#' as one nested expression built from the leaf constructors
#' (\code{\link{omop_variable}}, \code{\link{omop_filter}},
#' \code{\link{omop_output}}, \code{\link{omop_population}},
#' \code{\link{omop_variable_block}}, and friends). Each argument accepts a
#' single object or a list of objects. The recipe is assembled in dependency
#' order (populations, then blocks, then variables, then filters, then outputs,
#' then the base cohort, then options) so later items can reference earlier ones.
#' Most users then work at the recipe level with \code{\link{recipe_preview}},
#' \code{\link{recipe_execute}}, \code{\link{recipe_save}}, and
#' \code{\link{recipe_load}}; \code{\link{recipe_to_plan}} exposes the
#' lower-level execution contract sent to the server.
#'
#' @param variables A single \code{\link{omop_variable}} (including the
#'   convenience \code{omop_variable_*} derived constructors) or a list of them.
#' @param filters A single \code{\link{omop_filter}} /
#'   \code{\link{omop_filter_group}} or a list of them. A \emph{named} list uses
#'   each name as the filter ID.
#' @param outputs A single \code{\link{omop_output}} or a list of them.
#' @param populations A single \code{\link{omop_population}} or a list of them
#'   (the implicit \code{"base"} population always exists; parents must be
#'   declared before their children).
#' @param blocks A single \code{\link{omop_variable_block}} or a list of them;
#'   each block is expanded into individual variables.
#' @param cohort Recipe scope and/or base cohort. A scalar OMOP
#'   \code{cohort_definition_id} keeps the existing behaviour: it becomes the
#'   base population's cohort (equivalent to \code{recipe_set_cohort}). A cohort
#'   handle (\code{dsomop_cohort_handle}) or a server-side cohort table name is
#'   instead taken as a recipe-level \emph{scope} that is intersected into every
#'   population (alongside \code{tables}). \code{NULL} sets neither.
#' @param tables Character vector of server-side \code{omop.table} symbol names,
#'   or \code{NULL}. Their distinct persons form a recipe-level scope folded with
#'   any \code{cohort} scope by \code{combine} and intersected into every
#'   population (the server resolves the symbol names to frames).
#' @param combine Character; how to fold multiple scope sources together:
#'   \code{"union"} (the default) or \code{"intersect"}.
#' @param options Named list of plan-wide options (\code{translate_concepts},
#'   \code{block_sensitive}, \code{factor_concepts}); only supplied keys
#'   override the defaults.
#' @return An \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(
#'   blocks = omop_variable_block(
#'     table = "condition_occurrence",
#'     concept_ids = c(201820),
#'     format = "binary"
#'   ),
#'   variables = list(
#'     omop_variable_age(),
#'     omop_variable(table = "measurement", concept_id = 3004410,
#'                   format = "mean")
#'   ),
#'   filters = omop_filter_sex("F"),
#'   outputs = omop_output(name = "study", type = "wide")
#' )
#' recipe_execute(recipe)
#'
#' # Multi-population: build two criteria subgroups, UNION them into one
#' # population, then run an output against that union while a recipe-level
#' # scope (a cohort handle INTERSECTED with a workspace omop.table's persons)
#' # narrows every population.
#' recipe2 <- omop_recipe(
#'   populations = list(
#'     omop_population("diabetic", "Diabetics",
#'                     filters = list(omop_filter_has_concept(
#'                       201820, "condition_occurrence"))),
#'     omop_population("hypertensive", "Hypertensives",
#'                     filters = list(omop_filter_has_concept(
#'                       320128, "condition_occurrence"))),
#'     omop_population("either", "Diabetic or hypertensive",
#'                     union = c("diabetic", "hypertensive"))
#'   ),
#'   variables = omop_variable_age(),
#'   outputs = omop_output(name = "study", type = "wide",
#'                         population_id = "either"),
#'   cohort = my_cohort_handle,   # cohort handle / table name as scope
#'   tables = "my_inclusion_set", # workspace omop.table symbol name
#'   combine = "intersect"
#' )
#' recipe_execute(recipe2)
#' }
#' @seealso \code{\link{omop_variable}}, \code{\link{omop_filter}},
#'   \code{\link{omop_output}}, \code{\link{omop_population}},
#'   \code{\link{omop_variable_block}}, \code{\link{recipe_preview}},
#'   \code{\link{recipe_execute}}, \code{\link{recipe_save}},
#'   \code{\link{recipe_to_plan}}
#' @export
omop_recipe <- function(variables = NULL,
                        filters = NULL,
                        outputs = NULL,
                        populations = NULL,
                        blocks = NULL,
                        cohort = NULL,
                        tables = NULL,
                        combine = "union",
                        options = NULL) {
  recipe <- .omop_recipe_empty()

  # Delegate to the internal slot-filling setters in dependency order so the
  # declarative form is identical by construction to step-by-step building.
  for (p in .recipe_arg_list(populations)) {
    recipe <- recipe_add_population(recipe, p)
  }
  for (b in .recipe_arg_list(blocks)) {
    recipe <- recipe_add_block(recipe, b)
  }
  for (v in .recipe_arg_list(variables)) {
    recipe <- recipe_add_variable(recipe, v)
  }
  filter_items <- .recipe_arg_list(filters)
  filter_ids <- names(filter_items)
  for (i in seq_along(filter_items)) {
    fid <- if (!is.null(filter_ids) && nzchar(filter_ids[i]))
      filter_ids[i] else NULL
    recipe <- recipe_add_filter(recipe, filter_items[[i]], id = fid)
  }
  for (o in .recipe_arg_list(outputs)) {
    recipe <- recipe_add_output(recipe, o)
  }
  # `cohort` is dual-purpose by type: a scalar cohort_definition_id keeps the
  # legacy meaning (base population cohort), while a cohort handle / table-name
  # ref is a recipe-level scope. `tables` symbols are always scope. Scope (when
  # any) is folded by `combine` and intersected into every population server-side.
  scope_cohort <- NULL
  if (!is.null(cohort)) {
    if (is.numeric(cohort) && !inherits(cohort, "dsomop_cohort_handle")) {
      recipe <- recipe_set_cohort(recipe, cohort)
    } else {
      scope_cohort <- cohort
    }
  }
  recipe <- recipe_set_scope(recipe, cohort = scope_cohort, tables = tables,
                             combine = combine)
  if (!is.null(options)) {
    recipe <- recipe_set_options(recipe,
      translate_concepts = options$translate_concepts,
      block_sensitive    = options$block_sensitive,
      factor_concepts    = options$factor_concepts)
  }

  recipe
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
#' @seealso \code{\link{omop_population}}, \code{\link{omop_recipe}}
#' @keywords internal
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
  # Validate set-op members are already declared (so populations() is assembled
  # in dependency order, exactly like parents). A member may itself be a set-op
  # population, which lets set operations nest.
  if (!is.null(population$setop)) {
    missing <- setdiff(population$setop$members, names(recipe$populations))
    if (length(missing) > 0) {
      stop("Set-op population '", population$id, "' references unknown member",
           if (length(missing) > 1) "s" else "", " '",
           paste(missing, collapse = "', '"),
           "'; declare member populations before the set-op that combines them.",
           call. = FALSE)
    }
  }
  recipe$populations[[population$id]] <- population
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Set the base cohort definition for a recipe
#'
#' Updates the base population to reference an existing OMOP cohort definition.
#' During \code{\link{recipe_to_plan}}, this is compiled to
#' \code{\link{ds.omop.plan.cohort}} with \code{cohort_definition_id}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param cohort_definition_id Integer or \code{NULL}; cohort definition ID to
#'   use as the recipe base population. Use \code{NULL} to clear the reference.
#' @return The modified \code{omop_recipe} object.
#' @seealso \code{\link{omop_recipe}}, \code{\link{recipe_to_plan}},
#'   \code{\link{ds.omop.plan.cohort}}
#' @keywords internal
recipe_set_cohort <- function(recipe, cohort_definition_id) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  recipe$populations$base$cohort_definition_id <-
    if (!is.null(cohort_definition_id)) as.integer(cohort_definition_id) else NULL
  recipe$meta$modified <- Sys.time()
  recipe
}

#' Set the recipe-level population scope
#'
#' Records a recipe-wide scope that the server folds into ONE cohort and
#' intersects into every population before extraction. The scope mixes an
#' optional cohort reference (a \code{dsomop_cohort_handle}, a
#' \code{cohort_definition_id}, or a server-side cohort table name) with zero or
#' more workspace \code{omop.table} symbol \emph{names}; the sources are combined
#' on the person key by \code{combine} (\code{"union"}/\code{"intersect"}). The
#' cohort reference is normalised with the shared \code{\link{.cohort_scope_arg}}
#' resolver (the same one the exploration / analysis wrappers use) so the server
#' receives the value its \code{.resolveCohortArg} expects; table symbol names
#' travel as strings for the server to resolve to frames (matching the
#' \code{ds.omop.analysis.run} scope contract). Stored on \code{recipe$scope}
#' and serialized to \code{plan$scope} by \code{\link{recipe_to_plan}}. With no
#' cohort and no tables the scope is cleared (\code{recipe$scope <- NULL}), so a
#' plain recipe carries no scope and is byte-identical to one built without it.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param cohort Cohort reference or \code{NULL}.
#' @param tables Character vector of \code{omop.table} symbol names, or
#'   \code{NULL}.
#' @param combine Character; \code{"union"} (default) or \code{"intersect"}.
#' @return The modified \code{omop_recipe} object.
#' @seealso \code{\link{omop_recipe}}, \code{\link{recipe_execute}},
#'   \code{\link{recipe_to_plan}}, \code{\link{.cohort_scope_arg}}
#' @keywords internal
recipe_set_scope <- function(recipe, cohort = NULL, tables = NULL,
                             combine = "union") {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  combine <- match.arg(combine, c("union", "intersect"))

  if (!is.null(tables)) {
    if (!is.character(tables)) {
      stop("tables must be the name(s) of server-side omop.table symbol(s).",
           call. = FALSE)
    }
    tables <- tables[nzchar(tables)]
    if (length(tables) == 0) tables <- NULL
  }

  cohort_val <- .cohort_scope_arg(cohort)

  # No scope at all -> leave the slot empty so an unscoped recipe stays compact
  # and round-trips byte-for-byte.
  if (is.null(cohort_val) && is.null(tables)) {
    recipe$scope <- NULL
    recipe$meta$modified <- Sys.time()
    return(recipe)
  }

  recipe$scope <- list(
    cohort  = cohort_val,
    tables  = tables,
    combine = combine
  )
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
#' @seealso \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_variable_block}}, \code{\link{omop_recipe}}
#' @keywords internal
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
    # Provenance marker so recipe_to_code() can skip block-derived variables
    # without guessing by name.
    v$block_id <- block$id
    if (isTRUE(block$expand)) v$expand <- TRUE
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
#' @seealso \code{\link{omop_variable}}, \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_filter}}, \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_output}}, \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_recipe}}
#' @keywords internal
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
#' @seealso \code{\link{omop_recipe}}
#' @keywords internal
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

# Build the raw-column spec for one table's variables as a NAMED list: names
# are the per-variable aliases (`v$name`), values are the OMOP source columns.
# The server renames source -> alias, so this is what carries raw-column naming
# through the recipe/YAML path (the fluent API carries it via named
# `tables=`/`columns=` vectors). A named list (not a named atomic vector) is
# used so the aliases survive jsonlite serialisation as a JSON object rather
# than being dropped to a bare array (see `.fill_alias_names`). Every element is
# named; `value_source` columns keep their own name. De-duplicated by source
# column (first alias wins).
.raw_table_columns <- function(vs) {
  src <- character(0)
  alias <- character(0)
  for (v in vs) {
    if (!is.null(v$column)) {
      src <- c(src, v$column)
      alias <- c(alias, v$name %||% v$column)
    }
    if (!is.null(v$value_source)) {
      src <- c(src, v$value_source)
      alias <- c(alias, v$value_source)
    }
  }
  if (length(src) == 0) return(NULL)
  keep <- !duplicated(src)
  as.list(stats::setNames(src[keep], alias[keep]))
}

#' Serialize one population for the server's multi-population plan
#'
#' Each \code{\link{omop_population}} compiles to a transport-safe spec the
#' server materializes and gates independently. A set-op population carries its
#' \code{list(op, members)} verbatim (the server folds the named members with the
#' matching cohort algebra). A criteria population compiles its person-level
#' filter chain to the SAME nested AND/OR \code{filter_tree} the base cohort uses
#' (via \code{\link{.compile_population_filter_tree}}), so the server reuses its
#' existing \code{.buildCohortFromFilters} path, and carries any
#' \code{cohort_definition_id}. \code{filter_tree}/\code{cohort_definition_id}
#' are included only when set so a bare population stays compact.
#'
#' @param pop An \code{omop_population} object.
#' @return A named list spec with \code{id}, \code{label}, \code{kind}
#'   (\code{"setop"} or \code{"criteria"}), and the kind-specific fields.
#' @keywords internal
.compile_population_spec <- function(pop) {
  if (!is.null(pop$setop)) {
    return(list(
      id    = pop$id,
      label = pop$label,
      kind  = "setop",
      setop = list(op = pop$setop$op,
                   members = as.character(pop$setop$members))
    ))
  }

  spec <- list(id = pop$id, label = pop$label, kind = "criteria")
  pop_filters <- .extract_filters_by_level(pop$filters %||% list(), "population")
  filter_tree <- .compile_population_filter_tree(pop_filters)
  if (!is.null(filter_tree)) spec$filter_tree <- filter_tree
  if (!is.null(pop$cohort_definition_id))
    spec$cohort_definition_id <- as.integer(pop$cohort_definition_id)
  spec
}

#' Resolve the population a (possibly table-split) plan output belongs to
#'
#' A "long"/features output spanning multiple tables is split into
#' \code{<name>_<table>} children in the plan. To recover an output's
#' \code{population_id} we match the plan output name to a recipe output: a
#' direct hit wins; otherwise the longest recipe-output name it is prefixed by
#' (\code{<recipe_name>_...}) is its parent. Falls back to \code{"base"} when no
#' recipe output matches (defensive; should not happen for plan-built outputs).
#'
#' @param plan_out_name Character; a name in \code{plan$outputs}.
#' @param recipe_outputs Named list of \code{omop_output} objects.
#' @param recipe_out_names Character vector; \code{names(recipe_outputs)}.
#' @return Character; the target population ID.
#' @keywords internal
.recipe_output_population <- function(plan_out_name, recipe_outputs,
                                      recipe_out_names) {
  o <- recipe_outputs[[plan_out_name]]
  if (!is.null(o)) return(o$population_id %||% "base")
  # Split output: pick the longest matching <recipe_name>_ prefix so outputs
  # whose names share a prefix are disambiguated deterministically.
  hits <- recipe_out_names[vapply(recipe_out_names, function(rn) {
    startsWith(plan_out_name, paste0(rn, "_"))
  }, logical(1))]
  if (length(hits) > 0) {
    parent <- hits[which.max(nchar(hits))]
    return(recipe_outputs[[parent]]$population_id %||% "base")
  }
  "base"
}

#' Convert a recipe to an extraction plan
#'
#' Compiles the recipe into an \code{omop_plan} suitable for server-side
#' execution via \code{\link{ds.omop.plan.execute}}. The conversion maps
#' population-level filters to cohort specifications, groups variables by
#' output and table, selects the appropriate plan builder (person_level,
#' features, events, survival, intervals) for each output type, and attaches
#' row-level filter trees.
#'
#' Multiple populations and recipe-level scope are both serialized into the
#' plan for the server to execute:
#' \itemize{
#'   \item \code{plan$populations} carries every recipe population. A
#'     criteria population serializes as
#'     \code{list(id, label, kind = "criteria", filter_tree, cohort_definition_id)};
#'     a set-op population as
#'     \code{list(id, label, kind = "setop", setop = list(op, members))}. The
#'     base population is always included so its cohort drives single-population
#'     recipes exactly as before.
#'   \item every \code{plan$outputs[[name]]} carries the \code{population_id} it
#'     was authored against (default \code{"base"}), so the server materializes
#'     and gates each output against the right population.
#'   \item \code{plan$scope} carries the recipe-level scope
#'     (\code{list(cohort, tables, combine)}) the server folds and intersects
#'     into every population. It is omitted when no scope was set.
#' }
#'
#' Recipes are the recommended interface for ordinary analysis code. Plans are
#' retained as an explicit lower-level contract so advanced users, tests, and
#' the server can inspect the exact payload before it is executed.
#'
#' @param recipe An \code{omop_recipe} object.
#' @return An \code{omop_plan} object ready for execution.
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(
#'   blocks = omop_variable_block(
#'     table = "condition_occurrence",
#'     concept_ids = c(201820), format = "binary"),
#'   outputs = omop_output(type = "wide"))
#' recipe_execute(recipe)
#' plan <- recipe_to_plan(recipe)  # advanced: inspect the server payload
#' }
#' @seealso \code{\link{recipe_execute}}, \code{\link{omop_recipe}},
#'   \code{\link{ds.omop.plan}}
#' @export
recipe_to_plan <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  # Every output / block must target a population that the recipe declares, so
  # the server never silently runs an output against the wrong (or a missing)
  # cohort. Fail-closed on a dangling reference before anything is compiled.
  declared_pops <- names(recipe$populations)
  out_pops <- vapply(recipe$outputs, function(o) o$population_id %||% "base",
                     character(1))
  block_pops <- vapply(recipe$blocks, function(b) b$population_id %||% "base",
                       character(1))
  unknown_pops <- setdiff(unique(c(out_pops, block_pops)), declared_pops)
  if (length(unknown_pops) > 0) {
    stop("Output/block targets undeclared population",
         if (length(unknown_pops) > 1) "s" else "", " '",
         paste(unknown_pops, collapse = "', '"),
         "'; add it via omop_recipe(populations = ...) or omop_population().",
         call. = FALSE)
  }

  plan <- ds.omop.plan()

  # Apply recipe-level plan options (single source of truth). Read defensively
  # with `%||%` so recipes created before the `options` slot existed (or
  # imported from older JSON) degrade to the constructor defaults.
  opts <- recipe$options %||% list()
  plan <- ds.omop.plan.options(
    plan,
    translate_concepts = opts$translate_concepts,
    block_sensitive    = opts$block_sensitive,
    factor_concepts    = opts$factor_concepts
  )

  # Fail closed at compile time when a variable's value_source / raw column is a
  # known free-text / source-value column the server can never release (e.g.
  # value_as_string, *_source_value, sig). Silently dropping it server-side would
  # yield a person-id-only frame with no signal; an explicit request must error
  # here, before any DataSHIELD round-trip.
  .assert_no_blocked_value_sources(recipe$variables)

  # Build cohort from base population + population-level filters
  base_pop <- recipe$populations[["base"]]
  if (!is.null(base_pop$cohort_definition_id)) {
    plan <- ds.omop.plan.cohort(plan,
      cohort_definition_id = base_pop$cohort_definition_id)
  }

  # Collect all population-level filters. `spec` stays as the legacy flat
  # AND list for older servers; `filter_tree` preserves nested AND/OR groups
  # for servers that support the richer cohort filter DSL.
  pop_filter_items <- .collect_pop_filter_items(recipe)
  pop_filters <- .flatten_filters(pop_filter_items, level = "population")
  if (length(pop_filters) > 0) {
    spec <- lapply(pop_filters, function(f) {
      list(type = f$type, params = f$params)
    })
    filter_tree <- .compile_population_filter_tree(pop_filter_items)
    if (is.null(plan$cohort)) {
      plan$cohort <- list(type = "spec", spec = spec)
    } else {
      plan$cohort$spec <- spec
    }
    if (!is.null(filter_tree)) plan$cohort$filter_tree <- filter_tree
  }

  # Serialize EVERY population (base + criteria + set-op) for the server to
  # materialize and gate independently. The base cohort above is kept as-is so
  # single-population recipes execute through the unchanged plan$cohort path;
  # plan$populations is the multi-population contract layered on top.
  plan$populations <- lapply(recipe$populations, .compile_population_spec)
  names(plan$populations) <- names(recipe$populations)

  # Recipe-level scope (cohort and/or omop.table symbol NAMES, folded by
  # combine): the server folds it into one cohort and intersects it into every
  # population. `tables` stays a character vector here because omop.table SYMBOLS
  # cannot ride in the plan JSON — ds.omop.plan.execute() splices them into the
  # DataSHIELD call by name (via .analysis_scope_expr, the ds.omop.analysis.run
  # scope contract), so the server resolves them to frames; only the cohort
  # literal and combine travel as data.
  if (!is.null(recipe$scope)) plan$scope <- recipe$scope

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
        tables_spec <- lapply(by_table, .raw_table_columns)
        plan <- ds.omop.plan.baseline(plan,
          columns = tables_spec[["person"]] %||% c("gender_concept_id",
            "year_of_birth", "race_concept_id"),
          name = out_name)
      } else {
        # Separate person-derived variables from event/raw variables
        person_derived_fmts <- c("age", "sex_mf", "obs_duration",
                                       "prior_obs", "followup",
                                       "demo_missingness",
                                       "charlson", "chads2", "chadsvasc",
                                       "dcsi", "hfrs")
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
            # Per-variable row filters now ride on each feature spec (per-spec
            # scoping in .build_feature_specs), so the server applies them inside
            # .toFeatures per column. They are intentionally NOT also merged into
            # the output's custom slot, which would AND mutually-exclusive slices
            # into a contradiction and zero the rows.
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
                  concept_set = .concept_set_arg(feat_vs, concept_ids),
                  features = specs
                )
                # Per-variable row filters ride on each feature spec (per-spec
                # scoping), applied inside .toFeatures per column. Not merged into
                # the table's $filters slot, which would AND mutually-exclusive
                # slices into a contradiction before aggregation.
              } else {
                tables_spec[[tbl]] <- .raw_table_columns(vs)
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
          tables_spec <- lapply(ev_by_table, .raw_table_columns)
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

        # Forward per-variable row filters (value_bin / date_range / ...) as the
        # server custom filter DSL, and per-variable index-relative time windows
        # as a temporal spec. Output-level temporal wins; the variable window
        # only supplies index_window when the output sets none.
        var_filter <- .variables_custom_filter(vs)
        temporal <- out$options$temporal %||% .variables_time_window(vs)
        # Visit-linkage + concept-scope come off the variables (first set wins);
        # unit/type filtering is expressed as row filters on the relevant
        # *_concept_id column and travels through var_filter above.
        visit_filter <- .first_non_null(lapply(vs, function(v) v$visit_filter))
        concept_col  <- .first_non_null(lapply(vs, function(v) v$concept_col))

        nm <- if (length(by_table) > 1) paste0(out_name, "_", tbl)
              else out_name
        plan <- ds.omop.plan.events(
          plan, name = nm, table = tbl,
          columns = if (length(columns) > 0) columns else NULL,
          concept_set = .concept_set_arg(vs, concept_ids),
          temporal = temporal,
          date_handling = out$options$date_handling,
          filters = var_filter,
          visit_filter = visit_filter,
          concept_col = concept_col
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
        # Per-variable row filters ride on each feature spec (per-spec scoping
        # in .build_feature_specs); the server applies them inside .toFeatures
        # per column rather than ANDing them all into one output-level WHERE.
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
      # Per-variable row filters narrow which outcome events qualify (e.g. a
      # value range on the outcome). Route them into the output's custom slot;
      # the server ANDs them into the outcome-event SELECT before time-to-event.
      plan <- .apply_var_filters(plan, out_name, vars)
    } else if (out$type == "intervals") {
      tables <- names(by_table)
      plan <- ds.omop.plan.intervals(plan, tables = tables,
                                      name = out_name)
    }
  }

  # Apply recipe-level row filters to outputs (preserving AND/OR tree). These go
  # into output$filters$custom — the slot the server actually executes (see
  # dsOMOP .planExecute / .compileSelect). Per-variable filters wired into the
  # event paths above are ANDed with this recipe-level tree so neither is lost.
  row_filter_items <- .extract_filters_by_level(recipe$filters, "row")
  if (length(row_filter_items) > 0) {
    filter_tree <- .compile_filter_tree(row_filter_items, level = "row")
    for (out_name in names(plan$outputs)) {
      existing <- plan$outputs[[out_name]]$filters$custom
      plan$outputs[[out_name]]$filters$custom <-
        if (is.null(existing)) filter_tree
        else list(and = list(existing, filter_tree))
    }
  }

  # Stamp each plan output with the population it targets so the server runs (and
  # gates) it against the right cohort. A "long"/features output that spanned
  # several tables was split into <name>_<table> children; map those back to the
  # parent recipe output to recover its population_id (same logic as
  # recipe_execute / .resolve_plan_out use for symbol naming).
  recipe_out_names <- names(recipe$outputs)
  for (plan_out_name in names(plan$outputs)) {
    pid <- .recipe_output_population(plan_out_name, recipe$outputs,
                                     recipe_out_names)
    plan$outputs[[plan_out_name]]$population_id <- pid
  }

  plan
}

#' Set plan-wide options on a recipe
#'
#' Stores global execution options on the recipe itself, the single source of
#' truth. These options are applied to the compiled plan by
#' \code{\link{recipe_to_plan}} and therefore reach every downstream path
#' (\code{\link{recipe_execute}}, \code{\link{recipe_preview}},
#' \code{\link{recipe_validate}}). Only non-NULL arguments are updated;
#' existing option values are preserved for omitted arguments. Mirrors
#' \code{\link{ds.omop.plan.options}}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param translate_concepts Logical; if \code{TRUE}, concept ID columns are
#'   translated to human-readable concept names in output tables.
#' @param block_sensitive Logical; if \code{TRUE}, sensitive columns
#'   (e.g. exact dates, free-text notes) are excluded from outputs.
#' @param factor_concepts Logical; if \code{TRUE}, after a memory-mode
#'   execution every \code{_concept_id} column is converted to a factor whose
#'   levels are harmonized across all connected servers.
#' @return The modified \code{omop_recipe} with updated options.
#' @seealso \code{\link{omop_recipe}}, \code{\link{recipe_to_plan}},
#'   \code{\link{ds.omop.plan.options}}
#' @keywords internal
recipe_set_options <- function(recipe,
                               translate_concepts = NULL,
                               block_sensitive = NULL,
                               factor_concepts = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  if (is.null(recipe$options)) recipe$options <- list()
  if (!is.null(translate_concepts))
    recipe$options$translate_concepts <- translate_concepts
  if (!is.null(block_sensitive))
    recipe$options$block_sensitive <- block_sensitive
  if (!is.null(factor_concepts))
    recipe$options$factor_concepts <- factor_concepts
  recipe
}

#' Build a concept_set argument, expanding descendants when requested
#'
#' Returns a bare integer vector by default, or a concept-set spec of the form
#' \code{list(concepts = ids, include_descendants = TRUE)} when any contributing
#' variable carries \code{$expand = TRUE}. The server expands the spec via
#' \code{.vocabExpandConceptSet} at execution time.
#'
#' @param vs List of \code{omop_variable} objects contributing the concepts.
#' @param ids Integer vector of concept IDs.
#' @return Either an integer vector, a concept-set spec list, or \code{NULL}.
#' @keywords internal
.concept_set_arg <- function(vs, ids) {
  ids <- as.integer(ids[!is.na(ids)])
  if (length(ids) == 0) return(NULL)
  if (any(vapply(vs, function(v) isTRUE(v$expand), logical(1)))) {
    return(list(concepts = ids, include_descendants = TRUE))
  }
  ids
}

#' Is a column name a server-blocked free-text / source-value column?
#'
#' Mirrors the server's blocked-column patterns (dsOMOP blueprint:
#' \code{*_source_value}, \code{value_as_string}, \code{value_source_value},
#' \code{sig}, \code{*_source_concept_value}, note text). These are never
#' releasable, so a recipe that requests one as a variable's \code{value_source}
#' or raw \code{column} is rejected at compile time rather than silently dropped.
#'
#' @param col Character; a column name (case-insensitive).
#' @return Logical scalar.
#' @keywords internal
.is_blocked_column <- function(col) {
  if (is.null(col) || !nzchar(col)) return(FALSE)
  col <- tolower(col)
  patterns <- c("_source_value$", "^value_as_string$", "^value_source_value$",
                "^sig$", "^note_text$", "^note_source_value$")
  any(vapply(patterns, function(p) grepl(p, col), logical(1)))
}

#' Reject variables whose value_source / raw column is a blocked column
#'
#' @param variables Named list of \code{omop_variable} objects.
#' @return Invisibly TRUE, or stops with a disclosure error.
#' @keywords internal
.assert_no_blocked_value_sources <- function(variables) {
  for (nm in names(variables %||% list())) {
    v <- variables[[nm]]
    for (col in c(v$value_source, v$column)) {
      if (.is_blocked_column(col)) {
        stop("Disclosive: variable '", nm %||% v$name %||% "?",
             "' requests blocked free-text/source column '", col,
             "', which the server cannot release.", call. = FALSE)
      }
    }
  }
  invisible(TRUE)
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
      sd            = omop.feature.sd_value,
      cv            = omop.feature.cv_value,
      slope         = omop.feature.slope_value,
      abnormal_high = omop.feature.abnormal_high,
      abnormal_low  = omop.feature.abnormal_low,
      gap_max       = omop.feature.gap_max_days,
      gap_mean      = omop.feature.gap_mean_days,
      duration_sum  = omop.feature.duration_sum,
      omop.feature.boolean
    )
    args <- list(
      name = v$name,
      concept_set = if (isTRUE(v$expand) && !is.null(v$concept_id))
        list(concepts = as.integer(v$concept_id), include_descendants = TRUE)
      else if (!is.null(v$concept_id)) v$concept_id
      else integer(0)
    )
    # Pass value_column for feature types that support it
    if (!is.null(v$value_source) && fmt %in% c("mean", "min", "max",
        "first_value", "last_value", "sum", "sd", "cv", "slope")) {
      args$value_column <- v$value_source
    }
    # Pass agg for drug_duration
    if (fmt == "drug_duration" && !is.null(v$derived$agg)) {
      args$agg <- v$derived$agg
    }
    spec <- do.call(feat_fn, args)
    # Attach this variable's OWN row filter to its spec (per-spec scoping) so the
    # server applies it only when computing THIS feature. Multiple variables on
    # one table with mutually-exclusive unit/type slices (e.g. HbA1c in % vs
    # mmol/mol) must not have their filters AND-merged into one contradictory
    # WHERE — each slice is its own column over its own rows.
    vf <- .variables_custom_filter(list(v))
    if (!is.null(vf)) spec$filter <- vf
    # A per-variable concept_col override (e.g. scoping by unit_concept_id)
    # likewise belongs to this spec alone.
    if (!is.null(v$concept_col)) spec$concept_col <- v$concept_col
    spec
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
  filters <- .flatten_filters(recipe$filters, level = "population")
  base_filters <- recipe$populations$base$filters %||% list()
  c(filters, .flatten_filters(base_filters, level = "population"))
}

#' Collect population-level filter items while preserving groups
#'
#' @param recipe An \code{omop_recipe} object.
#' @return List of matching \code{omop_filter} or \code{omop_filter_group}
#'   objects.
#' @keywords internal
.collect_pop_filter_items <- function(recipe) {
  filters <- .extract_filters_by_level(recipe$filters, "population")
  base_filters <- recipe$populations$base$filters %||% list()
  c(filters, .extract_filters_by_level(base_filters, "population"))
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
#' @param level Character or \code{NULL}; if provided, filters at other levels
#'   are skipped while preserving valid descendants inside groups.
#' @return A nested list structure representing the filter tree, or \code{NULL}
#'   if empty.
#' @keywords internal
.compile_filter_tree <- function(filters, default_operator = "and", level = NULL) {
  if (length(filters) == 0) return(NULL)
  children <- lapply(filters, .compile_filter_node, level = level)
  children <- Filter(Negate(is.null), children)
  if (length(children) == 0) return(NULL)
  if (length(children) == 1) return(children[[1]])
  setNames(list(children), default_operator)
}

#' Compile a single filter or filter group into the server filter DSL
#'
#' @param f An \code{omop_filter} or \code{omop_filter_group} object.
#' @param level Character or \code{NULL}; if provided, filters at other levels
#'   are skipped.
#' @return A nested list node for the filter tree, or \code{NULL}.
#' @keywords internal
.compile_filter_node <- function(f, level = NULL) {
  if (inherits(f, "omop_filter_group")) {
    op <- tolower(f$operator)
    children <- lapply(f$children, .compile_filter_node, level = level)
    children <- Filter(Negate(is.null), children)
    if (length(children) == 0) return(NULL)
    if (length(children) == 1) return(children[[1]])
    return(setNames(list(children), op))
  }
  if (inherits(f, "omop_filter")) {
    if (!is.null(level) && f$level != level) return(NULL)
    return(.filter_to_leaf(f))
  }
  NULL
}

#' Build a custom filter tree from a set of variables' row-level filters
#'
#' Collects every \code{"row"}-level \code{omop_filter} attached to the given
#' variables (\code{v$filters}) and compiles them into a single AND/OR tree in
#' the server's \code{.compileFilter()} DSL. This is what carries per-variable
#' row filters (e.g. \code{value_bin}, \code{date_range}) from the recipe into
#' the plan's \code{output$filters$custom}, where the server validates them
#' fail-closed and ANDs them into the extraction WHERE.
#'
#' @param vars List of \code{omop_variable} objects (typically one table's).
#' @return A filter-tree list, or \code{NULL} if no row filters are present.
#' @keywords internal
.variables_custom_filter <- function(vars) {
  row_filters <- list()
  for (v in vars) {
    vf <- v$filters %||% list()
    row_filters <- c(row_filters,
                     .extract_filters_by_level(vf, "row"))
  }
  if (length(row_filters) == 0) return(NULL)
  .compile_filter_tree(row_filters, level = "row")
}

#' AND a set of variables' row filters into an output's custom filter slot
#'
#' Routes per-variable \code{"row"}-level filters into
#' \code{plan$outputs[[name]]$filters$custom} — the nested AND/OR slot the
#' server consumes (see dsOMOP \code{.planExecute} / \code{.compileSelect}).
#' Used by the non-long output branches (features / wide / covariates_sparse /
#' survival), which build outputs via dedicated plan helpers that don't take a
#' \code{filters} argument, so the variable filters would otherwise be dropped
#' and the variable computed over all rows. Any existing \code{custom} tree
#' (e.g. a recipe-level row filter) is preserved by ANDing, exactly as the
#' recipe-level merge in \code{recipe_to_plan} does. A no-op when the variables
#' carry no row filters or the output does not exist.
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; the output key to update.
#' @param vars List of \code{omop_variable} objects feeding that output.
#' @return The modified \code{omop_plan}.
#' @keywords internal
.apply_var_filters <- function(plan, name, vars) {
  if (is.null(plan$outputs[[name]])) return(plan)
  var_filter <- .variables_custom_filter(vars)
  if (is.null(var_filter)) return(plan)
  existing <- plan$outputs[[name]]$filters$custom
  plan$outputs[[name]]$filters$custom <-
    if (is.null(existing)) var_filter
    else list(and = list(existing, var_filter))
  plan
}

#' Derive an index-relative temporal spec from variables' time windows
#'
#' Variable \code{time_window}s are index-relative day offsets
#' (\code{list(start=, end=)}). A single \code{long} output is one event stream,
#' so when several variables carry windows we take their UNION (the minimum start
#' and maximum end) as the output's \code{index_window}, rather than letting the
#' first variable silently win and drop the others' events. This guarantees every
#' windowed variable's events fall inside the extracted span; per-variable
#' narrowing (e.g. distinct peri-index columns) is the job of the features/wide
#' path, which scopes each spec independently. Returns \code{NULL} when no
#' variable sets a window.
#'
#' @param vars List of \code{omop_variable} objects.
#' @return A temporal spec list with \code{index_window}, or \code{NULL}.
#' @keywords internal
.variables_time_window <- function(vars) {
  starts <- c(); ends <- c(); any_window <- FALSE
  for (v in vars) {
    tw <- v$time_window
    if (!is.null(tw) && (!is.null(tw$start) || !is.null(tw$end))) {
      any_window <- TRUE
      if (!is.null(tw$start)) starts <- c(starts, as.numeric(tw$start))
      if (!is.null(tw$end))   ends   <- c(ends, as.numeric(tw$end))
    }
  }
  if (!any_window) return(NULL)
  list(index_window = list(
    start = if (length(starts) > 0) min(starts) else NULL,
    end   = if (length(ends) > 0) max(ends) else NULL))
}

.compile_population_filter_tree <- function(filters, default_operator = "and") {
  if (length(filters) == 0) return(NULL)
  children <- lapply(filters, .compile_population_filter_node)
  children <- Filter(Negate(is.null), children)
  if (length(children) == 0) return(NULL)
  if (length(children) == 1) return(children[[1]])
  setNames(list(children), default_operator)
}

.compile_population_filter_node <- function(f) {
  if (inherits(f, "omop_filter_group")) {
    op <- tolower(f$operator)
    children <- lapply(f$children, .compile_population_filter_node)
    children <- Filter(Negate(is.null), children)
    if (length(children) == 0) return(NULL)
    if (length(children) == 1) return(children[[1]])
    return(setNames(list(children), op))
  }
  if (inherits(f, "omop_filter") && f$level == "population") {
    return(list(type = f$type, params = f$params))
  }
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
      op = "in", value = f$params$concept_id %||% f$params$concept_ids),
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

#' Format an R value for recipe code generation
#'
#' Deparse-based replacement for \code{.format_r_value()}: \code{deparse()}
#' escapes quotes/backslashes in strings correctly, keeps the \code{L} suffix
#' on integers, and emits valid literals for (nested) named lists.
#'
#' @param x An R value, or a string wrapped with \code{.codegen_raw()}.
#' @return Character string of valid R code.
#' @keywords internal
.codegen_value <- function(x) {
  if (is.null(x)) return("NULL")
  if (inherits(x, "codegen_raw")) return(as.character(x))
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}

#' Mark a string as already-generated R code
#'
#' Wraps a code string so \code{.codegen_call()} injects it verbatim instead
#' of deparsing it (used for nested constructor calls such as filter lists).
#'
#' @param code Character; R code to inject verbatim.
#' @return The string with class \code{"codegen_raw"}.
#' @keywords internal
.codegen_raw <- function(code) structure(code, class = "codegen_raw")

#' Wrap constructor-call code strings as a declarative slot argument
#'
#' Used by \code{\link{recipe_to_code}} to pass each \code{omop_recipe()} slot
#' (populations, blocks, variables, outputs) a single nested expression. A lone
#' element is emitted bare (e.g. \code{omop_output(...)}); multiple elements are
#' wrapped in \code{list(...)}. Returns a \code{.codegen_raw()} value so the
#' enclosing \code{.codegen_call()} injects it verbatim.
#'
#' @param code_strings Character vector of constructor-call code strings.
#' @return A \code{.codegen_raw()} string, or \code{NULL} when empty.
#' @keywords internal
.codegen_list_arg <- function(code_strings) {
  if (length(code_strings) == 0) return(NULL)
  if (length(code_strings) == 1) return(.codegen_raw(code_strings))
  .codegen_raw(paste0("list(", paste(code_strings, collapse = ", "), ")"))
}

#' Quote a list element name for code generation when needed
#'
#' Returns the name unchanged if it is a syntactic R name, otherwise wraps it
#' in backticks so \code{list(<name> = ...)} stays valid (filter IDs such as
#' \code{"f1_sex"} are syntactic; defensive for arbitrary IDs).
#'
#' @param nm Character; the element name.
#' @return Character; a safe name token.
#' @keywords internal
.codegen_name <- function(nm) {
  if (identical(nm, make.names(nm))) nm else paste0("`", nm, "`")
}

#' Build an R call string with deparse-based argument formatting
#'
#' Like \code{.build_code()} but escapes strings safely via
#' \code{.codegen_value()} and supports verbatim arguments created with
#' \code{.codegen_raw()}. \code{NULL} arguments are dropped.
#'
#' @param fn_name Character; fully qualified function name.
#' @param ... Named arguments to include in the call.
#' @return Character string of the R call.
#' @keywords internal
.codegen_call <- function(fn_name, ...) {
  args <- list(...)
  parts <- vapply(names(args), function(nm) {
    val <- args[[nm]]
    if (is.null(val)) return(NA_character_)
    paste0(nm, " = ", .codegen_value(val))
  }, character(1))
  parts <- parts[!is.na(parts)]
  paste0(fn_name, "(", paste(parts, collapse = ", "), ")")
}

#' Generate a verbatim `filters = list(...)` argument value
#'
#' @param filters List of \code{omop_filter}/\code{omop_filter_group} objects.
#' @return A \code{.codegen_raw()} string, or \code{NULL} when empty (so
#'   \code{.codegen_call()} drops the argument).
#' @keywords internal
.codegen_filter_list <- function(filters) {
  if (is.null(filters) || length(filters) == 0) return(NULL)
  code <- vapply(filters, function(f) {
    if (inherits(f, "omop_filter_group")) .codegen_filter_group(f)
    else .codegen_filter(f)
  }, character(1))
  .codegen_raw(paste0("list(", paste(code, collapse = ", "), ")"))
}

#' Generate reproducible R code from a recipe
#'
#' Produces a minimal R script that, when executed, recreates the recipe from
#' scratch. The output is a single declarative \code{\link{omop_recipe}(...)}
#' expression whose arguments are nested calls to the leaf constructors
#' (\code{omop_variable*}, \code{omop_filter*}, \code{omop_output},
#' \code{omop_population}, \code{omop_variable_block}). Does not include
#' \code{library()} calls or header comments.
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

  # Each named omop_recipe() slot is built as a verbatim code string and passed
  # to a single top-level omop_recipe(...) call, mirroring the declarative
  # authoring form (no step-by-step recipe_add_*/recipe_set_* calls).
  slot_args <- list()

  # Populations (skip the implicit base).
  pop_code <- character(0)
  for (pid in names(recipe$populations)) {
    if (pid == "base") next
    p <- recipe$populations[[pid]]
    if (!is.null(p$setop)) {
      # Set-op population: emit the matching named set arg (union/intersect/
      # setdiff = c(members)), never filters/cohort_definition_id.
      pop_args <- list("omop_population",
                       id = p$id, label = p$label, parent_id = p$parent_id)
      pop_args[[p$setop$op]] <- as.character(p$setop$members)
      pop_code <- c(pop_code, do.call(.codegen_call, pop_args))
    } else {
      pop_code <- c(pop_code, .codegen_call("omop_population",
        id = p$id, label = p$label, parent_id = p$parent_id,
        filters = .codegen_filter_list(p$filters),
        cohort_definition_id = p$cohort_definition_id))
    }
  }
  if (length(pop_code) > 0)
    slot_args$populations <- .codegen_list_arg(pop_code)

  # Blocks (re-expanded by omop_recipe() into variables at run time).
  block_code <- character(0)
  for (bid in names(recipe$blocks)) {
    b <- recipe$blocks[[bid]]
    block_code <- c(block_code, .codegen_call("omop_variable_block",
      id = b$id, table = b$table,
      concept_ids = b$concept_ids,
      concept_names = b$concept_names,
      time_window = b$time_window,
      format = b$format,
      value_source = b$value_source,
      suffix_mode = if (!identical(b$suffix_mode %||% "index", "index"))
        b$suffix_mode else NULL,
      filters = .codegen_filter_list(b$filters),
      population_id = b$population_id,
      expand = if (isTRUE(b$expand)) TRUE else NULL))
  }
  if (length(block_code) > 0)
    slot_args$blocks <- .codegen_list_arg(block_code)

  # Standalone variables (those not produced by a block). Variables expanded
  # from a block carry a `block_id` provenance marker; recipes serialized
  # before that marker existed fall back to the legacy name-matching heuristic.
  has_provenance <- any(vapply(recipe$variables,
                               function(v) !is.null(v$block_id), logical(1)))
  block_vars <- if (has_provenance) character(0) else
    unlist(lapply(recipe$blocks, function(b) {
      vapply(seq_along(b$concept_ids), function(i) {
        cname <- if (!is.null(b$concept_names) && i <= length(b$concept_names))
          b$concept_names[i] else NULL
        if (!is.null(cname)) .sanitize_name(cname)
        else paste0(b$table, "_c", b$concept_ids[i])
      }, character(1))
    }))
  var_code <- character(0)
  for (nm in names(recipe$variables)) {
    v <- recipe$variables[[nm]]
    if (!is.null(v$block_id) || nm %in% block_vars) next

    # Use convenience constructors for derived variables
    code <- NULL
    if (!is.null(v$derived)) {
      code <- switch(v$derived$kind,
        "age" = .codegen_call("omop_variable_age",
          name = v$name,
          reference = v$derived$reference %||% "today",
          reference_date = v$derived$reference_date),
        "sex_mf" = .codegen_call("omop_variable_sex", name = v$name),
        "obs_duration" = .codegen_call("omop_variable_obs_duration",
          name = v$name),
        "drug_duration" = .codegen_call("omop_variable_drug_duration",
          concept_id = v$concept_id,
          concept_name = v$concept_name,
          name = v$name, agg = v$derived$agg %||% "mean"),
        "sum" = .codegen_call("omop_variable_sum",
          table = v$table, column = v$derived$column,
          concept_id = v$concept_id, concept_name = v$concept_name,
          name = v$name),
        "n_distinct" = .codegen_call("omop_variable_n_distinct",
          table = v$table, name = v$name),
        "prior_obs" = .codegen_call("omop_variable_prior_obs",
          name = v$name,
          reference_date = v$derived$reference_date),
        "followup" = .codegen_call("omop_variable_followup",
          name = v$name,
          reference_date = v$derived$reference_date),
        "demo_missingness" = .codegen_call("omop_variable_demo_missingness",
          name = v$name),
        "sd" = .codegen_call("omop_variable_sd",
          table = v$table, concept_id = v$concept_id,
          concept_name = v$concept_name, name = v$name,
          value_source = v$value_source),
        "cv" = .codegen_call("omop_variable_cv",
          table = v$table, concept_id = v$concept_id,
          concept_name = v$concept_name, name = v$name,
          value_source = v$value_source),
        "slope" = .codegen_call("omop_variable_slope",
          table = v$table, concept_id = v$concept_id,
          concept_name = v$concept_name, name = v$name,
          value_source = v$value_source),
        "charlson" = .codegen_call("omop_variable_charlson",
          name = v$name),
        "chads2" = .codegen_call("omop_variable_chads2",
          name = v$name),
        "chadsvasc" = .codegen_call("omop_variable_chadsvasc",
          name = v$name),
        "dcsi" = .codegen_call("omop_variable_dcsi",
          name = v$name),
        "hfrs" = .codegen_call("omop_variable_hfrs",
          name = v$name),
        NULL
      )
    }
    if (is.null(code)) {
      code <- .codegen_call("omop_variable",
        name = v$name, table = v$table, column = v$column,
        concept_id = v$concept_id, concept_name = v$concept_name,
        type = if (!identical(v$type %||% "auto", "auto")) v$type else NULL,
        format = v$format, value_source = v$value_source,
        time_window = v$time_window,
        suffix_mode = if (!identical(v$suffix_mode %||% "index", "index"))
          v$suffix_mode else NULL,
        filters = .codegen_filter_list(v$filters),
        visit_filter = v$visit_filter,
        concept_col = v$concept_col,
        expand = if (isTRUE(v$expand)) TRUE else NULL)
    }
    var_code <- c(var_code, code)
  }
  if (length(var_code) > 0)
    slot_args$variables <- .codegen_list_arg(var_code)

  # Filters (preserve their IDs as the list element names).
  filt_code <- character(0)
  for (id in names(recipe$filters)) {
    f <- recipe$filters[[id]]
    one <- if (inherits(f, "omop_filter_group")) .codegen_filter_group(f)
           else .codegen_filter(f)
    filt_code <- c(filt_code, paste0(.codegen_name(id), " = ", one))
  }
  if (length(filt_code) > 0)
    slot_args$filters <- .codegen_raw(
      paste0("list(", paste(filt_code, collapse = ", "), ")"))

  # Outputs.
  out_code <- character(0)
  for (nm in names(recipe$outputs)) {
    o <- recipe$outputs[[nm]]
    out_code <- c(out_code, .codegen_call("omop_output",
      name = o$name, type = o$type,
      variables = o$variables,
      population_id = o$population_id,
      options = if (length(o$options %||% list()) > 0) o$options else NULL,
      result_symbol = o$result_symbol))
  }
  if (length(out_code) > 0)
    slot_args$outputs <- .codegen_list_arg(out_code)

  # Base cohort reference (the base population itself is implicit).
  if (!is.null(recipe$populations$base$cohort_definition_id)) {
    slot_args$cohort <- recipe$populations$base$cohort_definition_id
  }

  # Recipe-level scope. `tables`/`combine` re-parse as scope directly; the scope
  # `cohort` re-parses as scope only when non-numeric (a handle/table name) —
  # a bare-id scope cohort is a codegen corner the JSON/YAML round-trip covers.
  if (!is.null(recipe$scope)) {
    sc <- recipe$scope
    if (!is.null(sc$cohort) && !is.numeric(sc$cohort) &&
        is.null(slot_args$cohort)) {
      slot_args$cohort <- sc$cohort
    }
    if (!is.null(sc$tables)) slot_args$tables <- sc$tables
    if (!identical(sc$combine %||% "union", "union"))
      slot_args$combine <- sc$combine
  }

  # Plan options (only when something differs from the constructor defaults,
  # so empty/default recipes stay clean).
  o <- recipe$options %||% list()
  def <- list(translate_concepts = TRUE, block_sensitive = TRUE,
              factor_concepts = TRUE)
  if (!identical(o$translate_concepts %||% TRUE, def$translate_concepts) ||
      !identical(o$block_sensitive    %||% TRUE,  def$block_sensitive) ||
      !identical(o$factor_concepts    %||% TRUE,  def$factor_concepts)) {
    slot_args$options <- list(
      translate_concepts = o$translate_concepts %||% TRUE,
      block_sensitive    = o$block_sensitive    %||% TRUE,
      factor_concepts    = o$factor_concepts    %||% TRUE)
  }

  lines <- paste0("recipe <- ",
                  do.call(.codegen_call, c("omop_recipe", slot_args)))

  # The block re-expansion order (blocks first, then standalone variables)
  # may differ from the recipe's insertion order; restore it so column
  # ordering in outputs is preserved. Only possible when block provenance
  # markers exist (legacy recipes keep the rebuild order). This is the one
  # piece the declarative call cannot express, so it trails as a fixup.
  if (has_provenance && length(recipe$variables) > 0) {
    is_block_var <- vapply(recipe$variables,
                           function(v) !is.null(v$block_id), logical(1))
    rebuilt_order <- c(
      unlist(lapply(names(recipe$blocks), function(bid) {
        names(recipe$variables)[vapply(recipe$variables, function(v)
          identical(v$block_id, bid), logical(1))]
      })),
      names(recipe$variables)[!is_block_var]
    )
    if (!identical(unname(rebuilt_order), names(recipe$variables)) &&
        setequal(rebuilt_order, names(recipe$variables))) {
      lines <- c(lines, paste0(
        "recipe$variables <- recipe$variables[",
        .codegen_value(names(recipe$variables)), "]"
      ))
    }
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
    .codegen_call("omop_filter_sex", value = f$params$value)
  } else if (f$type == "cohort") {
    .codegen_call("omop_filter_cohort",
      cohort_definition_id = f$params$cohort_definition_id)
  } else if (f$type == "age_group") {
    .codegen_call("omop_filter_age_group", groups = f$params$groups)
  } else if (f$type == "age_range") {
    .codegen_call("omop_filter_age", min = f$params$min, max = f$params$max)
  } else if (f$type == "has_concept") {
    .codegen_call("omop_filter_has_concept",
      concept_id = f$params$concept_id,
      table = f$params$table,
      concept_name = f$params$concept_name,
      window = f$params$window,
      min_count = f$params$min_count)
  } else if (f$type == "date_range") {
    .codegen_call("omop_filter_date_range",
      start = f$params$start, end = f$params$end)
  } else if (f$type == "value_bin") {
    # The disclosure-safe bin was already resolved when the filter was built
    # (via ds.omop.safe.cutpoints()), so regenerate it verbatim instead of
    # emitting a non-executable omop_filter_value() call.
    .codegen_call("omop_filter",
      type = f$type, level = f$level,
      params = f$params, label = f$label)
  } else if (f$type == "not_has_concept") {
    .codegen_call("omop_filter_not_has_concept",
      concept_id = f$params$concept_id,
      table = f$params$table,
      concept_name = f$params$concept_name,
      window = f$params$window)
  } else if (f$type == "value_concept") {
    .codegen_call("omop_filter_value_concept",
      concept_ids = f$params$value,
      column = f$params$var,
      concept_name = f$params$concept_name)
  } else if (f$type == "concept_count") {
    .codegen_call("omop_filter_concept_count",
      concept_id = f$params$concept_id,
      table = f$params$table,
      min_count = f$params$min_count,
      concept_name = f$params$concept_name)
  } else if (f$type == "prior_observation") {
    .codegen_call("omop_filter_prior_observation",
      min_days = f$params$min_days)
  } else if (f$type == "followup") {
    .codegen_call("omop_filter_followup",
      min_days = f$params$min_days)
  } else if (f$type == "visit_count") {
    .codegen_call("omop_filter_visit_count",
      min_count = f$params$min_count,
      visit_concept_id = f$params$visit_concept_id)
  } else if (f$type == "has_measurement") {
    .codegen_call("omop_filter_has_measurement",
      concept_id = f$params$concept_id,
      min_value = f$params$min_value,
      max_value = f$params$max_value)
  } else if (f$type == "missing_measurement") {
    .codegen_call("omop_filter_missing_measurement",
      concept_id = f$params$concept_id,
      window = f$params$window)
  } else {
    .codegen_call("omop_filter", type = f$type, level = f$level,
      params = if (length(f$params %||% list()) > 0) f$params else NULL,
      label = f$label)
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

# --- Recipe import/export ---

.recipe_strip_classes <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, .recipe_strip_classes)
    class(x) <- "list"
  }
  x
}

.recipe_plain <- function(recipe) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  out <- list(
    version     = "2.0",
    populations = .recipe_strip_classes(recipe$populations),
    blocks      = .recipe_strip_classes(recipe$blocks),
    variables   = .recipe_strip_classes(recipe$variables),
    filters     = .recipe_strip_classes(recipe$filters),
    outputs     = .recipe_strip_classes(recipe$outputs),
    options     = recipe$options %||% list(),
    meta        = .recipe_strip_classes(recipe$meta)
  )
  # Only serialize the scope when set, so an unscoped recipe round-trips
  # byte-for-byte (no spurious `scope: null` key). Population set-op fields are
  # plain lists and survive .recipe_strip_classes above unchanged.
  if (!is.null(recipe$scope)) out$scope <- recipe$scope
  out
}

.recipe_restore_filter <- function(f) {
  if (is.null(f)) return(NULL)

  if (!is.null(f$operator) || !is.null(f$children)) {
    children <- .recipe_restore_filter_list(f$children %||% list())
    if (length(children) == 0) return(NULL)
    args <- c(
      unname(children),
      list(operator = toupper(f$operator %||% "AND"), label = f$label)
    )
    return(do.call(omop_filter_group, args))
  }

  omop_filter(
    type = f$type %||% "custom",
    level = f$level %||% "population",
    params = .recipe_restore_params(f$params %||% list()),
    label = f$label
  )
}

#' Normalize imported filter params back to their constructed form
#'
#' JSON parsing (\code{simplifyVector = FALSE}) turns atomic vectors into
#' unnamed lists of scalars, and loses the integer/double distinction. This
#' restores atomic vectors and re-applies the canonical storage types used by
#' the \code{omop_filter_*} constructors so an export/import round-trip
#' compares \code{identical()}. Named lists (e.g. \code{window},
#' \code{value}) are kept intact.
#'
#' @param params Named list of filter parameters.
#' @return The normalized parameter list.
#' @keywords internal
.recipe_restore_params <- function(params) {
  if (length(params) == 0) return(list())
  # Unnamed lists of atomic scalars -> atomic vectors
  params <- lapply(params, function(p) {
    if (is.list(p) && length(p) > 0 && is.null(names(p)) &&
        all(vapply(p, function(e) is.atomic(e) && length(e) == 1L,
                   logical(1))))
      unlist(p)
    else p
  })
  # Canonical numeric types (constructors store these as integer / double)
  int_params <- c("concept_id", "cohort_definition_id", "min_count",
                  "min_days", "visit_concept_id")
  dbl_params <- c("min", "max", "min_value", "max_value")
  for (nm in intersect(int_params, names(params))) {
    if (is.numeric(params[[nm]])) params[[nm]] <- as.integer(params[[nm]])
  }
  for (nm in intersect(dbl_params, names(params))) {
    if (is.numeric(params[[nm]])) params[[nm]] <- as.numeric(params[[nm]])
  }
  if (is.list(params$value)) {
    for (nm in intersect(c("lower", "upper"), names(params$value))) {
      if (is.numeric(params$value[[nm]]))
        params$value[[nm]] <- as.numeric(params$value[[nm]])
    }
  }
  params
}

.recipe_restore_filter_list <- function(filters) {
  if (is.null(filters) || length(filters) == 0) return(list())
  ids <- names(filters)
  restored <- lapply(filters, .recipe_restore_filter)
  keep <- !vapply(restored, is.null, logical(1))
  restored <- restored[keep]
  if (!is.null(ids)) names(restored) <- ids[keep]
  restored
}

.recipe_restore_int <- function(x) {
  if (is.null(x)) return(NULL)
  as.integer(x)
}

.recipe_restore_chr <- function(x) {
  if (is.null(x)) return(NULL)
  as.character(x)
}

.recipe_from_plain <- function(data) {
  recipe <- omop_recipe()

  if (!is.null(data$populations)) {
    recipe$populations <- list()
    for (pid in names(data$populations)) {
      p <- data$populations[[pid]]
      if (!is.null(p$setop)) {
        # Set-op population: rebuild through the matching named argument so the
        # constructor re-validates (one op, >= 2 members) on import.
        members <- as.character(unlist(p$setop$members))
        setop_args <- stats::setNames(list(members), p$setop$op)
        recipe$populations[[pid]] <- do.call(omop_population, c(
          list(id = p$id %||% pid, label = p$label %||% pid,
               parent_id = p$parent_id),
          setop_args))
      } else {
        recipe$populations[[pid]] <- omop_population(
          id = p$id %||% pid,
          label = p$label %||% pid,
          parent_id = p$parent_id,
          filters = .recipe_restore_filter_list(p$filters),
          cohort_definition_id = p$cohort_definition_id
        )
      }
    }
  }

  if (!is.null(data$blocks)) {
    recipe$blocks <- list()
    for (bid in names(data$blocks)) {
      b <- data$blocks[[bid]]
      blk <- omop_variable_block(
        id = b$id %||% bid,
        table = b$table,
        concept_ids = .recipe_restore_int(b$concept_ids) %||% integer(0),
        concept_names = .recipe_restore_chr(b$concept_names),
        time_window = b$time_window,
        format = b$format %||% "raw",
        value_source = b$value_source,
        suffix_mode = b$suffix_mode %||% "index",
        filters = .recipe_restore_filter_list(b$filters),
        population_id = b$population_id %||% "base"
      )
      if (isTRUE(b$expand)) blk$expand <- TRUE
      recipe$blocks[[bid]] <- blk
    }
  }

  if (!is.null(data$variables)) {
    recipe$variables <- list()
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
        value_source = v$value_source,
        time_window = v$time_window,
        suffix_mode = v$suffix_mode %||% "index",
        filters = .recipe_restore_filter_list(v$filters),
        visit_filter = v$visit_filter,
        concept_col = v$concept_col
      )
      if (!is.null(v$derived)) var$derived <- v$derived
      if (!is.null(v$block_id)) var$block_id <- v$block_id
      if (isTRUE(v$expand)) var$expand <- TRUE
      recipe$variables[[nm]] <- var
    }
  }

  if (!is.null(data$filters)) {
    recipe$filters <- .recipe_restore_filter_list(data$filters)
  }

  if (!is.null(data$outputs)) {
    recipe$outputs <- list()
    for (nm in names(data$outputs)) {
      o <- data$outputs[[nm]]
      recipe$outputs[[nm]] <- omop_output(
        name = o$name %||% nm,
        type = o$type %||% "wide",
        variables = o$variables,
        population_id = o$population_id %||% "base",
        options = o$options %||% list(),
        result_symbol = o$result_symbol
      )
    }
  }

  if (!is.null(data$options)) {
    # Restore plan options, falling back to constructor defaults for any
    # missing key so older exports degrade gracefully. keep.null = TRUE so any
    # serialized `key: null` keeps its key (and an older export carrying a now-
    # removed option such as min_persons imports without error, simply ignored).
    recipe$options <- utils::modifyList(recipe$options %||% list(),
                                        data$options, keep.null = TRUE)
  }

  if (!is.null(data$scope)) {
    # Rebuild through the setter so types are normalised and an empty scope is
    # dropped. The serialized cohort value is already the resolved scope arg, so
    # it passes straight through .cohort_scope_arg unchanged.
    sc <- data$scope
    recipe <- recipe_set_scope(
      recipe,
      cohort  = sc$cohort,
      tables  = .recipe_restore_chr(if (!is.null(sc$tables))
        unlist(sc$tables) else NULL),
      combine = sc$combine %||% "union")
  }

  if (!is.null(data$meta)) {
    recipe$meta <- data$meta
  }
  # Serialization turns POSIXct into character; restore the type.
  if (is.character(recipe$meta$created)) {
    recipe$meta$created <- as.POSIXct(
      recipe$meta$created, tz = "",
      tryFormats = c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
    )
  }
  recipe$meta$modified <- Sys.time()
  recipe
}

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
  json <- jsonlite::toJSON(.recipe_plain(recipe), auto_unbox = TRUE, pretty = TRUE,
                            null = "null")
  if (is.null(file)) return(as.character(json))
  writeLines(as.character(json), file)
  invisible(file)
}

#' Import a recipe from JSON
#'
#' Reconstructs an \code{omop_recipe} from a JSON string or file previously
#' created by \code{\link{recipe_export_json}}. Automatically detects whether
#' the input is a file path or a raw JSON string. Nested filter groups, blocks,
#' output options, population filters, variable filters, and metadata are
#' preserved.
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
  if (length(json) == 1 && file.exists(json)) {
    json <- paste(readLines(json, warn = FALSE), collapse = "\n")
  }
  data <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  .recipe_from_plain(data)
}

#' Export a recipe to YAML
#'
#' Serializes the same portable recipe representation as
#' \code{\link{recipe_export_json}} to a YAML string or file.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param file Character or \code{NULL}; file path to write. If \code{NULL},
#'   returns the YAML string directly.
#' @return If \code{file} is \code{NULL}, returns a YAML string. Otherwise
#'   writes to \code{file} and returns the file path invisibly.
#' @examples
#' \dontrun{
#' yaml <- recipe_export_yaml(recipe)
#' recipe_export_yaml(recipe, file = "my_recipe.yml")
#' }
#' @seealso \code{\link{recipe_import_yaml}}, \code{\link{recipe_export_json}}
#' @export
recipe_export_yaml <- function(recipe, file = NULL) {
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Package 'yaml' is required for YAML export.", call. = FALSE)
  text <- yaml::as.yaml(.recipe_plain(recipe))
  if (is.null(file)) return(text)
  writeLines(text, file)
  invisible(file)
}

#' Import a recipe from YAML
#'
#' Reconstructs an \code{omop_recipe} from a YAML string or file previously
#' created by \code{\link{recipe_export_yaml}}.
#'
#' @param yaml Character; a YAML string, or a file path to a YAML file.
#' @return An \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_import_yaml("my_recipe.yml")
#' }
#' @seealso \code{\link{recipe_export_yaml}}, \code{\link{recipe_import_json}}
#' @export
recipe_import_yaml <- function(yaml) {
  if (!requireNamespace("yaml", quietly = TRUE))
    stop("Package 'yaml' is required for YAML import.", call. = FALSE)
  if (length(yaml) == 1 && file.exists(yaml)) {
    yaml <- paste(readLines(yaml, warn = FALSE), collapse = "\n")
  }
  .recipe_from_plain(yaml::yaml.load(yaml))
}

#' Save a recipe to JSON or YAML
#'
#' Convenience wrapper around \code{\link{recipe_export_json}} and
#' \code{\link{recipe_export_yaml}}. The format is inferred from the file
#' extension unless supplied explicitly.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param file Character; destination path ending in \code{.json},
#'   \code{.yml}, or \code{.yaml}.
#' @param format Character or \code{NULL}; optional explicit format:
#'   \code{"json"} or \code{"yaml"}.
#' @return The file path invisibly.
#' @examples
#' \dontrun{
#' recipe_save(recipe, "analysis_recipe.yml")
#' recipe_save(recipe, "analysis_recipe.json")
#' }
#' @seealso \code{\link{recipe_load}}, \code{\link{recipe_export_json}},
#'   \code{\link{recipe_export_yaml}}
#' @export
recipe_save <- function(recipe, file, format = NULL) {
  if (missing(file) || length(file) != 1L || !nzchar(file)) {
    stop("file must be a single non-empty path.", call. = FALSE)
  }
  fmt <- .recipe_file_format(file, format)
  switch(fmt,
    json = recipe_export_json(recipe, file = file),
    yaml = recipe_export_yaml(recipe, file = file)
  )
}

#' Load a recipe from JSON or YAML
#'
#' Convenience wrapper around \code{\link{recipe_import_json}} and
#' \code{\link{recipe_import_yaml}}. The parser is selected from the file
#' extension.
#'
#' @param file Character; source path ending in \code{.json}, \code{.yml}, or
#'   \code{.yaml}.
#' @return An \code{omop_recipe} object.
#' @examples
#' \dontrun{
#' recipe <- recipe_load("analysis_recipe.yml")
#' }
#' @seealso \code{\link{recipe_save}}, \code{\link{recipe_import_json}},
#'   \code{\link{recipe_import_yaml}}
#' @export
recipe_load <- function(file) {
  if (missing(file) || length(file) != 1L || !nzchar(file)) {
    stop("file must be a single non-empty path.", call. = FALSE)
  }
  fmt <- .recipe_file_format(file)
  switch(fmt,
    json = recipe_import_json(file),
    yaml = recipe_import_yaml(file)
  )
}

.recipe_file_format <- function(file, format = NULL) {
  fmt <- format
  if (is.null(fmt)) {
    ext <- tolower(tools::file_ext(file))
    fmt <- switch(ext,
      json = "json",
      yml = "yaml",
      yaml = "yaml",
      NULL
    )
  } else {
    fmt <- tolower(fmt)
    if (length(fmt) == 1L && fmt == "yml") fmt <- "yaml"
  }
  if (is.null(fmt) || length(fmt) != 1L || !fmt %in% c("json", "yaml")) {
    stop("Recipe format must be 'json' or 'yaml', or file must end in ",
         ".json, .yml, or .yaml.", call. = FALSE)
  }
  fmt
}

# --- Preview schema helpers ---

#' Map a variable format to its preview R type
#' @keywords internal
.schema_r_type <- function(format, source_type = NULL) {
  format <- format %||% "raw"
  factor_fmts  <- c("binary", "sex_mf", "abnormal_high", "abnormal_low",
                    "binned")
  integer_fmts <- c("count", "n_distinct", "gap_max", "gap_mean",
                    "charlson", "chads2", "chadsvasc", "dcsi", "hfrs",
                    "demo_missingness")
  numeric_fmts <- c("mean", "min", "max", "sum", "sd", "cv", "slope",
                    "first_value", "last_value",
                    "age", "obs_duration", "drug_duration", "time_since",
                    "prior_obs", "followup", "duration_sum")
  if (format %in% factor_fmts)  return("factor")
  if (format %in% integer_fmts) return("integer")
  if (format %in% numeric_fmts) return("numeric")
  if (identical(format, "raw")) {
    st <- source_type %||% "auto"
    return(switch(st,
      numeric = "numeric", integer = "integer",
      boolean = "logical", date = "Date",
      categorical = , character = "character",
      "character"))   # auto / unknown -> character
  }
  "character"
}

#' Render a variable time_window as a preview string
#' @keywords internal
.schema_time_window <- function(tw) {
  if (is.null(tw)) return("all time")
  start <- tw$start; end <- tw$end
  rel   <- tw$relative_to %||% tw$rel %||% "index"
  if (is.null(start) && is.null(end)) return("all time")
  paste0("[", start %||% "-Inf", ",", end %||% "Inf", "] d rel ", rel)
}

#' Resolve the output column names a variable expands to
#' @keywords internal
.schema_expand_names <- function(v) {
  sn <- v[[".suffix_names"]] %||% v[["suffix_names"]]
  if (!is.null(sn) && length(sn) > 0) return(as.character(sn))
  n  <- v$n_values %||% v$n_cols
  if (!is.null(n) && is.numeric(n) && n > 1) {
    return(.suffix_names(v$name, as.integer(n),
                         mode = v$suffix_mode %||% "index",
                         labels = v$concept_names))
  }
  v$name
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

  empty_schema <- function() data.frame(
    output = character(0), column = character(0), source = character(0),
    concept = character(0), concept_name = character(0),
    type = character(0), format = character(0), r_type = character(0),
    time_window = character(0), stringsAsFactors = FALSE
  )

  schemas <- list()
  for (out_name in names(recipe$outputs)) {
    out <- recipe$outputs[[out_name]]
    var_names <- out$variables %||% names(recipe$variables)
    vars <- recipe$variables[var_names]
    vars <- Filter(Negate(is.null), vars)

    # Implicit person_id join-key row, pinned first per output.
    pid_row <- data.frame(
      output = out_name, column = "person_id", source = "person.person_id",
      concept = "", concept_name = "", type = "integer", format = "id",
      r_type = "integer", time_window = "all time", stringsAsFactors = FALSE
    )

    rows <- lapply(vars, function(v) {
      col_names <- .schema_expand_names(v)
      data.frame(
        output       = out_name,
        column       = col_names,
        source       = paste0(v$table, ".", v$column %||% "*"),
        concept      = if (!is.null(v$concept_id)) as.character(v$concept_id) else "",
        concept_name = v$concept_name %||% "",
        type         = v$type,
        format       = v$format,
        r_type       = .schema_r_type(v$format, v$type),
        time_window  = .schema_time_window(v$time_window),
        .table       = v$table,   # per-row table provenance (dropped below)
        stringsAsFactors = FALSE
      )
    })

    body <- if (length(rows) > 0) do.call(rbind, rows) else
      cbind(empty_schema(), .table = character(0))
    schema <- rbind(cbind(pid_row, .table = "person"), body)

    tables_used <- unique(vapply(vars, function(v) v$table, character(1)))

    # For long outputs spanning multiple tables, mark the per-row table split
    # (one body row per expanded column already carries its source table).
    if (identical(out$type, "long") && length(tables_used) > 1) {
      schema$table_split <- schema$.table
    }
    schema$.table <- NULL

    attr(schema, "join_key")      <- "person_id"
    attr(schema, "tables")        <- tables_used
    attr(schema, "output_type")   <- out$type
    attr(schema, "population_id") <- out$population_id

    schemas[[out_name]] <- schema
  }

  schemas
}

# --- Recipe execution helpers ---

#' Preview a recipe on the server
#'
#' Compiles a recipe to a plan and calls \code{\link{ds.omop.plan.preview}}.
#' This keeps the plan layer available for advanced users while allowing the
#' usual recipe workflow to stay one-step.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param symbol Character; OMOP session symbol on the server (default
#'   \code{"omop"}).
#' @param conns DSI connections or \code{NULL} (uses active connections).
#' @return A named list of server-side plan preview results.
#' @examples
#' \dontrun{
#' preview <- recipe_preview(recipe)
#' }
#' @seealso \code{\link{recipe_validate}}, \code{\link{recipe_execute}},
#'   \code{\link{recipe_to_plan}}
#' @export
recipe_preview <- function(recipe, symbol = "omop", conns = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  ds.omop.plan.preview(recipe_to_plan(recipe), symbol = symbol, conns = conns)
}

#' Validate a recipe on the server
#'
#' Compiles a recipe to a plan and calls \code{\link{ds.omop.plan.validate}}.
#'
#' @param recipe An \code{omop_recipe} object.
#' @param symbol Character; OMOP session symbol on the server (default
#'   \code{"omop"}).
#' @param conns DSI connections or \code{NULL} (uses active connections).
#' @return A named list of server-side validation results.
#' @examples
#' \dontrun{
#' validation <- recipe_validate(recipe)
#' }
#' @seealso \code{\link{recipe_preview}}, \code{\link{recipe_execute}},
#'   \code{\link{recipe_to_plan}}
#' @export
recipe_validate <- function(recipe, symbol = "omop", conns = NULL) {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)
  ds.omop.plan.validate(recipe_to_plan(recipe), symbol = symbol, conns = conns)
}

# Formats that are computed from the person table (no concept extraction), so
# rules keyed on concept_id (e.g. concept_zero) must skip them.
.lint_person_derived_fmts <- c(
  "age", "sex_mf", "obs_duration", "drug_duration",
  "prior_obs", "followup", "demo_missingness",
  "charlson", "chads2", "chadsvasc", "dcsi", "hfrs"
)

# Aggregate formats that pull a numeric value column; flagged when value_source
# is unset (the server then silently defaults to value_as_number).
.lint_value_source_fmts <- c(
  "mean", "min", "max", "sum", "sd", "cv", "slope",
  "first_value", "last_value"
)

# Domains where a categorical/factor representation risks high cardinality.
.lint_highcard_tables <- c(
  "condition_occurrence", "drug_exposure", "measurement",
  "observation", "procedure_occurrence"
)

#' One-row lint result (keeps the 4-column contract in one place)
#' @keywords internal
.lint_row <- function(severity, code, message, locus) {
  data.frame(
    severity = severity, code = code, message = message, locus = locus,
    stringsAsFactors = FALSE
  )
}

#' Lint a recipe for common authoring mistakes (pure client-side)
#'
#' Walks an \code{omop_recipe} and returns a tidy report of problems without
#' contacting any server. The first and most important check is whether the
#' recipe even compiles: \code{\link{recipe_to_plan}} is run inside
#' \code{tryCatch} and any compile error becomes the \code{no_compile} lint.
#' Remaining rules inspect outputs, variables, and population filters for
#' structural and disclosure-safety issues that would otherwise surface only at
#' execution time on the server.
#'
#' Severities are \code{"ERROR"} (will fail), \code{"WARNING"} (likely rejected
#' or wrong), and \code{"INFO"} (advisory). Rules:
#' \describe{
#'   \item{no_compile (ERROR)}{\code{recipe_to_plan()} throws.}
#'   \item{empty_output (ERROR)}{an output resolves to zero variables.}
#'   \item{dup_output (ERROR)}{duplicate output names.}
#'   \item{concept_zero (ERROR)}{a non-derived variable has an unmapped
#'     \code{concept_id} (0 or NA).}
#'   \item{concept_unnamed (WARNING)}{\code{concept_id} present but
#'     \code{concept_name} missing.}
#'   \item{age_no_index (WARNING)}{\code{format = "age"} with index reference
#'     but the base population has neither a cohort nor a date filter.}
#'   \item{narrow_filter (WARNING)}{population \code{age_range} width < 5 years
#'     or \code{date_range} width < 30 days (server rejects).}
#'   \item{highcard_factor (WARNING)}{a raw \code{_concept_id} column with
#'     \code{factor_concepts = TRUE}, or a categorical format on a
#'     high-cardinality domain.}
#'   \item{window_inverted (WARNING)}{a variable \code{time_window} with
#'     \code{start > end}.}
#'   \item{long_split (INFO)}{a \code{"long"} output spanning > 1 table.}
#'   \item{no_value_source (INFO)}{an aggregate format with no
#'     \code{value_source}.}
#' }
#' Edge cases: a \code{NULL} recipe returns a single \code{empty_recipe} info
#' row; a recipe with no outputs adds a \code{no_output} warning.
#'
#' @param recipe An \code{omop_recipe} object, or \code{NULL}.
#' @return A \code{data.frame} with columns \code{severity}, \code{code},
#'   \code{message}, \code{locus} (zero rows if the recipe is clean).
#' @examples
#' \dontrun{
#' recipe <- omop_recipe(outputs = omop_output(type = "wide"))
#' recipe_lint(recipe)
#' }
#' @seealso \code{\link{recipe_to_plan}}, \code{\link{recipe_validate}}
#' @export
recipe_lint <- function(recipe) {
  empty <- .lint_row(character(0), character(0), character(0), character(0))

  if (is.null(recipe)) {
    return(.lint_row("INFO", "empty_recipe", "Recipe is NULL.", "recipe"))
  }
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  rows <- list()
  add <- function(severity, code, message, locus) {
    rows[[length(rows) + 1L]] <<- .lint_row(severity, code, message, locus)
  }

  # ERROR no_compile: the compile error IS the first lint.
  compile_err <- tryCatch({
    recipe_to_plan(recipe)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(compile_err)) {
    add("ERROR", "no_compile",
        paste0("recipe_to_plan() failed: ", compile_err), "recipe")
  }

  outputs <- recipe$outputs %||% list()
  vars <- recipe$variables %||% list()

  # WARNING no_output: recipe with zero outputs.
  if (length(outputs) == 0) {
    add("WARNING", "no_output", "Recipe has no outputs.", "outputs")
  }

  # ERROR dup_output: duplicate output names.
  out_names <- names(outputs)
  if (length(out_names) > 0) {
    for (d in unique(out_names[duplicated(out_names)])) {
      add("ERROR", "dup_output",
          paste0("Duplicate output name '", d, "'."),
          paste0("outputs$", d))
    }
  }

  # Per-output: empty_output + long_split.
  for (onm in out_names) {
    out <- outputs[[onm]]
    resolved <- out$variables %||% names(vars)
    resolved_vars <- Filter(Negate(is.null), vars[resolved])

    if (length(resolved_vars) == 0) {
      add("ERROR", "empty_output",
          paste0("Output '", onm, "' resolves to zero variables."),
          paste0("outputs$", onm))
    }

    if (identical(out$type, "long")) {
      tables <- vapply(resolved_vars,
                       function(v) v$table %||% NA_character_, character(1))
      tables <- unique(tables[!is.na(tables)])
      if (length(tables) > 1) {
        add("INFO", "long_split",
            paste0("Long output '", onm, "' spans ", length(tables),
                   " tables; will split into ", onm, "_<table>."),
            paste0("outputs$", onm))
      }
    }
  }

  factor_concepts <- isTRUE((recipe$options %||% list())$factor_concepts)

  # Per-variable checks.
  for (vnm in names(vars)) {
    v <- vars[[vnm]]
    fmt <- v$format %||% "raw"
    is_person_derived <- fmt %in% .lint_person_derived_fmts

    # ERROR concept_zero: unmapped concept (non-person-derived only).
    if (!is_person_derived) {
      cid <- v$concept_id
      if (!is.null(cid) && (is.na(cid) || cid == 0)) {
        add("ERROR", "concept_zero",
            paste0("Variable '", vnm, "' has unmapped concept_id (",
                   if (is.na(cid)) "NA" else cid, ")."),
            paste0("variables$", vnm))
      }
    }

    # WARNING concept_unnamed: concept present but no concept_name.
    if (!is.null(v$concept_id) &&
        (is.null(v$concept_name) || !nzchar(v$concept_name))) {
      add("WARNING", "concept_unnamed",
          paste0("Variable '", vnm, "' has concept_id ", v$concept_id,
                 " but no concept_name."),
          paste0("variables$", vnm))
    }

    # WARNING window_inverted: time_window start > end.
    tw <- v$time_window
    if (!is.null(tw) && !is.null(tw$start) && !is.null(tw$end) &&
        tw$start > tw$end) {
      add("WARNING", "window_inverted",
          paste0("Variable '", vnm, "' time_window start (", tw$start,
                 ") > end (", tw$end, ")."),
          paste0("variables$", vnm))
    }

    # INFO no_value_source: aggregate format without a value_source.
    if (fmt %in% .lint_value_source_fmts && is.null(v$value_source)) {
      add("INFO", "no_value_source",
          paste0("Variable '", vnm, "' format '", fmt,
                 "' has no value_source (server defaults to value_as_number)."),
          paste0("variables$", vnm))
    }

    # WARNING highcard_factor: raw _concept_id column with factor_concepts=TRUE,
    # or a categorical format on a high-cardinality domain.
    raw_concept_col <- !is.null(v$column) && grepl("_concept_id$", v$column)
    if (raw_concept_col && factor_concepts) {
      add("WARNING", "highcard_factor",
          paste0("Variable '", vnm, "' is a raw _concept_id column with ",
                 "factor_concepts=TRUE (>40 levels / 0.33 density risk)."),
          paste0("variables$", vnm))
    } else if (identical(v$type, "categorical") &&
               (v$table %||% "") %in% .lint_highcard_tables) {
      add("WARNING", "highcard_factor",
          paste0("Variable '", vnm, "' is a categorical format on ",
                 "high-cardinality domain '", v$table,
                 "' (>40 levels / 0.33 density risk)."),
          paste0("variables$", vnm))
    }

    # WARNING age_no_index: age relative to index but base has no anchor.
    if (identical(fmt, "age") &&
        identical((v$derived %||% list())$reference, "index")) {
      base_pop <- (recipe$populations %||% list())$base
      has_cohort <- !is.null(base_pop$cohort_definition_id)
      all_filters <- c(.flatten_filters(recipe$filters %||% list()),
                       .flatten_filters(base_pop$filters %||% list()))
      has_date <- any(vapply(all_filters,
        function(f) identical(f$type, "date_range"), logical(1)))
      if (!has_cohort && !has_date) {
        add("WARNING", "age_no_index",
            paste0("Variable '", vnm, "' uses reference='index' but base ",
                   "population has no cohort and no date filter ",
                   "(negative-age risk)."),
            paste0("variables$", vnm))
      }
    }
  }

  # WARNING narrow_filter: population filters too narrow (server rejects).
  pop_filters <- c(
    .flatten_filters(recipe$filters %||% list(), level = "population"),
    .flatten_filters((recipe$populations$base$filters) %||% list(),
                     level = "population")
  )
  for (f in pop_filters) {
    if (identical(f$type, "age_range")) {
      lo <- f$params$min; hi <- f$params$max
      if (!is.null(lo) && !is.null(hi) && (hi - lo) < 5) {
        add("WARNING", "narrow_filter",
            paste0("age_range filter width (", hi - lo,
                   ") < 5 years; server will reject."), "filters")
      }
    } else if (identical(f$type, "date_range")) {
      s <- f$params$start; e <- f$params$end
      if (!is.null(s) && !is.null(e)) {
        dwidth <- tryCatch(
          as.numeric(as.Date(e) - as.Date(s)),
          error = function(err) NA_real_, warning = function(w) NA_real_)
        if (!is.na(dwidth) && dwidth < 30) {
          add("WARNING", "narrow_filter",
              paste0("date_range filter width (", dwidth,
                     " days) < 30; server will reject."), "filters")
        }
      }
    }
  }

  if (length(rows) == 0) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

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
                                        symbol = symbol,
                                        conns = conns)
      results[[tbl]] <- stats_res
    }, error = function(e) {
      results[[tbl]] <<- list(error = conditionMessage(e))
    })
  }

  dsomop_result(
    per_site = results,
    meta = list(
      call_code = .codegen_call("recipe_preview_stats",
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
#' @param output_mode Character; \code{"memory"} (default) or \code{"staged"}.
#'   Passed through to \code{\link{ds.omop.plan.execute}}.
#' @param cohort Optional recipe-level scope cohort applied at execution time: a
#'   \code{dsomop_cohort_handle}, a \code{cohort_definition_id}, or a server-side
#'   cohort table name. When supplied (with or without \code{tables}) it replaces
#'   any scope already on the recipe; \code{NULL} (the default) leaves the
#'   recipe's own scope untouched. Folded with \code{tables} by \code{combine}
#'   and intersected into every population (see \code{\link{omop_recipe}}).
#' @param tables Optional character vector of \code{omop.table} symbol names to
#'   add to the execution-time scope (their distinct persons). May be combined
#'   with \code{cohort}.
#' @param combine Character; how to fold the scope sources together:
#'   \code{"union"} (the default) or \code{"intersect"}.
#' @return Invisibly, the output symbol mapping (a named character vector).
#'   As with \code{\link{ds.omop.plan.execute}}, the produced symbols are
#'   recorded on the session so the manipulation wrappers can default to the
#'   last one.
#' @examples
#' \dontrun{
#' recipe_execute(recipe)
#' recipe_execute(recipe, output_mode = "staged")
#' # Or with explicit symbol mapping:
#' recipe_execute(recipe, out = c(features_wide = "D_features"))
#' # Scope every population to a cohort intersected with a workspace table:
#' recipe_execute(recipe, cohort = my_cohort, tables = "inclusion_set",
#'                combine = "intersect")
#' }
#' @seealso \code{\link{recipe_to_plan}}, \code{\link{ds.omop.plan.execute}}
#' @export
recipe_execute <- function(recipe, out = NULL, symbol = "omop", conns = NULL,
                           output_mode = "memory", cohort = NULL,
                           tables = NULL, combine = "union") {
  if (!inherits(recipe, "omop_recipe"))
    stop("recipe must be an omop_recipe object", call. = FALSE)

  # Execution-time scope overrides the recipe's stored scope when given. A scalar
  # cohort_definition_id here is a SCOPE (unlike omop_recipe(cohort=), where a
  # scalar sets the base cohort) — recipe_execute scopes, it does not re-root the
  # base population.
  if (!is.null(cohort) || !is.null(tables)) {
    recipe <- recipe_set_scope(recipe, cohort = cohort, tables = tables,
                               combine = combine)
  }

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

  ds.omop.plan.execute(plan, out = out, symbol = symbol, conns = conns,
                       output_mode = output_mode)
}
