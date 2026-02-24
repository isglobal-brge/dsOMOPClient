# ==============================================================================
# dsOMOPClient v2 - Cart / Recipe Infrastructure
# ==============================================================================
# The cart is the single source of truth for an OMOP extraction recipe.
# It holds: populations (who), variables (what), filters (constraints),
# variable blocks (grouped variables), and output specs (how to shape).
#
# Architecture:
#   Population DAG: base cohort -> filter chains -> subpopulations
#   Variable blocks: groups sharing table/time_window/row_filters
#   Filter groups: AND/OR nested condition chains
#   Outputs: multiple tables (wide, long, features, joined_long, etc.)
#
# Everything is reproducible via cart_to_code() and cart_export_json().
# ==============================================================================

# ==============================================================================
# Naming engine
# ==============================================================================

# Sanitize a concept name into a valid R variable name
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
#' @param name Character; proposed name
#' @param existing Character vector; existing names
#' @return Character; unique name (with _2, _3 suffixes if needed)
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
#' @param base_name Character; base variable name
#' @param n Integer; number of columns
#' @param mode Character; suffix mode
#' @param labels Character vector; labels for "label" mode (optional)
#' @param ranges Numeric matrix; start/end for "range" mode (optional)
#' @return Character vector of suffixed names
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

# ==============================================================================
# omop_variable: What to extract
# ==============================================================================

#' Create a variable specification
#'
#' Describes a single variable to extract from the CDM. Variables reference
#' a source table and column, and may include concept-level filtering and
#' output formatting options.
#'
#' @param name Character; output column name (auto-generated if NULL)
#' @param table Character; source OMOP table
#' @param column Character; source column to extract
#' @param concept_id Integer or NULL; concept ID filter (for concept columns)
#' @param concept_name Character or NULL; human-readable concept name
#' @param type Character; variable type hint
#' @param format Character; output format
#' @param value_source Character or NULL; column to extract value from
#'   (e.g. "value_as_number" for measurements)
#' @param time_window Named list with start/end offsets relative to index,
#'   or NULL for no window constraint
#' @param suffix_mode Character; how to name multi-column expansions
#' @param filters List of omop_filter objects to apply to this variable
#' @return An omop_variable object
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
                                     "binned"),
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

#' @export
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

# ==============================================================================
# omop_filter + omop_filter_group: Conditions & chaining
# ==============================================================================

#' Create a filter specification
#'
#' Filters restrict the population or events included in the extraction.
#' Three levels: population (person-level), row (event-level), output (post-extraction).
#'
#' @param type Character; filter type
#' @param level Character; "population", "row", or "output"
#' @param params Named list; filter-specific parameters
#' @param label Character or NULL; human-readable description
#' @return An omop_filter object
#' @export
omop_filter <- function(type = c("sex", "age_range", "cohort",
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

#' @export
print.omop_filter <- function(x, ...) {
  cat("omop_filter [", x$level, "]:", x$label, "\n")
  invisible(x)
}

#' Create an AND/OR group of filters
#'
#' Allows nested condition chains. Groups can contain filters or other groups.
#'
#' @param ... omop_filter or omop_filter_group objects
#' @param operator Character; "AND" or "OR"
#' @param label Character or NULL; human-readable description
#' @return An omop_filter_group object
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

#' @export
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

# Convenience constructors for common filters

#' @rdname omop_filter
#' @param value Character; "M", "F", or concept ID
#' @export
omop_filter_sex <- function(value) {
  omop_filter(
    type = "sex", level = "population",
    params = list(value = value),
    label = paste("Sex =", value)
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
#' @param op Character; comparison operator
#' @param value Numeric; threshold value
#' @param column Character; column to compare
#' @export
omop_filter_value <- function(op, value, column = "value_as_number") {
  omop_filter(
    type = "value_threshold", level = "row",
    params = list(op = op, value = value, column = column)
  )
}

# ==============================================================================
# omop_population: DAG of subpopulations
# ==============================================================================

#' Create a population node
#'
#' Populations form a DAG: a base cohort, with derived subpopulations
#' created by applying filter chains.
#'
#' @param id Character; population ID
#' @param label Character; human-readable label
#' @param parent_id Character or NULL; parent population ID
#' @param filters List of omop_filter/omop_filter_group objects
#' @param cohort_definition_id Integer or NULL; base cohort ID
#' @return An omop_population object
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

#' @export
print.omop_population <- function(x, ...) {
  parent_txt <- if (!is.null(x$parent_id)) paste("->", x$parent_id) else "(root)"
  cat("omop_population:", x$id, parent_txt, "-", x$label, "\n")
  if (!is.null(x$cohort_definition_id))
    cat("  Cohort ID:", x$cohort_definition_id, "\n")
  if (length(x$filters) > 0)
    cat("  Filters:", length(x$filters), "\n")
  invisible(x)
}

# ==============================================================================
# omop_variable_block: Grouped variables
# ==============================================================================

#' Create a variable block
#'
#' A variable block groups variables that share a source table, time window,
#' and row-level filters. Variables within a block inherit these defaults
#' but can override individually.
#'
#' @param id Character; block ID
#' @param table Character; shared source table
#' @param concept_ids Integer vector; concept IDs for all variables in block
#' @param concept_names Character vector or NULL; names matching concept_ids
#' @param time_window Named list with start/end offsets, or NULL
#' @param format Character; default format for variables in this block
#' @param value_source Character or NULL; default value source column
#' @param suffix_mode Character; naming mode for multi-column expansion
#' @param filters List of omop_filter objects; row-level filters for the block
#' @param population_id Character; which population this block belongs to
#' @return An omop_variable_block object
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

#' @export
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

# ==============================================================================
# omop_output: How to shape the result
# ==============================================================================

#' Create an output specification
#'
#' @param name Character; output table name
#' @param type Character; output type
#' @param variables Character vector; variable names to include (NULL = all)
#' @param population_id Character; which population to use
#' @param options Named list; type-specific options
#' @return An omop_output object
#' @export
omop_output <- function(name = "output_1",
                        type = c("wide", "long", "features",
                                 "survival", "intervals",
                                 "baseline", "joined_long",
                                 "covariates_sparse"),
                        variables = NULL,
                        population_id = "base",
                        options = list()) {
  type <- match.arg(type)
  obj <- list(
    name          = name,
    type          = type,
    variables     = variables,
    population_id = population_id,
    options       = options
  )
  class(obj) <- c("omop_output", "list")
  obj
}

#' @export
print.omop_output <- function(x, ...) {
  cat("omop_output:", x$name, "[", x$type, "]",
      "pop:", x$population_id, "\n")
  if (!is.null(x$variables))
    cat("  Variables:", paste(x$variables, collapse = ", "), "\n")
  invisible(x)
}

# ==============================================================================
# omop_cart: The single source of truth
# ==============================================================================

#' Create an empty cart (recipe)
#'
#' The cart holds all selections for an OMOP extraction recipe:
#' populations (who), variable blocks (what, grouped), individual variables,
#' filters (constraints), and output specs (how to shape).
#'
#' @return An omop_cart object
#' @export
omop_cart <- function() {
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
  class(obj) <- c("omop_cart", "list")
  obj
}

#' Add a population to the cart
#'
#' @param cart An omop_cart
#' @param population An omop_population object
#' @return The modified cart
#' @export
cart_add_population <- function(cart, population) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  if (!inherits(population, "omop_population"))
    stop("population must be an omop_population object", call. = FALSE)
  # Validate parent exists
  if (!is.null(population$parent_id) &&
      !population$parent_id %in% names(cart$populations)) {
    stop("Parent population '", population$parent_id, "' not found in cart",
         call. = FALSE)
  }
  cart$populations[[population$id]] <- population
  cart$meta$modified <- Sys.time()
  cart
}

#' Remove a population from the cart
#'
#' @param cart An omop_cart
#' @param id Character; population ID to remove
#' @return The modified cart
#' @export
cart_remove_population <- function(cart, id) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  if (id == "base") stop("Cannot remove base population", call. = FALSE)
  cart$populations[[id]] <- NULL
  cart$meta$modified <- Sys.time()
  cart
}

#' Add a variable block to the cart
#'
#' Expands concept_ids into individual omop_variable objects using the block's
#' defaults, and ensures unique naming.
#'
#' @param cart An omop_cart
#' @param block An omop_variable_block object
#' @return The modified cart
#' @export
cart_add_block <- function(cart, block) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  if (!inherits(block, "omop_variable_block"))
    stop("block must be an omop_variable_block object", call. = FALSE)

  cart$blocks[[block$id]] <- block

  # Expand concepts into variables
  existing_names <- names(cart$variables)
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
    cart$variables[[var_name]] <- v
    existing_names <- c(existing_names, var_name)
  }

  cart$meta$modified <- Sys.time()
  cart
}

#' Add a variable to the cart
#'
#' @param cart An omop_cart
#' @param variable An omop_variable object, or NULL to construct from ...
#' @param ... If variable is NULL, arguments passed to omop_variable()
#' @return The modified cart
#' @export
cart_add_variable <- function(cart, variable = NULL, ...) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

  if (is.null(variable)) {
    variable <- omop_variable(...)
  } else if (!inherits(variable, "omop_variable")) {
    stop("variable must be an omop_variable object", call. = FALSE)
  }

  # Ensure unique name
  variable$name <- .ensure_unique_name(variable$name, names(cart$variables))

  cart$variables[[variable$name]] <- variable
  cart$meta$modified <- Sys.time()
  cart
}

#' Remove a variable from the cart
#'
#' @param cart An omop_cart
#' @param name Character; variable name to remove
#' @return The modified cart
#' @export
cart_remove_variable <- function(cart, name) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  cart$variables[[name]] <- NULL
  cart$meta$modified <- Sys.time()
  cart
}

#' Add a filter to the cart
#'
#' @param cart An omop_cart
#' @param filter An omop_filter or omop_filter_group object
#' @param id Character or NULL; filter ID (auto-generated if NULL)
#' @return The modified cart
#' @export
cart_add_filter <- function(cart, filter, id = NULL) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  if (!inherits(filter, "omop_filter") && !inherits(filter, "omop_filter_group"))
    stop("filter must be an omop_filter or omop_filter_group object",
         call. = FALSE)

  if (is.null(id)) {
    ftype <- if (inherits(filter, "omop_filter_group")) filter$operator
             else filter$type
    id <- paste0("f", length(cart$filters) + 1L, "_", ftype)
  }
  cart$filters[[id]] <- filter
  cart$meta$modified <- Sys.time()
  cart
}

#' Remove a filter from the cart
#'
#' @param cart An omop_cart
#' @param id Character; filter ID to remove
#' @return The modified cart
#' @export
cart_remove_filter <- function(cart, id) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  cart$filters[[id]] <- NULL
  cart$meta$modified <- Sys.time()
  cart
}

#' Add an output specification to the cart
#'
#' @param cart An omop_cart
#' @param output An omop_output object
#' @return The modified cart
#' @export
cart_add_output <- function(cart, output) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  if (!inherits(output, "omop_output"))
    stop("output must be an omop_output object", call. = FALSE)

  cart$outputs[[output$name]] <- output
  cart$meta$modified <- Sys.time()
  cart
}

#' Remove an output specification from the cart
#'
#' @param cart An omop_cart
#' @param name Character; output name to remove
#' @return The modified cart
#' @export
cart_remove_output <- function(cart, name) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  cart$outputs[[name]] <- NULL
  cart$meta$modified <- Sys.time()
  cart
}

#' Clear the entire cart
#'
#' @param cart An omop_cart
#' @return An empty cart
#' @export
cart_clear <- function(cart) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  omop_cart()
}

#' @export
print.omop_cart <- function(x, ...) {
  cat("=== omop_cart ===\n")

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

# ==============================================================================
# Cart -> Plan conversion
# ==============================================================================

#' Convert a cart to an extraction plan
#'
#' Translates the cart's populations, variables, filters, and outputs into
#' an omop_plan suitable for server-side execution.
#'
#' @param cart An omop_cart object
#' @return An omop_plan object
#' @export
cart_to_plan <- function(cart) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

  plan <- ds.omop.plan()

  # Build cohort from base population + population-level filters
  base_pop <- cart$populations[["base"]]
  if (!is.null(base_pop$cohort_definition_id)) {
    plan <- ds.omop.plan.cohort(plan,
      cohort_definition_id = base_pop$cohort_definition_id)
  }

  # Collect all population-level filters
  pop_filters <- .collect_pop_filters(cart)
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
  for (out_name in names(cart$outputs)) {
    out <- cart$outputs[[out_name]]
    var_names <- out$variables %||% names(cart$variables)
    vars <- cart$variables[var_names]
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
      tables_spec <- lapply(by_table, function(vs) {
        cols <- unique(unlist(lapply(vs, function(v) {
          c(v$column, v$value_source)
        })))
        cols[!is.null(cols)]
      })
      if (out$type == "baseline") {
        plan <- ds.omop.plan.baseline(plan,
          columns = tables_spec[["person"]] %||% c("gender_concept_id",
            "year_of_birth", "race_concept_id"),
          name = out_name)
      } else {
        plan <- ds.omop.plan.person_level(plan, tables = tables_spec,
                                           name = out_name)
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
        specs <- lapply(vs, function(v) {
          feat_fn <- switch(v$format,
            binary = omop.feature.boolean,
            count = omop.feature.count,
            first_value = omop.feature.first_value,
            last_value = omop.feature.latest_value,
            mean = omop.feature.mean_value,
            min = omop.feature.min_value,
            max = omop.feature.max_value,
            time_since = omop.feature.time_since,
            omop.feature.boolean
          )
          feat_fn(
            name = v$name,
            concept_set = if (!is.null(v$concept_id)) v$concept_id else integer(0)
          )
        })
        names(specs) <- vapply(vs, function(v) v$name, character(1))

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

  # Apply row-level filters to relevant outputs
  row_filters <- .collect_row_filters(cart)
  if (length(row_filters) > 0) {
    for (out_name in names(plan$outputs)) {
      out <- plan$outputs[[out_name]]
      if (is.null(out$filters)) out$filters <- list()
      for (rf in row_filters) {
        if (rf$type == "date_range") {
          out$filters$time_window <- rf$params
        } else if (rf$type == "value_threshold") {
          out$filters$value_threshold <- rf$params
        } else if (rf$type == "concept_set") {
          out$filters$concept_set <- list(ids = rf$params$concept_ids)
        }
      }
      plan$outputs[[out_name]] <- out
    }
  }

  plan
}

# Collect all population-level filters (flattening groups)
.collect_pop_filters <- function(cart) {
  .flatten_filters(cart$filters, level = "population")
}

# Collect all row-level filters (flattening groups)
.collect_row_filters <- function(cart) {
  .flatten_filters(cart$filters, level = "row")
}

# Flatten filter groups into a list of individual filters at a given level
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

# ==============================================================================
# Cart -> Code generation (minimal, no library())
# ==============================================================================

#' Generate reproducible R code from a cart
#'
#' Produces minimal R code that recreates the cart. Does not include
#' library() calls or header comments.
#'
#' @param cart An omop_cart object
#' @return Character string of R code
#' @export
cart_to_code <- function(cart) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

  lines <- c("cart <- omop_cart()")

  # Populations (skip base)
  for (pid in names(cart$populations)) {
    if (pid == "base") next
    p <- cart$populations[[pid]]
    lines <- c(lines, paste0(
      "cart <- cart_add_population(cart, ",
      .build_code("omop_population",
        id = p$id, label = p$label, parent_id = p$parent_id,
        cohort_definition_id = p$cohort_definition_id),
      ")"
    ))
  }

  # Blocks
  for (bid in names(cart$blocks)) {
    b <- cart$blocks[[bid]]
    lines <- c(lines, paste0(
      "cart <- cart_add_block(cart, ",
      .build_code("omop_variable_block",
        id = b$id, table = b$table,
        concept_ids = b$concept_ids,
        format = b$format, population_id = b$population_id),
      ")"
    ))
  }

  # Individual variables (not from blocks)
  block_vars <- unlist(lapply(cart$blocks, function(b) {
    vapply(seq_along(b$concept_ids), function(i) {
      cname <- if (!is.null(b$concept_names) && i <= length(b$concept_names))
        b$concept_names[i] else NULL
      if (!is.null(cname)) .sanitize_name(cname)
      else paste0(b$table, "_c", b$concept_ids[i])
    }, character(1))
  }))
  for (nm in names(cart$variables)) {
    if (nm %in% block_vars) next
    v <- cart$variables[[nm]]
    lines <- c(lines, paste0(
      "cart <- cart_add_variable(cart, ",
      .build_code("omop_variable",
        name = v$name, table = v$table, column = v$column,
        concept_id = v$concept_id, concept_name = v$concept_name,
        format = v$format, value_source = v$value_source),
      ")"
    ))
  }

  # Filters
  for (id in names(cart$filters)) {
    f <- cart$filters[[id]]
    if (inherits(f, "omop_filter_group")) {
      lines <- c(lines, paste0(
        "cart <- cart_add_filter(cart, ",
        .codegen_filter_group(f), ', id = "', id, '")'
      ))
    } else {
      lines <- c(lines, paste0(
        "cart <- cart_add_filter(cart, ",
        .codegen_filter(f), ', id = "', id, '")'
      ))
    }
  }

  # Outputs
  for (nm in names(cart$outputs)) {
    o <- cart$outputs[[nm]]
    lines <- c(lines, paste0(
      "cart <- cart_add_output(cart, ",
      .build_code("omop_output",
        name = o$name, type = o$type, population_id = o$population_id),
      ")"
    ))
  }

  paste(lines, collapse = "\n")
}

# Generate code for a single filter
.codegen_filter <- function(f) {
  if (f$type == "sex") {
    .build_code("omop_filter_sex", value = f$params$value)
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

# Generate code for a filter group
.codegen_filter_group <- function(fg) {
  child_code <- vapply(fg$children, function(ch) {
    if (inherits(ch, "omop_filter_group")) .codegen_filter_group(ch)
    else .codegen_filter(ch)
  }, character(1))
  paste0("omop_filter_group(",
         paste(child_code, collapse = ", "),
         ', operator = "', fg$operator, '")')
}

# ==============================================================================
# JSON import/export
# ==============================================================================

#' Export a cart to JSON
#'
#' @param cart An omop_cart object
#' @param file Character or NULL; file path to write (NULL returns string)
#' @return If file is NULL, returns JSON string; otherwise writes to file
#' @export
cart_export_json <- function(cart, file = NULL) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

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
    populations = strip(cart$populations),
    blocks      = strip(cart$blocks),
    variables   = strip(cart$variables),
    filters     = strip(cart$filters),
    outputs     = strip(cart$outputs)
  )

  json <- jsonlite::toJSON(export, auto_unbox = TRUE, pretty = TRUE,
                            null = "null")
  if (is.null(file)) return(as.character(json))
  writeLines(as.character(json), file)
  invisible(file)
}

#' Import a cart from JSON
#'
#' @param json Character; JSON string or file path
#' @return An omop_cart object
#' @export
cart_import_json <- function(json) {
  # Detect if it's a file path
  if (length(json) == 1 && file.exists(json)) {
    json <- paste(readLines(json, warn = FALSE), collapse = "\n")
  }

  data <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  cart <- omop_cart()

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
      cart$populations[[pid]] <- pop
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
      cart$variables[[nm]] <- var
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
      cart$filters[[fid]] <- filt
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
        population_id = o$population_id %||% "base"
      )
      cart$outputs[[nm]] <- out
    }
  }

  cart
}

# ==============================================================================
# Preview schema
# ==============================================================================

#' Preview the output schema for a cart
#'
#' Returns projected columns, join keys, and output shape for each output,
#' without executing anything.
#'
#' @param cart An omop_cart object
#' @return A list of data.frames, one per output
#' @export
cart_preview_schema <- function(cart) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

  schemas <- list()
  for (out_name in names(cart$outputs)) {
    out <- cart$outputs[[out_name]]
    var_names <- out$variables %||% names(cart$variables)
    vars <- cart$variables[var_names]
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

# ==============================================================================
# Cart execution helpers
# ==============================================================================

#' Preview aggregate stats for a cart (without materializing)
#'
#' Runs aggregate-only queries to show safe counts and distributions
#' for the variables in the cart.
#'
#' @param cart An omop_cart object
#' @param scope Character; "per_site", "pooled", or "both"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections or NULL
#' @return A dsomop_result with aggregate stats per output
#' @export
cart_preview_stats <- function(cart,
                               scope = c("per_site", "pooled", "both"),
                               symbol = "omop", conns = NULL) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)
  scope <- match.arg(scope)

  # Collect tables referenced
  tables <- unique(vapply(cart$variables, function(v) v$table, character(1)))

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
      call_code = .build_code("cart_preview_stats",
        scope = scope, symbol = symbol),
      scope = scope
    )
  )
}

#' Execute a cart: compile to plan and run
#'
#' @param cart An omop_cart object
#' @param out Named character vector; output_name -> symbol_name mapping.
#'   If NULL, auto-generates symbol names.
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections or NULL
#' @return Invisible; the output symbol mapping
#' @export
cart_execute <- function(cart, out = NULL, symbol = "omop", conns = NULL) {
  if (!inherits(cart, "omop_cart"))
    stop("cart must be an omop_cart object", call. = FALSE)

  plan <- cart_to_plan(cart)

  # Auto-generate output symbols if not provided
  if (is.null(out)) {
    out_names <- names(plan$outputs)
    out <- stats::setNames(paste0("D_", out_names), out_names)
  }

  ds.omop.plan.execute(plan, out = out, symbol = symbol, conns = conns)
}
