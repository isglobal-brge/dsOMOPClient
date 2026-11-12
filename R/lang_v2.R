# Module: Recipe language 2.0 -- advanced-layer constructors (design-2.10.3 section 2)
#
# Every constructor returns a plain list with an S3 class whose fields are the
# constructor's own (canonicalised) arguments, so that the object prints,
# serialises to JSON/YAML and regenerates its own R call
# (recipe_to_code()). Validation here is structural only; semantic validation
# (references, cycles, roles) happens in recipe_to_plan() and authoritatively on
# the server. Parameter placeholders (objects of class "omop_param", created by
# the parameter layer) are accepted wherever a value is expected and are never
# validated here: recipe_to_plan() refuses any parameter that is still unbound.

# --- shared structural validators -------------------------------------------

#' Is a value an unbound recipe parameter placeholder?
#' @param x Any value.
#' @return Logical scalar.
#' @keywords internal
.v2_is_param <- function(x) inherits(x, "omop_param")

.V2_NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_]{0,63}$"
.V2_COLUMN_PATTERN <- "^[a-z][a-z0-9_]{0,63}$"
.V2_DATE_PATTERN <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

.v2_name <- function(x, field, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl(.V2_NAME_PATTERN, x)) {
    stop(field, " must be one name matching ", .V2_NAME_PATTERN,
         " (a letter, then up to 63 letters, digits or underscores).",
         call. = FALSE)
  }
  x
}

.v2_column <- function(x, field, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      !grepl(.V2_COLUMN_PATTERN, x)) {
    stop(field, " must be one lower-case OMOP column name.", call. = FALSE)
  }
  x
}

.v2_table <- function(x, field = "table") {
  if (.v2_is_param(x)) return(x)
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(field, " must be one OMOP table name.", call. = FALSE)
  }
  x <- tolower(x)
  if (!grepl(.V2_COLUMN_PATTERN, x)) {
    stop(field, " must be one lower-case OMOP table name.", call. = FALSE)
  }
  x
}

.v2_int <- function(x, field, min = NULL, max = NULL, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x != trunc(x) || abs(x) > .Machine$integer.max) {
    stop(field, " must be one exact integer.", call. = FALSE)
  }
  x <- as.integer(x)
  if (!is.null(min) && x < min) stop(field, " must be >= ", min, ".", call. = FALSE)
  if (!is.null(max) && x > max) stop(field, " must be <= ", max, ".", call. = FALSE)
  x
}

.v2_number <- function(x, field, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x)) {
    stop(field, " must be one finite number.", call. = FALSE)
  }
  as.numeric(x)
}

.v2_ids <- function(x, field, allow_empty = FALSE, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) || any(x != trunc(x)) ||
      any(x < 0) || any(x > .Machine$integer.max)) {
    stop(field, " must contain non-negative integer concept IDs.", call. = FALSE)
  }
  if (!allow_empty && length(x) == 0L) {
    stop(field, " must not be empty.", call. = FALSE)
  }
  if (length(x) > 100000L) {
    stop(field, " has more than 100000 concept IDs.", call. = FALSE)
  }
  unique(as.integer(x))
}

.v2_flag <- function(x, field) {
  if (.v2_is_param(x)) return(x)
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(field, " must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

.v2_choice <- function(x, choices, field) {
  if (.v2_is_param(x)) return(x)
  # A missing argument arrives as the whole choice vector (match.arg idiom).
  if (is.character(x) && identical(x, choices)) return(choices[[1L]])
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices) {
    stop(field, " must be one of: ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  x
}

.v2_date <- function(x, field, allow_null = TRUE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  .plan_iso_date(x, field)
}

.v2_chr_vector <- function(x, field, allow_null = TRUE, unique = TRUE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x)) return(x)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (!is.character(x) || length(x) == 0L || anyNA(x) || any(!nzchar(x))) {
    stop(field, " must be a non-empty character vector.", call. = FALSE)
  }
  if (unique && anyDuplicated(x)) {
    stop(field, " must not contain duplicates.", call. = FALSE)
  }
  unname(x)
}

# Classes whose objects are event or interval sources.
.v2_is_event <- function(x) inherits(x, "omop_event")
.v2_is_interval <- function(x) inherits(x, "omop_interval")

#' Validate a source argument (an event, an interval, or a registry name)
#' @keywords internal
.v2_source <- function(x, field, intervals = TRUE, allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(NULL)
    stop(field, " is required.", call. = FALSE)
  }
  if (.v2_is_param(x) || .v2_is_event(x)) return(x)
  if (.v2_is_interval(x)) {
    if (!intervals) {
      stop(field, " must be an event (omop_event()), not an interval.",
           call. = FALSE)
    }
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) &&
      grepl(.V2_NAME_PATTERN, x)) {
    return(x)
  }
  stop(field, " must be an omop_event()", if (intervals) ", an omop_era()",
       " or the name of one declared in the recipe.", call. = FALSE)
}

# --- 2.1 Concept sets ----------------------------------------------------------

#' Define a concept set
#'
#' A concept set is a list of OMOP concept IDs with optional vocabulary
#' expansion. Resolution order: \code{concepts}, plus their descendants
#' (\code{concept_ancestor}) when \code{descendants = TRUE}, plus the concepts
#' that map to the set (\code{'Maps to'}) when \code{mapped = TRUE}, minus the
#' expansion of \code{exclude}. An object-valued \code{exclude} is expanded
#' with its own flags; a bare vector means exact IDs. A specified set that
#' resolves to nothing matches no row (it never means "all concepts").
#'
#' A named concept set is registered in the plan's \code{concept_sets} and
#' referenced by name; an unnamed one stays inline in the event that uses it.
#'
#' @param concepts Integer vector of concept IDs (may be empty: a set that
#'   matches nothing).
#' @param descendants Logical; include descendants.
#' @param mapped Logical; include source concepts that map to the set.
#' @param exclude \code{NULL}, an integer vector of exact concept IDs, or an
#'   unnamed \code{omop_concept_set()} expanded with its own flags.
#' @param name Optional registry name.
#' @return An \code{omop_concept_set} object.
#' @examples
#' t2dm <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
#' @seealso \code{\link{omop_event}}, \code{\link{omop_recipe}}
#' @export
omop_concept_set <- function(concepts, descendants = FALSE, mapped = FALSE,
                             exclude = NULL, name = NULL) {
  if (missing(concepts)) stop("omop_concept_set(): concepts is required.", call. = FALSE)
  concepts <- .v2_ids(concepts, "omop_concept_set(): concepts", allow_empty = TRUE)
  descendants <- .v2_flag(descendants, "omop_concept_set(): descendants")
  mapped <- .v2_flag(mapped, "omop_concept_set(): mapped")
  if (!is.null(exclude) && !.v2_is_param(exclude)) {
    if (inherits(exclude, "omop_concept_set")) {
      if (!is.null(exclude$name)) {
        stop("omop_concept_set(): an exclude set is expanded inline; give it ",
             "no name.", call. = FALSE)
      }
    } else {
      exclude <- .v2_ids(exclude, "omop_concept_set(): exclude", allow_empty = TRUE)
    }
  }
  name <- .v2_name(name, "omop_concept_set(): name")
  structure(list(concepts = concepts, descendants = descendants,
                 mapped = mapped, exclude = exclude, name = name),
            class = c("omop_concept_set", "list"))
}

#' @export
print.omop_concept_set <- function(x, ...) {
  ids <- if (.v2_is_param(x$concepts)) "<parameter>" else
    paste(utils::head(x$concepts, 8L), collapse = ", ")
  cat("omop_concept_set", if (!is.null(x$name)) paste0("'", x$name, "'"), ":",
      ids, if (length(x$concepts) > 8L) "...", "\n")
  flags <- c(if (isTRUE(x$descendants)) "descendants", if (isTRUE(x$mapped)) "mapped",
             if (!is.null(x$exclude)) "exclusions")
  if (length(flags)) cat("  includes:", paste(flags, collapse = ", "), "\n")
  invisible(x)
}

# --- 2.2 Events, predicates, units ---------------------------------------------

.V2_NOT_EVENT_TABLES <- c("cost", "provider", "location", "care_site",
                          "episode_event", "fact_relationship")

#' Define an event source
#'
#' An event is a typed row source over one OMOP table: the rows of the table
#' (restricted to a concept set when one is given), narrowed by a row
#' predicate (\code{where}) and an optional visit restriction, with the
#' numeric value normalised by \code{unit} \emph{before} any predicate or
#' reducer reads it.
#'
#' @param table OMOP table with a role record (any event table; not
#'   \code{cost}, \code{provider}, \code{location}, \code{care_site} or
#'   \code{episode_event}, which are reached through the bounded adapters
#'   \code{\link{omop_cost}} and \code{\link{omop_episode_members}}).
#' @param concepts Concept IDs, an \code{\link{omop_concept_set}}, the name of
#'   a registry concept set, or \code{NULL} for every row of the table.
#' @param descendants,mapped,exclude Expansion flags used when \code{concepts}
#'   is a vector of IDs (see \code{\link{omop_concept_set}}).
#' @param where \code{NULL} or a row predicate from \code{\link{omop_where}} /
#'   \code{\link{omop_where_tree}} over the table's filterable columns and the
#'   normalised \code{value}.
#' @param visit \code{NULL}, \code{list(type = <visit_concept_ids>)} or
#'   \code{"index"} (the visit of the index event).
#' @param value Name of the numeric value column (default: the table's value
#'   role, \code{value_as_number} where present).
#' @param unit \code{NULL} or an \code{\link{omop_unit}} conversion.
#' @param concept_col Scope the concepts on another concept column (for
#'   example \code{"unit_concept_id"}).
#' @param end \code{NULL} (the table's end column), \code{"start"} (point
#'   events) or \code{list(fallback = "days_supply")}.
#' @param name Event name (registry key; generated when \code{NULL}).
#' @param on_missing_table \code{"error"} (default) or \code{"unavailable"}: a
#'   site without the table gets a typed unavailable state (criteria UNKNOWN,
#'   numeric and count features NA) instead of a failure.
#' @return An \code{omop_event} object.
#' @examples
#' hba1c <- omop_event("measurement", 3004410,
#'   unit = omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15))),
#'   where = omop_where(!is.na(value)), name = "hba1c")
#' @seealso \code{\link{omop_where}}, \code{\link{omop_unit}},
#'   \code{\link{omop_era}}, \code{\link{omop_variable}}
#' @export
omop_event <- function(table, concepts = NULL, descendants = FALSE, mapped = FALSE,
                       exclude = NULL, where = NULL, visit = NULL, value = NULL,
                       unit = NULL, concept_col = NULL, end = NULL, name = NULL,
                       on_missing_table = c("error", "unavailable")) {
  if (missing(table)) stop("omop_event(): table is required.", call. = FALSE)
  table <- .v2_table(table, "omop_event(): table")
  if (!.v2_is_param(table) && table %in% .V2_NOT_EVENT_TABLES) {
    stop("omop_event(): '", table, "' is not an event source; use ",
         "omop_cost() or omop_episode_members().", call. = FALSE)
  }
  concepts <- .v2_event_concepts(concepts, descendants, mapped, exclude,
                                 "omop_event()")
  where <- .v2_predicate_arg(where, "omop_event(): where")
  visit <- .v2_visit(visit, "omop_event(): visit")
  value <- .v2_column(value, "omop_event(): value")
  if (!is.null(unit) && !.v2_is_param(unit) && !inherits(unit, "omop_unit")) {
    stop("omop_event(): unit must be created with omop_unit().", call. = FALSE)
  }
  concept_col <- .v2_column(concept_col, "omop_event(): concept_col")
  end <- .v2_event_end(end)
  name <- .v2_name(name, "omop_event(): name")
  on_missing_table <- .v2_choice(on_missing_table, c("error", "unavailable"),
                                 "omop_event(): on_missing_table")
  structure(list(kind = "table", table = table, concepts = concepts,
                 where = where, visit = visit, value = value, unit = unit,
                 concept_col = concept_col, end = end, name = name,
                 on_missing_table = on_missing_table),
            class = c("omop_event", "list"))
}

.v2_event_concepts <- function(concepts, descendants, mapped, exclude, ctx) {
  flags_given <- !isFALSE(descendants) || !isFALSE(mapped) || !is.null(exclude)
  if (is.null(concepts)) {
    if (flags_given) {
      stop(ctx, ": descendants/mapped/exclude need concepts.", call. = FALSE)
    }
    return(NULL)
  }
  if (.v2_is_param(concepts) || inherits(concepts, "omop_concept_set")) {
    if (flags_given) {
      stop(ctx, ": give descendants/mapped/exclude on the concept set itself.",
           call. = FALSE)
    }
    return(concepts)
  }
  if (is.character(concepts)) {
    if (flags_given) {
      stop(ctx, ": a registry concept-set name carries its own flags.",
           call. = FALSE)
    }
    return(.v2_name(concepts, paste0(ctx, ": concepts")))
  }
  omop_concept_set(concepts, descendants = descendants, mapped = mapped,
                   exclude = exclude)
}

.v2_visit <- function(visit, field) {
  if (is.null(visit) || .v2_is_param(visit)) return(visit)
  if (is.character(visit) && length(visit) == 1L && identical(visit, "index")) {
    return("index")
  }
  if (is.list(visit) && identical(names(visit), "type")) {
    return(list(type = .v2_ids(visit$type, paste0(field, "$type"))))
  }
  stop(field, " must be NULL, \"index\" or list(type = <visit_concept_ids>).",
       call. = FALSE)
}

.v2_event_end <- function(end) {
  if (is.null(end) || .v2_is_param(end)) return(end)
  if (identical(end, "start")) return("start")
  if (is.list(end) && identical(names(end), "fallback") &&
      identical(end$fallback, "days_supply")) {
    return(list(fallback = "days_supply"))
  }
  stop("omop_event(): end must be NULL, \"start\" or ",
       "list(fallback = \"days_supply\").", call. = FALSE)
}

#' @export
print.omop_event <- function(x, ...) {
  label <- switch(x$kind %||% "table",
    table = x$table,
    episode_members = "episode members",
    cost = paste0("cost (", x$field, ")"))
  cat("omop_event", if (!is.null(x$name)) paste0("'", x$name, "'"), ":", label, "\n")
  if (inherits(x$concepts, "omop_concept_set")) {
    cat("  concepts:", if (.v2_is_param(x$concepts$concepts)) "<parameter>" else
      paste(utils::head(x$concepts$concepts, 8L), collapse = ", "),
      if (isTRUE(x$concepts$descendants)) "(+ descendants)", "\n")
  } else if (is.character(x$concepts)) {
    cat("  concepts: concept set '", x$concepts, "'\n", sep = "")
  }
  if (!is.null(x$where)) cat("  where:", .v2_predicate_text(x$where), "\n")
  if (!is.null(x$unit)) cat("  unit: normalised to", x$unit$target, "\n")
  if (identical(x$on_missing_table, "unavailable")) {
    cat("  missing table: unavailable (typed NA/UNKNOWN)\n")
  }
  invisible(x)
}

#' Row predicates over an event's columns
#'
#' \code{omop_where()} captures an R expression as data (it is never
#' evaluated) using the closed grammar \code{==}, \code{!=}, \code{<},
#' \code{<=}, \code{>}, \code{>=}, \code{\%in\%}, \code{is.na()}, \code{!},
#' \code{&}, \code{|} and parentheses, with literal numbers, strings (ISO dates
#' are strings), logical constants and concept IDs. Several arguments are
#' combined with AND. Bare names are column names; the normalised value of an
#' event is \code{value}. Logic is three-valued as in SQL: a comparison with a
#' NULL column is UNKNOWN and only TRUE rows are kept.
#'
#' \code{omop_where_tree()} takes the explicit tree form of the plan schema
#' (\code{list(var =, op =, value =)} leaves, \code{and}/\code{or} lists,
#' \code{not}), which also offers \code{between}.
#'
#' @param ... Predicate expressions (unevaluated).
#' @param tree A predicate tree.
#' @return An \code{omop_predicate} object.
#' @examples
#' omop_where(value >= 6.5)
#' omop_where(!is.na(value), unit_concept_id %in% c(8554, 8753))
#' omop_where_tree(list(var = "value", op = "between",
#'                      value = list(lower = 5, upper = 10)))
#' @export
omop_where <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  if (length(exprs) == 0L) {
    stop("omop_where(): give at least one predicate.", call. = FALSE)
  }
  if (!is.null(names(exprs)) && any(nzchar(names(exprs)))) {
    stop("omop_where(): predicates are unnamed expressions (use == for ",
         "equality, not =).", call. = FALSE)
  }
  parts <- lapply(exprs, .v2_where_expr)
  tree <- if (length(parts) == 1L) parts[[1L]] else .v2_pred_flatten("and", parts)
  .v2_predicate(tree)
}

#' @rdname omop_where
#' @export
omop_where_tree <- function(tree) {
  if (inherits(tree, "omop_predicate")) return(tree)
  .v2_predicate(.v2_tree_canonical(tree))
}

.v2_predicate <- function(tree) {
  .v2_check_predicate_tree(tree)
  structure(list(tree = tree), class = c("omop_predicate", "list"))
}

.v2_predicate_arg <- function(x, field) {
  if (is.null(x) || .v2_is_param(x)) return(x)
  if (inherits(x, "omop_predicate")) return(x)
  if (is.list(x)) return(omop_where_tree(x))
  stop(field, " must be NULL or a predicate from omop_where().", call. = FALSE)
}

#' @export
print.omop_predicate <- function(x, ...) {
  cat("omop_predicate:", .v2_predicate_text(x), "\n")
  invisible(x)
}

.V2_CMP_OPS <- c("==", "!=", "<", "<=", ">", ">=")
.V2_CMP_FLIP <- c("==" = "==", "!=" = "!=", "<" = ">", "<=" = ">=",
                  ">" = "<", ">=" = "<=")

.v2_where_expr <- function(e) {
  if (is.symbol(e)) {
    stop("omop_where(): a bare column (", as.character(e), ") is not a ",
         "predicate; compare it, e.g. ", as.character(e), " == 1.", call. = FALSE)
  }
  if (!is.call(e)) {
    stop("omop_where(): a literal is not a predicate.", call. = FALSE)
  }
  fn <- as.character(e[[1L]])
  if (identical(fn, "(")) return(.v2_where_expr(e[[2L]]))
  if (fn %in% c("&", "|")) {
    op <- if (fn == "&") "and" else "or"
    return(.v2_pred_flatten(op, list(.v2_where_expr(e[[2L]]),
                                     .v2_where_expr(e[[3L]]))))
  }
  if (fn %in% c("&&", "||")) {
    stop("omop_where(): use & and | (vectorised), not ", fn, ".", call. = FALSE)
  }
  if (identical(fn, "!")) {
    inner <- e[[2L]]
    while (is.call(inner) && identical(as.character(inner[[1L]]), "(")) inner <- inner[[2L]]
    if (is.call(inner) && identical(as.character(inner[[1L]]), "is.na")) {
      return(list(var = .v2_where_column(inner[[2L]]), op = "not_null"))
    }
    if (is.call(inner) && identical(as.character(inner[[1L]]), "%in%")) {
      leaf <- .v2_where_expr(inner)
      leaf$op <- "not_in"
      return(leaf)
    }
    return(list(not = .v2_where_expr(inner)))
  }
  if (identical(fn, "is.na")) {
    if (length(e) != 2L) stop("omop_where(): is.na() takes one column.", call. = FALSE)
    return(list(var = .v2_where_column(e[[2L]]), op = "is_null"))
  }
  if (fn %in% .V2_CMP_OPS) {
    lhs <- e[[2L]]; rhs <- e[[3L]]
    if (is.symbol(lhs) && !is.symbol(rhs)) {
      return(list(var = .v2_where_column(lhs), op = fn,
                  value = .v2_where_literal(rhs)))
    }
    if (is.symbol(rhs) && !is.symbol(lhs)) {
      return(list(var = .v2_where_column(rhs), op = unname(.V2_CMP_FLIP[[fn]]),
                  value = .v2_where_literal(lhs)))
    }
    stop("omop_where(): a comparison needs one column and one literal.",
         call. = FALSE)
  }
  if (identical(fn, "%in%")) {
    lhs <- e[[2L]]
    if (!is.symbol(lhs)) {
      stop("omop_where(): the left side of %in% must be a column.", call. = FALSE)
    }
    values <- .v2_where_literal(e[[3L]], vector = TRUE)
    if (length(values) == 0L || length(values) > 10000L) {
      stop("omop_where(): %in% needs 1 to 10000 values.", call. = FALSE)
    }
    return(list(var = .v2_where_column(lhs), op = "in", value = values))
  }
  stop("omop_where(): '", fn, "' is not part of the predicate grammar ",
       "(== != < <= > >= %in% is.na ! & |).", call. = FALSE)
}

.v2_where_column <- function(s) {
  if (!is.symbol(s)) stop("omop_where(): expected a column name.", call. = FALSE)
  col <- as.character(s)
  if (!grepl(.V2_COLUMN_PATTERN, col)) {
    stop("omop_where(): '", col, "' is not a lower-case column name.", call. = FALSE)
  }
  col
}

.v2_where_literal <- function(e, vector = FALSE) {
  one <- function(v) {
    if (is.call(v) && identical(as.character(v[[1L]]), "-") && length(v) == 2L &&
        is.numeric(v[[2L]])) {
      v <- -v[[2L]]
    }
    if (is.call(v) && identical(as.character(v[[1L]]), "(")) return(one(v[[2L]]))
    if (is.numeric(v) && length(v) == 1L && is.finite(v)) return(v)
    if (is.character(v) && length(v) == 1L && !is.na(v)) return(v)
    if (is.logical(v) && length(v) == 1L && !is.na(v)) return(v)
    stop("omop_where(): literals must be numbers, strings or TRUE/FALSE; ",
         "expressions are never evaluated.", call. = FALSE)
  }
  if (vector) {
    if (is.call(e) && identical(as.character(e[[1L]]), "c")) {
      return(lapply(as.list(e)[-1L], one))
    }
    return(list(one(e)))
  }
  if (is.call(e) && identical(as.character(e[[1L]]), "c")) {
    stop("omop_where(): use %in% to compare with several values.", call. = FALSE)
  }
  one(e)
}

.v2_pred_flatten <- function(op, parts) {
  out <- list()
  for (p in parts) {
    if (identical(names(p), op)) out <- c(out, p[[op]]) else out <- c(out, list(p))
  }
  stats::setNames(list(out), op)
}

# Canonicalise a user-supplied tree: arrays as unnamed lists, scalars atomic.
.v2_tree_canonical <- function(tree) {
  if (!is.list(tree) || is.null(names(tree))) {
    stop("omop_where_tree(): a predicate node must be a named list.", call. = FALSE)
  }
  if (identical(names(tree), "and") || identical(names(tree), "or")) {
    children <- tree[[1L]]
    if (!is.list(children) || length(children) == 0L || !is.null(names(children))) {
      stop("omop_where_tree(): '", names(tree), "' needs a list of predicates.",
           call. = FALSE)
    }
    return(stats::setNames(list(lapply(children, .v2_tree_canonical)), names(tree)))
  }
  if (identical(names(tree), "not")) {
    return(list(not = .v2_tree_canonical(tree$not)))
  }
  out <- tree
  if (!is.null(tree$op) && tree$op %in% c("in", "not_in")) {
    out$value <- as.list(unlist(tree$value, use.names = FALSE))
  }
  if (identical(tree$op, "between") && is.list(tree$value)) {
    out$value <- tree$value[intersect(c("lower", "upper", "upper_inclusive"),
                                      names(tree$value))]
  }
  out
}

.v2_check_predicate_tree <- function(tree) {
  res <- .jsonSchemaValidate(.v2_schema_def("predicate"), tree,
                             root = .v2_plan_schema())
  if (!res$valid) {
    stop("Invalid predicate: ", paste(utils::head(res$errors, 3L), collapse = "; "),
         call. = FALSE)
  }
  between_bad <- function(node) {
    if (!is.list(node)) return(FALSE)
    if (identical(node[["op"]], "between")) {
      lo <- node$value$lower; hi <- node$value$upper
      if (is.numeric(lo) && is.numeric(hi) && lo > hi) return(TRUE)
      if (is.character(lo) && is.character(hi) && lo > hi) return(TRUE)
    }
    children <- c(node[["and"]], node[["or"]])
    any(vapply(children, between_bad, logical(1))) ||
      (!is.null(node[["not"]]) && between_bad(node[["not"]]))
  }
  if (between_bad(tree)) {
    stop("Invalid predicate: between bounds must be ordered.", call. = FALSE)
  }
  invisible(TRUE)
}

.v2_predicate_text <- function(x) {
  tree <- if (inherits(x, "omop_predicate")) x$tree else x
  node <- function(n) {
    if (!is.null(n$and)) return(paste0("(", paste(vapply(n$and, node, ""), collapse = " & "), ")"))
    if (!is.null(n$or)) return(paste0("(", paste(vapply(n$or, node, ""), collapse = " | "), ")"))
    if (!is.null(n$not)) return(paste0("!", node(n$not)))
    lit <- function(v) if (is.character(v)) paste0("\"", v, "\"") else format(v)
    switch(n$op,
      is_null = paste0("is.na(", n$var, ")"),
      not_null = paste0("!is.na(", n$var, ")"),
      `in` = paste0(n$var, " %in% c(", paste(vapply(n$value, lit, ""), collapse = ", "), ")"),
      not_in = paste0("!(", n$var, " %in% c(", paste(vapply(n$value, lit, ""), collapse = ", "), "))"),
      between = paste0(n$var, " between [", lit(n$value$lower), ", ", lit(n$value$upper),
                       if (isFALSE(n$value$upper_inclusive)) ")" else "]"),
      paste(n$var, n$op, lit(n$value)))
  }
  node(tree)
}

#' Unit normalisation for an event's numeric value
#'
#' Rows whose unit is the \code{target} keep their value; rows in a unit
#' listed in \code{convert} are converted with \code{value * factor + offset}
#' (also applied to \code{range_low}/\code{range_high}); conversion happens
#' before every predicate and reducer.
#'
#' @param target Target unit concept ID.
#' @param convert Named list \code{"<unit_concept_id>" = c(factor = , offset = )};
#'   \code{factor} finite and > 0, \code{offset} finite (default 0).
#' @param unknown Rows in any other unit: \code{"missing"} (value NA) or
#'   \code{"exclude"} (row dropped).
#' @param censored Rows whose operator concept is an inequality (\code{<},
#'   \code{>}, ...): \code{"keep"} the value or \code{"exclude"} the row.
#' @return An \code{omop_unit} object.
#' @export
omop_unit <- function(target, convert = list(), unknown = c("missing", "exclude"),
                      censored = c("keep", "exclude")) {
  if (missing(target)) stop("omop_unit(): target is required.", call. = FALSE)
  target <- .v2_int(target, "omop_unit(): target", min = 0L)
  if (!.v2_is_param(convert)) {
    if (length(convert) > 0L &&
        (!is.list(convert) || is.null(names(convert)) ||
         any(!grepl("^[0-9]+$", names(convert))) || anyDuplicated(names(convert)))) {
      stop("omop_unit(): convert must be a list named by unit concept IDs.",
           call. = FALSE)
    }
    unit_ids <- names(convert) %||% character(0)
    convert <- lapply(unit_ids, function(u) {
      spec <- convert[[u]]
      if (is.list(spec)) spec <- unlist(spec)
      if (!is.numeric(spec) || is.null(names(spec)) ||
          !all(names(spec) %in% c("factor", "offset")) || !"factor" %in% names(spec) ||
          anyDuplicated(names(spec))) {
        stop("omop_unit(): convert[['", u, "']] must be c(factor = , offset = ).",
             call. = FALSE)
      }
      factor <- .v2_number(spec[["factor"]], paste0("omop_unit(): factor for ", u))
      offset <- if ("offset" %in% names(spec)) {
        .v2_number(spec[["offset"]], paste0("omop_unit(): offset for ", u))
      } else 0
      if (factor <= 0) {
        stop("omop_unit(): factor for ", u, " must be > 0.", call. = FALSE)
      }
      c(factor = factor, offset = offset)
    })
    names(convert) <- unit_ids
    if (length(convert) == 0L) convert <- list()
  }
  unknown <- .v2_choice(unknown, c("missing", "exclude"), "omop_unit(): unknown")
  censored <- .v2_choice(censored, c("keep", "exclude"), "omop_unit(): censored")
  structure(list(target = target, convert = convert, unknown = unknown,
                 censored = censored), class = c("omop_unit", "list"))
}

# --- 11 Bounded domain adapters ----------------------------------------------

#' Events linked to oncology/treatment episodes
#'
#' An event source whose rows are the events linked to the episodes of
#' \code{episode} (an \code{omop_event("episode", ...)}) through
#' \code{episode_event}, using the package's reviewed field-concept mapping.
#' The same event linked to two episodes yields two rows.
#'
#' @param episode An \code{omop_event} over the \code{episode} table (or its
#'   name).
#' @param tables Optional member tables to keep (default: every mapped table).
#' @param where Optional row predicate over the member rows.
#' @param name Event name.
#' @param on_missing_table \code{"error"} or \code{"unavailable"}.
#' @return An \code{omop_event} object (adapter kind \code{episode_members}).
#' @export
omop_episode_members <- function(episode, tables = NULL, where = NULL, name = NULL,
                                 on_missing_table = "error") {
  if (missing(episode)) stop("omop_episode_members(): episode is required.", call. = FALSE)
  episode <- .v2_source(episode, "omop_episode_members(): episode", intervals = FALSE)
  if (.v2_is_event(episode) && !identical(episode$table, "episode")) {
    stop("omop_episode_members(): episode must be an event over the episode table.",
         call. = FALSE)
  }
  if (!is.null(tables) && !.v2_is_param(tables)) {
    tables <- vapply(.v2_chr_vector(tables, "omop_episode_members(): tables"),
                     .v2_table, "", field = "omop_episode_members(): tables")
  }
  where <- .v2_predicate_arg(where, "omop_episode_members(): where")
  name <- .v2_name(name, "omop_episode_members(): name")
  on_missing_table <- .v2_choice(on_missing_table, c("error", "unavailable"),
                                 "omop_episode_members(): on_missing_table")
  structure(list(kind = "episode_members", episode = episode, tables = tables,
                 where = where, name = name, on_missing_table = on_missing_table),
            class = c("omop_event", "list"))
}

.V2_COST_FIELDS <- c("total_charge", "total_cost", "total_paid", "paid_by_payer",
                     "paid_by_patient", "paid_patient_copay",
                     "paid_patient_coinsurance", "paid_patient_deductible",
                     "paid_by_primary", "paid_ingredient_cost",
                     "paid_dispensing_fee", "amount_allowed")

#' Cost rows linked to an event
#'
#' An event source whose rows are the \code{cost} rows linked to the rows of a
#' table event (through the reviewed event-domain mapping). Its value is the
#' one declared amount \code{field}; amounts are never summed across
#' currencies (a source mixing currencies is refused unless \code{currency}
#' restricts it).
#'
#' @param event A table \code{omop_event} (or its name).
#' @param field One cost amount column.
#' @param currency Optional currency concept ID.
#' @param cost_type Optional cost type concept IDs.
#' @param where Optional row predicate over the cost rows.
#' @param name Event name.
#' @param on_missing_table \code{"error"} or \code{"unavailable"}.
#' @return An \code{omop_event} object (adapter kind \code{cost}).
#' @export
omop_cost <- function(event, field = "total_cost", currency = NULL, cost_type = NULL,
                      where = NULL, name = NULL, on_missing_table = "error") {
  if (missing(event)) stop("omop_cost(): event is required.", call. = FALSE)
  event <- .v2_source(event, "omop_cost(): event", intervals = FALSE)
  if (.v2_is_event(event) && !identical(event$kind, "table")) {
    stop("omop_cost(): event must be a table event.", call. = FALSE)
  }
  field <- .v2_choice(field, .V2_COST_FIELDS, "omop_cost(): field")
  currency <- .v2_int(currency, "omop_cost(): currency", min = 0L, allow_null = TRUE)
  cost_type <- .v2_ids(cost_type, "omop_cost(): cost_type", allow_null = TRUE)
  where <- .v2_predicate_arg(where, "omop_cost(): where")
  name <- .v2_name(name, "omop_cost(): name")
  on_missing_table <- .v2_choice(on_missing_table, c("error", "unavailable"),
                                 "omop_cost(): on_missing_table")
  structure(list(kind = "cost", event = event, field = field, currency = currency,
                 cost_type = cost_type, where = where, name = name,
                 on_missing_table = on_missing_table),
            class = c("omop_event", "list"))
}

# --- 2.3 Criteria (population predicates) -------------------------------------

.v2_criterion <- function(kind, ...) {
  structure(c(list(kind = kind), list(...)), class = c("omop_criterion", "list"))
}

.v2_distinct <- function(distinct, field) {
  .v2_choice(distinct, c("row", "day", "visit", "concept"), field)
}

.v2_window_arg <- function(window, field) {
  if (is.null(window) || .v2_is_param(window) || inherits(window, "omop_window")) {
    return(window)
  }
  stop(field, " must be NULL or an omop_window().", call. = FALSE)
}

#' Population criteria
#'
#' Criteria are population predicates evaluated per person or per episode with
#' three-valued logic: a criterion over an undefined window (a required anchor
#' is missing) is UNKNOWN for every quantifier, UNKNOWN propagates through
#' \code{omop_not}/\code{omop_all}/\code{omop_any}, and only TRUE survives the
#' population boundary.
#'
#' \itemize{
#'   \item \code{omop_has(event, n, op, window, distinct)}: at least \code{n}
#'     (or \code{op n}) rows of the event in the window; \code{distinct}
#'     counts rows, distinct days, visits (NULL visit keys ignored) or
#'     concepts.
#'   \item \code{omop_has_not(event, window, nonmissing)}: no row (or, with
#'     \code{nonmissing = "value_as_number"}, no row with a non-null value).
#'   \item \code{omop_count_is(event, op, n, window, distinct)}: a count
#'     comparison; \code{op = "between"} takes \code{n = c(min, max)}.
#'   \item \code{omop_not}, \code{omop_all}, \code{omop_any}: logic.
#'   \item \code{omop_person(where)}: a person-table predicate (birth
#'     components may be referenced directly).
#'   \item \code{omop_age_is(min, max, reference, year)}: annual age
#'     (reference year minus year of birth) at the index (or in \code{year}).
#'   \item \code{omop_observed(before, after, gap)}: observation coverage of
#'     at least \code{before} days before and \code{after} days after the
#'     index (\code{gap} tolerates unobserved days between records).
#'   \item \code{omop_in_cohort(cohort_definition_id)}: membership of a
#'     results cohort.
#' }
#'
#' @param event An \code{omop_event}, an \code{omop_era} or a registry name.
#' @param n Count threshold (\code{c(min, max)} for \code{between}).
#' @param op Comparison: \code{">="}, \code{"<="}, \code{"=="} or
#'   \code{"between"}.
#' @param window \code{NULL} (any time) or an \code{\link{omop_window}}.
#' @param distinct What to count: \code{"row"}, \code{"day"}, \code{"visit"}
#'   or \code{"concept"}.
#' @param nonmissing \code{NULL} or a column that must be non-null.
#' @param criterion A criterion.
#' @param ... Criteria to combine.
#' @param where A predicate from \code{\link{omop_where}}.
#' @param min,max Age bounds (inclusive; \code{NULL} = open).
#' @param reference \code{"index"}.
#' @param year Optional fixed reference year (instead of the index).
#' @param before,after Required observed days before/after the index.
#' @param gap \code{NULL} or tolerated unobserved days between records.
#' @param cohort_definition_id A results-cohort definition ID.
#' @return An \code{omop_criterion} object.
#' @examples
#' omop_all(omop_observed(before = 365),
#'          omop_has_not(omop_event("drug_exposure", 1503297), window = omop_window(-365, -1)))
#' @export
omop_has <- function(event, n = 1L, op = ">=", window = NULL,
                     distinct = c("row", "day", "visit", "concept")) {
  if (missing(event)) stop("omop_has(): event is required.", call. = FALSE)
  .v2_count_criterion("omop_has()", "has", event, op, n, window, distinct)
}

#' @rdname omop_has
#' @export
omop_has_not <- function(event, window = NULL, nonmissing = NULL) {
  if (missing(event)) stop("omop_has_not(): event is required.", call. = FALSE)
  .v2_criterion("has_not",
                event = .v2_source(event, "omop_has_not(): event"),
                window = .v2_window_arg(window, "omop_has_not(): window"),
                nonmissing = .v2_column(nonmissing, "omop_has_not(): nonmissing"))
}

#' @rdname omop_has
#' @export
omop_count_is <- function(event, op, n, window = NULL,
                          distinct = c("row", "day", "visit", "concept")) {
  if (missing(event) || missing(op) || missing(n)) {
    stop("omop_count_is(): event, op and n are required.", call. = FALSE)
  }
  .v2_count_criterion("omop_count_is()", "count_is", event, op, n, window, distinct)
}

.v2_count_criterion <- function(ctx, kind, event, op, n, window, distinct) {
  event <- .v2_source(event, paste0(ctx, ": event"))
  op <- .v2_choice(op, c(">=", "<=", "==", "between"), paste0(ctx, ": op"))
  if (!.v2_is_param(n)) {
    if (identical(op, "between")) {
      if (!is.numeric(n) || length(n) != 2L) {
        stop(ctx, ": op = \"between\" needs n = c(min, max).", call. = FALSE)
      }
      n <- vapply(seq_len(2L), function(i) .v2_int(n[[i]], paste0(ctx, ": n"), min = 0L), 1L)
      if (n[[1L]] > n[[2L]]) stop(ctx, ": between bounds must be ordered.", call. = FALSE)
    } else {
      n <- .v2_int(n, paste0(ctx, ": n"), min = 0L)
    }
  }
  .v2_criterion(kind, event = event, op = op, n = n,
                window = .v2_window_arg(window, paste0(ctx, ": window")),
                distinct = .v2_distinct(distinct, paste0(ctx, ": distinct")))
}

#' @rdname omop_has
#' @export
omop_not <- function(criterion) {
  if (missing(criterion)) stop("omop_not(): criterion is required.", call. = FALSE)
  .v2_criterion("not", criterion = .v2_criterion_arg(criterion, "omop_not()"))
}

#' @rdname omop_has
#' @export
omop_all <- function(...) {
  items <- lapply(list(...), .v2_criterion_arg, ctx = "omop_all()")
  if (length(items) == 0L) stop("omop_all(): give at least one criterion.", call. = FALSE)
  .v2_criterion("all", criteria = unname(items))
}

#' @rdname omop_has
#' @export
omop_any <- function(...) {
  items <- lapply(list(...), .v2_criterion_arg, ctx = "omop_any()")
  if (length(items) == 0L) stop("omop_any(): give at least one criterion.", call. = FALSE)
  .v2_criterion("any", criteria = unname(items))
}

.v2_criterion_arg <- function(x, ctx) {
  if (inherits(x, c("omop_criterion", "omop_filter", "omop_filter_group")) ||
      .v2_is_param(x)) {
    return(x)
  }
  stop(ctx, ": expected a criterion (omop_has(), omop_observed(), ...).",
       call. = FALSE)
}

#' @rdname omop_has
#' @export
omop_person <- function(where) {
  if (missing(where)) stop("omop_person(): where is required.", call. = FALSE)
  .v2_criterion("person", where = .v2_predicate_arg(where, "omop_person(): where"))
}

#' @rdname omop_has
#' @export
omop_age_is <- function(min = NULL, max = NULL, reference = "index", year = NULL) {
  min <- .v2_int(min, "omop_age_is(): min", allow_null = TRUE)
  max <- .v2_int(max, "omop_age_is(): max", allow_null = TRUE)
  if (is.null(min) && is.null(max)) {
    stop("omop_age_is(): give min and/or max.", call. = FALSE)
  }
  if (is.integer(min) && is.integer(max) && min > max) {
    stop("omop_age_is(): min must not exceed max.", call. = FALSE)
  }
  reference <- .v2_choice(reference, "index", "omop_age_is(): reference")
  year <- .v2_int(year, "omop_age_is(): year", min = 1800L, max = 2200L,
                  allow_null = TRUE)
  .v2_criterion("age_is", min = min, max = max, reference = reference, year = year)
}

#' @rdname omop_has
#' @export
omop_observed <- function(before = NULL, after = NULL, gap = NULL) {
  .v2_criterion("observed",
                before = .v2_int(before, "omop_observed(): before", allow_null = TRUE),
                after = .v2_int(after, "omop_observed(): after", allow_null = TRUE),
                gap = .v2_int(gap, "omop_observed(): gap", min = 0L, allow_null = TRUE))
}

#' @rdname omop_has
#' @export
omop_in_cohort <- function(cohort_definition_id) {
  if (missing(cohort_definition_id)) {
    stop("omop_in_cohort(): cohort_definition_id is required.", call. = FALSE)
  }
  .v2_criterion("in_cohort", cohort_definition_id =
                  .v2_int(cohort_definition_id, "omop_in_cohort(): cohort_definition_id",
                          min = 1L))
}

#' @export
print.omop_criterion <- function(x, ...) {
  cat("omop_criterion:", .v2_criterion_text(x), "\n")
  invisible(x)
}

.v2_source_label <- function(s) {
  if (.v2_is_param(s)) return("<parameter>")
  if (is.character(s)) return(s)
  s$name %||% (if (.v2_is_interval(s)) "<era>" else s$table %||% s$kind %||% "<event>")
}

.v2_criterion_text <- function(x) {
  if (inherits(x, c("omop_filter", "omop_filter_group"))) return(x$label %||% "legacy filter")
  switch(x$kind,
    has = , count_is = paste0("count(", .v2_source_label(x$event),
                              if (!identical(x$distinct, "row")) paste0(", distinct ", x$distinct),
                              ") ", x$op, " ", paste(x$n, collapse = ".."),
                              if (!is.null(x$window)) paste0(" in ", .v2_window_text(x$window))),
    has_not = paste0("no ", .v2_source_label(x$event),
                     if (!is.null(x$nonmissing)) paste0(" with non-null ", x$nonmissing),
                     if (!is.null(x$window)) paste0(" in ", .v2_window_text(x$window))),
    not = paste0("NOT (", .v2_criterion_text(x$criterion), ")"),
    all = paste0("(", paste(vapply(x$criteria, .v2_criterion_text, ""), collapse = " AND "), ")"),
    any = paste0("(", paste(vapply(x$criteria, .v2_criterion_text, ""), collapse = " OR "), ")"),
    person = paste0("person: ", .v2_predicate_text(x$where)),
    age_is = paste0("age in [", x$min %||% "-", ", ", x$max %||% "-", "]",
                    if (!is.null(x$year)) paste0(" in ", x$year) else " at index"),
    observed = paste0("observed", if (!is.null(x$before)) paste0(" >= ", x$before, " d before"),
                      if (!is.null(x$after)) paste0(" >= ", x$after, " d after"),
                      if (!is.null(x$gap)) paste0(" (gap ", x$gap, ")")),
    in_cohort = paste0("in cohort ", x$cohort_definition_id),
    x$kind)
}

# --- 2.4 Windows and anchors ---------------------------------------------------

.V2_BUILTIN_ANCHORS <- c("index", "index_end", "observation_start", "observation_end")

.v2_anchor_arg <- function(anchor, field) {
  if (.v2_is_param(anchor) || inherits(anchor, "omop_anchor")) return(anchor)
  if (is.character(anchor) && length(anchor) == 1L && !is.na(anchor)) {
    if (anchor %in% .V2_BUILTIN_ANCHORS) return(anchor)
    if (grepl(.V2_DATE_PATTERN, anchor)) return(.plan_iso_date(anchor, field))
  }
  if (inherits(anchor, "Date") && length(anchor) == 1L) {
    return(format(anchor, "%Y-%m-%d"))
  }
  stop(field, " must be one of ", paste(.V2_BUILTIN_ANCHORS, collapse = ", "),
       ", an ISO date or an omop_anchor().", call. = FALSE)
}

.v2_window_endpoint <- function(x, field, side) {
  if (is.null(x) || .v2_is_param(x) || inherits(x, "omop_bound")) return(x)
  if (is.numeric(x) && length(x) == 1L && !is.na(x) && is.infinite(x)) {
    if (side == "start" && x < 0) return(NULL)
    if (side == "end" && x > 0) return(NULL)
    stop(field, ": ", if (side == "start") "Inf" else "-Inf",
         " is not an unbounded ", side, "; only -Inf (start) and Inf (end) ",
         "mean unbounded.", call. = FALSE)
  }
  .v2_int(x, field)
}

#' Windows, bounds and anchors
#'
#' \code{omop_window(start, end, anchor)} is an inclusive range of whole days
#' relative to an anchor (default the index date). \code{NULL} (or
#' \code{-Inf} for \code{start}, \code{Inf} for \code{end}) leaves an endpoint
#' unbounded. Each endpoint may instead be an \code{omop_bound(anchor,
#' offset)} with its own anchor, so start and end can be anchored
#' independently (e.g. on-treatment follow-up ending 30 days after the
#' treatment course).
#'
#' Anchors are \code{"index"}, \code{"index_end"}, \code{"observation_start"},
#' \code{"observation_end"}, an ISO date, or \code{omop_anchor(event, ...)}:
#' one complete event row selected by the order \code{(start_date, source,
#' key)}; \code{field = "end"} reads that row's end; \code{latest_end}
#' selects by end (only with \code{field = "end"}); \code{scope = "episode"}
#' confines the search to the episode's observed span; \code{search} is a
#' window restricting the candidate rows. A missing anchor makes a window
#' undefined (criteria UNKNOWN, reducers NA).
#'
#' @param start,end Day offsets, \code{NULL}/\code{-Inf}/\code{Inf}, or
#'   \code{omop_bound()}.
#' @param anchor Anchor of integer offsets (default \code{"index"}).
#' @param offset Integer day offset of a bound.
#' @param event An \code{omop_event}, \code{omop_era} or registry name.
#' @param which \code{"first"}, \code{"last"}, \code{"nth"} or
#'   \code{"latest_end"}.
#' @param n Row number for \code{which = "nth"}.
#' @param field \code{"start"} or \code{"end"} of the selected row.
#' @param scope \code{"person"} (default, may precede the index) or
#'   \code{"episode"}.
#' @param search \code{NULL} or an \code{omop_window()} restricting candidates.
#' @return \code{omop_window}, \code{omop_bound} or \code{omop_anchor} objects.
#' @examples
#' omop_window(-365, -1)
#' omop_window(omop_bound("index", 1), omop_bound("index_end", 30))
#' @export
omop_window <- function(start = NULL, end = NULL, anchor = "index") {
  start <- .v2_window_endpoint(start, "omop_window(): start", "start")
  end <- .v2_window_endpoint(end, "omop_window(): end", "end")
  anchor <- .v2_anchor_arg(anchor, "omop_window(): anchor")
  if (is.integer(start) && is.integer(end) && start > end) {
    stop("omop_window(): start must not be after end.", call. = FALSE)
  }
  structure(list(start = start, end = end, anchor = anchor),
            class = c("omop_window", "list"))
}

#' @rdname omop_window
#' @export
omop_bound <- function(anchor, offset = 0L) {
  if (missing(anchor)) stop("omop_bound(): anchor is required.", call. = FALSE)
  structure(list(anchor = .v2_anchor_arg(anchor, "omop_bound(): anchor"),
                 offset = .v2_int(offset, "omop_bound(): offset")),
            class = c("omop_bound", "list"))
}

#' @rdname omop_window
#' @export
omop_anchor <- function(event, which = c("first", "last", "nth", "latest_end"), n = 1L,
                        field = c("start", "end"), scope = c("person", "episode"),
                        search = NULL) {
  if (missing(event)) stop("omop_anchor(): event is required.", call. = FALSE)
  event <- .v2_source(event, "omop_anchor(): event")
  which <- .v2_choice(which, c("first", "last", "nth", "latest_end"), "omop_anchor(): which")
  n <- .v2_int(n, "omop_anchor(): n", min = 1L)
  field <- .v2_choice(field, c("start", "end"), "omop_anchor(): field")
  scope <- .v2_choice(scope, c("person", "episode"), "omop_anchor(): scope")
  if (identical(which, "latest_end") && !identical(field, "end")) {
    stop("omop_anchor(): which = \"latest_end\" is valid only with field = \"end\".",
         call. = FALSE)
  }
  if (!identical(which, "nth") && !.v2_is_param(n) && n != 1L) {
    stop("omop_anchor(): n is used only with which = \"nth\".", call. = FALSE)
  }
  structure(list(event = event, which = which, n = n, field = field, scope = scope,
                 search = .v2_window_arg(search, "omop_anchor(): search")),
            class = c("omop_anchor", "list"))
}

#' @export
print.omop_window <- function(x, ...) {
  cat("omop_window:", .v2_window_text(x), "\n")
  invisible(x)
}

.v2_anchor_text <- function(a) {
  if (.v2_is_param(a)) return("<parameter>")
  if (is.character(a)) return(a)
  paste0(a$which, if (identical(a$which, "nth")) paste0("[", a$n, "]"), " ",
         .v2_source_label(a$event), " ", a$field)
}

.v2_window_text <- function(w) {
  ep <- function(x, inf) {
    if (is.null(x)) return(inf)
    if (inherits(x, "omop_bound")) {
      return(paste0(.v2_anchor_text(x$anchor), if (x$offset >= 0) "+", x$offset))
    }
    format(x)
  }
  paste0("[", ep(w$start, "-Inf"), ", ", ep(w$end, "Inf"), "] days from ",
         .v2_anchor_text(w$anchor))
}

# --- 2.5 Index definitions, coverage ------------------------------------------

#' Index definitions, episode ends and observation coverage
#'
#' \code{omop_index()} defines the episodes (index dates) of an indexed
#' population from an event, an era, a results cohort
#' (\code{list(cohort_definition_id = n)}) or \code{omop_any_of(...)} arms.
#' \code{select = "first"}, \code{"last"} and \code{"nth"} select per person
#' \emph{before} the criteria are evaluated (the legacy, Circe primary-limit
#' order); \code{"first_eligible"} keeps the earliest candidate whose complete
#' criterion tree is TRUE; \code{"all"} keeps every candidate. \code{offset}
#' shifts the index date before coverage lookup; end-before-start episodes are
#' privately ineligible.
#'
#' \code{omop_end()} sets the episode end (\code{index_end}):
#' \code{observation_end} (default), \code{fixed} (\code{days} after the
#' index), \code{event_end} (the source row's end plus \code{pad}) or
#' \code{interval_end} (the era's end plus \code{pad}), each capped at the
#' observation end when covered.
#'
#' \code{omop_coverage()} is the observation-period policy: \code{unique}
#' (exactly one covering record; several follow \code{on_ambiguous}),
#' \code{earliest} (one complete record) or \code{union} (records coalesced
#' with at most \code{gap} unobserved days); \code{on_missing} decides
#' uncovered indexes. The new indexed-population default is
#' \code{unique/drop/drop}.
#'
#' @param source An event, era, \code{list(cohort_definition_id = n)} or
#'   \code{omop_any_of()}.
#' @param select Candidate selection (see Details).
#' @param n Row for \code{select = "nth"}.
#' @param offset Day offset added to the source start.
#' @param end An \code{omop_end()}.
#' @param restrict_to_observation Logical; \code{FALSE} keeps uncovered
#'   indexes (coverage \code{on_missing = "keep"}).
#' @param ... Index sources (events or eras) for \code{omop_any_of()}.
#' @param order Optional character vector naming the arms in their declared
#'   tie-break order (default: argument order).
#' @param strategy End strategy.
#' @param days Days after the index for \code{strategy = "fixed"}.
#' @param pad Non-negative days added to an event/interval end.
#' @param policy,on_ambiguous,on_missing,gap Coverage policy.
#' @return \code{omop_index}, \code{omop_any_of}, \code{omop_end} or
#'   \code{omop_coverage} objects.
#' @export
omop_index <- function(source, select = c("first", "first_eligible", "last", "all", "nth"),
                       n = 1L, offset = 0L, end = omop_end("observation_end"),
                       restrict_to_observation = TRUE) {
  if (missing(source)) stop("omop_index(): source is required.", call. = FALSE)
  source <- .v2_index_source(source)
  select <- .v2_choice(select, c("first", "first_eligible", "last", "all", "nth"),
                       "omop_index(): select")
  n <- .v2_int(n, "omop_index(): n", min = 1L)
  if (!identical(select, "nth") && !.v2_is_param(n) && n != 1L) {
    stop("omop_index(): n is used only with select = \"nth\".", call. = FALSE)
  }
  if (!.v2_is_param(end) && !inherits(end, "omop_end")) {
    stop("omop_index(): end must be created with omop_end().", call. = FALSE)
  }
  structure(list(source = source, select = select, n = n,
                 offset = .v2_int(offset, "omop_index(): offset"), end = end,
                 restrict_to_observation = .v2_flag(restrict_to_observation,
                   "omop_index(): restrict_to_observation")),
            class = c("omop_index", "list"))
}

.v2_index_source <- function(source) {
  if (.v2_is_param(source) || inherits(source, "omop_any_of")) return(source)
  if (is.list(source) && !inherits(source, c("omop_event", "omop_interval")) &&
      identical(names(source), "cohort_definition_id")) {
    id <- source$cohort_definition_id
    return(list(cohort_definition_id = .v2_int(id, "omop_index(): cohort_definition_id",
                                               min = 1L)))
  }
  .v2_source(source, "omop_index(): source")
}

#' @rdname omop_index
#' @export
omop_any_of <- function(..., order = NULL) {
  arms <- lapply(list(...), .v2_source, field = "omop_any_of(): arm")
  if (length(arms) < 2L) stop("omop_any_of(): give at least two arms.", call. = FALSE)
  if (!is.null(order)) {
    order <- .v2_chr_vector(order, "omop_any_of(): order")
    labels <- vapply(arms, .v2_source_label, "")
    if (!setequal(order, labels) || length(order) != length(arms)) {
      stop("omop_any_of(): order must name every arm exactly once.", call. = FALSE)
    }
    arms <- arms[match(order, labels)]
  }
  structure(list(arms = unname(arms)), class = c("omop_any_of", "list"))
}

#' @rdname omop_index
#' @export
omop_end <- function(strategy = c("observation_end", "fixed", "event_end", "interval_end"),
                     days = NULL, pad = 0L) {
  strategy <- .v2_choice(strategy, c("observation_end", "fixed", "event_end", "interval_end"),
                         "omop_end(): strategy")
  days <- .v2_int(days, "omop_end(): days", allow_null = TRUE)
  if (identical(strategy, "fixed") && is.null(days)) {
    stop("omop_end(): strategy = \"fixed\" needs days.", call. = FALSE)
  }
  if (!identical(strategy, "fixed") && !is.null(days)) {
    stop("omop_end(): days is used only with strategy = \"fixed\".", call. = FALSE)
  }
  structure(list(strategy = strategy, days = days,
                 pad = .v2_int(pad, "omop_end(): pad", min = 0L)),
            class = c("omop_end", "list"))
}

#' @rdname omop_index
#' @export
omop_coverage <- function(policy = c("unique", "earliest", "union"),
                          on_ambiguous = c("drop", "error"), on_missing = c("drop", "keep"),
                          gap = 0L) {
  structure(list(
    policy = .v2_choice(policy, c("unique", "earliest", "union"), "omop_coverage(): policy"),
    on_ambiguous = .v2_choice(on_ambiguous, c("drop", "error"), "omop_coverage(): on_ambiguous"),
    on_missing = .v2_choice(on_missing, c("drop", "keep"), "omop_coverage(): on_missing"),
    gap = .v2_int(gap, "omop_coverage(): gap", min = 0L)),
    class = c("omop_coverage", "list"))
}

# --- 2.6 Intervals, eras and censoring ----------------------------------------

#' Eras (intervals) built from an event
#'
#' Recorded closed intervals are normalised by the era's own \code{end} and
#' \code{fallback} from the event's original recorded end and supply roles,
#' then coalesced per person and group: a new era starts when a record starts
#' more than \code{gap} unobserved days after the running maximum end.
#' \code{by = "ingredient"} groups drug concepts by their RxNorm ingredient
#' (unmapped concepts follow \code{unmapped}); \code{persistence} extends
#' courses only; \code{stockpile = TRUE} adds an effective-supply relation
#' selected explicitly by \code{covered_days(coverage = "effective_supply")}.
#'
#' @param event An \code{omop_event} (or its name).
#' @param gap Tolerated uncovered days between records.
#' @param persistence Days added to each course end.
#' @param min_days Minimum course length.
#' @param by \code{"event"}, \code{"concept"} or \code{"ingredient"}.
#' @param unmapped \code{"own_concept"} or \code{"drop"} (ingredient grouping).
#' @param end \code{"recorded_end"} or \code{"start"} (point records).
#' @param fallback \code{"days_supply"} or \code{"none"}.
#' @param stockpile Logical.
#' @param name Interval name.
#' @return An \code{omop_interval} object.
#' @export
omop_era <- function(event, gap = 30L, persistence = 0L, min_days = 0L,
                     by = c("event", "concept", "ingredient"),
                     unmapped = c("own_concept", "drop"), end = c("recorded_end", "start"),
                     fallback = c("days_supply", "none"), stockpile = FALSE, name = NULL) {
  if (missing(event)) stop("omop_era(): event is required.", call. = FALSE)
  structure(list(
    event = .v2_source(event, "omop_era(): event", intervals = FALSE),
    gap = .v2_int(gap, "omop_era(): gap", min = 0L),
    persistence = .v2_int(persistence, "omop_era(): persistence", min = 0L),
    min_days = .v2_int(min_days, "omop_era(): min_days", min = 0L),
    by = .v2_choice(by, c("event", "concept", "ingredient"), "omop_era(): by"),
    unmapped = .v2_choice(unmapped, c("own_concept", "drop"), "omop_era(): unmapped"),
    end = .v2_choice(end, c("recorded_end", "start"), "omop_era(): end"),
    fallback = .v2_choice(fallback, c("days_supply", "none"), "omop_era(): fallback"),
    stockpile = .v2_flag(stockpile, "omop_era(): stockpile"),
    name = .v2_name(name, "omop_era(): name")),
    class = c("omop_interval", "list"))
}

#' @export
print.omop_interval <- function(x, ...) {
  cat("omop_era", if (!is.null(x$name)) paste0("'", x$name, "'"), "over",
      .v2_source_label(x$event), "- gap", x$gap, "days, by", x$by,
      if (isTRUE(x$stockpile)) "(stockpiled)", "\n")
  invisible(x)
}

#' Censoring by an event or era
#'
#' A censoring source for survival outputs: the date selected relative to the
#' effective entry (\code{first_end_after_entry} by default, everywhere) plus
#' \code{offset} days, optionally restricted to a \code{search} window.
#' \code{missing = "ineligible"} makes an episode without such a date
#' ineligible. \code{when_index_source} (amendment A-7) restricts the censor to
#' episodes whose \code{index_source} is one of the given arm tags, so a
#' two-arm study can censor at a switch to the \emph{other} arm only.
#'
#' @param source An event, era or registry name.
#' @param which \code{"first_end_after_entry"}, \code{"first_start_after_entry"}
#'   or \code{"containing_entry_end"}.
#' @param offset Integer days added (e.g. a grace period).
#' @param search \code{NULL} or an \code{omop_window()}.
#' @param missing \code{"ignore"} or \code{"ineligible"}.
#' @param when_index_source \code{NULL} or a non-empty unique character vector
#'   of index-source tags (arm names of the consumer's index).
#' @return An \code{omop_censor} object.
#' @export
omop_censor <- function(source, which = c("first_end_after_entry", "first_start_after_entry",
                                          "containing_entry_end"),
                        offset = 0L, search = NULL, missing = c("ignore", "ineligible"),
                        when_index_source = NULL) {
  if (base::missing(source)) stop("omop_censor(): source is required.", call. = FALSE)
  structure(list(
    source = .v2_source(source, "omop_censor(): source"),
    which = .v2_choice(which, c("first_end_after_entry", "first_start_after_entry",
                                "containing_entry_end"), "omop_censor(): which"),
    offset = .v2_int(offset, "omop_censor(): offset"),
    search = .v2_window_arg(search, "omop_censor(): search"),
    missing = .v2_choice(missing, c("ignore", "ineligible"), "omop_censor(): missing"),
    when_index_source = if (is.null(when_index_source)) NULL else {
      tags <- .v2_chr_vector(when_index_source, "omop_censor(): when_index_source")
      if (any(nchar(tags) > 64L)) {
        stop("omop_censor(): when_index_source tags have at most 64 characters.",
             call. = FALSE)
      }
      tags
    }),
    class = c("omop_censor", "list"))
}

# --- 2.7 Reducers and derived variables ----------------------------------------

.V2_REDUCER_FNS <- c("binary", "count", "n_distinct", "first_value", "last_value",
                     "nth_value", "closest_value", "mean", "min", "max", "sum", "sd",
                     "cv", "median", "quantile", "mode", "slope", "any", "all",
                     "first_date", "last_date", "days_since", "days_until", "time_since",
                     "duration_sum", "covered_days", "supply_days", "drug_duration",
                     "gap_max", "gap_mean", "abnormal_high", "abnormal_low",
                     "era_count", "era_days", "first_era_days")

# Reducers whose input is the event's (normalised) value V (section 2.7 table).
.V2_VALUE_REDUCERS <- c("first_value", "last_value", "nth_value", "closest_value",
                        "mean", "min", "max", "sum", "sd", "cv", "median", "quantile",
                        "slope")

#' Reducers: how an event stream becomes one value per person or episode
#'
#' The reducer contract (frozen, design section 2.7) fixes for every
#' \code{fn} its input, ordering, NULL handling, empty-window value and type.
#' Rows are ordered by \code{(start date, source, row key)}. A few rules worth
#' knowing: \code{first_value}/\code{last_value} skip NULL values by default
#' (\code{missing = "keep"} selects the row first); \code{count} and
#' \code{binary} are 0 over an empty window, value reducers NA;
#' \code{covered_days} measures the union of the recorded intervals inside the
#' window (never summed durations; \code{observed_only = TRUE} intersects it
#' with the observed days, the PDC numerator); \code{supply_days} sums
#' \code{days_supply} of fills starting in the window (\code{fills =
#' "clipped"} clips each fill to the window), the MPR numerator; an undefined
#' anchor gives NA for every reducer.
#'
#' @param fn Reducer name (see \code{fn} in the plan schema).
#' @param value Value column (default: the normalised \code{value} for value
#'   reducers).
#' @param window \code{NULL} or an \code{omop_window()}.
#' @param which \code{NULL} or \code{"first"}, \code{"last"}, \code{"nth"},
#'   \code{"closest"}.
#' @param n Row number(s) for \code{nth_value} (a vector gives suffixed
#'   columns).
#' @param side \code{closest_value}: \code{"either"}, \code{"before"},
#'   \code{"after"}.
#' @param missing \code{"skip"} (default) or \code{"keep"} NULL values before
#'   selection.
#' @param distinct \code{count}: \code{"row"}, \code{"day"}, \code{"visit"},
#'   \code{"concept"}.
#' @param p Quantile probability in (0, 1].
#' @param agg \code{drug_duration}: \code{"mean"}, \code{"sum"}, \code{"max"}.
#' @param reference Anchor for \code{days_since}/\code{days_until}.
#' @param predicate \code{any}/\code{all}: an \code{omop_where()} predicate.
#' @param reference_date \code{time_since}: fixed ISO date (legacy).
#' @param unit \code{time_since}: \code{"day"} or \code{"month"}.
#' @param coverage \code{covered_days}: \code{"recorded"} or
#'   \code{"effective_supply"} (stockpiled eras only).
#' @param fills \code{supply_days}: \code{"start_in_window"} or
#'   \code{"clipped"}.
#' @param observed_only \code{covered_days}: intersect with observed days.
#' @return An \code{omop_reducer} object.
#' @examples
#' omop_reduce("closest_value", window = omop_window(-180, 0), side = "before")
#' omop_reduce("covered_days", window = omop_window(1, 90), observed_only = TRUE)
#' @export
omop_reduce <- function(fn, value = NULL, window = NULL, which = NULL, n = 1L,
                        side = c("either", "before", "after"), missing = c("skip", "keep"),
                        distinct = NULL, p = NULL, agg = NULL, reference = NULL,
                        predicate = NULL, reference_date = NULL, unit = NULL,
                        coverage = c("recorded", "effective_supply"),
                        fills = c("start_in_window", "clipped"),
                        observed_only = FALSE) {
  if (base::missing(fn)) stop("omop_reduce(): fn is required.", call. = FALSE)
  fn <- .v2_choice(fn, .V2_REDUCER_FNS, "omop_reduce(): fn")
  value <- .v2_column(value, "omop_reduce(): value")
  window <- .v2_window_arg(window, "omop_reduce(): window")
  which <- if (is.null(which)) NULL else
    .v2_choice(which, c("first", "last", "nth", "closest"), "omop_reduce(): which")
  if (!.v2_is_param(n)) {
    if (!is.numeric(n) || length(n) < 1L || length(n) > 50L) {
      stop("omop_reduce(): n must be 1 to 50 positive integers.", call. = FALSE)
    }
    n <- vapply(seq_along(n), function(i) .v2_int(n[[i]], "omop_reduce(): n", min = 1L), 1L)
    if (length(n) > 1L && !identical(fn, "nth_value")) {
      stop("omop_reduce(): a vector n is used only with fn = \"nth_value\".", call. = FALSE)
    }
  }
  side <- .v2_choice(side, c("either", "before", "after"), "omop_reduce(): side")
  missing <- .v2_choice(missing, c("skip", "keep"), "omop_reduce(): missing")
  distinct <- if (is.null(distinct)) NULL else .v2_distinct(distinct, "omop_reduce(): distinct")
  if (!is.null(p) && !.v2_is_param(p)) {
    p <- .v2_number(p, "omop_reduce(): p")
    if (p <= 0 || p > 1) stop("omop_reduce(): p must be in (0, 1].", call. = FALSE)
  }
  agg <- if (is.null(agg)) NULL else .v2_choice(agg, c("mean", "sum", "max"), "omop_reduce(): agg")
  reference <- if (is.null(reference)) NULL else .v2_anchor_arg(reference, "omop_reduce(): reference")
  predicate <- .v2_predicate_arg(predicate, "omop_reduce(): predicate")
  reference_date <- .v2_date(reference_date, "omop_reduce(): reference_date")
  unit <- if (is.null(unit)) NULL else .v2_choice(unit, c("day", "month"), "omop_reduce(): unit")
  coverage <- .v2_choice(coverage, c("recorded", "effective_supply"), "omop_reduce(): coverage")
  fills <- .v2_choice(fills, c("start_in_window", "clipped"), "omop_reduce(): fills")
  observed_only <- .v2_flag(observed_only, "omop_reduce(): observed_only")

  # Shape rules of the reducer contract that need no recipe context.
  if (!identical(fn, "covered_days") &&
      (!identical(coverage, "recorded") || isTRUE(observed_only))) {
    stop("omop_reduce(): coverage and observed_only apply only to covered_days.",
         call. = FALSE)
  }
  if (!identical(fn, "supply_days") && !identical(fills, "start_in_window")) {
    stop("omop_reduce(): fills applies only to supply_days.", call. = FALSE)
  }
  if (identical(fn, "quantile") && is.null(p)) {
    stop("omop_reduce(): quantile needs p.", call. = FALSE)
  }
  if (!identical(fn, "quantile") && !is.null(p)) {
    stop("omop_reduce(): p is used only with quantile.", call. = FALSE)
  }
  if (fn %in% c("any", "all") && is.null(predicate)) {
    stop("omop_reduce(): ", fn, " needs a predicate.", call. = FALSE)
  }
  if (!fn %in% c("any", "all") && !is.null(predicate)) {
    stop("omop_reduce(): predicate is used only with any/all.", call. = FALSE)
  }
  if (fn %in% c("days_since", "days_until") && is.null(reference)) {
    stop("omop_reduce(): ", fn, " needs a reference anchor.", call. = FALSE)
  }
  if (identical(fn, "time_since") && is.null(reference_date)) {
    stop("omop_reduce(): time_since needs a fixed reference_date (use days_since ",
         "with reference = \"index\" for index-relative recency).", call. = FALSE)
  }
  if (!identical(fn, "time_since") && (!is.null(reference_date) || !is.null(unit))) {
    stop("omop_reduce(): reference_date/unit are used only with time_since.",
         call. = FALSE)
  }
  if (!identical(fn, "drug_duration") && !is.null(agg)) {
    stop("omop_reduce(): agg is used only with drug_duration.", call. = FALSE)
  }
  if (!is.null(distinct) && !fn %in% c("count", "n_distinct")) {
    stop("omop_reduce(): distinct is used only with count/n_distinct.", call. = FALSE)
  }
  if (!identical(side, "either") && !identical(fn, "closest_value")) {
    stop("omop_reduce(): side is used only with closest_value.", call. = FALSE)
  }
  if (is.null(value) && fn %in% .V2_VALUE_REDUCERS) value <- "value"
  structure(list(fn = fn, value = value, window = window, which = which, n = n,
                 side = side, missing = missing, distinct = distinct, p = p, agg = agg,
                 reference = reference, predicate = predicate,
                 reference_date = reference_date, unit = unit, coverage = coverage,
                 fills = fills, observed_only = observed_only),
            class = c("omop_reducer", "list"))
}

#' @export
print.omop_reducer <- function(x, ...) {
  cat("omop_reducer:", x$fn, if (!is.null(x$window)) paste("over", .v2_window_text(x$window)),
      "\n")
  invisible(x)
}

# A v2 variable built by a helper: kind = "derived" or "population_column".
.v2_variable <- function(kind, ..., name = NULL) {
  structure(c(list(name = name, kind = kind), list(...)),
            class = c("omop_variable", "list"))
}

.v2_is_v2_variable <- function(v) inherits(v, "omop_variable") && !is.null(v$kind)

#' Person-derived and population-column variables
#'
#' Helpers that return ready variables (their name is taken from the list
#' element name in \code{omop_recipe(variables = list(age = omop_age()))}, or
#' a default).
#'
#' \code{omop_age()} is the annual age (reference year minus year of birth) at
#' the index date (or today / a fixed \code{reference_date} or \code{year});
#' \code{bands = TRUE} gives the legacy \code{age_group} on the server's
#' default grid or on \code{age_breaks} (any strictly increasing integer lower
#' bounds starting at 0). Exact age and exact birth components are ordinary
#' quasi-identifier columns on assign paths (released only through the final
#' minimum-persons rule). \code{omop_score()} computes a comorbidity score
#' (charlson, chads2, chadsvasc, dcsi, hfrs) over conditions on or before the
#' reference date (the index when \code{NULL}).
#'
#' @param reference \code{"index"} or \code{"today"} (today's date is
#'   recorded at construction so executions stay reproducible).
#' @param year Optional reference year (shorthand for \code{reference_date}).
#' @param reference_date Optional fixed ISO reference date.
#' @param bands Logical; legacy \code{age_group} bands.
#' @param age_breaks \code{NULL} or integer band lower bounds starting at 0.
#' @param width Birth-year band width (years).
#' @param before Optional day offset: died before index + \code{before}.
#' @param period_policy Observation-duration policy.
#' @param window An \code{omop_window()} for \code{omop_observed_days()}.
#' @param type Score type.
#' @param name Population column: \code{"index_source"}, \code{"set_id"},
#'   \code{"is_case"} or \code{"sampling_weight"}.
#' @return An \code{omop_variable}.
#' @examples
#' omop_age(); omop_age(bands = TRUE, age_breaks = c(0, 18, 65))
#' omop_score("charlson"); omop_observed_days(omop_window(-365, 0))
#' @export
omop_age <- function(reference = c("index", "today"), year = NULL, reference_date = NULL,
                     bands = FALSE, age_breaks = NULL) {
  reference <- .v2_choice(reference, c("index", "today"), "omop_age(): reference")
  if (!is.null(year)) {
    if (!is.null(reference_date)) {
      stop("omop_age(): give year or reference_date, not both.", call. = FALSE)
    }
    year <- .v2_int(year, "omop_age(): year", min = 1000L, max = 9999L)
    reference_date <- sprintf("%04d-07-01", year)
  }
  if (is.null(reference_date) && identical(reference, "today")) {
    reference_date <- format(Sys.Date(), "%Y-%m-%d")
  }
  reference_date <- .v2_date(reference_date, "omop_age(): reference_date")
  bands <- .v2_flag(bands, "omop_age(): bands")
  if (!is.null(age_breaks) && !.v2_is_param(age_breaks)) {
    if (!isTRUE(bands)) stop("omop_age(): age_breaks needs bands = TRUE.", call. = FALSE)
    age_breaks <- .v2_age_breaks(age_breaks, "omop_age(): age_breaks")
  }
  .v2_variable("derived", derived = list(kind = "age", reference = reference,
                                         reference_date = reference_date, bands = bands,
                                         age_breaks = age_breaks))
}

.v2_age_breaks <- function(x, field) {
  if (!is.numeric(x) || length(x) < 2L || anyNA(x) || any(!is.finite(x)) ||
      any(x != trunc(x)) || any(x < 0)) {
    stop(field, " must be at least two non-negative integers.", call. = FALSE)
  }
  x <- as.integer(x)
  if (x[[1L]] != 0L || any(diff(x) <= 0L)) {
    stop(field, " must be strictly increasing and start at 0.", call. = FALSE)
  }
  x
}

#' @rdname omop_age
#' @export
omop_sex <- function() .v2_variable("derived", derived = list(kind = "sex_mf"))

#' @rdname omop_age
#' @export
omop_race <- function() .v2_variable("derived", derived = list(kind = "race"))

#' @rdname omop_age
#' @export
omop_ethnicity <- function() .v2_variable("derived", derived = list(kind = "ethnicity"))

#' @rdname omop_age
#' @export
omop_birth_year_band <- function(width = 5) {
  .v2_variable("derived", derived = list(kind = "birth_year_band",
               width = .v2_int(width, "omop_birth_year_band(): width", min = 1L)))
}

#' @rdname omop_age
#' @export
omop_died <- function(before = NULL) {
  .v2_variable("derived", derived = list(kind = "died",
               before = .v2_int(before, "omop_died(): before", allow_null = TRUE)))
}

#' @rdname omop_age
#' @export
omop_days_to_death <- function() .v2_variable("derived", derived = list(kind = "days_to_death"))

#' @rdname omop_age
#' @export
omop_n_observation_periods <- function() {
  .v2_variable("derived", derived = list(kind = "n_observation_periods"))
}

#' @rdname omop_age
#' @export
omop_obs_duration <- function(period_policy = c("total", "first", "last", "longest")) {
  .v2_variable("derived", derived = list(kind = "obs_duration",
               period_policy = .v2_choice(period_policy, c("total", "first", "last", "longest"),
                                          "omop_obs_duration(): period_policy")))
}

#' @rdname omop_age
#' @export
omop_prior_obs <- function(reference_date) {
  if (missing(reference_date)) {
    stop("omop_prior_obs(): reference_date is required (fixed-date legacy semantics; ",
         "use omop_observed_days() for index-relative observation).", call. = FALSE)
  }
  .v2_variable("derived", derived = list(kind = "prior_obs",
               reference_date = .v2_date(reference_date, "omop_prior_obs(): reference_date",
                                         allow_null = FALSE),
               period_policy = "containing"))
}

#' @rdname omop_age
#' @export
omop_followup <- function(reference_date) {
  if (missing(reference_date)) {
    stop("omop_followup(): reference_date is required (fixed-date legacy semantics).",
         call. = FALSE)
  }
  .v2_variable("derived", derived = list(kind = "followup",
               reference_date = .v2_date(reference_date, "omop_followup(): reference_date",
                                         allow_null = FALSE),
               period_policy = "containing"))
}

#' @rdname omop_age
#' @export
omop_observed_days <- function(window) {
  if (missing(window) || !inherits(window, "omop_window")) {
    stop("omop_observed_days(): window must be an omop_window().", call. = FALSE)
  }
  .v2_variable("derived", derived = list(kind = "observed_days", window = window))
}

#' @rdname omop_age
#' @export
omop_score <- function(type, reference_date = NULL) {
  if (missing(type)) stop("omop_score(): type is required.", call. = FALSE)
  type <- .v2_choice(type, c("charlson", "chads2", "chadsvasc", "dcsi", "hfrs"),
                     "omop_score(): type")
  .v2_variable("derived", derived = list(kind = type,
               reference_date = .v2_date(reference_date, "omop_score(): reference_date")))
}

#' @rdname omop_age
#' @export
omop_population_column <- function(name = c("index_source", "set_id", "is_case",
                                             "sampling_weight")) {
  name <- .v2_choice(name, c("index_source", "set_id", "is_case", "sampling_weight"),
                     "omop_population_column(): name")
  .v2_variable("population_column", population_column = name)
}

# Default variable name for a helper variable that is not in a named list.
.v2_variable_default_name <- function(v) {
  if (identical(v$kind, "derived")) {
    k <- v$derived$kind
    if (identical(k, "age") && isTRUE(v$derived$bands)) return("age_group")
    if (identical(k, "sex_mf")) return("sex")
    return(k)
  }
  if (identical(v$kind, "population_column")) return(v$population_column)
  if (identical(v$kind, "reducer")) {
    src <- .v2_source_label(v$event)
    return(paste0(if (grepl(.V2_NAME_PATTERN, src)) src else "var", "_", v$reducer$fn))
  }
  "var"
}

# --- 2.8 Matching ----------------------------------------------------------------

#' Matched and nested case-control sampling
#'
#' A bounded, private, server-side risk-set sampler: for every case episode
#' (case time = the case's index date) eligible controls are the persons
#' satisfying the \code{controls} criteria population evaluated \emph{at the
#' case time}, alive, at risk (no outcome on or before the case time under
#' \code{prior_outcome = "exclude"}), in the same strata; controls are taken in
#' a seeded hash order. The result population has the episodes
#' \code{(person, index = case time)} with \code{set_id}, \code{is_case} and
#' \code{sampling_weight} (\code{\link{omop_population_column}}).
#'
#' @param cases Population id (or object) whose episodes are cases.
#' @param controls Criteria population id (or object) used as a template.
#' @param outcome Event giving the complete outcome history.
#' @param at \code{"index"}.
#' @param match List of \code{"sex"}, \code{"calendar_year"},
#'   \code{omop_age_band()} and \code{omop_strata()} items.
#' @param controls_per_case Controls per case.
#' @param prior_outcome,future_cases,replacement,insufficient,missing_strata
#'   Sampling policies (see the design).
#' @param seed Non-negative integer seed.
#' @param max_candidates Cap on the candidate pairs (exceeding it is a typed
#'   refusal, never a truncation).
#' @param width Age band width (years) at case time.
#' @param event An event (strata presence before case time).
#' @param before Strata: presence in the \code{before} days up to case time.
#' @param window Strata: an explicit window around case time.
#' @return \code{omop_match}, or a match item for \code{match}.
#' @export
omop_match <- function(cases, controls, outcome, at = "index", match = list(),
                       controls_per_case = 4L, prior_outcome = c("exclude", "allow"),
                       future_cases = c("eligible", "ineligible"),
                       replacement = c("between_sets", "none"), seed = 1L,
                       insufficient = c("keep_available", "drop_case"),
                       missing_strata = c("ineligible", "match_missing"),
                       max_candidates = 1e6) {
  if (missing(cases) || missing(controls) || missing(outcome)) {
    stop("omop_match(): cases, controls and outcome are required.", call. = FALSE)
  }
  pop_id <- function(x, field) {
    if (inherits(x, "omop_population")) x <- x$id
    .v2_name(x, field, allow_null = FALSE)
  }
  items <- if (inherits(match, "omop_match_item") || is.character(match)) list(match) else match
  if (!is.list(items)) stop("omop_match(): match must be a list.", call. = FALSE)
  items <- lapply(unname(items), function(it) {
    if (inherits(it, "omop_match_item") || .v2_is_param(it)) return(it)
    if (is.character(it) && length(it) == 1L && it %in% c("sex", "calendar_year")) return(it)
    stop("omop_match(): match items are \"sex\", \"calendar_year\", omop_age_band() ",
         "or omop_strata().", call. = FALSE)
  })
  structure(list(
    cases = pop_id(cases, "omop_match(): cases"),
    controls = pop_id(controls, "omop_match(): controls"),
    outcome = .v2_source(outcome, "omop_match(): outcome"),
    at = .v2_choice(at, "index", "omop_match(): at"),
    match = items,
    controls_per_case = .v2_int(controls_per_case, "omop_match(): controls_per_case", min = 1L),
    prior_outcome = .v2_choice(prior_outcome, c("exclude", "allow"), "omop_match(): prior_outcome"),
    future_cases = .v2_choice(future_cases, c("eligible", "ineligible"), "omop_match(): future_cases"),
    replacement = .v2_choice(replacement, c("between_sets", "none"), "omop_match(): replacement"),
    seed = .v2_int(seed, "omop_match(): seed", min = 0L),
    insufficient = .v2_choice(insufficient, c("keep_available", "drop_case"),
                              "omop_match(): insufficient"),
    missing_strata = .v2_choice(missing_strata, c("ineligible", "match_missing"),
                                "omop_match(): missing_strata"),
    max_candidates = .v2_int(max_candidates, "omop_match(): max_candidates", min = 1L)),
    class = c("omop_match", "list"))
}

#' @rdname omop_match
#' @export
omop_age_band <- function(width = 5L) {
  structure(list(kind = "age_band", width = .v2_int(width, "omop_age_band(): width", min = 1L)),
            class = c("omop_match_item", "list"))
}

#' @rdname omop_match
#' @export
omop_strata <- function(event, before = NULL, window = NULL) {
  if (missing(event)) stop("omop_strata(): event is required.", call. = FALSE)
  if (!is.null(before) && !is.null(window)) {
    stop("omop_strata(): give before or window, not both.", call. = FALSE)
  }
  if (!is.null(before)) {
    before <- .v2_int(before, "omop_strata(): before", min = 0L)
    window <- omop_window(-before, 0L)
  }
  structure(list(kind = "strata", event = .v2_source(event, "omop_strata(): event"),
                 window = .v2_window_arg(window, "omop_strata(): window")),
            class = c("omop_match_item", "list"))
}

#' @export
print.omop_match <- function(x, ...) {
  cat("omop_match: cases", x$cases, "controls", x$controls, "-",
      x$controls_per_case, "per case, seed", x$seed, "\n")
  invisible(x)
}

.v2_source_label_any <- function(s) {
  if (inherits(s, "omop_any_of")) {
    return(paste0("any of (", paste(vapply(s$arms, .v2_source_label, ""), collapse = ", "), ")"))
  }
  if (is.list(s) && identical(names(s), "cohort_definition_id")) {
    return(paste0("results cohort ", s$cohort_definition_id))
  }
  .v2_source_label(s)
}
