# Module: Recipe language 2.0 -- recipe-level registries (concept sets, events,
# intervals, publication roots) and OHDSI concept-set exchange
# (design-2.10.3 sections 2.1 and 2.10).

# Items of a registry argument: a single object, a list, or a named list whose
# names must agree with the objects' own names.
.v2_named_items <- function(x, class, arg) {
  if (is.null(x)) return(list())
  items <- if (inherits(x, class)) list(x) else x
  if (!is.list(items)) stop("omop_recipe(): ", arg, " must be a list.", call. = FALSE)
  nms <- names(items)
  lapply(seq_along(items), function(i) {
    it <- items[[i]]
    if (!inherits(it, class)) {
      stop("omop_recipe(): ", arg, " must hold ", class, " objects.", call. = FALSE)
    }
    nm <- if (!is.null(nms) && nzchar(nms[[i]])) nms[[i]] else NULL
    if (!is.null(nm)) {
      if (is.null(it$name)) it$name <- .v2_name(nm, paste0(arg, " name"))
      else if (!identical(it$name, nm)) {
        stop("omop_recipe(): ", arg, " element '", nm, "' is named '", it$name,
             "' by its constructor; use one name.", call. = FALSE)
      }
    }
    it
  })
}

.v2_recipe_check <- function(recipe) {
  if (!inherits(recipe, "omop_recipe")) {
    stop("recipe must be an omop_recipe object", call. = FALSE)
  }
}

#' Language-2 recipe registries
#'
#' \code{recipe_add_concept_set()}, \code{recipe_add_event()} and
#' \code{recipe_add_interval()} declare named nodes that other objects
#' reference by name; \code{recipe_set_publish()} sets the publication roots.
#' (The parameter layer exports the complete set of editing functions.)
#'
#' @param recipe An \code{omop_recipe}.
#' @param concept_set,event,interval The object to declare (it must be named,
#'   except events and intervals, which get a deterministic name when compiled).
#' @param name Name of the node to remove.
#' @param publish Character vector of output names, \code{character(0)} for none
#'   (every output is then a private computed input), or \code{NULL} for all.
#' @return The modified recipe.
#' @keywords internal
recipe_add_concept_set <- function(recipe, concept_set) {
  .v2_recipe_check(recipe)
  if (!inherits(concept_set, "omop_concept_set")) {
    stop("concept_set must be an omop_concept_set object.", call. = FALSE)
  }
  if (is.null(concept_set$name)) {
    stop("A registry concept set needs a name (omop_concept_set(name = )).",
         call. = FALSE)
  }
  recipe$concept_sets <- recipe$concept_sets %||% list()
  if (!is.null(recipe$concept_sets[[concept_set$name]]) &&
      !identical(recipe$concept_sets[[concept_set$name]], concept_set)) {
    stop("Concept set '", concept_set$name, "' is already declared differently.",
         call. = FALSE)
  }
  recipe$concept_sets[[concept_set$name]] <- concept_set
  recipe$meta$modified <- Sys.time()
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_add_event <- function(recipe, event) {
  .v2_recipe_check(recipe)
  if (!inherits(event, "omop_event")) stop("event must be an omop_event object.", call. = FALSE)
  recipe$events <- c(recipe$events %||% list(), list(event))
  nms <- vapply(recipe$events, function(e) e$name %||% "", "")
  names(recipe$events) <- nms
  if (anyDuplicated(nms[nzchar(nms)])) {
    dup <- unique(nms[nzchar(nms)][duplicated(nms[nzchar(nms)])])
    same <- all(vapply(dup, function(d) {
      hits <- recipe$events[nms == d]
      all(vapply(hits, identical, logical(1), hits[[1L]]))
    }, logical(1)))
    if (!same) stop("Event name(s) declared twice with different definitions: ",
                    paste(dup, collapse = ", "), ".", call. = FALSE)
    recipe$events <- recipe$events[!duplicated(nms) | !nzchar(nms)]
  }
  recipe$meta$modified <- Sys.time()
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_add_interval <- function(recipe, interval) {
  .v2_recipe_check(recipe)
  if (!inherits(interval, "omop_interval")) {
    stop("interval must be an omop_era() object.", call. = FALSE)
  }
  recipe$intervals <- c(recipe$intervals %||% list(), list(interval))
  nms <- vapply(recipe$intervals, function(e) e$name %||% "", "")
  names(recipe$intervals) <- nms
  if (anyDuplicated(nms[nzchar(nms)])) {
    stop("Interval name(s) declared twice: ",
         paste(unique(nms[nzchar(nms)][duplicated(nms[nzchar(nms)])]), collapse = ", "),
         ".", call. = FALSE)
  }
  recipe$meta$modified <- Sys.time()
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_remove_concept_set <- function(recipe, name) {
  .v2_recipe_check(recipe)
  recipe$concept_sets[[name]] <- NULL
  if (length(recipe$concept_sets) == 0L) recipe$concept_sets <- NULL
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_remove_event <- function(recipe, name) {
  .v2_recipe_check(recipe)
  keep <- !vapply(recipe$events %||% list(), function(e) identical(e$name, name), logical(1))
  recipe$events <- recipe$events[keep]
  if (length(recipe$events) == 0L) recipe$events <- NULL
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_remove_interval <- function(recipe, name) {
  .v2_recipe_check(recipe)
  keep <- !vapply(recipe$intervals %||% list(), function(e) identical(e$name, name), logical(1))
  recipe$intervals <- recipe$intervals[keep]
  if (length(recipe$intervals) == 0L) recipe$intervals <- NULL
  recipe
}

#' @rdname recipe_add_concept_set
#' @keywords internal
recipe_set_publish <- function(recipe, publish) {
  .v2_recipe_check(recipe)
  if (is.null(publish)) {
    recipe$publish <- NULL
    return(recipe)
  }
  # An explicitly empty root list publishes nothing (every output is a private
  # computed input); NULL keeps the default, every declared output.
  if (length(publish) == 0L && (is.character(publish) || is.list(publish))) {
    recipe$publish <- character(0)
    return(recipe)
  }
  publish <- .v2_chr_vector(publish, "publish")
  for (p in publish) .v2_name(p, "publish")
  recipe$publish <- publish
  recipe
}

# --- concept-set registry exchange (OHDSI concept-set JSON) -----------------------

#' Concept sets of a recipe; OHDSI concept-set exchange
#'
#' \code{recipe_concept_sets()} lists the recipe's registry concept sets.
#' \code{recipe_export_concept_sets()} writes them as OHDSI (ATLAS)
#' concept-set expressions; \code{recipe_import_concept_sets()} reads such
#' expressions into the registry. Items must share their descendant/mapped
#' flags; excluded items become the set's \code{exclude} (exact IDs, or a set
#' with descendants when the excluded items include descendants). A nested
#' exclusion (an excluded set with its own exclusion) has no OHDSI form and is
#' refused on export rather than flattened.
#'
#' @param recipe An \code{omop_recipe}.
#' @param file Optional path; \code{NULL} returns the JSON string.
#' @param x A JSON string or file with one concept-set expression
#'   (\code{list(items = ...)}), a named list of them, or an ATLAS
#'   \code{ConceptSets} array (elements with \code{name} and
#'   \code{expression}).
#' @return A named list of \code{omop_concept_set} objects, a JSON string, or
#'   the modified recipe.
#' @export
recipe_concept_sets <- function(recipe) {
  .v2_recipe_check(recipe)
  recipe$concept_sets %||% list()
}

#' @rdname recipe_concept_sets
#' @export
recipe_export_concept_sets <- function(recipe, file = NULL) {
  .v2_recipe_check(recipe)
  sets <- recipe$concept_sets %||% list()
  arr <- lapply(seq_along(sets), function(i) {
    cs <- sets[[i]]
    items <- lapply(cs$concepts, function(id) list(
      concept = list(CONCEPT_ID = id), isExcluded = FALSE,
      includeDescendants = isTRUE(cs$descendants), includeMapped = isTRUE(cs$mapped)))
    if (!is.null(cs$exclude)) {
      ex <- cs$exclude
      # An OHDSI expression has one level of exclusion: an exclusion with its
      # own exclusion has no equivalent there and is never flattened.
      if (inherits(ex, "omop_concept_set") && !is.null(ex$exclude)) {
        stop("recipe_export_concept_sets(): concept set '", names(sets)[[i]], "' excludes ",
             "a concept set that has its own exclusion (a nested exclusion); an OHDSI ",
             "concept-set expression cannot carry it. Export would change the concept ",
             "definition, so it is refused; spell the effective set with one exclusion.",
             call. = FALSE)
      }
      ex_ids <- if (inherits(ex, "omop_concept_set")) ex$concepts else ex
      ex_desc <- inherits(ex, "omop_concept_set") && isTRUE(ex$descendants)
      ex_map <- inherits(ex, "omop_concept_set") && isTRUE(ex$mapped)
      items <- c(items, lapply(ex_ids, function(id) list(
        concept = list(CONCEPT_ID = id), isExcluded = TRUE,
        includeDescendants = ex_desc, includeMapped = ex_map)))
    }
    list(id = i - 1L, name = names(sets)[[i]], expression = list(items = items))
  })
  json <- as.character(jsonlite::toJSON(arr, auto_unbox = TRUE, pretty = TRUE,
                                        null = "null", digits = NA))
  if (is.null(file)) return(json)
  writeLines(json, file)
  invisible(file)
}

#' @rdname recipe_concept_sets
#' @export
recipe_import_concept_sets <- function(recipe, x) {
  .v2_recipe_check(recipe)
  if (length(x) == 1L && is.character(x) && file.exists(x)) {
    x <- paste(readLines(x, warn = FALSE), collapse = "\n")
  }
  data <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  entries <- if (!is.null(data$items)) {
    list(list(name = "concept_set_1", expression = data))
  } else if (is.null(names(data))) {
    data
  } else {
    lapply(names(data), function(nm) list(name = nm, expression = data[[nm]]))
  }
  for (e in entries) {
    expr <- e$expression %||% e
    cs <- .v2_concept_set_from_ohdsi(expr, .v2_truncate_name(.sanitize_name(e$name %||% "concept_set")))
    recipe <- recipe_add_concept_set(recipe, cs)
  }
  recipe
}

# One OHDSI concept-set expression as an omop_concept_set.
.v2_concept_set_from_ohdsi <- function(expr, name) {
  items <- expr$items %||% list()
  if (length(items) == 0L) stop("Concept-set expression '", name, "' has no items.", call. = FALSE)
  flag <- function(it, f) isTRUE(it[[f]])
  incl <- Filter(function(it) !flag(it, "isExcluded"), items)
  excl <- Filter(function(it) flag(it, "isExcluded"), items)
  ids_of <- function(lst) vapply(lst, function(it) {
    id <- suppressWarnings(as.integer(it$concept$CONCEPT_ID))
    if (length(id) != 1L || is.na(id) || id < 0L) {
      stop("Concept-set '", name, "': CONCEPT_ID must be a non-negative integer.", call. = FALSE)
    }
    id
  }, integer(1))
  uniform <- function(lst, f) {
    v <- unique(vapply(lst, flag, logical(1), f = f))
    if (length(v) > 1L) {
      stop("Concept-set '", name, "': ", f, " must be the same for every ",
           if (identical(lst, excl)) "excluded" else "included", " item.", call. = FALSE)
    }
    isTRUE(v[1L])
  }
  if (length(incl) == 0L) stop("Concept-set '", name, "' has only excluded items.", call. = FALSE)
  exclude <- NULL
  if (length(excl)) {
    ex_desc <- uniform(excl, "includeDescendants")
    ex_map <- uniform(excl, "includeMapped")
    exclude <- if (ex_desc || ex_map) {
      omop_concept_set(ids_of(excl), descendants = ex_desc, mapped = ex_map)
    } else ids_of(excl)
  }
  omop_concept_set(ids_of(incl), descendants = uniform(incl, "includeDescendants"),
                   mapped = uniform(incl, "includeMapped"), exclude = exclude, name = name)
}
