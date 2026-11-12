# Module: Recipe language 2.0 -- the simple layer (design-2.10.3 section 3)
#
# Every simple-layer call returns advanced-layer objects: omop_study() an
# omop_recipe, omop_new_users() an omop_population, the shortcuts reducer
# variables (omop_variable(event =, reducer =)) and omop_history() a named
# list of them. Nothing here has its own wire form or server semantics; each
# expansion is the advanced recipe written out by hand (tests compare the two
# plans), print() lists the defaults a study chose and recipe_to_code() prints
# the expansion.

# --- windows of the shortcuts --------------------------------------------------------

# before = n -> [-n, 0], after = n -> [1, n], window = an omop_window(); none
# of them -> any time up to and including the index date ([-Inf, 0]).
# before/after accept Inf for an unbounded side. They are mutually exclusive.
.simple_window <- function(before, after, window, ctx) {
  given <- c(before = !is.null(before), after = !is.null(after), window = !is.null(window))
  if (sum(given) > 1L) {
    stop(ctx, ": before, after and window are mutually exclusive; give one (got ",
         paste(names(given)[given], collapse = " and "), ").", call. = FALSE)
  }
  if (!is.null(window)) {
    if (!inherits(window, "omop_window") && !.v2_is_param(window)) {
      stop(ctx, ": window must be an omop_window().", call. = FALSE)
    }
    return(window)
  }
  if (!is.null(before)) {
    if (.v2_is_param(before)) return(omop_window(-before, 0L))
    if (is.numeric(before) && length(before) == 1L && identical(as.numeric(before), Inf)) {
      return(omop_window(NULL, 0L))
    }
    before <- .v2_int(before, paste0(ctx, ": before"), min = 0L)
    return(omop_window(-before, 0L))
  }
  if (!is.null(after)) {
    if (.v2_is_param(after)) return(omop_window(1L, after))
    if (is.numeric(after) && length(after) == 1L && identical(as.numeric(after), Inf)) {
      return(omop_window(1L, NULL))
    }
    after <- .v2_int(after, paste0(ctx, ": after"), min = 1L)
    return(omop_window(1L, after))
  }
  omop_window(NULL, 0L)
}

# The event a shortcut reads: an omop_event()/omop_era() given as `table`, or
# omop_event(table, concepts, ...) built from a table name.
.simple_event <- function(table, concepts, dots, ctx, unit = NULL, value = NULL) {
  if (missing(table) || is.null(table)) stop(ctx, ": table is required.", call. = FALSE)
  if (.v2_is_event(table) || .v2_is_interval(table)) {
    if (!is.null(concepts) || length(dots) || !is.null(unit) || !is.null(value)) {
      stop(ctx, ": an event object carries its own concepts and options; give them to ",
           "omop_event() instead.", call. = FALSE)
    }
    return(table)
  }
  if (!is.character(table) || length(table) != 1L || is.na(table)) {
    stop(ctx, ": table must be one OMOP table name, or an omop_event() / omop_era().",
         call. = FALSE)
  }
  args <- c(list(table, concepts), dots)
  if (!is.null(value)) args$value <- value
  if (!is.null(unit)) {
    if (inherits(unit, "omop_unit") || .v2_is_param(unit)) {
      args$unit <- unit
    } else {
      # A numeric unit is a unit-restricted event, never an implicit
      # conversion (design section 3); conversions are given with omop_unit().
      # The literal is the one omop_where(unit_concept_id == U) captures.
      ids <- as.numeric(.v2_ids(unit, paste0(ctx, ": unit")))
      restriction <- if (length(ids) == 1L) {
        list(var = "unit_concept_id", op = "==", value = ids)
      } else {
        list(var = "unit_concept_id", op = "in", value = as.list(ids))
      }
      if (!is.null(args$where)) {
        stop(ctx, ": give a numeric unit or a where predicate, not both (add the unit ",
             "restriction to the predicate).", call. = FALSE)
      }
      args$where <- omop_where_tree(restriction)
    }
  }
  do.call(omop_event, args)
}

.simple_variable <- function(event, reducer) {
  omop_variable(event = event, reducer = reducer)
}

#' Simple-layer variables (shortcuts)
#'
#' Each shortcut returns an ordinary language-2 variable
#' (\code{omop_variable(event = omop_event(table, concepts, ...), reducer =
#' omop_reduce(...))}); its name comes from the list it is placed in
#' (\code{baseline = list(hba1c = omop_value_at_index(...))}). The windows are
#' relative to the index date: \code{before = n} is \code{omop_window(-n, 0)},
#' \code{after = n} is \code{omop_window(1, n)}, \code{window} is any
#' \code{\link{omop_window}}; the three are mutually exclusive, and without any
#' of them the window is any time up to and including the index date
#' (\code{omop_window(NULL, 0)}). \code{Inf} leaves the far side unbounded.
#'
#' \itemize{
#'   \item \code{omop_ever()}: 1 if the event occurs in the window, else 0
#'     (reducer \code{binary}).
#'   \item \code{omop_count()}: number of rows (or distinct days, visits,
#'     concepts) in the window.
#'   \item \code{omop_first_value()}, \code{omop_last_value()},
#'     \code{omop_mean_value()}, \code{omop_min_value()},
#'     \code{omop_max_value()}: the normalised value of the first / last row in
#'     the window, or its mean, minimum or maximum.
#'   \item \code{omop_value_at_index()}: the value closest to the index date
#'     within \code{tolerance} days (reducer \code{closest_value}).
#'   \item \code{omop_days_since()} / \code{omop_days_until()}: days from the
#'     last row on or before / the first row on or after the index date.
#'   \item \code{omop_first_date()} / \code{omop_last_date()}: day of the
#'     first / last row at any time, relative to the index date.
#' }
#' A numeric \code{unit = U} restricts the event to rows recorded in unit
#' \code{U} (\code{unit_concept_id == U}); it never converts. Give an
#' \code{\link{omop_unit}} to convert values to one unit.
#'
#' @param table OMOP table name, or an \code{\link{omop_event}} /
#'   \code{\link{omop_era}} (then \code{concepts}, \code{unit}, \code{value} and
#'   \code{...} are not used).
#' @param concepts Concept IDs, an \code{\link{omop_concept_set}}, the name of
#'   a registry concept set, or \code{NULL} for every row of the table.
#' @param before,after Days before (up to and including the index date) or
#'   after (from the day after the index date); \code{Inf} for no limit.
#' @param window An \code{\link{omop_window}}.
#' @param distinct What \code{omop_count()} counts: \code{"row"},
#'   \code{"day"}, \code{"visit"} or \code{"concept"}.
#' @param value Numeric value column of the event (default: the table's value
#'   role, \code{value_as_number}).
#' @param unit A unit concept ID (restriction) or an \code{\link{omop_unit}}
#'   (conversion).
#' @param tolerance Window \code{c(start, end)} around the index date.
#' @param ... Further \code{\link{omop_event}} arguments (\code{descendants},
#'   \code{mapped}, \code{exclude}, \code{where}, \code{visit}, \code{value},
#'   \code{unit}, \code{concept_col}, \code{end}, \code{name},
#'   \code{on_missing_table}).
#' @return An \code{omop_variable}.
#' @examples
#' omop_ever("condition_occurrence", 201826, descendants = TRUE, before = 365)
#' omop_count("visit_occurrence", 9201, before = 365)
#' omop_value_at_index("measurement", 3004410, unit = 8554)
#' @seealso \code{\link{omop_study}}, \code{\link{omop_reduce}}
#' @export
omop_ever <- function(table, concepts = NULL, before = NULL, after = NULL, window = NULL, ...) {
  ctx <- "omop_ever()"
  ev <- .simple_event(table, concepts, list(...), ctx)
  .simple_variable(ev, omop_reduce("binary", window = .simple_window(before, after, window, ctx)))
}

#' @rdname omop_ever
#' @export
omop_count <- function(table, concepts = NULL, before = NULL, after = NULL, window = NULL,
                       distinct = "row", ...) {
  ctx <- "omop_count()"
  distinct <- .v2_distinct(distinct, paste0(ctx, ": distinct"))
  ev <- .simple_event(table, concepts, list(...), ctx)
  w <- .simple_window(before, after, window, ctx)
  red <- if (identical(distinct, "row")) omop_reduce("count", window = w) else
    omop_reduce("count", window = w, distinct = distinct)
  .simple_variable(ev, red)
}

.simple_value <- function(fn, ctx, table, concepts, value, unit, before, after, window) {
  ev <- .simple_event(table, concepts, list(), ctx, unit = unit, value = value)
  .simple_variable(ev, omop_reduce(fn, window = .simple_window(before, after, window, ctx)))
}

#' @rdname omop_ever
#' @export
omop_first_value <- function(table, concepts, value = NULL, unit = NULL, before = NULL,
                             after = NULL, window = NULL) {
  if (missing(concepts)) concepts <- NULL
  .simple_value("first_value", "omop_first_value()", table, concepts, value, unit, before,
                after, window)
}

#' @rdname omop_ever
#' @export
omop_last_value <- function(table, concepts, value = NULL, unit = NULL, before = NULL,
                            after = NULL, window = NULL) {
  if (missing(concepts)) concepts <- NULL
  .simple_value("last_value", "omop_last_value()", table, concepts, value, unit, before,
                after, window)
}

#' @rdname omop_ever
#' @export
omop_mean_value <- function(table, concepts, value = NULL, unit = NULL, before = NULL,
                            after = NULL, window = NULL) {
  if (missing(concepts)) concepts <- NULL
  .simple_value("mean", "omop_mean_value()", table, concepts, value, unit, before, after, window)
}

#' @rdname omop_ever
#' @export
omop_min_value <- function(table, concepts, value = NULL, unit = NULL, before = NULL,
                           after = NULL, window = NULL) {
  if (missing(concepts)) concepts <- NULL
  .simple_value("min", "omop_min_value()", table, concepts, value, unit, before, after, window)
}

#' @rdname omop_ever
#' @export
omop_max_value <- function(table, concepts, value = NULL, unit = NULL, before = NULL,
                           after = NULL, window = NULL) {
  if (missing(concepts)) concepts <- NULL
  .simple_value("max", "omop_max_value()", table, concepts, value, unit, before, after, window)
}

#' @rdname omop_ever
#' @export
omop_value_at_index <- function(table, concepts, tolerance = c(-90, 30), unit = NULL) {
  ctx <- "omop_value_at_index()"
  if (missing(concepts)) concepts <- NULL
  if (.v2_is_param(tolerance) || !is.numeric(tolerance) || length(tolerance) != 2L) {
    stop(ctx, ": tolerance must be c(start, end) in days around the index date.",
         call. = FALSE)
  }
  w <- omop_window(tolerance[[1L]], tolerance[[2L]])
  ev <- .simple_event(table, concepts, list(), ctx, unit = unit)
  .simple_variable(ev, omop_reduce("closest_value", window = w))
}

#' @rdname omop_ever
#' @export
omop_days_since <- function(table, concepts) {
  if (missing(concepts)) concepts <- NULL
  ev <- .simple_event(table, concepts, list(), "omop_days_since()")
  .simple_variable(ev, omop_reduce("days_since", reference = "index"))
}

#' @rdname omop_ever
#' @export
omop_days_until <- function(table, concepts) {
  if (missing(concepts)) concepts <- NULL
  ev <- .simple_event(table, concepts, list(), "omop_days_until()")
  .simple_variable(ev, omop_reduce("days_until", reference = "index"))
}

#' @rdname omop_ever
#' @export
omop_first_date <- function(table, concepts) {
  if (missing(concepts)) concepts <- NULL
  ev <- .simple_event(table, concepts, list(), "omop_first_date()")
  .simple_variable(ev, omop_reduce("first_date"))
}

#' @rdname omop_ever
#' @export
omop_last_date <- function(table, concepts) {
  if (missing(concepts)) concepts <- NULL
  ev <- .simple_event(table, concepts, list(), "omop_last_date()")
  .simple_variable(ev, omop_reduce("last_date"))
}

#' Medical history covariates (simple layer)
#'
#' One binary variable per item: 1 when the condition, drug or procedure was
#' recorded in the \code{days_before} days up to and including the index date
#' (\code{omop_ever(table, item, before = days_before)}). Each argument is a
#' vector of concept IDs (one variable per ID; vector names name the
#' variables) or a named list of concept IDs / \code{\link{omop_concept_set}}s
#' (one variable per element). Unnamed items are named
#' \code{<condition|drug|procedure>_<concept_id>}.
#'
#' @param condition,drug,procedure Items over \code{condition_occurrence},
#'   \code{drug_exposure} and \code{procedure_occurrence}.
#' @param days_before Lookback in days (\code{Inf}: any time up to the index).
#' @return A named list of \code{omop_variable}s, for
#'   \code{omop_study(baseline = )} or \code{omop_recipe(variables = )}.
#' @examples
#' omop_history(condition = c(diabetes = 201820, hypertension = 320128),
#'              drug = list(statins = omop_concept_set(1539403, descendants = TRUE)))
#' @seealso \code{\link{omop_ever}}, \code{\link{omop_study}}
#' @export
omop_history <- function(condition = NULL, drug = NULL, procedure = NULL, days_before = 365L) {
  ctx <- "omop_history()"
  if (!(is.numeric(days_before) && length(days_before) == 1L &&
        identical(as.numeric(days_before), Inf))) {
    days_before <- .v2_int(days_before, paste0(ctx, ": days_before"), min = 0L)
  }
  domains <- list(condition = list(condition, "condition_occurrence"),
                  drug = list(drug, "drug_exposure"),
                  procedure = list(procedure, "procedure_occurrence"))
  out <- list()
  for (d in names(domains)) {
    items <- domains[[d]][[1L]]
    if (is.null(items)) next
    table <- domains[[d]][[2L]]
    if (inherits(items, "omop_concept_set")) items <- list(items)
    if (is.numeric(items)) {
      ids <- .v2_ids(items, paste0(ctx, ": ", d))
      nms <- names(items)
      items <- as.list(ids)
      names(items) <- if (!is.null(nms) && length(nms) == length(ids)) nms else NULL
    }
    if (!is.list(items) || length(items) == 0L) {
      stop(ctx, ": ", d, " must be concept IDs or a list of concept IDs / concept sets.",
           call. = FALSE)
    }
    nms <- names(items) %||% rep("", length(items))
    for (i in seq_along(items)) {
      it <- items[[i]]
      nm <- nms[[i]]
      if (!nzchar(nm)) {
        key <- if (inherits(it, "omop_concept_set")) {
          it$name %||% (if (length(it$concepts)) it$concepts[[1L]] else "empty")
        } else paste(utils::head(unlist(it), 1L))
        nm <- paste0(d, "_", key)
      }
      nm <- .v2_name(nm, paste0(ctx, ": variable name"), allow_null = FALSE)
      if (nm %in% names(out)) stop(ctx, ": variable name '", nm, "' is used twice.", call. = FALSE)
      out[[nm]] <- omop_ever(table, it, before = days_before)
    }
  }
  if (length(out) == 0L) stop(ctx, ": give condition, drug and/or procedure items.", call. = FALSE)
  out
}

# --- new users -------------------------------------------------------------------------

#' New users of a drug (simple layer)
#'
#' The population of first-time users after a washout: drug exposures of
#' \code{drug} are grouped into eras (\code{\link{omop_era}}, 30-day gap,
#' \code{by}), the index is the era start selected by \code{select}
#' (default \code{"first_eligible"}: the earliest era that meets the
#' criteria), the episode ends at the era end (\code{omop_end("interval_end")}),
#' and the criteria require \code{washout_days} days of prior observation
#' (\code{omop_observed(before = washout_days)}) and no exposure in the
#' \code{washout_days} days before the index (\code{omop_has_not(rx, window =
#' omop_window(-washout_days, -1))}). \code{washout_days = 0} adds no criteria.
#' A parameterized washout is expanded after binding or default resolution;
#' zero keeps independently supplied population and study requirements.
#'
#' @param drug Concept IDs, an \code{\link{omop_concept_set}}, the name of a
#'   registry concept set, or an \code{omop_event()} over
#'   \code{drug_exposure}.
#' @param washout_days Washout in days (a non-negative integer).
#' @param select Index selection (see \code{\link{omop_index}}).
#' @param by Era grouping: \code{"ingredient"} (default), \code{"concept"} or
#'   \code{"event"}.
#' @return An \code{omop_population} with id \code{"new_users"} (rename with
#'   \code{p$id <- ...}); \code{\link{omop_study}} uses it as its index and
#'   ANDs its own requirements.
#' @examples
#' metf <- omop_concept_set(1503297, descendants = TRUE, name = "metformin")
#' omop_new_users(metf, washout_days = 365)
#' @seealso \code{\link{omop_study}}, \code{\link{omop_era}}, \code{\link{omop_index}}
#' @export
omop_new_users <- function(drug, washout_days = 365L, select = "first_eligible",
                           by = "ingredient") {
  ctx <- "omop_new_users()"
  if (missing(drug)) stop(ctx, ": drug is required.", call. = FALSE)
  rx <- if (.v2_is_event(drug)) {
    if (!identical(drug$kind %||% "table", "table") || !identical(drug$table, "drug_exposure")) {
      stop(ctx, ": drug must be an event over drug_exposure.", call. = FALSE)
    }
    drug
  } else if (.v2_is_interval(drug)) {
    stop(ctx, ": drug is the exposure (concepts or an omop_event()); the eras are built ",
         "here.", call. = FALSE)
  } else {
    omop_event("drug_exposure", drug)
  }
  washout_days <- .v2_int(washout_days, paste0(ctx, ": washout_days"), min = 0L)
  by <- .v2_choice(by, c("event", "concept", "ingredient"), paste0(ctx, ": by"))
  era <- omop_era(rx, by = by)
  index <- omop_index(era, select = select, end = omop_end("interval_end"))
  filters <- .simple_washout_filters(rx, washout_days)
  pop <- omop_population("new_users", label = "New users", index = index, filters = filters)
  pop$simple <- list(new_users = list(washout_days = washout_days, event = rx,
                                     by = era$by, gap = era$gap, select = index$select,
                                     pending = .v2_is_param(washout_days)))
  pop
}

.simple_washout_filters <- function(rx, washout_days) {
  if (.v2_is_param(washout_days) || washout_days == 0L) return(list())
  list(omop_observed(before = washout_days),
       omop_has_not(rx, window = omop_window(-washout_days, -1L)))
}

# Retained expansion choices are client-side observability metadata, never
# emitted into the plan. Printing does not bind or change the population.
.simple_new_users_text <- function(pop) {
  spec <- pop$simple$new_users
  if (is.null(spec)) return(character())
  value <- function(x) if (.v2_is_param(x)) .v2_param_label(x) else as.character(x)
  w <- spec$washout_days
  chosen <- paste0("New users: washout ", value(w), " days; era grouping ", value(spec$by),
                   "; era gap ", value(spec$gap), " days; selection ", value(spec$select))
  if (.v2_is_param(w)) {
    detail <- paste0("Washout criteria deferred: observed before ", value(w),
                     "; exclusion window [-", value(w), ", -1]; zero adds neither")
  } else if (w == 0L) {
    detail <- "Washout criteria: none (zero washout)"
  } else {
    detail <- paste0("Washout criteria: observation window [-", w,
                     ", 0]; exclusion window [-", w, ", -1]")
  }
  c(chosen, detail)
}

# --- studies ---------------------------------------------------------------------------------

.STUDY_OUTPUTS <- c("baseline", "survival", "long", "panel")
.STUDY_FOLLOW_UP <- list(start = 1L, end = NULL, censor = c("observation_end", "death"))
.STUDY_REQUIRE_KEYS <- c("prior_observation", "followup", "age", "sex", "has",
                         "no_history_of", "in_cohort")

# Events of a `has` / `no_history_of` requirement: one event or a list.
.study_events <- function(x, key) {
  if (.v2_is_event(x) || .v2_is_interval(x) || is.character(x)) x <- list(x)
  if (!is.list(x) || length(x) == 0L) {
    stop("omop_study(): require$", key, " must be an event or a list of events.", call. = FALSE)
  }
  lapply(unname(x), .v2_source, field = paste0("omop_study(): require$", key))
}

# The criteria of a `require` list, in the order given.
.study_require <- function(require) {
  if (is.null(require)) return(list())
  if (inherits(require, "omop_criterion")) require <- list(require)
  if (!is.list(require) || inherits(require, c("omop_event", "omop_interval"))) {
    stop("omop_study(): require must be a list.", call. = FALSE)
  }
  nms <- names(require) %||% rep("", length(require))
  out <- list()
  for (i in seq_along(require)) {
    item <- require[[i]]
    key <- nms[[i]]
    if (inherits(item, "omop_criterion")) {
      out <- c(out, list(item))
      next
    }
    if (!nzchar(key)) {
      stop("omop_study(): an unnamed require entry must be a criterion (omop_has(), ",
           "omop_not(), omop_person(), ...).", call. = FALSE)
    }
    crit <- switch(key,
      prior_observation = list(omop_observed(before = .v2_int(item,
        "omop_study(): require$prior_observation", min = 0L))),
      followup = list(omop_observed(after = .v2_int(item, "omop_study(): require$followup",
                                                    min = 0L))),
      age = {
        if (!is.numeric(item) && !is.logical(item) || length(item) != 2L) {
          stop("omop_study(): require$age must be c(min, max), NA for an open bound.",
               call. = FALSE)
        }
        lo <- if (is.na(item[[1L]])) NULL else item[[1L]]
        hi <- if (is.na(item[[2L]])) NULL else item[[2L]]
        list(omop_age_is(min = lo, max = hi))
      },
      sex = {
        code <- if (is.character(item) && length(item) == 1L) {
          switch(toupper(item), F = , FEMALE = 8532, M = , MALE = 8507, NA)
        } else if (is.numeric(item) && length(item) == 1L && item %in% c(8507, 8532)) {
          as.numeric(item)
        } else NA
        if (is.na(code)) {
          stop("omop_study(): require$sex must be \"F\" or \"M\" (or 8532 / 8507).",
               call. = FALSE)
        }
        list(omop_person(omop_where_tree(list(var = "gender_concept_id", op = "==",
                                              value = code))))
      },
      has = lapply(.study_events(item, "has"), function(e) {
        omop_has(e, window = omop_window(NULL, 0L))
      }),
      no_history_of = lapply(.study_events(item, "no_history_of"), function(e) {
        omop_has_not(e, window = omop_window(NULL, -1L))
      }),
      in_cohort = list(omop_in_cohort(item)),
      stop("omop_study(): unknown require entry '", key, "' (known: ",
           paste(.STUDY_REQUIRE_KEYS, collapse = ", "),
           ", or any omop_has*/omop_not/omop_person criterion).", call. = FALSE))
    out <- c(out, crit)
  }
  out
}

# Baseline variables as a named list (nested lists, e.g. omop_history(), are
# spliced; an outer name prefixes the inner names).
.study_baseline <- function(baseline) {
  if (is.null(baseline) || length(baseline) == 0L) return(list())
  if (inherits(baseline, "omop_variable")) baseline <- list(baseline)
  if (!is.list(baseline)) stop("omop_study(): baseline must be a list of variables.", call. = FALSE)
  nms <- names(baseline) %||% rep("", length(baseline))
  out <- list()
  add <- function(v, nm) {
    if (!inherits(v, "omop_variable")) {
      stop("omop_study(): baseline entries must be variables (omop_age(), omop_ever(), ...).",
           call. = FALSE)
    }
    if (!nzchar(nm)) nm <- v$name %||% .v2_variable_default_name(v)
    if (nm %in% names(out)) {
      stop("omop_study(): baseline variable name '", nm, "' is used twice.", call. = FALSE)
    }
    # A 2.10.1-spelled variable carries its own name; the list name replaces it.
    if (!.v2_is_v2_variable(v)) v$name <- nm
    out[[nm]] <<- v
  }
  for (i in seq_along(baseline)) {
    item <- baseline[[i]]
    if (inherits(item, "omop_variable")) {
      add(item, nms[[i]])
    } else if (is.list(item)) {
      inner <- names(item) %||% rep("", length(item))
      for (j in seq_along(item)) {
        nm <- inner[[j]]
        if (nzchar(nms[[i]]) && nzchar(nm)) nm <- paste0(nms[[i]], "_", nm)
        add(item[[j]], nm)
      }
    } else {
      stop("omop_study(): baseline entries must be variables (omop_age(), omop_ever(), ...).",
           call. = FALSE)
    }
  }
  out
}

.study_outcomes <- function(outcomes) {
  if (is.null(outcomes) || length(outcomes) == 0L) return(list())
  if (.v2_is_event(outcomes) || .v2_is_interval(outcomes)) outcomes <- list(outcomes)
  if (!is.list(outcomes)) stop("omop_study(): outcomes must be a named list of events.", call. = FALSE)
  nms <- names(outcomes) %||% rep("", length(outcomes))
  for (i in seq_along(outcomes)) {
    o <- outcomes[[i]]
    if (!(.v2_is_event(o) || .v2_is_interval(o))) {
      stop("omop_study(): outcomes must be omop_event()s or omop_era()s.", call. = FALSE)
    }
    if (!nzchar(nms[[i]])) {
      if (is.null(o$name)) {
        stop("omop_study(): name every outcome (outcomes = list(mi = <event>)).", call. = FALSE)
      }
      nms[[i]] <- o$name
    }
    .v2_name(nms[[i]], "omop_study(): outcome name", allow_null = FALSE)
  }
  if (anyDuplicated(nms)) stop("omop_study(): outcome names must be unique.", call. = FALSE)
  stats::setNames(outcomes, nms)
}

.study_follow_up <- function(follow_up) {
  if (is.null(follow_up)) follow_up <- list()
  if (!is.list(follow_up) || (length(follow_up) && is.null(names(follow_up)))) {
    stop("omop_study(): follow_up must be list(start =, end =, censor =).", call. = FALSE)
  }
  unknown <- setdiff(names(follow_up), names(.STUDY_FOLLOW_UP))
  if (length(unknown)) {
    stop("omop_study(): unknown follow_up entr", if (length(unknown) > 1L) "ies " else "y ",
         paste(unknown, collapse = ", "), " (known: start, end, censor).", call. = FALSE)
  }
  fu <- .STUDY_FOLLOW_UP
  for (nm in names(follow_up)) fu[nm] <- list(follow_up[[nm]])
  censor <- fu$censor
  if (is.null(censor) || length(censor) == 0L) {
    stop("omop_study(): follow_up$censor must list at least one censoring source.",
         call. = FALSE)
  }
  if (is.character(censor)) censor <- as.list(censor)
  if (inherits(censor, c("omop_censor", "omop_event", "omop_interval"))) censor <- list(censor)
  fu$censor <- unname(censor)
  fu$tar <- omop_window(fu$start, fu$end)
  fu
}

#' A study in one call (simple layer)
#'
#' \code{omop_study()} returns an ordinary \code{omop_recipe}: one indexed
#' population (id \code{name}) and the requested outputs over it. It is
#' exactly the advanced recipe below (\code{recipe_to_code()} prints it;
#' \code{print()} lists every default the study chose):
#' \itemize{
#'   \item population: \code{omop_population(name, index = omop_index(index,
#'     select = select), filters = <require>, coverage = coverage)}; when
#'     \code{index} is an \code{omop_population} (e.g. from
#'     \code{\link{omop_new_users}}) or an \code{omop_index}, its index and
#'     criteria are kept and the requirements are ANDed after them.
#'   \item \code{require}: \code{prior_observation = n} is
#'     \code{omop_observed(before = n)}, \code{followup = n} is
#'     \code{omop_observed(after = n)}, \code{age = c(min, max)} is
#'     \code{omop_age_is(min, max)} (\code{NA} = open), \code{sex = "F"} /
#'     \code{"M"} is \code{omop_person(omop_where(gender_concept_id == 8532))}
#'     (8507), \code{has = list(<events>)} is \code{omop_has(e, window =
#'     omop_window(NULL, 0))} (any time up to the index date),
#'     \code{no_history_of = list(<events>)} is \code{omop_has_not(e, window
#'     = omop_window(NULL, -1))} (any time before the index date),
#'     \code{in_cohort = id} is \code{omop_in_cohort(id)}; criterion objects
#'     are used as they are; all in the order given.
#'   \item \code{"baseline"}: a \code{wide} output with episode grain and
#'     every baseline variable (nested lists such as \code{omop_history()} are
#'     spliced in).
#'   \item \code{"survival"}: a \code{survival} output (\code{competing_risk}
#'     with two or more outcomes) with \code{tar = omop_window(start, end)} and
#'     the \code{censor} list of \code{follow_up}.
#'   \item \code{"long"}: a \code{long} output with the outcome events as
#'     sources (relative dates).
#'   \item \code{"panel"}: a dense \code{person_period} output with 30-day bins
#'     over the follow-up window, counting the outcomes.
#' }
#' The simple layer never exposes grain, ties or date handling: every output
#' has episode grain, survival ties use the priority rule and dates are
#' relative to the index; \code{recipe_explain()} states them. A
#' \code{"survival"}, \code{"long"} or \code{"panel"} output without
#' \code{outcomes}, or a \code{"baseline"} output without \code{baseline}
#' variables, is a structural error.
#'
#' @param index An \code{omop_event}, \code{omop_era},
#'   \code{list(cohort_definition_id = )}, \code{omop_any_of()}, an
#'   \code{omop_index()} or an indexed \code{omop_population}.
#' @param select Index selection: \code{"first"} (default),
#'   \code{"first_eligible"}, \code{"all"} or \code{"last"}.
#' @param require Named list of requirements (see Details).
#' @param baseline Named list of baseline variables.
#' @param outcomes Named list of outcome events.
#' @param follow_up \code{list(start = 1, end = NULL, censor =
#'   c("observation_end", "death"))}; give only the entries to change.
#' @param outputs Subset of \code{"baseline"}, \code{"survival"},
#'   \code{"long"}, \code{"panel"}.
#' @param name Population name.
#' @param coverage Observation coverage of the population
#'   (\code{\link{omop_coverage}}).
#' @return An \code{omop_recipe}.
#' @examples
#' t2dm <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
#' metf <- omop_concept_set(1503297, descendants = TRUE, name = "metformin")
#' mi <- omop_concept_set(4329847, descendants = TRUE, name = "mi")
#' study <- omop_study(
#'   index    = omop_new_users(metf, washout_days = 365),
#'   require  = list(prior_observation = 365,
#'                   has = list(omop_event("condition_occurrence", t2dm))),
#'   baseline = list(age = omop_age(), sex = omop_sex(),
#'                   hba1c = omop_value_at_index("measurement", 3004410, unit = 8554),
#'                   inpatient = omop_count("visit_occurrence", 9201, before = 365),
#'                   charlson = omop_score("charlson")),
#'   outcomes = list(mi = omop_event("condition_occurrence", mi)),
#'   follow_up = list(start = 1, end = NULL, censor = c("observation_end", "death")))
#' print(study)
#' @seealso \code{\link{omop_new_users}}, \code{\link{omop_ever}},
#'   \code{\link{omop_recipe}}, \code{\link{recipe_to_code}}
#' @export
omop_study <- function(index, select = c("first", "first_eligible", "all", "last"),
                       require = list(), baseline = list(), outcomes = list(),
                       follow_up = list(start = 1L, end = NULL,
                                        censor = c("observation_end", "death")),
                       outputs = c("baseline", "survival"), name = "study",
                       coverage = omop_coverage()) {
  if (missing(index)) stop("omop_study(): index is required.", call. = FALSE)
  name <- .v2_name(name, "omop_study(): name", allow_null = FALSE)
  select_given <- !missing(select)
  outputs_given <- !missing(outputs)
  coverage_given <- !missing(coverage)
  follow_up_given <- if (missing(follow_up)) character(0) else names(follow_up)
  select <- .v2_choice(select, c("first", "first_eligible", "all", "last"),
                       "omop_study(): select")
  if (!inherits(coverage, "omop_coverage")) {
    stop("omop_study(): coverage must be created with omop_coverage().", call. = FALSE)
  }
  outputs <- unique(.v2_chr_vector(outputs, "omop_study(): outputs", unique = FALSE))
  bad <- setdiff(outputs, .STUDY_OUTPUTS)
  if (length(bad)) {
    stop("omop_study(): unknown output(s) ", paste(bad, collapse = ", "), " (outputs are a ",
         "subset of ", paste(.STUDY_OUTPUTS, collapse = ", "), ").", call. = FALSE)
  }
  criteria <- .study_require(require)
  vars <- .study_baseline(baseline)
  outs <- .study_outcomes(outcomes)
  fu <- .study_follow_up(follow_up)
  defaults <- character(0)
  note <- function(...) defaults <<- c(defaults, paste0(...))

  # The population.
  if (inherits(index, "omop_population")) {
    if (select_given) {
      stop("omop_study(): an omop_population index keeps its own selection; select ",
           "is not used.", call. = FALSE)
    }
    if (is.null(index[["index"]]) && is.null(index$index_event)) {
      stop("omop_study(): the population given as index has no index (omop_index()).",
           call. = FALSE)
    }
    if (!is.null(index$parent_id) || !is.null(index$setop) || !is.null(index$match)) {
      stop("omop_study(): the population given as index must stand alone (no parent, ",
           "set operation or matching).", call. = FALSE)
    }
    pop <- index
    pop$id <- name
    pop$filters <- c(pop$filters %||% list(), criteria)
    if (is.null(pop$coverage)) {
      pop$coverage <- coverage
      if (!coverage_given) note("observation coverage ", .study_coverage_text(coverage), " (default)")
    } else if (coverage_given && !identical(pop$coverage, coverage)) {
      stop("omop_study(): the population given as index has its own coverage; give one.",
           call. = FALSE)
    }
  } else {
    idx <- if (inherits(index, "omop_index")) {
      if (select_given) {
        stop("omop_study(): an omop_index() carries its own selection; select is not used.",
             call. = FALSE)
      }
      index
    } else {
      if (!select_given) note("index selection \"", select, "\" (default): the first ",
                              "candidate per person, selected before the requirements")
      omop_index(index, select = select)
    }
    pop <- omop_population(name, index = idx, filters = criteria, coverage = coverage)
    if (!coverage_given) note("observation coverage ", .study_coverage_text(coverage), " (default)")
  }

  # The outputs.
  out_objs <- list()
  need_outcomes <- function(o) {
    if (length(outs) == 0L) {
      stop("omop_study(): output \"", o, "\" needs outcomes (outcomes = list(<name> = ",
           "<event>)).", call. = FALSE)
    }
  }
  for (o in outputs) {
    out_objs[[o]] <- switch(o,
      baseline = {
        if (length(vars) == 0L) {
          stop("omop_study(): output \"baseline\" needs baseline variables (baseline = ",
               "list(...)).", call. = FALSE)
        }
        omop_output("baseline", rows = name, format = "wide", grain = "episode")
      },
      survival = {
        need_outcomes(o)
        fmt <- if (length(outs) > 1L) "competing_risk" else "survival"
        if (length(outs) > 1L) note("survival format \"competing_risk\": each episode's first ",
                                    "outcome is named (two or more outcomes)")
        omop_output("survival", rows = name, format = "survival", grain = "episode",
                    options = list(outcomes = outs, tar = fu$tar, censor = fu$censor,
                                   format = fmt))
      },
      long = {
        need_outcomes(o)
        omop_output("long", rows = name, format = "long", grain = "episode",
                    sources = unname(outs))
      },
      panel = {
        need_outcomes(o)
        note("panel: dense person-period rows with 30-day bins over the follow-up window")
        omop_output("panel", rows = name, format = "person_period", grain = "episode",
                    options = list(representation = "dense", bins = list(width = 30L),
                                   window = fu$tar, outcomes = outs))
      })
  }
  if (!outputs_given) note("outputs ", paste(outputs, collapse = ", "), " (default)")
  if (any(c("survival", "panel") %in% outputs)) {
    fu_given <- follow_up_given
    if (!"start" %in% fu_given || !"end" %in% fu_given) {
      note("follow-up ", .v2_window_text(fu$tar), if (is.null(fu$end)) " (unbounded end)",
           " (default ", paste(setdiff(c("start", "end"), fu_given), collapse = " and "), ")")
    }
    if ("survival" %in% outputs && !"censor" %in% fu_given) {
      note("censoring at ", paste(vapply(fu$censor, function(z) {
        if (is.character(z)) z else .v2_source_label(z$source %||% z)
      }, ""), collapse = ", "), " (default)")
    }
  }
  note("every output has episode grain; survival ties follow the priority rule; dates ",
       "are relative to the index date (simple-layer choices)")

  recipe <- omop_recipe(populations = list(pop), variables = vars, outputs = out_objs)
  recipe$meta$study <- list(name = name, defaults = defaults)
  recipe
}

.study_coverage_text <- function(cov) {
  paste0(cov$policy, "/", cov$on_ambiguous, "/", cov$on_missing,
         if (!identical(cov$gap, 0L)) paste0(" (gap ", cov$gap, ")"))
}
