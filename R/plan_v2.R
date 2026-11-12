# Module: Recipe language 2.0 -- compilation of a recipe to a v2 plan
# (design-2.10.3 sections 2, 4 and 5).
#
# recipe_to_plan(recipe, version = 2L) builds the wire plan of
# inst/schema/plan-v2.json: named registries of concept sets, events, intervals,
# populations, variables and outputs with typed references ({"event": name},
# {"interval": name}). Inline objects are interned under deterministic names;
# equal objects share a node; a name may not be reused for a different
# definition. The emitted plan is validated against the vendored schema before
# it is returned and is never sent when invalid.

.V2_LANGUAGE_VERSION <- "2.0"
.V2_SEMANTIC_VERSION <- "2.0.0"

.v2_schema_cache <- new.env(parent = emptyenv())

#' The vendored plan-v2 wire schema (parsed once per session)
#' @keywords internal
.v2_plan_schema <- function() {
  if (is.null(.v2_schema_cache$plan)) {
    .v2_schema_cache$plan <- .jsonSchemaPackaged("plan-v2", package = "dsOMOPClient")
  }
  .v2_schema_cache$plan
}

.v2_schema_def <- function(name) {
  def <- .v2_plan_schema()[["$defs"]][[name]]
  if (is.null(def)) stop("Unknown plan-v2 schema definition '", name, "'.", call. = FALSE)
  def
}

# --- small wire helpers ---------------------------------------------------------

.v2_empty_object <- function() structure(list(), names = character(0))

.v2_array <- function(x) {
  if (is.null(x)) return(list())
  if (is.list(x)) return(unname(x))
  unname(as.list(x))
}

# Drop NULL members except those listed in `keep` (kept as JSON null).
.v2_compact <- function(x, keep = character(0)) {
  drop <- vapply(names(x), function(nm) is.null(x[[nm]]) && !nm %in% keep, logical(1))
  out <- x[!drop]
  if (length(out) == 0L) return(.v2_empty_object())
  out
}

.v2_table_abbrev <- c(
  condition_occurrence = "dx", drug_exposure = "rx", procedure_occurrence = "px",
  measurement = "lab", observation = "obs", device_exposure = "device",
  visit_occurrence = "visit", visit_detail = "visit_detail", death = "death",
  specimen = "specimen", note = "note", episode = "episode",
  payer_plan_period = "payer_plan", observation_period = "observation_period",
  condition_era = "dx_era", drug_era = "rx_era", dose_era = "dose_era")

.v2_truncate_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  if (!grepl("^[A-Za-z]", x)) x <- paste0("n_", x)
  substr(x, 1L, 64L)
}

# --- compile context ------------------------------------------------------------

.v2_new_context <- function(recipe) {
  ctx <- new.env(parent = emptyenv())
  ctx$recipe <- recipe
  ctx$concept_sets <- list()      # name -> wire concept set
  ctx$events <- list()            # name -> wire event
  ctx$intervals <- list()         # name -> wire interval
  ctx$populations <- list()       # id -> wire population
  ctx$variables <- list()         # name -> wire variable
  ctx$outputs <- list()           # name -> wire output
  ctx$pop_objects <- recipe$populations %||% list()
  ctx$declared_events <- character(0)
  ctx$declared_intervals <- character(0)
  ctx$referenced_pops <- character(0)
  ctx$reserved_names <- character(0)
  ctx
}

.v2_stop <- function(...) stop(..., call. = FALSE)

.v2_param_value <- function(x, context) {
  # Parameter placeholders must be bound (recipe_bind()) before emission. A
  # placeholder that still carries a default is bound to that default.
  if (!is.null(x$default)) return(x$default)
  .v2_stop("Unbound parameter '", x$name %||% "?", "' in ", context,
           "; bind it with recipe_bind() before recipe_to_plan().")
}

# Replace every parameter placeholder by its value (default or error).
.v2_resolve_params <- function(x, context = "the recipe") {
  if (.v2_is_param(x)) return(.v2_resolve_params(.v2_param_value(x, context), context))
  if (is.list(x) && !is.data.frame(x)) {
    cls <- class(x)
    nms <- names(x)
    out <- lapply(x, .v2_resolve_params, context = context)
    # lapply drops explicit NULL members only when assigning; keep structure.
    names(out) <- nms
    class(out) <- cls
    return(out)
  }
  x
}

# --- concept sets -----------------------------------------------------------------

.v2w_concept_set_inline <- function(cs) {
  out <- list(concepts = .v2_array(cs$concepts),
              include_descendants = isTRUE(cs$descendants))
  if (isTRUE(cs$mapped)) out$include_mapped <- TRUE
  if (!is.null(cs$exclude)) {
    out$exclude <- if (inherits(cs$exclude, "omop_concept_set")) {
      .v2w_concept_set_inline(cs$exclude)
    } else {
      .v2_array(cs$exclude)
    }
  }
  out
}

.v2c_register <- function(ctx, slot, name, wire, what) {
  current <- ctx[[slot]][[name]]
  if (!is.null(current)) {
    if (!identical(.jcsCanonical(current), .jcsCanonical(wire))) {
      .v2_stop("Recipe ", what, " name '", name, "' is used for two different ",
               "definitions; rename one of them.")
    }
    return(invisible(name))
  }
  reg <- ctx[[slot]]
  reg[[name]] <- wire
  ctx[[slot]] <- reg
  invisible(name)
}

# Wire value of an event's concept set: a registry name or an inline object.
.v2c_concept_set <- function(ctx, cs, context) {
  if (is.null(cs)) return(NULL)
  if (.v2_is_param(cs)) cs <- .v2_param_value(cs, context)
  if (is.character(cs)) {
    if (is.null(ctx$concept_sets[[cs]])) {
      .v2_stop(context, " references concept set '", cs, "', which the recipe ",
               "does not declare (omop_recipe(concept_sets = ...)).")
    }
    return(cs)
  }
  if (!inherits(cs, "omop_concept_set")) {
    .v2_stop(context, ": invalid concept set.")
  }
  wire <- .v2w_concept_set_inline(cs)
  if (!is.null(cs$name)) {
    .v2c_register(ctx, "concept_sets", cs$name, wire, "concept set")
    return(cs$name)
  }
  wire
}

# --- events ------------------------------------------------------------------------

.v2w_unit <- function(u) {
  convert <- if (length(u$convert) == 0L) .v2_empty_object() else {
    stats::setNames(lapply(u$convert, function(spec) {
      list(factor = unname(spec[["factor"]]), offset = unname(spec[["offset"]]))
    }), names(u$convert))
  }
  list(target = u$target, convert = convert, unknown = u$unknown, censored = u$censored)
}

.v2w_predicate <- function(p) {
  if (is.null(p)) return(NULL)
  if (.v2_is_param(p)) p <- .v2_param_value(p, "a predicate")
  if (inherits(p, "omop_predicate")) return(p$tree)
  omop_where_tree(p)$tree
}

.v2_and_predicates <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
  .v2_pred_flatten("and", list(a, b))
}

# Tables whose role record has a numeric value role (design section 6.2).
.V2_VALUE_ROLE_TABLES <- c("measurement", "observation")

.v2w_event <- function(ctx, ev, context) {
  if (!identical(ev$kind %||% "table", "table")) {
    return(.v2w_adapter_event(ctx, ev, context))
  }
  out <- list(table = ev$table)
  cs <- .v2c_concept_set(ctx, ev$concepts, context)
  if (!is.null(cs)) out$concept_set <- cs
  if (!is.null(ev$concept_col)) out$concept_col <- ev$concept_col
  value <- ev$value
  if (is.null(value) && ev$table %in% .V2_VALUE_ROLE_TABLES) value <- "value_as_number"
  if (!is.null(value)) out$value <- list(column = value)
  if (!is.null(ev$unit)) {
    unit <- ev$unit
    if (.v2_is_param(unit)) unit <- .v2_param_value(unit, context)
    out$unit <- .v2w_unit(unit)
  }
  if (!is.null(ev$where)) out$where <- .v2w_predicate(ev$where)
  if (!is.null(ev$visit)) {
    out$visit <- if (identical(ev$visit, "index")) list(index = TRUE) else
      list(type = .v2_array(ev$visit$type))
  }
  if (!is.null(ev$end)) out$end <- ev$end
  if (identical(ev$on_missing_table, "unavailable")) out$on_missing_table <- "unavailable"
  out
}

.v2w_adapter_event <- function(ctx, ev, context) {
  if (identical(ev$kind, "episode_members")) {
    inner <- list(episode = list(event = .v2c_event(ctx, ev$episode, context)))
    if (!is.null(ev$tables)) inner$tables <- .v2_array(ev$tables)
    if (!is.null(ev$where)) inner$where <- .v2w_predicate(ev$where)
    if (identical(ev$on_missing_table, "unavailable")) inner$on_missing_table <- "unavailable"
    return(list(episode_members = inner))
  }
  if (identical(ev$kind, "cost")) {
    inner <- list(event = list(event = .v2c_event(ctx, ev$event, context)),
                  field = ev$field)
    if (!is.null(ev$currency)) inner$currency <- ev$currency
    if (!is.null(ev$cost_type)) inner$cost_type <- .v2_array(ev$cost_type)
    if (!is.null(ev$where)) inner$where <- .v2w_predicate(ev$where)
    if (identical(ev$on_missing_table, "unavailable")) inner$on_missing_table <- "unavailable"
    return(list(cost = inner))
  }
  .v2_stop(context, ": unknown event kind '", ev$kind, "'.")
}

.v2_event_base_name <- function(ctx, ev, wire) {
  kind <- ev$kind %||% "table"
  if (identical(kind, "episode_members")) {
    return(paste0(wire$episode_members$episode$event, "_members"))
  }
  if (identical(kind, "cost")) {
    return(paste0(wire$cost$event$event, "_", wire$cost$field))
  }
  abbrev <- unname(.v2_table_abbrev[ev$table]) %||% ev$table
  if (is.na(abbrev)) abbrev <- ev$table
  cs <- wire$concept_set
  if (is.character(cs)) return(paste0(cs, "_", abbrev))
  if (is.list(cs) && length(cs$concepts) > 0L) {
    return(paste0(abbrev, "_", cs$concepts[[1L]]))
  }
  abbrev
}

# Intern an event (object or name); returns its registry name.
.v2c_event <- function(ctx, ev, context) {
  if (.v2_is_param(ev)) ev <- .v2_param_value(ev, context)
  if (is.character(ev)) {
    if (!is.null(ctx$events[[ev]])) return(ev)
    if (!is.null(ctx$intervals[[ev]])) {
      .v2_stop(context, ": '", ev, "' is an interval, not an event.")
    }
    .v2_stop(context, " references event '", ev, "', which the recipe does not declare.")
  }
  if (.v2_is_interval(ev)) .v2_stop(context, ": expected an event, got an era.")
  if (!.v2_is_event(ev)) .v2_stop(context, ": expected an omop_event().")
  wire <- .v2w_event(ctx, ev, context)
  if (!is.null(ev$name)) {
    .v2c_register(ctx, "events", ev$name, wire, "event")
    return(ev$name)
  }
  .v2c_intern_unnamed(ctx, "events", wire, .v2_event_base_name(ctx, ev, wire))
}

.v2c_intern_unnamed <- function(ctx, slot, wire, base) {
  key <- .jcsCanonical(wire)
  reg <- ctx[[slot]]
  for (nm in names(reg)) {
    if (identical(.jcsCanonical(reg[[nm]]), key)) return(nm)
  }
  base <- .v2_truncate_name(base)
  name <- base
  i <- 2L
  taken <- c(names(ctx$events), names(ctx$intervals), ctx$reserved_names)
  while (name %in% taken) {
    name <- .v2_truncate_name(paste0(substr(base, 1L, 60L), "_", i))
    i <- i + 1L
  }
  reg[[name]] <- wire
  ctx[[slot]] <- reg
  name
}

# --- intervals -----------------------------------------------------------------------

.v2w_interval <- function(ctx, iv, context) {
  list(source = list(event = .v2c_event(ctx, iv$event, context)),
       gap = iv$gap, persistence = iv$persistence, min_days = iv$min_days,
       by = iv$by, unmapped = iv$unmapped, end = iv$end, fallback = iv$fallback,
       stockpile = isTRUE(iv$stockpile))
}

.v2c_interval <- function(ctx, iv, context) {
  if (.v2_is_param(iv)) iv <- .v2_param_value(iv, context)
  if (is.character(iv)) {
    if (!is.null(ctx$intervals[[iv]])) return(iv)
    .v2_stop(context, " references interval '", iv, "', which the recipe does not declare.")
  }
  if (!.v2_is_interval(iv)) .v2_stop(context, ": expected an omop_era().")
  wire <- .v2w_interval(ctx, iv, context)
  if (!is.null(iv$name)) {
    .v2c_register(ctx, "intervals", iv$name, wire, "interval")
    return(iv$name)
  }
  .v2c_intern_unnamed(ctx, "intervals", wire, paste0(wire$source$event, "_era"))
}

# A typed source reference {"event": name} or {"interval": name}.
.v2c_source <- function(ctx, x, context, intervals = TRUE) {
  if (.v2_is_param(x)) x <- .v2_param_value(x, context)
  if (is.character(x) && length(x) == 1L) {
    in_ev <- !is.null(ctx$events[[x]])
    in_iv <- !is.null(ctx$intervals[[x]])
    if (in_ev && in_iv) {
      .v2_stop(context, ": '", x, "' names both an event and an interval; pass the object.")
    }
    if (in_iv) {
      if (!intervals) .v2_stop(context, ": '", x, "' is an interval; an event is required.")
      return(list(interval = x))
    }
    if (in_ev) return(list(event = x))
    .v2_stop(context, " references '", x, "', which is neither a declared event nor interval.")
  }
  if (.v2_is_interval(x)) {
    if (!intervals) .v2_stop(context, ": an event is required, not an era.")
    return(list(interval = .v2c_interval(ctx, x, context)))
  }
  list(event = .v2c_event(ctx, x, context))
}

# --- windows and anchors --------------------------------------------------------------

.v2w_anchor <- function(ctx, a, context) {
  if (.v2_is_param(a)) a <- .v2_param_value(a, context)
  if (is.character(a)) {
    if (a %in% .V2_BUILTIN_ANCHORS) return(a)
    return(list(date = a))
  }
  if (inherits(a, "omop_anchor")) {
    out <- list(source = .v2c_source(ctx, a$event, paste0(context, " anchor")),
                which = a$which)
    if (identical(a$which, "nth")) out$n <- a$n
    out$field <- a$field
    out$scope <- a$scope
    if (!is.null(a$search)) out$search <- .v2w_window(ctx, a$search, paste0(context, " anchor search"))
    return(out)
  }
  .v2_stop(context, ": invalid anchor.")
}

.v2w_endpoint <- function(ctx, x, context) {
  if (is.null(x)) return(NULL)
  if (.v2_is_param(x)) x <- .v2_param_value(x, context)
  if (inherits(x, "omop_bound")) {
    return(list(anchor = .v2w_anchor(ctx, x$anchor, context), offset = x$offset))
  }
  as.integer(x)
}

.v2w_window <- function(ctx, w, context) {
  if (is.null(w)) return(NULL)
  if (.v2_is_param(w)) w <- .v2_param_value(w, context)
  if (!inherits(w, "omop_window")) .v2_stop(context, ": expected an omop_window().")
  list(start = .v2w_endpoint(ctx, w$start, context),
       end = .v2w_endpoint(ctx, w$end, context),
       anchor = .v2w_anchor(ctx, w$anchor, context))
}

# --- criteria ----------------------------------------------------------------------------

.v2w_criteria_list <- function(ctx, items, context) {
  parts <- lapply(items, .v2w_criterion, ctx = ctx, context = context)
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0L) return(NULL)
  if (length(parts) == 1L) return(parts[[1L]])
  .v2_pred_flatten("and", parts)
}

.v2w_criterion <- function(ctx, x, context) {
  if (.v2_is_param(x)) x <- .v2_param_value(x, context)
  if (inherits(x, "omop_filter_group")) {
    op <- if (identical(toupper(x$operator), "OR")) "or" else "and"
    parts <- lapply(x$children, .v2w_criterion, ctx = ctx, context = context)
    if (length(parts) == 1L) return(parts[[1L]])
    return(.v2_pred_flatten(op, parts))
  }
  if (inherits(x, "omop_filter")) return(.v2_lower_legacy_filter(ctx, x, context))
  if (!inherits(x, "omop_criterion")) .v2_stop(context, ": invalid criterion.")
  switch(x$kind,
    has = , count_is = {
      inner <- list(source = .v2c_source(ctx, x$event, context), op = x$op,
                    n = if (length(x$n) == 2L) .v2_array(x$n) else x$n)
      if (!is.null(x$window)) inner$window <- .v2w_window(ctx, x$window, context)
      if (!identical(x$distinct, "row")) inner$distinct <- x$distinct
      list(has = inner)
    },
    has_not = {
      inner <- list(source = .v2c_source(ctx, x$event, context), op = "==", n = 0L)
      if (!is.null(x$window)) inner$window <- .v2w_window(ctx, x$window, context)
      if (!is.null(x$nonmissing)) inner$nonmissing <- x$nonmissing
      list(has = inner)
    },
    not = list(not = .v2w_criterion(ctx, x$criterion, context)),
    all = .v2_pred_flatten("and", lapply(x$criteria, .v2w_criterion, ctx = ctx, context = context)),
    any = .v2_pred_flatten("or", lapply(x$criteria, .v2w_criterion, ctx = ctx, context = context)),
    person = list(person = .v2w_predicate(x$where)),
    age_is = {
      inner <- list()
      if (!is.null(x$min)) inner$min <- x$min
      if (!is.null(x$max)) inner$max <- x$max
      if (!is.null(x$year)) inner$reference <- list(year = x$year)
      list(age = if (length(inner)) inner else .v2_empty_object())
    },
    observed = list(observed = .v2_compact(list(before = x$before, after = x$after,
                                                gap = x$gap))),
    in_cohort = list(cohort = list(cohort_definition_id = x$cohort_definition_id)),
    .v2_stop(context, ": unknown criterion kind '", x$kind, "'."))
}

# Legacy omop_filter_*() population filters lowered to native v2 criteria
# (design section 2.3: "Legacy omop_filter_*() constructors remain and lower to
# these"). Filters without a faithful native form are refused, naming them.
.v2_lower_legacy_filter <- function(ctx, f, context) {
  p <- f$params %||% list()
  where <- paste0(context, " (legacy filter '", f$type, "')")
  if (!identical(f$level, "population")) {
    .v2_stop(where, ": row-level filters belong on an event's where = in a v2 recipe.")
  }
  window_of <- function() {
    if (is.null(p$window) && is.null(p$reference_date)) return(NULL)
    w <- p$window %||% list()
    anchor <- if (!is.null(p$reference_date)) p$reference_date else "index"
    .v2w_window(ctx, omop_window(w$start, w$end, anchor = anchor), where)
  }
  legacy_event <- function(table, ids) {
    list(event = .v2c_event(ctx, omop_event(table, as.integer(ids)), where))
  }
  switch(f$type,
    sex = list(person = list(var = "gender_concept_id", op = "==",
                             value = if (identical(p$value, "M")) 8507L else 8532L)),
    age_range = {
      inner <- list(min = as.integer(p$min), max = as.integer(p$max))
      if (!is.null(p$reference_date)) {
        inner$reference <- list(year = as.integer(substr(p$reference_date, 1L, 4L)))
      }
      list(age = inner)
    },
    age_group = {
      groups <- lapply(as.character(unlist(p$groups)), function(g) {
        m <- regmatches(g, regexec("^\\s*([0-9]+)\\s*-\\s*([0-9]+)\\s*$", g))[[1L]]
        plus <- regmatches(g, regexec("^\\s*([0-9]+)\\s*\\+\\s*$", g))[[1L]]
        inner <- if (length(m) == 3L) {
          list(min = as.integer(m[[2L]]), max = as.integer(m[[3L]]))
        } else if (length(plus) == 2L) {
          list(min = as.integer(plus[[2L]]))
        } else {
          .v2_stop(where, ": age group '", g, "' is not of the form 'a-b' or 'a+'.")
        }
        if (!is.null(p$reference_date)) {
          inner$reference <- list(year = as.integer(substr(p$reference_date, 1L, 4L)))
        }
        list(age = inner)
      })
      if (length(groups) == 1L) groups[[1L]] else list(or = groups)
    },
    cohort = list(cohort = list(cohort_definition_id = as.integer(p$cohort_definition_id))),
    has_concept = , concept_count = {
      inner <- list(source = legacy_event(p$table, p$concept_id %||% p$concept_ids),
                    op = ">=", n = as.integer(p$min_count %||% 1L))
      w <- window_of()
      if (!is.null(w)) inner$window <- w
      list(has = inner)
    },
    not_has_concept = {
      inner <- list(source = legacy_event(p$table, p$concept_id %||% p$concept_ids),
                    op = "==", n = 0L)
      w <- window_of()
      if (!is.null(w)) inner$window <- w
      list(has = inner)
    },
    prior_observation = , followup = {
      if (!is.null(p$reference_date)) {
        .v2_stop(where, ": a fixed-reference observation filter has no native v2 ",
                 "form; use omop_observed() relative to an index.")
      }
      key <- if (identical(f$type, "prior_observation")) "before" else "after"
      list(observed = stats::setNames(list(as.integer(p$min_days)), key))
    },
    visit_count = {
      ids <- p$visit_concept_id %||% p$visit_concept_ids
      ev <- if (is.null(ids)) omop_event("visit_occurrence") else
        omop_event("visit_occurrence", as.integer(ids))
      inner <- list(source = list(event = .v2c_event(ctx, ev, where)), op = ">=",
                    n = as.integer(p$min_count %||% 1L))
      w <- window_of()
      if (!is.null(w)) inner$window <- w
      list(has = inner)
    },
    missing_measurement = {
      # v1 meaning (B-7): no measurement row with a non-null value.
      inner <- list(source = legacy_event("measurement", p$concept_id %||% p$concept_ids),
                    op = "==", n = 0L, nonmissing = "value_as_number")
      w <- window_of()
      if (!is.null(w)) inner$window <- w
      list(has = inner)
    },
    has_measurement = {
      if (!is.null(p$min_value) || !is.null(p$max_value)) {
        .v2_stop(where, ": a numeric has_measurement range has no faithful native v2 ",
                 "form here; use omop_has(omop_event(\"measurement\", ..., where = ",
                 "omop_where(value >= a & value <= b))).")
      }
      inner <- list(source = legacy_event("measurement", p$concept_id %||% p$concept_ids),
                    op = ">=", n = 1L)
      w <- window_of()
      if (!is.null(w)) inner$window <- w
      list(has = inner)
    },
    .v2_stop(where, ": no native v2 lowering."))
}

# --- index, coverage, populations ------------------------------------------------------

.v2w_coverage <- function(cov) {
  list(policy = cov$policy, on_ambiguous = cov$on_ambiguous,
       on_missing = cov$on_missing, gap = cov$gap)
}

.v2w_index_source <- function(ctx, src, context) {
  if (.v2_is_param(src)) src <- .v2_param_value(src, context)
  if (inherits(src, "omop_any_of")) {
    arms <- lapply(src$arms, .v2c_source, ctx = ctx, context = context)
    keys <- vapply(arms, .jcsCanonical, "")
    if (anyDuplicated(keys)) .v2_stop(context, ": omop_any_of() arms must be distinct.")
    return(list(any_of = arms))
  }
  if (is.list(src) && identical(names(src), "cohort_definition_id")) {
    id <- src$cohort_definition_id
    if (.v2_is_param(id)) id <- .v2_param_value(id, context)
    return(list(cohort_definition_id = .v2_int(id, paste0(context, ": cohort_definition_id"),
                                               min = 1L)))
  }
  .v2c_source(ctx, src, context)
}

.v2w_index <- function(ctx, idx, context) {
  idx <- .v2_resolve_params(idx, context)
  out <- list(source = .v2w_index_source(ctx, idx$source, context), select = idx$select)
  if (identical(idx$select, "nth")) out$n <- idx$n
  if (!identical(idx$offset, 0L)) out$offset <- idx$offset
  end <- idx$end
  if (!identical(end$strategy, "observation_end") || end$pad != 0L) {
    out$end <- list(strategy = end$strategy)
    if (!is.null(end$days)) out$end$days <- end$days
    if (end$pad != 0L) out$end$pad <- end$pad
  }
  if (isFALSE(idx$restrict_to_observation)) out$restrict_to_observation <- FALSE
  out
}

# Legacy omop_index_event() lowered to a native index (select-before-eligibility
# first/last/all keep their meaning; a DateOffset end becomes fixed/event_end).
.v2_lower_index_event <- function(ctx, ie, context) {
  cs <- if (is.null(ie$concept_id)) NULL else
    omop_concept_set(ie$concept_id, descendants = isTRUE(ie$include_descendants),
                     mapped = isTRUE(ie$include_mapped))
  ev <- omop_event(ie$table, cs)
  end <- omop_end("observation_end")
  if (!is.null(ie$end_strategy)) {
    off <- ie$end_strategy$DateOffset
    if (identical(off$DateField, "StartDate")) {
      end <- omop_end("fixed", days = as.integer(off$Offset))
    } else if (off$Offset >= 0L) {
      end <- omop_end("event_end", pad = as.integer(off$Offset))
    } else {
      .v2_stop(context, ": a negative EndDate offset has no native v2 end strategy.")
    }
  }
  .v2w_index(ctx, omop_index(ev, select = ie$primary_limit, end = end), context)
}

.v2_population_is_implicit_base <- function(pop) {
  identical(pop$id, "base") && is.null(pop$cohort_definition_id) &&
    is.null(pop$episode_policy) && is.null(pop$index_event) && is.null(pop[["index"]]) &&
    is.null(pop$match) && is.null(pop$setop) && is.null(pop$coverage) &&
    is.null(pop$parent_id) && length(pop$filters %||% list()) == 0L
}

.V2_DEFAULT_POPULATION_LABELS <- c("Base Population", "All Persons")

.v2w_population <- function(ctx, pop, extra_filters = list()) {
  context <- paste0("population '", pop$id, "'")
  if (!is.null(pop[["circe_v2"]])) {
    # A Circe import carries its language-2 form (recipe_import_circe()).
    shadow <- pop[["circe_v2"]]
    pop$index <- shadow$index
    pop$filters <- shadow$filters
    pop$coverage <- shadow$coverage
    pop$index_event <- NULL
    pop$circe_v2 <- NULL
  }
  out <- list()
  label <- pop$label
  if (!is.null(label) && !label %in% .V2_DEFAULT_POPULATION_LABELS) out$label <- label
  if (!is.null(pop$match)) {
    if (length(pop$filters %||% list()) || !is.null(pop$parent_id) || !is.null(pop$coverage)) {
      .v2_stop(context, ": a match population takes no filters, parent or coverage.")
    }
    return(c(list(kind = "match"), out, list(match = .v2w_match(ctx, pop$match, context))))
  }
  if (!is.null(pop$setop)) {
    out <- c(list(kind = "setop"), out)
    if (!is.null(pop$parent_id)) out$parent <- pop$parent_id
    out$setop <- list(op = pop$setop$op, members = .v2_array(as.character(pop$setop$members)))
    return(out)
  }
  filters <- c(pop$filters %||% list(), extra_filters)
  where <- .v2w_criteria_list(ctx, filters, context)
  if (!is.null(pop[["index"]]) || !is.null(pop$index_event)) {
    out <- c(list(kind = "index"), out)
    if (!is.null(pop$parent_id)) out$parent <- pop$parent_id
    out$index <- if (!is.null(pop[["index"]])) .v2w_index(ctx, pop[["index"]], context) else
      .v2_lower_index_event(ctx, pop$index_event, context)
    if (!is.null(pop$coverage)) {
      cov <- .v2w_coverage(pop$coverage)
      restrict <- !isFALSE(out$index$restrict_to_observation)
      if (!restrict && identical(cov$on_missing, "drop")) {
        .v2_stop(context, ": restrict_to_observation = FALSE conflicts with ",
                 "coverage on_missing = \"drop\".")
      }
      # An explicit coverage on_missing = "keep" is what restrict_to_observation
      # = FALSE lowers to; keep both fields consistent on the wire.
      if (restrict && identical(cov$on_missing, "keep")) {
        out$index$restrict_to_observation <- FALSE
      }
      out$coverage <- cov
    }
    if (!is.null(where)) out$where <- where
    return(out)
  }
  out <- c(list(kind = "criteria"), out)
  if (!is.null(pop$parent_id)) out$parent <- pop$parent_id
  if (!is.null(where)) out$where <- where
  if (!is.null(pop$cohort_definition_id)) {
    out$cohort_definition_id <- as.integer(pop$cohort_definition_id)
  }
  if (!is.null(pop$episode_policy)) out$episode_policy <- pop$episode_policy
  if (!is.null(pop$coverage)) out$coverage <- .v2w_coverage(pop$coverage)
  out
}

.v2w_match <- function(ctx, m, context) {
  m <- .v2_resolve_params(m, context)
  items <- lapply(m$match, function(it) {
    if (is.character(it)) return(it)
    if (identical(it$kind, "age_band")) return(list(age_band = it$width))
    if (identical(it$kind, "strata")) {
      inner <- list(source = .v2c_source(ctx, it$event, paste0(context, " strata")))
      if (!is.null(it$window)) inner$window <- .v2w_window(ctx, it$window, paste0(context, " strata"))
      return(list(strata = inner))
    }
    .v2_stop(context, ": invalid match item.")
  })
  list(cases = m$cases, controls = m$controls,
       outcome = .v2c_source(ctx, m$outcome, paste0(context, " outcome")),
       at = m$at, match = items, controls_per_case = m$controls_per_case,
       prior_outcome = m$prior_outcome, future_cases = m$future_cases,
       replacement = m$replacement, seed = m$seed, insufficient = m$insufficient,
       missing_strata = m$missing_strata, max_candidates = m$max_candidates)
}

# --- variables and reducers -------------------------------------------------------------

.v2w_reducer <- function(ctx, r, context) {
  r <- .v2_resolve_params(r, context)
  out <- list(fn = r$fn)
  if (!is.null(r$value)) out$value <- r$value
  if (!is.null(r$window)) out$window <- .v2w_window(ctx, r$window, context)
  if (!is.null(r$which)) out$which <- r$which
  if (length(r$n) > 1L) out$n <- .v2_array(r$n) else if (!identical(r$n, 1L)) out$n <- r$n
  if (!identical(r$side, "either")) out$side <- r$side
  if (!identical(r$missing, "skip")) out$missing <- r$missing
  if (!is.null(r$distinct)) out$distinct <- r$distinct
  if (!is.null(r$p)) out$p <- r$p
  if (!is.null(r$agg)) out$agg <- r$agg
  if (!is.null(r$reference)) out$reference <- .v2w_anchor(ctx, r$reference, context)
  if (!is.null(r$predicate)) out$predicate <- .v2w_predicate(r$predicate)
  if (!is.null(r$reference_date)) out$reference_date <- r$reference_date
  if (!is.null(r$unit)) out$unit <- r$unit
  if (!identical(r$coverage, "recorded")) out$coverage <- r$coverage
  if (!identical(r$fills, "start_in_window")) out$fills <- r$fills
  if (isTRUE(r$observed_only)) out$observed_only <- TRUE
  out
}

# The event a reducer variable reads, narrowed by the variable's own
# where/visit (a derived event node named <event>_<variable>).
.v2_variable_source <- function(ctx, v, name, context) {
  ev <- v$event
  if (.v2_is_param(ev)) ev <- .v2_param_value(ev, context)
  if (is.null(v$where) && is.null(v$visit)) return(.v2c_source(ctx, ev, context))
  if (is.character(ev)) {
    .v2_stop(context, ": where/visit on a variable need the event object, not a name.")
  }
  if (.v2_is_interval(ev)) {
    .v2_stop(context, ": where/visit narrow an event; narrow the era's event instead.")
  }
  narrowed <- ev
  base_name <- ev$name %||% .v2c_event(ctx, ev, context)
  narrowed$where <- .v2_predicate(.v2_and_predicates(.v2w_predicate(ev$where),
                                                      .v2w_predicate(v$where)))
  if (!is.null(v$visit)) {
    if (!is.null(ev$visit) && !identical(ev$visit, v$visit)) {
      .v2_stop(context, ": the variable's visit conflicts with its event's visit.")
    }
    narrowed$visit <- v$visit
  }
  narrowed$name <- .v2_truncate_name(paste0(base_name, "_", name))
  .v2c_source(ctx, narrowed, context)
}

.v2w_derived <- function(d) {
  out <- list(kind = d$kind)
  switch(d$kind,
    age = {
      out$reference <- d$reference %||% "index"
      if (!is.null(d$reference_date)) out$reference_date <- d$reference_date
      out$bands <- isTRUE(d$bands)
      if (!is.null(d$age_breaks)) out$age_breaks <- .v2_array(d$age_breaks)
    },
    birth_year_band = out$width <- d$width,
    obs_duration = out$period_policy <- d$period_policy %||% "total",
    prior_obs = , followup = {
      out$reference_date <- d$reference_date
      out$period_policy <- "containing"
    },
    died = if (!is.null(d$before)) out$before <- d$before,
    observed_days = NULL,
    charlson = , chads2 = , chadsvasc = , dcsi = , hfrs = {
      if (!is.null(d$reference_date)) out$reference_date <- d$reference_date
    },
    NULL)
  out
}

.v2w_variable <- function(ctx, v, name) {
  context <- paste0("variable '", name, "'")
  if (!.v2_is_v2_variable(v)) return(.v2_lower_legacy_variable(ctx, v, name, context))
  switch(v$kind,
    reducer = {
      if (is.null(v$reducer)) .v2_stop(context, ": a reducer is required.")
      out <- list(source = .v2_variable_source(ctx, v, name, context),
                  reducer = .v2w_reducer(ctx, v$reducer, context))
      if (isTRUE(v$broadcast)) out$broadcast <- TRUE
      out
    },
    derived = {
      d <- .v2_resolve_params(v$derived, context)
      wire <- .v2w_derived(d)
      if (identical(d$kind, "observed_days")) {
        wire$window <- .v2w_window(ctx, d$window, context)
      }
      list(derived = wire)
    },
    population_column = list(population_column = list(name = v$population_column)),
    .v2_stop(context, ": unknown variable kind '", v$kind, "'."))
}

.V2_LEGACY_DERIVED <- c("age", "sex_mf", "obs_duration", "prior_obs", "followup",
                        "demo_missingness")
.V2_LEGACY_FEATURES <- c("binary", "count", "first_value", "last_value", "mean", "min",
                         "max", "sum", "sd", "cv", "slope", "n_distinct", "abnormal_high",
                         "abnormal_low", "gap_max", "gap_mean", "duration_sum",
                         "drug_duration", "time_since")

# Legacy table/format spelling lowered to the native event/reducer, derived or
# raw forms (design section 2.7: "table/format and event/reducer are two
# spellings").
.v2_lower_legacy_variable <- function(ctx, v, name, context) {
  fmt <- v$format %||% "raw"
  if (fmt %in% c("charlson", "chads2", "chadsvasc", "dcsi", "hfrs")) {
    .v2_stop(context, ": the legacy score constructor counts all-time conditions; ",
             "use omop_score(\"", fmt, "\") (conditions up to the reference date) in a ",
             "v2 recipe, or recipe_to_plan(version = 1).")
  }
  if (fmt %in% .V2_LEGACY_DERIVED) {
    d <- v$derived %||% list(kind = fmt)
    if (identical(fmt, "age")) {
      d <- list(kind = "age", reference = d$reference %||% "today",
                reference_date = d$reference_date, bands = FALSE)
    }
    return(list(derived = .v2w_derived(d)))
  }
  if (identical(fmt, "raw")) {
    if (!v$table %in% c("person", "death", "observation_period") || is.null(v$column)) {
      .v2_stop(context, ": a raw column of '", v$table, "' is an event row, not one ",
               "value per person; use a long output (format = \"long\", sources =) ",
               "or a reducer.")
    }
    return(list(raw = list(table = v$table, column = tolower(v$column))))
  }
  if (!fmt %in% .V2_LEGACY_FEATURES) {
    .v2_stop(context, ": legacy format '", fmt, "' has no v2 lowering.")
  }
  where <- .v2_lower_row_filters(v$filters, v$table, context)
  if (!is.null(v[["where"]])) where <- .v2_and_predicates(where, .v2w_predicate(v[["where"]]))
  visit <- if (!is.null(v$visit_filter)) {
    list(type = as.integer(unlist(v$visit_filter$concept_ids %||% v$visit_filter)))
  } else if (identical(v[["visit"]], "index")) "index" else NULL
  concepts <- if (length(v$concept_id %||% integer(0))) {
    omop_concept_set(v$concept_id, descendants = isTRUE(v$expand))
  } else NULL
  ev <- omop_event(v$table, concepts, where = if (!is.null(where)) .v2_predicate(where),
                   visit = visit, value = v$value_source, concept_col = v$concept_col)
  window <- if (!is.null(v$time_window)) {
    tw <- .normalize_recipe_time_window(v$time_window, name)
    omop_window(tw$start, tw$end)
  } else NULL
  red <- switch(fmt,
    drug_duration = omop_reduce("drug_duration", window = window,
                                agg = v$derived$agg %||% "mean"),
    time_since = omop_reduce("time_since", window = window,
                             reference_date = v$derived$reference_date,
                             unit = v$derived$unit %||% "day"),
    omop_reduce(fmt, window = window))
  out <- list(source = .v2c_source(ctx, ev, context), reducer = .v2w_reducer(ctx, red, context))
  if (isTRUE(v[["broadcast"]])) out$broadcast <- TRUE
  out
}

# Legacy row filters (omop_filter level "row") as one v2 predicate tree.
.v2_lower_row_filters <- function(filters, table, context) {
  if (length(filters %||% list()) == 0L) return(NULL)
  node <- function(f) {
    if (inherits(f, "omop_filter_group")) {
      op <- if (identical(toupper(f$operator), "OR")) "or" else "and"
      parts <- lapply(f$children, node)
      return(if (length(parts) == 1L) parts[[1L]] else .v2_pred_flatten(op, parts))
    }
    p <- f$params %||% list()
    switch(f$type,
      date_range = {
        col <- p$date_column %||% .default_omop_date_column(table)
        if (is.null(col)) .v2_stop(context, ": date_range needs date_column on '", table, "'.")
        parts <- Filter(Negate(is.null), list(
          if (!is.null(p$start)) list(var = col, op = ">=", value = p$start),
          if (!is.null(p$end)) list(var = col, op = "<=", value = p$end)))
        if (length(parts) == 1L) parts[[1L]] else list(and = parts)
      },
      concept_set = list(var = p$concept_col %||% .plan_domain_concept_column(table),
                         op = "in", value = .v2_array(as.integer(unlist(p$concept_ids %||% p$concept_id)))),
      value_concept = list(var = p$var %||% "value_as_concept_id", op = "in",
                           value = .v2_array(as.integer(unlist(p$value)))),
      value_bin = list(and = list(list(var = p$var, op = ">=", value = p$value$lower),
                                  list(var = p$var, op = "<", value = p$value$upper))),
      custom = {
        op <- tolower(p$op)
        if (op %in% c("is_null", "not_null")) list(var = p$var, op = op)
        else if (op %in% c("in", "not_in")) list(var = p$var, op = op,
                                                 value = .v2_array(unlist(p$value)))
        else if (identical(op, "between")) {
          b <- unlist(p$value)
          list(var = p$var, op = "between", value = list(lower = b[[1L]], upper = b[[2L]]))
        } else list(var = p$var, op = op, value = p$value)
      },
      .v2_stop(context, ": row filter '", f$type, "' has no v2 lowering."))
  }
  parts <- lapply(filters, node)
  tree <- if (length(parts) == 1L) parts[[1L]] else .v2_pred_flatten("and", parts)
  omop_where_tree(tree)$tree
}

# --- outputs ---------------------------------------------------------------------------------

.V2_FORMAT_OPTIONS <- list(
  wide = c("date_handling", "time_window", "temporal"),
  long = c("date_handling", "temporal", "unit", "columns_derived", "anchors", "relations",
           "event_log"),
  survival = c("outcomes", "tar", "tars", "entry", "censor", "prior_outcome", "format",
               "outcome_mode", "event_order", "washout_days", "tie_policy",
               "same_day_priority", "gap_policy", "transitions", "initial_state",
               "state_hierarchy", "state_step", "complete"),
  intervals = c("sources", "window", "interval_match", "event_select", "select_n",
                "select_by", "anchor"),
  temporal_covariates = c("sources", "bin_width", "window", "analyses"),
  person_period = c("representation", "sources", "bins", "window", "partition", "outcomes",
                    "exposures", "carry_forward", "analyses"),
  sparse = c("sources", "analyses"),
  membership = c("date_handling"),
  dictionary = c("source_outputs"),
  joined = c("from", "how", "cardinality"))

.v2w_date_handling <- function(dh, context) {
  if (is.null(dh)) return(NULL)
  if (is.character(dh) && length(dh) == 1L) {
    mode <- tolower(dh)
    if (identical(mode, "relative_to_index")) mode <- "relative"
    return(list(mode = .v2_choice(mode, c("remove", "relative", "binned", "absolute"),
                                  paste0(context, ": date_handling"))))
  }
  if (is.list(dh)) {
    out <- list(mode = .v2_choice(tolower(dh$mode %||% "relative"),
                                  c("remove", "relative", "binned", "absolute"),
                                  paste0(context, ": date_handling mode")))
    if (!is.null(dh$reference)) out$reference <- dh$reference
    if (!is.null(dh$bin_width)) out$bin_width <- dh$bin_width
    if (!is.null(dh$date_columns)) out$date_columns <- .v2_array(as.character(unlist(dh$date_columns)))
    return(out)
  }
  .v2_stop(context, ": date_handling must be a mode string or omop.date_handling().")
}

.v2w_sources <- function(ctx, sources, context) {
  if (is.null(sources)) return(NULL)
  if (.v2_is_event(sources) || .v2_is_interval(sources) || is.character(sources)) {
    sources <- if (is.character(sources)) as.list(sources) else list(sources)
  }
  lapply(unname(sources), .v2c_source, ctx = ctx, context = context)
}

.v2w_named_sources <- function(ctx, x, context, intervals_only = FALSE) {
  if (is.null(x)) return(NULL)
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    .v2_stop(context, " must be a named list.")
  }
  out <- lapply(names(x), function(nm) {
    ref <- .v2c_source(ctx, x[[nm]], paste0(context, " '", nm, "'"))
    if (intervals_only && is.null(ref$interval)) {
      .v2_stop(context, " '", nm, "' must be an era (interval).")
    }
    ref
  })
  stats::setNames(out, names(x))
}

.v2w_censor_item <- function(ctx, x, context) {
  if (.v2_is_param(x)) x <- .v2_param_value(x, context)
  if (is.character(x) && length(x) == 1L && x %in% c("observation_end", "death", "index_end")) {
    return(x)
  }
  if (is.list(x) && identical(names(x), "admin_date")) {
    return(list(admin_date = .plan_iso_date(x$admin_date, paste0(context, " admin_date"))))
  }
  if (inherits(x, "omop_censor")) {
    inner <- list(source = .v2c_source(ctx, x$source, context), which = x$which)
    if (!identical(x$offset, 0L)) inner$offset <- x$offset
    if (!is.null(x$search)) inner$search <- .v2w_window(ctx, x$search, context)
    if (!identical(x$missing, "ignore")) inner$missing <- x$missing
    if (!is.null(x$when_index_source)) inner$when_index_source <- .v2_array(x$when_index_source)
    return(list(censor = inner))
  }
  .v2c_source(ctx, x, context)
}

.v2_default_event_order <- function(format) {
  if (format %in% c("recurrent_events", "counting_process", "multi_state")) "all" else "first"
}

.v2w_options_survival <- function(ctx, o, context) {
  out <- list()
  if (is.null(o$outcomes) || length(o$outcomes) == 0L) {
    .v2_stop(context, ": survival needs outcomes = list(<name> = <event>).")
  }
  out$outcomes <- .v2w_named_sources(ctx, o$outcomes, paste0(context, " outcome"))
  if (!is.null(o[["tar"]]) && !is.null(o[["tars"]])) .v2_stop(context, ": give tar or tars, not both.")
  if (!is.null(o[["tar"]])) out$tar <- .v2w_window(ctx, o[["tar"]], paste0(context, " tar"))
  if (!is.null(o$tars)) {
    if (!is.list(o$tars) || is.null(names(o$tars)) || inherits(o$tars, "omop_window")) {
      .v2_stop(context, ": tars must be a named list of omop_window().")
    }
    out$tars <- lapply(o$tars, .v2w_window, ctx = ctx, context = paste0(context, " tar"))
  }
  if (is.null(o[["tar"]]) && is.null(o[["tars"]])) {
    # F-4: the legacy 730-day TAR stays the default; lint/explain report it.
    out$tar <- list(start = 0L, end = 730L, anchor = "index")
  }
  if (!is.null(o$entry)) {
    if (!inherits(o$entry, "omop_bound")) .v2_stop(context, ": entry must be an omop_bound().")
    out$entry <- .v2w_endpoint(ctx, o$entry, paste0(context, " entry"))
  }
  if (is.null(o$censor) || length(o$censor) == 0L) {
    .v2_stop(context, ": v2 survival needs an explicit censor list (e.g. ",
             "censor = list(\"observation_end\", \"death\")); only listed sources cap ",
             "follow-up.")
  }
  censor <- if (is.character(o$censor)) as.list(o$censor) else
    if (inherits(o$censor, c("omop_censor", "omop_event", "omop_interval"))) list(o$censor) else o$censor
  out$censor <- lapply(unname(censor), .v2w_censor_item, ctx = ctx,
                       context = paste0(context, " censor"))
  if (!is.null(o$prior_outcome)) {
    po <- o$prior_outcome
    out$prior_outcome <- list(lookback = if (is.null(po$lookback)) NULL else
      .v2_int(po$lookback, paste0(context, " prior_outcome lookback")))
    if (!is.null(po$action)) out$prior_outcome$action <- po$action
  }
  format <- tolower(o$format %||% "survival")
  out$format <- format
  out$event_order <- o$event_order %||% .v2_default_event_order(format)
  out$outcome_mode <- o$outcome_mode %||% (if (format == "survival") "composite" else "named")
  for (nm in c("washout_days", "tie_policy", "same_day_priority", "gap_policy",
               "initial_state", "state_step", "complete")) {
    if (!is.null(o[[nm]])) out[[nm]] <- o[[nm]]
  }
  if (!is.null(o$transitions)) {
    out$transitions <- lapply(unname(o$transitions), function(t) {
      list(from = as.character(t$from), to = as.character(t$to))
    })
  }
  if (!is.null(o$state_hierarchy)) out$state_hierarchy <- .v2_array(as.character(o$state_hierarchy))
  .v2_check_survival_matrix(out, context)
  out
}

.v2_check_survival_matrix <- function(o, context) {
  f <- o$format; ord <- o$event_order
  bad <- function(msg) .v2_stop(context, ": ", msg)
  if (f == "survival" && ord == "all") bad("survival requires event_order first or last.")
  if (f == "competing_risk" && ord != "first") bad("competing_risk requires event_order = \"first\".")
  if (f %in% c("recurrent_events", "counting_process") && ord == "last") {
    bad("recurrent/counting formats require event_order first or all.")
  }
  if (f == "multi_state" && ord != "all") bad("multi_state requires event_order = \"all\".")
  if (f == "multi_state" && !identical(o$outcome_mode, "named")) {
    bad("multi_state requires outcome_mode = \"named\".")
  }
  tie <- o$tie_policy %||% "priority"
  if (tie == "error") bad("tie_policy = \"error\" is unavailable (a data-dependent failure is a disclosure oracle).")
  if (tie == "all" && f != "recurrent_events") bad("tie_policy = \"all\" is supported only for recurrent_events.")
  if (tie == "sequential" && f != "multi_state") bad("tie_policy = \"sequential\" is supported only for multi_state.")
  if (!is.null(o$transitions) && f != "multi_state") bad("transitions require format = \"multi_state\".")
  invisible(TRUE)
}

.v2w_options <- function(ctx, format, o, context, sources_arg = NULL) {
  o <- o %||% list()
  if (length(o) && (is.null(names(o)) || any(!nzchar(names(o))))) {
    .v2_stop(context, ": options must be a named list.")
  }
  unknown <- setdiff(names(o), .V2_FORMAT_OPTIONS[[format]])
  if (length(unknown)) {
    .v2_stop(context, " (format \"", format, "\") has unsupported option(s): ",
             paste(unknown, collapse = ", "), ". Options are a closed set.")
  }
  if (identical(format, "survival")) return(.v2w_options_survival(ctx, o, context))
  out <- list()
  if (!is.null(o$date_handling)) out$date_handling <- .v2w_date_handling(o$date_handling, context)
  if (!is.null(o$time_window)) out$time_window <- .v2w_window(ctx, o$time_window, context)
  if (!is.null(o$temporal)) {
    out$temporal <- .v2_compact(list(event_select = o$temporal$event_select,
                                     min_gap = o$temporal$min_gap))
  }
  if (!is.null(o$sources)) out$sources <- .v2w_sources(ctx, o$sources, paste0(context, " source"))
  if (!is.null(o$window)) out$window <- .v2w_window(ctx, o$window, paste0(context, " window"))
  for (nm in c("unit", "event_log", "interval_match", "event_select", "select_n",
               "select_by", "bin_width", "representation", "how", "cardinality")) {
    if (!is.null(o[[nm]])) out[[nm]] <- o[[nm]]
  }
  if (!is.null(o[["anchor"]])) out$anchor <- .v2w_anchor(ctx, o[["anchor"]], context)
  if (!is.null(o$columns_derived)) out$columns_derived <- .v2_array(as.character(o$columns_derived))
  if (!is.null(o[["anchors"]])) {
    out$anchors <- lapply(o[["anchors"]], .v2w_anchor, ctx = ctx, context = paste0(context, " anchor"))
  }
  if (!is.null(o$relations)) {
    out$relations <- lapply(names(o$relations), function(nm) {
      r <- o$relations[[nm]]
      rel <- list(source = .v2c_source(ctx, r$event %||% r$source, paste0(context, " relation")),
                  within = .v2_array(as.integer(r$within)))
      if (!is.null(r$distinct_by)) rel$distinct_by <- r$distinct_by
      if (!is.null(r$exclude_self)) rel$exclude_self <- r$exclude_self
      rel
    })
    names(out$relations) <- names(o$relations)
  }
  if (!is.null(o$analyses)) out$analyses <- .v2_array(as.character(o$analyses))
  if (!is.null(o$bins)) {
    out$bins <- if (!is.null(o$bins$calendar)) list(calendar = o$bins$calendar) else
      list(width = .v2_int(o$bins$width, paste0(context, " bins width"), min = 1L))
  }
  if (!is.null(o$partition)) {
    pt <- o$partition
    part <- list()
    if (!is.null(pt$boundaries)) part$boundaries <- .v2w_sources(ctx, pt$boundaries, paste0(context, " partition"))
    if (!is.null(pt$age_bands)) part$age_bands <- .v2_int(pt$age_bands, paste0(context, " age_bands"), min = 1L)
    if (!is.null(pt$calendar)) part$calendar <- pt$calendar
    out$partition <- if (length(part)) part else .v2_empty_object()
  }
  if (!is.null(o$outcomes)) out$outcomes <- .v2w_named_sources(ctx, o$outcomes, paste0(context, " outcome"))
  if (!is.null(o$exposures)) {
    out$exposures <- .v2w_named_sources(ctx, o$exposures, paste0(context, " exposure"),
                                        intervals_only = TRUE)
  }
  if (!is.null(o$carry_forward)) {
    out$carry_forward <- lapply(o$carry_forward, function(cf) {
      list(lookback = .v2_int(cf$lookback, paste0(context, " carry_forward lookback"), min = 0L),
           expire = if (is.null(cf$expire)) NULL else
             .v2_int(cf$expire, paste0(context, " carry_forward expire")))
    })
  }
  if (!is.null(o$source_outputs)) out$source_outputs <- .v2_array(as.character(o$source_outputs))
  if (!is.null(o$from)) {
    out$from <- lapply(unname(o$from), function(f) {
      if (is.character(f)) f <- list(output = f)
      .v2_compact(list(output = f$output, component = f$component))
    })
  }
  out
}

# Legacy output types mapped onto v2 formats.
.v2_output_format <- function(out) {
  if (!is.null(out$format)) return(out$format)
  type <- out$type %||% "wide"
  switch(type,
    wide = , features = , baseline = "wide",
    .v2_stop("output '", out$name, "': legacy type '", type, "' has no v2 lowering in ",
             "recipe_to_plan(version = 2) (its v1 component shapes and censoring ",
             "defaults are kept only by the v1 adapter); use omop_output(format = \"",
             type, "\", ...) with v2 sources, or recipe_to_plan(version = 1)."))
}

# Populations that yield episodes (one row per index episode) by default.
.v2_population_is_indexed <- function(pops, id, seen = character(0)) {
  p <- pops[[id]]
  if (is.null(p) || id %in% seen) return(FALSE)
  if (identical(p$kind, "index") || identical(p$kind, "match")) return(TRUE)
  seen <- c(seen, id)
  if (identical(p$kind, "setop")) {
    members <- unlist(p$setop$members)
    return(any(vapply(members, function(m) .v2_population_is_indexed(pops, m, seen), logical(1))))
  }
  if (!is.null(p$parent)) return(.v2_population_is_indexed(pops, p$parent, seen))
  FALSE
}

# Populations whose rows have an index date (episodes): indexed or matched
# populations, criteria populations seeded from a results cohort, and their
# descendants.
.v2_population_has_index <- function(pops, id, seen = character(0)) {
  p <- pops[[id]]
  if (is.null(p) || id %in% seen) return(FALSE)
  if (p$kind %in% c("index", "match") || !is.null(p$cohort_definition_id)) return(TRUE)
  seen <- c(seen, id)
  if (identical(p$kind, "setop")) {
    return(all(vapply(unlist(p$setop$members), function(m)
      .v2_population_has_index(pops, m, seen), logical(1))))
  }
  if (!is.null(p$parent)) return(.v2_population_has_index(pops, p$parent, seen))
  FALSE
}

.v2_register_variable <- function(ctx, name, wire) {
  .v2c_register(ctx, "variables", name, wire, "variable")
}

.v2_legacy_output_options <- function(out, context) {
  o <- out$options %||% list()
  allowed <- c("temporal", "time_window", "date_handling", "grain")
  unknown <- setdiff(names(o), allowed)
  if (length(unknown)) {
    .v2_stop(context, ": legacy option(s) ", paste(unknown, collapse = ", "),
             " have no v2 lowering.")
  }
  if (!is.null(o$time_window)) {
    .v2_stop(context, ": a calendar time_window has no v2 lowering; put the date range ",
             "on the events' where =.")
  }
  if (!is.null(o$temporal$index_window) || !is.null(o$temporal$calendar)) {
    .v2_stop(context, ": temporal index/calendar windows belong on the variables' ",
             "reducer windows in a v2 recipe.")
  }
  res <- list()
  if (!is.null(o$date_handling)) res$date_handling <- .v2w_date_handling(o$date_handling, context)
  if (!is.null(o$temporal)) {
    res$temporal <- .v2_compact(list(event_select = o$temporal$event_select,
                                     min_gap = o$temporal$min_gap))
  }
  res
}

.v2w_output <- function(ctx, out) {
  name <- out$name
  context <- paste0("output '", name, "'")
  if (!grepl(.V2_NAME_PATTERN, name)) .v2_stop(context, ": invalid output name.")
  legacy <- is.null(out$format)
  format <- .v2_output_format(out)
  rows <- out$rows %||% out$population_id %||% "base"
  ctx$referenced_pops <- union(ctx$referenced_pops, rows)
  wire <- list(rows = rows, format = format)
  grain <- out$grain
  if (legacy) {
    grain <- grain %||% out$options$grain
    if (identical(out$type, "baseline")) grain <- grain %||% "episode"
  }
  wire$grain <- grain %||% "<default>"
  wire$result_symbol <- out$result_symbol %||% paste0("D_", name)

  columns <- out$columns %||% out$variables
  recipe_vars <- ctx$recipe$variables %||% list()
  if (format %in% c("wide", "long", "sparse")) {
    take <- if (is.null(columns)) {
      if (identical(format, "wide")) names(recipe_vars) else character(0)
    } else columns
    missing <- setdiff(take, names(recipe_vars))
    if (length(missing)) {
      .v2_stop(context, " references unknown variable(s): ", paste(missing, collapse = ", "), ".")
    }
    for (vn in take) {
      v <- recipe_vars[[vn]]
      if (.v2_is_v2_variable(v)) next
      if (identical(out$type, "baseline") && identical(v$format, "age")) {
        d <- v$derived %||% list()
        wire_v <- list(derived = .v2w_derived(list(kind = "age", reference = "index",
                                                   reference_date = d$reference_date,
                                                   bands = TRUE)))
        .v2_register_variable(ctx, vn, wire_v)
      } else {
        .v2_register_variable(ctx, vn, .v2_lower_legacy_variable(ctx, v, vn,
                                                                  paste0("variable '", vn, "'")))
      }
    }
  } else if (!is.null(columns)) {
    .v2_stop(context, ": format \"", format, "\" takes no columns/variables.")
  }
  if (!is.null(columns)) wire$columns <- .v2_array(as.character(columns))
  if (!is.null(out$collapse)) wire$collapse <- out$collapse
  if (!is.null(out$sources)) {
    wire$sources <- .v2w_sources(ctx, out$sources, paste0(context, " source"))
  }
  opts <- if (legacy) .v2_legacy_output_options(out, context) else
    .v2w_options(ctx, format, out$options, context)
  if (length(opts)) wire$options <- opts
  wire
}

#' Compile a recipe to a v2 wire plan
#' @keywords internal
.recipe_to_plan_v2 <- function(recipe) {
  # Unbound parameter placeholders: bound to their default or refused.
  recipe <- .v2_resolve_params(recipe)
  ctx <- .v2_new_context(recipe)
  # Names given explicitly anywhere are reserved before inline objects are
  # interned, so a generated name never collides with a user's name.
  .v2_walk(recipe, function(node) {
    if ((.v2_is_event(node) || .v2_is_interval(node)) && is.character(node$name)) {
      ctx$reserved_names <- union(ctx$reserved_names, node$name)
    }
  })
  # 1. Declared registries, in declaration order.
  for (i in seq_along(recipe$concept_sets %||% list())) {
    cs <- recipe$concept_sets[[i]]
    nm <- names(recipe$concept_sets)[[i]]
    if (!inherits(cs, "omop_concept_set")) .v2_stop("concept_sets: expected omop_concept_set() objects.")
    if (is.null(cs$name)) cs$name <- nm
    .v2c_concept_set(ctx, cs, paste0("concept set '", cs$name, "'"))
  }
  for (ev in recipe$events %||% list()) {
    ctx$declared_events <- c(ctx$declared_events, .v2c_event(ctx, ev, "recipe events"))
  }
  for (iv in recipe$intervals %||% list()) {
    ctx$declared_intervals <- c(ctx$declared_intervals, .v2c_interval(ctx, iv, "recipe intervals"))
  }
  # 2. v2 variables (every one is emitted; legacy ones when an output uses them).
  for (vn in names(recipe$variables %||% list())) {
    v <- recipe$variables[[vn]]
    if (.v2_is_v2_variable(v)) .v2_register_variable(ctx, vn, .v2w_variable(ctx, v, vn))
  }
  # 3. Populations; recipe-level population filters constrain the base.
  pops <- recipe$populations %||% list()
  recipe_filters <- .extract_filters_by_level(recipe$filters %||% list(), "population")
  recipe_criteria <- Filter(function(f) inherits(f, "omop_criterion"), recipe$filters %||% list())
  if (length(.extract_filters_by_level(recipe$filters %||% list(), "row"))) {
    .v2_stop("Recipe-level row filters have no v2 lowering: put the predicate on ",
             "each event's where = (or use recipe_to_plan(version = 1)).")
  }
  base_extra <- c(recipe_filters, recipe_criteria)
  for (id in names(pops)) {
    pop <- pops[[id]]
    extra <- if (identical(id, "base")) base_extra else list()
    if (identical(id, "base") && .v2_population_is_implicit_base(pop) && length(extra) == 0L) {
      ctx$implicit_base <- TRUE
      ctx$populations[[id]] <- list(kind = "criteria")
      next
    }
    ctx$populations[[id]] <- .v2w_population(ctx, pop, extra)
  }
  # 4. Outputs.
  outs <- recipe$outputs %||% list()
  if (length(outs) == 0L) {
    if (length(recipe$variables %||% list()) == 0L) .v2_stop("Recipe has no outputs.")
    outs <- list(output = omop_output(name = "output"))
  }
  for (nm in names(outs)) ctx$outputs[[nm]] <- .v2w_output(ctx, outs[[nm]])
  # 5. Population closure: drop the implicit base when nothing references it.
  wire_pops <- ctx$populations
  needed <- unique(c(ctx$referenced_pops,
                     unlist(lapply(wire_pops, function(p) c(p$parent, unlist(p$setop$members),
                                                             p$match$cases, p$match$controls)))))
  if (isTRUE(ctx$implicit_base) && !"base" %in% needed) wire_pops$base <- NULL
  undeclared <- setdiff(needed, names(wire_pops))
  if (length(undeclared)) {
    .v2_stop("Recipe references undeclared population(s): ", paste(undeclared, collapse = ", "),
             "; add them via omop_recipe(populations = ...).")
  }
  # 6. Grain defaults (episode for an indexed population, person otherwise).
  for (nm in names(ctx$outputs)) {
    o <- ctx$outputs[[nm]]
    if (identical(o$grain, "<default>")) {
      o$grain <- if (.v2_population_is_indexed(wire_pops, o$rows)) "episode" else "person"
      ctx$outputs[[nm]] <- o
    }
  }
  plan <- list(version = 2L,
               meta = list(language_version = .V2_LANGUAGE_VERSION,
                           semantic_version = .V2_SEMANTIC_VERSION,
                           generated_by = "dsOMOPClient",
                           client_version = as.character(utils::packageVersion("dsOMOPClient"))))
  opts <- recipe$options %||% list()
  plan_opts <- list()
  for (nm in c("translate_concepts", "block_sensitive", "factor_concepts")) {
    if (!is.null(opts[[nm]]) && !isTRUE(opts[[nm]])) plan_opts[[nm]] <- FALSE
  }
  if (!is.null(recipe$coverage)) plan_opts$coverage <- .v2w_coverage(recipe$coverage)
  if (length(plan_opts)) plan$options <- plan_opts
  if (length(ctx$concept_sets)) plan$concept_sets <- ctx$concept_sets
  if (length(ctx$events)) plan$events <- ctx$events
  if (length(ctx$intervals)) plan$intervals <- ctx$intervals
  plan$populations <- wire_pops
  if (length(ctx$variables)) plan$variables <- ctx$variables
  plan$outputs <- ctx$outputs
  if (!is.null(recipe$publish)) plan$publish <- .v2_array(as.character(recipe$publish))
  if (!is.null(recipe$scope)) {
    sc <- recipe$scope
    plan$scope <- .v2_compact(list(cohort = sc$cohort,
                                   tables = if (length(sc$tables)) .v2_array(as.character(sc$tables)),
                                   combine = if (!identical(sc$combine %||% "union", "union")) sc$combine))
  }
  .v2_validate_plan_semantics(plan)
  .v2_assert_no_class1(plan)
  .v2_assert_schema(plan)
  structure(plan, class = c("omop_plan_v2", "list"))
}

# Class-1 columns (section 7.6) are never projected, used as value columns or
# referenced by predicates (no free-text predicates, decision 5).
.v2_assert_no_class1 <- function(plan) {
  bad <- character(0)
  check <- function(col, where) {
    if (is.character(col) && length(col) == 1L && .is_blocked_column(col)) {
      bad <<- c(bad, paste0(col, " (", where, ")"))
    }
  }
  pred_vars <- function(node, where) {
    if (!is.list(node)) return(invisible())
    if (!is.null(node$var)) check(node$var, where)
    for (k in intersect(names(node), c("and", "or"))) for (ch in node[[k]]) pred_vars(ch, where)
    if (!is.null(node$not)) pred_vars(node$not, where)
  }
  for (nm in names(plan$events)) {
    ev <- plan$events[[nm]]
    check(ev$value$column, paste0("event '", nm, "' value"))
    check(ev$concept_col, paste0("event '", nm, "' concept_col"))
    pred_vars(ev$where %||% ev$episode_members$where %||% ev$cost$where,
              paste0("event '", nm, "' where"))
  }
  for (nm in names(plan$variables)) {
    v <- plan$variables[[nm]]
    check(v$raw$column, paste0("variable '", nm, "'"))
    check(v$reducer$value, paste0("variable '", nm, "' reducer value"))
    pred_vars(v$reducer$predicate, paste0("variable '", nm, "' predicate"))
  }
  for (id in names(plan$populations)) {
    .v2_walk(plan$populations[[id]]$where, function(node) {
      if (is.list(node) && !is.null(node$person)) pred_vars(node$person, paste0("population '", id, "'"))
    })
  }
  if (length(bad)) {
    stop("Disclosive: the plan requests blocked (class-1) column(s) that the server can ",
         "never release or filter on: ", paste(unique(bad), collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate an emitted v2 plan against the vendored wire schema
#' @keywords internal
.v2_assert_schema <- function(plan) {
  res <- .jsonSchemaValidate(.v2_plan_schema(), .v2_plain(plan))
  if (!res$valid) {
    stop("The compiled plan does not validate against plan-v2.json: ",
         paste(utils::head(res$errors, 6L), collapse = "; "),
         if (length(res$errors) > 6L) "; ..." else "", call. = FALSE)
  }
  invisible(TRUE)
}

.v2_plain <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x)
    out <- lapply(x, .v2_plain)
    if (is.null(nms)) return(unname(out))
    out <- structure(out, names = nms)
    return(out)
  }
  x
}

# Semantic rules that the schema cannot express and that need no server
# (design section 4.1, the subset checkable on the client).
.v2_validate_plan_semantics <- function(plan) {
  pops <- plan$populations %||% list()
  vars <- plan$variables %||% list()
  outs <- plan$outputs %||% list()
  # population graph: parents, set-op members, match inputs, acyclic
  deps <- lapply(pops, function(p) unique(c(p$parent, unlist(p$setop$members),
                                            p$match$cases, p$match$controls)))
  for (id in names(pops)) {
    missing <- setdiff(deps[[id]], names(pops))
    if (length(missing)) .v2_stop("population '", id, "' references unknown population(s): ",
                                  paste(missing, collapse = ", "), ".")
    p <- pops[[id]]
    if (identical(p$kind, "setop")) {
      members <- unlist(p$setop$members)
      if (anyDuplicated(members)) .v2_stop("population '", id, "': set-op members must be distinct.")
      if (identical(p$setop$op, "setdiff") && length(members) != 2L) {
        .v2_stop("population '", id, "': setdiff takes exactly two members.")
      }
    }
    if (identical(p$kind, "match")) {
      if (!identical(pops[[p$match$cases]]$kind, "index")) {
        .v2_stop("population '", id, "': match cases must be an indexed population.")
      }
      if (!identical(pops[[p$match$controls]]$kind, "criteria")) {
        .v2_stop("population '", id, "': match controls must be a criteria population.")
      }
    }
  }
  state <- stats::setNames(rep(0L, length(pops)), names(pops))
  visit <- function(id, path) {
    if (state[[id]] == 1L) .v2_stop("Population dependency cycle: ",
                                    paste(c(path, id), collapse = " -> "), ".")
    if (state[[id]] == 2L) return(invisible())
    state[[id]] <<- 1L
    for (d in deps[[id]]) visit(d, c(path, id))
    state[[id]] <<- 2L
  }
  for (id in names(pops)) visit(id, character(0))
  # An index-relative age criterion needs an index (episodes) to refer to.
  for (id in names(pops)) {
    p <- pops[[id]]
    if (!p$kind %in% c("index", "criteria") || .v2_population_has_index(pops, id)) next
    uses_index_age <- FALSE
    .v2_walk(p$where, function(node) {
      if (is.list(node) && !is.null(node$age) && length(node) == 1L &&
          !is.list(node$age$reference)) uses_index_age <<- TRUE
    })
    if (uses_index_age) {
      .v2_stop("population '", id, "': an age criterion at the index needs an indexed ",
               "population (or a results cohort); give omop_age_is(year = ) for a fixed ",
               "reference year.")
    }
  }
  for (nm in names(outs)) {
    o <- outs[[nm]]
    context <- paste0("output '", nm, "'")
    if (is.null(pops[[o$rows]])) .v2_stop(context, " targets unknown population '", o$rows, "'.")
    indexed <- .v2_population_is_indexed(pops, o$rows)
    if (identical(o$grain, "person") && indexed && is.null(o$collapse)) {
      .v2_stop(context, ": person grain over the indexed population '", o$rows,
               "' needs collapse = \"first_episode\", \"last_episode\" or \"any\".")
    }
    if (!is.null(o$collapse) && !identical(o$grain, "person")) {
      .v2_stop(context, ": collapse applies only to person grain.")
    }
    missing <- setdiff(unlist(o$columns), names(vars))
    if (length(missing)) .v2_stop(context, " references unknown variable(s): ",
                                  paste(missing, collapse = ", "), ".")
    if (identical(o$format, "survival")) {
      tags <- .v2_index_source_tags(pops, o$rows)
      for (item in o$options$censor) {
        w <- if (is.list(item)) item$censor$when_index_source else NULL
        if (!is.null(w) && !is.null(tags)) {
          bad <- setdiff(unlist(w), tags)
          if (length(bad)) {
            .v2_stop(context, ": when_index_source tag(s) ", paste(bad, collapse = ", "),
                     " are not index sources of population '", o$rows, "' (",
                     paste(tags, collapse = ", "), ").")
          }
        }
      }
    }
    if (identical(o$format, "joined")) {
      from <- vapply(o$options$from, function(f) f$output, "")
      unknown <- setdiff(from, names(outs))
      if (length(unknown)) .v2_stop(context, ": joined input(s) ", paste(unknown, collapse = ", "),
                                    " are not outputs of the plan.")
      if (length(unique(vapply(outs[from], function(x) x$rows, ""))) != 1L ||
          !identical(outs[[from[[1L]]]]$rows, o$rows)) {
        .v2_stop(context, ": joined inputs must share the output's rows population.")
      }
    }
    if (identical(o$format, "dictionary")) {
      unknown <- setdiff(unlist(o$options$source_outputs), names(outs))
      if (length(unknown)) .v2_stop(context, ": unknown source output(s) ",
                                    paste(unknown, collapse = ", "), ".")
    }
    if (identical(o$format, "person_period") && length(o$options$carry_forward)) {
      srcs <- vapply(o$options$sources %||% list(), function(s) s$event %||% s$interval, "")
      bad <- setdiff(names(o$options$carry_forward), srcs)
      if (length(bad)) .v2_stop(context, ": carry_forward key(s) ", paste(bad, collapse = ", "),
                                " must name members of sources.")
    }
  }
  for (vn in names(vars)) {
    r <- vars[[vn]]$reducer
    if (identical(r$coverage, "effective_supply")) {
      iv <- vars[[vn]]$source$interval
      if (is.null(iv) || !isTRUE(plan$intervals[[iv]]$stockpile)) {
        .v2_stop("variable '", vn, "': coverage = \"effective_supply\" needs a stockpiled ",
                 "era (omop_era(stockpile = TRUE)) as its source.")
      }
    }
    if (!is.null(r) && r$fn %in% c("era_count", "era_days", "first_era_days") &&
        is.null(vars[[vn]]$source$interval)) {
      .v2_stop("variable '", vn, "': ", r$fn, " needs an era (interval) source.")
    }
  }
  unknown_pub <- setdiff(unlist(plan$publish), names(outs))
  if (length(unknown_pub)) .v2_stop("publish names unknown output(s): ",
                                    paste(unknown_pub, collapse = ", "), ".")
  invisible(TRUE)
}

# Possible index_source tags of a population (NULL when unknown statically).
.v2_index_source_tags <- function(pops, id) {
  p <- pops[[id]]
  if (is.null(p)) return(NULL)
  if (identical(p$kind, "index")) {
    src <- p$index$source
    if (!is.null(src$any_of)) return(vapply(src$any_of, function(s) s$event %||% s$interval, ""))
    if (!is.null(src$cohort_definition_id)) return(paste0("cohort:", src$cohort_definition_id))
    return(src$event %||% src$interval)
  }
  if (identical(p$kind, "match")) return(paste0("match:", id))
  if (!is.null(p$parent)) return(.v2_index_source_tags(pops, p$parent))
  if (identical(p$kind, "setop")) {
    return(unique(unlist(lapply(unlist(p$setop$members), .v2_index_source_tags, pops = pops))))
  }
  NULL
}

#' @export
print.omop_plan_v2 <- function(x, ...) {
  cat("omop_plan (language ", x$meta$language_version, ")\n", sep = "")
  for (slot in c("concept_sets", "events", "intervals", "populations", "variables", "outputs")) {
    n <- length(x[[slot]] %||% list())
    if (n) cat("  ", slot, ": ", paste(names(x[[slot]]), collapse = ", "), "\n", sep = "")
  }
  if (!is.null(x$publish)) cat("  publish:", paste(unlist(x$publish), collapse = ", "), "\n")
  invisible(x)
}

#' The plan-v2 (design section 4) JSON form of one language-2 object
#'
#' Compiles a single object the way \code{recipe_to_plan()} compiles it inside a
#' recipe and returns its wire node together with the registry nodes it
#' references (concept sets, events, intervals), so the object's JSON form can
#' be inspected or reused (section 12.1: "JSON = section 4").
#'
#' @param x A language-2 object (concept set, event, era, criterion, window,
#'   anchor, index, censor, reducer, match or variable) or a recipe.
#' @return \code{list(node, concept_sets, events, intervals)} (a recipe gives its
#'   whole plan).
#' @keywords internal
.v2_wire_of <- function(x) {
  if (inherits(x, "omop_recipe")) return(.v2_plain(recipe_to_plan(x, version = 2L)))
  ctx <- .v2_new_context(omop_recipe())
  ctx$reserved_names <- character(0)
  context <- paste0(class(x)[[1L]], " object")
  node <- if (inherits(x, "omop_concept_set")) {
    .v2c_concept_set(ctx, x, context)
  } else if (.v2_is_event(x)) {
    .v2w_event(ctx, x, context)
  } else if (.v2_is_interval(x)) {
    .v2w_interval(ctx, x, context)
  } else if (inherits(x, c("omop_criterion", "omop_filter", "omop_filter_group"))) {
    .v2w_criterion(ctx, x, context)
  } else if (inherits(x, "omop_window")) {
    .v2w_window(ctx, x, context)
  } else if (inherits(x, c("omop_anchor"))) {
    .v2w_anchor(ctx, x, context)
  } else if (inherits(x, "omop_bound")) {
    .v2w_endpoint(ctx, x, context)
  } else if (inherits(x, "omop_index")) {
    .v2w_index(ctx, x, context)
  } else if (inherits(x, "omop_censor")) {
    .v2w_censor_item(ctx, x, context)
  } else if (inherits(x, "omop_reducer")) {
    .v2w_reducer(ctx, x, context)
  } else if (inherits(x, "omop_match")) {
    .v2w_match(ctx, x, context)
  } else if (inherits(x, "omop_coverage")) {
    .v2w_coverage(x)
  } else if (inherits(x, "omop_variable")) {
    .v2w_variable(ctx, x, x$name %||% "variable")
  } else {
    stop("No plan-v2 JSON form for an object of class ", class(x)[[1L]],
         "; outputs and populations are compiled with their recipe (recipe_to_plan()).",
         call. = FALSE)
  }
  list(node = .v2_plain(node), concept_sets = ctx$concept_sets, events = ctx$events,
       intervals = ctx$intervals)
}
