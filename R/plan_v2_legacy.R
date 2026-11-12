# Legacy (2.10.1) output presets lowered onto language-2 formats.
#
# Design section 2.9 keeps survival, intervals, temporal_covariates and
# person_period as presets with their legacy defaults, and B-22 makes the
# sparse format reachable (the legacy covariates_sparse label). Each lowering
# reads the output's legacy variables as event sources: the variable's table,
# concepts (with descendants when expand = TRUE), concept_col, row filters and
# where, value column and visit (visit_filter or "index"). Sources carry
# visit/concept_col (design 2.2, 6.12) and covariate identities come from the
# server's per-plan registry (6.12), so the v1 refusals of those shapes do not
# apply here. Legacy behaviour the wire cannot carry is refused, naming it.

#' The language-2 event a legacy variable reads
#'
#' @param v A legacy \code{omop_variable} (table/format spelling).
#' @param context Character; error context.
#' @return An \code{omop_event} object (unnamed; interned by the compiler).
#' @keywords internal
.v2_legacy_variable_event <- function(v, context) {
  where <- .v2_lower_row_filters(v$filters, v$table, context)
  if (!is.null(v[["where"]])) where <- .v2_and_predicates(where, .v2w_predicate(v[["where"]]))
  visit <- if (!is.null(v$visit_filter)) {
    list(type = as.integer(unlist(v$visit_filter$concept_ids %||% v$visit_filter)))
  } else if (identical(v[["visit"]], "index")) "index" else NULL
  concepts <- if (length(v$concept_id %||% integer(0))) {
    omop_concept_set(v$concept_id, descendants = isTRUE(v$expand))
  } else NULL
  omop_event(v$table, concepts, where = if (!is.null(where)) .v2_predicate(where),
             visit = visit, value = v$value_source, concept_col = v$concept_col)
}

# Legacy variables that read the same rows except for their concepts become
# one source with the union of their concepts (the v1 per-table concept
# union); concept-scoped and unscoped variables of a table stay separate
# sources, as do variables with different narrowing (filters, where, visit,
# concept_col, value column, descendants).
.v2_legacy_source_key <- function(v, ids) {
  paste(deparse(list(table = v$table, scoped = length(ids) > 0L,
                     expand = isTRUE(v$expand), filters = v$filters,
                     where = v[["where"]], visit_filter = v$visit_filter,
                     visit = v[["visit"]], concept_col = v$concept_col,
                     value_source = v$value_source)), collapse = "")
}

.v2_legacy_grouped_events <- function(vars, context) {
  keys <- character(0)
  groups <- list()
  for (nm in names(vars)) {
    v <- vars[[nm]]
    ids <- as.integer(unlist(v$concept_id %||% integer(0)))
    key <- .v2_legacy_source_key(v, ids)
    i <- match(key, keys)
    if (is.na(i)) {
      keys <- c(keys, key)
      groups[[length(groups) + 1L]] <- list(v = v, ids = ids)
    } else {
      groups[[i]]$ids <- unique(c(groups[[i]]$ids, ids))
    }
  }
  lapply(groups, function(g) {
    v <- g$v
    v$concept_id <- if (length(g$ids)) g$ids else NULL
    .v2_legacy_variable_event(v, context)
  })
}

.v2_legacy_check_options <- function(o, allowed, context) {
  unknown <- setdiff(names(o %||% list()), allowed)
  if (length(unknown)) {
    .v2_stop(context, ": legacy option(s) ", paste(unknown, collapse = ", "),
             " have no v2 lowering.")
  }
  invisible(TRUE)
}

.v2_legacy_check_vars <- function(vars, formats, context, allow_window = TRUE) {
  if (length(vars) == 0L) .v2_stop(context, " has no variables to compile.")
  for (nm in names(vars)) {
    v <- vars[[nm]]
    if (.v2_is_v2_variable(v)) {
      .v2_stop(context, ": variable '", nm, "' is a language-2 variable; give the ",
               "sources of a format = output instead of a legacy type.")
    }
    if (!(v$format %||% "raw") %in% formats || !is.null(v$column)) {
      .v2_stop(context, ": variable '", nm, "' must be a concept variable (format ",
               paste(formats, collapse = "/"), ") without a raw column.")
    }
    if (!allow_window && !is.null(v$time_window)) {
      .v2_stop(context, ": variable '", nm, "' has its own time_window, which this ",
               "output cannot carry (the output window is the risk window).")
    }
  }
  invisible(TRUE)
}

# The v1 rules on concept scope per table that stay in 2.10.3: exact and
# descendant concepts are not mixed in one table (descendant scope stays
# explicit), and, where the format keeps one concept set per table
# (temporal covariates), concept-scoped and all-concepts variables of a table
# are not mixed and an all-concepts variable does not expand descendants.
.v2_legacy_check_concept_scope <- function(vars, context, one_scope_per_table = FALSE) {
  tables <- unique(vapply(vars, function(v) v$table, character(1)))
  for (tbl in tables) {
    vs <- Filter(function(v) identical(v$table, tbl), vars)
    scoped <- vapply(vs, function(v) length(v$concept_id %||% integer(0)) > 0L, logical(1))
    expands <- vapply(vs, function(v) isTRUE(v$expand), logical(1))
    if (one_scope_per_table && any(scoped) && !all(scoped)) {
      .v2_stop(context, ": cannot mix concept-scoped variables with an all-concepts ",
               "variable in table '", tbl, "'; split them into outputs.")
    }
    if (one_scope_per_table && !any(scoped) && any(expands)) {
      .v2_stop(context, ": cannot expand descendants without a concept_id.")
    }
    if (any(expands[scoped]) && !all(expands[scoped])) {
      .v2_stop(context, ": mixes expanded and exact concepts in table '", tbl,
               "'; split them so descendant scope remains explicit.")
    }
  }
  invisible(TRUE)
}

# The single index-relative window shared by the variables (NULL when none
# has one); mixed or conflicting variable windows are refused (one output
# window has no per-source scope).
.v2_legacy_common_window <- function(vars, context, output_name) {
  tryCatch(.common_recipe_time_window(vars, output_name),
           error = function(e) .v2_stop(context, ": ", conditionMessage(e)))
}

.v2_legacy_window <- function(start, end) {
  omop_window(if (is.null(start)) -Inf else start, if (is.null(end)) Inf else end)
}

#' Legacy survival preset lowered to the language-2 survival format
#'
#' Outcomes are the output's concept variables (one composite outcome over
#' their concepts, or one named endpoint per variable); the TAR keeps the
#' legacy default of 0 to 730 days after index (F-4); censoring keeps the
#' legacy rules: the end of the observation period containing the index
#' (unless \code{censoring$observation_period_end = FALSE}), death when
#' \code{censoring$death = TRUE}, an administrative date when given, and the
#' mandatory cohort-end cap as \code{"index_end"}.
#' @keywords internal
.v2_legacy_survival_options <- function(ctx, out, vars, context) {
  o <- out$options %||% list()
  .v2_legacy_check_options(o, c("tar", "event_order", "outcome_mode", "censoring", "format",
                                "washout_days", "tie_policy", "transitions",
                                "initial_state", "state_hierarchy", "state_step"), context)
  .v2_legacy_check_vars(vars, c("raw", "binary"), context, allow_window = FALSE)
  for (nm in names(vars)) {
    v <- vars[[nm]]
    if (!is.null(v$value_source)) {
      .v2_stop(context, ": outcome variable '", nm, "' has a value column; an outcome ",
               "is an event, not a value.")
    }
    if (length(v$concept_id %||% integer(0)) == 0L) {
      .v2_stop(context, ": outcome variable '", nm, "' must be concept-scoped.")
    }
  }
  format <- tolower(o$format %||% "survival")
  if (identical(format, "multi_state") || !is.null(o$transitions) ||
      !is.null(o$initial_state) || !is.null(o$state_hierarchy) || !is.null(o$state_step)) {
    .v2_stop(context, ": a legacy multi-state survival output has no v2 lowering; it ",
             "executes as its v1 plan (recipe_to_plan(version = 1)), or write ",
             "omop_output(format = \"survival\", options = list(format = \"multi_state\", ",
             "transitions = ...)) in language 2.")
  }
  mode <- tolower(o$outcome_mode %||% if (identical(format, "survival")) "composite" else "named")
  if (identical(mode, "composite") &&
      length(unique(vapply(vars, function(v) v$table, character(1)))) != 1L) {
    .v2_stop(context, ": a composite survival output must use outcome variables from ",
             "exactly one OMOP table; use outcome_mode='named' to keep endpoints from ",
             "multiple tables.")
  }
  if (identical(mode, "composite")) .v2_legacy_check_concept_scope(vars, context)
  outcomes <- if (identical(mode, "named")) {
    used <- character(0)
    res <- list()
    for (nm in names(vars)) {
      en <- .ensure_unique_name(.sanitize_name(vars[[nm]]$name %||% nm), used)
      used <- c(used, en)
      res[[en]] <- .v2_legacy_variable_event(vars[[nm]], paste0(context, " outcome '", en, "'"))
    }
    res
  } else {
    evs <- .v2_legacy_grouped_events(vars, context)
    if (length(evs) == 1L) list(outcome = evs[[1L]]) else
      stats::setNames(evs, paste0("outcome_", seq_along(evs)))
  }
  tar <- o$tar %||% list(start_offset = 0L, end_offset = 730L)
  if (!is.list(tar) || length(setdiff(names(tar), c("start_offset", "end_offset")))) {
    .v2_stop(context, ": tar must be list(start_offset =, end_offset =).")
  }
  end <- tar$end_offset
  if (!is.null(end) && is.infinite(end)) end <- NULL
  cz <- o$censoring %||% list()
  if (length(cz) && (!is.list(cz) || is.null(names(cz)) || any(!nzchar(names(cz))) ||
                     length(setdiff(names(cz), c("cohort_end", "observation_period_end",
                                                 "death", "admin_date"))))) {
    .v2_stop(context, ": censoring contains unsupported fields.")
  }
  for (field in intersect(names(cz), c("cohort_end", "observation_period_end", "death"))) {
    if (!is.logical(cz[[field]]) || length(cz[[field]]) != 1L || is.na(cz[[field]])) {
      .v2_stop(context, ": censoring$", field, " must be TRUE or FALSE.")
    }
  }
  if (isFALSE(cz$cohort_end)) {
    .v2_stop(context, ": censoring$cohort_end must remain TRUE (the legacy cohort-end cap).")
  }
  censor <- list()
  if (!isFALSE(cz$observation_period_end)) censor <- c(censor, list("observation_end"))
  if (isTRUE(cz$death)) censor <- c(censor, list("death"))
  if (!is.null(cz$admin_date)) censor <- c(censor, list(list(admin_date = cz$admin_date)))
  censor <- c(censor, list("index_end"))
  o2 <- list(outcomes = outcomes,
             tar = .v2_legacy_window(tar$start_offset %||% 0L, end),
             censor = censor, format = format, outcome_mode = mode,
             event_order = o$event_order, washout_days = o$washout_days,
             tie_policy = o$tie_policy, transitions = o$transitions,
             initial_state = o$initial_state, state_hierarchy = o$state_hierarchy,
             state_step = o$state_step)
  .v2w_options_survival(ctx, Filter(Negate(is.null), o2), context)
}

#' Legacy intervals preset lowered to the language-2 intervals format
#' @keywords internal
.v2_legacy_intervals_options <- function(ctx, out, vars, context) {
  o <- out$options %||% list()
  .v2_legacy_check_options(o, c("window", "interval_match", "event_select", "select_n",
                                "select_by", "anchor"), context)
  .v2_legacy_check_vars(vars, "raw", context)
  for (nm in names(vars)) {
    if (!is.null(vars[[nm]]$value_source)) {
      .v2_stop(context, ": variable '", nm, "' has a value column; intervals return ",
               "event spans.")
    }
  }
  if (!is.null(o$anchor) && !identical(as.integer(o$anchor), 0L)) {
    .v2_stop(context, ": a legacy anchor shifted from index has no v2 lowering; use an ",
             "omop_anchor() in a format = \"intervals\" output.")
  }
  .v2_legacy_check_concept_scope(vars, context)
  vw <- .v2_legacy_common_window(vars, context, out$name)
  window <- if (!is.null(o$window)) .normalize_recipe_time_window(o$window, out$name) else vw
  if (!is.null(o$window) && !is.null(vw) && !identical(window, vw)) {
    .v2_stop(context, ": conflicting variable and output windows.")
  }
  o2 <- list(sources = .v2_legacy_grouped_events(vars, context),
             window = if (!is.null(window)) .v2_legacy_window(window$start, window$end),
             interval_match = o$interval_match, event_select = o$event_select,
             select_n = o$select_n, select_by = o$select_by)
  .v2w_options(ctx, "intervals", Filter(Negate(is.null), o2), context)
}

# A legacy variable's temporal analysis.
.v2_legacy_analysis <- function(v) if (identical(v$format, "count")) "count" else "binary"

# The variables declare the (concept, analysis) pairs; a legacy
# options$analyses may only order them: naming an analysis no variable
# requests, or omitting a requested one, is refused (the v1 compiler refused
# the undeclared product).
.v2_legacy_check_analyses <- function(per_var, analyses, context) {
  omitted <- setdiff(per_var, analyses)
  if (length(omitted)) {
    .v2_stop(context, ": the variables request the ", paste(unique(omitted), collapse = ", "),
             " analysis, which options$analyses omits.")
  }
  extra <- setdiff(analyses, per_var)
  if (length(extra)) {
    .v2_stop(context, ": options$analyses names ", paste(extra, collapse = ", "),
             ", which no variable requests; declare it with a variable of that format.")
  }
  invisible(TRUE)
}

# Temporal sources: the variables of one narrowing group read the same rows;
# their concepts are split by the analyses requested for each concept, so a
# source's concepts share one analysis set (A-14: sources whose concept sets
# need different analysis selections stay distinct). Returns the events and,
# per event, its analyses in the output's order.
.v2_legacy_temporal_sources <- function(vars, analyses, context) {
  keys <- character(0)
  groups <- list()
  for (nm in names(vars)) {
    v <- vars[[nm]]
    a <- .v2_legacy_analysis(v)
    ids <- as.integer(unlist(v$concept_id %||% integer(0)))
    key <- .v2_legacy_source_key(v, ids)
    i <- match(key, keys)
    if (is.na(i)) {
      keys <- c(keys, key)
      groups[[length(groups) + 1L]] <- list(v = v, by_concept = list(), unscoped = character(0))
      i <- length(groups)
    }
    if (length(ids)) {
      for (cid in as.character(ids)) {
        groups[[i]]$by_concept[[cid]] <- union(groups[[i]]$by_concept[[cid]], a)
      }
    } else {
      groups[[i]]$unscoped <- union(groups[[i]]$unscoped, a)
    }
  }
  events <- list()
  sets <- list()
  in_order <- function(set) analyses[analyses %in% set]
  for (g in groups) {
    if (length(g$by_concept)) {
      set_key <- vapply(g$by_concept, function(x) paste(in_order(x), collapse = ","), "")
      for (sk in unique(set_key)) {
        v <- g$v
        v$concept_id <- as.integer(names(g$by_concept)[set_key == sk])
        events[[length(events) + 1L]] <- .v2_legacy_variable_event(v, context)
        sets[[length(sets) + 1L]] <- in_order(g$by_concept[[which(set_key == sk)[[1L]]]])
      }
    } else {
      v <- g$v
      v$concept_id <- NULL
      events[[length(events) + 1L]] <- .v2_legacy_variable_event(v, context)
      sets[[length(sets) + 1L]] <- in_order(g$unscoped)
    }
  }
  list(events = events, sets = sets)
}

#' Legacy temporal-covariates and person-period presets lowered to v2
#'
#' Sources are the groups of variables reading the same rows, split by the
#' analyses each concept requests (\code{count} for count variables,
#' \code{binary} otherwise); the output's analyses are the requested ones
#' unless \code{options$analyses} names them. A source whose analyses differ
#' from the output's carries them in \code{source_analyses} (A-14), so only
#' the declared \code{(source, concept, analysis)} pairs are computed, each a
#' covariate identity of the server's registry (design 6.12).
#' @keywords internal
.v2_legacy_temporal_options <- function(ctx, out, vars, context, format) {
  o <- out$options %||% list()
  .v2_legacy_check_options(o, c("bin_width", "window_start", "window_end", "analyses"),
                           context)
  .v2_legacy_check_vars(vars, c("raw", "binary", "count"), context)
  for (nm in names(vars)) {
    if (!is.null(vars[[nm]]$value_source)) {
      .v2_stop(context, ": variable '", nm, "' has a value column; temporal covariates ",
               "are presence/count analyses.")
    }
  }
  .v2_legacy_check_concept_scope(vars, context, one_scope_per_table = TRUE)
  vw <- .v2_legacy_common_window(vars, context, out$name)
  ws <- o$window_start %||% vw$start %||% -365L
  we <- o$window_end %||% vw$end %||% 0L
  if (!is.null(vw$start) && !is.null(o$window_start) && as.integer(o$window_start) != vw$start) {
    .v2_stop(context, ": conflicting variable and output window_start.")
  }
  if (!is.null(vw$end) && !is.null(o$window_end) && as.integer(o$window_end) != vw$end) {
    .v2_stop(context, ": conflicting variable and output window_end.")
  }
  per_var <- vapply(vars, .v2_legacy_analysis, character(1))
  analyses <- as.character(unlist(o$analyses %||% unique(per_var)))
  .v2_legacy_check_analyses(per_var, analyses, context)
  src <- .v2_legacy_temporal_sources(vars, analyses, context)
  o2 <- list(sources = src$events, window = .v2_legacy_window(ws, we), analyses = analyses)
  if (identical(format, "person_period")) {
    o2$bins <- list(width = o$bin_width %||% 30L)
  } else {
    o2$bin_width <- o$bin_width %||% 30L
  }
  res <- .v2w_options(ctx, format, o2, context)
  refs <- vapply(res$sources, function(r) as.character(r$event), "")
  if (anyDuplicated(refs)) {
    .v2_stop(context, ": two sources resolve to one event; give the variables distinct scopes.")
  }
  sa <- list()
  for (i in seq_along(refs)) {
    if (!identical(src$sets[[i]], analyses)) sa[[refs[[i]]]] <- .v2_array(src$sets[[i]])
  }
  if (length(sa)) res$source_analyses <- sa
  res
}

#' Legacy covariates_sparse label lowered to the language-2 sparse format (B-22)
#' @keywords internal
.v2_legacy_sparse_options <- function(ctx, out, vars, context) {
  o <- out$options %||% list()
  .v2_legacy_check_options(o, "analyses", context)
  .v2_legacy_check_vars(vars, c("raw", "binary", "count"), context, allow_window = FALSE)
  for (nm in names(vars)) {
    if (!is.null(vars[[nm]]$value_source)) {
      .v2_stop(context, ": variable '", nm, "' has a value column; sparse covariates ",
               "are presence/count analyses.")
    }
  }
  per_var <- vapply(vars, .v2_legacy_analysis, character(1))
  analyses <- as.character(unlist(o$analyses %||% unique(per_var)))
  .v2_legacy_check_analyses(per_var, analyses, context)
  # The sparse format has no per-source analysis selection: different analyses
  # per concept would compute the undeclared pairs too, so they are refused.
  src <- .v2_legacy_temporal_sources(vars, analyses, context)
  if (!all(vapply(src$sets, identical, logical(1), analyses))) {
    .v2_stop(context, ": the variables declare different analyses per concept, which a ",
             "sparse output cannot select per source; split them into outputs with one ",
             "analysis set each.")
  }
  .v2w_options(ctx, "sparse", list(sources = .v2_legacy_grouped_events(vars, context),
                                   analyses = analyses), context)
}

# Options of a legacy output whose preset lowers onto a source-based format.
.v2_legacy_source_output_options <- function(ctx, out, format, vars, context) {
  switch(format,
    survival = .v2_legacy_survival_options(ctx, out, vars, context),
    intervals = .v2_legacy_intervals_options(ctx, out, vars, context),
    temporal_covariates = , person_period =
      .v2_legacy_temporal_options(ctx, out, vars, context, format),
    sparse = .v2_legacy_sparse_options(ctx, out, vars, context))
}
