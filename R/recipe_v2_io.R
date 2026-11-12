# Module: Recipe language 2.0 -- serialisation of the language objects
# (JSON / YAML export and import, recipe_to_code()).
#
# Every language-2 object is described by its constructor call ("ctor spec":
# the constructor name and its canonical arguments). The same spec drives the
# plain (JSON/YAML) form -- the arguments plus an `omop_ctor` tag --, the
# import (the constructor is called again, so imported objects are validated
# exactly like hand-written ones) and the generated R code.

.V2_TAGGED_CLASSES <- c("omop_concept_set", "omop_event", "omop_predicate", "omop_unit",
                        "omop_criterion", "omop_window", "omop_bound", "omop_anchor",
                        "omop_index", "omop_any_of", "omop_end", "omop_coverage",
                        "omop_interval", "omop_censor", "omop_reducer", "omop_match",
                        "omop_match_item", "omop_param")

.V2_CTOR_TAG <- "omop_ctor"

.v2_is_tagged <- function(x) {
  inherits(x, .V2_TAGGED_CLASSES) || .v2_is_v2_variable(x)
}

# Constructor name and arguments that rebuild a language-2 object.
.v2_ctor_spec <- function(x) {
  if (.v2_is_v2_variable(x)) {
    if (identical(x$kind, "reducer")) {
      return(list(ctor = "omop_variable",
                  args = list(name = x$name, event = x$event, reducer = x$reducer,
                              where = x$where, visit = x$visit,
                              broadcast = if (isTRUE(x$broadcast)) TRUE)))
    }
    if (identical(x$kind, "population_column")) {
      return(list(ctor = "omop_population_column", args = list(name = x$population_column),
                  name = x$name))
    }
    d <- x$derived
    spec <- switch(d$kind,
      age = list(ctor = "omop_age", args = list(reference = d$reference,
                 reference_date = d$reference_date, bands = d$bands, age_breaks = d$age_breaks)),
      sex_mf = list(ctor = "omop_sex", args = list()),
      race = list(ctor = "omop_race", args = list()),
      ethnicity = list(ctor = "omop_ethnicity", args = list()),
      birth_year_band = list(ctor = "omop_birth_year_band", args = list(width = d$width)),
      died = list(ctor = "omop_died", args = list(before = d$before)),
      days_to_death = list(ctor = "omop_days_to_death", args = list()),
      n_observation_periods = list(ctor = "omop_n_observation_periods", args = list()),
      obs_duration = list(ctor = "omop_obs_duration", args = list(period_policy = d$period_policy)),
      prior_obs = list(ctor = "omop_prior_obs", args = list(reference_date = d$reference_date)),
      followup = list(ctor = "omop_followup", args = list(reference_date = d$reference_date)),
      observed_days = list(ctor = "omop_observed_days", args = list(window = d$window)),
      list(ctor = "omop_score", args = list(type = d$kind, reference_date = d$reference_date)))
    spec$name <- x$name
    return(spec)
  }
  cls <- class(x)[[1L]]
  switch(cls,
    omop_param = list(ctor = "omop_param", args = list(
      name = x$name, type = x$type, default = x$default, description = x$description)),
    omop_param_op = list(ctor = ".v2_param_op", args = list(
      op = x$op, e1 = x$args[[1L]], e2 = if (length(x$args) > 1L) x$args[[2L]])),
    omop_concept_set = list(ctor = "omop_concept_set", args = list(
      concepts = x$concepts, descendants = x$descendants, mapped = x$mapped,
      exclude = x$exclude, name = x$name)),
    omop_event = switch(x$kind %||% "table",
      table = list(ctor = "omop_event", args = list(
        table = x$table, concepts = x$concepts, where = x$where, visit = x$visit,
        value = x$value, unit = x$unit, concept_col = x$concept_col, end = x$end,
        name = x$name, on_missing_table = x$on_missing_table)),
      episode_members = list(ctor = "omop_episode_members", args = list(
        episode = x$episode, tables = x$tables, where = x$where, name = x$name,
        on_missing_table = x$on_missing_table)),
      cost = list(ctor = "omop_cost", args = list(
        event = x$event, field = x$field, currency = x$currency, cost_type = x$cost_type,
        where = x$where, name = x$name, on_missing_table = x$on_missing_table))),
    omop_predicate = list(ctor = "omop_where_tree", args = list(tree = x$tree)),
    omop_unit = list(ctor = "omop_unit", args = list(
      target = x$target, convert = x$convert, unknown = x$unknown, censored = x$censored)),
    omop_criterion = switch(x$kind,
      has = list(ctor = "omop_has", args = list(event = x$event, n = x$n, op = x$op,
                                                window = x$window, distinct = x$distinct)),
      count_is = list(ctor = "omop_count_is", args = list(event = x$event, op = x$op, n = x$n,
                                                          window = x$window, distinct = x$distinct)),
      has_not = list(ctor = "omop_has_not", args = list(event = x$event, window = x$window,
                                                        nonmissing = x$nonmissing)),
      not = list(ctor = "omop_not", args = list(criterion = x$criterion)),
      all = list(ctor = "omop_all", args = unname(x$criteria)),
      any = list(ctor = "omop_any", args = unname(x$criteria)),
      person = list(ctor = "omop_person", args = list(where = x$where)),
      age_is = list(ctor = "omop_age_is", args = list(min = x$min, max = x$max,
                                                      reference = x$reference, year = x$year)),
      observed = list(ctor = "omop_observed", args = list(before = x$before, after = x$after,
                                                          gap = x$gap)),
      in_cohort = list(ctor = "omop_in_cohort",
                       args = list(cohort_definition_id = x$cohort_definition_id))),
    omop_window = list(ctor = "omop_window", args = list(start = x$start, end = x$end,
                                                         anchor = x$anchor)),
    omop_bound = list(ctor = "omop_bound", args = list(anchor = x$anchor, offset = x$offset)),
    omop_anchor = list(ctor = "omop_anchor", args = list(
      event = x$event, which = x$which, n = x$n, field = x$field, scope = x$scope,
      search = x$search)),
    omop_index = list(ctor = "omop_index", args = list(
      source = x$source, select = x$select, n = x$n, offset = x$offset, end = x$end,
      restrict_to_observation = x$restrict_to_observation)),
    omop_any_of = list(ctor = "omop_any_of", args = unname(x$arms)),
    omop_end = list(ctor = "omop_end", args = list(strategy = x$strategy, days = x$days,
                                                   pad = x$pad)),
    omop_coverage = list(ctor = "omop_coverage", args = list(
      policy = x$policy, on_ambiguous = x$on_ambiguous, on_missing = x$on_missing, gap = x$gap)),
    omop_interval = list(ctor = "omop_era", args = list(
      event = x$event, gap = x$gap, persistence = x$persistence, min_days = x$min_days,
      by = x$by, unmapped = x$unmapped, end = x$end, fallback = x$fallback,
      stockpile = x$stockpile, name = x$name)),
    omop_censor = list(ctor = "omop_censor", args = list(
      source = x$source, which = x$which, offset = x$offset, search = x$search,
      missing = x$missing, when_index_source = x$when_index_source)),
    omop_reducer = list(ctor = "omop_reduce", args = list(
      fn = x$fn, value = x$value, window = x$window, which = x$which, n = x$n,
      side = x$side, missing = x$missing, distinct = x$distinct, p = x$p, agg = x$agg,
      reference = x$reference, predicate = x$predicate, reference_date = x$reference_date,
      unit = x$unit, coverage = x$coverage, fills = x$fills,
      observed_only = x$observed_only)),
    omop_match = list(ctor = "omop_match", args = list(
      cases = x$cases, controls = x$controls, outcome = x$outcome, at = x$at,
      match = x$match, controls_per_case = x$controls_per_case,
      prior_outcome = x$prior_outcome, future_cases = x$future_cases,
      replacement = x$replacement, seed = x$seed, insufficient = x$insufficient,
      missing_strata = x$missing_strata, max_candidates = x$max_candidates)),
    omop_match_item = if (identical(x$kind, "age_band")) {
      list(ctor = "omop_age_band", args = list(width = x$width))
    } else {
      list(ctor = "omop_strata", args = list(event = x$event, window = x$window))
    },
    stop("No constructor spec for class ", cls, ".", call. = FALSE))
}

# Plain (JSON/YAML-ready) form of any value: language-2 objects become their
# tagged constructor arguments; everything else keeps its plain structure.
.v2_plain_value <- function(x) {
  if (.v2_is_tagged(x)) {
    spec <- .v2_ctor_spec(x)
    args <- lapply(spec$args, .v2_plain_value)
    out <- stats::setNames(list(spec$ctor), .V2_CTOR_TAG)
    if (length(args) && is.null(names(spec$args))) {
      # Positional (dots) constructors keep their items as one array.
      out$items <- unname(args)
    } else if (length(args)) {
      out <- c(out, args)
    }
    if (!is.null(spec$name)) out$variable_name <- spec$name
    if (!is.null(attr(x, "omop_namespace", exact = TRUE))) {
      out$omop_namespace <- .v2_plain_value(attr(x, "omop_namespace", exact = TRUE))
    }
    return(out)
  }
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x)
    out <- lapply(x, .v2_plain_value)
    attributes(out) <- NULL
    names(out) <- nms
    return(out)
  }
  # Named atomic vectors (e.g. c(factor = , offset = )) keep their names.
  if (is.atomic(x) && !is.null(names(x))) return(as.list(x))
  x
}

# Inverse of .v2_plain_value(): rebuild tagged objects through their
# constructors (re-validating them), recursively.
.v2_restore <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  ctor <- x[[.V2_CTOR_TAG]]
  if (is.null(ctor)) {
    nms <- names(x)
    out <- lapply(x, .v2_restore)
    names(out) <- nms
    return(out)
  }
  ctor <- as.character(ctor)[[1L]]
  allowed <- c("omop_variable", "omop_population_column", "omop_age", "omop_sex", "omop_race",
               "omop_ethnicity", "omop_birth_year_band", "omop_died", "omop_days_to_death",
               "omop_n_observation_periods", "omop_obs_duration", "omop_prior_obs",
               "omop_followup", "omop_observed_days", "omop_score", "omop_concept_set",
               "omop_event", "omop_episode_members", "omop_cost", "omop_where_tree",
               "omop_unit", "omop_has", "omop_count_is", "omop_has_not", "omop_not", "omop_all",
               "omop_any", "omop_person", "omop_age_is", "omop_observed", "omop_in_cohort",
               "omop_window", "omop_bound", "omop_anchor", "omop_index", "omop_any_of",
               "omop_end", "omop_coverage", "omop_era", "omop_censor", "omop_reduce",
               "omop_match", "omop_age_band", "omop_strata", "omop_param", ".v2_param_op")
  if (!ctor %in% allowed) stop("Unknown language-2 object '", ctor, "' in the recipe.", call. = FALSE)
  variable_name <- x$variable_name
  namespace <- x$omop_namespace
  x$omop_namespace <- NULL
  x[[.V2_CTOR_TAG]] <- NULL
  x$variable_name <- NULL
  if (!is.null(x$items)) {
    args <- lapply(x$items, .v2_restore)
  } else {
    args <- lapply(x, .v2_restore)
    names(args) <- names(x)
    # JSON arrays of scalars come back as lists: restore atomic vectors (a
    # predicate tree, match items and nested objects stay lists).
    keep_list <- c("tree", "match", "criteria", "convert")
    for (nm in setdiff(names(args), keep_list)) {
      a <- args[[nm]]
      if (is.list(a) && length(a) > 0L && is.null(names(a)) &&
          all(vapply(a, function(e) is.atomic(e) && length(e) == 1L, logical(1)))) {
        args[[nm]] <- unlist(a, use.names = FALSE)
      }
    }
    if (identical(ctor, "omop_unit") && is.list(args$convert)) {
      args$convert <- lapply(args$convert, unlist)
    }
    if (identical(ctor, "omop_match") && is.list(args$match)) {
      args$match <- lapply(args$match, function(it) if (is.list(it)) it else as.character(it))
    }
  }
  # A NULL stored argument always equals its constructor default.
  if (!is.null(names(args))) args <- args[!vapply(args, is.null, logical(1))]
  obj <- do.call(ctor, args)
  if (!is.null(variable_name)) obj$name <- variable_name
  if (!is.null(namespace)) {
    if (!inherits(obj, "omop_param") || !is.list(namespace) ||
        !all(vapply(namespace, function(step) {
          is.list(step) && is.character(step$prefix) && length(step$prefix) == 1L &&
            !is.na(step$prefix) && is.list(step$maps)
        }, logical(1)))) {
      stop("Invalid parameter reference namespace in the recipe.", call. = FALSE)
    }
    attr(obj, "omop_namespace") <- namespace
  }
  obj
}

# R code that rebuilds a language-2 object.
.v2_codegen <- function(x) {
  if (inherits(x, "omop_param_op")) {
    # A parameter expression is written as R arithmetic on its placeholders.
    operand <- function(a) {
      if (inherits(a, "omop_param_op")) return(paste0("(", .v2_codegen(a), ")"))
      if (.v2_is_param(a)) return(.v2_codegen(a))
      .codegen_value(a)
    }
    if (length(x$args) == 1L) return(paste0("-", operand(x$args[[1L]])))
    return(paste(operand(x$args[[1L]]), x$op, operand(x$args[[2L]])))
  }
  spec <- .v2_ctor_spec(x)
  arg_code <- function(v) {
    if (.v2_is_tagged(v)) return(.codegen_raw(.v2_codegen(v)))
    if (.v2_has_tagged(v)) return(.codegen_raw(.v2_codegen_list(v)))
    v
  }
  args <- lapply(spec$args, arg_code)
  value_code <- function(a) if (inherits(a, "codegen_raw")) as.character(a) else .codegen_value(a)
  if (is.null(names(spec$args))) {
    parts <- vapply(args, value_code, "")
  } else {
    keep <- !vapply(args, is.null, logical(1))
    parts <- vapply(names(args)[keep], function(nm) {
      paste0(nm, " = ", value_code(args[[nm]]))
    }, "")
  }
  # Built by hand: do.call(.codegen_call, ...) would partially match an
  # argument called `fn` to its `fn_name` formal.
  code <- paste0(spec$ctor, "(", paste(parts, collapse = ", "), ")")
  namespace <- attr(x, "omop_namespace", exact = TRUE)
  if (!is.null(namespace)) {
    code <- paste0("structure(", code, ", omop_namespace = ", .codegen_value(namespace), ")")
  }
  code
}

# A serialised visit restriction: "index" or list(type = <ids>).
.v2_restore_visit <- function(x) {
  if (is.null(x) || is.character(x)) return(x)
  if (is.list(x) && !is.null(x$type)) return(list(type = as.integer(unlist(x$type))))
  x
}

# Does x hold a language-2 object at any depth?
.v2_has_tagged <- function(x) {
  if (.v2_is_tagged(x)) return(TRUE)
  is.list(x) && !is.data.frame(x) && any(vapply(x, .v2_has_tagged, logical(1)))
}

# R code of a (possibly named) list mixing language-2 objects and plain values.
.v2_codegen_list <- function(x) {
  nms <- names(x)
  parts <- vapply(seq_along(x), function(i) {
    el <- x[[i]]
    code <- if (.v2_is_tagged(el)) .v2_codegen(el) else
      if (.v2_has_tagged(el)) .v2_codegen_list(el) else .codegen_value(el)
    if (!is.null(nms) && nzchar(nms[[i]])) paste0(.codegen_name(nms[[i]]), " = ", code) else code
  }, "")
  paste0("list(", paste(parts, collapse = ", "), ")")
}
