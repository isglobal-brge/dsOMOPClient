# Module: Recipe language 2.0 -- explain and the default-visibility lint rules
# (design-2.10.3 sections 6.15, 7.6 and the defect ledger F-4, F-5, B-7, B-8).

#' Explain a recipe: its semantics and every default it relies on
#'
#' Renders what a recipe means as prose: each population (index, selection,
#' end, observation coverage, criteria), each variable (reducer, window,
#' missing-value rule, the column class of what it projects), each output
#' (grain, collapse, follow-up, censoring, representation) and the
#' publication roots. Every default the recipe relies on is stated (for
#' example the 730-day follow-up of a survival output without \code{tar}, the
#' banded \code{age_group} of a baseline output, the unique/drop/drop
#' observation coverage of an indexed population). No counts and no SQL are
#' shown.
#'
#' Without connections the explanation is rendered from the client-side
#' normalised plan. With \code{conns}, each server's
#' \code{omopPlanExplainDS} (dsOMOP 2.10.3) is also asked for its normalised
#' plan, the role records used, the column classes and the dialect preflight,
#' which are attached as \code{$server}.
#'
#' @param recipe An \code{omop_recipe}.
#' @param detail \code{"summary"} (one line per node) or \code{"semantics"}
#'   (every default and rule spelled out).
#' @param conns Optional DSI connections (uses the active OMOP session).
#' @param symbol OMOP session symbol used with \code{conns}.
#' @return An \code{omop_recipe_explain} object: \code{$text} (character
#'   lines), \code{$plan} (the normalised plan), \code{$defaults} (a data
#'   frame of the defaults relied on) and \code{$server} (per-server explain
#'   payloads, when \code{conns} is given). Printing it shows the text.
#' @examples
#' \dontrun{
#' recipe_explain(recipe, detail = "semantics")
#' }
#' @seealso \code{\link{recipe_lint}}, \code{\link{recipe_to_plan}}
#' @export
recipe_explain <- function(recipe, detail = c("summary", "semantics"), conns = NULL,
                           symbol = "omop") {
  .v2_recipe_check(recipe)
  detail <- match.arg(detail)
  plan <- tryCatch(recipe_to_plan(recipe, version = 2L), error = function(e) e)
  if (inherits(plan, "error")) {
    if (length(.recipe_needs_v2(recipe))) stop(conditionMessage(plan), call. = FALSE)
    return(.explain_v1(recipe, conditionMessage(plan)))
  }
  norm <- .planNormalizeV2(plan)
  defaults <- .explain_defaults(recipe, norm)
  text <- .explain_text(norm, defaults, detail)
  server <- NULL
  if (!is.null(conns)) {
    session <- .get_session(symbol)
    .v2_assert_supported(plan, session, names(conns))
    server <- .ds_safe_aggregate(conns, expr = call("omopPlanExplainDS", session$res_symbol,
                                                    .ds_encode_lossless(plan)))
  }
  structure(list(text = text, plan = norm, defaults = defaults, server = server,
                 digest = .planSemanticDigest(norm)$sha256),
            class = c("omop_recipe_explain", "list"))
}

#' @export
print.omop_recipe_explain <- function(x, ...) {
  cat(x$text, sep = "\n")
  if (!is.null(x$server)) cat("\n[server explain attached for:",
                              paste(names(x$server), collapse = ", "), "]\n")
  invisible(x)
}

# A recipe whose outputs have no v2 lowering is explained from its v1 shape.
.explain_v1 <- function(recipe, why) {
  lines <- c("Recipe (2.10.1 constructors; executes as its unchanged v1 plan)",
             paste0("  (no language-2 rendering: ", why, ")"))
  defaults <- .explain_defaults(recipe, NULL)
  for (nm in names(recipe$outputs %||% list())) {
    o <- recipe$outputs[[nm]]
    lines <- c(lines, paste0("Output '", nm, "' [", o$type %||% o$format, "] on population '",
                             o$population_id %||% "base", "'"))
  }
  if (nrow(defaults)) {
    lines <- c(lines, "Defaults relied on:",
               paste0("  - ", defaults$locus, ": ", defaults$statement))
  }
  structure(list(text = lines, plan = NULL, defaults = defaults, server = NULL, digest = NULL),
            class = c("omop_recipe_explain", "list"))
}

.explain_row <- function(locus, code, statement) {
  data.frame(locus = locus, code = code, statement = statement, stringsAsFactors = FALSE)
}

# Defaults the recipe relies on, from the recipe (what the user omitted) and
# the normalised plan (what the default means).
.explain_defaults <- function(recipe, norm) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- .explain_row(...)
  for (nm in names(recipe$outputs %||% list())) {
    o <- recipe$outputs[[nm]]
    fmt <- o$format %||% o$type
    if (identical(fmt, "survival") && is.null(o$options[["tar"]]) &&
        is.null(o$options[["tars"]])) {
      add(paste0("output '", nm, "'"), "tar_default",
          "follow-up ends 730 days after index (default TAR; tar = omop_window(1, NULL) is unbounded)")
    }
    if (identical(o$type, "survival") && is.null(o$format)) {
      add(paste0("output '", nm, "'"), "censoring_default",
          paste0("legacy survival censors at the cohort end and at observation end; ",
                 "death censors only when censoring$death = TRUE"))
    }
    if (identical(o$type, "baseline")) {
      add(paste0("output '", nm, "'"), "age_group_default",
          "baseline reports age as age_group bands on the server's default grid (omop_age(bands = FALSE) in a wide output gives exact age)")
    }
  }
  if (!is.null(norm)) {
    for (id in names(norm$populations)) {
      p <- norm$populations[[id]]
      user <- recipe$populations[[id]]
      if (identical(p$kind, "index") && is.null(user$coverage) && is.null(recipe$coverage) &&
          is.null(user[["circe_v2"]])) {
        add(paste0("population '", id, "'"), "coverage",
            paste0("observation coverage ", p$coverage$policy, "/", p$coverage$on_ambiguous,
                   "/", p$coverage$on_missing, " (default): the index must lie in exactly one ",
                   "observation period; uncovered and ambiguous indexes are dropped"))
      }
    }
    for (nm in names(norm$outputs)) {
      o <- norm$outputs[[nm]]
      if (identical(o$grain, "person") && !is.null(o$collapse)) {
        add(paste0("output '", nm, "'"), "collapse",
            paste0("one row per person from an episode population, collapse = \"", o$collapse, "\""))
      }
      if (identical(o$format, "person_period")) {
        add(paste0("output '", nm, "'"), "representation",
            paste0("person_period representation = \"", o$options$representation, "\"",
                   if (identical(o$options$representation, "dense"))
                     " (every period of every episode is emitted)" else ""))
      }
    }
    for (vn in names(norm$variables)) {
      r <- norm$variables[[vn]]$reducer
      if (!is.null(r) && r$fn %in% c("first_value", "last_value", "nth_value", "closest_value") &&
          identical(r$missing, "skip")) {
        add(paste0("variable '", vn, "'"), "missing_skip",
            "rows with a NULL value are skipped before the row is selected (missing = \"skip\")")
      }
    }
  }
  if (length(rows) == 0L) return(.explain_row(character(0), character(0), character(0)))
  do.call(rbind, rows)
}

.explain_window <- function(w) {
  if (is.null(w)) return("any time")
  ep <- function(x, inf) {
    if (is.null(x)) return(inf)
    if (is.list(x)) {
      a <- if (is.character(x$anchor)) x$anchor else "<anchor>"
      return(paste0(a, if (x$offset >= 0) "+", x$offset))
    }
    as.character(x)
  }
  a <- if (is.character(w$anchor)) w$anchor else if (!is.null(w$anchor$date)) w$anchor$date else
    paste0(w$anchor$which, " ", w$anchor$source$event %||% w$anchor$source$interval, " ",
           w$anchor$field)
  paste0("days [", ep(w$start, "-Inf"), ", ", ep(w$end, "+Inf"), "] relative to ", a)
}

.explain_source <- function(s) {
  if (!is.null(s$event)) return(paste0("event '", s$event, "'"))
  if (!is.null(s$interval)) return(paste0("era '", s$interval, "'"))
  if (!is.null(s$cohort_definition_id)) return(paste0("results cohort ", s$cohort_definition_id))
  if (!is.null(s$any_of)) {
    return(paste0("any of (", paste(vapply(s$any_of, .explain_source, ""), collapse = ", "), ")"))
  }
  "?"
}

.explain_criteria <- function(node) {
  if (is.null(node)) return("none")
  if (!is.null(node$and)) return(paste0("(", paste(vapply(node$and, .explain_criteria, ""), collapse = " AND "), ")"))
  if (!is.null(node$or)) return(paste0("(", paste(vapply(node$or, .explain_criteria, ""), collapse = " OR "), ")"))
  if (!is.null(node$not)) return(paste0("NOT ", .explain_criteria(node$not)))
  if (!is.null(node$has)) {
    h <- node$has
    what <- if (identical(h$op, "==") && identical(as.integer(unlist(h$n)), 0L)) {
      paste0("no ", .explain_source(h$source),
             if (!is.null(h$nonmissing)) paste0(" with non-null ", h$nonmissing))
    } else {
      paste0(h$distinct, " count of ", .explain_source(h$source), " ", h$op, " ",
             paste(unlist(h$n), collapse = ".."))
    }
    return(paste0(what, " in ", .explain_window(h$window)))
  }
  if (!is.null(node$observed)) {
    o <- node$observed
    return(paste0("observed", if (!is.null(o$before)) paste0(" >= ", o$before, " days before"),
                  if (!is.null(o$after)) paste0(" >= ", o$after, " days after"), " the index",
                  if (!is.null(o$gap)) paste0(" (gaps up to ", o$gap, " days tolerated)")))
  }
  if (!is.null(node$age)) {
    a <- node$age
    ref <- if (is.list(a$reference)) paste0("in ", a$reference$year) else "at the index"
    return(paste0("annual age ", ref, " in [", a$min %||% "-", ", ", a$max %||% "-", "]"))
  }
  if (!is.null(node$person)) return(paste0("person: ", .v2_predicate_text(node$person)))
  if (!is.null(node$cohort)) return(paste0("in results cohort ", node$cohort$cohort_definition_id))
  "?"
}

.explain_column_class <- function(col) {
  if (.is_blocked_column(col)) return("class 1 (never released)")
  if (.is_quasi_identifier_column(col)) return("class 2 quasi-identifier (released only through the final minimum-persons rule)")
  "class 3"
}

.explain_variable <- function(name, v) {
  if (!is.null(v$reducer)) {
    r <- v$reducer
    return(paste0(name, ": ", r$fn, " of ", .explain_source(v$source),
                  if (!is.null(r$value)) paste0(" (value '", r$value, "')"),
                  " over ", .explain_window(r$window),
                  if (r$fn %in% c("first_value", "last_value", "nth_value", "closest_value"))
                    paste0("; missing = \"", r$missing, "\""),
                  if (identical(r$fn, "covered_days"))
                    paste0("; coverage = \"", r$coverage, "\", observed_only = ", r$observed_only),
                  if (identical(r$fn, "supply_days")) paste0("; fills = \"", r$fills, "\"")))
  }
  if (!is.null(v$derived)) {
    d <- v$derived
    desc <- switch(d$kind,
      age = if (isTRUE(d$bands)) "age_group bands (legacy grid or age_breaks)" else
        "exact annual age (class 2 quasi-identifier)",
      sex_mf = "sex (M/F)",
      observed_days = paste0("observed days in ", .explain_window(d$window)),
      d$kind)
    return(paste0(name, ": ", desc))
  }
  if (!is.null(v$raw)) {
    return(paste0(name, ": ", v$raw$table, ".", v$raw$column, " - ",
                  .explain_column_class(v$raw$column)))
  }
  if (!is.null(v$population_column)) return(paste0(name, ": population column ", v$population_column$name))
  name
}

.explain_text <- function(norm, defaults, detail) {
  lines <- c(paste0("Recipe plan (language ", norm$meta$language_version, ", semantic ",
                    norm$meta$semantic_version, ")"))
  for (id in names(norm$populations)) {
    p <- norm$populations[[id]]
    head <- paste0("Population '", id, "' (", p$kind, ")")
    if (identical(p$kind, "index")) {
      ix <- p$index
      lines <- c(lines, paste0(head, ": index = ", .explain_source(ix$source), ", select = \"",
                               ix$select, "\"", if (identical(ix$select, "nth")) paste0(" (n = ", ix$n, ")"),
                               ", end = ", ix$end$strategy,
                               if (identical(ix$end$strategy, "fixed")) paste0(" (", ix$end$days, " days)"),
                               if (!is.null(p$parent)) paste0(", within '", p$parent, "'")))
      if (identical(detail, "semantics")) {
        lines <- c(lines, paste0("  coverage: ", p$coverage$policy, "/", p$coverage$on_ambiguous, "/",
                                 p$coverage$on_missing, ", gap ", p$coverage$gap),
                   paste0("  criteria: ", .explain_criteria(p$where)),
                   if (identical(ix$select, "first_eligible"))
                     "  the earliest candidate whose complete criteria hold is kept" else
                     if (ix$select %in% c("first", "last", "nth"))
                       "  candidates are selected per person before the criteria are applied")
      }
    } else if (identical(p$kind, "criteria")) {
      lines <- c(lines, paste0(head, if (!is.null(p$parent)) paste0(" within '", p$parent, "'"),
                               ": ", .explain_criteria(p$where)))
    } else if (identical(p$kind, "setop")) {
      lines <- c(lines, paste0(head, ": ", p$setop$op, " of ",
                               paste(unlist(p$setop$members), collapse = ", ")))
    } else if (identical(p$kind, "match")) {
      m <- p$match
      lines <- c(lines, paste0(head, ": ", m$controls_per_case, " controls per case of '", m$cases,
                               "' from '", m$controls, "', seed ", m$seed, ", ", m$replacement,
                               ", prior outcome ", m$prior_outcome))
    }
  }
  if (length(norm$variables)) {
    lines <- c(lines, "Variables:")
    lines <- c(lines, paste0("  ", vapply(names(norm$variables), function(n)
      .explain_variable(n, norm$variables[[n]]), "")))
  }
  for (nm in names(norm$outputs)) {
    o <- norm$outputs[[nm]]
    line <- paste0("Output '", nm, "' [", o$format, "] rows '", o$rows, "', grain ", o$grain,
                   if (!is.null(o$collapse)) paste0(" (collapse ", o$collapse, ")"),
                   if (!nm %in% unlist(norm$publish)) " - private input, never published")
    lines <- c(lines, line)
    if (identical(o$format, "survival") && identical(detail, "semantics")) {
      opts <- o$options
      tars <- if (length(opts$tars)) opts$tars else list(tar = opts$tar)
      for (t in names(tars)) lines <- c(lines, paste0("  TAR '", t, "': ", .explain_window(tars[[t]])))
      lines <- c(lines, paste0("  censoring (only these cap follow-up): ",
        paste(vapply(opts$censor, function(cz) if (is.character(cz)) cz else
          if (!is.null(cz$admin_date)) paste0("admin date ", cz$admin_date) else
          if (!is.null(cz$censor)) paste0(cz$censor$which, " of ", .explain_source(cz$censor$source),
                                          if (length(cz$censor$when_index_source))
                                            paste0(" when index source is ",
                                                   paste(unlist(cz$censor$when_index_source), collapse = "/")))
          else .explain_source(cz), ""), collapse = "; ")),
        paste0("  format ", opts$format, ", event order ", opts$event_order, ", ties ", opts$tie_policy))
    }
  }
  lines <- c(lines, paste0("Published: ", paste(unlist(norm$publish), collapse = ", ")))
  if (nrow(defaults)) {
    lines <- c(lines, "Defaults relied on:", paste0("  - ", defaults$locus, ": ", defaults$statement))
  }
  lines
}

# --- default-visibility and structural lint rules (2.10.3) ---------------------------

#' Language-2 lint rules: defaults made visible, column classes, legacy pairs
#' @keywords internal
.lint_v2_rows <- function(recipe) {
  rows <- list()
  add <- function(severity, code, message, locus) {
    rows[[length(rows) + 1L]] <<- .lint_row(severity, code, message, locus)
  }
  defaults <- tryCatch({
    plan <- if (length(.recipe_needs_v2(recipe))) recipe_to_plan(recipe, version = 2L) else
      tryCatch(recipe_to_plan(recipe, version = 2L), error = function(e) NULL)
    .explain_defaults(recipe, if (is.null(plan)) NULL else .planNormalizeV2(plan))
  }, error = function(e) .explain_defaults(recipe, NULL))
  for (i in seq_len(nrow(defaults))) {
    loc <- sub("^(output|population|variable) '(.*)'$", "\\1s$\\2", defaults$locus[[i]])
    add("INFO", defaults$code[[i]], defaults$statement[[i]], loc)
  }
  for (vn in names(recipe$variables %||% list())) {
    v <- recipe$variables[[vn]]
    cols <- character(0)
    if (.v2_is_v2_variable(v)) {
      if (identical(v$kind, "derived") && identical(v$derived$kind, "age") && !isTRUE(v$derived$bands)) {
        cols <- "age"
      }
      if (identical(v$kind, "derived") && identical(v$derived$kind, "birth_year_band")) {
        cols <- "birth_year_band"
      }
    } else {
      cols <- c(v$column, v$value_source)
      if (identical(v$format, "age")) cols <- c(cols, "age")
    }
    for (col in cols[vapply(cols, .is_quasi_identifier_column, logical(1))]) {
      add("INFO", "quasi_identifier",
          paste0("Variable '", vn, "' projects '", col, "', a class-2 quasi-identifier: it is ",
                 "released only through the final minimum-persons rule."),
          paste0("variables$", vn))
    }
  }
  legacy_filters <- c(.flatten_filters(recipe$filters %||% list()),
                      unlist(lapply(recipe$populations %||% list(), function(p)
                        .flatten_filters(p$filters %||% list())), recursive = FALSE))
  types <- vapply(legacy_filters, function(f) f$type %||% "", "")
  if (any(types %in% c("has_measurement", "missing_measurement"))) {
    add("INFO", "measurement_pair",
        paste0("has_measurement and missing_measurement are not complements: ",
               "missing_measurement means no row with a non-null value. In language 2, ",
               "omop_has_not(event) means no row and omop_has_not(event, nonmissing = ",
               "\"value_as_number\") means no non-null value."), "filters")
  }
  for (id in names(recipe$populations %||% list())) {
    p <- recipe$populations[[id]]
    if (!is.null(p$parent_id)) {
      add("INFO", "parent_inheritance",
          paste0("Population '", id, "' is a subset of '", p$parent_id, "' from 2.10.3 ",
                 "(before 2.10.3 parent_id was recorded but not applied); servers without ",
                 "population.parent refuse the recipe."), paste0("populations$", id))
    }
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}
