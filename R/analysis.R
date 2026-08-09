# Module: Unified Analysis Catalog (Client)
# Client-side wrappers for the server's single analysis catalog, which folds the
# curated QueryLibrary SQL templates, the pre-computed Achilles analyses, and the
# generic OHDSI result tables behind ONE registry, ONE fail-closed run path, and
# ONE per-patient disclosure gate (see dsOMOP/R/analysis_catalog.R). Entries are
# named with a stable, pack-prefixed id (\code{"dsomop:<id>"}).
#
# These wrappers mirror the achilles/ohdsi wrapper style: list/get return the
# (server-identical) catalog metadata; run executes one entry across every
# connected server and returns a \code{dsomop_result}. Population scoping is
# expressed by a cohort reference and/or one or more workspace \code{omop.table}
# symbols, folded with \code{combine} server-side into one re-gated cohort.

utils::globalVariables(".data")

.analysis_scope_literal_is_valid <- function(value) {
  (is.character(value) && length(value) == 1L && !is.na(value) &&
     nzchar(value)) ||
    (is.numeric(value) && length(value) == 1L && !is.na(value) &&
       is.finite(value) && value == floor(value) &&
       value > 0 && value <= .Machine$integer.max)
}

.analysis_cohort_literals <- function(cohort) {
  if (is.null(cohort)) return(NULL)
  if (is.call(cohort) || is.name(cohort) || is.environment(cohort)) {
    stop("cohort must resolve to one or more literal cohort ids or table names.",
         call. = FALSE)
  }

  # Preserve mixed target/comparator types by normalising each list element on
  # its own. An atomic character vector mixing numeric-looking ids and table
  # names is ambiguous after R's coercion (e.g. c(handle, 2L)); require a list.
  items <- if (is.list(cohort) && !is.data.frame(cohort)) {
    unname(cohort)
  } else {
    as.list(cohort)
  }
  if (length(items) == 0L) {
    stop("cohort must resolve to one or more literal cohort ids or table names.",
         call. = FALSE)
  }

  if (is.character(cohort) && length(cohort) > 1L) {
    numeric_text <- grepl("^[+-]?[0-9]+$", cohort)
    if (any(numeric_text) && !all(numeric_text)) {
      stop("A mixed cohort id/table vector is ambiguous after R coercion; ",
           "use list(table_handle, cohort_id) to preserve each type.",
           call. = FALSE)
    }
  }

  lapply(items, function(item) {
    if (is.list(item) || is.call(item) || is.name(item) || length(item) != 1L) {
      stop("Each cohort reference must be one scalar literal id or table name.",
           call. = FALSE)
    }
    if (is.numeric(item) &&
        (is.na(item) || !is.finite(item) || item != floor(item) ||
         item <= 0 || item > .Machine$integer.max)) {
      stop("cohort ids must be positive finite integer-like values.",
           call. = FALSE)
    }
    if (is.character(item) && grepl("^[+-]?[0-9]+$", item)) {
      numeric_id <- suppressWarnings(as.numeric(item))
      if (!is.finite(numeric_id) || numeric_id != floor(numeric_id) ||
          numeric_id <= 0 || numeric_id > .Machine$integer.max) {
        stop("cohort ids must be positive finite integer-like values.",
             call. = FALSE)
      }
    }
    value <- .cohort_scope_arg(item)
    if (is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value)) {
      return(unname(value))
    }
    if (is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value) && value == floor(value) &&
        value > 0 && value <= .Machine$integer.max) {
      return(as.integer(value))
    }
    stop("cohort ids must be positive finite integer-like values and table ",
         "names must be non-empty strings.", call. = FALSE)
  })
}

#' Build named server-side scope arguments for an analysis run
#'
#' A single cohort reference travels as the literal named argument \code{scope};
#' multiple references travel as separate scalar \code{scope_cohort_1},
#' \code{scope_cohort_2}, ... arguments.
#' Workspace \code{omop.table} symbols travel separately as bare-symbol named
#' arguments \code{scope_table_1}, \code{scope_table_2}, and so on. No nested
#' \code{list()} or \code{c()} expression is emitted: allowing those generic
#' AggregateMethods would let a caller evaluate an unreviewed container around
#' protected objects.
#'
#' Forms produced:
#' \itemize{
#'   \item no cohort, no tables -> \code{NULL} (no scoping argument).
#'   \item one cohort -> \code{list(scope = <literal>)}; multiple cohorts ->
#'     sequential scalar \code{scope_cohort_<n>} arguments.
#'   \item table symbol(s) -> a named list whose values are bare symbols and
#'     whose names are the sequential \code{scope_table_<n>} arguments.
#' }
#'
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side table name) or \code{NULL}.
#' @param tables Character vector of server-side \code{omop.table} symbol names,
#'   or \code{NULL}.
#' @return \code{NULL} or a named local list of call arguments. Its values are
#'   cohort literals and/or bare table symbols; it never contains a call.
#' @keywords internal
.analysis_scope_expr <- function(cohort = NULL, tables = NULL) {
  cohort_val <- .analysis_cohort_literals(cohort)

  if (!is.null(tables)) {
    if (!is.character(tables) || anyNA(tables) || any(!nzchar(tables)) ||
        any(!grepl("^[A-Za-z.][A-Za-z0-9._]*$", tables)) ||
        any(grepl("^\\.[0-9]", tables)) || anyDuplicated(tables)) {
      stop("tables must be the name(s) of server-side omop.table symbol(s).",
           call. = FALSE)
    }
  }

  if (is.null(cohort_val) && (is.null(tables) || length(tables) == 0L)) {
    return(NULL)
  }

  args <- if (length(cohort_val) == 1L) {
    list(scope = cohort_val[[1L]])
  } else if (length(cohort_val) > 1L) {
    cohort_args <- cohort_val
    names(cohort_args) <- paste0("scope_cohort_", seq_along(cohort_args))
    cohort_args
  } else list()
  if (!is.null(tables) && length(tables) > 0L) {
    table_args <- lapply(tables, as.name)
    names(table_args) <- paste0("scope_table_", seq_along(table_args))
    args <- c(args, table_args)
  }
  args
}

#' Build the (possibly scope-bearing) server-side analysis run call
#'
#' Constructs the unevaluated DataSHIELD call for \code{omopAnalysisRunDS} /
#' \code{omopAnalysisRunAssignDS}. \code{params} is JSON/base64-encoded for Opal
#' transport (\code{\link{.ds_encode}}); \code{scope_args} is spliced as closed,
#' named arguments. Table scopes are bare symbols, never nested calls.
#' \code{combine} is passed by name so absent scope arguments cannot shift it
#' into the wrong positional slot.
#'
#' @param fn Character; the server method name.
#' @param res_symbol Character; the server-side handle symbol.
#' @param name Character; the catalog entry name.
#' @param params Named list of parameter values.
#' @param scope_args \code{NULL} or the closed named argument list returned by
#'   \code{\link{.analysis_scope_expr}}.
#' @param combine Character; \code{"union"} or \code{"intersect"}.
#' @return An unevaluated \code{call}.
#' @keywords internal
.analysis_run_call <- function(fn, res_symbol, name, params, scope_args,
                               combine, date_handling = NULL) {
  args <- list(as.name(fn), res_symbol, name, .ds_encode(params))
  if (!is.null(scope_args)) {
    if (!is.list(scope_args) || is.null(names(scope_args)) ||
        anyNA(names(scope_args)) || any(!nzchar(names(scope_args))) ||
        anyDuplicated(names(scope_args))) {
      stop("scope_args must be a closed named argument list.", call. = FALSE)
    }
    has_cohort <- "scope" %in% names(scope_args)
    cohort_names <- grep("^scope_cohort_[0-9]+$", names(scope_args),
                         value = TRUE)
    table_names <- grep("^scope_table_[0-9]+$", names(scope_args), value = TRUE)
    expected_names <- c(
      if (has_cohort) "scope" else character(0),
      if (length(cohort_names) > 0L) {
        paste0("scope_cohort_", seq_along(cohort_names))
      } else character(0),
      if (length(table_names) > 0L) {
        paste0("scope_table_", seq_along(table_names))
      } else character(0)
    )
    if (!identical(names(scope_args), expected_names) ||
        (has_cohort && length(cohort_names) > 0L) ||
        (has_cohort && !.analysis_scope_literal_is_valid(scope_args$scope)) ||
        any(!vapply(scope_args[cohort_names],
                    .analysis_scope_literal_is_valid, logical(1L))) ||
        any(!vapply(scope_args[table_names], is.name, logical(1L)))) {
      stop("scope_args may contain only scalar literal scope/scope_cohort_<n> ",
           "arguments and sequential bare scope_table_<n> symbols.",
           call. = FALSE)
    }
    args <- c(args, scope_args)
  }
  args <- c(args, list(combine = combine))
  if (!is.null(date_handling)) {
    args <- c(args, list(date_handling = date_handling))
  }
  as.call(args)
}

.analysis_consistent_metadata <- function(raw, expected_servers, context) {
  errors <- attr(raw, "ds_errors") %||% list()
  missing <- setdiff(expected_servers, names(raw))
  if (length(errors) > 0L || length(missing) > 0L) {
    stop("Cannot verify ", context, " on every server; unavailable: ",
         paste(unique(c(names(errors), missing)), collapse = ", "), ".",
         call. = FALSE)
  }
  reference <- raw[[expected_servers[[1L]]]]
  different <- expected_servers[!vapply(raw[expected_servers], identical,
                                        logical(1), reference)]
  if (length(different) > 0L) {
    stop("Federated ", context, " differs across servers: ",
         paste(different, collapse = ", "),
         ". Align dsOMOP and analysis-pack versions before execution.",
         call. = FALSE)
  }
  reference
}

.analysis_complete_results <- function(raw, expected_servers, context) {
  errors <- attr(raw, "ds_errors") %||% list()
  missing <- setdiff(expected_servers, names(raw))
  failed <- unique(c(names(errors), missing))
  if (length(failed) > 0L) {
    stop("Federated ", context, " failed or returned no verifiable result on: ",
         paste(failed, collapse = ", "),
         ". Partial-site analysis results are not published or pooled.",
         call. = FALSE)
  }
  raw[expected_servers]
}

#' Render an entry's client-side plot over already-gated pooled data
#'
#' The plotting half of the analysis catalog runs ENTIRELY on the client, over
#' data that has ALREADY passed the server's single per-patient disclosure gate.
#' For entries that ship one, the server returns declarative plot metadata. The
#' client accepts only an allowlisted \code{plot$type} plus optional column-name
#' mappings and dispatches to local renderers. Remote source text is never
#' parsed or evaluated.
#'
#' Safety model:
#' \itemize{
#'   \item Server metadata cannot execute code in the analyst's R process.
#'   \item Only a local, allowlisted renderer sees \code{df} — the pooled data
#'     frame that already cleared the gate (small-cell suppressed, banded and
#'     distribution-protected). Remote source text is ignored.
#'   \item Rendering is wrapped in \code{\link[base]{tryCatch}} so incompatible
#'     declarative metadata never costs the analyst the already-returned data;
#'     it degrades to a warning and \code{NULL} plot.
#' }
#' \pkg{ggplot2} is required only on this path (\code{plot = TRUE}); a clear
#' message is raised if it is not installed, rather than failing obscurely inside
#' the recipe.
#'
#' @param meta Named list; one entry's metadata from \code{omopAnalysisGetDS}.
#'   The plot recipe is read from \code{meta$plot} (a \code{list(type, mapping)}),
#'   tolerating a nested \code{meta$compute$plot} for forward compatibility.
#' @param pooled Data frame; the pooled, gate-passed aggregate to plot.
#' @param params Named list; retained for API compatibility with future local
#'   renderers. It is never evaluated as code.
#' @return A \code{ggplot} object, or \code{NULL} when the entry ships no plot
#'   recipe or the recipe could not be built (with a warning in the latter case).
#' @keywords internal
.analysis_render_plot <- function(meta, pooled, params) {
  # The recipe lives at meta$plot (flat, as omopAnalysisGetDS exposes it); accept
  # a nested compute$plot too so the client is robust to either metadata shape.
  recipe <- meta$plot %||% meta$compute$plot
  type <- tolower(recipe$type %||% "")
  if (!nzchar(type)) {
    warning("Analysis '", meta$name %||% "?",
            "' does not provide a plot; returning data only.", call. = FALSE)
    return(NULL)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("plot = TRUE requires the 'ggplot2' package; install it or call with ",
         "plot = FALSE to get the data only.", call. = FALSE)
  }
  if (!is.data.frame(pooled) || nrow(pooled) == 0) {
    warning("Analysis '", meta$name %||% "?",
            "' returned no pooled data to plot; returning data only.",
            call. = FALSE)
    return(NULL)
  }

  allowed_types <- c("bar", "line", "step", "point", "scatter",
                     "histogram", "box", "forest")
  if (!type %in% allowed_types) {
    warning("Analysis '", meta$name %||% "?", "' requested unsupported plot type '",
            type, "'; returning data only.", call. = FALSE)
    return(NULL)
  }

  # Optional declarative mappings are untrusted strings: accept only known
  # roles pointing at columns that actually exist in the gated frame.
  mapping <- recipe$mapping %||% list()
  if (!is.list(mapping) ||
      length(setdiff(names(mapping), c("x", "y", "fill", "colour",
                                       "ymin", "ymax"))) > 0L ||
      any(!vapply(mapping, function(x) is.character(x) && length(x) == 1L &&
                                    !is.na(x) && x %in% names(pooled),
                  logical(1)))) {
    warning("Analysis '", meta$name %||% "?",
            "' supplied an invalid declarative plot mapping; returning data only.",
            call. = FALSE)
    return(NULL)
  }

  numeric_cols <- names(pooled)[vapply(pooled, is.numeric, logical(1))]
  label_cols <- setdiff(names(pooled), numeric_cols)
  prefer <- function(candidates, pattern) {
    hit <- grep(pattern, candidates, value = TRUE, ignore.case = TRUE)
    if (length(hit)) hit[[1]] else if (length(candidates)) candidates[[1]] else NULL
  }
  x_col <- mapping$x %||% prefer(label_cols,
    "step|stratum|concept|covariate|metric|category|arm|month|window|bin|day|time")
  y_col <- mapping$y %||% prefer(numeric_cols,
    "^(n|count|persons|subjects|person_count|n_persons|concept_count|outcome_events|outcomes|rate|average|median_value|estimate|irr|statistic|pct_treated|survival_probability)$")
  if (is.null(x_col)) x_col <- setdiff(names(pooled), y_col)[1]

  # A failure in local rendering must never lose the already-returned data.
  tryCatch({
    if (type == "box" && all(c("p10_value", "p25_value", "median_value",
                                "p75_value", "p90_value") %in% names(pooled))) {
      x_col <- x_col %||% names(pooled)[[1]]
      return(ggplot2::ggplot(pooled, ggplot2::aes(x = .data[[x_col]])) +
        ggplot2::geom_boxplot(ggplot2::aes(
          lower = .data[["p25_value"]], upper = .data[["p75_value"]],
          middle = .data[["median_value"]], ymin = .data[["p10_value"]],
          ymax = .data[["p90_value"]]), stat = "identity"))
    }
    if (type == "forest") {
      y_col <- mapping$y %||% prefer(numeric_cols, "estimate|irr|effect")
      ymin <- mapping$ymin %||% prefer(numeric_cols, "ci_lo|lower")
      ymax <- mapping$ymax %||% prefer(numeric_cols, "ci_hi|upper")
      if (any(vapply(list(x_col, y_col, ymin, ymax), is.null, logical(1)))) {
        stop("forest plots require label, estimate, lower and upper columns.",
             call. = FALSE)
      }
      return(ggplot2::ggplot(pooled, ggplot2::aes(
        x = .data[[x_col]], y = .data[[y_col]])) +
        ggplot2::geom_point() + ggplot2::geom_errorbar(
          ggplot2::aes(ymin = .data[[ymin]], ymax = .data[[ymax]]), width = .1))
    }
    if (type == "histogram" && is.null(y_col)) {
      x_col <- mapping$x %||% numeric_cols[[1]]
      return(ggplot2::ggplot(pooled, ggplot2::aes(x = .data[[x_col]])) +
        ggplot2::geom_histogram(bins = 30L))
    }
    if (is.null(x_col) || is.null(y_col)) {
      stop("plot type requires usable x and y columns.", call. = FALSE)
    }
    base <- ggplot2::ggplot(pooled, ggplot2::aes(
      x = .data[[x_col]], y = .data[[y_col]],
      fill = if (!is.null(mapping$fill)) .data[[mapping$fill]] else NULL,
      colour = if (!is.null(mapping$colour)) .data[[mapping$colour]] else NULL))
    switch(type,
      bar =, histogram = base + ggplot2::geom_col(),
      line = base + ggplot2::geom_line(),
      step = base + ggplot2::geom_step(),
      point =, scatter = base + ggplot2::geom_point(),
      box = base + ggplot2::geom_boxplot()
    )
  }, error = function(e) {
    warning("Analysis '", meta$name %||% "?", "' plot could not be built (",
            conditionMessage(e), "); returning data only.", call. = FALSE)
    NULL
  })
}

#' List unified analysis catalog entries
#'
#' Returns metadata for every entry in the server's unified analysis catalog —
#' the single registry that folds the curated QueryLibrary SQL templates, the
#' pre-computed Achilles analyses, and the generic OHDSI result tables behind one
#' stable, pack-prefixed naming scheme (\code{"dsomop:<id>"}). Because the
#' catalog is defined by the server package, the client requires an identical
#' response from every participating server before exposing a pooled view. No
#' SQL, compute functions, or other server internals are exposed.
#'
#' @param domain Character; optional clinical-domain filter (e.g.
#'   \code{"condition"}, \code{"person"}). \code{NULL} (the default) returns
#'   entries for all domains.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}; dsBaseClient-style aliases are accepted. A split view
#'   can inspect nodes whose catalogs differ; combined views require equality.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}. The pooled
#'   element is a data frame with one row per entry: \code{name} (the id),
#'   \code{domain}, \code{adapter}, \code{mode}, disclosure \code{unit},
#'   \code{description} (title), parameter summary, the
#'   \code{accepts_cohort}/\code{accepts_tables} scoping flags, whether the entry
#'   \code{requires_cohort} (un-scoped runs error), and whether it ships a plot
#'   (\code{has_plot}).
#' @examples
#' \dontrun{
#' catalog <- ds.omop.analysis.list()
#' head(catalog$pooled)
#'
#' # Only condition-domain analyses
#' cond <- ds.omop.analysis.list(domain = "condition")
#' cond$pooled$name
#' }
#' @seealso \code{\link{ds.omop.analysis.get}}, \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.analysis.list <- function(domain = NULL, symbol = "omop",
                                  conns = NULL, type = "both") {
  type <- .normalize_result_type(type)
  code <- .build_code("ds.omop.analysis.list", domain = domain, symbol = symbol,
                      type = type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisListDS", session$res_symbol, domain)
  )

  pooled <- if (.result_type_wants_combine(type)) {
    .analysis_consistent_metadata(raw, names(conns), "analysis catalog")
  } else NULL

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = if (.result_type_wants_combine(type)) {
      "pooled"
    } else "per_site", type = type)), type)
}

#' Get unified analysis catalog entry metadata
#'
#' Returns full metadata for a single catalog entry: its parameter specs,
#' compute kind, disclosure spec, and scoping capabilities. Use it to discover
#' an entry's parameters and to check whether it accepts cohort/table scoping
#' before running it. Execution requires identical metadata from every server;
#' a mixed-version or partially unavailable federation fails closed.
#'
#' @param name Character; the entry id (e.g. \code{"dsomop:achilles.401"}) or a
#'   shorthand for it (native id without the \code{"dsomop:"} prefix, or a unique
#'   id suffix; an ambiguous shorthand errors with the candidates).
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}; dsBaseClient-style aliases are accepted. A split view
#'   can inspect nodes whose metadata differ; combined views require equality.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}. The pooled
#'   element is a named list with the entry's \code{name}, \code{description},
#'   \code{domain}, \code{mode}, \code{params}, \code{compute_kind},
#'   \code{disclosure}, \code{scope}, \code{adapter}, and the inert client-side
#'   \code{plot} recipe (\code{NULL} when the entry ships none). External packs
#'   also expose their pinned package/version and closed output contract, so
#'   federated execution can require exact metadata equality across nodes.
#' @examples
#' \dontrun{
#' meta <- ds.omop.analysis.get("dsomop:achilles.401")
#' meta$pooled$params
#' meta$pooled$mode
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}, \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.analysis.get <- function(name, symbol = "omop", conns = NULL,
                                 type = "both") {
  type <- .normalize_result_type(type)
  code <- .build_code("ds.omop.analysis.get", name = name, symbol = symbol,
                      type = type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisGetDS", session$res_symbol, name)
  )

  pooled <- if (.result_type_wants_combine(type)) {
    .analysis_consistent_metadata(
      raw, names(conns), paste0("analysis metadata for '", name, "'")
    )
  } else NULL

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = if (.result_type_wants_combine(type)) {
      "pooled"
    } else "per_site", type = type)), type)
}

#' Run a unified analysis catalog entry
#'
#' Executes one catalog entry across every connected server through the server's
#' single fail-closed run path, which validates and sanitizes parameters, applies
#' optional population scoping, runs the entry's compute step (SQL template or
#' wrapped Achilles/OHDSI accessor), and funnels the result through the ONE
#' per-patient disclosure gate. Aggregate entries return disclosure-controlled
#' data frames that are additionally pooled across servers (count columns are
#' summed with suppression propagation). Assign-mode QueryLibrary loaders
#' (detected from the entry metadata) instead store their result server-side and
#' return per-server assignment confirmations.
#'
#' Scoping: pass a \code{cohort} reference and/or one or more workspace
#' \code{omop.table} symbol names in \code{tables}. Multiple sources are folded
#' server-side with \code{combine} (\code{"union"}/\code{"intersect"} on the
#' person key) into a single re-gated cohort, and SQL entries are restricted to
#' it. Pre-computed Achilles/OHDSI entries hold no per-row person key and reject
#' scoping (the server raises a clear error).
#'
#' \code{name} accepts the full pack-prefixed id (\code{"dsomop:fe.prevalence"})
#' and the natural shorthands: the native id without the prefix
#' (\code{"fe.prevalence"}) or a unique id suffix (\code{"prevalence"}). An
#' ambiguous shorthand errors with the candidate ids.
#'
#' @section Which tool when:
#' Three layers, simplest first — reach for the lowest one that does the job:
#' \itemize{
#'   \item \strong{One-liners} (\code{\link{ds.omop.prevalence}},
#'     \code{\link{ds.omop.distribution}}): the fastest path for the two most
#'     common summaries over a cohort. One call plus good defaults; thin wrappers
#'     over this function.
#'   \item \strong{Analysis catalog} (\code{\link{ds.omop.analysis.list}} /
#'     \code{\link{ds.omop.analysis.get}} / \code{ds.omop.analysis.run}): the full
#'     menu of curated, pre-gated analyses (QueryLibrary, Achilles, OHDSI, native
#'     diagnostics). Use it to discover and run any named analysis with explicit
#'     params.
#'   \item \strong{Recipes} (\code{\link[=omop_recipe]{omop_recipe}} +
#'     \code{\link[=recipe_execute]{recipe_execute}}): author a bespoke EXTRACTION
#'     — choose populations, variables, filters, and output shape — when no single
#'     catalog analysis fits. The complete, declarative query surface.
#' }
#'
#' @param name Character; the entry id, or a shorthand for it (native id without
#'   the \code{"dsomop:"} prefix, or a unique id suffix).
#' @param params Named list; parameter values for the entry (see
#'   \code{\link{ds.omop.analysis.get}} for the entry's parameter specs).
#' @param cohort Optional cohort reference to scope the population to: a
#'   \code{dsomop_cohort_handle} (from \code{\link{ds.omop.cohort.create}},
#'   \code{\link{ds.omop.cohort.combine}}, or \code{\link{ds.omop.cohort.from_table}}),
#'   a \code{cohort_definition_id}, or a server-side cohort table name.
#'   \code{NULL} (the default) means no cohort scoping.
#' @param tables Optional character vector of server-side \code{omop.table}
#'   symbol names to scope the population to (their distinct persons). May be
#'   combined with \code{cohort}. Each table crosses DataSHIELD as its own bare
#'   named \code{scope_table_<n>} argument, never through a generic
#'   \code{list()} or \code{c()} AggregateMethod.
#' @param combine Character; how to fold multiple scope sources together:
#'   \code{"union"} (the default) or \code{"intersect"}.
#' @param pooling_policy Character; how suppressed (NA) cells are handled when
#'   pooling aggregate results across servers. \code{"strict"} (default)
#'   requires each pooled group on every server; \code{"pooled_only_ok"} pools
#'   only available disclosure-safe contributions. Kaplan-Meier curves still
#'   require every preceding risk-set bin because their values are cumulative.
#' @param plot Logical; when \code{TRUE} AND the entry ships a plot recipe, build
#'   a \code{ggplot} CLIENT-SIDE over the pooled, gate-passed data and attach it
#'   to the result (also returned via the \code{"plot"} attribute and
#'   \code{meta$plot}). \code{FALSE} (the default) returns data only and never
#'   touches \pkg{ggplot2}. The plot is purely a client-side rendering of numbers
#'   that already cleared the server's disclosure gate (see Safety, below). A
#'   broken/incompatible recipe degrades to a warning and a \code{NULL} plot — it
#'   never costs you the returned data.
#' @param date_handling For assign-mode loaders, the server-side date policy:
#'   \code{"remove"} (default), \code{"relative"}, \code{"binned"}, or
#'   server-authorized \code{"absolute"}. Ignored for aggregate entries.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param type Result view, following dsBaseClient terminology: \code{"both"}
#'   (default) returns per-server and correctly combined results,
#'   \code{"split"} returns only per-server results, and \code{"combine"}
#'   returns only the combined result. The aliases \code{"b"},
#'   \code{"splits"}/\code{"s"}, and \code{"combined"}/\code{"c"} are
#'   accepted. Combining is performed only from the server-owned pooling
#'   contract; no column-name heuristics are used.
#' @return A \code{dsomop_result} object. For aggregate entries, \code{per_site}
#'   holds each server's disclosure-controlled data frame and \code{pooled} holds
#'   the cross-server aggregation. For assign-mode entries, \code{per_site} holds
#'   per-server assignment confirmations (the data stays on the server) and the
#'   server-side symbol name is recorded in the result metadata. When
#'   \code{plot = TRUE} and the entry ships a plot recipe, the built \code{ggplot}
#'   is attached as the \code{"plot"} attribute (and \code{meta$plot}).
#'
#' @section Safety (client-side plotting): The server may advertise only an
#'   allowlisted plot type and declarative column mappings. The client dispatches
#'   to installed local renderers over already-gated pooled data; it never parses
#'   or evaluates source code received from a server.
#' @examples
#' \dontrun{
#' # Discover, inspect, then run an entry scoped to a cohort.
#' catalog <- ds.omop.analysis.list(domain = "condition")
#' entry   <- catalog$pooled$name[1]
#' meta    <- ds.omop.analysis.get(entry)
#' meta$pooled$params
#'
#' diabetes <- ds.omop.cohort.create(spec = ..., cohort_id = 1)
#' res <- ds.omop.analysis.run(
#'   entry,
#'   params = list(top_n = 25),
#'   cohort = diabetes
#' )
#' res$pooled
#'
#' # Scope by one or more workspace omop.table symbols, intersected.
#' res2 <- ds.omop.analysis.run(
#'   entry,
#'   tables  = c("my_cohort", "my_other_cohort"),
#'   combine = "intersect"
#' )
#'
#' # Build the entry's client-side plot over the pooled, gate-passed data.
#' res3 <- ds.omop.analysis.run(entry, params = list(top_n = 25), plot = TRUE)
#' attr(res3, "plot")   # the ggplot (NULL if the entry ships no plot recipe)
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}, \code{\link{ds.omop.analysis.get}}
#' @export
ds.omop.analysis.run <- function(name, params = list(), cohort = NULL,
                                 tables = NULL, combine = "union",
                                 pooling_policy = "strict", plot = FALSE,
                                 date_handling = NULL,
                                 symbol = "omop", conns = NULL,
                                 type = "both") {
  combine <- match.arg(combine, c("union", "intersect"))
  pooling_policy <- match.arg(pooling_policy, c("strict", "pooled_only_ok"))
  type <- .resolve_result_type(type = type, default_type = "both")
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("plot must be TRUE or FALSE.", call. = FALSE)
  }

  code <- .build_code("ds.omop.analysis.run", name = name, symbol = symbol,
                      type = type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  scope_args <- .analysis_scope_expr(cohort, tables)
  contract <- .session_harmonization_for_connections(session, conns)
  if (!is.null(contract)) {
    table_cap <- contract$max_analysis_scope_tables
    if (length(tables %||% character(0)) > table_cap) {
      stop("Analysis scope exceeds negotiated max_analysis_scope_tables cap ",
           "of ", table_cap, ".", call. = FALSE)
    }
    if (length(scope_args %||% list()) > table_cap + 1L) {
      stop("Analysis scope exceeds negotiated total source cap of ",
           table_cap + 1L, ".", call. = FALSE)
    }
  }

  # Mode and output semantics must be identical on every node. A first-server
  # decision could otherwise route the same entry through aggregate on one site
  # and assign on another.
  metadata <- .ds_safe_aggregate(
    conns, expr = call("omopAnalysisGetDS", session$res_symbol, name)
  )
  entry_meta <- .analysis_consistent_metadata(
    metadata, names(conns), paste0("analysis metadata for '", name, "'")
  )
  if (!is.list(entry_meta) || !is.character(entry_meta$mode) ||
      length(entry_meta$mode) != 1L || is.na(entry_meta$mode) ||
      !entry_meta$mode %in% c("aggregate", "assign")) {
    stop("Analysis metadata has no supported aggregate/assign mode.",
         call. = FALSE)
  }
  is_assign <- identical(entry_meta$mode, "assign")

  if (is_assign && identical(type, "combine")) {
    stop("Assign-mode analyses create server-side objects and cannot return a ",
         "combined client result; use type = 'split' or 'both'.",
         call. = FALSE)
  }
  if (!is_assign && .result_type_wants_combine(type)) {
    # Validate before releasing any analysis result. A missing/malformed
    # contract is a deployment error, never permission to fall back to a
    # heuristic pooler.
    .validate_analysis_pooling_contract(entry_meta$pooling_contract)
    if (length(conns) > 1L &&
        .analysis_pooling_contract_uses_counts(entry_meta$pooling_contract) &&
        !is.null(contract) && !isTRUE(contract$poolable_counts)) {
      stop("Federated count pooling blocked: server count-band settings are ",
           "not identical. Align dsomop.nfilter.band across servers.",
           call. = FALSE)
    }
  }

  if (!is.null(date_handling)) {
    if (!is.character(date_handling) || length(date_handling) != 1L ||
        is.na(date_handling) ||
        !tolower(date_handling) %in% c("remove", "relative",
                                       "relative_to_index", "binned",
                                       "absolute")) {
      stop("date_handling must be remove, relative, binned, or absolute.",
           call. = FALSE)
    }
    date_handling <- tolower(date_handling)
  }

  if (is_assign) {
    # Assign-mode loader: reserve a fresh symbol, assign on every node, and
    # commit only after post-execution inventory proves complete federation
    # coverage. Any partial assignment is removed everywhere.
    inventory <- .plan_symbol_inventory(conns, "analysis assignment preflight")
    newobj <- NULL
    for (attempt in seq_len(10L)) {
      candidate <- .generate_symbol("omop.analysis")
      if (all(!vapply(inventory, function(x) candidate %in% x, logical(1)))) {
        newobj <- candidate
        break
      }
    }
    if (is.null(newobj)) {
      stop("Could not reserve a collision-free analysis output symbol.",
           call. = FALSE)
    }
    run_expr <- .analysis_run_call(
      "omopAnalysisRunAssignDS", session$res_symbol, name, params,
      scope_args, combine, date_handling = date_handling)
    succeeded <- character(0)
    failures <- character(0)
    condition <- tryCatch({
      DSI::datashield.assign.expr(
        conns, symbol = newobj, expr = run_expr,
        success = function(server) {
          succeeded <<- c(succeeded, server)
        },
        error = function(server, message) {
          failures[[server]] <<- message
        }
      )
      NULL
    }, error = identity)
    incomplete <- unique(c(names(failures), setdiff(names(conns), succeeded)))
    if (!is.null(condition) || length(incomplete) > 0L) {
      .plan_remove_output_symbols(conns, list(newobj), verify = TRUE)
      detail <- if (!is.null(condition)) conditionMessage(condition) else
        paste(incomplete, collapse = ", ")
      stop("Federated analysis assignment failed and was rolled back: ",
           detail, ".", call. = FALSE)
    }
    committed <- .plan_symbol_inventory(conns, "analysis assignment commit")
    missing <- names(committed)[!vapply(
      committed, function(x) newobj %in% x, logical(1)
    )]
    if (length(missing) > 0L) {
      .plan_remove_output_symbols(conns, list(newobj), verify = TRUE)
      stop("Could not prove analysis assignment commit on: ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    .record_session_outputs(symbol, newobj)
    per_site <- stats::setNames(as.list(rep(TRUE, length(conns))), names(conns))
    result <- dsomop_result(
      per_site = per_site, pooled = NULL,
      meta = list(call_code = code, scope = "per_site",
                  type = "split", assign_symbol = newobj))
    result$meta$assign_symbol <- newobj
    return(result)
  }

  # Aggregate entry: run on each server, then pool the returned frames.
  run_expr <- .analysis_run_call(
    "omopAnalysisRunDS", session$res_symbol, name, params,
    scope_args, combine)
  raw <- .ds_safe_aggregate(conns, expr = run_expr)
  raw <- .analysis_complete_results(
    raw, names(conns), paste0("analysis '", name, "'")
  )

  pool_out <- if (.result_type_wants_combine(type)) {
    .pool_analysis_contract(
      raw, entry_meta$pooling_contract, policy = pooling_policy,
      harmonization = contract
    )
  } else {
    list(result = NULL, warnings = character(0), harmonization = contract)
  }

  # Optional client-side plot over the pooled, gate-passed data. The data is
  # already in hand; .analysis_render_plot degrades to NULL (with a warning) on
  # any failure so plotting can never lose the returned aggregate.
  gg <- if (isTRUE(plot) && .result_type_wants_combine(type)) {
    .analysis_render_plot(entry_meta, pool_out$result, params)
  } else {
    if (isTRUE(plot)) {
      warning("plot = TRUE requires type = 'both' or 'combine'; no plot was ",
              "built for the split view.", call. = FALSE)
    }
    NULL
  }

  result <- dsomop_result(
    per_site = raw, pooled = pool_out$result,
    meta = list(call_code = code,
                scope = if (.result_type_wants_combine(type)) "pooled" else
                  "per_site",
                type = type, servers = names(raw),
                pooling_policy = pooling_policy,
                warnings = pool_out$warnings))
  # The dsomop_result constructor keeps only its known meta fields, so attach the
  # built plot explicitly (as documented: available via both the "plot"
  # attribute and meta$plot). NULL when plot = FALSE or no recipe was built.
  attr(result, "plot") <- gg
  result$meta$plot <- gg
  result$meta$pooling_contract <- entry_meta$pooling_contract
  result$meta$harmonization <- pool_out$harmonization
  .result_type_view(
    result, type,
    combine_reason = if (identical(entry_meta$pooling_contract$strategy,
                                   "not_poolable")) {
      entry_meta$pooling_contract$reason
    } else NULL
  )
}

# --- One-liner convenience wrappers (over ds.omop.analysis.run) --------------
#
# Thin shortcuts for the two analyses an analyst reaches for most — covariate
# prevalence and continuous-value distributions. They add NO new compute and NO
# new gate: each builds the catalog entry's params and delegates to
# ds.omop.analysis.run(), inheriting its scoping, pooling, plotting, and the ONE
# per-patient disclosure gate verbatim. The verbose ds.omop.analysis.run() stays
# the power path; these only spare the simple case the entry-name + params boilerplate.

#' Map a human domain name (or code) to the catalog domain_code
#'
#' The covariate analyses select their event family with a \code{domain_code}
#' ("0" condition, "1" drug, ...). Accept the friendly domain NAME as well so a
#' caller writes \code{domain = "condition"} instead of memorising the code; a
#' code passed through unchanged.
#'
#' @param domain Character/numeric domain name or code, or \code{NULL}.
#' @param default Character; the code to use when \code{domain} is \code{NULL}.
#' @return Character domain code.
#' @keywords internal
.analysis_domain_code <- function(domain = NULL, default = "0") {
  if (is.null(domain)) return(default)
  d <- tolower(trimws(as.character(domain)[[1]]))
  switch(d,
    "0" =, "condition" = "0",
    "1" =, "drug" = "1",
    "2" =, "procedure" = "2",
    "3" =, "measurement" = "3",
    "4" =, "observation" = "4",
    stop("Unknown domain '", domain, "'. Use one of condition, drug, procedure, ",
         "measurement, observation (or the codes 0-4).", call. = FALSE))
}

#' Subset an already-gated result's frames to requested concept id(s)
#'
#' Post-gate, cosmetic row selection: keeps only the rows whose covariate/concept
#' id is in \code{concept_id}, in BOTH the pooled and per-site frames. This is a
#' plain subset of numbers that already cleared the server's disclosure gate — it
#' is NOT a new gate and never recovers a suppressed cell. Frames without an id
#' column are returned untouched (defensive).
#'
#' @param result A \code{dsomop_result}.
#' @param concept_id Integer vector of concept ids to keep, or \code{NULL}.
#' @return The \code{dsomop_result} with its frames row-subset.
#' @keywords internal
.analysis_filter_concepts <- function(result, concept_id = NULL) {
  if (is.null(concept_id) || length(concept_id) == 0) return(result)
  ids <- as.integer(concept_id)
  id_cols <- c("covariate_id", "concept_id")
  subset_df <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) return(df)
    col <- intersect(id_cols, names(df))
    if (length(col) == 0) return(df)
    df[as.integer(df[[col[1]]]) %in% ids, , drop = FALSE]
  }
  result$pooled <- subset_df(result$pooled)
  result$per_site <- lapply(result$per_site, subset_df)
  result
}

#' Covariate prevalence over a cohort, in one call
#'
#' Thin wrapper over \code{\link{ds.omop.analysis.run}} for the catalog's
#' feature-prevalence analysis (\code{"dsomop:fe.prevalence"}): the per-covariate
#' distinct-person count and proportion over a scoped cohort, for one clinical
#' domain. It builds the analysis params and delegates, so cohort/table scoping,
#' cross-server pooling, optional plotting, and the ONE per-patient disclosure
#' gate are inherited unchanged.
#'
#' Because the cohort IS the analysis population, a \code{cohort} (or
#' \code{tables}) scope is required; an un-scoped call fails closed with a clear
#' error from the server rather than returning an empty frame.
#'
#' @param concept_id Integer vector or \code{NULL}; when supplied, the gated
#'   result is narrowed to these covariate concept id(s) (a post-gate row
#'   subset). \code{NULL} (the default) returns the domain's top covariates.
#' @param cohort Cohort reference to scope to: a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side cohort table name. Required
#'   unless \code{tables} is given.
#' @param domain Character; clinical domain by name (\code{"condition"},
#'   \code{"drug"}, \code{"procedure"}, \code{"measurement"}, \code{"observation"})
#'   or its code (\code{"0"}-\code{"4"}). Default \code{"condition"}.
#' @param top_n Integer; number of top covariates to return (default 50).
#' @param tables Optional character vector of \code{omop.table} symbol names to
#'   scope to (their distinct persons); may be combined with \code{cohort}.
#' @param plot Logical; build the entry's client-side plot over the gated data
#'   (default \code{FALSE}). See \code{\link{ds.omop.analysis.run}}.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}; dsBaseClient-style aliases are accepted.
#' @param symbol Character; the session symbol (default \code{"omop"}).
#' @param conns DSI connection object(s) or \code{NULL} to use the session
#'   default.
#' @return A \code{dsomop_result} (see \code{\link{ds.omop.analysis.run}}).
#' @examples
#' \dontrun{
#' # Simplest path: top condition prevalence over a cohort, one call.
#' ds.omop.prevalence(cohort = my_cohort)
#'
#' # A specific concept's prevalence (drug domain).
#' ds.omop.prevalence(concept_id = 1503297, cohort = my_cohort, domain = "drug")
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}, \code{\link{ds.omop.distribution}},
#'   \code{\link{ds.omop.cohort.create}}
#' @export
ds.omop.prevalence <- function(concept_id = NULL, cohort = NULL,
                               domain = "condition", top_n = 50,
                               tables = NULL, plot = FALSE,
                               symbol = "omop", conns = NULL,
                               type = "both") {
  if (is.null(cohort) && is.null(tables)) {
    stop("ds.omop.prevalence() computes prevalence WITHIN a cohort: pass ",
         "cohort= (a cohort handle, cohort_definition_id, or cohort table) ",
         "or tables= (omop.table symbols). For database-wide counts, use ",
         "ds.omop.analysis.run() with an Achilles/QueryLibrary analysis.",
         call. = FALSE)
  }
  params <- list(domain_code = .analysis_domain_code(domain, "0"),
                 top_n = as.integer(top_n))
  res <- ds.omop.analysis.run("dsomop:fe.prevalence", params = params,
                              cohort = cohort, tables = tables, plot = plot,
                              symbol = symbol, conns = conns, type = type)
  .analysis_filter_concepts(res, concept_id)
}

#' Continuous-value distribution over a cohort, in one call
#'
#' Thin wrapper over \code{\link{ds.omop.analysis.run}} for the catalog's
#' continuous-covariate analysis (\code{"dsomop:fe.continuous"}): per-covariate
#' count and avg/sd/median/p10-p90 over a scoped cohort (measurement values, age,
#' or time-in-cohort). It builds the params and delegates, inheriting scoping,
#' pooling, optional plotting, and the ONE disclosure gate (which strips min/max
#' and masks sub-threshold stats) unchanged.
#'
#' As with \code{\link{ds.omop.prevalence}}, the cohort IS the population, so a
#' \code{cohort}/\code{tables} scope is required; an un-scoped call errors clearly.
#'
#' @param cohort Cohort reference to scope to (handle, \code{cohort_definition_id},
#'   or server-side table name). Required unless \code{tables} is given.
#' @param metric Character; \code{"measurement_value"} (default), \code{"age"},
#'   or \code{"time_in_cohort"}.
#' @param domain Character; value domain for \code{metric = "measurement_value"}
#'   by name (\code{"measurement"} / \code{"observation"}) or code
#'   (\code{"3"}/\code{"4"}). Default \code{"measurement"}.
#' @param top_n Integer; number of top covariates to return (default 50).
#' @param concept_id Integer vector or \code{NULL}; narrow the gated result to
#'   these covariate concept id(s) (post-gate row subset). Default \code{NULL}.
#' @param tables Optional character vector of \code{omop.table} symbol names to
#'   scope to; may be combined with \code{cohort}.
#' @param plot Logical; build the entry's client-side plot (default \code{FALSE}).
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}; dsBaseClient-style aliases are accepted.
#' @param symbol Character; the session symbol (default \code{"omop"}).
#' @param conns DSI connection object(s) or \code{NULL} to use the session
#'   default.
#' @return A \code{dsomop_result} (see \code{\link{ds.omop.analysis.run}}).
#' @examples
#' \dontrun{
#' # Measurement-value distributions over a cohort, one call.
#' ds.omop.distribution(cohort = my_cohort)
#'
#' # Age distribution of the cohort.
#' ds.omop.distribution(cohort = my_cohort, metric = "age")
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}, \code{\link{ds.omop.prevalence}}
#' @export
ds.omop.distribution <- function(cohort = NULL, metric = "measurement_value",
                                 domain = "measurement", top_n = 50,
                                 concept_id = NULL, tables = NULL, plot = FALSE,
                                 symbol = "omop", conns = NULL,
                                 type = "both") {
  if (is.null(cohort) && is.null(tables)) {
    stop("ds.omop.distribution() computes a distribution WITHIN a cohort: pass ",
         "cohort= (a cohort handle, cohort_definition_id, or cohort table) ",
         "or tables= (omop.table symbols).", call. = FALSE)
  }
  params <- list(metric = metric,
                 domain_code = .analysis_domain_code(domain, "3"),
                 top_n = as.integer(top_n))
  res <- ds.omop.analysis.run("dsomop:fe.continuous", params = params,
                              cohort = cohort, tables = tables, plot = plot,
                              symbol = symbol, conns = conns, type = type)
  .analysis_filter_concepts(res, concept_id)
}

#' Meta-analyze a comparative effect estimate across databases (evidence synthesis)
#'
#' The CLIENT half of OHDSI evidence synthesis: run a per-site fitted comparative
#' effect estimate on every server, then INVERSE-VARIANCE meta-analyze the
#' per-site log-estimates into ONE pooled estimate + 95\% CI (the
#' \code{metafor::rma} pattern by hand — no new dependency). A single site cannot
#' compute a cross-database pooled estimate, so the server-side
#' \code{dsomop:cm.effect_estimate} (CohortMethod HR/RR; the
#' \code{es_cm_result} delegate) and \code{dsomop:sccs.incidence_rate_ratio}
#' (SCCS IRR; the \code{es_sccs_result} delegate) each emit only the
#' disclosure-safe per-site \code{log_estimate} + SE; this function pools them.
#'
#' Both a FIXED-effect and a random-effects (DerSimonian-Laird) pooled estimate
#' are returned, with Cochran's Q, \eqn{I^2}, and \eqn{\tau^2} heterogeneity. No
#' patient data crosses sites — only the already-gated per-site sufficient
#' statistics. A site whose per-site estimate the server suppressed (small/empty
#' arm) is ABSENT from the pool: under \code{pooling_policy = "strict"} (default)
#' any suppressed site aborts the pool fail-closed; \code{"pooled_only_ok"} pools
#' the remaining sites and warns.
#'
#' @param name Character; the per-site effect-estimate analysis id. Default
#'   \code{"dsomop:cm.effect_estimate"} (CohortMethod). Use
#'   \code{"dsomop:sccs.incidence_rate_ratio"} for SCCS, or the
#'   \code{es_cm_result} / \code{es_sccs_result} evidence-synthesis ids.
#' @param params Named list of analysis params (e.g. \code{outcome_concept_id},
#'   \code{model_type}); passed through to the per-site analysis unchanged.
#' @param cohort For CohortMethod, the two-population target+comparator scope (a
#'   length-2 set of cohort handles / ids / table names); for SCCS, the scoped
#'   case population.
#' @param tables Optional \code{omop.table} symbol scope (see
#'   \code{\link{ds.omop.analysis.run}}).
#' @param combine Character; \code{"union"} (default) or \code{"intersect"} for
#'   multi-source scope folding.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default \code{"omop"}).
#' @param conns DSI connection object(s) or \code{NULL} to use the session
#'   default.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}; see \code{\link{ds.omop.analysis.run}}.
#' @return A \code{dsomop_result}: \code{per_site} holds each server's gated
#'   per-site effect-estimate frame; \code{pooled} holds the one-row meta-analysis
#'   (pooled HR/RR + CI under both models, \code{n_databases}, \code{i2},
#'   \code{tau2}).
#' @examples
#' \dontrun{
#' # Pool a CohortMethod hazard ratio across databases.
#' res <- ds.omop.meta.effect_estimate(
#'   params = list(outcome_concept_id = 4329847),
#'   cohort = c(target_cohort, comparator_cohort))
#' res$pooled   # estimate_random, ci_lo_random, ci_hi_random, i2, ...
#' }
#' @seealso \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.meta.effect_estimate <- function(name = "dsomop:cm.effect_estimate",
                                         params = list(), cohort = NULL,
                                         tables = NULL, combine = "union",
                                         pooling_policy = "strict",
                                         symbol = "omop", conns = NULL,
                                         type = "both") {
  # The catalog's server-owned effect_estimate contract selects the
  # inverse-variance dispatcher. No wrapper-side column-name inference is used.
  ds.omop.analysis.run(
    name, params = params, cohort = cohort, tables = tables,
    combine = combine, pooling_policy = pooling_policy, plot = FALSE,
    symbol = symbol, conns = conns, type = type
  )
}
