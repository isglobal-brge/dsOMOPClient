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

#' Build the server-side scope expression for an analysis run
#'
#' The server's \code{omopAnalysisRunDS}/\code{omopAnalysisRunAssignDS} take a
#' single \code{scope} argument that may mix a cohort reference (a temp-table
#' name or a \code{cohort_definition_id}) with one or more workspace
#' \code{omop.table} frames. On the client we hold the \code{omop.table}s only by
#' SYMBOL NAME, so we splice them into the call as unevaluated symbols
#' (\code{as.name}) — exactly as the data-manipulation verbs and
#' \code{omopFactorLevelsDS} do — and let DataSHIELD resolve each name to the
#' server-side frame. The cohort reference travels as a literal value.
#'
#' Forms produced:
#' \itemize{
#'   \item no cohort, no tables -> \code{NULL} (no scoping argument).
#'   \item a cohort only -> the resolved cohort value (string / id), passed as-is.
#'   \item table symbol(s) (with or without a cohort) -> a \code{list(...)} call
#'     whose elements are the cohort value (if any) followed by one
#'     \code{as.name(<symbol>)} per table.
#' }
#'
#' @param cohort Cohort reference (a \code{dsomop_cohort_handle}, a
#'   \code{cohort_definition_id}, or a server-side table name) or \code{NULL}.
#' @param tables Character vector of server-side \code{omop.table} symbol names,
#'   or \code{NULL}.
#' @return \code{NULL}, a single literal cohort value, or a \code{call} to
#'   \code{list()} mixing the cohort literal and table symbols.
#' @keywords internal
.analysis_scope_expr <- function(cohort = NULL, tables = NULL) {
  cohort_val <- .cohort_scope_arg(cohort)

  if (!is.null(tables)) {
    if (!is.character(tables)) {
      stop("tables must be the name(s) of server-side omop.table symbol(s).",
           call. = FALSE)
    }
    tables <- tables[nzchar(tables)]
  }

  if (is.null(tables) || length(tables) == 0) {
    # Cohort-only (or nothing): pass the cohort value straight through, matching
    # the exploration wrappers' single-cohort scope.
    return(cohort_val)
  }

  # One or more table symbols (optionally plus a cohort): build a list() call so
  # the server receives a mixed scope of a cohort literal + omop.table frames.
  elems <- c(
    if (!is.null(cohort_val)) list(cohort_val) else list(),
    lapply(tables, as.name)
  )
  as.call(c(as.name("list"), elems))
}

#' Build the (possibly scope-bearing) server-side analysis run call
#'
#' Constructs the unevaluated DataSHIELD call for \code{omopAnalysisRunDS} /
#' \code{omopAnalysisRunAssignDS}. \code{params} is JSON/base64-encoded for Opal
#' transport (\code{\link{.ds_encode}}); \code{scope_expr} (when present) is
#' spliced in by NAME so a \code{list(as.name(<table>))} scope resolves to the
#' server-side \code{omop.table} frames, and \code{combine} is passed by name so
#' a \code{NULL} scope never shifts it into the wrong positional slot.
#'
#' @param fn Character; the server method name.
#' @param res_symbol Character; the server-side handle symbol.
#' @param name Character; the catalog entry name.
#' @param params Named list of parameter values.
#' @param scope_expr \code{NULL}, a literal cohort value, or a \code{list(...)}
#'   call (from \code{\link{.analysis_scope_expr}}).
#' @param combine Character; \code{"union"} or \code{"intersect"}.
#' @return An unevaluated \code{call}.
#' @keywords internal
.analysis_run_call <- function(fn, res_symbol, name, params, scope_expr,
                               combine) {
  args <- list(as.name(fn), res_symbol, name, .ds_encode(params))
  if (!is.null(scope_expr)) {
    args <- c(args, list(scope = scope_expr))
  }
  args <- c(args, list(combine = combine))
  as.call(args)
}

#' Render an entry's client-side plot over already-gated pooled data
#'
#' The plotting half of the analysis catalog runs ENTIRELY on the client, over
#' data that has ALREADY passed the server's single per-patient disclosure gate.
#' For entries that ship one, the server returns an INERT plot recipe in the
#' entry metadata (\code{omopAnalysisGetDS}): \code{plot$type} (a label) and
#' \code{plot$code}, the SOURCE TEXT of a \code{function(df, params)} that builds
#' a \code{ggplot}. That source ships INSIDE the installed dsOMOP / analysis-pack
#' package; it is authored by the same trusted maintainers as the gate itself.
#'
#' Safety model (why eval here is not a code-injection surface):
#' \itemize{
#'   \item The client NEVER sends plot code to the server, and the server NEVER
#'     evaluates it — the server only carries the inert text alongside the gated
#'     aggregate. There is no client->server code path here.
#'   \item The code is evaluated CLIENT-SIDE, in the analyst's own session, only
#'     over \code{df} — the pooled data frame that already cleared the gate
#'     (small-cell suppressed, banded, distributions masked). A plot can only
#'     ever redraw disclosure-controlled numbers; it cannot reach back into any
#'     server or recover a suppressed cell.
#'   \item Evaluation is wrapped in \code{\link[base]{tryCatch}} so a broken or
#'     incompatible plot recipe NEVER costs the analyst the (already returned)
#'     data — it degrades to a warning and \code{NULL} plot.
#' }
#' \pkg{ggplot2} is required only on this path (\code{plot = TRUE}); a clear
#' message is raised if it is not installed, rather than failing obscurely inside
#' the recipe.
#'
#' @param meta Named list; one entry's metadata from \code{omopAnalysisGetDS}.
#'   The plot recipe is read from \code{meta$plot} (a \code{list(type, code)}),
#'   tolerating a nested \code{meta$compute$plot} for forward compatibility.
#' @param pooled Data frame; the pooled, gate-passed aggregate to plot.
#' @param params Named list; the same parameter values passed to the run, handed
#'   to the recipe's \code{function(df, params)} second argument.
#' @return A \code{ggplot} object, or \code{NULL} when the entry ships no plot
#'   recipe or the recipe could not be built (with a warning in the latter case).
#' @keywords internal
.analysis_render_plot <- function(meta, pooled, params) {
  # The recipe lives at meta$plot (flat, as omopAnalysisGetDS exposes it); accept
  # a nested compute$plot too so the client is robust to either metadata shape.
  recipe <- meta$plot %||% meta$compute$plot
  code <- recipe$code
  if (is.null(code) || !nzchar(code)) {
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

  # Parse + evaluate the recipe and call it on the already-gated pooled frame.
  # A failure here must never lose the data the caller already has, so the whole
  # build degrades to a warning + NULL plot.
  tryCatch({
    plot_fn <- eval(parse(text = code))
    if (!is.function(plot_fn)) {
      stop("plot recipe did not evaluate to a function(df, params).",
           call. = FALSE)
    }
    p <- plot_fn(pooled, params)
    if (!inherits(p, "ggplot")) {
      stop("plot recipe did not return a ggplot object.", call. = FALSE)
    }
    p
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
#' catalog is defined by the server package, it is identical across servers and
#' the first responding server's result is returned as the pooled view. No SQL,
#' compute functions, or other server internals are exposed.
#'
#' @param domain Character; optional clinical-domain filter (e.g.
#'   \code{"condition"}, \code{"person"}). \code{NULL} (the default) returns
#'   entries for all domains.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
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
                                  conns = NULL) {
  code <- .build_code("ds.omop.analysis.list", domain = domain, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisListDS", session$res_symbol, domain)
  )

  # Catalog is identical across servers: pooled view is the first server's.
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
}

#' Get unified analysis catalog entry metadata
#'
#' Returns full metadata for a single catalog entry: its parameter specs,
#' compute kind, disclosure spec, and scoping capabilities. Use it to discover
#' an entry's parameters and to check whether it accepts cohort/table scoping
#' before running it. The metadata is identical across servers, so the first
#' responding server's result is returned as the pooled view.
#'
#' @param name Character; the entry id (e.g. \code{"dsomop:achilles.401"}) or a
#'   shorthand for it (native id without the \code{"dsomop:"} prefix, or a unique
#'   id suffix; an ambiguous shorthand errors with the candidates).
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"}. The pooled
#'   element is a named list with the entry's \code{name}, \code{description},
#'   \code{domain}, \code{mode}, \code{params}, \code{compute_kind},
#'   \code{disclosure}, \code{scope}, \code{adapter}, and the inert client-side
#'   \code{plot} recipe (\code{NULL} when the entry ships none).
#' @examples
#' \dontrun{
#' meta <- ds.omop.analysis.get("dsomop:achilles.401")
#' meta$pooled$params
#' meta$pooled$mode
#' }
#' @seealso \code{\link{ds.omop.analysis.list}}, \code{\link{ds.omop.analysis.run}}
#' @export
ds.omop.analysis.get <- function(name, symbol = "omop", conns = NULL) {
  code <- .build_code("ds.omop.analysis.get", name = name, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopAnalysisGetDS", session$res_symbol, name)
  )

  # Metadata is identical across servers: pooled view is the first server's.
  pooled <- if (length(raw) > 0) raw[[1]] else NULL

  dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled"))
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
#'   combined with \code{cohort}.
#' @param combine Character; how to fold multiple scope sources together:
#'   \code{"union"} (the default) or \code{"intersect"}.
#' @param pooling_policy Character; how suppressed (NA) cells are handled when
#'   pooling aggregate results across servers. \code{"strict"} (the default)
#'   sets the pooled value to NA if any server suppressed it; \code{"pooled_only_ok"}
#'   sums only the non-suppressed values.
#' @param plot Logical; when \code{TRUE} AND the entry ships a plot recipe, build
#'   a \code{ggplot} CLIENT-SIDE over the pooled, gate-passed data and attach it
#'   to the result (also returned via the \code{"plot"} attribute and
#'   \code{meta$plot}). \code{FALSE} (the default) returns data only and never
#'   touches \pkg{ggplot2}. The plot is purely a client-side rendering of numbers
#'   that already cleared the server's disclosure gate (see Safety, below). A
#'   broken/incompatible recipe degrades to a warning and a \code{NULL} plot — it
#'   never costs you the returned data.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return A \code{dsomop_result} object. For aggregate entries, \code{per_site}
#'   holds each server's disclosure-controlled data frame and \code{pooled} holds
#'   the cross-server aggregation. For assign-mode entries, \code{per_site} holds
#'   per-server assignment confirmations (the data stays on the server) and the
#'   server-side symbol name is recorded in the result metadata. When
#'   \code{plot = TRUE} and the entry ships a plot recipe, the built \code{ggplot}
#'   is attached as the \code{"plot"} attribute (and \code{meta$plot}).
#'
#' @section Safety (client-side plotting): Some entries ship an INERT plot recipe
#'   — the source text of a \code{function(df, params)} that builds a
#'   \code{ggplot} — which the server returns as metadata ALONGSIDE the gated
#'   aggregate. That source ships inside the installed dsOMOP / analysis-pack
#'   package (authored by the same maintainers as the disclosure gate). When
#'   \code{plot = TRUE} the client evaluates it LOCALLY and calls it only on
#'   \code{pooled} — data that has already passed the server's single per-patient
#'   gate (small-cell suppressed, banded, distributions masked). The client never
#'   sends plot code to the server and the server never evaluates it; a plot can
#'   only redraw disclosure-controlled numbers, never recover a suppressed cell.
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
                                 symbol = "omop", conns = NULL) {
  combine <- match.arg(combine, c("union", "intersect"))

  code <- .build_code("ds.omop.analysis.run", name = name, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Fetch the entry metadata once: it decides aggregate vs assign (assign loaders
  # keep their result server-side) AND carries the inert plot recipe used below
  # when plot = TRUE. Identical across servers, so the first server's view is used.
  entry_meta <- tryCatch({
    meta <- DSI::datashield.aggregate(
      conns, expr = call("omopAnalysisGetDS", session$res_symbol, name))
    if (length(meta) > 0) meta[[1]] else NULL
  }, error = function(e) NULL)
  is_assign <- identical(entry_meta$mode, "assign")

  scope_expr <- .analysis_scope_expr(cohort, tables)

  if (is_assign) {
    # Assign-mode loader: the server stores the result; nothing returns to pool.
    newobj <- .generate_symbol("omop.analysis")
    run_expr <- .analysis_run_call(
      "omopAnalysisRunAssignDS", session$res_symbol, name, params,
      scope_expr, combine)
    per_site <- list()
    for (srv in names(conns)) {
      ok <- tryCatch({
        DSI::datashield.assign.expr(conns[srv], symbol = newobj,
                                    expr = run_expr)
        TRUE
      }, error = function(e) e$message)
      per_site[[srv]] <- ok
    }
    return(dsomop_result(
      per_site = per_site, pooled = NULL,
      meta = list(call_code = code, scope = "per_site",
                  assign_symbol = newobj)))
  }

  # Aggregate entry: run on each server, then pool the returned frames.
  run_expr <- .analysis_run_call(
    "omopAnalysisRunDS", session$res_symbol, name, params,
    scope_expr, combine)
  raw <- .ds_safe_aggregate(conns, expr = run_expr)

  pool_out <- .pool_result(raw, "ohdsi_results", pooling_policy)

  # Optional client-side plot over the pooled, gate-passed data. The data is
  # already in hand; .analysis_render_plot degrades to NULL (with a warning) on
  # any failure so plotting can never lose the returned aggregate.
  gg <- if (isTRUE(plot)) {
    .analysis_render_plot(entry_meta, pool_out$result, params)
  } else NULL

  result <- dsomop_result(
    per_site = raw, pooled = pool_out$result,
    meta = list(call_code = code, scope = "pooled",
                pooling_policy = pooling_policy,
                warnings = pool_out$warnings))
  # The dsomop_result constructor keeps only its known meta fields, so attach the
  # built plot explicitly (as documented: available via both the "plot"
  # attribute and meta$plot). NULL when plot = FALSE or no recipe was built.
  attr(result, "plot") <- gg
  result$meta$plot <- gg
  result
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
                               symbol = "omop", conns = NULL) {
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
                              symbol = symbol, conns = conns)
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
                                 symbol = "omop", conns = NULL) {
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
                              symbol = symbol, conns = conns)
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
                                         symbol = "omop", conns = NULL) {
  # Run the per-site effect estimate via the power path (inherits scoping, the
  # single per-patient gate, and the per_site frames), then RE-POOL the per-site
  # frames with the inverse-variance meta-analysis result_type. The default
  # "ohdsi_results" pooling would wrongly stack the per-arm rows; effect
  # estimates must be combined on the log scale, weighted by 1/SE^2.
  res <- ds.omop.analysis.run(name, params = params, cohort = cohort,
                              tables = tables, combine = combine,
                              pooling_policy = pooling_policy, plot = FALSE,
                              symbol = symbol, conns = conns)
  pool_out <- .pool_result(res$per_site, "effect_estimate", pooling_policy)
  res$pooled <- pool_out$result
  res$meta$scope <- "pooled"
  res$meta$warnings <- c(res$meta$warnings, pool_out$warnings)
  res
}
