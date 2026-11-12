# Module: Recipe language 2.0 -- execution: capability negotiation, lossless
# plan transport, federation contract, publication status and the
# on_small_site matrix (design-2.10.3 sections 4.4, 5 and 7.3).

.V2_REJECTION_CODES <- c("insufficient_support", "coverage_ambiguous", "join_cardinality",
                         "match_candidates_exceeded", "dialect_unsupported",
                         "unavailable_table", "validation_error")
.V2_INSUFFICIENT_MESSAGE <- paste0(
  "Disclosive: operation blocked - insufficient individuals to meet disclosure ",
  "threshold. No further details available.")

.v2_is_plan_v2 <- function(plan) {
  inherits(plan, "omop_plan_v2") ||
    (is.list(plan) && identical(suppressWarnings(as.integer(plan$version %||% NA)), 2L))
}

# --- capability negotiation ----------------------------------------------------------

#' The recipe-language capability block each server advertises
#' @keywords internal
.v2_server_language <- function(session, servers) {
  caps <- session$capabilities %||% list()
  stats::setNames(lapply(servers, function(s) {
    rl <- caps[[s]]$recipe_language
    if (is.null(rl)) return(NULL)
    rl$operations <- as.character(unlist(rl$operations %||% list()))
    rl$reducers <- as.character(unlist(rl$reducers %||% list()))
    rl
  }), servers)
}

#' Operations a v2 plan needs that each server lacks
#'
#' @param plan A v2 plan.
#' @param session The OMOP session.
#' @param servers Server names.
#' @param extra Additional operations required for execution.
#' @return Named list: per server, the missing operations (empty when ok).
#' @keywords internal
.v2_support_gaps <- function(plan, session, servers, extra = character(0)) {
  ops <- .plan_v2_operations(plan)
  reducers <- attr(ops, "reducers")
  lang <- .v2_server_language(session, servers)
  stats::setNames(lapply(servers, function(s) {
    rl <- lang[[s]]
    if (is.null(rl) || suppressWarnings(as.integer(rl$version %||% 0L)) < 2L) {
      return("recipe_language 2")
    }
    c(setdiff(c(ops, extra), rl$operations),
      if (length(rl$reducers)) paste0("reducer ", setdiff(reducers, rl$reducers)))
  }), servers)
}

.v2_assert_supported <- function(plan, session, servers, extra = character(0)) {
  gaps <- .v2_support_gaps(plan, session, servers, extra)
  bad <- gaps[vapply(gaps, length, integer(1)) > 0L]
  if (length(bad)) {
    detail <- paste(vapply(names(bad), function(s) {
      paste0(s, " lacks ", paste(bad[[s]], collapse = ", "))
    }, ""), collapse = "; ")
    stop("This recipe needs recipe-language 2 operations that the server(s) do not ",
         "support (", detail, "). It has no equivalent v1 plan: parent inheritance, ",
         "coverage policies, event censoring, matching and the other language-2 ",
         "constructs are never dropped to fit an older server. Upgrade dsOMOP on ",
         "those servers to 2.10.3.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Constructs that make a recipe need a v2 plan
#'
#' Language-2 objects, and \code{parent_id} (whose v1 rendering silently ignored
#' inheritance, defect B-8). A recipe without them is v1-expressible: it
#' executes as its unchanged 2.10.1 plan on every server.
#' @keywords internal
.recipe_needs_v2 <- function(recipe) {
  parents <- unlist(lapply(names(recipe$populations %||% list()), function(id) {
    if (!is.null(recipe$populations[[id]]$parent_id)) {
      paste0("population '", id, "': parent_id inheritance")
    }
  }))
  c(.recipe_v2_constructs(recipe), parents)
}

.v2_has_criterion <- function(f) {
  if (inherits(f, "omop_criterion")) return(TRUE)
  if (inherits(f, "omop_filter_group")) {
    return(any(vapply(f$children, .v2_has_criterion, logical(1))))
  }
  FALSE
}

#' Language-2 constructs of a recipe (no v1 equivalent)
#' @keywords internal
.recipe_v2_constructs <- function(recipe) {
  out <- character(0)
  add <- function(x) out <<- c(out, x)
  if (length(recipe$concept_sets %||% list())) add("a concept-set registry (concept_sets =)")
  if (length(recipe$events %||% list())) add("an event registry (events =)")
  if (length(recipe$intervals %||% list())) add("intervals (omop_era())")
  if (!is.null(recipe$coverage)) add("a recipe coverage policy")
  if (!is.null(recipe$publish)) add("publication roots (publish =)")
  if (any(vapply(recipe$filters %||% list(), .v2_has_criterion, logical(1)))) {
    add("language-2 criteria in recipe filters")
  }
  for (id in names(recipe$populations %||% list())) {
    p <- recipe$populations[[id]]
    if (!is.null(p[["index"]])) add(paste0("population '", id, "': omop_index()"))
    if (!is.null(p$match)) add(paste0("population '", id, "': omop_match()"))
    if (!is.null(p$coverage)) add(paste0("population '", id, "': omop_coverage()"))
    if (any(vapply(p$filters %||% list(), .v2_has_criterion, logical(1)))) {
      add(paste0("population '", id, "': language-2 criteria"))
    }
  }
  for (vn in names(recipe$variables %||% list())) {
    v <- recipe$variables[[vn]]
    if (.v2_is_v2_variable(v)) {
      add(paste0("variable '", vn, "': ", if (identical(v$kind, "reducer")) "event/reducer" else v$kind))
    } else if (!is.null(v[["where"]]) || !is.null(v[["visit"]]) || isTRUE(v[["broadcast"]])) {
      add(paste0("variable '", vn, "': where/visit/broadcast"))
    }
  }
  for (on in names(recipe$outputs %||% list())) {
    o <- recipe$outputs[[on]]
    if (!is.null(o$format)) add(paste0("output '", on, "': format = \"", o$format, "\""))
    else if (!is.null(o$grain) || !is.null(o$collapse) || !is.null(o$sources)) {
      add(paste0("output '", on, "': grain/collapse/sources"))
    }
  }
  out
}

#' The plan a recipe executes as on the connected servers
#' @keywords internal
.recipe_plan_for_execution <- function(recipe, session, conns) {
  if (length(.recipe_needs_v2(recipe)) == 0L) {
    return(recipe_to_plan(recipe, version = 1L))
  }
  plan <- recipe_to_plan(recipe, version = 2L)
  .v2_assert_supported(plan, session, names(conns))
  plan
}

# --- lossless transport ---------------------------------------------------------------

#' Encode a v2 plan for DataSHIELD transport without losing precision
#'
#' Like \code{.ds_encode()} (URL-safe base64 JSON) but with every digit kept
#' (\code{digits = NA}), explicit JSON nulls and empty objects preserved, so
#' the server parses exactly the plan whose semantic digest the client
#' computed (unit conversion factors such as 0.45359237 must not be rounded).
#' @keywords internal
.ds_encode_lossless <- function(x) {
  json <- as.character(jsonlite::toJSON(.v2_plain(x), auto_unbox = TRUE, null = "null",
                                        digits = NA))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(enc2utf8(json))))
  b64 <- gsub("\\+", "-", b64)
  b64 <- gsub("/", "_", b64)
  b64 <- gsub("=+$", "", b64)
  paste0("B64:", b64)
}

# --- federation contract for v2 plans -----------------------------------------------------

#' Tables a v2 plan needs on every server
#' @keywords internal
.plan_v2_manifest <- function(plan) {
  x <- .v2_plain(plan)
  tables <- c("person")
  vocab <- FALSE
  for (ev in x$events %||% list()) {
    if (!is.null(ev$episode_members)) {
      if (!identical(ev$episode_members$on_missing_table, "unavailable")) {
        tables <- c(tables, "episode_event")
      }
      next
    }
    if (!is.null(ev$cost)) {
      if (!identical(ev$cost$on_missing_table, "unavailable")) tables <- c(tables, "cost")
      next
    }
    if (!identical(ev$on_missing_table, "unavailable")) tables <- c(tables, ev$table)
    cs <- if (is.character(ev$concept_set)) x$concept_sets[[ev$concept_set]] else ev$concept_set
    if (isTRUE(cs$include_descendants) || isTRUE(cs$include_mapped)) vocab <- TRUE
  }
  for (cs in x$concept_sets %||% list()) {
    if (isTRUE(cs$include_descendants) || isTRUE(cs$include_mapped)) vocab <- TRUE
  }
  for (iv in x$intervals %||% list()) if (identical(iv$by, "ingredient")) vocab <- TRUE
  if (length(x$populations %||% list())) tables <- c(tables, "observation_period")
  pops_cohort <- vapply(x$populations %||% list(), function(p) {
    !is.null(p$index$source$cohort_definition_id) || !is.null(p$cohort_definition_id)
  }, logical(1))
  if (any(pops_cohort)) tables <- c(tables, "cohort")
  for (v in x$variables %||% list()) {
    k <- v$derived$kind %||% ""
    if (k %in% c("charlson", "chads2", "chadsvasc", "dcsi", "hfrs")) {
      tables <- c(tables, "condition_occurrence")
      vocab <- TRUE
    }
    if (k %in% c("died", "days_to_death")) tables <- c(tables, "death")
    if (!is.null(v$raw)) tables <- c(tables, v$raw$table)
  }
  for (o in x$outputs %||% list()) {
    if (identical(o$format, "survival") && "death" %in% unlist(o$options$censor)) {
      tables <- c(tables, "death")
    }
  }
  if (!isFALSE(x$options$translate_concepts)) vocab <- TRUE
  tables <- sort(unique(tolower(tables)))
  list(tables = stats::setNames(rep(list(character(0)), length(tables)), tables),
       needs_vocabulary_identity = vocab)
}

#' Bind (or re-check) the multi-server contract of a v2 plan
#'
#' The contract binds the plan's semantic digest (section 4.4: the federation
#' harmonisation signature and the DP provenance capsule bind the same
#' digest), the participating servers and the schema snapshot of every table
#' the plan needs. It is stored as the plan's \code{"harmonization"} attribute,
#' never inside the wire plan (whose schema is closed).
#' @keywords internal
.plan_v2_federation <- function(plan, symbol, conns) {
  if (length(conns) < 2L) return(plan)
  manifest <- .plan_v2_manifest(plan)
  comparison <- ds.omop.compare(symbol = symbol, conns = conns,
                                tables = names(manifest$tables))
  if (length(comparison$column_errors %||% character(0)) > 0L) {
    stop("Cannot establish a common schema contract: schema introspection failed for ",
         paste(names(comparison$column_errors), collapse = ", "), ".", call. = FALSE)
  }
  absent <- setdiff(names(manifest$tables), tolower(comparison$common_tables %||% character(0)))
  if (length(absent)) {
    stop("The plan needs table(s) that are not present on every server: ",
         paste(absent, collapse = ", "), ". Declare optional sources with ",
         "on_missing_table = \"unavailable\".", call. = FALSE)
  }
  schema <- .plan_schema_snapshot(comparison, manifest$tables, manifest$needs_vocabulary_identity)
  signature <- .plan_v2_digest(plan)$sha256
  existing <- attr(plan, "harmonization")
  if (!is.null(existing)) {
    if (!identical(existing$plan_signature, signature)) {
      stop("The plan changed after its multi-server contract was bound; harmonize again.",
           call. = FALSE)
    }
    if (!identical(existing$schema, schema)) {
      stop("Servers or relevant OMOP schemas changed after the plan contract was bound.",
           call. = FALSE)
    }
    return(plan)
  }
  attr(plan, "harmonization") <- list(version = 3L, mode = "intersection", strict = TRUE,
                                      manifest = manifest, schema = schema,
                                      plan_signature = signature)
  plan
}

# --- publication status (section 7.3) -------------------------------------------------------

#' Do all servers expose the publication status transport?
#' @keywords internal
.plan_status_capable <- function(session, servers) {
  lang <- .v2_server_language(session, servers)
  length(servers) > 0L && all(vapply(lang, function(rl) {
    !is.null(rl) && "status.accessor" %in% rl$operations
  }, logical(1)))
}

.plan_validate_status <- function(st, server) {
  ok <- is.list(st) && all(c("status", "published", "rejected") %in% names(st)) &&
    is.character(st$status) && length(st$status) == 1L &&
    st$status %in% c("ok", "partial", "refused") && is.list(st$published %||% list()) &&
    is.list(st$rejected %||% list())
  if (!ok) stop("Server '", server, "' returned a malformed publication status.", call. = FALSE)
  for (root in names(st$published)) {
    for (comp in names(st$published[[root]])) {
      e <- st$published[[root]][[comp]]
      sym <- e$symbol %||% e$path
      if (!is.character(sym) || length(sym) != 1L || !nzchar(sym)) {
        stop("Server '", server, "' returned a malformed publication inventory.", call. = FALSE)
      }
    }
  }
  for (root in names(st$rejected)) {
    code <- st$rejected[[root]]$code
    if (!is.character(code) || length(code) != 1L || !code %in% .V2_REJECTION_CODES) {
      stop("Server '", server, "' returned an unknown rejection code.", call. = FALSE)
    }
  }
  st
}

#' Revoke one execution on every given server (federation rollback)
#' @keywords internal
.plan_revoke_execution <- function(conns, exec_symbol, servers) {
  failures <- character(0)
  for (server in servers) {
    res <- tryCatch({
      DSI::datashield.aggregate(conns[server], call("omopStagedRevokeDS", exec_symbol))
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(res)) failures <- c(failures, server)
  }
  for (server in servers) {
    tryCatch(DSI::datashield.rm(conns[server], exec_symbol), error = function(e) NULL)
  }
  if (length(failures)) {
    stop("Federation rollback could not be confirmed on: ", paste(failures, collapse = ", "),
         ". Remove this execution's outputs before continuing.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Run one execution under the publication status protocol
#'
#' Sends the execution with \code{commit_policy} derived from
#' \code{on_small_site} (\code{"skip"} asks every server to commit all or none,
#' so a refused site never commits anything and nothing is cleaned after a
#' commit), reads each server's bounded status through
#' \code{omopPlanStatusDS()} before the status symbol is deleted, and applies
#' the frozen strict/non-strict x error/skip matrix of section 7.3: a final
#' support rejection is an error (federation rollback) under
#' \code{"error"} on a strict server, a warning naming the rejected families
#' on a non-strict server, and a skipped (NULL) site under \code{"skip"}.
#' Every other rejection code, a transport failure, an execution failure or an
#' inventory that differs between servers rolls the federation back and is an
#' error under both settings.
#' @keywords internal
.plan_execute_with_status <- function(conns, exec_args, exec_symbol, on_small_site,
                                      context = "Plan execution") {
  servers <- names(conns)
  commit_policy <- if (identical(on_small_site, "skip")) "all_or_none" else "server"
  exec_args <- c(exec_args, list(commit_policy = commit_policy))
  success <- character(0)
  errors <- character(0)
  condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns, symbol = exec_symbol, expr = as.call(exec_args),
      success = function(server) success <<- c(success, server),
      error = function(server, message) errors[[server]] <<- message)
    NULL
  }, error = identity)
  failed <- unique(c(names(errors), setdiff(servers, success)))
  if (!is.null(condition) || length(failed)) {
    reasons <- if (length(errors)) as.list(errors) else
      tryCatch(DSI::datashield.errors(), error = function(e) NULL)
    .plan_revoke_execution(conns, exec_symbol, servers)
    detail <- if (length(reasons)) {
      paste(names(reasons), vapply(reasons, function(x) as.character(x)[[1L]], ""),
            sep = ": ", collapse = "; ")
    } else if (!is.null(condition)) conditionMessage(condition) else paste(failed, collapse = ", ")
    stop(context, " failed; every server's execution was rolled back. Affected server(s): ",
         detail, ".", call. = FALSE)
  }
  statuses <- list()
  for (server in servers) {
    st <- tryCatch({
      res <- DSI::datashield.aggregate(conns[server], call("omopPlanStatusDS", exec_symbol))
      .plan_validate_status(res[[server]] %||% res[[1L]], server)
    }, error = function(e) e)
    if (inherits(st, "error")) {
      .plan_revoke_execution(conns, exec_symbol, servers)
      stop(context, ": the publication status of server '", server, "' could not be read (",
           conditionMessage(st), "); every server's execution was rolled back.", call. = FALSE)
    }
    statuses[[server]] <- st
  }
  technical <- unlist(lapply(servers, function(server) {
    rej <- statuses[[server]]$rejected %||% list()
    bad <- names(rej)[vapply(rej, function(r) !identical(r$code, "insufficient_support"), logical(1))]
    if (length(bad)) paste0(server, ": ", bad, " (", vapply(rej[bad], `[[`, "", "code"), ")")
  }))
  if (length(technical)) {
    .plan_revoke_execution(conns, exec_symbol, servers)
    stop(context, " was refused for reasons other than the minimum number of persons; ",
         "every server's execution was rolled back: ", paste(technical, collapse = "; "), ".",
         call. = FALSE)
  }
  refused <- servers[vapply(statuses, function(s) identical(s$status, "refused"), logical(1))]
  partial <- servers[vapply(statuses, function(s) identical(s$status, "partial"), logical(1))]
  skipped <- character(0)
  if (length(refused)) {
    if (identical(on_small_site, "skip")) {
      skipped <- refused
    } else {
      .plan_revoke_execution(conns, exec_symbol, servers)
      stop(.V2_INSUFFICIENT_MESSAGE, " (server", if (length(refused) > 1L) "s", ": ",
           paste(refused, collapse = ", "), "; every server's execution was rolled back. ",
           "Use on_small_site = \"skip\" to keep the other servers.)", call. = FALSE)
    }
  }
  retained <- setdiff(servers, skipped)
  # Inventory: the same root/component must carry the same symbol everywhere.
  roots <- unique(unlist(lapply(statuses[retained], function(s) names(s$published))))
  mapping <- character(0)
  missing_somewhere <- character(0)
  for (root in roots) {
    comps <- unique(unlist(lapply(statuses[retained], function(s) names(s$published[[root]]))))
    for (comp in comps) {
      syms <- vapply(statuses[retained], function(s) {
        e <- s$published[[root]][[comp]]
        if (is.null(e)) NA_character_ else as.character(e$symbol %||% e$path)
      }, "")
      present <- syms[!is.na(syms)]
      if (length(unique(present)) > 1L) {
        .plan_revoke_execution(conns, exec_symbol, servers)
        stop(context, ": servers published '", root, "' under different names (",
             paste(names(present), present, sep = " = ", collapse = ", "),
             "); every server's execution was rolled back. Choose fresh output names.",
             call. = FALSE)
      }
      key <- if (length(comps) == 1L && comp %in% c(root, "", "result")) root else
        paste0(root, ".", comp)
      if (length(present) < length(retained)) {
        missing_somewhere <- c(missing_somewhere, key)
      } else {
        mapping[[key]] <- unname(present[[1L]])
      }
    }
  }
  for (server in servers) {
    tryCatch(DSI::datashield.rm(conns[server], exec_symbol), error = function(e) NULL)
  }
  if (length(partial) && !identical(on_small_site, "skip")) {
    detail <- paste(vapply(partial, function(server) {
      rej <- statuses[[server]]$rejected
      paste0(server, ": ", paste0(names(rej), " (", vapply(rej, `[[`, "", "code"), ")",
                                  collapse = ", "))
    }, ""), collapse = "; ")
    warning("Partial publication on a non-strict server; rejected families: ", detail,
            ". The other families were published.", call. = FALSE)
  }
  if (length(missing_somewhere)) {
    warning("Not published on every server: ", paste(missing_somewhere, collapse = ", "),
            ".", call. = FALSE)
  }
  if (length(skipped)) {
    if (length(retained) == 0L) {
      warning("Every server was skipped under on_small_site = \"skip\" (",
              .V2_INSUFFICIENT_MESSAGE, ")", call. = FALSE)
    } else {
      message("Skipped server(s) under on_small_site = \"skip\": ",
              paste(skipped, collapse = ", "), ".")
    }
  }
  inventory <- stats::setNames(lapply(servers, function(s) {
    if (s %in% skipped) NULL else statuses[[s]]$published
  }), servers)
  list(mapping = mapping, inventory = inventory, retained = retained, skipped = skipped,
       statuses = statuses)
}

#' Finish an execution under the status protocol (shared by v1 and v2 plans)
#' @keywords internal
.plan_finish_with_status <- function(plan, conns, exec_args, exec_symbol, on_small_site,
                                     output_mode, harmonize_factors, symbol) {
  res <- .plan_execute_with_status(conns, exec_args, exec_symbol, on_small_site)
  if (isTRUE(harmonize_factors) && identical(output_mode, "memory") &&
      !isFALSE(plan$options$factor_concepts) && length(res$retained) > 0L &&
      length(res$mapping) > 0L) {
    .harmonizeConceptFactors(as.list(res$mapping), conns[res$retained])
  }
  if (length(res$mapping)) .record_session_outputs(symbol, as.list(res$mapping))
  invisible(structure(res$mapping, inventory = res$inventory, skipped = res$skipped))
}

#' Execute a v2 plan
#' @keywords internal
.plan_execute_v2 <- function(plan, out = NULL, symbol = "omop", conns = NULL,
                             output_mode = "memory", server_method = "omopPlanExecuteDS",
                             harmonize_factors = TRUE, on_small_site = "error") {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  .v2_assert_supported(plan, session, names(conns), extra = "status.accessor")
  if (identical(output_mode, "staged") && !isFALSE(plan$options$translate_concepts)) {
    # Staged outputs keep numeric OMOP concept IDs (see ds.omop.plan.execute).
    if (is.null(plan$options)) plan$options <- list()
    plan$options$translate_concepts <- FALSE
    attr(plan, "harmonization") <- NULL
    .v2_assert_schema(plan)
  }
  plan <- .plan_v2_federation(plan, symbol, conns)
  roots <- as.character(unlist(plan$publish)) %||% names(plan$outputs)
  if (length(roots) == 0L) roots <- names(plan$outputs)
  out <- .resolve_plan_out(list(outputs = plan$outputs[roots]), out,
                           reserved_symbols = c(symbol, session$res_symbol))
  scope_args <- NULL
  combine <- "union"
  scope_tables <- as.character(unlist(plan$scope$tables %||% list()))
  if (!is.null(plan$scope$cohort) || length(scope_tables) > 0L) {
    scope_args <- .analysis_scope_expr(cohort = plan$scope$cohort,
                                       tables = if (length(scope_tables)) scope_tables)
    combine <- plan$scope$combine %||% "union"
  }
  collisions <- intersect(unname(out), scope_tables)
  if (length(collisions) > 0L) {
    stop("Output symbols collide with execution-scope input symbol(s): ",
         paste(collisions, collapse = ", "), ". Choose different outputs.", call. = FALSE)
  }
  inventory <- .plan_symbol_inventory(conns, "plan output reservation")
  occupied <- unlist(lapply(names(inventory), function(server) {
    found <- intersect(unname(out), inventory[[server]])
    if (length(found) == 0L) character(0) else paste0(server, ":", found)
  }), use.names = FALSE)
  if (length(occupied) > 0L) {
    stop("Plan output destinations must be fresh on every server; occupied: ",
         paste(occupied, collapse = ", "),
         ". Choose different out names or explicitly remove obsolete objects.", call. = FALSE)
  }
  exec_symbol <- NULL
  for (attempt in seq_len(10L)) {
    candidate <- .generate_symbol("dsOexec")
    if (!candidate %in% out &&
        all(!vapply(inventory, function(x) candidate %in% x, logical(1)))) {
      exec_symbol <- candidate
      break
    }
  }
  if (is.null(exec_symbol)) {
    stop("Could not reserve a fresh plan execution status symbol.", call. = FALSE)
  }
  exec_args <- list(as.name(server_method), session$res_symbol, .ds_encode_lossless(plan),
                    .ds_encode(out), output_mode)
  if (!is.null(scope_args)) exec_args <- c(exec_args, scope_args, list(combine = combine))
  .plan_finish_with_status(plan, conns, exec_args, exec_symbol, on_small_site,
                           output_mode, harmonize_factors, symbol)
}

#' Server-side dry run of a v2 plan (validate / preview)
#' @keywords internal
.plan_v2_preview <- function(plan, symbol, conns) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  .v2_assert_supported(plan, session, names(conns))
  plan <- .plan_v2_federation(plan, symbol, conns)
  .ds_safe_aggregate(conns, expr = call("omopPlanPreviewDS", session$res_symbol,
                                        .ds_encode_lossless(plan)))
}
