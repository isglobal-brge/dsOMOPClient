# Module: Recipe language 2.0 -- execution preparation: capability
# negotiation, v1/v2 routing, lossless plan transport and the federation
# contract (design-2.10.3 sections 4.4 and 5). Publication status, the
# on_small_site matrix and rollback are the shared executor's
# (.plan_execute_impl() in R/plan.R, section 7.3).

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
    missing_reducers <- if (length(rl$reducers)) setdiff(reducers, rl$reducers) else character(0)
    c(setdiff(c(ops, extra), rl$operations),
      if (length(missing_reducers)) paste0("reducer ", missing_reducers))
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
#' executes as its unchanged 2.10.1 plan (plan execution itself needs dsOMOP
#' 2.10.3 servers: the publication status methods).
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

#' A v1 refusal of a shape the 2.10.3 language permits
#'
#' The v1 compiler refuses some 2.10.1-style recipes that 2.10.3 newly permits
#' (design 5, corpus class \code{newly_permitted}): NOT filter groups, the
#' \code{covariates_sparse} label (B-22), feature outputs mixing windowed and
#' unwindowed variables (B-19), survival outcomes and interval or
#' temporal-covariate sources narrowed by visit or concept column, interval
#' outputs mixing concept-scoped and unscoped variables of a table, and
#' temporal covariates declaring different analyses per concept. Those
#' refusals carry this condition class; execution then routes the recipe to
#' its language-2 plan. Every other v1 refusal stands.
#' @param ... Message parts.
#' @return A condition object.
#' @keywords internal
.v1_newly_permitted <- function(...) {
  structure(class = c("dsomop_v1_newly_permitted", "error", "condition"),
            list(message = paste0(...), call = NULL))
}

# The v1 plan of a 2.10.1-style recipe, or a "dsomop_no_v1" marker (carrying
# the v1 message) when the v1 compiler refuses a newly permitted shape: the
# recipe then runs as its v2 plan. Other v1 refusals are raised.
.recipe_v1_or_null <- function(recipe) {
  tryCatch(recipe_to_plan(recipe, version = 1L),
           dsomop_v1_newly_permitted = function(e) {
             structure(list(), v1_refusal = conditionMessage(e), class = "dsomop_no_v1")
           })
}

#' The plan a recipe executes as on the connected servers
#'
#' A recipe built only from 2.10.1 constructors executes as its unchanged v1
#' plan (the server's legacy adapters keep its results),
#' unless the v1 compiler refuses one of the shapes 2.10.3 newly permits
#' (\code{.v1_newly_permitted()}). Such a recipe, and any recipe that needs a
#' language-2 construct, executes as its v2 plan where every server speaks it
#' and is refused, naming the reason, otherwise.
#' @keywords internal
.recipe_plan_for_execution <- function(recipe, session, conns) {
  if (length(.recipe_needs_v2(recipe)) == 0L) {
    v1 <- .recipe_v1_or_null(recipe)
    if (!inherits(v1, "dsomop_no_v1")) return(v1)
    plan <- recipe_to_plan(recipe, version = 2L)
    supported <- tryCatch(.v2_assert_supported(plan, session, names(conns)),
                          error = function(e) e)
    if (inherits(supported, "error")) {
      stop(attr(v1, "v1_refusal"), " At least one connected server is older than ",
           "dsOMOP 2.10.3 (", conditionMessage(supported), ")", call. = FALSE)
    }
    return(plan)
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
