# Module: Recipe language 2.0 -- normalisation, semantic digest and the
# operation inventory used for capability negotiation (design-2.10.3 sections
# 4.1, 4.4, 6.1 and 6.15).
#
# .planNormalizeV2() expands every default exactly once, so that two plans that
# mean the same thing have the same normal form: (1) the `default` keywords of
# the vendored plan-v2.json, applied in the schema branch the instance matches
# (oneOf / anyOf / if-then-else); (2) the semantic defaults the schema states in
# prose: `options` and empty registries are present, an absent `publish` means
# every output (in declaration order), a population without `coverage` gets the
# plan-level coverage or the new indexed default unique/drop/drop (on_missing =
# "keep" when restrict_to_observation is FALSE), an output without `grain` gets
# "episode" over an indexed population and "person" otherwise, a wide output
# without `columns` takes every variable in declaration order, and a survival
# output without event_order / outcome_mode gets the format's default. The
# normal form is what explain renders and what the semantic digest hashes.

#' Apply a JSON Schema's `default` keywords to an instance
#'
#' @param schema Parsed schema node.
#' @param x Instance (R value as parsed with \code{simplifyVector = FALSE}).
#' @param root Root schema for \code{$ref} resolution.
#' @return The instance with every absent defaulted property present.
#' @keywords internal
.jsonApplyDefaults <- function(schema, x, root) {
  if (is.logical(schema) || !is.list(schema)) return(x)
  if (!is.null(schema[["$ref"]])) {
    x <- .jsonApplyDefaults(.jsonSchemaResolveRef(schema[["$ref"]], root), x, root)
  }
  type <- tryCatch(.jsonInstanceType(x), error = function(e) "unsupported")
  if (identical(type, "object")) {
    props <- schema[["properties"]]
    if (!is.null(props)) {
      for (nm in names(props)) {
        if (!nm %in% names(x) && is.list(props[[nm]]) && "default" %in% names(props[[nm]])) {
          x[nm] <- list(props[[nm]][["default"]])
        }
      }
      for (nm in intersect(names(props), names(x))) {
        x[nm] <- list(.jsonApplyDefaults(props[[nm]], x[[nm]], root))
      }
    }
    ap <- schema[["additionalProperties"]]
    if (is.list(ap)) {
      for (nm in setdiff(names(x), names(props))) {
        x[nm] <- list(.jsonApplyDefaults(ap, x[[nm]], root))
      }
    }
  }
  if (identical(type, "array") && !is.null(schema[["items"]])) {
    x <- lapply(.jsonArrayItems(x), function(item) .jsonApplyDefaults(schema[["items"]], item, root))
  }
  for (s in .jsonArrayItems(schema[["allOf"]] %||% list())) {
    x <- .jsonApplyDefaults(s, x, root)
  }
  for (key in c("oneOf", "anyOf")) {
    branches <- .jsonArrayItems(schema[[key]] %||% list())
    if (length(branches)) {
      hits <- vapply(branches, function(s) .jsv_quiet(s, x, root, ""), logical(1))
      if (any(hits)) x <- .jsonApplyDefaults(branches[[which(hits)[[1L]]]], x, root)
    }
  }
  if (!is.null(schema[["if"]])) {
    branch <- if (.jsv_quiet(schema[["if"]], x, root, "")) "then" else "else"
    if (!is.null(schema[[branch]])) x <- .jsonApplyDefaults(schema[[branch]], x, root)
  }
  x
}

.V2_DEFAULT_COVERAGE <- list(policy = "unique", on_ambiguous = "drop", on_missing = "drop", gap = 0L)

#' Normalise a v2 plan: every default expanded exactly once
#'
#' @param plan A v2 plan (\code{omop_plan_v2} or a parsed plan list).
#' @return The normalised plan as a plain list (no S3 classes).
#' @keywords internal
.planNormalizeV2 <- function(plan) {
  x <- .v2_plain(plan)
  if (!identical(as.integer(x$version %||% 0L), 2L)) {
    stop("Only v2 plans are normalised on the client.", call. = FALSE)
  }
  empty <- .v2_empty_object()
  if (is.null(x$options)) x$options <- empty
  for (slot in c("concept_sets", "events", "intervals", "populations", "variables")) {
    if (is.null(x[[slot]])) x[[slot]] <- empty
  }
  x <- .jsonApplyDefaults(.v2_plan_schema(), x, .v2_plan_schema())
  if (is.null(x$publish)) x$publish <- .v2_array(names(x$outputs))
  plan_cov <- x$options$coverage
  for (id in names(x$populations)) {
    p <- x$populations[[id]]
    if (p$kind %in% c("index", "criteria") && is.null(p$coverage)) {
      cov <- plan_cov %||% .V2_DEFAULT_COVERAGE
      if (identical(p$kind, "index") && isFALSE(p$index$restrict_to_observation)) {
        cov$on_missing <- "keep"
      }
      p$coverage <- cov
      x$populations[[id]] <- p
    }
  }
  for (nm in names(x$outputs)) {
    o <- x$outputs[[nm]]
    if (is.null(o$grain)) {
      o$grain <- if (.v2_population_is_indexed(x$populations, o$rows)) "episode" else "person"
    }
    if (identical(o$format, "wide") && is.null(o$columns)) {
      o$columns <- .v2_array(names(x$variables))
    }
    if (identical(o$format, "survival")) {
      f <- o$options$format %||% "survival"
      if (is.null(o$options$event_order)) o$options$event_order <- .v2_default_event_order(f)
      if (is.null(o$options$outcome_mode)) {
        o$options$outcome_mode <- if (identical(f, "survival")) "composite" else "named"
      }
    }
    x$outputs[[nm]] <- o
  }
  x
}

#' Semantic digest of a v2 plan (normalised first)
#'
#' The digest binds the federation harmonisation signature and the DP
#' provenance capsule: SHA-256 of the RFC 8785 serialisation of the normalised
#' plan without its advisory fields (labels, result symbols, generator
#' metadata), computed by the shared \code{.planSemanticDigest()}.
#'
#' @param plan A v2 plan.
#' @return A list with \code{payload} and \code{sha256}.
#' @keywords internal
.plan_v2_digest <- function(plan) {
  .planSemanticDigest(.planNormalizeV2(plan))
}

# --- operation inventory (section 4.4) ---------------------------------------------

.v2_walk <- function(x, fn) {
  fn(x)
  if (is.list(x)) for (el in x) .v2_walk(el, fn)
  invisible(NULL)
}

#' Language operations a v2 plan needs from a server
#'
#' @param plan A v2 plan.
#' @return A sorted character vector of operation names (design section 4.4)
#'   and, as attribute \code{"reducers"}, the reducer functions used.
#' @keywords internal
.plan_v2_operations <- function(plan) {
  x <- .v2_plain(plan)
  ops <- character(0)
  add <- function(...) ops <<- c(ops, ...)
  inline_cs <- FALSE
  for (ev in x$events %||% list()) {
    if (!is.null(ev$episode_members)) {
      add("event.episode_members")
      if (identical(ev$episode_members$on_missing_table, "unavailable")) add("event.on_missing_table")
      next
    }
    if (!is.null(ev$cost)) {
      add("event.cost")
      if (identical(ev$cost$on_missing_table, "unavailable")) add("event.on_missing_table")
      next
    }
    add("event")
    if (is.list(ev$concept_set)) inline_cs <- TRUE
    if (!is.null(ev$unit)) add("event.unit")
    if (identical(ev$on_missing_table, "unavailable")) add("event.on_missing_table")
  }
  if (length(x$concept_sets %||% list()) || inline_cs) add("concept_set")
  for (iv in x$intervals %||% list()) {
    add("interval")
    if (identical(iv$by, "ingredient")) add("interval.ingredient")
    if (isTRUE(iv$stockpile)) add("interval.stockpile")
  }
  if (identical(x$options$coverage$policy, "union")) add("population.coverage.union")
  for (p in x$populations %||% list()) {
    add(paste0("population.", p$kind))
    if (!is.null(p$parent)) add("population.parent")
    if (identical(p$index$select, "first_eligible")) add("population.first_eligible")
    if (!is.null(p$index$source$any_of)) add("population.any_of")
    if (identical(p$coverage$policy, "union")) add("population.coverage.union")
  }
  ops <- sub("^population\\.match$", "population.match", ops)
  reducers <- character(0)
  .v2_walk(x$populations, function(node) {
    if (is.list(node) && !is.null(names(node))) {
      if ("not" %in% names(node) && length(node) == 1L) add("criteria.not")
      if ("age" %in% names(node) && length(node) == 1L) add("criteria.age")
    }
  })
  .v2_walk(x, function(node) {
    if (is.list(node) && !is.null(names(node)) && "source" %in% names(node) &&
        "which" %in% names(node) && any(c("field", "scope") %in% names(node))) {
      add("window.anchor.source")
    }
  })
  for (v in x$variables %||% list()) {
    if (!is.null(v$reducer)) {
      add("variable.reducer")
      reducers <- c(reducers, v$reducer$fn)
      if (identical(v$reducer$fn, "supply_days")) add("reducer.supply_days")
      if (identical(v$reducer$fn, "covered_days") &&
          identical(v$reducer$coverage, "effective_supply")) {
        add("reducer.covered_days.effective_supply")
      }
    }
    if (!is.null(v$derived)) add("variable.derived")
    if (!is.null(v$raw)) add("variable.raw")
    if (!is.null(v$population_column)) add("variable.population_column")
  }
  for (o in x$outputs %||% list()) {
    add(paste0("output.", o$format))
    if (identical(o$format, "long") && isTRUE(o$options$event_log)) add("output.long.event_log")
    if (identical(o$format, "survival")) {
      if (any(vapply(o$options$censor %||% list(), is.list, logical(1)))) {
        add("output.survival.censor_source")
      }
    }
    if (identical(o$format, "person_period") && length(o$options$partition %||% list())) {
      add("output.person_period.partition")
    }
  }
  if (!is.null(x$publish)) add("publish.roots")
  ops <- sort(unique(ops))
  attr(ops, "reducers") <- sort(unique(reducers))
  ops
}
