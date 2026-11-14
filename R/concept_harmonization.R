# Uses only omopFactorLevelsDS metadata from the retained sites of a component.
.conceptUnionReports <- function(per_server) {
  if (any(vapply(per_server, function(x) !identical(as.integer(x$concept_protocol), 1L), logical(1)))) {
    stop("Incomplete concept presentation protocol across retained sites.", call. = FALSE)
  }
  if (any(vapply(per_server, function(x) length(x$unsafe) > 0L, logical(1)))) {
    stop("Concept reconciliation refused by the existing factor-level disclosure policy.", call. = FALSE)
  }
  columns <- names(per_server[[1L]]$concepts)
  if (any(!vapply(per_server, function(x) setequal(names(x$concepts), columns), logical(1)))) {
    stop("Incompatible concept-column schemas across retained sites.", call. = FALSE)
  }
  cap <- min(vapply(per_server, function(x) x$nfilter_levels_max, 0))
  explanation <- character()
  result <- setNames(lapply(columns, function(col) {
    records <- unlist(lapply(per_server, function(x) x$concepts[[col]]), recursive = FALSE)
    calendars <- lapply(per_server, function(x) x$calendars[[col]])
    if (any(!vapply(calendars, is.null, logical(1)))) {
      if (any(vapply(calendars, is.null, logical(1))) || length(unique(vapply(calendars, .jcsCanonical, ""))) != 1L) .dateReject("Calendar/policy mismatch across retained sites.")
      resolved <- .dateResolveRecords(records, calendars[[1L]])
    } else resolved <- .conceptResolve(records)
    if (length(resolved$ids) > cap || length(resolved$levels) > cap) {
      stop("Shared concept union exceeds the retained sites' factor-level cap.", call. = FALSE)
    }
    for (decision in resolved$decisions) {
      candidates <- sort(unique(vapply(Filter(function(x) identical(x$id,decision$id) && !is.null(x$name), records),
        function(x) x$name, "")), method="radix")
      explanation <<- c(explanation, paste0(col, ": ", decision$id, " -> ", decision$label,
        if (length(candidates)>1L) paste0("; vocabulary conflict: ",paste(candidates,collapse=" / ")),
        if (!is.null(decision$version)) paste0("; version ",decision$version), "; ",decision$rule))
    }
    shared <- resolved[intersect(names(resolved), c("ids", "labels", "levels", "vocabulary", "kind", "calendar"))]
    naming_ids <- unique(unlist(lapply(per_server,function(x) x$reference_names[[col]]),use.names=FALSE))
    if (length(naming_ids)) shared$naming_ids <- naming_ids[.conceptIdOrder(naming_ids)]
    shared
  }), columns)
  keys <- per_server[[1L]]$keys
  if (any(!vapply(per_server, function(x) setequal(x$keys, keys), logical(1)))) {
    stop("Incompatible component schemas; presentation cannot add missing feature columns.", call. = FALSE)
  }
  generated <- per_server[[1L]]$generated %||% list()
  if (any(!vapply(per_server, function(x) setequal(names(x$generated), names(generated)), logical(1)))) {
    stop("Incomplete generated-column identity contract.", call. = FALSE)
  }
  for (key in names(generated)) {
    rows <- lapply(per_server, function(x) x$generated[[key]])
    identity <- lapply(rows, function(g) g[c("id", "prefix", "suffix", "owner", "explicit")])
    if (any(!vapply(identity, identical, logical(1), identity[[1L]]))) {
      stop("Inconsistent generated-column identity or explicit name.", call. = FALSE)
    }
    generated[[key]]$candidates <- unlist(lapply(rows, function(g) g$candidates), recursive = FALSE)
  }
  for (key in names(generated)) {
    choice <- .conceptChooseName(generated[[key]]$candidates)
    explanation <- c(explanation, paste0("Generated key ",key,": ",choice$name %||% generated[[key]]$id,
      if (!is.null(choice$version)) paste0("; version ",choice$version),"; ",choice$rule))
  }
  spec <- list(concept_protocol = 1L, columns = result,
       evidence = unique(unlist(lapply(per_server, function(x) x$evidence), use.names = FALSE)),
       names = if (length(generated)) as.list(.conceptGeneratedNames(keys, generated)) else list())
  attr(spec,"concept_explain") <- explanation
  spec
}

# This receipt is display-only client metadata from a completed publication.
# It is never consulted by planning, validation, support checks or execution.
.conceptPlanKey <- function(plan) {
  if (.v2_is_plan_v2(plan)) .planNormalizedDigest(plan)$sha256 else .plan_harmonization_signature(plan)
}

.conceptPublishedExplain <- function(session, plan, conns) {
  receipt <- session$concept_presentation
  if (is.null(receipt) || !length(receipt$components) || !identical(receipt$key,.conceptPlanKey(plan)) ||
      !setequal(receipt$sites,names(conns))) return(character())
  lines <- "Concept resolutions from the last successful publication (not a new presence query):"
  for (component in names(receipt$components)) {
    lines <- c(lines,paste0(component,": ",receipt$components[[component]]))
  }
  lines
}
