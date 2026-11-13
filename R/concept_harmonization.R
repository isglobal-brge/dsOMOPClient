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
  result <- setNames(lapply(columns, function(col) {
    records <- unlist(lapply(per_server, function(x) x$concepts[[col]]), recursive = FALSE)
    resolved <- .conceptResolve(records)
    if (length(resolved$ids) > cap || length(resolved$levels) > cap) {
      stop("Shared concept union exceeds the retained sites' factor-level cap.", call. = FALSE)
    }
    resolved[c("ids", "labels", "levels", "vocabulary")]
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
  list(concept_protocol = 1L, columns = result,
       evidence = unique(unlist(lapply(per_server, function(x) x$evidence), use.names = FALSE)),
       names = if (length(generated)) as.list(.conceptGeneratedNames(keys, generated)) else list())
}
