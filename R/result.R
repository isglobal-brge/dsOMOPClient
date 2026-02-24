# ==============================================================================
# dsOMOPClient v2 - dsomop_result S3 class
# ==============================================================================
# Wraps every function return with per-site + pooled results, reproducible
# code, and structured metadata.
# ==============================================================================

#' Create a dsomop_result object
#'
#' @param per_site Named list: server_name -> raw result
#' @param pooled NULL or single aggregated result
#' @param meta List of metadata (call_code, scope, pooling_policy, warnings)
#' @return A dsomop_result object
#' @keywords internal
dsomop_result <- function(per_site, pooled = NULL, meta = list()) {
  obj <- list(
    per_site = per_site,
    pooled   = pooled,
    meta     = list(
      call_code      = meta$call_code %||% "",
      timestamp      = Sys.time(),
      servers        = names(per_site),
      scope          = meta$scope %||% "per_site",
      pooling_policy = meta$pooling_policy %||% "strict",
      warnings       = meta$warnings %||% character(0)
    )
  )
  class(obj) <- c("dsomop_result", "list")
  obj
}

#' Print a dsomop_result
#'
#' @param x A dsomop_result object
#' @param ... Ignored
#' @export
print.dsomop_result <- function(x, ...) {
  cat("dsomop_result\n")
  cat("  Servers:", paste(x$meta$servers, collapse = ", ") %||% "(none)", "\n")
  cat("  Scope:  ", x$meta$scope, "\n")
  if (!is.null(x$pooled)) {
    cat("  Pooled:  yes\n")
  } else {
    cat("  Pooled:  no\n")
  }
  if (length(x$meta$warnings) > 0) {
    cat("  Warnings:", length(x$meta$warnings), "\n")
    for (w in x$meta$warnings) cat("    - ", w, "\n")
  }
  if (nchar(x$meta$call_code) > 0) {
    cat("  Code:   ", substr(x$meta$call_code, 1, 80),
        if (nchar(x$meta$call_code) > 80) "..." else "", "\n")
  }
  invisible(x)
}

#' Access dsomop_result elements with backward compatibility
#'
#' If name is not a top-level field (per_site, pooled, meta), falls through
#' to per_site for backward compatibility with code like result$server_a.
#'
#' @param x A dsomop_result object
#' @param name Element name
#' @return The requested element
#' @export
`$.dsomop_result` <- function(x, name) {
  if (name %in% c("per_site", "pooled", "meta")) return(.subset2(x, name))
  # Fall through to per_site for backward compat
  ps <- .subset2(x, "per_site")
  if (name %in% names(ps)) return(ps[[name]])
  .subset2(x, name)
}

#' Convert dsomop_result to data.frame
#'
#' Returns pooled result if available, otherwise the first server's result.
#'
#' @param x A dsomop_result object
#' @param ... Ignored
#' @return A data.frame
#' @export
as.data.frame.dsomop_result <- function(x, ...) {
  if (!is.null(x$pooled) && is.data.frame(x$pooled)) {
    return(x$pooled)
  }
  ps <- x$per_site
  if (length(ps) > 0) {
    first <- ps[[1]]
    if (is.data.frame(first)) return(first)
  }
  data.frame()
}

#' Extract reproducible R code from a dsomop_result
#'
#' @param x A dsomop_result object
#' @return Character string of R code
#' @export
ds.omop.code <- function(x) {
  if (!inherits(x, "dsomop_result")) {
    stop("ds.omop.code() requires a dsomop_result object", call. = FALSE)
  }
  x$meta$call_code
}

#' Copy reproducible R code to clipboard
#'
#' @param x A dsomop_result object
#' @return Invisibly returns the code string
#' @export
ds.omop.copy_code <- function(x) {
  code <- ds.omop.code(x)
  tryCatch({
    if (requireNamespace("clipr", quietly = TRUE)) {
      clipr::write_clip(code)
      message("Code copied to clipboard.")
    } else {
      message("Install the 'clipr' package for clipboard support.")
      message("Code:\n", code)
    }
  }, error = function(e) {
    message("Could not copy to clipboard: ", conditionMessage(e))
    message("Code:\n", code)
  })
  invisible(code)
}
