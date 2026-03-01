# Module: Result Objects
# dsomop_result construction, printing, and utility functions.

#' Create a dsomop_result object
#'
#' Constructs a standardised \code{dsomop_result} S3 object that wraps every
#' dsOMOPClient function return value. The object stores per-site results,
#' an optional pooled (cross-server aggregated) result, and metadata
#' including the reproducible R code that produced the result, the timestamp,
#' and any pooling warnings.
#'
#' @param per_site Named list mapping server names to their raw results
#'   (data frames, lists, or scalars).
#' @param pooled \code{NULL} (default) or a single aggregated result
#'   (typically a data frame) combining all servers.
#' @param meta Named list of metadata. Recognised elements:
#'   \code{call_code} (character; reproducible R code),
#'   \code{scope} (character; \code{"per_site"} or \code{"pooled"}),
#'   \code{pooling_policy} (character; \code{"strict"} or
#'   \code{"pooled_only_ok"}),
#'   \code{warnings} (character vector of pooling warnings).
#' @return A \code{dsomop_result} object (a list with class
#'   \code{c("dsomop_result", "list")}).
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
#' Displays a compact summary of a \code{dsomop_result} object, including
#' the list of servers, scope, pooling status, any warnings, and a truncated
#' preview of the reproducible R code.
#'
#' @param x A \code{dsomop_result} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.status()
#' print(res)
#' }
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
#' Custom \code{$} operator for \code{dsomop_result} objects. Top-level
#' fields (\code{per_site}, \code{pooled}, \code{meta}) are returned
#' directly. Any other name falls through to the \code{per_site} list,
#' allowing backward-compatible access patterns such as
#' \code{result$server_a} instead of \code{result$per_site$server_a}.
#'
#' @param x A \code{dsomop_result} object.
#' @param name Character; the element name to access.
#' @return The requested element: a top-level field, or the matching entry
#'   from \code{per_site}, or \code{NULL} if not found.
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.status()
#' res$per_site          # top-level access
#' res$server_a          # falls through to per_site[["server_a"]]
#' }
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
#' Extracts a single data frame from a \code{dsomop_result} object. If a
#' pooled result is available and is a data frame, it is returned. Otherwise,
#' the first server's result is used. Returns an empty \code{data.frame()}
#' if no valid data frame is found.
#'
#' @param x A \code{dsomop_result} object.
#' @param ... Additional arguments (ignored).
#' @return A data frame: the pooled result, the first server's result, or
#'   an empty data frame as fallback.
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.results(analysis_ids = 1, scope = "pooled")
#' df <- as.data.frame(res)
#' head(df)
#' }
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

#' Get the R code that produced a result
#'
#' Extracts the stored R code string from a \code{dsomop_result} object,
#' which can be used to reproduce the analysis. The code is captured
#' automatically when each client wrapper function is called.
#'
#' @param x A \code{dsomop_result} object.
#' @return Character string containing the reproducible R code. Returns an
#'   empty string if no code was captured.
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.results(analysis_ids = 1)
#' ds.omop.code(res)
#' }
#' @export
ds.omop.code <- function(x) {
  if (!inherits(x, "dsomop_result")) {
    stop("ds.omop.code() requires a dsomop_result object", call. = FALSE)
  }
  x$meta$call_code
}

#' Copy reproducible R code to clipboard
#'
#' Extracts the stored R code from a \code{dsomop_result} object and copies
#' it to the system clipboard using the \pkg{clipr} package. If \pkg{clipr}
#' is not installed or clipboard access fails, the code is printed to the
#' console instead.
#'
#' @param x A \code{dsomop_result} object.
#' @return Invisibly returns the code string (character).
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.results(analysis_ids = 1)
#' ds.omop.copy_code(res)  # copies to clipboard
#' }
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
