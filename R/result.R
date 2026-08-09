# Module: Result Objects
# dsomop_result construction, printing, and utility functions.

# Disclosure: never surface suppressed rows. A row flagged `suppressed` reveals
# that a rare concept/value EXISTS (with too few persons) -- which is itself
# disclosive. Drop such rows entirely (indistinguishable from absent) and remove
# the now-redundant column. Only data.frames carrying a `suppressed` column are
# touched; list/scalar results (e.g. table-stat suppression flags) are left
# as-is.
.hide_suppressed <- function(x) {
  if (is.data.frame(x) && "suppressed" %in% names(x)) {
    x <- x[!(x$suppressed %in% TRUE), , drop = FALSE]
    x$suppressed <- NULL
    rownames(x) <- NULL
  }
  x
}

# Normalise the dsBaseClient-style result-view vocabulary used by aggregate
# wrappers. Keep this internal so every public surface accepts exactly the same
# aliases without duplicating validation.
.normalize_result_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !nzchar(trimws(type))) {
    stop("type must be one of 'split', 'combine', or 'both'.", call. = FALSE)
  }
  value <- tolower(trimws(type))
  aliases <- c(
    split = "split", splits = "split", s = "split", per_site = "split",
    combine = "combine", combined = "combine", c = "combine",
    pooled = "combine", both = "both", b = "both"
  )
  resolved <- unname(aliases[value])
  if (length(resolved) != 1L || is.na(resolved)) {
    stop("type must be one of 'split', 'combine', or 'both' ",
         "(aliases: splits/s/per_site, combined/c/pooled, b).",
         call. = FALSE)
  }
  resolved
}

# Resolve a new result view while retaining the established meanings of legacy
# `scope` and `pool`. Historically `scope = "pooled"` and `pool = TRUE` kept
# both the site results and the pooled result, hence they map to `both`.
.resolve_result_type <- function(type = NULL, scope = NULL, pool = NULL,
                                 scope_missing = TRUE, pool_missing = TRUE,
                                 default_type = "split") {
  resolved <- if (is.null(type)) {
    if (!isTRUE(scope_missing)) {
      if (identical(scope, "pooled")) "both" else "split"
    } else if (!isTRUE(pool_missing)) {
      if (isTRUE(pool)) "both" else "split"
    } else {
      .normalize_result_type(default_type)
    }
  } else {
    .normalize_result_type(type)
  }

  if (!is.null(type) && !isTRUE(scope_missing)) {
    compatible <- if (identical(scope, "pooled")) {
      resolved %in% c("combine", "both")
    } else {
      identical(resolved, "split")
    }
    if (!compatible) {
      stop("type and legacy scope request conflicting result views.",
           call. = FALSE)
    }
  }
  if (!is.null(type) && !isTRUE(pool_missing)) {
    compatible <- if (isTRUE(pool)) {
      resolved %in% c("combine", "both")
    } else {
      identical(resolved, "split")
    }
    if (!compatible) {
      stop("type and legacy pool request conflicting result views.",
           call. = FALSE)
    }
  }
  resolved
}

.result_type_wants_combine <- function(type) {
  type %in% c("combine", "both")
}

# Apply a result view only after dsomop_result() has captured server names and
# aggregate errors. This deliberately removes a view; it never derives or
# materialises individual-level data on the client.
.result_type_view <- function(result, type, combine_reason = NULL) {
  if (!inherits(result, "dsomop_result")) {
    stop("result must be a dsomop_result object.", call. = FALSE)
  }
  type <- .normalize_result_type(type)
  if (.result_type_wants_combine(type) && is.null(result[["pooled"]]) &&
      !is.null(combine_reason) && length(combine_reason) > 0L) {
    reason <- as.character(combine_reason[[1L]])
    result[["meta"]]$warnings <- unique(c(
      result[["meta"]]$warnings %||% character(0), reason
    ))
    result[["meta"]]$pooling_reason <- reason
  }
  if (identical(type, "split")) {
    result["pooled"] <- list(NULL)
  } else if (identical(type, "combine")) {
    result[["per_site"]] <- list()
  }
  result[["meta"]]$type <- type
  result[["meta"]]$scope <- if (identical(type, "split")) {
    "per_site"
  } else {
    "pooled"
  }
  result
}

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
#'   \code{type} (character; \code{"split"}, \code{"combine"}, or
#'   \code{"both"}),
#'   \code{pooling_policy} (character; \code{"strict"} or
#'   \code{"pooled_only_ok"}),
#'   \code{warnings} (character vector of pooling warnings).
#' @return A \code{dsomop_result} object (a list with class
#'   \code{c("dsomop_result", "list")}).
#' @keywords internal
dsomop_result <- function(per_site, pooled = NULL, meta = list()) {
  # Drop suppressed rows/column from every per-site + pooled table, but PRESERVE
  # list attributes -- lapply() drops them, and per_site carries a `ds_errors`
  # attribute with per-server error messages that must survive.
  ps_attrs <- attributes(per_site)
  ds_errors <- attr(per_site, "ds_errors")
  error_warnings <- if (length(ds_errors) > 0L) {
    paste0("Server errors: ", paste(
      names(ds_errors), unlist(ds_errors, use.names = FALSE),
      sep = ": ", collapse = "; "
    ))
  } else character(0)
  per_site <- lapply(per_site, .hide_suppressed)
  for (a in setdiff(names(ps_attrs), "names")) attr(per_site, a) <- ps_attrs[[a]]
  pooled   <- .hide_suppressed(pooled)
  obj <- list(
    per_site = per_site,
    pooled   = pooled,
    meta     = list(
      call_code      = meta$call_code %||% "",
      timestamp      = Sys.time(),
      servers        = meta$servers %||% names(per_site),
      scope          = meta$scope %||% "per_site",
      type           = meta$type %||% if (identical(meta$scope, "pooled")) {
        "both"
      } else {
        "split"
      },
      pooling_policy = meta$pooling_policy %||% "strict",
      warnings       = unique(c(meta$warnings %||% character(0),
                                error_warnings))
    )
  )
  class(obj) <- c("dsomop_result", "list")
  obj
}

#' Print a dsomop_result
#'
#' Prints the per-site result tables and the pooled (cross-server) result
#' table, followed by any disclosure/pooling warnings.
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
  ps     <- .subset2(x, "per_site")
  pooled <- .subset2(x, "pooled")
  meta   <- .subset2(x, "meta")
  warns  <- meta$warnings
  servers <- meta$servers %||% names(ps)

  cat("<dsomop_result>\n")
  cat("Servers: ", if (length(servers) > 0L) {
    paste(servers, collapse = ", ")
  } else {
    "<none>"
  }, "\n", sep = "")
  cat("Scope: ", meta$scope %||% "per_site", "\n\n", sep = "")

  for (nm in names(ps)) {
    cat("$", nm, "\n", sep = "")
    print(ps[[nm]], ...)
    cat("\n")
  }
  if (!is.null(pooled)) {
    cat("$pooled\n")
    print(pooled, ...)
    cat("\n")
  }
  if (length(warns) > 0) {
    cat("Warnings:\n")
    for (w in warns) cat("  - ", w, "\n", sep = "")
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
#' the first server's result is used only for a per-site result. A pooled-scope
#' result whose pooled value is \code{NULL} returns an empty
#' \code{data.frame()} so a failed strict federation cannot silently degrade to
#' one server. Returns an empty \code{data.frame()} if no valid data frame is
#' found.
#'
#' @param x A \code{dsomop_result} object.
#' @param ... Additional arguments (ignored).
#' @return A data frame: the pooled result, the first server's result for
#'   per-site scope, or an empty data frame as fallback.
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
  if (identical(x$meta$scope, "pooled")) return(data.frame())
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
