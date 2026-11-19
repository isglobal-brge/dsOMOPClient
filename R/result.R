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
# aliases without duplicating validation. "pooled" means "both", as the legacy
# scope = "pooled" always kept the per-site results too.
.normalize_result_type <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) ||
      !nzchar(trimws(type))) {
    stop("type must be one of 'split', 'combine', or 'both'.", call. = FALSE)
  }
  value <- tolower(trimws(type))
  aliases <- c(
    split = "split", splits = "split", s = "split", per_site = "split",
    combine = "combine", combined = "combine", c = "combine",
    both = "both", b = "both", pooled = "both"
  )
  resolved <- unname(aliases[value])
  if (length(resolved) != 1L || is.na(resolved)) {
    stop("type must be one of 'split', 'combine', or 'both' ",
         "(aliases: splits/s/per_site, combined/c, b/pooled).",
         call. = FALSE)
  }
  resolved
}

# Resolve a result view while retaining the established meanings of legacy
# `scope` and `pool`. Historically `scope = "pooled"` and `pool = TRUE` kept
# both the site results and the pooled result, hence they map to `both`. A
# `type` that was not supplied (or NULL) takes the legacy argument when one was
# given, otherwise the wrapper's default. `fn` names the caller for the one-time
# alias message.
.resolve_result_type <- function(type = NULL, scope = NULL, pool = NULL,
                                 scope_missing = TRUE, pool_missing = TRUE,
                                 default_type = "split",
                                 type_missing = is.null(type), fn = NULL) {
  legacy <- NULL
  if (!isTRUE(scope_missing)) {
    legacy <- if (identical(scope, "pooled") || identical(scope, "both")) {
      "both"
    } else "split"
    if (!is.null(fn)) .alias_note(fn, "scope", "type")
  } else if (!isTRUE(pool_missing)) {
    legacy <- if (isTRUE(pool)) "both" else "split"
    if (!is.null(fn)) .alias_note(fn, "pool", "type")
  }
  if (isTRUE(type_missing) || is.null(type)) {
    return(legacy %||% .normalize_result_type(default_type))
  }
  resolved <- .normalize_result_type(type)
  if (!is.null(legacy)) {
    compatible <- if (identical(legacy, "both")) {
      resolved %in% c("combine", "both")
    } else {
      identical(resolved, "split")
    }
    if (!compatible) {
      stop("type and legacy ", if (!isTRUE(scope_missing)) "scope" else "pool",
           " request conflicting result views.", call. = FALSE)
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
#
# One rule when pooling is impossible: with a `combine_reason` (an executed
# call, not a dry run) and no pooled value, "both" keeps the per-site results
# with meta$pooling_reason, and "combine" stops with that reason. `pooling`
# (method and estimand) is recorded on every pooled result.
.result_type_view <- function(result, type, combine_reason = NULL,
                              pooling = NULL) {
  if (!inherits(result, "dsomop_result")) {
    stop("result must be a dsomop_result object.", call. = FALSE)
  }
  type <- .normalize_result_type(type)
  meta <- .subset2(result, "meta")
  if (.result_type_wants_combine(type) && is.null(.subset2(result, "pooled")) &&
      !is.null(combine_reason) && length(combine_reason) > 0L) {
    reason <- as.character(combine_reason[[1L]])
    detail <- setdiff(meta$warnings %||% character(0), reason)
    detail <- detail[!grepl("^Server errors: ", detail)]
    full <- if (length(detail) > 0L) {
      paste0(reason, " (", paste(detail, collapse = " "), ")")
    } else reason
    if (identical(type, "combine")) stop(full, call. = FALSE)
    meta$warnings <- unique(c(meta$warnings %||% character(0), reason))
    meta$pooling_reason <- full
  }
  if (.result_type_wants_combine(type) && !is.null(.subset2(result, "pooled")) &&
      !is.null(pooling)) {
    meta$pooling <- pooling
  }
  if (identical(type, "split")) {
    result["pooled"] <- list(NULL)
  } else if (identical(type, "combine")) {
    result[["per_site"]] <- list()
  }
  meta$type <- type
  meta$scope <- if (identical(type, "split")) "per_site" else "pooled"
  result[["meta"]] <- meta
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
#'   \code{warnings} (character vector of pooling warnings). Further
#'   elements, such as \code{pooling} (method and estimand of the pooled
#'   result) and \code{pooling_reason} (why no pooled result was formed),
#'   are kept as given.
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
  # Keep any further metadata (pooling, pooling_reason, protection, ...).
  known <- c("call_code", "timestamp", "servers", "scope", "type",
             "pooling_policy", "warnings")
  for (field in setdiff(names(meta), known)) {
    if (!is.null(meta[[field]])) obj$meta[[field]] <- meta[[field]]
  }
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
  # With one server, a pooled result equal to its answer is not repeated.
  same <- !is.null(pooled) && length(ps) == 1L && is.null(setdiff(oldClass(ps), "list")) &&
    isTRUE(all.equal(pooled, ps[[1L]], check.attributes = FALSE))

  cat("<dsomop_result>\n")
  cat("Servers: ", if (length(servers) > 0L) {
    paste(servers, collapse = ", ")
  } else {
    "<none>"
  }, "\n", sep = "")
  type <- meta$type %||% if (identical(meta$scope, "pooled")) "both" else
    "split"
  cat("Type: ", type, " (", switch(type,
    split = "per-server results",
    combine = "pooled result",
    both = "per-server and pooled results",
    type), ")\n", sep = "")
  if (is.list(meta$pooling) && !is.null(pooled) && !same) {
    cat("Pooling: ", meta$pooling$method,
        if (!is.null(meta$pooling$estimand)) paste0(" (", meta$pooling$estimand, ")"),
        "\n", sep = "")
  }
  if (!is.null(meta$pooling_reason) && is.null(pooled)) {
    cat("Not pooled: ", meta$pooling_reason, "\n", sep = "")
    warns <- setdiff(warns, meta$pooling_reason)
  }
  cat("\n")

  # A per-site object with its own print method (e.g. DP status) prints itself.
  ps_class <- setdiff(oldClass(ps), "list")
  if (length(ps_class) > 0L && length(ps) > 0L) {
    print(ps, ...)
    cat("\n")
  } else {
    for (nm in names(ps)) {
      cat("$", nm, "\n", sep = "")
      print(ps[[nm]], ...)
      cat("\n")
    }
  }
  if (same) {
    cat("$pooled: the same as $", names(ps)[[1L]], " (one server)\n\n", sep = "")
  } else if (!is.null(pooled)) {
    cat("$pooled\n")
    print(pooled, ...)
    cat("\n")
  }
  .print_protection(meta$protection)
  if (length(warns) > 0) {
    cat("Warnings:\n")
    for (w in warns) cat("  - ", w, "\n", sep = "")
  }
  invisible(x)
}

# Print how each server protected its result (dsOMOP >= 2.9.0 servers).
.print_protection <- function(protection) {
  if (!is.list(protection) || length(protection) == 0L) return(invisible())
  describe <- function(p) {
    kind <- switch(p$protection,
      dp = paste0("differential privacy (epsilon ", format(p$epsilon),
                  ", sensitivity ", format(p$sensitivity), " person)"),
      # Only a fixed synopsis declares a delta (0, before the exact support
      # gates); heuristic releases are never described with one.
      fixed_synopsis = paste0(
        "fixed-synopsis protection (declared vector epsilon ",
        format(p$mechanism_epsilon),
        if (identical(suppressWarnings(as.numeric(p$delta)), 0)) {
          ", delta 0"
        } else "",
        "; reference calibration ", format(p$noise_reference_epsilon), ")"),
      realised_sticky = paste0(
        "heuristic realised-contribution protection (reference calibration ",
        format(p$noise_reference_epsilon), "; no joint DP guarantee)"),
      mixed = if (is.list(p$blocks) && length(p$blocks) > 0L) {
        paste0("mixed: ", paste(names(p$blocks), unlist(p$blocks),
                                sep = " ", collapse = ", "))
      } else "mixed: standard binary rows with protected distribution rows",
      standard_fallback = "standard controls (DP unavailable on the server)",
      standard = "standard DataSHIELD controls",
      p$protection
    )
    if (length(p$masked_fields)) {
      kind <- paste0(kind, "; unavailable: ",
                     paste(p$masked_fields, collapse = ", "),
                     " (", p$masked_reason %||% "protected_output_required",
                     ")")
    }
    kind
  }
  cat("Protection:\n")
  kinds <- vapply(protection, describe, character(1))
  if (length(unique(kinds)) == 1L) {
    cat("  all servers: ", kinds[[1L]], "\n", sep = "")
  } else {
    for (nm in names(kinds)) cat("  ", nm, ": ", kinds[[nm]], "\n", sep = "")
  }
  estimands <- unique(unlist(lapply(protection, `[[`, "estimand")))
  for (e in estimands) cat("  Estimand: ", e, "\n", sep = "")
  notes <- unique(unlist(lapply(protection, `[[`, "note")))
  for (n in notes) cat("  ", n, "\n", sep = "")
  if (length(unique(vapply(protection, `[[`, "", "protection"))) > 1L) {
    cat("  Servers differ in protection; pooled values mix them.\n")
  }
  cat("\n")
  invisible()
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

#' @rdname cash-.dsomop_result
#' @param i Element name or position, as for \code{[[}.
#' @param exact Passed to \code{[[}.
#' @export
`[[.dsomop_result` <- function(x, i, exact = TRUE) {
  if (is.character(i) && length(i) == 1L &&
      !i %in% c("per_site", "pooled", "meta")) {
    ps <- .subset2(x, "per_site")
    if (i %in% names(ps)) return(.subset2(ps, i))
  }
  .subset2(x, i, exact = exact)
}

#' Convert dsomop_result to data.frame
#'
#' Extracts one data frame from a \code{dsomop_result} object. A split result
#' (\code{type = "split"}) stacks the per-server data frames into one frame
#' with a leading \code{server} column, so no site is dropped silently.
#' A \code{"combine"} or \code{"both"} result returns its pooled data frame;
#' when no pooled value could be formed, it returns an empty
#' \code{data.frame()} so a failed strict federation never degrades to a
#' partial view (use \code{type = "split"} or \code{$per_site} for the
#' per-server results).
#'
#' @param x A \code{dsomop_result} object.
#' @param ... Additional arguments (ignored).
#' @return A data frame: the pooled result, the stacked per-server results
#'   with a \code{server} column for split results, or an empty data frame.
#' @examples
#' \dontrun{
#' res <- ds.omop.achilles.results(analysis_ids = 1, type = "both")
#' df <- as.data.frame(res)
#' head(df)
#' }
#' @export
as.data.frame.dsomop_result <- function(x, ...) {
  pooled <- .subset2(x, "pooled")
  if (!is.null(pooled) && is.data.frame(pooled)) {
    return(pooled)
  }
  meta <- .subset2(x, "meta")
  type <- meta$type %||% if (identical(meta$scope, "pooled")) "both" else
    "split"
  if (!identical(type, "split")) return(data.frame())
  .stack_per_site(.subset2(x, "per_site"))
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
