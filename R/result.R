# Module: Result Objects
# dsomop_result construction, printing, and utility functions.

# A missing released field is unknown, never zero. Indexing preserves Date,
# integer, factor and other atomic column types, including empty prototypes.
.result_na_like <- function(template, n = 1L) {
  if (is.null(template)) return(rep(NA, n))
  if (is.list(template)) return(rep(list(NA), n))
  template[rep(NA_integer_, n)]
}

# Shared union for released frames; it does not infer aggregation semantics.
.bind_result_frames <- function(frames) {
  frames <- Filter(is.data.frame, frames)
  if (!length(frames)) return(data.frame())
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  prototypes <- setNames(lapply(columns, function(column) {
    frames[[which(vapply(frames, function(x) column %in% names(x),
                         logical(1)))[[1L]]]][[column]]
  }), columns)
  frames <- lapply(frames, function(frame) {
    for (column in setdiff(columns, names(frame))) {
      frame[[column]] <- .result_na_like(prototypes[[column]], nrow(frame))
    }
    frame[, columns, drop = FALSE]
  })
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

# One boundary for all pooled readers. Preserve the released schema, filling
# unavailable/unimplemented statistics with typed NA, never an inferred sum.
# A NULL root still means pooling was refused or not requested. Wire envelopes
# (DP) can supply an empty schema when their display is a different contract.
.complete_pooled_shape <- function(pooled, per_site = list()) {
  missing <- character(0)
  complete <- function(value, templates, path) {
    templates <- Filter(Negate(is.null), templates)
    prototype <- if (length(templates)) templates[[1L]] else NULL
    if (is.null(value)) {
      missing <<- c(missing, path)
      if (is.data.frame(prototype)) {
        value <- prototype[NA_integer_, , drop = FALSE]
        rownames(value) <- NULL
      } else if (is.list(prototype) && !is.null(names(prototype))) {
        value <- list()
      } else {
        return(.result_na_like(prototype))
      }
    }
    if (is.data.frame(value)) {
      frames <- Filter(is.data.frame, templates)
      columns <- setdiff(unique(unlist(lapply(frames, names),
                                      use.names = FALSE)), "suppressed")
      for (column in setdiff(columns, names(value))) {
        template <- frames[[which(vapply(frames, function(x) {
          column %in% names(x)
        }, logical(1)))[[1L]]]][[column]]
        value[[column]] <- .result_na_like(template, nrow(value))
        missing <<- c(missing, paste0(path, "$", column))
      }
      for (column in names(value)) {
        if (is.list(value[[column]])) {
          value[[column]] <- lapply(value[[column]], function(cell) {
            complete(cell, list(), paste0(path, "$", column))
          })
        }
      }
    } else if (is.list(value)) {
      templates <- Filter(function(x) is.list(x) && !is.data.frame(x), templates)
      unnamed <- if (is.null(names(value))) seq_along(value) else
        which(is.na(names(value)) | !nzchar(names(value)))
      for (i in unnamed) {
        value[i] <- list(complete(value[[i]], list(),
                                  paste0(path, "[[", i, "]]")))
      }
      fields <- unique(c(names(value), unlist(lapply(templates, names),
                                              use.names = FALSE)))
      fields <- fields[!is.na(fields) & nzchar(fields)]
      if (length(fields)) {
        for (field in fields) {
          value[field] <- list(complete(value[[field]],
            lapply(templates, `[[`, field), paste0(path, "$", field)))
        }
      } else {
        value <- lapply(value, complete, templates = list(), path = path)
      }
    }
    value
  }
  if (is.null(pooled)) return(list(result = NULL, warnings = character(0)))
  out <- complete(pooled, per_site, "pooled")
  if (is.list(out) && !is.data.frame(out)) {
    class(out) <- unique(c("dsomop_summary", class(out)))
  }
  list(result = out, warnings = if (length(missing)) paste0(
    "Unavailable fields are NA; no pooled statistic is inferred from missing ",
    "releases or without a pooling rule: ", paste(unique(missing), collapse = ", "),
    ".") else character(0))
}

# Scalar summaries are one row. Nested components remain intact in list
# columns, avoiding Cartesian expansion/recycling of unrelated table rows.
.result_frame <- function(x) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x)) return(x)
  if (is.matrix(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
  if (is.atomic(x) && is.null(names(x))) {
    return(data.frame(value = x, stringsAsFactors = FALSE))
  }
  if (is.atomic(x)) x <- as.list(x)
  if (!is.list(x)) return(data.frame(value = I(list(x))))
  if (!length(x)) return(data.frame())
  if (is.null(names(x))) names(x) <- paste0("V", seq_along(x))
  unnamed <- is.na(names(x)) | !nzchar(names(x))
  names(x)[unnamed] <- paste0("V", which(unnamed))
  columns <- lapply(x, function(value) {
    if (is.null(value)) return(NA)
    if (is.atomic(value) && length(value) == 1L && is.null(dim(value))) {
      return(value)
    }
    I(list(value))
  })
  as.data.frame(columns, optional = TRUE, stringsAsFactors = FALSE)
}

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
#' @param schema Per-site prototypes for the pooled output, defaulting to
#'   \code{per_site}. Use an empty list for a derived display whose schema
#'   differs from the server wire payload (for example a DP release).
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
#'   \code{c("dsomop_result", "list")}). A present pooled result has no
#'   NULL leaves. Fields without a released value or a pooling rule are typed
#'   \code{NA}, with a warning; non-additive fields are never implicitly summed.
#'   List summaries support direct \code{as.data.frame()} conversion.
#' @keywords internal
dsomop_result <- function(per_site, pooled = NULL, meta = list(), schema = per_site) {
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
  shaped <- .complete_pooled_shape(pooled, schema)
  pooled <- shaped$result
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
                                error_warnings, shaped$warnings))
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
#' A \code{"combine"} or \code{"both"} result converts its pooled value;
#' scalar summaries become one row and nested components become list columns.
#' Matrices become frames and named vectors become one-row summaries. Split
#' results use the same conversion for each site before stacking the rows.
#' When no pooled value could be formed, it returns an empty
#' \code{data.frame()} so a failed strict federation never degrades to a
#' partial view (use \code{type = "split"} or \code{$per_site} for the
#' per-server results).
#'
#' @param x A \code{dsomop_result} object or its pooled list summary.
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
  if (!is.null(pooled)) {
    return(.result_frame(pooled))
  }
  meta <- .subset2(x, "meta")
  type <- meta$type %||% if (identical(meta$scope, "pooled")) "both" else
    "split"
  if (!identical(type, "split")) return(data.frame())
  .stack_per_site(.subset2(x, "per_site"))
}

#' @rdname as.data.frame.dsomop_result
#' @export
as.data.frame.dsomop_summary <- function(x, ...) .result_frame(x)

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

# Readers that returned their per-server list before 2.10.9
# (ds.omop.tables(), ds.omop.columns(), ds.omop.cohort.list(),
# ds.omop.disclosure.settings(), ds.omop.dp.status() and the plan and recipe
# previews and validations) keep that shape: the object is the per-server
# list, so names(), length(), [[i]], res$<server>, loops and lapply() work as
# before. The view rides along: $per_site, $pooled and $meta answer as on a
# dsomop_result, and printing shows the view.
.as_listing <- function(result) {
  per_site <- .subset2(result, "per_site")
  out <- if (is.null(per_site)) list() else per_site
  attr(out, "dsomop_view") <- list(pooled = .subset2(result, "pooled"),
                                   meta = .subset2(result, "meta"))
  class(out) <- unique(c("dsomop_listing", oldClass(per_site), "list"))
  out
}

# The per-server list itself, as the reader returned it before 2.10.9.
.listing_per_site <- function(x) {
  attr(x, "dsomop_view") <- NULL
  kept <- setdiff(oldClass(x), c("dsomop_listing", "list"))
  class(x) <- if (length(kept)) kept else NULL
  x
}

.listing_result <- function(x) {
  view <- attr(x, "dsomop_view", exact = TRUE)
  dsomop_result(per_site = .listing_per_site(x), pooled = view$pooled,
                meta = view$meta %||% list())
}

#' @rdname cash-.dsomop_result
#' @export
`$.dsomop_listing` <- function(x, name) {
  ps <- .listing_per_site(x)
  if (name %in% names(ps)) return(ps[[name]])
  view <- attr(x, "dsomop_view", exact = TRUE)
  switch(name,
    per_site = ps,
    pooled = view$pooled,
    meta = view$meta,
    ps[[name, exact = FALSE]])
}

#' @rdname cash-.dsomop_result
#' @export
`[[.dsomop_listing` <- function(x, i, exact = TRUE) {
  ps <- .listing_per_site(x)
  if (is.character(i) && length(i) == 1L && !i %in% names(ps) &&
      i %in% c("per_site", "pooled", "meta")) {
    return(`$.dsomop_listing`(x, i))
  }
  ps[[i, exact = exact]]
}

#' @export
print.dsomop_listing <- function(x, ...) {
  print(.listing_result(x), ...)
  invisible(x)
}

#' @export
as.data.frame.dsomop_listing <- function(x, ...) {
  as.data.frame(.listing_result(x), ...)
}
