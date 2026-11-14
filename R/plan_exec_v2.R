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
         "those servers to ", "2.10.5", ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' Semantic digest of a v2 plan over its normal form
#'
#' The digest binds the federation harmonisation signature and the DP
#' provenance capsule: \code{.planNormalizedDigest()} of the vendored A-11
#' normaliser (R/plan_normalize_v2.R), i.e. SHA-256 of the RFC 8785
#' serialisation of the normalised plan without its advisory fields.
#'
#' @param plan A v2 plan.
#' @return A list with \code{payload} and \code{sha256}.
#' @keywords internal
.plan_v2_digest <- function(plan) {
  .planNormalizedDigest(plan)
}

.v2_has_not <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (identical(names(x), "not")) return(TRUE)
  any(vapply(x, .v2_has_not, logical(1)))
}

.v2_has_source_anchor <- function(x) {
  if (!is.list(x)) return(FALSE)
  for (key in intersect(names(x) %||% character(0), c("anchor", "reference", "from", "to"))) {
    a <- x[[key]]
    if (is.list(a) && !is.null(a[["source"]])) return(TRUE)
  }
  any(vapply(x, .v2_has_source_anchor, logical(1)))
}

#' Language operations a v2 plan needs from a server
#'
#' The same inventory the server computes (\code{.irOperations()}), over the
#' normal form of the plan, so the client sends a v2 plan exactly when every
#' server advertises what it needs (design section 4.4).
#'
#' @param plan A v2 plan.
#' @return A sorted character vector of operation names and, as attribute
#'   \code{"reducers"}, the reducer functions used.
#' @keywords internal
.plan_v2_operations <- function(plan) {
  x <- .planNormalizeV2(plan)
  ops <- if (.conceptNeedsPresentation(plan)) "output.concept_labels" else character(0)
  ops <- c(ops, .numberCapabilities(x), .categoryCapabilities(x), "output.codebook_metadata")
  date_vars <- .numberVariables(x)
