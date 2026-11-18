#' Declared category levels from the public vocabulary
#'
#' Expands a concept set on every connected server's vocabulary tables and
#' returns the union as declared category keys for
#' \code{omop_category_spec(levels = )}. The expansion reads only vocabulary
#' reference tables (\code{concept}, \code{concept_ancestor},
#' \code{concept_relationship}), never patient rows.
#'
#' Declared levels are public. A concept column presented with them is built
#' from the declaration at every site, so nothing about which concepts occur
#' at a site is released and no factor-level limit applies to them, however
#' many levels the list holds. Observed values outside the list are still
#' discovered under DataSHIELD's factor-level rule (\code{nfilter.levels.max}
#' and \code{nfilter.levels.density}). Counts and models keep their usual
#' disclosure controls.
#'
#' The union covers sites whose vocabulary versions differ. The keys are
#' stored in the recipe, so it records exactly which levels it declares.
#'
#' @param concept_set An \code{\link{omop_concept_set}()}; \code{descendants},
#'   \code{mapped} and \code{exclude} expand as in an event.
#' @param symbol Character; the OMOP session symbol.
#' @param conns DSI connections; defaults to the session's connections.
#' @return Character vector of \code{"concept:<id>"} keys in concept ID order.
#' @examples
#' \dontrun{
#' causes <- omop_concept_levels(omop_concept_set(4306655, descendants = TRUE))
#' cause <- omop_category(
#'   omop_variable(event = omop_event("death", name = "cause_of_death"),
#'                 reducer = omop_reduce("first_value", value = "cause_concept_id")),
#'   omop_category_spec(levels = causes))
#' }
#' @seealso \code{\link{omop_category_spec}}, \code{\link{omop_concept_set}}
#' @export
omop_concept_levels <- function(concept_set, symbol = "omop", conns = NULL) {
  if (!inherits(concept_set, "omop_concept_set")) {
    stop("omop_concept_levels(): concept_set must be an omop_concept_set().", call. = FALSE)
  }
  if (.v2_is_param(concept_set$concepts) || .v2_is_param(concept_set$exclude)) {
    stop("omop_concept_levels(): bind recipe parameters before declaring levels.", call. = FALSE)
  }
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  expand <- function(cs) {
    spec <- list(concepts = as.integer(cs$concepts),
                 include_descendants = isTRUE(cs$descendants),
                 include_mapped = isTRUE(cs$mapped))
    raw <- .ds_safe_aggregate(conns,
      expr = call("omopExpandConceptSetDS", session$res_symbol, .ds_encode(spec)))
    failed <- union(names(attr(raw, "ds_errors")), setdiff(names(conns), names(raw)))
    if (length(failed)) {
      stop("omop_concept_levels(): the vocabulary expansion failed on ",
           paste(failed, collapse = ", "), ". ",
           paste(unlist(attr(raw, "ds_errors"), use.names = FALSE), collapse = "; "), call. = FALSE)
    }
    unique(as.integer(unlist(raw, use.names = FALSE)))
  }
  ids <- expand(concept_set)
  if (!is.null(concept_set$exclude)) {
    drop <- if (inherits(concept_set$exclude, "omop_concept_set")) expand(concept_set$exclude) else
      as.integer(concept_set$exclude)
    ids <- setdiff(ids, drop)
  }
  ids <- sort(unique(ids[!is.na(ids)]))
  if (!length(ids)) {
    stop("omop_concept_levels(): the concept set expands to no concepts.", call. = FALSE)
  }
  paste0("concept:", ids)
}
