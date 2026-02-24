# ==============================================================================
# dsOMOPClient v2 - Vocabulary Operations
# ==============================================================================

#' Search concepts by name pattern
#'
#' @param pattern Character; search pattern
#' @param domain Character; domain filter
#' @param vocabulary Character; vocabulary filter
#' @param standard_only Logical; only standard concepts
#' @param limit Integer; max results
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.concept.search <- function(pattern, domain = NULL,
                                   vocabulary = NULL,
                                   standard_only = TRUE,
                                   limit = 50,
                                   symbol = "omop",
                                   conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopSearchConceptsDS", session$res_symbol,
                pattern, domain, vocabulary,
                standard_only, as.integer(limit))
  )
}

#' Lookup concepts by ID
#'
#' @param concept_ids Numeric vector of concept IDs
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.concept.lookup <- function(concept_ids,
                                   symbol = "omop",
                                   conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopLookupConceptsDS", session$res_symbol,
                as.integer(concept_ids))
  )
}

#' Get descendant concepts via concept_ancestor
#'
#' @param ancestor_ids Numeric vector of ancestor concept IDs
#' @param include_self Logical; include the ancestors themselves
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of data frames
#' @export
ds.omop.concept.descendants <- function(ancestor_ids,
                                        include_self = TRUE,
                                        symbol = "omop",
                                        conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopGetDescendantsDS", session$res_symbol,
                as.integer(ancestor_ids), include_self)
  )
}

#' Build a concept set specification (client-only)
#'
#' @param concepts Numeric vector of concept IDs
#' @param include_descendants Logical; include descendants
#' @param include_mapped Logical; include mapped concepts
#' @param exclude Numeric vector; concept IDs to exclude
#' @return An omop_concept_set object
#' @export
ds.omop.concept.set <- function(concepts,
                                include_descendants = FALSE,
                                include_mapped = FALSE,
                                exclude = NULL) {
  spec <- list(
    concepts = as.integer(concepts),
    include_descendants = include_descendants,
    include_mapped = include_mapped,
    exclude = if (!is.null(exclude)) as.integer(exclude) else NULL
  )
  class(spec) <- c("omop_concept_set", "list")
  spec
}

#' Expand a concept set to full ID list
#'
#' @param concept_set An omop_concept_set object
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) of integer vectors
#' @export
ds.omop.concept.expand <- function(concept_set,
                                   symbol = "omop",
                                   conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopExpandConceptSetDS", session$res_symbol,
                concept_set)
  )
}
