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
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
#' @export
ds.omop.concept.search <- function(pattern, domain = NULL,
                                   vocabulary = NULL,
                                   standard_only = TRUE,
                                   limit = 50,
                                   symbol = "omop",
                                   conns = NULL,
                                   execute = TRUE) {
  code <- .build_code("ds.omop.concept.search",
    pattern = pattern, domain = domain, vocabulary = vocabulary,
    standard_only = standard_only, limit = limit, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = "per_site")))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopSearchConceptsDS", session$res_symbol,
                pattern, domain, vocabulary,
                standard_only, as.integer(limit))
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}

#' Lookup concepts by ID
#'
#' @param concept_ids Numeric vector of concept IDs
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
#' @export
ds.omop.concept.lookup <- function(concept_ids,
                                   symbol = "omop",
                                   conns = NULL,
                                   execute = TRUE) {
  code <- .build_code("ds.omop.concept.lookup",
    concept_ids = concept_ids, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = "per_site")))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopLookupConceptsDS", session$res_symbol,
                as.integer(concept_ids))
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}

#' Get descendant concepts via concept_ancestor
#'
#' @param ancestor_ids Numeric vector of ancestor concept IDs
#' @param include_self Logical; include the ancestors themselves
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
#' @export
ds.omop.concept.descendants <- function(ancestor_ids,
                                        include_self = TRUE,
                                        symbol = "omop",
                                        conns = NULL,
                                        execute = TRUE) {
  code <- .build_code("ds.omop.concept.descendants",
    ancestor_ids = ancestor_ids, include_self = include_self,
    symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = "per_site")))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopGetDescendantsDS", session$res_symbol,
                as.integer(ancestor_ids), include_self)
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
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
#' @param execute Logical; if FALSE, return dry-run result with code only
#' @return A dsomop_result object
#' @export
ds.omop.concept.expand <- function(concept_set,
                                   symbol = "omop",
                                   conns = NULL,
                                   execute = TRUE) {
  code <- .build_code("ds.omop.concept.expand",
    concept_set = concept_set, symbol = symbol)

  if (!execute) {
    return(dsomop_result(
      per_site = list(), pooled = NULL,
      meta = list(call_code = code, scope = "per_site")))
  }

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- DSI::datashield.aggregate(
    conns,
    expr = call("omopExpandConceptSetDS", session$res_symbol,
                concept_set)
  )

  dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site"))
}
