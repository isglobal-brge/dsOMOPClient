# Module: Vocabulary Operations
# Client-side wrappers for OMOP CDM vocabulary search, concept lookup,
# descendant retrieval, and concept set expansion.

#' Search for OMOP concepts by name
#'
#' Searches the vocabulary tables across all connected DataSHIELD servers for
#' concepts matching the given pattern. Results include concept ID, name,
#' domain, vocabulary, and standard concept flag. The search is executed
#' server-side via \code{omopSearchConceptsDS} so that vocabulary tables
#' never leave the server.
#'
#' @param pattern Character; search term or SQL LIKE pattern to match against
#'   concept names. Supports partial matching (e.g., \code{"diabetes"} matches
#'   \code{"Type 2 diabetes mellitus"}).
#' @param domain Character; restrict results to a specific OMOP domain
#'   (e.g., \code{"Condition"}, \code{"Drug"}, \code{"Measurement"}).
#'   \code{NULL} (the default) returns concepts from all domains.
#' @param vocabulary Character; restrict results to a specific vocabulary
#'   (e.g., \code{"SNOMED"}, \code{"ICD10CM"}, \code{"RxNorm"}).
#'   \code{NULL} (the default) searches across all vocabularies.
#' @param standard_only Logical; if \code{TRUE} (the default), only standard
#'   concepts are returned. Set to \code{FALSE} to include non-standard and
#'   classification concepts.
#' @param limit Integer; maximum number of results to return per server
#'   (default: 50). Increase for broader searches, but larger values will
#'   increase server-side processing time.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#'   Each server's result is a data frame with columns such as
#'   \code{concept_id}, \code{concept_name}, \code{domain_id},
#'   \code{vocabulary_id}, and \code{standard_concept}.
#' @examples
#' \dontrun{
#' # Search for diabetes-related conditions
#' results <- ds.omop.concept.search("diabetes", domain = "Condition")
#' results$pooled
#'
#' # Search across all domains, including non-standard concepts
#' all_hits <- ds.omop.concept.search("aspirin", standard_only = FALSE, limit = 100)
#' }
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

#' Look up OMOP concepts by ID
#'
#' Retrieves full concept metadata for one or more concept IDs from the
#' vocabulary tables on each connected server. This is useful for resolving
#' concept IDs obtained from clinical data tables back to their human-readable
#' names, domains, and vocabulary membership.
#'
#' @param concept_ids Integer or numeric vector of OMOP concept IDs to look up
#'   (e.g., \code{c(201826, 4329847)}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#'   Each server's result is a data frame with concept metadata columns.
#' @examples
#' \dontrun{
#' # Look up a single concept
#' info <- ds.omop.concept.lookup(201826)
#' info$per_site
#'
#' # Look up multiple concepts at once
#' info <- ds.omop.concept.lookup(c(201826, 4329847, 316139))
#' }
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

#' Get descendant concepts via the concept_ancestor table
#'
#' Traverses the OMOP \code{concept_ancestor} hierarchy on each connected
#' server and returns all descendant concepts for the given ancestor IDs.
#' This is the standard way to expand a high-level concept (e.g., "Diabetes
#' mellitus") into all of its more specific child concepts.
#'
#' @param ancestor_ids Integer or numeric vector of ancestor concept IDs
#'   to expand (e.g., \code{c(201820)}).
#' @param include_self Logical; if \code{TRUE} (the default), the ancestor
#'   concepts themselves are included in the result alongside their
#'   descendants.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#'   Each server's result is a data frame of descendant concept rows.
#' @examples
#' \dontrun{
#' # Get all descendants of "Type 2 diabetes mellitus" (concept 201826)
#' desc <- ds.omop.concept.descendants(201826)
#' nrow(desc$per_site[[1]])
#'
#' # Exclude the ancestor itself
#' desc <- ds.omop.concept.descendants(201826, include_self = FALSE)
#' }
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
#' Creates a local \code{omop_concept_set} object that defines a set of OMOP
#' concepts along with expansion rules. This is a client-side-only helper
#' that does not contact any server; the resulting object is passed to
#' \code{\link{ds.omop.concept.expand}} for server-side resolution.
#'
#' @param concepts Integer or numeric vector of seed concept IDs that form
#'   the base of the concept set.
#' @param include_descendants Logical; if \code{TRUE}, all descendants of
#'   the seed concepts (via \code{concept_ancestor}) will be included when
#'   the set is expanded. Default: \code{FALSE}.
#' @param include_mapped Logical; if \code{TRUE}, non-standard concepts
#'   mapped to the seed concepts (via \code{concept_relationship}) will be
#'   included when the set is expanded. Default: \code{FALSE}.
#' @param exclude Integer or numeric vector of concept IDs to explicitly
#'   remove from the expanded set. \code{NULL} (the default) excludes
#'   nothing.
#' @return An \code{omop_concept_set} object (a list with class
#'   \code{c("omop_concept_set", "list")}).
#' @examples
#' \dontrun{
#' # Simple concept set with descendants
#' cs <- ds.omop.concept.set(c(201826, 316139), include_descendants = TRUE)
#'
#' # Expand the concept set on the server
#' expanded <- ds.omop.concept.expand(cs)
#' }
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

#' Expand a concept set to a full list of concept IDs
#'
#' Takes an \code{omop_concept_set} object (built with
#' \code{\link{ds.omop.concept.set}}) and resolves it on each connected
#' server. Expansion applies descendant inclusion, mapped-concept inclusion,
#' and exclusion rules, returning the final flat list of concept IDs that
#' the set represents.
#'
#' @param concept_set An \code{omop_concept_set} object created by
#'   \code{\link{ds.omop.concept.set}}.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#'   Each server's result contains the resolved concept IDs.
#' @examples
#' \dontrun{
#' cs <- ds.omop.concept.set(c(201826), include_descendants = TRUE)
#' expanded <- ds.omop.concept.expand(cs)
#' expanded$per_site[[1]]
#' }
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
