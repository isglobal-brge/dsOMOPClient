# Module: Vocabulary Operations
# Client-side wrappers for OMOP CDM vocabulary search, concept lookup,
# descendant retrieval, and concept set expansion.

#' Search for OMOP concepts by name
#'
#' Searches the vocabulary tables across all connected DataSHIELD servers for
#' concepts matching the given pattern and/or filters. Results include concept
#' ID, name, domain, vocabulary, and standard concept flag. The search is
#' executed server-side via \code{omopSearchConceptsDS} so that vocabulary
#' tables never leave the server. Vocabulary metadata carries no patient data,
#' so this reader is not disclosure-gated.
#'
#' @param pattern Character; search term or SQL LIKE pattern to match against
#'   concept names. Supports partial matching (e.g., \code{"diabetes"} matches
#'   \code{"Type 2 diabetes mellitus"}). Now optional: pass \code{NULL} to
#'   search by \code{concept_id} or by the metadata filters alone.
#' @param domain Character; restrict results to a specific OMOP domain
#'   (e.g., \code{"Condition"}, \code{"Drug"}, \code{"Measurement"}).
#'   \code{NULL} (the default) returns concepts from all domains.
#' @param vocabulary Character; restrict results to a specific vocabulary
#'   (e.g., \code{"SNOMED"}, \code{"ICD10CM"}, \code{"RxNorm"}).
#'   \code{NULL} (the default) searches across all vocabularies.
#' @param standard_only Logical; if \code{TRUE} (the default), only standard
#'   concepts are returned. Set to \code{FALSE} to include non-standard and
#'   classification concepts. Ignored when \code{standard} is supplied.
#' @param limit Integer; maximum number of results to return per server
#'   (default: 50). Increase for broader searches, but larger values will
#'   increase server-side processing time.
#' @param concept_id Integer or numeric vector; restrict the search to these
#'   exact concept IDs. \code{NULL} (the default) applies no ID filter.
#' @param standard Character; explicit \code{standard_concept} value to filter
#'   on (e.g. \code{"S"} for standard, \code{"C"} for classification). When
#'   supplied this overrides \code{standard_only}. \code{NULL} (the default)
#'   applies no explicit value filter.
#' @param valid Logical; \code{TRUE} keeps only currently-valid concepts
#'   (\code{invalid_reason IS NULL}), \code{FALSE} only invalidated ones.
#'   \code{NULL} (the default) returns both.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"} (a
#'   de-duplicated cross-site view of the shared vocabulary; per-site frames
#'   remain available).
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
#'
#' # Look up exact IDs, keeping only currently-valid standard concepts
#' hits <- ds.omop.concept.search(NULL, concept_id = c(201826, 4329847),
#'                                standard = "S", valid = TRUE)
#' }
#' @export
ds.omop.concept.search <- function(pattern = NULL, domain = NULL,
                                   vocabulary = NULL,
                                   standard_only = TRUE,
                                   limit = 50,
                                   concept_id = NULL,
                                   standard = NULL,
                                   valid = NULL,
                                   symbol = "omop",
                                   conns = NULL,
                                   execute = TRUE,
                                   type = "both",
                                   datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.search")
  result_type <- .resolve_result_type(type, default_type = "both")
  if (!is.null(pattern) &&
      (!is.character(pattern) || length(pattern) != 1L || is.na(pattern))) {
    stop("pattern must be NULL or one non-missing character string.",
         call. = FALSE)
  }
  code <- .build_code("ds.omop.concept.search",
    pattern = pattern, domain = domain, vocabulary = vocabulary,
    standard_only = standard_only, limit = limit, concept_id = concept_id,
    standard = standard, valid = valid, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopSearchConceptsDS", session$res_symbol,
                if (!is.null(pattern)) .ds_encode_scalar(pattern) else NULL,
                domain, vocabulary,
                standard_only, as.integer(limit),
                concept_id = if (!is.null(concept_id))
                  .ds_encode(as.integer(concept_id)) else NULL,
                standard = standard, valid = valid)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_concept_metadata(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
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
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"} (a
#'   de-duplicated cross-site view of the shared vocabulary; per-site frames
#'   remain available).
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
                                   execute = TRUE,
                                   type = "both",
                                   datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.lookup")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.lookup",
    concept_ids = concept_ids, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopLookupConceptsDS", session$res_symbol,
                .ds_encode(as.integer(concept_ids)))
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_concept_metadata(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
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
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"} (a
#'   de-duplicated cross-site view of the shared vocabulary; per-site frames
#'   remain available).
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
                                        execute = TRUE,
                                        type = "both",
                                        datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.descendants")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.descendants",
    ancestor_ids = ancestor_ids, include_self = include_self,
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopGetDescendantsDS", session$res_symbol,
                .ds_encode(as.integer(ancestor_ids)), include_self)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_concept_metadata(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
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
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result} object with \code{scope = "pooled"} (a
#'   de-duplicated cross-site view of the shared vocabulary; per-site frames
#'   remain available).
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
                                   execute = TRUE,
                                   type = "both",
                                   datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.expand")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.expand",
    concept_set = concept_set, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopExpandConceptSetDS", session$res_symbol,
                .ds_encode(concept_set))
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_concept_metadata(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

# --- Vocabulary catalog pooling helper ----------------------------------------

#' Union and de-duplicate per-site vocabulary data frames
#'
#' Vocabulary/CDM metadata is reference data: a given concept, vocabulary,
#' domain, or concept class exists independently on each server, so the cross-
#' site view is simply their set union (not a sum). This helper row-binds the
#' per-site data frames (intersecting on common columns so heterogeneous schemas
#' still combine) and drops duplicate rows. Empty / non-data-frame entries are
#' skipped. A partial server response returns \code{NULL} rather than claiming
#' an incomplete union is federation-wide. Returns \code{NULL} when there is
#' nothing to pool.
#'
#' @param per_site Named list of per-server data frames.
#' @return A single de-duplicated data frame, or \code{NULL}.
#' @keywords internal
.pool_vocab_union <- function(per_site) {
  if (length(attr(per_site, "ds_errors")) > 0L) return(NULL)
  dfs <- Filter(function(d) is.data.frame(d) && nrow(d) > 0, per_site)
  if (length(dfs) == 0) return(.empty_metadata_union(per_site))
  common <- Reduce(intersect, lapply(dfs, names))
  if (length(common) == 0) return(NULL)
  combined <- do.call(rbind, lapply(dfs, function(d) d[, common, drop = FALSE]))
  combined <- unique(combined)
  rownames(combined) <- NULL
  combined
}

# --- Concept hierarchy / synonyms / relationships -----------------------------

#' Get a concept's ancestors and descendants (hierarchy)
#'
#' Returns both the ancestors and descendants of one or more concept IDs from
#' the OMOP \code{concept_ancestor} table on each connected server, in a single
#' frame tagged with a \code{direction} column (\code{"ancestor"} or
#' \code{"descendant"}) and \code{levels_of_separation}. This is the Athena/ATLAS
#' hierarchy ("relationships" tree) view. Vocabulary reference data carries no
#' patient information, so this reader is not disclosure-gated and the per-site
#' frames are pooled by set union.
#'
#' @param concept_ids Integer or numeric vector of concept IDs to expand the
#'   hierarchy for (e.g., \code{c(201826)}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's frame
#'   (with \code{direction} and \code{levels_of_separation}); \code{pooled} is
#'   the de-duplicated union across servers.
#' @examples
#' \dontrun{
#' tree <- ds.omop.concept.ancestors(201826)
#' tree$pooled                       # union across sites
#' subset(tree$pooled, direction == "ancestor")
#' }
#' @export
ds.omop.concept.ancestors <- function(concept_ids,
                                      symbol = "omop",
                                      conns = NULL,
                                      execute = TRUE,
                                      type = "both",
                                      datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.ancestors")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.ancestors",
    concept_ids = concept_ids, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopConceptAncestorsDS", session$res_symbol,
                .ds_encode(as.integer(concept_ids)))
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

#' Get the synonyms of one or more concepts
#'
#' Returns the alternative names for one or more concept IDs from the OMOP
#' \code{concept_synonym} table on each connected server, mirroring the Athena
#' concept "synonyms" panel. Vocabulary reference data carries no patient
#' information, so this reader is not disclosure-gated and the per-site frames
#' are pooled by set union.
#'
#' @param concept_ids Integer or numeric vector of concept IDs to fetch
#'   synonyms for (e.g., \code{c(201826)}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's
#'   \code{concept_id} / \code{concept_synonym_name} frame; \code{pooled} is the
#'   de-duplicated union across servers.
#' @examples
#' \dontrun{
#' syns <- ds.omop.concept.synonyms(201826)
#' syns$pooled
#' }
#' @export
ds.omop.concept.synonyms <- function(concept_ids,
                                     symbol = "omop",
                                     conns = NULL,
                                     execute = TRUE,
                                     type = "both",
                                     datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.synonyms")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.synonyms",
    concept_ids = concept_ids, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopConceptSynonymsDS", session$res_symbol,
                .ds_encode(as.integer(concept_ids)))
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

#' Get the relationships of one or more concepts
#'
#' Returns every \code{concept_relationship} edge touching the given concept IDs
#' on each connected server, in \strong{both} directions (the related concept's
#' name is joined in and a \code{direction} column distinguishes
#' \code{"forward"} from \code{"reverse"}). An optional \code{relationship_id}
#' narrows the result to a single relationship type. Vocabulary reference data
#' carries no patient information, so this reader is not disclosure-gated and the
#' per-site frames are pooled by set union.
#'
#' @param concept_ids Integer or numeric vector of concept IDs to fetch
#'   relationships for (e.g., \code{c(201826)}).
#' @param relationship_id Character; optional single \code{relationship_id}
#'   filter (e.g. \code{"Maps to"}, \code{"Is a"}, \code{"Subsumes"}).
#'   \code{NULL} (the default) returns all relationship types.
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's
#'   relationship frame (with a \code{direction} column); \code{pooled} is the
#'   de-duplicated union across servers.
#' @examples
#' \dontrun{
#' # All relationships of a concept
#' rels <- ds.omop.concept.relationships(201826)
#' rels$pooled
#'
#' # Only the "Maps to" edges
#' maps <- ds.omop.concept.relationships(201826, relationship_id = "Maps to")
#' }
#' @export
ds.omop.concept.relationships <- function(concept_ids,
                                          relationship_id = NULL,
                                          symbol = "omop",
                                          conns = NULL,
                                          execute = TRUE,
                                          type = "both",
                                          datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.relationships")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.relationships",
    concept_ids = concept_ids, relationship_id = relationship_id,
    type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopConceptRelationshipsDS", session$res_symbol,
                .ds_encode(as.integer(concept_ids)),
                relationship_id = relationship_id)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

# --- Paged concept catalog ----------------------------------------------------

#' Browse the concept catalog with pagination
#'
#' Pages through the OMOP \code{concept} catalog on each connected server,
#' filtered by domain, vocabulary, concept class, standard status and validity,
#' using OFFSET/LIMIT pagination (page size capped server-side at 1000). Unlike
#' \code{\link{ds.omop.concept.search}} this returns the total matching count
#' alongside the current page so callers can build pagination controls.
#' Vocabulary reference data carries no patient information, so this reader is
#' not disclosure-gated; the per-site pages are pooled by set union. The
#' pooled \code{total_count} is the sum of the per-site totals, labelled as
#' such: a concept present on several servers (shared vocabularies usually
#' are) counts once per server, so it is an upper bound of the distinct
#' concepts; \code{total_count_max}, the largest per-site total, is a lower
#' bound.
#'
#' @param domain Character; filter by \code{domain_id} (e.g. \code{"Condition"}).
#'   \code{NULL} (the default) applies no domain filter.
#' @param vocabulary Character; filter by \code{vocabulary_id} (e.g.
#'   \code{"SNOMED"}). \code{NULL} (the default) applies no vocabulary filter.
#' @param concept_class Character; filter by \code{concept_class_id}.
#'   \code{NULL} (the default) applies no class filter.
#' @param standard Character; filter by \code{standard_concept} value (e.g.
#'   \code{"S"}). \code{NULL} (the default) applies no standard filter.
#' @param valid Logical; \code{TRUE} keeps only currently-valid concepts,
#'   \code{FALSE} only invalidated ones. \code{NULL} (the default) returns both.
#' @param offset Integer; number of rows to skip (page start). Default \code{0}.
#' @param limit Integer; page size (default \code{100}; capped at 1000
#'   server-side).
#' @param order Character; column to order by. \code{NULL} (the default) uses
#'   the server default (\code{concept_id}).
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's
#'   \code{list(rows, total_count, offset, limit)}; \code{pooled} is a list with
#'   \code{rows} (the de-duplicated union of the page rows),
#'   \code{total_count} (the sum of the per-site totals, an upper bound of the
#'   distinct concepts), \code{total_count_max} (the largest per-site total,
#'   a lower bound), \code{total_count_basis} (that label), \code{offset}
#'   and \code{limit}.
#' @examples
#' \dontrun{
#' # First page of SNOMED conditions, with the total available count
#' page <- ds.omop.concept.list(domain = "Condition", vocabulary = "SNOMED",
#'                              limit = 100)
#' page$pooled$total_count
#' nrow(page$pooled$rows)
#'
#' # Next page
#' page2 <- ds.omop.concept.list(domain = "Condition", vocabulary = "SNOMED",
#'                               offset = 100, limit = 100)
#' }
#' @export
ds.omop.concept.list <- function(domain = NULL, vocabulary = NULL,
                                 concept_class = NULL, standard = NULL,
                                 valid = NULL, offset = 0, limit = 100,
                                 order = NULL,
                                 symbol = "omop", conns = NULL,
                                 execute = TRUE,
                                 type = "both",
                                 datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.concept.list")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.concept.list",
    domain = domain, vocabulary = vocabulary, concept_class = concept_class,
    standard = standard, valid = valid, offset = offset, limit = limit,
    order = order, type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopListConceptsDS", session$res_symbol,
                domain = domain, vocabulary = vocabulary,
                concept_class = concept_class, standard = standard,
                valid = valid, offset = as.integer(offset),
                limit = as.integer(limit), order = order)
  )

  # Union the page rows across sites. Each site reports the size of ITS own
  # matching set: their sum counts a shared concept once per server (an upper
  # bound) and their maximum is a lower bound of the distinct concepts.
  page_rows <- lapply(raw, function(s) if (is.list(s)) s$rows else NULL)
  names(page_rows) <- names(raw)
  attr(page_rows, "ds_errors") <- attr(raw, "ds_errors")
  totals <- vapply(raw, function(s) {
    if (is.list(s) && !is.null(s$total_count)) as.numeric(s$total_count)
    else NA_real_
  }, numeric(1))
  incomplete <- length(attr(raw, "ds_errors")) > 0L || all(is.na(totals))
  pooled <- if (.result_type_wants_combine(result_type)) list(
    rows = .pool_vocab_union(page_rows),
    total_count = if (incomplete) NA_real_ else sum(totals, na.rm = TRUE),
    total_count_max = if (incomplete) NA_real_ else max(totals, na.rm = TRUE),
    total_count_basis = paste0(
      "total_count sums the per-site totals (a concept on several servers ",
      "counts once per server); total_count_max is the largest per-site ",
      "total"),
    offset = as.integer(offset),
    limit = as.integer(limit)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code)), result_type,
    "Not every server returned this concept page.",
    pooling = .pooling_meta("metadata_union"))
}

# --- Vocabulary catalogs (vocabularies / domains / classes) -------------------

#' List the vocabularies available on each server
#'
#' Returns the distinct vocabularies from the OMOP \code{vocabulary} table on
#' each connected server (falling back to distinct \code{vocabulary_id} values
#' on \code{concept} when the vocabulary table is not loaded). Vocabulary
#' reference data carries no patient information, so this reader is not
#' disclosure-gated and the per-site frames are pooled by set union (a
#' vocabulary may exist on several servers).
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's
#'   vocabulary frame; \code{pooled} is the de-duplicated union across servers.
#' @examples
#' \dontrun{
#' vocabs <- ds.omop.vocab.vocabularies()
#' vocabs$pooled
#' }
#' @export
ds.omop.vocab.vocabularies <- function(symbol = "omop", conns = NULL,
                                       execute = TRUE,
                                       type = "both",
                                       datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.vocab.vocabularies")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.vocab.vocabularies", type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopVocabulariesDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

#' List the domains available on each server
#'
#' Returns the distinct domains from the OMOP \code{domain} table on each
#' connected server (falling back to distinct \code{domain_id} values on
#' \code{concept} when the domain table is not loaded). Vocabulary reference
#' data carries no patient information, so this reader is not disclosure-gated
#' and the per-site frames are pooled by set union.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's domain
#'   frame; \code{pooled} is the de-duplicated union across servers.
#' @examples
#' \dontrun{
#' domains <- ds.omop.vocab.domains()
#' domains$pooled
#' }
#' @export
ds.omop.vocab.domains <- function(symbol = "omop", conns = NULL,
                                  execute = TRUE,
                                  type = "both",
                                  datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.vocab.domains")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.vocab.domains", type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopDomainsDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

#' List the concept classes available on each server
#'
#' Returns the distinct concept classes from the OMOP \code{concept_class} table
#' on each connected server (falling back to distinct \code{concept_class_id}
#' values on \code{concept} when the concept_class table is not loaded).
#' Vocabulary reference data carries no patient information, so this reader is
#' not disclosure-gated and the per-site frames are pooled by set union.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"both"} (default), \code{"split"}, or
#'   \code{"combine"}. The pooled view is the de-duplicated union of the
#'   servers' rows (shared reference data, no patient counts); it is
#'   \code{NULL} with \code{meta$pooling_reason} when a server did not
#'   answer. See \link{dsomop_conventions}.
#' @return A \code{dsomop_result}. \code{per_site} holds each server's concept
#'   class frame; \code{pooled} is the de-duplicated union across servers.
#' @examples
#' \dontrun{
#' classes <- ds.omop.vocab.classes()
#' classes$pooled
#' }
#' @export
ds.omop.vocab.classes <- function(symbol = "omop", conns = NULL,
                                  execute = TRUE,
                                  type = "both",
                                  datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.vocab.classes")
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.vocab.classes", type = result_type, symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopConceptClassesDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw,
    pooled = if (.result_type_wants_combine(result_type)) .pool_vocab_union(raw),
    meta = list(call_code = code)), result_type,
    "Not every server returned this vocabulary result.",
    pooling = .pooling_meta("metadata_union"))
}

# --- CDM source / version metadata (per-server) -------------------------------

#' Get the CDM source description from each server
#'
#' Returns the full \code{cdm_source} table row(s) from each connected server,
#' describing the data source (name, abbreviation, holder, release/version
#' dates, etc.). This is metadata, not patient data, so this reader is not
#' disclosure-gated. Sites may describe genuinely different data sources, so the
#' result is deliberately kept \strong{per-server} and is not merged.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"split"} (default) or \code{"both"}. Data
#'   source descriptions are site-specific: \code{"both"} returns
#'   \code{pooled = NULL} with \code{meta$pooling_reason}, and
#'   \code{"combine"} stops before contacting the servers.
#' @return A \code{dsomop_result} (per-server view). Each server's
#'   result is its \code{cdm_source} data frame (empty if the table is absent).
#'   \code{pooled} is \code{NULL} by design.
#' @examples
#' \dontrun{
#' src <- ds.omop.cdm.source()
#' src$per_site            # one cdm_source frame per server
#' }
#' @export
ds.omop.cdm.source <- function(symbol = "omop", conns = NULL,
                               execute = TRUE,
                               type = "split",
                               datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.cdm.source")
  result_type <- .resolve_result_type(type, default_type = "split")
  reason <- "CDM source descriptions are site-specific and are not merged."
  .combine_unavailable(result_type, reason)
  code <- .build_code("ds.omop.cdm.source", type = result_type,
                      symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopCdmSourceDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL, meta = list(call_code = code)),
    result_type, reason)
}

#' Get the CDM version reported by each server
#'
#' Returns the CDM version reported by each connected server (preferring
#' \code{cdm_source.cdm_version} and falling back to the version inferred from
#' the table structure). This is metadata, not patient data, so this reader is
#' not disclosure-gated. Sites may legitimately run different CDM versions, so
#' the result is deliberately kept \strong{per-server} and is never silently
#' merged into a single version.
#'
#' @param symbol Character; the session symbol used when the OMOP connection
#'   was initialised (default: \code{"omop"}).
#' @param conns Deprecated alias of \code{datasources}, kept for backward
#'   compatibility (a one-time message names the new argument).
#' @param datasources DataSHIELD connections (a named list of
#'   \code{DSConnection} objects). \code{NULL} (the default) uses the OMOP
#'   session's connections, or \code{DSI::datashield.connections_find()} when
#'   no session exists. See \link{dsomop_conventions}.
#' @param execute Logical; if \code{FALSE}, returns a dry-run
#'   \code{dsomop_result} containing only the reproducible R code without
#'   contacting the servers.
#' @param type Result view: \code{"split"} (default), \code{"both"}, or
#'   \code{"combine"}. The pooled view is the version record every server
#'   reported identically; when the servers differ it is \code{NULL} with
#'   \code{meta$pooling_reason}, never a silent merge.
#' @return A \code{dsomop_result}. Each server's result is a list with
#'   \code{cdm_version}, \code{source}, and \code{vocabulary_version}.
#' @examples
#' \dontrun{
#' ver <- ds.omop.cdm.version()
#' # CDM version per server
#' lapply(ver$per_site, function(v) v$cdm_version)
#' }
#' @export
ds.omop.cdm.version <- function(symbol = "omop", conns = NULL,
                                execute = TRUE,
                                type = "split",
                                datasources = NULL) {
  conns <- .datasources_arg(datasources, conns, "ds.omop.cdm.version")
  result_type <- .resolve_result_type(type, default_type = "split")
  code <- .build_code("ds.omop.cdm.version", type = result_type,
                      symbol = symbol)

  if (!execute) return(.reader_dry_run(code, result_type))

  session <- .get_session(symbol)
  conns <- .resolve_datasources(conns, symbol, session)

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopCdmVersionDS", session$res_symbol)
  )

  consensus <- list(result = NULL, reason = NULL)
  if (.result_type_wants_combine(result_type)) {
    missing <- unique(c(names(attr(raw, "ds_errors")),
                        setdiff(names(conns), names(raw))))
    versions <- vapply(raw, function(v) {
      as.character(v$cdm_version %||% NA_character_)[[1L]]
    }, character(1))
    consensus <- if (length(missing) > 0L) {
      list(result = NULL, reason = paste0(
        "No CDM version from: ", paste(missing, collapse = ", "), "."))
    } else if (length(raw) > 0L &&
               all(vapply(raw, identical, logical(1), raw[[1L]]))) {
      list(result = raw[[1L]], reason = NULL)
    } else {
      list(result = NULL, reason = paste0(
        "Servers report different CDM version records (",
        paste(names(versions), versions, sep = " = ", collapse = ", "),
        "); they are not merged."))
    }
  }
  .result_type_view(dsomop_result(
    per_site = raw, pooled = consensus$result,
    meta = list(call_code = code)), result_type,
    consensus$reason %||% "No CDM version was reported by every server.",
    pooling = .pooling_meta("metadata_consensus"))
}
