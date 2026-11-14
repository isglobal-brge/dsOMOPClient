# The catalogue is local client data. Templates use only the language-2 API.
.library_root <- function() system.file("recipe-library", package = "dsOMOPClient")

.library_catalog <- function() {
  paths <- list.files(.library_root(), pattern = "^metadata.json$", recursive = TRUE,
                      full.names = TRUE)
  entries <- lapply(paths, jsonlite::fromJSON, simplifyVector = FALSE)
  .library_validate_catalog(entries)
}

.library_validate_catalog <- function(entries) {
  for (entry in entries) {
    for (field in c("id", "template_version", "title", "category", "summary",
                    "min_client_version", "status")) {
      .library_string(entry[[field]], paste("Recipe library", field))
    }
    if (!grepl("^[a-z][a-z0-9_]*\\.[a-z][a-z0-9_]*$", entry$id) ||
        !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", entry$template_version) ||
        !entry$status %in% c("recommended", "supported", "withdrawn") ||
        !identical(as.integer(entry$language_version), 2L)) {
      stop("Malformed recipe library definition metadata.", call. = FALSE)
    }
  }
  keys <- vapply(entries, function(x) paste(x$id, x$template_version), "")
  if (anyDuplicated(keys)) stop("Duplicate recipe library ID/version.", call. = FALSE)
  recommended <- vapply(Filter(function(x) x$status == "recommended", entries), `[[`, "", "id")
  if (anyDuplicated(recommended)) stop("Multiple recommended recipe library versions.", call. = FALSE)
  if (!length(entries)) return(entries)
  entries[order(vapply(entries, `[[`, "", "category"), vapply(entries, `[[`, "", "id"),
                numeric_version(vapply(entries, `[[`, "", "template_version")))]

}

.library_entry <- function(id, version = NULL) {
  .library_string(id, "recipe library id")
  if (!is.null(version)) .library_string(version, "recipe library version")
  entries <- Filter(function(x) identical(x$id, id) &&
    if (is.null(version)) identical(x$status, "recommended") else
      identical(x$template_version, version), .library_catalog())
  if (length(entries) != 1L) stop("Unknown installed recipe library ID/version: ", id,
    if (!is.null(version)) paste0(" / ", version), ". Use recipe_library_list().", call. = FALSE)
  entry <- entries[[1L]]
  if (identical(entry$status, "withdrawn")) stop("This recipe library version is withdrawn.", call. = FALSE)
  entry
}

# Additional catalogue constraints, never executable plan fields. Constructor
# validation remains authoritative for all native objects.
.library_rules <- function(id) {
  r <- list(select = list(choices = c("first", "first_eligible", "last", "all")),
    lookback = list(min = if (id == "panel.measurements") 0L else 1L),
    horizon = list(min = if (id %in% c("demographics.death_cause", "panel.measurements",
      "panel.incidence", "panel.sccs", "survival.first_event")) 0L else 1L),
    washout_days = list(min = if (id == "cohort.active_comparator") 1L else 0L),
    gap = list(min = 0L), persistence = list(min = 0L), grace_days = list(min = 0L),
    prior_days = list(min = 0L), start = list(min = 0L), expire = list(min = 0L),
    bin_days = list(min = 1L), age_width = list(min = 1L),
    controls_per_case = list(min = 1L), seed = list(min = 0L),
    by = list(choices = c("ingredient", "concept", "event")))
  if (id %in% c("socioeconomic.numeric_observation", "panel.measurements")) {
    r$measure <- list(numeric_event = TRUE, panel = id == "panel.measurements")
  }
  r
}

# Prove a single declared unit from the existing predicate grammar. NULL means
# unrestricted/unknown, character(0) an impossible predicate. AND intersects
# restrictions; OR is safe only when every arm restricts the same unit.
.library_unit_domain <- function(tree) {
  if (is.null(tree)) return(NULL)
  if (!is.null(tree$and)) {
    domains <- Filter(Negate(is.null), lapply(tree$and, .library_unit_domain))
    if (!length(domains)) return(NULL)
    return(Reduce(intersect, domains))
  }
  if (!is.null(tree$or)) {
    domains <- lapply(tree$or, .library_unit_domain)
    if (any(vapply(domains, is.null, logical(1)))) return(NULL)
    return(unique(unlist(domains, use.names = FALSE)))
  }
  if (!identical(tree$var, "unit_concept_id") ||
      !tree$op %in% c("==", "in")) return(NULL)
  value <- unlist(tree$value, use.names = FALSE)
  if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
      any(value < 0 | value != trunc(value))) return(NULL)
  unique(as.character(value))
}

.library_check_scale <- function(unit, where, fail) {
  if (!is.null(unit)) return(invisible(NULL)) # native unit validation is authoritative
  tree <- if (inherits(where, "omop_predicate")) where$tree else where
  # Embedded parameter expressions are resolved and checked on the emitted plan.
  has_param <- function(x) .v2_is_param(x) ||
    (is.list(x) && any(vapply(x, has_param, logical(1))))
  if (has_param(tree)) return(invisible(NULL))
  domain <- .library_unit_domain(tree)
  if (is.null(domain) || length(domain) != 1L) {
    fail(paste0("requires an explicit single scale/unit policy: supply omop_unit() ",
      "or restrict unit_concept_id to one declared unit with omop_where()"))
  }
  invisible(NULL)
}

.library_check_value <- function(value, rule, name, recipe) {
  if (is.null(value) || .v2_is_param(value)) return(invisible(NULL))
  fail <- function(message) stop("Recipe library parameter '", name, "' ", message, ".", call. = FALSE)
  if (!is.null(rule$min) && (!is.numeric(value) || length(value) != 1L ||
      is.na(value) || !is.finite(value) || value < rule$min || value != trunc(value))) {
    fail(paste0("must be an integer at least ", rule$min))
  }
  if (!is.null(rule$choices) && !value %in% unlist(rule$choices)) {
    fail(paste0("must be one of ", paste(unlist(rule$choices), collapse = ", ")))
  }
  if (isTRUE(rule$numeric_event)) {
    if (is.character(value)) {
      value <- recipe$events[[value]] %||% recipe$intervals[[value]]
      # A deferred registry reference is checked at plan emission by the
      # native compiler and by .library_check_plan().
      if (is.null(value)) return(invisible(NULL))
    }
    if (!inherits(value, "omop_event") ||
        !value$table %in% c("measurement", "observation") ||
        !identical(value$value %||% "value_as_number", "value_as_number")) {
      fail("requires a numeric observation or measurement event")
    }
    if (isTRUE(rule$panel)) {
      if (!identical(value$name, "measure")) fail("requires the source name 'measure'")
      if (!identical(value$table, "measurement")) fail("requires a measurement event")
      if (is.null(value$concepts)) fail("requires an explicit finite concept set")
    }
    .library_check_scale(value$unit, value$where, fail)
  }
  invisible(NULL)
}

.library_check_bindings <- function(recipe, values = list()) {
  if (!length(recipe$meta$recipe_library)) return(invisible(NULL))
  params <- recipe_params(recipe)
  for (entry in recipe$meta$recipe_library %||% list()) {
    for (nm in intersect(names(entry$constraints), names(params))) {
      value <- values[[nm]] %||% params[[nm]]$default
      .library_check_value(value, entry$constraints[[nm]], nm, recipe)
    }
  }
  invisible(NULL)
}

.library_checksum <- function(recipe, constraints) {
  artifact <- .recipe_plain(recipe)
  artifact$meta <- NULL
  paste(openssl::sha256(charToRaw(.jcsCanonical(list(
    recipe = artifact, constraints = constraints)))))
}

#' Discover and instantiate the installed recipe library
#'
#' The 24 versioned templates produce ordinary language-2 recipes. Discovery
#' and binding are local: no sites are contacted and no rows are retrieved.
#' Required parameters have NULL defaults and remain unbound until supplied
#' through \code{recipe_bind()}. Plan emission resolves defaults and refuses
#' missing required inputs. Clinical concept definitions are study inputs,
#' never inferred from site data.
#'
#' \code{recipe_library_describe()} includes the complete ordinary-language
#' expansion, contextual constraints, fixed policies, schema documentation,
#' provenance and fixture reference. Numeric measurement schemas can depend
#' on the declared concept set and vocabulary; discovery never estimates
#' support. Execution uses existing capability negotiation and final support
#' checks, with memory output and small-site errors by default.
#'
#' Metadata is preserved by recipe JSON/YAML export/import. The definition
#' checksum identifies the unbound artifact; the actual bound plan digest
#' determines execution identity. Edited recipes are customised derivatives;
#' their library provenance does not certify equality to a pinned template.
#' See the installed \code{recipe-library/README.md} and each entry's README
#' for definitions, fixed defaults, output grain, limitations and composition.
#'
#' @param category Exact category or vector of categories; NULL selects all.
#' @param query Case-insensitive literal substring of ID, title, summary or tags.
#' @param versions \code{"recommended"} or \code{"all"} installed versions.
#' @param id Exact catalogue ID, with no fuzzy matching.
#' @param version Exact template version; NULL selects the installed recommendation.
#' @param ... Named parameter bindings, passed to \code{recipe_bind()}.
#' @return \code{list}: a data frame ordered by category, ID and version;
#'   \code{describe}: an inspectable description list;
#'   \code{instantiate}: a fresh \code{omop_recipe}.
#' @examples
#' recipe_library_list(category = "demographics")
#' recipe_library_describe("demographics.baseline")
#' r <- recipe_library_instantiate("demographics.baseline",
#'   index = omop_event("visit_occurrence", 9201))
#' recipe_to_plan(r, version = 2L)
#' @export
recipe_library_list <- function(category = NULL, query = NULL,
                                versions = c("recommended", "all")) {
  versions <- match.arg(versions)
  if (!is.null(category)) category <- .v2_chr_vector(category, "category")
  if (!is.null(query)) .library_string(query, "query")
  entries <- Filter(function(x) (versions == "all" || x$status == "recommended") &&
    (is.null(category) || x$category %in% category) &&
    (is.null(query) || grepl(tolower(query), tolower(paste(x$id, x$title,
      x$summary, paste(unlist(x$tags), collapse = " "))), fixed = TRUE)), .library_catalog())
  fields <- c("id", "template_version", "title", "category", "summary",
              "language_version", "min_client_version", "status")
  result <- as.data.frame(setNames(lapply(fields, function(nm)
    vapply(entries, function(x) as.character(x[[nm]]), "")), fields), stringsAsFactors = FALSE)
  result$language_version <- as.integer(result$language_version)
  result$required_parameters <- I(lapply(entries, function(x) {
    p <- recipe_params(.library_build(x$id))
    names(p)[vapply(p, function(p) is.null(p$default), logical(1))]
  }))
  result$output_names <- I(lapply(entries, function(x) .library_build(x$id)$publish))
  result
}

#' @rdname recipe_library_list
#' @export
recipe_library_instantiate <- function(id, version = NULL, ...) {
  entry <- .library_entry(id, version)
  recipe <- .library_build(entry$id)
  params <- recipe_params(recipe)
  rules <- .library_rules(id)
  rules <- rules[intersect(names(rules), names(params))]
  recipe$meta$recipe_library <- list(list(id = id,
    template_version = entry$template_version,
    origin_template_version = entry$template_version,
    definition_checksum = .library_checksum(recipe, rules),
    documentation_revision = entry$documentation_revision,
    package_version = as.character(utils::packageVersion("dsOMOPClient")),
    constraints = rules,
    numeric_output = if (id == "panel.measurements") "measurements" else
      if (id == "socioeconomic.numeric_observation") "numeric_observation" else NULL,
    numeric_variable = if (id == "socioeconomic.numeric_observation") "numeric_value" else NULL,
    derivative_policy = "Edits are customised derivatives; compare actual plan digests."))
  .library_check_bindings(recipe)
  recipe_bind(recipe, ...)
}

#' @rdname recipe_library_list
#' @export
recipe_library_describe <- function(id, version = NULL) {
  entry <- .library_entry(id, version)
  recipe <- recipe_library_instantiate(id, version)
  params <- recipe_params(recipe)
  path <- file.path(.library_root(), id, entry$template_version)
  entry$template <- recipe
  entry$expanded_template <- recipe_to_code(recipe)
  entry$parameters <- params
  entry$unresolved_parameters <- names(params)[vapply(params, function(x) is.null(x$default), logical(1))]
  entry$defaults <- lapply(params, `[[`, "default")
  entry$constraints <- recipe$meta$recipe_library[[1L]]$constraints
  entry$outputs <- recipe$outputs
  entry$output_schema <- jsonlite::fromJSON(file.path(path, "output_schema.json"), simplifyVector = FALSE)
  entry$schema_binding <- "Synthetic default-binding native schema; panel.measurements columns depend on the bound finite concept set and vocabulary."
  entry$publish <- recipe$publish
  entry$documentation <- paste(readLines(file.path(path, "README.md"), warn = FALSE), collapse = "\n")
  entry$shared_semantics <- paste(readLines(file.path(.library_root(), "README.md"), warn = FALSE), collapse = "\n")
  entry$required_operations <- "Conditional on event bindings; derived by native capability preflight on the bound plan."
  entry$vocabulary_policy <- "Analyst-defined concept sets; ordinary OMOP vocabulary for descendants, mappings, ingredients, scores and labels. No clinical-row-derived schema."
  entry$schema_status <- if (id == "panel.measurements") "Vocabulary-dependent concept columns; see documentation." else "Native schema and episode grain; see documentation."
  entry$provenance <- recipe$meta$recipe_library[[1L]]
  entry$definition_checksum <- entry$provenance$definition_checksum
  class(entry) <- c("omop_recipe_library_description", "list")
  entry
}

#' @export
print.omop_recipe_library_description <- function(x, ...) {
  cat(x$id, " / ", x$template_version, "\n", x$summary, "\n\n", sep = "")
  cat(x$documentation, "\n\n", x$shared_semantics, "\n\n", sep = "")
  print(unclass(x)[setdiff(names(x), c("documentation", "shared_semantics", "template"))])
  invisible(x)
}

.library_string <- function(x, field) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(field, " must be one nonempty string.", call. = FALSE)
  invisible(x)
}

# Validate deferred source references on the actual compiled native objects.
# Metadata only selects a contextual check; it never creates or changes a node.
.library_check_plan <- function(recipe, plan) {
  for (entry in recipe$meta$recipe_library %||% list()) {
    output <- plan$outputs[[entry$numeric_output %||% ""]]
    if (is.null(output)) next
    source <- if (entry$id == "panel.measurements") output$options$sources[[1L]] else {
      variable <- plan$variables[[entry$numeric_variable %||% ""]]
      if (is.null(variable)) next
      variable$source
    }
    event <- plan$events[[source$event %||% ""]]
    if (is.null(event) || !event$table %in% c("measurement", "observation") ||
        !identical(event$value$column %||% "value_as_number", "value_as_number")) {
      stop("Recipe library measure requires a numeric observation or measurement event.", call. = FALSE)
    }
    if (entry$id == "panel.measurements" &&
        (!identical(event$table, "measurement") || is.null(event$concept_set))) {
      stop("Recipe library measure requires a measurement event with an explicit finite concept set.", call. = FALSE)
    }
    .library_check_scale(event$unit, event$where, function(message)
      stop("Recipe library measure ", message, ".", call. = FALSE))
  }
  invisible(NULL)
}
