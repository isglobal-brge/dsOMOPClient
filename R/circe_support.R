# Client-only inspection of the closed Circe bridge. No remote discovery.
.CIRCE_ADAPTER_VERSION <- "circe-bridge-2"
.CIRCE_MATRIX_VERSION <- "circe-support-1"

#' Inspect the supported Circe exchange subset
#'
#' The matrix distinguishes import, legacy export and native export. Conditional
#' support requires the stated subset; native expressibility does not imply an
#' export adapter. Inspection never executes a recipe or queries a server.
#' @return A static data frame of construct, import, legacy_export,
#'   native_export, conditions, reason_code, remedy and adapter_version.
#' @export
recipe_circe_support <- function() {
  constructs <- c(unname(unlist(.circe_domain_map)), "Death", "DrugEra",
    "ConditionEra", "ObservationPeriod", "VisitDetail", "ConceptSets",
    "ExcludedItems", "MixedExpansionFlags", "EntryCardinality", "FirstLast",
    "All", "Demographics", "Occurrence", "MeasurementBounds",
    "ObservationWindow", "DateOffset", "AdditionalCriteria", "Groups",
    "CensoringCriteria", "CensorWindow", "CollapseSettings", "NativeIndex")
  conditional <- c(unname(unlist(.circe_domain_map)), "ConceptSets",
    "EntryCardinality", "FirstLast", "Demographics", "Occurrence",
    "MeasurementBounds", "ObservationWindow", "DateOffset", "CollapseSettings")
  conditions <- c(rep("One mapped domain; only the declared codeset predicate.", 7L),
    rep("No adapter implemented.", 5L),
    "Strict concept IDs and Boolean flags; uniform flags; no excluded items.",
    "Excluded items have no cohort-bridge translation.",
    "Every item must have identical expansion flags.",
    "Exactly one entry criterion.", "First or Last; downstream limits First.",
    "All requires upstream ERA collapse semantics.",
    "Age integer bounds and one standard Female/Male concept.",
    "At least N or exactly zero; supported start window only.",
    "One finite gte or lt bound; occurrence at least one.",
    "Nonnegative integral PriorDays and PostDays.",
    "DateField and Offset; StartDate or EndDate with an integer offset.",
    "Additional criteria have no adapter.", "Nested CriteriaGroups have no import adapter.",
    "Censoring criteria have no adapter.", "Censor windows have no adapter.",
    "ERA with EraPad zero, and First/Last selection only.",
    "Native populations have no implemented export adapter.")
  status <- ifelse(constructs %in% conditional, "conditional", "unsupported")
  legacy <- status
  legacy[constructs == "Groups"] <- "conditional"
  conditions[constructs == "Groups"] <- "No nested import; legacy export permits one top-level OR group of supported occurrence criteria."
  conditions[constructs == "ConditionOccurrence"] <- paste0(conditions[constructs == "ConditionOccurrence"],
    " An absent ConditionType with ConditionTypeExclude=false is an explicit no-op; pinned PhenotypeLibrary 304 (manifest.json).")
  data.frame(construct = constructs, import = status, legacy_export = legacy,
    native_export = "unsupported", conditions = conditions,
    reason_code = ifelse(status == "conditional", "conditional_circe_subset", "unsupported_circe_construct"),
    remedy = "Use the declared bridge subset or retain the complete definition for an implemented adapter.",
    adapter_version = .CIRCE_ADAPTER_VERSION, stringsAsFactors = FALSE)
}

.circe_check_result <- function(issues) {
  if (!length(issues)) {
    frame <- data.frame(path = character(), construct = character(),
      reason_code = character(), remedy = character())
  } else {
    frame <- unique(do.call(rbind, issues))
    frame <- frame[order(frame$path, frame$construct, method = "radix"), , drop = FALSE]
    rownames(frame) <- NULL
  }
  list(supported = !nrow(frame), adapter_version = .CIRCE_ADAPTER_VERSION,
       matrix_version = .CIRCE_MATRIX_VERSION, issues = frame)
}

#' Check a Circe definition without execution
#'
#' Reports independent unsupported clauses in structural path order. Malformed
#' JSON, duplicate keys and exceeded size/depth limits produce a root refusal.
#' Arbitrary input values and exception payloads are never included in issues.
#' Import/export use this same checker and never return a weakened definition.
#' @param x A JSON string, file or parsed list for import; a recipe for export.
#' @param direction Exactly \code{"import"} or \code{"export"}.
#' @return A list with supported, adapter_version, matrix_version and issues.
#' @export
recipe_circe_check <- function(x, direction = "import") {
  if (!is.character(direction) || length(direction) != 1L || is.na(direction) ||
      !direction %in% c("import", "export")) {
    stop("invalid_direction: $; choose import or export.", call. = FALSE)
  }
  tryCatch(.circe_check_impl(x, direction), error = function(e) {
    .circe_check_result(list(data.frame(path = "$", construct = "Definition",
      reason_code = "invalid_definition",
      remedy = "Provide a well-formed definition within the structural size and depth limits.")))
  })
}

.circe_check_impl <- function(x, direction) {
  issues <- list()
  add <- function(path, construct, remedy, code = "unsupported_circe_construct") {
    issues[[length(issues) + 1L]] <<- data.frame(path = path, construct = construct,
      reason_code = code, remedy = remedy, stringsAsFactors = FALSE)
  }
  probe <- function(expr, path, construct, remedy) {
    tryCatch({ force(expr); TRUE }, error = function(e) {
      add(path, construct, remedy); FALSE
    })
  }
  # Unknown object names are addressed by position, never copied into errors.
  fields <- function(obj, allowed, path) {
    if (!is.list(obj) || (length(obj) && is.null(names(obj)))) {
      add(path, "Object", "Provide an object with the documented structural fields.", "invalid_definition")
      return(FALSE)
    }
    for (i in which(!names(obj) %in% allowed)) {
      add(paste0(path, "[field:", i, "]"), "UnknownField", "Remove unsupported fields only if they are not required by the study.")
    }
    TRUE
  }
  if (direction == "export") {
    if (!inherits(x, "omop_recipe")) {
      add("$", "Recipe", "Provide an omop_recipe.", "invalid_definition")
      return(.circe_check_result(issues))
    }
    for (i in seq_along(x$populations)) {
      pop <- x$populations[[i]]
      path <- paste0("$.populations[", i, "]")
      if (!is.null(pop[["index"]])) {
        add(paste0(path, ".index"), "NativeIndex", "Retain the native recipe; no native export adapter is implemented.",
            "unsupported_native_export")
        next
      }
      coverage <- pop$coverage %||% pop$circe_v2$coverage
      if (!is.null(coverage)) probe({
        stopifnot(identical(.v2w_coverage(coverage), .v2w_coverage(omop_coverage("unique"))))
      }, paste0(path, ".coverage"), "Coverage",
        "This observation policy has no Circe export adapter; retain the native recipe with recipe_save() or recipe_to_code().")
      if (!is.null(pop$setop)) add(paste0(path, ".setop"), "SetOperation", "Circe cannot export set-operation populations.")
      if (!is.null(pop$cohort_definition_id)) add(paste0(path, ".cohort_definition_id"), "CohortReference", "Use a portable definition instead of cohort_definition_id.")
      if (is.null(pop$setop) && is.null(pop$cohort_definition_id)) probe(
        .circe_primary_from_index(pop$index_event, list()), paste0(path, ".index_event"),
        "LegacyIndex", "Use an explicit omop_index_event in a mapped domain with First/Last; other domains are outside the executable subset, and All requires ERA collapse.")
      for (j in seq_along(pop$filters)) {
        part <- pop
        part$filters <- pop$filters[j]
        remedy <- "Use supported occurrence/demographic filters; visit_count is outside the executable subset."
        filter <- pop$filters[[j]]
        if (identical(filter$type, "has_measurement")) {
          if (!is.null(filter$params$safe_scope)) {
            remedy <- "Circe export cannot preserve an authenticated safe-bin contract; retain the native recipe."
          } else if (!is.null(filter$params$min_value) && !is.null(filter$params$max_value)) {
            remedy <- "Circe export cannot represent a half-open numeric range; retain the native recipe."
          }
        }
        probe(.circe_from_population(part, list()), paste0(path, ".filters[", j, "]"),
          "Filter", remedy)
      }
    }
    return(.circe_check_result(issues))
  }
  expr <- tryCatch(.ohdsi_read_definition(x), error = function(e) NULL)
  if (is.null(expr) || is.null(names(expr))) {
    add("$", "Definition", "Provide valid JSON with unique keys within the size and depth limits.", "invalid_definition")
    return(.circe_check_result(issues))
  }
  fields(expr, c("ConceptSets", "PrimaryCriteria", "QualifiedLimit", "ExpressionLimit",
    "InclusionRules", "EndStrategy", "AdditionalCriteria", "CensoringCriteria",
    "CensorWindow", "CollapseSettings", "cdmVersionRange", ".dsomop"), "$")
  for (field in c("AdditionalCriteria", "CensoringCriteria", "CensorWindow")) {
    if (.circe_has_content(expr[[field]])) add(paste0("$.", field), field,
      paste0(field, " has no implemented adapter; retain the complete definition."))
  }
  sets <- expr$ConceptSets %||% list()
  for (i in seq_along(sets)) {
    at <- paste0("$.ConceptSets[", i, "]")
    s <- sets[[i]]
    if (!fields(s, c("id", "name", "expression"), at)) next
    items <- tryCatch(.ohdsi_concept_items(s$expression), error = function(e) {
      # Only closed reason codes may cross this boundary.
      code <- strsplit(conditionMessage(e), ":", fixed = TRUE)[[1L]][[1L]]
      if (!code %in% c("invalid_concept_id", "invalid_concept_flag", "invalid_concept_item",
                       "invalid_concept_expression")) code <- "invalid_definition"
      add(paste0(at, ".expression"), "ConceptSets", "Provide strict integer concept IDs and Boolean flags.", code)
      NULL
    })
    if (is.null(items)) next
    if (any(vapply(items, `[[`, logical(1), "is_excluded"))) {
      add(paste0(at, ".expression.items"), "ExcludedItems", "The excluded concept-set items are not supported by the cohort bridge.")
    }
    for (f in c("include_descendants", "include_mapped")) {
      if (length(unique(vapply(items, `[[`, logical(1), f))) > 1L) {
        add(paste0(at, ".expression.items"), "MixedExpansionFlags", "Concept-set expansion flags must be uniform across all items.")
      }
    }
  }
  primary <- expr$PrimaryCriteria
  scan_criterion <- function(criterion, path, entry = FALSE) {
    if (!is.list(criterion)) return(invisible(NULL))
    inner <- if (entry) criterion else criterion$Criteria %||% list()
    if (!entry) {
      fields(criterion, c("Criteria", "StartWindow", "EndWindow", "Occurrence",
        "RestrictVisit", "IgnoreObservationPeriod"), path)
      for (f in c("EndWindow", "RestrictVisit", "IgnoreObservationPeriod")) {
        if (.circe_has_content(criterion[[f]]) && !identical(criterion[[f]], FALSE)) {
          add(paste0(path, ".", f), f, "This criterion option has no implemented adapter.")
        }
      }
      if (!is.null(criterion$StartWindow)) probe(.circe_window_to_recipe(criterion$StartWindow),
        paste0(path, ".StartWindow"), "StartWindow", "Use supported day offsets anchored to index start.")
    }
    if (is.list(inner)) for (k in seq_along(inner)) {
      domain <- if (is.null(names(inner))) "" else names(inner)[[k]]
      if (!domain %in% unlist(.circe_domain_map)) {
        known <- domain %in% c("Death", "DrugEra", "ConditionEra", "ObservationPeriod", "VisitDetail")
        add(paste0(path, if (entry) "" else ".Criteria", if (known) paste0(".", domain) else paste0("[field:", k, "]")),
          if (known) domain else "UnknownDomain", "This domain has no cohort-bridge adapter.")
      } else {
        spec <- if (entry) .circe_primary_spec(domain, inner[[k]]) else inner[[k]]
        if (!fields(spec, c("CodesetId", if (!entry && domain == "Measurement") "ValueAsNumber"),
          paste0(path, if (entry) "" else ".Criteria", ".", domain))) next
        at <- paste0(path, if (entry) "" else ".Criteria", ".", domain)
        probe(.circe_codeset_details(spec$CodesetId, sets), paste0(at, ".CodesetId"),
          "ConceptReference", "Use a declared concept-set ID with supported expansion semantics.")
        if (!entry && domain == "Measurement" && !is.null(spec$ValueAsNumber)) {
          # Probe bounds independently of occurrence, window and reference
          # errors. A neutral supported reference prevents another clause
          # from hiding an unsupported bound in this diagnostic pass.
          neutral <- list(list(id = 0L, expression = list(items = list(
            list(concept = list(CONCEPT_ID = 0L))))))
          probe(.circe_criteria_to_filter_unchecked(list(Criteria = list(Measurement = list(
            CodesetId = 0L, ValueAsNumber = spec$ValueAsNumber))), neutral),
            paste0(at, ".ValueAsNumber"), "MeasurementBounds",
            "Use one finite gte or lt bound; unsupported endpoint rules cannot be discarded.")
        }
        if (!entry) probe(.circe_occurrence_spec(criterion$Occurrence, domain,
          !is.null(spec$ValueAsNumber)), paste0(path, ".Occurrence"), "Occurrence",
          "Use at least N or exactly zero; measurement bounds require at least one.")
      }
    }
  }
  if (fields(primary, c("CriteriaList", "ObservationWindow", "PrimaryCriteriaLimit"), "$.PrimaryCriteria")) {
    entries <- primary$CriteriaList %||% list()
    if (length(entries) != 1L) add("$.PrimaryCriteria.CriteriaList", "EntryCardinality", "Exactly one PrimaryCriteria event is supported.")
    for (i in seq_along(entries)) {
      at <- paste0("$.PrimaryCriteria.CriteriaList[", i, "]")
      scan_criterion(entries[[i]], at, TRUE)
      probe(.circe_primary_to_index(entries[[i]], sets, "first"), at, "EntryCriterion",
        "Use exactly one supported domain with a CodesetId; Death, eras, ObservationPeriod and VisitDetail have no adapter.")
    }
    probe({
      v <- primary$PrimaryCriteriaLimit %||% list(Type = "First")
      stopifnot(is.list(v), all(names(v) %in% "Type"),
                is.character(v$Type %||% "First"), length(v$Type %||% "First") == 1L,
                tolower(v$Type %||% "First") %in% c("first", "last"))
    }, "$.PrimaryCriteria.PrimaryCriteriaLimit", "FirstLast", "Choose First/Last; All requires ERA collapse semantics.")
    ow <- primary$ObservationWindow %||% list()
    fields(ow, c("PriorDays", "PostDays"), "$.PrimaryCriteria.ObservationWindow")
    for (f in c("PriorDays", "PostDays")) probe({
      v <- ow[[f]] %||% 0L
      stopifnot(is.numeric(v), !is.object(v), length(v) == 1L, is.finite(v), v == trunc(v), v >= 0, v <= 2147483647)
    }, paste0("$.PrimaryCriteria.ObservationWindow.", f), "ObservationWindow", "Use non-negative integer observation-window days.")
  }
  for (f in c("QualifiedLimit", "ExpressionLimit")) probe({
    v <- expr[[f]] %||% list(Type = "First")
    stopifnot(is.list(v), all(names(v) %in% "Type"), identical(tolower(v$Type %||% "First"), "first"))
  }, paste0("$.", f), f, "Use First downstream limits for First/Last entry selection; All requires ERA collapse semantics.")
  probe(.circe_end_strategy_to_index(expr$EndStrategy), "$.EndStrategy", "DateOffset",
    "EndStrategy requires DateOffset with DateField and Offset; use supported StartDate/EndDate integer offsets.")
  if (.circe_has_content(expr$CollapseSettings)) probe({
    v <- expr$CollapseSettings
    stopifnot(is.list(v), all(names(v) %in% c("CollapseType", "EraPad")),
      identical(toupper(v$CollapseType), "ERA"), is.numeric(v$EraPad %||% 0L),
      identical(as.numeric(v$EraPad %||% 0L), 0))
  }, "$.CollapseSettings", "CollapseSettings", "Only default ERA collapse with EraPad = 0 and First/Last is supported.")
  # Inspect every independent clause even in a group with no executable
  # adapter. The bounded definition parser has already checked size and depth.
  scan_group <- function(ex, at) {
    if (!fields(ex, c("Type", "CriteriaList", "DemographicCriteriaList", "Groups"), at)) return(invisible(NULL))
    if (!is.character(ex$Type %||% "ALL") || length(ex$Type %||% "ALL") != 1L ||
        !toupper(ex$Type %||% "ALL") %in% c("ALL", "ANY")) {
      add(paste0(at, ".Type"), "InclusionRule", "Use ALL or ANY.")
    }
    for (j in seq_along(ex$CriteriaList)) {
      cp <- paste0(at, ".CriteriaList[", j, "]")
      scan_criterion(ex$CriteriaList[[j]], cp)
      probe(.circe_criteria_to_filter(ex$CriteriaList[[j]], sets), cp, "Occurrence",
        "Use a supported domain, at least N or exactly zero, and one-sided gte/lt measurement bounds and StartWindow.")
    }
    for (j in seq_along(ex$DemographicCriteriaList)) {
      demo <- ex$DemographicCriteriaList[[j]]
      dp <- paste0(at, ".DemographicCriteriaList[", j, "]")
      if (!fields(demo, c("Age", "Gender"), dp)) next
      for (field in intersect(c("Age", "Gender"), names(demo))) {
        probe(.circe_demographics_to_filters(list(demo[field])), paste0(dp, ".", field),
          "Demographics", "An unsupported Age operator or gender requires documented integer age bounds and one standard Female/Male concept.")
      }
    }
    if (length(ex$Groups)) {
      add(paste0(at, ".Groups"), "Groups", "The nested CriteriaGroups are not supported.")
      for (j in seq_along(ex$Groups)) scan_group(ex$Groups[[j]], paste0(at, ".Groups[", j, "]"))
    }
    if (!length(ex$CriteriaList) && !length(ex$DemographicCriteriaList) && !length(ex$Groups)) {
      add(at, "InclusionRule", "Empty inclusion rules are not supported.")
    }
    invisible(NULL)
  }
  for (i in seq_along(expr$InclusionRules)) {
    rule <- expr$InclusionRules[[i]]
    at <- paste0("$.InclusionRules[", i, "]")
    if (!fields(rule, c("name", "expression"), at)) next
    scan_group(rule$expression %||% list(), paste0(at, ".expression"))
  }
  # Final parser agreement catches relationships between otherwise valid clauses.
  if (!length(issues)) probe(.circe_import_unchecked(expr), "$", "Definition",
    "Use the complete documented executable subset; this combination has no lossless translation.")
  .circe_check_result(issues)
}

# ATLAS emits this false switch even without a ConditionType predicate. It is
# an explicit no-op only in that exact form; true/null/arrays still refuse.
.circe_primary_spec <- function(domain, spec) {
  if (identical(domain, "ConditionOccurrence") && is.list(spec) &&
      !"ConditionType" %in% names(spec) && identical(spec$ConditionTypeExclude, FALSE)) {
    spec$ConditionTypeExclude <- NULL
  }
  spec
}

.circe_require_supported <- function(check) {
  if (!isTRUE(check$supported)) {
    i <- check$issues[1L, ]
    stop(i$reason_code, ": ", i$path, "; ", i$remedy,
      " Inspect recipe_circe_check() for all structural issues.", call. = FALSE)
  }
  invisible(check)
}

.circe_hash <- function(x) unclass(as.character(openssl::sha256(charToRaw(enc2utf8(x)))))

.circe_population_digest <- function(pop) {
  pop$circe_provenance <- NULL
  .circe_hash(.jcsCanonical(.recipe_strip_classes(pop)))
}

.circe_origin <- function(origin) {
  if (is.null(origin)) return(list())
  fields <- c("uri", "phenotype_id", "release", "review_status", "deprecation_status")
  if (!is.list(origin) || is.null(names(origin)) || anyDuplicated(names(origin)) ||
      any(!names(origin) %in% fields) || any(!vapply(origin, function(v) {
        is.character(v) && length(v) == 1L && !is.na(v) && nchar(v, type = "bytes") <= 4096L
      }, logical(1)))) {
    stop("invalid_origin: $; provide declared scalar public provenance fields only.", call. = FALSE)
  }
  if (!is.null(origin$uri)) {
    # Strip userinfo, query and fragment; never fetch this declaration.
    origin$uri <- sub("[?#].*$", "", sub("^(https?://)[^/]*@", "\\1", origin$uri))
    if (!grepl("^https?://[^/@[:space:]]+(/[^[:space:]]*)?$", origin$uri)) {
      stop("invalid_origin: $.uri; provide a public HTTP(S) source URI.", call. = FALSE)
    }
  }
  origin[intersect(fields, names(origin))]
}

.circe_provenance <- function(input, expr, pop, origin) {
  origin <- .circe_origin(origin)
  if (is.character(input)) {
    bytes <- if (nchar(input, type = "bytes") < 4096L && file.exists(input)) {
      rawToChar(readBin(input, "raw", n = file.info(input)$size))
    } else as.character(input)
    form <- "json"
  } else {
    bytes <- .jcsCanonical(expr)
    form <- "list"
  }
  semantic <- expr
  semantic$.dsomop <- NULL
  digest <- .circe_population_digest(pop)
  list(original_json = bytes, original_sha256 = .circe_hash(bytes), origin_form = form,
    origin = origin, adapter_version = .CIRCE_ADAPTER_VERSION,
    supported_subset_version = .CIRCE_MATRIX_VERSION,
    original_semantic_digest = .circe_hash(.jcsCanonical(semantic)),
    original_recipe_digest = digest, recipe_digest = digest, derivative = FALSE)
}

.circe_refresh_provenance <- function(pop) {
  if (!is.null(pop$circe_provenance)) {
    digest <- .circe_population_digest(pop)
    pop$circe_provenance$recipe_digest <- digest
    pop$circe_provenance$derivative <- !identical(digest, pop$circe_provenance$original_recipe_digest)
  }
  pop
}

# An import's v2 shadow is a compatibility translation, not an independent
# authority over later public edits. Rebuild it from an edited legacy
# declaration, or honor an explicit native index. Conflicting edits to both
# representations require an explicit choice instead of silently losing one.
# Coverage is an orthogonal native override: .v2w_population always prefers the
# explicit public declaration to the translated default, including after edits.
.circe_reconcile_population <- function(pop) {
  if (is.null(pop$circe_v2)) return(pop)
  if (!is.null(pop[["index"]])) {
    pop$circe_v2 <- NULL
    return(pop)
  }
  provenance <- pop$circe_provenance
  if (is.null(provenance$original_json)) return(pop)
  baseline <- .circe_import_unchecked(provenance$original_json, id = pop$id, label = pop$label)
  legacy <- function(p) .recipe_strip_classes(list(index_event = p$index_event, filters = p$filters))
  legacy_digest <- .circe_hash(.jcsCanonical(legacy(pop)))
  prior_legacy <- provenance$translation_legacy_digest %||%
    .circe_hash(.jcsCanonical(legacy(baseline)))
  if (identical(legacy_digest, prior_legacy)) return(pop)
  prior_shadow <- provenance$translation_digest %||%
    .circe_hash(.jcsCanonical(.recipe_strip_classes(baseline$circe_v2)))
  if (!identical(.circe_hash(.jcsCanonical(.recipe_strip_classes(pop$circe_v2))), prior_shadow)) {
    stop("ambiguous_circe_edit: both imported representations changed; choose an explicit native index or remove circe_v2 to use the edited legacy declaration.", call. = FALSE)
  }
  # The public exporter/checker refuses any edit outside its supported subset.
  # This is local translation only: no server, patient query or remote fetch.
  legacy_pop <- pop
  legacy_pop$coverage <- NULL
  recipe <- omop_recipe(populations = legacy_pop)
  translated <- .circe_import_unchecked(recipe_export_circe(recipe, pop$id),
                                        id = pop$id, label = pop$label)
  pop$circe_v2 <- translated$circe_v2
  pop$circe_provenance$translation_legacy_digest <- legacy_digest
  pop$circe_provenance$translation_digest <- .circe_hash(.jcsCanonical(.recipe_strip_classes(pop$circe_v2)))
  pop
}
