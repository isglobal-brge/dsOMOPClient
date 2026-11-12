#' Reviewed identifier names used by the assignment compiler
#'
#' Mirrors the server's reviewed CDM 5.3/5.4 and analytic identifier names.
#' These names have typed semantics; unfamiliar id/key/identifier extensions
#' remain blocked. This does not retain row identifiers in assigned outputs:
#' the server applies its own pseudonymization and identifier-removal contract.
#' @keywords internal
.assignment_reviewed_identifiers <- function() {
  c(
    "person_id", "subject_id", "cohort_row_id", "row_id", "rowid", "set_id",
    "visit_occurrence_id", "visit_detail_id", "condition_occurrence_id",
    "drug_exposure_id", "procedure_occurrence_id", "measurement_id",
    "observation_id", "device_exposure_id", "specimen_id", "note_id",
    "note_nlp_id", "observation_period_id", "payer_plan_period_id",
    "condition_era_id", "drug_era_id", "dose_era_id", "episode_id",
    "metadata_id", "cost_id", "measurement_event_id", "observation_event_id",
    "note_event_id", "cost_event_id", "event_id", "episode_parent_id",
    "fact_id_1", "fact_id_2", "specimen_source_id", "production_id",
    "preceding_visit_occurrence_id", "preceding_visit_detail_id",
    "parent_visit_detail_id", "visit_detail_parent_id", "provider_id",
    "care_site_id", "location_id", "attribute_definition_id",
    "cohort_definition_id", "concept_class_id", "concept_id", "cost_domain_id",
    "domain_id", "relationship_id", "reverse_relationship_id",
    "source_vocabulary_id", "target_vocabulary_id", "vocabulary_id",
    "analysis_id", "case_id", "cohort_id", "covariate_id", "database_id",
    "dataset_id", "lineage_id", "outcome_id", "output_id", "population_id",
    "query_id", "release_id", "rule_id", "semantic_query_id", "snapshot_id",
    "time_id", "tool_id", "upstream_id"
  )
}

#' Is a column blocked on assignment paths?
#'
#' Mirrors the server's class-1 patterns, including untyped identifiers.
#' Birth components are permitted; an independent class-1 match still blocks
#' names such as \code{birth_date_source_value} or \code{mrn_birth_date}.
#' Aggregate admission is a separate server policy.
#' @param col Character; a column name (case-insensitive).
#' @return Logical scalar.
#' @keywords internal
.is_blocked_column <- function(col) {
  if (!is.character(col) || length(col) != 1L || is.na(col) ||
      !nzchar(trimws(col))) return(TRUE)
  col <- tolower(trimws(col))
  if (grepl("(^id$|_(id|key|identifier)$)", col) &&
      !grepl("_concept_id$", col) &&
      !col %in% .assignment_reviewed_identifiers()) return(TRUE)
  patterns <- c(
    "(^|_)patient_(id|identifier|key)($|_)",
    "(^|_)(mrn|medical_record_number|medical_record_id)($|_)",
    "(^|_)(ssn|social_security_number)($|_)",
    "(^|_)(email|email_address|e_mail)($|_)",
    "(^|_)(phone|phone_number|telephone|mobile|mobile_phone)($|_)",
    "(^|_)(comment|comments|free_text|freetext|narrative|remark|remarks)($|_)",
    "_source_value$", "_source_concept_id$", "^value_as_string$",
    "^value_source_value$", "^sig$", "^stop_reason$", "^lot_number$",
    "^unique_device_id$", "^note_text$", "^note_title$", "^note_source_value$",
    "^note_nlp_source_concept_id$", "^snippet$", "^lexical_variant$",
    "^note_nlp_concept_id$", "(^|_)term_modifiers$",
    "^address_1$", "^address_2$", "^city$", "^state$", "^zip$", "^county$",
    "^latitude$", "^longitude$", "^provider_name$", "^npi$", "^dea$",
    "^care_site_name$"
  )
  any(vapply(patterns, function(p) grepl(p, col), logical(1)))
}

#' Informational assignment quasi-identifier classification
#' @param col Character; a column name (case-insensitive).
#' @return Logical scalar; class-1 names take precedence.
#' @keywords internal
.is_quasi_identifier_column <- function(col) {
  !.is_blocked_column(col) && grepl(paste0(
    "(^|_)(dob|date_of_birth|birth_date|birth_datetime)($|_)|",
    "^(year|month|day)_of_birth$|",
    "^(age|age_group|birth_year_band)$|_date$|_datetime$"), tolower(col))
}
