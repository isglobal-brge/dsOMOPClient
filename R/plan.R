# Module: Extraction Plan
# Client-side functions for creating, validating, previewing, and managing
# extraction plans that define multi-table data retrieval from OMOP CDM.

#' Create a new extraction plan
#'
#' Initialises an empty \code{omop_plan} object that serves as the container
#' for cohort definitions, output specifications, and plan-wide options.
#' Build up the plan by piping it through \code{ds.omop.plan.*} helpers
#' such as \code{\link{ds.omop.plan.cohort}},
#' \code{\link{ds.omop.plan.baseline}}, and
#' \code{\link{ds.omop.plan.events}}.
#'
#' @return An \code{omop_plan} object (a list with class
#'   \code{c("omop_plan", "list")}) containing empty slots for cohort,
#'   anchor, outputs, and options.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.baseline(plan)
#' }
#' @seealso \code{\link{ds.omop.plan.execute}}, \code{\link{print.omop_plan}}
#' @export
ds.omop.plan <- function() {
  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(),
    options = list(
      translate_concepts = FALSE,
      block_sensitive = TRUE,
      min_persons = NULL
    )
  )
  class(plan) <- c("omop_plan", "list")
  plan
}

#' Set a cohort filter on the plan
#'
#' Attaches a cohort definition to the plan, restricting all downstream
#' outputs to persons who belong to the specified cohort. Exactly one of
#' \code{cohort_definition_id} or \code{spec} must be provided. Use
#' \code{cohort_definition_id} to reference an existing cohort table row,
#' or \code{spec} to define a cohort inline using the DSL.
#'
#' @param plan An \code{omop_plan} object.
#' @param cohort_definition_id Integer; ID of an existing cohort in the
#'   cohort table. Mutually exclusive with \code{spec}.
#' @param spec Named list; inline cohort specification DSL describing
#'   inclusion criteria. Mutually exclusive with \code{cohort_definition_id}.
#' @return The modified \code{omop_plan} with the cohort slot populated.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 42)
#'
#' # Or with an inline spec
#' plan <- ds.omop.plan.cohort(plan, spec = list(
#'   sex = "Female", age_range = c(40, 65)
#' ))
#' }
#' @seealso \code{\link{ds.omop.plan}}, \code{\link{ds.omop.plan.cohort_membership}}
#' @export
ds.omop.plan.cohort <- function(plan,
                                cohort_definition_id = NULL,
                                spec = NULL) {
  if (!is.null(cohort_definition_id)) {
    plan$cohort <- list(
      type = "cohort_table",
      cohort_definition_id = as.integer(cohort_definition_id)
    )
  } else if (!is.null(spec)) {
    plan$cohort <- list(
      type = "spec",
      spec = spec
    )
  }
  plan
}

#' Add a baseline demographics output to the plan
#'
#' Produces one row per cohort member with demographics from the person
#' table and optional derived fields. Requires a cohort to be set.
#' This is the recommended way to retrieve person-level demographic
#' variables when a cohort has been defined, because it can compute
#' cohort-relative derived fields such as age at index.
#'
#' @param plan An \code{omop_plan} object.
#' @param columns Character vector; person-table columns to include
#'   (e.g. \code{"gender_concept_id"}, \code{"year_of_birth"},
#'   \code{"race_concept_id"}).
#' @param derived Character vector; derived fields to compute.
#'   Supported values include \code{"age_at_index"},
#'   \code{"prior_observation"}, and \code{"future_observation"}.
#' @param name Character; output name used as a key in the plan's
#'   outputs list and as the default symbol name on the server.
#' @return The modified \code{omop_plan} with the baseline output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.baseline(plan,
#'   columns = c("gender_concept_id", "year_of_birth"),
#'   derived = c("age_at_index", "prior_observation")
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.person_level}},
#'   \code{\link{ds.omop.plan.cohort}}
#' @export
ds.omop.plan.baseline <- function(plan,
                                  columns = c("gender_concept_id",
                                              "year_of_birth",
                                              "race_concept_id"),
                                  derived = c("age_at_index"),
                                  name = "baseline") {
  plan$outputs[[name]] <- list(
    type = "baseline",
    columns = columns,
    derived = derived
  )
  plan
}

#' Add person-level tables to the plan (raw join)
#'
#' Joins one or more tables by person_id and merges into a single
#' wide data.frame. For cohort-aware demographics with derived fields,
#' use \code{\link{ds.omop.plan.baseline}} instead. This output type
#' is useful when you need raw columns from multiple OMOP tables
#' without cohort-relative computations.
#'
#' @param plan An \code{omop_plan} object.
#' @param tables Named list; each element maps a table name to a
#'   character vector of column names to include, e.g.
#'   \code{list(person = c("gender_concept_id"), visit_occurrence = c("visit_concept_id"))}.
#' @param name Character; output name used as a key in the plan's
#'   outputs list and as the default symbol name on the server.
#' @return The modified \code{omop_plan} with the person-level output
#'   appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.person_level(plan,
#'   tables = list(
#'     person = c("gender_concept_id", "year_of_birth"),
#'     visit_occurrence = c("visit_concept_id", "visit_type_concept_id")
#'   ),
#'   name = "demographics"
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.baseline}},
#'   \code{\link{ds.omop.plan.events}}
#' @export
ds.omop.plan.person_level <- function(plan, tables,
                                      name = "person_data") {
  plan$outputs[[name]] <- list(
    type = "person_level",
    tables = tables
  )
  plan
}

#' Add a survival (time-to-event) output to the plan
#'
#' Produces one row per cohort member with an event indicator (0/1) and
#' time-to-event in days. No calendar dates appear in the output, making
#' it safe for federated disclosure control. Requires a cohort to be set.
#'
#' @param plan An \code{omop_plan} object.
#' @param outcome_table Character; OMOP table containing outcome events
#'   (e.g. \code{"condition_occurrence"}, \code{"procedure_occurrence"}).
#' @param outcome_concepts Numeric vector; concept IDs that define the
#'   outcome event.
#' @param tar Named list; time-at-risk window with \code{start_offset}
#'   and \code{end_offset} (integer days relative to cohort_start_date).
#' @param event_order Character; \code{"first"} or \code{"last"} to
#'   select which event occurrence determines the time-to-event value.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @return The modified \code{omop_plan} with the survival output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.survival(plan,
#'   outcome_table = "condition_occurrence",
#'   outcome_concepts = c(201826, 443238),
#'   tar = list(start_offset = 0, end_offset = 365),
#'   event_order = "first"
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.events}},
#'   \code{\link{ds.omop.plan.cohort}}
#' @export
ds.omop.plan.survival <- function(plan,
                                  outcome_table = "condition_occurrence",
                                  outcome_concepts,
                                  tar = list(start_offset = 0,
                                             end_offset = 730),
                                  event_order = "first",
                                  name = "survival") {
  plan$outputs[[name]] <- list(
    type = "survival",
    outcome = list(
      table = outcome_table,
      concept_set = as.integer(outcome_concepts)
    ),
    tar = tar,
    event_order = event_order
  )
  plan
}

#' Add a concept dictionary output to the plan
#'
#' Scans other outputs in the plan for concept IDs and produces a lookup
#' table with concept names, domains, vocabulary IDs, and which outputs
#' reference each concept. Useful for translating numeric concept IDs in
#' other output tables into human-readable labels.
#'
#' @param plan An \code{omop_plan} object.
#' @param source_outputs Character vector; names of outputs to scan for
#'   concept IDs. If \code{NULL} (the default), all non-dictionary outputs
#'   in the plan are scanned.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @return The modified \code{omop_plan} with the concept dictionary
#'   output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.events(plan, "conditions", "condition_occurrence",
#'   concept_set = c(201826))
#' plan <- ds.omop.plan.concept_dictionary(plan)
#' }
#' @seealso \code{\link{ds.omop.plan.options}}
#' @export
ds.omop.plan.concept_dictionary <- function(plan,
                                             source_outputs = NULL,
                                             name = "concept_dictionary") {
  plan$outputs[[name]] <- list(
    type = "concept_dictionary",
    source_outputs = source_outputs
  )
  plan
}

#' Add an event-level extraction to the plan
#'
#' Extracts rows from a single OMOP clinical data table, optionally
#' filtered by concept set, time window, temporal specification, and
#' custom filters. The output format is controlled by the
#' \code{representation} parameter (long, wide, or features).
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @param table Character; source OMOP table name
#'   (e.g. \code{"condition_occurrence"}, \code{"drug_exposure"}).
#' @param columns Character vector; columns to include from the table.
#'   If \code{NULL}, the server selects default columns.
#' @param concept_set Numeric vector or concept set spec; concept IDs
#'   used to filter rows via the standard concept ID column of the table.
#' @param time_window Named list with \code{start_date} and
#'   \code{end_date} for calendar-based filtering.
#' @param temporal An \code{omop_temporal_spec} object or list; temporal
#'   filtering relative to a cohort index date. See
#'   \code{\link{omop.temporal}}.
#' @param date_handling A list; date handling specification controlling
#'   how date columns are transformed. See \code{\link{omop.date_handling}}.
#' @param filters Named list; additional custom filter DSL expressions.
#' @param representation Named list with \code{format} (one of
#'   \code{"long"}, \code{"wide"}, \code{"features"}) and optional
#'   format-specific settings.
#' @return The modified \code{omop_plan} with the event-level output
#'   appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.events(plan,
#'   name = "conditions",
#'   table = "condition_occurrence",
#'   concept_set = c(201826, 443238),
#'   temporal = omop.temporal(index_window = list(start = -365, end = 0)),
#'   date_handling = omop.date_handling(mode = "relative")
#' )
#' }
#' @seealso \code{\link{omop.temporal}}, \code{\link{omop.date_handling}},
#'   \code{\link{ds.omop.plan.features}}
#' @export
ds.omop.plan.events <- function(plan, name, table,
                                columns = NULL,
                                concept_set = NULL,
                                time_window = NULL,
                                temporal = NULL,
                                date_handling = NULL,
                                filters = NULL,
                                representation = list(
                                  format = "long")) {
  output <- list(
    type = "event_level",
    table = table,
    columns = columns,
    representation = representation
  )

  output$filters <- list()
  if (!is.null(concept_set)) {
    output$filters$concept_set <- list(ids = concept_set)
    output$concept_set <- concept_set
  }
  if (!is.null(time_window)) {
    output$filters$time_window <- time_window
  }
  if (!is.null(filters)) {
    output$filters$custom <- filters
  }
  if (!is.null(temporal)) {
    output$temporal <- temporal
  }
  if (!is.null(date_handling)) {
    output$date_handling <- date_handling
  }

  plan$outputs[[name]] <- output
  plan
}

#' Add feature extraction with feature specifications
#'
#' Adds a feature-extraction output that computes person-level summary
#' columns (boolean, count, mean, etc.) from event-level data in a
#' single OMOP table. Each \code{omop_feature_spec} in \code{specs}
#' produces one column in the resulting data frame. Concept IDs are
#' automatically collected from all specs for server-side filtering.
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @param table Character; source OMOP table name
#'   (e.g. \code{"condition_occurrence"}, \code{"measurement"}).
#' @param specs Named list of \code{omop_feature_spec} objects created
#'   by the \code{omop.feature.*} family of functions (e.g.
#'   \code{\link{omop.feature.boolean}}, \code{\link{omop.feature.count}}).
#' @return The modified \code{omop_plan} with the features output
#'   appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.features(plan,
#'   name = "lab_features",
#'   table = "measurement",
#'   specs = list(
#'     has_glucose = omop.feature.boolean(c(3004410)),
#'     glucose_mean = omop.feature.mean_value(c(3004410))
#'   )
#' )
#' }
#' @seealso \code{\link{omop.feature.boolean}},
#'   \code{\link{omop.feature.count}},
#'   \code{\link{ds.omop.plan.events}}
#' @export
ds.omop.plan.features <- function(plan, name, table,
                                  specs) {
  output <- list(
    type = "event_level",
    table = table,
    representation = list(
      format = "features",
      features = specs
    )
  )

  # Collect concept IDs from all specs
  all_concepts <- unique(unlist(lapply(specs, function(s) {
    cs <- s$concept_set
    if (is.list(cs) && !is.null(cs$concepts)) cs$concepts
    else as.integer(cs)
  })))

  if (length(all_concepts) > 0) {
    output$filters <- list(
      concept_set = list(ids = all_concepts)
    )
    output$concept_set <- all_concepts
  }

  plan$outputs[[name]] <- output
  plan
}

#' Add an outcome extraction (convenience wrapper)
#'
#' Convenience function that wraps \code{\link{ds.omop.plan.events}} with
#' \code{representation = list(format = "features")} to produce a
#' person-level binary outcome indicator for the given concept set.
#' This is a shorthand for defining an event-level features output
#' focused on outcome identification.
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @param concept_set Numeric vector; concept IDs that define the
#'   outcome event.
#' @param table Character; source OMOP table containing the outcome
#'   events.
#' @return The modified \code{omop_plan} with the outcome output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.outcome(plan,
#'   name = "diabetes_outcome",
#'   concept_set = c(201826),
#'   table = "condition_occurrence"
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.events}},
#'   \code{\link{ds.omop.plan.survival}}
#' @export
ds.omop.plan.outcome <- function(plan, name, concept_set,
                                 table = "condition_occurrence") {
  ds.omop.plan.events(
    plan, name = name, table = table,
    concept_set = concept_set,
    representation = list(format = "features")
  )
}

#' Add a cohort membership output to the plan
#'
#' Produces the standard OHDSI cohort table as a named output with
#' row_id, subject_id, cohort_definition_id, cohort_start_date, and
#' cohort_end_date. Requires a cohort to be set on the plan via
#' \code{\link{ds.omop.plan.cohort}}. This is useful when you need
#' the raw cohort membership data alongside other outputs.
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @return The modified \code{omop_plan} with the cohort membership
#'   output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.cohort_membership(plan, name = "my_cohort")
#' }
#' @seealso \code{\link{ds.omop.plan.cohort}},
#'   \code{\link{ds.omop.plan.baseline}}
#' @export
ds.omop.plan.cohort_membership <- function(plan,
                                            name = "cohort_membership") {
  plan$outputs[[name]] <- list(
    type = "cohort_membership"
  )
  plan
}

#' Add an intervals (long) output to the plan
#'
#' Extracts interval data (observation periods, visits, drug or condition
#' durations) with start and end days relative to the cohort index date.
#' Requires a cohort to be set. The output contains one row per interval
#' per person, with columns for table source, start day, end day, and
#' optionally concept IDs filtered by \code{concept_filter}.
#'
#' @param plan An \code{omop_plan} object.
#' @param tables Character vector; OMOP tables to extract intervals from.
#'   Defaults to observation_period, visit_occurrence, drug_exposure,
#'   and condition_occurrence.
#' @param concept_filter Named list; per-table concept ID filters where
#'   each element maps a table name to a numeric vector of concept IDs.
#'   If \code{NULL}, no concept filtering is applied.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @return The modified \code{omop_plan} with the intervals output
#'   appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.intervals(plan,
#'   tables = c("visit_occurrence", "drug_exposure"),
#'   concept_filter = list(drug_exposure = c(1127078, 1127433))
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.events}},
#'   \code{\link{ds.omop.plan.temporal_covariates}}
#' @export
ds.omop.plan.intervals <- function(plan,
                                    tables = c("observation_period",
                                               "visit_occurrence",
                                               "drug_exposure",
                                               "condition_occurrence"),
                                    concept_filter = NULL,
                                    name = "intervals") {
  plan$outputs[[name]] <- list(
    type = "intervals_long",
    tables = tables,
    concept_filter = concept_filter
  )
  plan
}

#' Add a temporal (time-binned) covariates output to the plan
#'
#' Produces FeatureExtraction-compatible sparse covariates binned into
#' time windows relative to the cohort index date. Returns three symbols
#' on the server: \code{<name>.temporalCovariates},
#' \code{<name>.covariateRef}, and \code{<name>.timeRef}. Requires a
#' cohort to be set.
#'
#' @param plan An \code{omop_plan} object.
#' @param table Character; source OMOP table to extract covariates from.
#' @param concept_set Numeric vector; concept IDs to include in the
#'   covariate computation.
#' @param bin_width Integer; width of each time bin in days.
#' @param window_start Integer; start of the observation window in days
#'   relative to the cohort index date (negative = before index).
#' @param window_end Integer; end of the observation window in days
#'   relative to the cohort index date (0 = index date).
#' @param analyses Character vector; types of analyses to compute.
#'   Supported values include \code{"binary"} and \code{"count"}.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @return The modified \code{omop_plan} with the temporal covariates
#'   output appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
#' plan <- ds.omop.plan.temporal_covariates(plan,
#'   table = "condition_occurrence",
#'   concept_set = c(201826, 443238),
#'   bin_width = 30L,
#'   window_start = -365L,
#'   window_end = 0L,
#'   analyses = c("binary", "count")
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.intervals}},
#'   \code{\link{ds.omop.plan.features}}
#' @export
ds.omop.plan.temporal_covariates <- function(plan,
                                              table,
                                              concept_set,
                                              bin_width = 30L,
                                              window_start = -365L,
                                              window_end = 0L,
                                              analyses = c("binary"),
                                              name = "temporal") {
  plan$outputs[[name]] <- list(
    type = "temporal_covariates",
    table = table,
    concept_set = as.integer(concept_set),
    bin_width = as.integer(bin_width),
    window_start = as.integer(window_start),
    window_end = as.integer(window_end),
    analyses = analyses
  )
  plan
}

#' Set plan-wide options
#'
#' Configures global options that apply to all outputs in the plan.
#' Only non-NULL arguments are updated; existing option values are
#' preserved for omitted arguments.
#'
#' @param plan An \code{omop_plan} object.
#' @param translate_concepts Logical; if \code{TRUE}, concept ID columns
#'   are automatically translated to human-readable concept names in
#'   output tables.
#' @param block_sensitive Logical; if \code{TRUE}, sensitive columns
#'   (e.g. exact dates, free-text notes) are excluded from outputs.
#' @param min_persons Integer; minimum person count threshold for
#'   disclosure control. Cells with fewer persons are suppressed.
#'   If \code{NULL}, no suppression is applied.
#' @return The modified \code{omop_plan} with updated options.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.options(plan,
#'   translate_concepts = TRUE,
#'   block_sensitive = TRUE,
#'   min_persons = 5L
#' )
#' }
#' @seealso \code{\link{ds.omop.plan}}, \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.options <- function(plan,
                                 translate_concepts = NULL,
                                 block_sensitive = NULL,
                                 min_persons = NULL) {
  if (!is.null(translate_concepts)) {
    plan$options$translate_concepts <- translate_concepts
  }
  if (!is.null(block_sensitive)) {
    plan$options$block_sensitive <- block_sensitive
  }
  if (!is.null(min_persons)) {
    plan$options$min_persons <- min_persons
  }
  plan
}

#' Build a temporal filtering specification
#'
#' Creates an \code{omop_temporal_spec} object that defines how events
#' are filtered relative to a cohort index date or calendar dates. The
#' spec can combine index-relative windows, calendar date ranges, event
#' selection (first/last N), and minimum gap requirements.
#'
#' @param index_window Named list with \code{start} and \code{end}
#'   (integer days relative to the cohort index date). Negative values
#'   denote time before the index date.
#' @param calendar Named list with \code{start} and \code{end}
#'   (character ISO 8601 dates, e.g. \code{"2020-01-01"}).
#' @param event_select Named list with \code{order} (\code{"first"} or
#'   \code{"last"}) and \code{n} (integer; number of events to keep per
#'   person).
#' @param min_gap Integer; minimum number of days between events. Events
#'   closer together are collapsed.
#' @return An \code{omop_temporal_spec} object (a list with class
#'   \code{c("omop_temporal_spec", "list")}).
#' @examples
#' \dontrun{
#' # Events within 1 year before index, keep first 3 per person
#' temporal <- omop.temporal(
#'   index_window = list(start = -365, end = 0),
#'   event_select = list(order = "first", n = 3)
#' )
#' plan <- ds.omop.plan.events(plan, "conditions",
#'   "condition_occurrence", temporal = temporal)
#' }
#' @seealso \code{\link{ds.omop.plan.events}},
#'   \code{\link{omop.date_handling}}
#' @export
omop.temporal <- function(index_window = NULL, calendar = NULL,
                          event_select = NULL, min_gap = NULL) {
  spec <- list()
  if (!is.null(index_window)) spec$index_window <- index_window
  if (!is.null(calendar)) spec$calendar <- calendar
  if (!is.null(event_select)) spec$event_select <- event_select
  if (!is.null(min_gap)) spec$min_gap <- min_gap
  class(spec) <- c("omop_temporal_spec", "list")
  spec
}

#' Build a date handling specification
#'
#' Creates a specification controlling how date columns are transformed
#' in event-level outputs. Dates can be kept as-is, converted to
#' relative days from an index date, binned into calendar periods, or
#' removed entirely for privacy.
#'
#' @param mode Character; transformation mode. One of \code{"absolute"}
#'   (keep original dates), \code{"relative"} (convert to days from
#'   reference), \code{"binned"} (aggregate into calendar bins), or
#'   \code{"remove"} (drop all date columns).
#' @param reference Character; reference point for relative mode.
#'   Currently only \code{"index"} (cohort index date) is supported.
#' @param bin_width Character; bin granularity for binned mode. One of
#'   \code{"week"}, \code{"month"}, or \code{"year"}.
#' @param date_columns Character vector; specific date columns to
#'   transform. If \code{NULL}, all date columns in the output are
#'   transformed.
#' @return A list with elements \code{mode}, \code{reference},
#'   \code{bin_width}, and \code{date_columns}.
#' @examples
#' \dontrun{
#' # Convert dates to days relative to cohort index
#' dh <- omop.date_handling(mode = "relative", reference = "index")
#'
#' # Bin dates by month, remove exact dates
#' dh <- omop.date_handling(mode = "binned", bin_width = "month")
#'
#' plan <- ds.omop.plan.events(plan, "conditions",
#'   "condition_occurrence", date_handling = dh)
#' }
#' @seealso \code{\link{ds.omop.plan.events}},
#'   \code{\link{omop.temporal}}
#' @export
omop.date_handling <- function(mode = "absolute", reference = "index",
                               bin_width = NULL, date_columns = NULL) {
  list(mode = mode, reference = reference,
       bin_width = bin_width, date_columns = date_columns)
}

#' Validate an extraction plan
#'
#' Sends the plan to each connected server for structural validation,
#' checking for missing required fields, invalid table references,
#' unsupported output types, and schema compatibility issues. This
#' performs a server-side check (via \code{omopPlanPreviewDS}) but does
#' not execute the plan or create any data. Use this to catch errors
#' before calling \code{\link{ds.omop.plan.execute}}.
#'
#' Note: despite the name difference, both \code{ds.omop.plan.validate}
#' and \code{\link{ds.omop.plan.preview}} currently call the same
#' server-side endpoint (\code{omopPlanPreviewDS}). Use
#' \code{ds.omop.plan.validate} when you want a pass/fail check, and
#' \code{ds.omop.plan.preview} when you want to inspect expected schemas
#' and row estimates.
#'
#' @param plan An \code{omop_plan} object.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return A named list (one element per server) containing validation
#'   results with expected output schemas, row estimates, and any
#'   warnings or errors.
#' @examples
#' \dontrun{
#' result <- ds.omop.plan.validate(my_plan)
#' # Check a specific server's result
#' result$server1$valid
#' result$server1$errors
#' }
#' @seealso \code{\link{ds.omop.plan.preview}},
#'   \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.validate <- function(plan, symbol = "omop",
                                  conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, plan)
  )
}

#' Preview a plan (server-side dry run)
#'
#' Sends the plan to each connected server for a dry-run preview that
#' returns expected output schemas, estimated row counts, and the SQL
#' queries that would be executed, without actually creating any data.
#' This is useful for verifying that the plan will produce the expected
#' structure before committing to a full execution.
#'
#' Note: both \code{\link{ds.omop.plan.validate}} and
#' \code{ds.omop.plan.preview} currently call the same server-side
#' endpoint (\code{omopPlanPreviewDS}). The distinction is semantic:
#' use validate for pass/fail checking, and preview for inspecting
#' expected output details.
#'
#' @param plan An \code{omop_plan} object.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return A named list (one element per server) containing preview
#'   information including expected column names and types, estimated
#'   row counts, and generated SQL queries for each output.
#' @examples
#' \dontrun{
#' preview <- ds.omop.plan.preview(my_plan)
#' # Inspect expected columns for the "baseline" output
#' preview$server1$outputs$baseline$columns
#' # Check estimated row count
#' preview$server1$outputs$baseline$estimated_rows
#' }
#' @seealso \code{\link{ds.omop.plan.validate}},
#'   \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.preview <- function(plan, symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, plan)
  )
}

#' Execute a plan and create server-side tables
#'
#' Sends the plan to each connected server for full execution. The
#' server-side \code{omopPlanExecuteDS} function processes the plan and
#' assigns each output directly into the DataSHIELD session as named
#' symbols specified in the \code{out} mapping. After execution, the
#' symbols can be used with standard DataSHIELD analysis functions.
#' Sparse outputs (e.g. temporal covariates) are split into multiple
#' symbols: \code{<name>.covariates} and \code{<name>.covariateRef}.
#'
#' @param plan An \code{omop_plan} object.
#' @param out Named character vector; maps output names (as defined in
#'   the plan) to server-side symbol names. For example,
#'   \code{c(baseline = "D_base", survival = "D_tte")} assigns the
#'   \code{baseline} output to symbol \code{D_base}.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return Invisible; the \code{out} symbol mapping (for chaining).
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#' plan <- ds.omop.plan.events(plan, "conditions",
#'   "condition_occurrence", concept_set = c(201826))
#' ds.omop.plan.execute(plan,
#'   out = c(baseline = "D_base", conditions = "D_cond")
#' )
#' # Now use D_base and D_cond with ds.summary(), ds.table(), etc.
#' }
#' @seealso \code{\link{ds.omop.plan.validate}},
#'   \code{\link{ds.omop.plan.preview}}
#' @export
ds.omop.plan.execute <- function(plan, out,
                                 symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Single assign call: server assigns each output directly into session
  exec_symbol <- .generate_symbol("dsOexec")

  DSI::datashield.assign.expr(
    conns,
    symbol = exec_symbol,
    expr = call("omopPlanExecuteDS",
                session$res_symbol, plan, out)
  )

  # exec_symbol holds TRUE (return value); clean up
  tryCatch(
    DSI::datashield.rm(conns, exec_symbol),
    error = function(e) NULL
  )

  invisible(out)
}

#' Harmonize a plan for multi-server execution
#'
#' Adjusts a plan so that it is compatible across all connected servers
#' by resolving schema differences. In \code{"intersection"} mode,
#' person-level outputs are trimmed to include only tables present on
#' all servers, and warnings are issued for event-level outputs that
#' reference missing tables. This ensures consistent output structure
#' when executing on heterogeneous OMOP CDM deployments.
#'
#' @param plan An \code{omop_plan} object.
#' @param mode Character; harmonization strategy. \code{"intersection"}
#'   keeps only tables common to all servers. \code{"union_with_missing"}
#'   keeps all tables and fills missing columns with NA.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return The harmonized \code{omop_plan} with outputs adjusted for
#'   cross-server compatibility.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.person_level(plan,
#'   tables = list(person = c("gender_concept_id"),
#'                 measurement = c("value_as_number")))
#' plan <- ds.omop.plan.harmonize(plan, mode = "intersection")
#' }
#' @seealso \code{\link{ds.omop.plan.execute}},
#'   \code{\link{ds.omop.compare}}
#' @export
ds.omop.plan.harmonize <- function(plan,
                                   mode = "intersection",
                                   symbol = "omop",
                                   conns = NULL) {
  comparison <- ds.omop.compare(symbol, conns)

  if (mode == "intersection") {
    common_tables <- comparison$common_tables
    for (out_name in names(plan$outputs)) {
      out <- plan$outputs[[out_name]]
      if (out$type == "person_level" && !is.null(out$tables)) {
        plan$outputs[[out_name]]$tables <-
          out$tables[tolower(names(out$tables)) %in%
                       common_tables]
      }
      if (out$type == "event_level") {
        tbl <- tolower(out$table %||% "")
        if (!tbl %in% common_tables) {
          warning("Output '", out_name,
                  "' references table '", tbl,
                  "' not present on all servers.")
        }
      }
    }
  }

  plan
}

#' Print method for extraction plans
#'
#' Displays a human-readable summary of an \code{omop_plan} including
#' the cohort definition, all configured outputs with their types and
#' key parameters, and plan-wide options.
#'
#' @param x An \code{omop_plan} object.
#' @param ... Additional arguments (ignored).
#' @return Invisible \code{x}, for use in pipelines.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#' print(plan)
#' }
#' @export
#' @method print omop_plan
print.omop_plan <- function(x, ...) {
  cat("=== dsOMOP Extraction Plan ===\n")

  if (!is.null(x$cohort)) {
    cat("Cohort: ",
        if (!is.null(x$cohort$cohort_definition_id))
          paste("ID", x$cohort$cohort_definition_id)
        else "custom spec", "\n")
  } else {
    cat("Cohort: none (all persons)\n")
  }

  cat("Outputs (", length(x$outputs), "):\n")
  for (name in names(x$outputs)) {
    out <- x$outputs[[name]]
    otype <- out$type %||% "event_level"

    if (otype == "person_level") {
      cat("  [person_level] ", name, ": ",
          length(out$tables), " tables\n")
    } else if (otype == "baseline") {
      n_cols <- length(out$columns %||% character(0))
      n_derived <- length(out$derived %||% character(0))
      cat("  [baseline] ", name, ": ",
          n_cols, " columns, ", n_derived, " derived\n")
    } else if (otype == "survival") {
      n_concepts <- length(out$outcome$concept_set %||% integer(0))
      tar_end <- out$tar$end_offset %||% "cohort_end"
      cat("  [survival] ", name, ": ",
          out$outcome$table %||% "?", " (",
          n_concepts, " concepts), TAR 0-", tar_end, " days\n")
    } else if (otype == "concept_dictionary") {
      srcs <- out$source_outputs %||% "all"
      cat("  [dictionary] ", name, ": from ",
          paste(srcs, collapse = ", "), "\n")
    } else if (otype == "cohort_membership") {
      cat("  [cohort] ", name, ": standard OHDSI format\n")
    } else if (otype == "intervals_long") {
      n_tbls <- length(out$tables %||% character(0))
      cat("  [intervals] ", name, ": ",
          n_tbls, " tables\n")
    } else if (otype == "temporal_covariates") {
      bw <- out$bin_width %||% 30L
      ws <- out$window_start %||% -365L
      we <- out$window_end %||% 0L
      cat("  [temporal] ", name, ": ", out$table,
          " bins=", bw, "d [", ws, ",", we, "]\n")
    } else {
      repr <- out$representation$format %||% "long"
      n_concepts <- length(
        out$filters$concept_set$ids %||%
          out$concept_set)
      temporal_info <- ""
      if (!is.null(out$temporal)) {
        parts <- character(0)
        if (!is.null(out$temporal$index_window))
          parts <- c(parts, "index-window")
        if (!is.null(out$temporal$calendar))
          parts <- c(parts, "calendar")
        if (!is.null(out$temporal$event_select))
          parts <- c(parts, out$temporal$event_select$order)
        if (length(parts) > 0)
          temporal_info <- paste0(" [", paste(parts, collapse = "+"), "]")
      }
      dh_info <- ""
      if (!is.null(out$date_handling)) {
        dh_info <- paste0(" dates:", out$date_handling$mode)
      }
      cat("  [", repr, "] ", name, ": ",
          out$table,
          if (n_concepts > 0)
            paste0(" (", n_concepts, " concepts)")
          else "",
          temporal_info, dh_info, "\n")
    }
  }

  cat("Options: translate=",
      x$options$translate_concepts %||% FALSE,
      " block_sensitive=",
      x$options$block_sensitive %||% TRUE, "\n")
  invisible(x)
}
