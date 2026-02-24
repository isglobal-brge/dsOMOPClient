# ==============================================================================
# dsOMOPClient v2 - Feature Recipe Helpers
# ==============================================================================
# Each function returns an omop_feature_spec list for use in
# ds.omop.plan.features().
# ==============================================================================

#' Binary presence/absence feature
#'
#' @param concept_set Numeric vector of concept IDs or omop_concept_set
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.boolean <- function(concept_set, name = NULL) {
  spec <- list(
    type = "boolean",
    concept_set = concept_set,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Count of events per person
#'
#' @param concept_set Numeric vector of concept IDs or omop_concept_set
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.count <- function(concept_set, name = NULL) {
  spec <- list(
    type = "count",
    concept_set = concept_set,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Most recent value feature
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param value_column Character; column to extract value from
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.latest_value <- function(concept_set,
                                      value_column = "value_as_number",
                                      name = NULL) {
  spec <- list(
    type = "latest_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' First recorded value feature
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param value_column Character; column to extract value from
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.first_value <- function(concept_set,
                                     value_column = "value_as_number",
                                     name = NULL) {
  spec <- list(
    type = "first_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Time since event feature
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param reference_date Character; reference date (ISO format)
#' @param unit Character; "day" or "month"
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.time_since <- function(concept_set,
                                    reference_date = NULL,
                                    unit = "day",
                                    name = NULL) {
  spec <- list(
    type = "time_since",
    concept_set = concept_set,
    reference_date = reference_date,
    unit = unit,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Mean value across events
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param value_column Character; column to extract value from
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.mean_value <- function(concept_set,
                                    value_column = "value_as_number",
                                    name = NULL) {
  spec <- list(
    type = "mean_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Minimum value across events
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param value_column Character; column to extract value from
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.min_value <- function(concept_set,
                                   value_column = "value_as_number",
                                   name = NULL) {
  spec <- list(
    type = "min_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Maximum value across events
#'
#' @param concept_set Numeric vector or omop_concept_set
#' @param value_column Character; column to extract value from
#' @param name Character; feature name
#' @return An omop_feature_spec list
#' @export
omop.feature.max_value <- function(concept_set,
                                   value_column = "value_as_number",
                                   name = NULL) {
  spec <- list(
    type = "max_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}
