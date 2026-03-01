# Module: Feature Specifications
# Factory functions for creating feature specification objects used in
# extraction plans and recipes.

#' Create a binary (presence/absence) feature specification
#'
#' Produces a feature spec that generates a boolean column indicating
#' whether the person has any records matching the concept set. When
#' used in a plan or recipe, the resulting column will contain
#' \code{TRUE} (at least one matching record) or \code{FALSE} (no
#' matching records).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with \code{type = "boolean"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.boolean(c(201826))
#' plan <- ds.omop.plan.features(plan, "has_diabetes",
#'   "condition_occurrence", specs = list(diabetes = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.count}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create an event count feature specification
#'
#' Produces a feature spec that generates an integer column containing
#' the number of records per person matching the concept set. Useful
#' for quantifying utilisation (e.g. number of visits, number of drug
#' dispensings).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to count.
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with \code{type = "count"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.count(c(9201, 9202, 9203))
#' plan <- ds.omop.plan.features(plan, "visit_counts",
#'   "visit_occurrence", specs = list(n_visits = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.boolean}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create a most-recent-value feature specification
#'
#' Produces a feature spec that extracts the value from the most recent
#' (latest) record matching the concept set. Ordered by the table's date
#' column, the last record's \code{value_column} is returned. Commonly
#' used for lab measurements where the latest reading is clinically
#' relevant (e.g. most recent HbA1c).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param value_column Character; name of the column from which to
#'   extract the value (default \code{"value_as_number"}).
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "latest_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.latest_value(c(3004410),
#'   value_column = "value_as_number")
#' plan <- ds.omop.plan.features(plan, "labs",
#'   "measurement", specs = list(latest_glucose = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.first_value}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create a first-recorded-value feature specification
#'
#' Produces a feature spec that extracts the value from the earliest
#' record matching the concept set. Ordered by the table's date column,
#' the first record's \code{value_column} is returned. Useful for
#' capturing baseline measurements at initial presentation.
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param value_column Character; name of the column from which to
#'   extract the value (default \code{"value_as_number"}).
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "first_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.first_value(c(3004410),
#'   value_column = "value_as_number")
#' plan <- ds.omop.plan.features(plan, "labs",
#'   "measurement", specs = list(initial_glucose = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.latest_value}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create a time-since-event feature specification
#'
#' Produces a feature spec that computes the elapsed time between the
#' most recent matching event and a reference date. The result is
#' expressed in the specified \code{unit} (days or months). Useful for
#' calculating recency of diagnoses, procedures, or measurements.
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param reference_date Character; ISO 8601 date string used as the
#'   reference point for the time calculation. If \code{NULL}, the
#'   cohort index date is used.
#' @param unit Character; time unit for the result. One of \code{"day"}
#'   or \code{"month"}.
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "time_since"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.time_since(c(201826), unit = "day")
#' plan <- ds.omop.plan.features(plan, "recency",
#'   "condition_occurrence",
#'   specs = list(days_since_diabetes = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.boolean}},
#'   \code{\link{omop.feature.count}}
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

#' Create a mean value feature specification
#'
#' Produces a feature spec that computes the arithmetic mean of a
#' numeric column across all records matching the concept set for each
#' person. Commonly used for averaging repeated lab measurements
#' (e.g. mean systolic blood pressure across visits).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param value_column Character; name of the numeric column to
#'   average (default \code{"value_as_number"}).
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "mean_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.mean_value(c(3004249),
#'   value_column = "value_as_number")
#' plan <- ds.omop.plan.features(plan, "vitals",
#'   "measurement", specs = list(mean_sbp = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.min_value}},
#'   \code{\link{omop.feature.max_value}}
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

#' Create a minimum value feature specification
#'
#' Produces a feature spec that computes the minimum of a numeric
#' column across all records matching the concept set for each person.
#' Useful for identifying the lowest recorded value (e.g. nadir
#' hemoglobin, minimum blood pressure).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param value_column Character; name of the numeric column from
#'   which to find the minimum (default \code{"value_as_number"}).
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "min_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.min_value(c(3000963),
#'   value_column = "value_as_number")
#' plan <- ds.omop.plan.features(plan, "labs",
#'   "measurement", specs = list(min_hemoglobin = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.max_value}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create a maximum value feature specification
#'
#' Produces a feature spec that computes the maximum of a numeric
#' column across all records matching the concept set for each person.
#' Useful for identifying peak values (e.g. maximum creatinine,
#' highest recorded temperature).
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object defining the concepts to match.
#' @param value_column Character; name of the numeric column from
#'   which to find the maximum (default \code{"value_as_number"}).
#' @param name Character; optional custom name for the feature column.
#'   If \code{NULL}, auto-generated from the concept and table context.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "max_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.max_value(c(3020891),
#'   value_column = "value_as_number")
#' plan <- ds.omop.plan.features(plan, "labs",
#'   "measurement", specs = list(peak_creatinine = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.min_value}},
#'   \code{\link{omop.feature.mean_value}}
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

#' Create a drug duration feature specification
#'
#' Produces a feature spec that computes the duration of drug exposure
#' records (\code{drug_exposure_end_date - drug_exposure_start_date})
#' and aggregates per person using the specified function.
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object.
#' @param agg Character; aggregation function — \code{"mean"},
#'   \code{"sum"}, or \code{"max"} (default \code{"mean"}).
#' @param name Character; optional custom name for the feature column.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "drug_duration"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.drug_duration(c(1124300), agg = "mean")
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.count}}
#' @export
omop.feature.drug_duration <- function(concept_set,
                                        agg = "mean",
                                        name = NULL) {
  spec <- list(
    type = "drug_duration",
    concept_set = concept_set,
    agg = agg,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Create a sum value feature specification
#'
#' Produces a feature spec that sums a numeric column across all records
#' matching the concept set for each person. Useful for computing total
#' days supply, total quantity, etc.
#'
#' @param concept_set Numeric vector of concept IDs, or an
#'   \code{omop_concept_set} object.
#' @param value_column Character; name of the numeric column to sum
#'   (default \code{"days_supply"}).
#' @param name Character; optional custom name for the feature column.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "sum_value"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.sum_value(c(1124300),
#'   value_column = "days_supply")
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.mean_value}}
#' @export
omop.feature.sum_value <- function(concept_set,
                                    value_column = "days_supply",
                                    name = NULL) {
  spec <- list(
    type = "sum_value",
    concept_set = concept_set,
    value_column = value_column,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}

#' Create a distinct concept count feature specification
#'
#' Produces a feature spec that counts the number of distinct concept IDs
#' per person across all records in the table. Unlike other features, this
#' operates across all concepts rather than per-concept.
#'
#' @param concept_set Numeric vector of concept IDs (default
#'   \code{integer(0)}, meaning all concepts in the table).
#' @param name Character; optional custom name for the feature column.
#' @return An \code{omop_feature_spec} object with
#'   \code{type = "n_distinct"}.
#' @examples
#' \dontrun{
#' spec <- omop.feature.n_distinct()
#' plan <- ds.omop.plan.features(plan, "diversity",
#'   "condition_occurrence", specs = list(n_conditions = spec))
#' }
#' @seealso \code{\link{ds.omop.plan.features}},
#'   \code{\link{omop.feature.count}}
#' @export
omop.feature.n_distinct <- function(concept_set = integer(0),
                                     name = NULL) {
  spec <- list(
    type = "n_distinct",
    concept_set = concept_set,
    name = name
  )
  class(spec) <- c("omop_feature_spec", "list")
  spec
}
