#' Add a regular episode-by-period panel to an extraction plan
#'
#' Declares the regular relative-time bins that intersect the unique OMOP
#' observation period covering each cohort index date. Event covariates are
#' stored sparsely; a missing \code{(rowId, timeId, covariateId)} row represents
#' zero only when that \code{(rowId, timeId)} exists in \code{personPeriods}.
#' Each roster row keeps the requested \code{startDay}/\code{endDay} bin and
#' adds inclusive \code{observationStartDay}, \code{observationEndDay}, and
#' \code{daysObserved} for the observed part of the bin. The output contains no
#' absolute dates or source event identifiers. It is a descriptive panel, not
#' an inferred risk set: use survival/counting-process output when cohort end,
#' death, or another censoring rule must define time at risk.
#'
#' @param plan An \code{omop_plan} object.
#' @param table OMOP event table used for covariates.
#' @param concept_set Optional concept IDs or an OHDSI-style concept-set spec.
#'   \code{NULL} requests all observed concepts subject to the server cap.
#' @param bin_width Positive integer bin width in days.
#' @param window_start,window_end Inclusive integer days from index.
#' @param analyses Unique subset of \code{"binary"} and \code{"count"}.
#' @param grain Must be \code{"episode"}.
#' @param time_origin Must be \code{"index"}.
#' @param name Output name.
#' @return The modified \code{omop_plan}.
#' @seealso \code{\link{ds.omop.plan.temporal_covariates}}
#' @export
ds.omop.plan.person_period <- function(plan,
                                       table,
                                       concept_set = NULL,
                                       bin_width = 30L,
                                       window_start = -365L,
                                       window_end = 0L,
                                       analyses = c("binary"),
                                       grain = "episode",
                                       time_origin = "index",
                                       name = "person_period") {
  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !identical(tolower(grain), "episode")) {
    stop("grain must be explicitly 'episode' for person_period.",
         call. = FALSE)
  }
  if (!is.character(time_origin) || length(time_origin) != 1L ||
      is.na(time_origin) || !identical(tolower(time_origin), "index")) {
    stop("time_origin must be explicitly 'index' for person_period.",
         call. = FALSE)
  }

  plan <- ds.omop.plan.temporal_covariates(
    plan = plan,
    table = table,
    concept_set = concept_set,
    bin_width = bin_width,
    window_start = window_start,
    window_end = window_end,
    analyses = analyses,
    name = name
  )
  plan$outputs[[name]]$type <- "person_period"
  plan$outputs[[name]]$grain <- "episode"
  plan$outputs[[name]]$time_origin <- "index"
  plan
}
