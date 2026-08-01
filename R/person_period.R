#' Add a regular episode-by-period panel to an extraction plan
#'
#' Declares a complete roster of cohort episodes crossed with regular relative
#' time bins. Event covariates are stored sparsely; a missing
#' \code{(rowId, timeId, covariateId)} row represents zero. The output contains
#' no absolute dates or source event identifiers.
#'
#' @param plan An \code{omop_plan} object.
#' @param table OMOP event table used for covariates.
#' @param concept_set Integer concept IDs.
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
                                       concept_set,
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
