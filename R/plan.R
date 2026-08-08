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
      translate_concepts = TRUE,
      block_sensitive = TRUE,
      factor_concepts = TRUE
    )
  )
  class(plan) <- c("omop_plan", "list")
  plan
}

# Normalize a public integer scalar without silently truncating fractional
# values before the server gets a chance to validate the plan.
.plan_integer_scalar <- function(value, field, min_value = NULL) {
  if (length(value) != 1L || !is.numeric(value) || is.na(value) ||
      !is.finite(value) || value != trunc(value) ||
      value < -.Machine$integer.max || value > .Machine$integer.max) {
    stop(field, " must be one exact integer.", call. = FALSE)
  }
  value <- as.integer(value)
  if (!is.null(min_value) && value < min_value) {
    stop(field, " must be >= ", min_value, ".", call. = FALSE)
  }
  value
}

.plan_integer_vector <- function(value, field, allow_empty = FALSE) {
  if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
      any(value != trunc(value)) ||
      any(value < -.Machine$integer.max | value > .Machine$integer.max) ||
      (!allow_empty && length(value) == 0L)) {
    stop(field, " must contain exact integers",
         if (!allow_empty) " and must not be empty" else "", ".",
         call. = FALSE)
  }
  as.integer(value)
}

.plan_concept_set <- function(value, field, allow_null = FALSE) {
  if (is.null(value)) {
    if (allow_null) return(NULL)
    stop(field, " must not be NULL.", call. = FALSE)
  }
  if (!is.list(value) || is.null(value$concepts)) {
    return(.plan_integer_vector(value, field))
  }
  allowed <- c(
    "concepts", "include_descendants", "include_mapped", "exclude"
  )
  if (is.null(names(value)) || any(!nzchar(names(value))) ||
      anyDuplicated(names(value)) || length(setdiff(names(value), allowed))) {
    stop(field, " may contain only concepts, include_descendants, ",
         "include_mapped, and exclude.", call. = FALSE)
  }
  normalized <- list(
    concepts = .plan_integer_vector(value$concepts, paste0(field, "$concepts"))
  )
  for (flag in c("include_descendants", "include_mapped")) {
    if (!is.null(value[[flag]])) {
      if (!is.logical(value[[flag]]) || length(value[[flag]]) != 1L ||
          is.na(value[[flag]])) {
        stop(field, "$", flag, " must be TRUE or FALSE.", call. = FALSE)
      }
      normalized[[flag]] <- value[[flag]]
    }
  }
  if (!is.null(value$exclude)) {
    normalized$exclude <- .plan_integer_vector(
      value$exclude, paste0(field, "$exclude"), allow_empty = TRUE
    )
  }
  normalized
}

.plan_iso_date <- function(value, field) {
  if (inherits(value, "Date")) value <- format(value, "%Y-%m-%d")
  if (length(value) != 1L || !is.character(value) || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    stop(field, " must be one ISO date in YYYY-MM-DD form.", call. = FALSE)
  }
  parsed <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
  if (is.na(parsed) || !identical(format(parsed, "%Y-%m-%d"), value)) {
    stop(field, " is not a valid calendar date.", call. = FALSE)
  }
  value
}

.plan_day_window <- function(window, field, strict = FALSE) {
  if (!is.list(window) || is.null(names(window)) ||
      any(!nzchar(names(window))) || anyDuplicated(names(window)) ||
      length(setdiff(names(window), c("start", "end"))) > 0L ||
      (is.null(window$start) && is.null(window$end))) {
    stop(field, " must be a named list with start and/or end.",
         call. = FALSE)
  }
  result <- list()
  if (!is.null(window$start)) {
    result$start <- .plan_integer_scalar(window$start,
                                         paste0(field, "$start"))
  }
  if (!is.null(window$end)) {
    result$end <- .plan_integer_scalar(window$end,
                                       paste0(field, "$end"))
  }
  invalid_order <- !is.null(result$start) && !is.null(result$end) &&
    (if (strict) result$start >= result$end else result$start > result$end)
  if (invalid_order) {
    stop(field, if (strict) " start must be before end."
         else " start must not be after end.", call. = FALSE)
  }
  result
}

.plan_calendar_window <- function(window, field = "calendar") {
  if (!is.list(window) || is.null(names(window)) ||
      any(!nzchar(names(window))) || anyDuplicated(names(window)) ||
      length(setdiff(names(window), c("start", "end"))) > 0L ||
      (is.null(window$start) && is.null(window$end))) {
    stop(field, " must be a named list with start and/or end.",
         call. = FALSE)
  }
  result <- list()
  if (!is.null(window$start)) {
    result$start <- .plan_iso_date(window$start, paste0(field, "$start"))
  }
  if (!is.null(window$end)) {
    result$end <- .plan_iso_date(window$end, paste0(field, "$end"))
  }
  if (!is.null(result$start) && !is.null(result$end) &&
      as.Date(result$start) > as.Date(result$end)) {
    stop(field, " start must not be after end.", call. = FALSE)
  }
  result
}

#' Set a cohort filter on the plan
#'
#' Attaches a cohort definition to the plan, restricting all downstream
#' outputs to the selected cohort episodes. A person may therefore contribute
#' multiple episodes under the same definition, including overlapping episodes.
#' Exactly one of
#' \code{cohort_definition_id} or \code{spec} must be provided. Use
#' \code{cohort_definition_id} to reference an existing cohort definition,
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
  has_id <- !is.null(cohort_definition_id)
  has_spec <- !is.null(spec)
  if (identical(has_id, has_spec)) {
    stop("Exactly one of cohort_definition_id or spec must be provided.",
         call. = FALSE)
  }
  if (has_id) {
    plan$cohort <- list(
      type = "cohort_table",
      cohort_definition_id = .plan_integer_scalar(
        cohort_definition_id, "cohort_definition_id", min_value = 0L
      )
    )
  } else {
    if (!is.list(spec) || length(spec) == 0L || is.null(names(spec)) ||
        anyNA(names(spec)) || any(!nzchar(names(spec))) ||
        anyDuplicated(names(spec))) {
      stop("spec must be a non-empty named cohort specification.",
           call. = FALSE)
    }
    plan$cohort <- list(
      type = "spec",
      spec = spec
    )
  }
  plan
}

#' Normalise an aliasing spec so it survives the DataSHIELD JSON transport
#'
#' Raw-column specs may be passed as named vectors to alias the output
#' columns, e.g. \code{c(sex = "gender_concept_id", "race_concept_id")}. Two
#' things must hold for the aliases to reach the server intact:
#' \enumerate{
#'   \item No blank object keys: any element left unnamed is given a name
#'     equal to its value, so a partially named vector becomes fully named.
#'   \item A \emph{named} spec must serialise as a JSON object, not an array.
#'     \code{jsonlite::toJSON(auto_unbox = TRUE)} drops the names of a named
#'     \emph{atomic} vector (it emits a bare \code{[...]} array), but keeps
#'     the names of a \emph{list}. So a named atomic vector is converted to a
#'     named list here.
#' }
#' A fully unnamed vector is returned unchanged (stays a plain array = no
#' aliasing). A spec that is already a list (e.g. a features spec) is left
#' alone.
#'
#' @param x A character vector (possibly partially named) or a list.
#' @return The normalised spec: unnamed vectors unchanged; named vectors
#'   filled and returned as a named list.
#' @keywords internal
.fill_alias_names <- function(x) {
  if (is.null(x)) return(x)
  nm <- names(x)
  if (is.null(nm)) return(x)
  blank <- !nzchar(nm)
  if (any(blank)) names(x)[blank] <- as.character(x)[blank]
  if (is.atomic(x)) x <- as.list(x)
  x
}

#' Add a baseline demographics output to the plan
#'
#' Produces one row per cohort episode, preserving \code{cohort_row_id} when a
#' person has recurrent membership, with demographics from the person table and
#' optional derived fields. Requires a cohort to be set. This is the recommended
#' way to retrieve cohort-indexed demographic variables because it can compute
#' episode-relative fields such as age at index.
#'
#' @param plan An \code{omop_plan} object.
#' @param columns Character vector; person-table columns to include
#'   (e.g. \code{"gender_concept_id"}, \code{"race_concept_id"}). Exact birth
#'   components are not releasable; request \code{"age_at_index"} through
#'   \code{derived} instead. Pass a \emph{named} vector to rename columns.
#'   Unnamed entries keep their source name. Identifier columns cannot be
#'   renamed (they are stripped server-side regardless).
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
#'   columns = c("gender_concept_id", "race_concept_id"),
#'   derived = c("age_at_index", "prior_observation")
#' )
#' }
#' @seealso \code{\link{ds.omop.plan.person_level}},
#'   \code{\link{ds.omop.plan.cohort}}
#' @export
ds.omop.plan.baseline <- function(plan,
                                  columns = c("gender_concept_id",
                                              "race_concept_id"),
                                  derived = c("age_at_index"),
                                  name = "baseline") {
  plan$outputs[[name]] <- list(
    type = "baseline",
    columns = .fill_alias_names(columns),
    derived = derived
  )
  plan
}

#' Add cardinality-safe person-level tables to the plan
#'
#' Merges one-row-per-person sources by \code{person_id}. Raw columns are
#' accepted only from tables whose cardinality is guaranteed to be at most one
#' row per person; repeatable clinical tables must be reduced through explicit
#' feature specifications. For cohort-aware demographics and age at index, use
#' \code{\link{ds.omop.plan.baseline}} instead.
#'
#' @param plan An \code{omop_plan} object.
#' @param tables Named list; each element maps a table name to a
#'   character vector of column names to include, e.g.
#'   \code{list(person = c("gender_concept_id", "race_concept_id"))}.
#'   Each column vector may be \emph{named} to rename columns in the output:
#'   \code{c(sex = "gender_concept_id", race = "race_concept_id")} yields
#'   output columns \code{sex} and \code{race}. Unnamed entries keep
#'   their source name. Identifier columns cannot be renamed (they are
#'   stripped server-side regardless).
#' @param name Character; output name used as a key in the plan's
#'   outputs list and as the default symbol name on the server.
#' @return The modified \code{omop_plan} with the person-level output
#'   appended.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.person_level(plan,
#'   tables = list(
#'     person = c("gender_concept_id", "race_concept_id")
#'   ),
#'   name = "demographics"
#' )
#'
#' # Rename columns at request time with a named vector
#' plan <- ds.omop.plan.person_level(plan,
#'   tables = list(person = c(sex = "gender_concept_id",
#'                            race = "race_concept_id")),
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
    tables = lapply(tables, .fill_alias_names)
  )
  plan
}

.plan_multistate_names <- function(value, field, allow_empty = FALSE) {
  if (is.list(value) && !is.data.frame(value)) {
    if (length(value) == 0L && allow_empty) return(character(0))
    valid <- vapply(value, function(item) {
      is.character(item) && length(item) == 1L && !is.na(item)
    }, logical(1L))
    if (!all(valid)) {
      stop(field, " must contain only state names.", call. = FALSE)
    }
    value <- unlist(value, use.names = FALSE)
  }
  if (!is.character(value) || (!allow_empty && length(value) == 0L) ||
      anyNA(value) || any(!nzchar(value))) {
    stop(field, " must contain only state names.", call. = FALSE)
  }
  unname(value)
}

.plan_multistate_transition_id <- function(value, field) {
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      length(integer) != 1L || is.na(integer) || number != integer ||
      integer < 1L) {
    stop(field, " must be one positive exact integer.", call. = FALSE)
  }
  integer
}

.plan_multistate_step <- function(value, n_states) {
  number <- suppressWarnings(as.numeric(value %||% 0.01))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      number <= 0 || number != round(number, 9L) ||
      number * max(0L, n_states - 1L) >= 1) {
    stop("state_step must be a positive decimal with at most nine places, and ",
         "state_step * (number of states - 1) must be below one day.",
         call. = FALSE)
  }
  number
}

# Convert mstate transition matrices and compact adjacency lists into the same
# plain graph that is transported in plan/recipe JSON. Transition numbering is
# public and deterministic; it never depends on which events occur at a site.
.plan_normalize_multistate <- function(outcome_names, transitions,
                                       initial_state,
                                       state_hierarchy = NULL,
                                       state_step = NULL) {
  initial_state <- .plan_multistate_names(
    initial_state %||% "index", "initial_state"
  )
  if (length(initial_state) != 1L) {
    stop("initial_state must be one state name.", call. = FALSE)
  }
  expected_states <- unique(c(initial_state, outcome_names))
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", expected_states)) ||
      anyDuplicated(tolower(expected_states))) {
    stop("Multi-state names must be portable and case-insensitively unique.",
         call. = FALSE)
  }

  edges <- NULL
  states <- NULL
  if (is.matrix(transitions)) {
    if (nrow(transitions) < 2L || nrow(transitions) != ncol(transitions) ||
        is.null(rownames(transitions)) || is.null(colnames(transitions)) ||
        !identical(rownames(transitions), colnames(transitions))) {
      stop("transitions matrix must be square with identical state dimnames.",
           call. = FALSE)
    }
    states <- rownames(transitions)
    if (any(!is.na(diag(transitions)))) {
      stop("Self transitions are not supported; the matrix diagonal must be NA.",
           call. = FALSE)
    }
    positions <- which(!is.na(transitions), arr.ind = TRUE)
    ids <- suppressWarnings(as.numeric(transitions[positions]))
    integer_ids <- suppressWarnings(as.integer(ids))
    if (length(ids) == 0L || anyNA(ids) || any(!is.finite(ids)) ||
        anyNA(integer_ids) || any(ids != integer_ids) ||
        !setequal(integer_ids, seq_along(integer_ids))) {
      stop("Non-NA transition matrix entries must be unique integers 1..K.",
           call. = FALSE)
    }
    positions <- positions[order(integer_ids), , drop = FALSE]
    edges <- lapply(seq_len(nrow(positions)), function(index) {
      list(
        from = states[positions[index, 1L]],
        to = states[positions[index, 2L]],
        trans = as.integer(index)
      )
    })
  } else if (is.list(transitions) && length(transitions) == 2L &&
             !is.null(names(transitions)) &&
             !anyDuplicated(names(transitions)) &&
             setequal(names(transitions), c("states", "edges"))) {
    states <- .plan_multistate_names(
      transitions$states, "transitions$states"
    )
    if (!is.list(transitions$edges) || length(transitions$edges) == 0L) {
      stop("transitions$edges must be a non-empty list.", call. = FALSE)
    }
    edges <- lapply(seq_along(transitions$edges), function(index) {
      edge <- transitions$edges[[index]]
      if (!is.list(edge) || length(edge) != 3L || is.null(names(edge)) ||
          anyDuplicated(names(edge)) ||
          !setequal(names(edge), c("from", "to", "trans"))) {
        stop("Each transitions$edges entry must contain from, to and trans.",
             call. = FALSE)
      }
      from <- .plan_multistate_names(
        edge$from, paste0("transitions$edges[[", index, "]]$from")
      )
      to <- .plan_multistate_names(
        edge$to, paste0("transitions$edges[[", index, "]]$to")
      )
      if (length(from) != 1L || length(to) != 1L) {
        stop("Every transition edge must have one from and one to state.",
             call. = FALSE)
      }
      list(
        from = from,
        to = to,
        trans = .plan_multistate_transition_id(
          edge$trans, paste0("transitions$edges[[", index, "]]$trans")
        )
      )
    })
    edge_ids <- vapply(edges, `[[`, integer(1L), "trans")
    edges <- edges[order(edge_ids)]
  } else if (is.list(transitions) && length(transitions) > 0L) {
    states <- names(transitions)
    if (is.null(states) || any(!nzchar(states)) || anyDuplicated(states)) {
      stop("Adjacency transitions must be a uniquely named state list.",
           call. = FALSE)
    }
    edges <- list()
    transition_id <- 0L
    for (from_index in seq_along(transitions)) {
      targets <- transitions[[from_index]]
      if (is.null(targets) || length(targets) == 0L) next
      if (is.list(targets)) targets <- unlist(targets, use.names = FALSE)
      numeric_targets <- is.numeric(targets) ||
        (is.character(targets) && all(grepl("^[0-9]+$", targets)))
      if (numeric_targets) {
        positions <- suppressWarnings(as.numeric(targets))
        integer_positions <- suppressWarnings(as.integer(targets))
        if (anyNA(positions) || anyNA(integer_positions) ||
            any(positions != integer_positions) ||
            any(integer_positions < 1L | integer_positions > length(states))) {
          stop("Numeric transition targets must be valid state positions.",
               call. = FALSE)
        }
        targets <- states[integer_positions]
      } else {
        targets <- .plan_multistate_names(
          targets, paste0("transitions$", states[from_index]),
          allow_empty = TRUE
        )
      }
      if (anyDuplicated(targets)) {
        stop("A state cannot declare the same destination twice.",
             call. = FALSE)
      }
      for (target in targets) {
        transition_id <- transition_id + 1L
        edges[[transition_id]] <- list(
          from = states[from_index], to = target, trans = transition_id
        )
      }
    }
  } else {
    stop("multi_state requires a transition matrix or adjacency list.",
         call. = FALSE)
  }

  states <- .plan_multistate_names(states, "transition state names")
  if (anyDuplicated(states) || anyDuplicated(tolower(states)) ||
      !setequal(states, expected_states)) {
    stop("Transition states must match initial_state plus named outcomes.",
         call. = FALSE)
  }
  if (length(edges) == 0L) {
    stop("The multi-state graph must declare at least one transition.",
         call. = FALSE)
  }
  ids <- vapply(edges, `[[`, integer(1L), "trans")
  if (!identical(ids, seq_along(edges))) {
    stop("Transition identifiers must be unique contiguous integers 1..K.",
         call. = FALSE)
  }
  from <- vapply(edges, `[[`, character(1L), "from")
  to <- vapply(edges, `[[`, character(1L), "to")
  if (any(!from %in% states) || any(!to %in% states) || any(from == to) ||
      anyDuplicated(paste(from, to, sep = "\r"))) {
    stop("Transition edges must uniquely connect declared distinct states.",
         call. = FALSE)
  }
  reachable <- initial_state
  repeat {
    next_states <- unique(c(reachable, to[from %in% reachable]))
    if (setequal(next_states, reachable)) break
    reachable <- next_states
  }
  if (length(setdiff(states, reachable)) > 0L) {
    stop("Every state must be graph-reachable from initial_state.",
         call. = FALSE)
  }
  hierarchy <- if (is.null(state_hierarchy) || length(state_hierarchy) == 0L) {
    states
  } else {
    .plan_multistate_names(state_hierarchy, "state_hierarchy")
  }
  if (anyDuplicated(hierarchy) || any(!hierarchy %in% states)) {
    stop("state_hierarchy must contain unique declared state names.",
         call. = FALSE)
  }
  hierarchy <- c(hierarchy, setdiff(states, hierarchy))
  list(
    initial_state = initial_state,
    transitions = list(states = unname(states), edges = edges),
    state_hierarchy = unname(hierarchy),
    state_step = .plan_multistate_step(state_step, length(states))
  )
}

#' Add a survival (time-to-event) output to the plan
#'
#' The historical single-outcome call produces one row per cohort episode with
#' an event indicator and time-to-event in days. Advanced calls can retain named
#' endpoints as survival, competing-risk, recurrent-event, counting-process,
#' or OHDSI-style expanded multi-state data. Calendar dates and source event
#' identifiers are never returned.
#' Requires a cohort to be set. Historical plans without an explicit censoring
#' field are censored at the end of the observation period containing the index
#' episode; they never bridge an unobserved gap to a later period.
#'
#' @param plan An \code{omop_plan} object.
#' @param outcome_table Character; OMOP table containing outcome events
#'   (e.g. \code{"condition_occurrence"}, \code{"procedure_occurrence"}).
#' @param outcome_concepts Numeric vector; concept IDs defining the historical
#'   composite outcome. Omit when using `outcomes`.
#' @param tar Named list; time-at-risk window with \code{start_offset}
#'   and \code{end_offset} (integer days relative to cohort_start_date).
#' @param event_order Character; \code{"first"} or \code{"last"} to
#'   select which event occurrence determines the time-to-event value;
#'   advanced recurrent/counting/multi-state formats also accept `all`.
#' @param name Character; output name used as a key in the plan's outputs list.
#' @param outcomes Named list of endpoint specifications. Each endpoint contains
#'   `table`, `concept_set`, and optional safe row `filters`.
#' @param censoring Named list controlling observation-period, death, cohort-end,
#'   and optional administrative-date censoring.
#' @param format Character; `survival`, `competing_risk`, `recurrent_events`,
#'   `counting_process`, or `multi_state`.
#' @param washout_days Non-negative integer washout between events of the same
#'   named endpoint.
#' @param tie_policy Character; `priority`, `all`, or `sequential`.
#'   `all` is restricted to recurrent events. `sequential` orders simultaneous
#'   reachable state transitions within their observed day and is restricted
#'   to multi-state output. The historical `error` policy is rejected before
#'   querying because data-dependent failures create a disclosure oracle.
#' @param transitions For `multi_state`, either an `mstate` transition matrix,
#'   a named adjacency list, or the canonical `list(states, edges)` form.
#' @param initial_state Initial state name for `multi_state` (default `index`).
#' @param state_hierarchy Optional public state priority used to resolve tied
#'   dates. Omitted states are appended in transition-state order.
#' @param state_step Positive within-day analytic offset used only by
#'   `tie_policy = "sequential"`; defaults to 0.01 days.
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
                                  outcome_concepts = NULL,
                                  tar = list(start_offset = 0,
                                             end_offset = 730),
                                  event_order = "first",
                                  name = "survival",
                                  outcomes = NULL,
                                  censoring = NULL,
                                  format = NULL,
                                  washout_days = 0L,
                                  tie_policy = "priority",
                                  transitions = NULL,
                                  initial_state = NULL,
                                  state_hierarchy = NULL,
                                  state_step = NULL) {
  event_order_missing <- missing(event_order)
  if (!is.character(event_order) || length(event_order) != 1L ||
      is.na(event_order)) {
    stop("event_order must be first, last, or all.", call. = FALSE)
  }
  if (!is.list(tar) || is.null(names(tar)) || any(!nzchar(names(tar))) ||
      anyDuplicated(names(tar)) ||
      length(setdiff(names(tar), c("start_offset", "end_offset"))) > 0L) {
    stop("tar must be a named list containing start_offset and/or end_offset.",
         call. = FALSE)
  }
  start_offset <- if (is.null(tar$start_offset)) 0L else
    .plan_integer_scalar(tar$start_offset, "tar$start_offset")
  end_offset <- if (is.null(tar$end_offset)) NULL else
    .plan_integer_scalar(tar$end_offset, "tar$end_offset")
  if (!is.null(end_offset) && start_offset > end_offset) {
    stop("tar$start_offset must not be after tar$end_offset.", call. = FALSE)
  }
  tar <- list(start_offset = start_offset)
  if (!is.null(end_offset)) tar$end_offset <- end_offset
  washout_days <- .plan_integer_scalar(washout_days, "washout_days",
                                       min_value = 0L)
  tie_policy <- match.arg(
    tie_policy, c("priority", "error", "all", "sequential")
  )
  if (identical(tie_policy, "error")) {
    stop("tie_policy='error' is unavailable because a data-dependent query ",
         "failure creates a disclosure oracle; use deterministic priority or ",
         "the format-specific all/sequential policy.", call. = FALSE)
  }
  advanced <- !is.null(outcomes) || !is.null(censoring) || !is.null(format) ||
    washout_days != 0L || tie_policy != "priority" ||
    identical(tolower(event_order), "all") || !is.null(transitions) ||
    !is.null(initial_state) || !is.null(state_hierarchy) ||
    !is.null(state_step)

  normalize_outcome <- function(outcome, label) {
    if (!is.list(outcome) || is.null(names(outcome)) ||
        any(!nzchar(names(outcome))) || anyDuplicated(names(outcome)) ||
        !all(c("table", "concept_set") %in% names(outcome)) ||
        length(setdiff(names(outcome),
                       c("table", "concept_set", "filters"))) > 0L) {
      stop("Outcome '", label,
           "' must contain table and concept_set, with optional filters only.",
           call. = FALSE)
    }
    if (!is.character(outcome$table) || length(outcome$table) != 1L ||
        is.na(outcome$table) ||
        !grepl("^[A-Za-z_][A-Za-z0-9_.]*$", outcome$table)) {
      stop("Outcome '", label, "' has an invalid table name.", call. = FALSE)
    }
    outcome$concept_set <- .plan_concept_set(
      outcome$concept_set, paste0("outcomes$", label, "$concept_set")
    )
    if (!is.null(outcome$filters) && !is.list(outcome$filters)) {
      stop("Outcome '", label, "' filters must be a filter DSL list.",
           call. = FALSE)
    }
    outcome
  }

  if (is.null(outcomes)) {
    outcome_concepts <- .plan_concept_set(
      outcome_concepts, "outcome_concepts"
    )
    legacy_outcome <- list(
      table = outcome_table,
      concept_set = outcome_concepts
    )
    if (!advanced) {
      event_order <- match.arg(event_order, c("first", "last"))
      plan$outputs[[name]] <- list(
        type = "survival",
        outcome = legacy_outcome,
        tar = tar,
        event_order = event_order
      )
      return(plan)
    }
    outcomes <- list(outcome = legacy_outcome)
  } else {
    if (!is.null(outcome_concepts)) {
      stop("Use either outcome_concepts or outcomes, not both.",
           call. = FALSE)
    }
    if (!is.list(outcomes) || length(outcomes) == 0L ||
        is.null(names(outcomes)) || any(!nzchar(names(outcomes))) ||
        anyDuplicated(names(outcomes)) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", names(outcomes)))) {
      stop("outcomes must be a non-empty uniquely named list with safe names.",
           call. = FALSE)
    }
  }
  outcome_names <- names(outcomes)
  outcomes <- lapply(seq_along(outcomes), function(i) {
    normalize_outcome(outcomes[[i]], outcome_names[i])
  })
  names(outcomes) <- outcome_names

  format <- match.arg(
    format %||% "survival",
    c("survival", "competing_risk", "recurrent_events", "counting_process",
      "multi_state")
  )
  if (identical(format, "multi_state") && event_order_missing) {
    event_order <- "all"
  }
  event_order <- match.arg(event_order, c("first", "last", "all"))
  if (identical(format, "survival") && event_order == "all") {
    stop("survival requires event_order first or last.", call. = FALSE)
  }
  if (identical(format, "competing_risk") && event_order != "first") {
    stop("competing_risk requires event_order='first'.", call. = FALSE)
  }
  if (format %in% c("recurrent_events", "counting_process") &&
      event_order == "last") {
    stop("Recurrent/counting formats require event_order first or all.",
         call. = FALSE)
  }
  if (tie_policy == "all" && format != "recurrent_events") {
    stop("tie_policy='all' is supported only for recurrent_events.",
         call. = FALSE)
  }
  if (identical(format, "multi_state") && event_order != "all") {
    stop("multi_state requires event_order='all'.", call. = FALSE)
  }
  if (tie_policy == "sequential" && format != "multi_state") {
    stop("tie_policy='sequential' is supported only for multi_state.",
         call. = FALSE)
  }
  graph_fields <- list(
    transitions = transitions,
    initial_state = initial_state,
    state_hierarchy = state_hierarchy,
    state_step = state_step
  )
  if (!identical(format, "multi_state") &&
      any(!vapply(graph_fields, is.null, logical(1L)))) {
    stop("Multi-state graph fields require format='multi_state'.",
         call. = FALSE)
  }
  multistate <- if (identical(format, "multi_state")) {
    .plan_normalize_multistate(
      names(outcomes), transitions, initial_state,
      state_hierarchy = state_hierarchy, state_step = state_step
    )
  } else {
    NULL
  }
  if (!is.null(censoring)) {
    if (!is.list(censoring) || is.null(names(censoring)) ||
        any(!nzchar(names(censoring))) || anyDuplicated(names(censoring)) ||
        length(setdiff(names(censoring), c(
          "cohort_end", "observation_period_end", "death", "admin_date"
        ))) > 0L) {
      stop("censoring contains unsupported fields.", call. = FALSE)
    }
    for (field in intersect(names(censoring),
                            c("cohort_end", "observation_period_end", "death"))) {
      if (!is.logical(censoring[[field]]) ||
          length(censoring[[field]]) != 1L || is.na(censoring[[field]])) {
        stop("censoring$", field, " must be TRUE or FALSE.", call. = FALSE)
      }
    }
    if (identical(censoring$cohort_end, FALSE)) {
      stop("censoring$cohort_end must remain TRUE.", call. = FALSE)
    }
    if (!is.null(censoring$admin_date)) {
      censoring$admin_date <- .plan_iso_date(
        censoring$admin_date, "censoring$admin_date"
      )
    }
  }
  output <- list(
    type = "survival",
    outcomes = outcomes,
    tar = tar,
    format = format,
    event_order = event_order,
    washout_days = washout_days,
    tie_policy = tie_policy
  )
  if (!is.null(multistate)) output <- c(output, multistate)
  if (!is.null(censoring)) output$censoring <- censoring
  plan$outputs[[name]] <- output
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
#' @param filters Named list; additional custom filter DSL expressions
#'   (nested \code{and}/\code{or} of leaves, each \code{list(var=, op=, value=)}).
#'   Validated fail-closed server-side: leaves on identifier or blocked columns,
#'   and narrow fingerprinting operators, are rejected. Use this to filter by
#'   \code{unit_concept_id} or a \code{*_type_concept_id} for unit/type scoping.
#' @param visit_filter Named list \code{list(concept_ids = ...)}; restrict events
#'   to visits of those \code{visit_concept_id} values via the
#'   \code{visit_occurrence_id} link.
#' @param concept_col Character; override the concept column the
#'   \code{concept_set} scopes (default: the table's domain concept), e.g.
#'   \code{"unit_concept_id"} to extract one unit for harmonization.
#' @param representation Named list with \code{format} (one of
#'   \code{"long"}, \code{"wide"}, \code{"features"}, or \code{"sparse"}) and
#'   optional format-specific settings. Wide/features/sparse may set \code{grain} to
#'   \code{"person"} (default) or \code{"episode"}; an index-relative window
#'   requires episode grain.
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
                                visit_filter = NULL,
                                concept_col = NULL,
                                representation = list(
                                  format = "long")) {
  if (!is.list(representation) || is.null(names(representation)) ||
      any(!nzchar(names(representation))) || anyDuplicated(names(representation)) ||
      length(setdiff(names(representation), c("format", "features", "grain"))) > 0L) {
    stop("representation must be a uniquely named format/features/grain list.",
         call. = FALSE)
  }
  format <- representation$format %||% "long"
  if (!is.character(format) || length(format) != 1L || is.na(format) ||
      !format %in% c("long", "wide", "features", "sparse")) {
    stop("representation$format must be long, wide, features, or sparse.",
         call. = FALSE)
  }
  grain <- representation$grain %||% "person"
  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !grain %in% c("person", "episode")) {
    stop("representation$grain must be 'person' or 'episode'.",
         call. = FALSE)
  }
  if (!format %in% c("wide", "features", "sparse") &&
      "grain" %in% names(representation)) {
    stop("representation$grain applies only to wide, features, or sparse.",
         call. = FALSE)
  }
  has_index_window <- !is.null(temporal$index_window)
  if (format %in% c("wide", "features", "sparse") && has_index_window &&
      !identical(grain, "episode")) {
    stop("Index-relative wide/features/sparse require ",
         "representation$grain='episode'.",
         call. = FALSE)
  }
  if (format %in% c("wide", "features", "sparse") && !has_index_window &&
      identical(grain, "episode")) {
    stop("Episode-grain wide/features/sparse require temporal$index_window.",
         call. = FALSE)
  }
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
  if (!is.null(visit_filter)) {
    output$filters$visit <- visit_filter
  }
  if (!is.null(concept_col)) {
    output$filters$concept_col <- concept_col
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
#' produces one column in the resulting data frame. Each spec is evaluated
#' against its own concept scope. No output-wide concept prefilter is
#' added: such a prefilter would change the meaning of an unscoped spec or of
#' specs that use different \code{concept_col} values.
#'
#' @param plan An \code{omop_plan} object.
#' @param name Character; output name used as a key in the plan's
#'   outputs list.
#' @param table Character; source OMOP table name
#'   (e.g. \code{"condition_occurrence"}, \code{"measurement"}).
#' @param specs Named list of \code{omop_feature_spec} objects created
#'   by the \code{omop.feature.*} family of functions (e.g.
#'   \code{\link{omop.feature.boolean}}, \code{\link{omop.feature.count}}).
#' @param grain Character; \code{"person"} (default) or \code{"episode"}.
#'   Episode grain preserves one row per cohort episode and requires
#'   \code{temporal$index_window}.
#' @param temporal Optional \code{omop.temporal()} specification. Supply an
#'   \code{index_window} when \code{grain = "episode"} or when feature specs
#'   contain episode-relative \code{time_window} values.
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
                                  specs, grain = "person",
                                  temporal = NULL) {
  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !grain %in% c("person", "episode")) {
    stop("grain must be 'person' or 'episode'.", call. = FALSE)
  }
  for (spec in specs) {
    concepts <- spec$concept_set
    if (is.list(concepts) && !is.null(concepts$concepts)) {
      concepts <- concepts$concepts
    }
    if (!is.null(concepts)) {
      .plan_integer_vector(concepts, "feature concept_set", allow_empty = TRUE)
    }
  }
  ds.omop.plan.events(
    plan = plan,
    name = name,
    table = table,
    temporal = temporal,
    representation = list(
      format = "features",
      grain = grain,
      features = specs
    )
  )
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
#' Produces a protected cohort-membership view with \code{row_id} (the
#' \code{cohort_row_id} alias), pseudonymized \code{subject_id},
#' \code{cohort_definition_id}, and dates transformed by the configured date
#' policy (removed by default). Recurrent episodes remain distinct. Requires a
#' cohort to be set on the plan via \code{\link{ds.omop.plan.cohort}}; this is
#' not an unrestricted raw export of the OHDSI cohort table.
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
#' Requires a cohort to be set. The output contains one row per matching
#' interval and cohort episode, with columns for table source, start day, end
#' day, and optionally concept IDs filtered by \code{concept_filter}. An event
#' that overlaps multiple recurrent cohort episodes can therefore appear once
#' for each matching episode, identified by \code{cohort_row_id}.
#'
#' @param plan An \code{omop_plan} object.
#' @param tables Character vector; OMOP tables to extract intervals from.
#'   Defaults to observation_period, visit_occurrence, drug_exposure,
#'   and condition_occurrence.
#' @param concept_filter Named list; each table maps to concept IDs or a
#'   standard concept-set specification with \code{concepts}, optional
#'   descendant/mapped expansion, and exclusions. If \code{NULL}, no concept
#'   filtering is applied.
#' @param filters Optional uniquely named per-table list of reviewed filter DSL
#'   trees. Each tree applies only to its named source table.
#' @param window Optional index-relative window. Supply start/end offsets for
#'   overlap, start, or end matching, or an at offset for active-at matching.
#' @param interval_match Interval relationship: \code{"overlaps"},
#'   \code{"starts_in"}, \code{"ends_in"}, or \code{"active_at"}. Without
#'   an explicit window, matching is against the cohort episode itself.
#' @param event_select Repeated-event policy: \code{"all"}, \code{"first"},
#'   \code{"last"}, or \code{"nearest"}.
#' @param select_n Positive number of intervals retained per selection group.
#' @param select_by Group selection by episode and source, optionally also by
#'   concept.
#' @param anchor Integer days from index used by nearest-event selection.
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
                                    filters = NULL,
                                    window = NULL,
                                    interval_match = "overlaps",
                                    event_select = "all",
                                    select_n = 1L,
                                    select_by = "episode_source",
                                    anchor = 0L,
                                    name = "intervals") {
  if (!is.character(tables) || length(tables) == 0L || anyNA(tables) ||
      any(!nzchar(tables)) || anyDuplicated(tolower(tables))) {
    stop("tables must be a non-empty unique character vector.",
         call. = FALSE)
  }
  tables <- tolower(tables)
  if (!is.null(concept_filter)) {
    if (!is.list(concept_filter) || is.null(names(concept_filter)) ||
        any(!nzchar(names(concept_filter))) ||
        anyDuplicated(tolower(names(concept_filter))) ||
        length(setdiff(tolower(names(concept_filter)), tolower(tables))) > 0L) {
      stop("concept_filter must be a uniquely named per-table list.",
           call. = FALSE)
    }
    concept_filter <- lapply(concept_filter, function(ids) {
      if (!is.list(ids) || is.null(ids$concepts)) {
        return(.plan_integer_vector(ids, "concept_filter"))
      }
      allowed <- c(
        "concepts", "include_descendants", "include_mapped", "exclude"
      )
      if (is.null(names(ids)) || any(!nzchar(names(ids))) ||
          anyDuplicated(names(ids)) || length(setdiff(names(ids), allowed))) {
        stop("Interval concept-set specs may contain only concepts, ",
             "include_descendants, include_mapped, and exclude.",
             call. = FALSE)
      }
      normalized <- list(
        concepts = .plan_integer_vector(ids$concepts, "concept_filter concepts")
      )
      for (flag in c("include_descendants", "include_mapped")) {
        if (!is.null(ids[[flag]])) {
          if (!is.logical(ids[[flag]]) || length(ids[[flag]]) != 1L ||
              is.na(ids[[flag]])) {
            stop("Interval concept-set expansion flags must be TRUE/FALSE.",
                 call. = FALSE)
          }
          normalized[[flag]] <- ids[[flag]]
        }
      }
      if (!is.null(ids$exclude)) {
        normalized$exclude <- .plan_integer_vector(
          ids$exclude, "concept_filter exclude", allow_empty = TRUE
        )
      }
      normalized
    })
    names(concept_filter) <- tolower(names(concept_filter))
  }
  if (!is.null(filters) &&
      (!is.list(filters) || is.null(names(filters)) ||
       any(!nzchar(names(filters))) || anyDuplicated(tolower(names(filters))) ||
       length(setdiff(tolower(names(filters)), tolower(tables))) > 0L)) {
    stop("filters must be a uniquely named per-table list.", call. = FALSE)
  }
  if (!is.null(filters)) names(filters) <- tolower(names(filters))
  interval_match <- match.arg(
    interval_match, c("overlaps", "starts_in", "ends_in", "active_at")
  )
  if (!is.null(window)) {
    if (!is.list(window) || length(window) == 0L || is.null(names(window)) ||
        any(!nzchar(names(window))) || anyDuplicated(names(window))) {
      stop("window must be a non-empty uniquely named list.", call. = FALSE)
    }
    allowed_window <- if (identical(interval_match, "active_at")) {
      "at"
    } else {
      c("start", "end")
    }
    if (length(setdiff(names(window), allowed_window)) > 0L ||
        (identical(interval_match, "active_at") &&
         !identical(names(window), "at"))) {
      stop("window fields do not match interval_match.", call. = FALSE)
    }
    window <- lapply(window, function(value) {
      .plan_integer_scalar(value, "longitudinal window offset")
    })
    if (!is.null(window$start) && !is.null(window$end) &&
        window$start > window$end) {
      stop("window$start must not be after window$end.", call. = FALSE)
    }
  }
  event_select <- match.arg(
    event_select, c("all", "first", "last", "nearest")
  )
  select_by <- match.arg(
    select_by, c("episode_source", "episode_source_concept")
  )
  select_n <- .plan_integer_scalar(select_n, "select_n")
  if (select_n < 1L) stop("select_n must be positive.", call. = FALSE)
  anchor <- .plan_integer_scalar(anchor, "anchor")

  output <- list(
    type = "intervals_long",
    tables = tables,
    concept_filter = concept_filter,
    interval_match = interval_match,
    event_select = event_select,
    select_n = select_n,
    select_by = select_by,
    anchor = anchor
  )
  if (!is.null(filters)) output$source_filters <- filters
  if (!is.null(window)) output$window <- window
  plan$outputs[[name]] <- output
  plan
}

#' Add a temporal (time-binned) covariates output to the plan
#'
#' Produces FeatureExtraction-style sparse covariates binned into
#' time windows relative to the cohort index date. Returns four symbols
#' on the server: \code{<name>.temporalCovariates},
#' \code{<name>.covariateRef}, \code{<name>.timeRef}, and
#' \code{<name>.personRef}. The last maps cohort episodes to pseudonymous
#' persons. Requires a cohort to be set.
#'
#' @param plan An \code{omop_plan} object.
#' @param table Character; source OMOP table to extract covariates from.
#' @param concept_set Optional concept IDs or an OHDSI-style concept-set spec
#'   with \code{concepts}, \code{include_descendants},
#'   \code{include_mapped}, and \code{exclude}. When \code{NULL}, all concepts
#'   present in the bounded event stream are retained, subject to the server
#'   concept cap.
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
                                              concept_set = NULL,
                                              bin_width = 30L,
                                              window_start = -365L,
                                              window_end = 0L,
                                              analyses = c("binary"),
                                              name = "temporal") {
  concept_set <- .plan_concept_set(
    concept_set, "concept_set", allow_null = TRUE
  )
  bin_width <- .plan_integer_scalar(bin_width, "bin_width", min_value = 1L)
  window_start <- .plan_integer_scalar(window_start, "window_start")
  window_end <- .plan_integer_scalar(window_end, "window_end")
  if (window_start > window_end) {
    stop("window_start must not be after window_end.", call. = FALSE)
  }
  if (!is.character(analyses) || length(analyses) == 0L || anyNA(analyses) ||
      any(!analyses %in% c("binary", "count")) || anyDuplicated(analyses)) {
    stop("analyses must contain unique values from 'binary' and 'count'.",
         call. = FALSE)
  }
  plan$outputs[[name]] <- list(
    type = "temporal_covariates",
    table = table,
    concept_set = concept_set,
    bin_width = bin_width,
    window_start = window_start,
    window_end = window_end,
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
#' @param factor_concepts Logical; if \code{TRUE} (default), after a
#'   memory-mode execution every \code{_concept_id} column is converted
#'   into a factor whose levels are harmonized across all connected
#'   servers, so pooled \code{ds.glm}/\code{ds.glmSLMA}/\code{ds.table}
#'   see an identical level coding. Columns whose distinct values exceed
#'   the server disclosure cap are left raw. Set \code{FALSE} to keep the
#'   raw integer ids (or translated character names) unchanged.
#' @return The modified \code{omop_plan} with updated options.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.options(plan,
#'   translate_concepts = TRUE,
#'   block_sensitive = TRUE,
#'   factor_concepts = TRUE
#' )
#' }
#' @seealso \code{\link{ds.omop.plan}}, \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.options <- function(plan,
                                 translate_concepts = NULL,
                                 block_sensitive = NULL,
                                 factor_concepts = NULL) {
  if (!is.null(translate_concepts)) {
    plan$options$translate_concepts <- translate_concepts
  }
  if (!is.null(block_sensitive)) {
    plan$options$block_sensitive <- block_sensitive
  }
  if (!is.null(factor_concepts)) {
    plan$options$factor_concepts <- factor_concepts
  }
  plan
}

#' Build a temporal filtering specification
#'
#' Creates an \code{omop_temporal_spec} object that defines how events
#' are filtered relative to a cohort index date or calendar dates. The
#' spec can combine index-relative windows, calendar date ranges, and event
#' selection (first/last N) and deterministic minimum-gap collapsing.
#'
#' @param index_window Named list with \code{start} and \code{end}
#'   (integer days relative to the cohort index date). Negative values
#'   denote time before the index date.
#' @param calendar Named list with \code{start} and \code{end}
#'   (character ISO 8601 dates, e.g. \code{"2020-01-01"}).
#' @param event_select Named list with \code{order} (\code{"first"} or
#'   \code{"last"}), \code{n} (integer; number of events), and optional
#'   \code{by = "grain"} (default) or \code{by = "concept"}. Concept mode
#'   keeps the first/last N independently for each concept within a cohort
#'   episode when \code{index_window} is present, otherwise within each person.
#' @param min_gap Positive integer days, or a named list with
#'   \code{days}, optional \code{by = "concept"} or \code{"grain"}, and
#'   optional \code{keep = "first"} or \code{"last"}. Adjacent events no
#'   more than \code{days} apart form one chain. The normalized policy defaults
#'   to concept-specific chains represented by their first event.
#' @return An \code{omop_temporal_spec} object (a list with class
#'   \code{c("omop_temporal_spec", "list")}).
#' @examples
#' \dontrun{
#' # Events within 1 year before each index episode; keep its first 3 events
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
  if (!is.null(index_window)) {
    spec$index_window <- .plan_day_window(index_window, "index_window")
  }
  if (!is.null(calendar)) {
    spec$calendar <- .plan_calendar_window(calendar)
  }
  if (!is.null(event_select)) {
    if (!is.list(event_select) || is.null(names(event_select)) ||
        any(!nzchar(names(event_select))) || anyDuplicated(names(event_select)) ||
        !all(c("order", "n") %in% names(event_select)) ||
        length(setdiff(names(event_select), c("order", "n", "by"))) > 0L) {
      stop("event_select must contain order, n, and optional by.",
           call. = FALSE)
    }
    by <- match.arg(event_select$by %||% "grain", c("grain", "concept"))
    spec$event_select <- list(
      order = match.arg(event_select$order, c("first", "last")),
      n = .plan_integer_scalar(event_select$n, "event_select$n", 1L),
      by = by
    )
  }
  if (!is.null(min_gap)) {
    if (!is.list(min_gap)) min_gap <- list(days = min_gap)
    if (is.null(names(min_gap)) || any(!nzchar(names(min_gap))) ||
        anyDuplicated(names(min_gap)) ||
        length(setdiff(names(min_gap), c("days", "by", "keep"))) > 0L) {
      stop("min_gap must be one integer or a named days/by/keep policy.",
           call. = FALSE)
    }
    spec$min_gap <- list(
      days = .plan_integer_scalar(min_gap$days, "min_gap$days", 1L),
      by = match.arg(min_gap$by %||% "concept", c("concept", "grain")),
      keep = match.arg(min_gap$keep %||% "first", c("first", "last"))
    )
  }
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
#' @param mode Character; transformation mode. Defaults to \code{"remove"}
#'   (privacy-preserving). One of \code{"absolute"}
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
omop.date_handling <- function(mode = "remove", reference = "index",
                               bin_width = NULL, date_columns = NULL) {
  mode <- match.arg(mode, c("remove", "relative", "binned", "absolute"))
  reference <- match.arg(reference, "index")
  if (identical(mode, "binned")) {
    if (is.null(bin_width)) {
      stop("bin_width is required when mode = 'binned'.", call. = FALSE)
    }
    bin_width <- match.arg(bin_width, c("week", "month", "year"))
  } else if (!is.null(bin_width)) {
    stop("bin_width is only valid when mode = 'binned'.", call. = FALSE)
  }
  if (!is.null(date_columns)) {
    if (!is.character(date_columns) || length(date_columns) == 0L ||
        anyNA(date_columns) || any(!nzchar(date_columns)) ||
        any(!grepl("^[A-Za-z][A-Za-z0-9_]*$", date_columns))) {
      stop("date_columns must contain non-empty column identifiers.",
           call. = FALSE)
    }
    date_columns <- unique(tolower(date_columns))
  }
  list(mode = mode, reference = reference,
       bin_width = bin_width, date_columns = date_columns)
}

#' Prepare a plan for the exact federation used by an operation
#'
#' Multi-server validate, preview and execute all use the same strict schema and
#' semantic binding. A newly harmonized contract is checked structurally and
#' against the plan signature; a pre-existing contract is fully re-introspected.
#'
#' @param plan An \code{omop_plan}.
#' @param symbol Client OMOP session name.
#' @param conns DSI connections.
#' @return The prepared plan.
#' @keywords internal
.prepare_plan_for_federation <- function(plan, symbol, conns) {
  if (length(conns) > 1L && is.null(plan$harmonization)) {
    plan <- ds.omop.plan.harmonize(
      plan, mode = "intersection", strict = TRUE,
      symbol = symbol, conns = conns
    )
    contract <- plan$harmonization
    required <- c("version", "mode", "manifest", "schema", "plan_signature")
    if (!is.list(contract) || !all(required %in% names(contract)) ||
        !identical(contract$version, 2L) ||
        !identical(contract$mode, "intersection") ||
        !is.list(contract$manifest) || !is.list(contract$schema) ||
        !identical(contract$plan_signature,
                   .plan_harmonization_signature(plan))) {
      stop("Strict multi-server harmonization did not produce a valid bound ",
           "contract; execution is blocked.", call. = FALSE)
    }
  } else {
    .verify_plan_schema_harmonization(plan, symbol, conns)
  }
  plan
}

#' Validate an extraction plan
#'
#' Sends the plan to each connected server for structural validation,
#' checking for missing required fields, invalid table references,
#' unsupported output types, and schema compatibility issues. This
#' performs a server-side check (via \code{omopPlanPreviewDS}) but does
#' not execute the plan or create any data. Use this to catch errors
#' before calling \code{\link{ds.omop.plan.execute}}.
#' Multi-server validation first establishes or revalidates the same strict
#' schema/semantic harmonization contract used by execution.
#'
#' Note: \code{ds.omop.plan.validate} and \code{\link{ds.omop.plan.preview}}
#' call the same server endpoint (\code{omopPlanPreviewDS}) and therefore
#' return the \emph{same} structure; the difference is only intent. Read the
#' \code{$validation} element (\code{valid}/\code{errors}/\code{warnings}) for
#' a pass/fail check here, and the \code{$outputs} element for the per-output
#' detail under \code{\link{ds.omop.plan.preview}}.
#'
#' @param plan An \code{omop_plan} object.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return A named list (one element per server). Each server's result is the
#'   preview payload, whose \code{$validation} sub-list reports
#'   \code{valid} (logical), \code{errors}, \code{warnings}, and
#'   \code{available_tables}. No raw rows or SQL are returned; the shared
#'   preview payload can include only the optional disclosure-banded person
#'   count described in \code{\link{ds.omop.plan.preview}}.
#' @examples
#' \dontrun{
#' result <- ds.omop.plan.validate(my_plan)
#' # Check a specific server's pass/fail and messages
#' result$server1$validation$valid
#' result$server1$validation$errors
#' result$server1$validation$warnings
#' }
#' @seealso \code{\link{ds.omop.plan.preview}},
#'   \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.validate <- function(plan, symbol = "omop",
                                  conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  plan <- .prepare_plan_for_federation(plan, symbol, conns)
  contract <- .session_harmonization_for_connections(session, conns)
  if (!is.null(contract)) {
    plan <- .apply_plan_harmonization(plan, contract)
    .validate_plan_harmonization(plan, contract)
  }

  .ds_safe_aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, .ds_encode(plan))
  )
}

#' Preview a plan (server-side dry run)
#'
#' Sends the plan to each connected server for a dry-run preview that, per
#' output, reports its expected/resolvable columns and any requested-but-missing
#' source columns without creating output data. A disclosure-banded
#' distinct-person count is included only when it can be computed honestly from
#' an unscoped, unfiltered, unreduced source. Cohort-, population-, filter-,
#' temporal- or feature-scoped outputs instead return \code{n_persons = NA},
#' \code{n_persons_available = FALSE}, and a reason; they are never labelled
#' with the whole source table's population. Available counts are banded down to
#' a multiple of the server's \code{band_width} and suppressed below the
#' disclosure floor. Raw row counts, min/max and SQL are never returned.
#' Multi-server preview first establishes or revalidates the same strict
#' schema/semantic harmonization contract used by execution.
#'
#' Note: \code{ds.omop.plan.preview} and \code{\link{ds.omop.plan.validate}}
#' call the same server endpoint (\code{omopPlanPreviewDS}) and return the
#' same structure; the distinction is only intent. Read \code{$outputs} here
#' for per-output detail, and \code{$validation} under
#' \code{\link{ds.omop.plan.validate}} for the pass/fail check.
#'
#' @param plan An \code{omop_plan} object.
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @return A named list (one element per server). Each server's result holds
#'   \code{$validation} (see \code{\link{ds.omop.plan.validate}}),
#'   \code{$band_width} (the count-banding granularity), and \code{$outputs},
#'   a per-output list with \code{columns}, \code{missing_columns},
#'   \code{n_persons} (banded when available),
#'   \code{n_persons_available}, \code{n_persons_unavailable_reason},
#'   \code{n_persons_banded}, \code{disclosive}, and \code{representation}.
#' @examples
#' \dontrun{
#' preview <- ds.omop.plan.preview(my_plan)
#' # Resolvable columns for the "baseline" output on one server
#' preview$server1$outputs$baseline$columns
#' # Disclosure-banded count, or NA plus a reason for scoped outputs
#' preview$server1$outputs$baseline$n_persons
#' preview$server1$outputs$baseline$n_persons_available
#' preview$server1$outputs$baseline$n_persons_unavailable_reason
#' }
#' @seealso \code{\link{ds.omop.plan.validate}},
#'   \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.preview <- function(plan, symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  plan <- .prepare_plan_for_federation(plan, symbol, conns)
  contract <- .session_harmonization_for_connections(session, conns)
  if (!is.null(contract)) {
    plan <- .apply_plan_harmonization(plan, contract)
    .validate_plan_harmonization(plan, contract)
  }

  .ds_safe_aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, .ds_encode(plan))
  )
}

#' Execute a plan and create server-side tables
#'
#' Sends the plan to each connected server for full execution. The
#' server-side \code{omopPlanExecuteDS} function processes the plan and
#' assigns each output directly into the DataSHIELD session as named
#' symbols specified in the \code{out} mapping. After execution, the
#' symbols can be used with standard DataSHIELD analysis functions.
#' Sparse outputs are split into multiple symbols:
#' \code{<name>.covariates}, \code{<name>.covariateRef}, and
#' \code{<name>.personRef}. Temporal covariates analogously use
#' \code{<name>.temporalCovariates}, \code{<name>.covariateRef},
#' \code{<name>.timeRef}, and
#' \code{<name>.personRef}. Person-period outputs additionally assign
#' \code{<name>.personPeriods}, the complete episode-by-bin roster.
#' Recurrent-event survival outputs assign \code{<name>.events} and
#' \code{<name>.riskSets}.
#' Multi-state outputs assign \code{<name>.msdata} and the public transition
#' dictionary \code{<name>.transitionRef}.
#'
#' When \code{output_mode = "staged"}, outputs are written to server-local
#' Parquet files (CSV fallback when Arrow is unavailable) and
#' assigned as \code{FlowerDatasetDescriptor} objects instead of final
#' data.frames. Long event and interval outputs preserve numeric OMOP concept
#' IDs and stream in bounded chunks to Parquet row groups in one file; labels
#' can be supplied as a separate concept-reference output. Outputs that still
#' require an R-side reshape or
#' derivation are materialized before staging. Descriptors are
#' server paths readable under the server OS identity; other service accounts
#' require a separately reviewed broker. They are not client download URLs and
#' do not by themselves establish compatibility with a particular external
#' package.
#' Cleanup is all-or-none for DataSHIELD-visible symbols, not a distributed
#' filesystem transaction: after a cross-node staged failure, private files may
#' remain registered with a successful server handle until its cleanup or
#' disconnect path runs.
#'
#' With two or more servers, execution first establishes (or revalidates) a
#' strict schema/semantic harmonization contract. This binds the plan to the
#' exact participating nodes, required OMOP columns and compatible type
#' families; vocabulary-dependent plans additionally require one reported
#' vocabulary version. All expected output components must land on every node.
#' A node failure, incomplete composite output, or factor-harmonization failure
#' removes the exact symbols owned by the attempted execution and fails closed.
#' Every requested output family must be absent on every server before execution;
#' existing workspace objects are never deleted or overwritten speculatively.
#' Choose fresh \code{out} names (or explicitly remove obsolete objects through
#' the ordinary DataSHIELD workspace API) when rerunning a plan.
#'
#' @param plan An \code{omop_plan} object.
#' @param out Optional output-to-symbol mapping. Three forms are accepted:
#'   \itemize{
#'     \item \strong{Missing or \code{NULL} (default):} symbol names are
#'       auto-derived for \emph{every} plan output exactly as
#'       \code{\link{recipe_execute}} does — the output's own
#'       \code{result_symbol} when set, otherwise \code{D_<name>} (so an
#'       output named \code{baseline} becomes symbol \code{D_baseline}).
#'       Single-output plans therefore just work with no \code{out}.
#'     \item \strong{A bare unnamed string} (e.g. \code{out = "D"}): allowed
#'       only when the plan has exactly \emph{one} output, which is bound to
#'       that symbol. With multiple outputs this stops with an error asking
#'       you to use the named form.
#'     \item \strong{A named character vector} (e.g.
#'       \code{c(baseline = "D_base", survival = "D_tte")}): maps each named
#'       plan output to its server-side symbol. This advanced multi-output
#'       form is unchanged and fully backward compatible.
#'   }
#' @param symbol Character; name of the OMOP session symbol on the
#'   server (default \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses the
#'   connections stored in the session.
#' @param output_mode Character; \code{"memory"} (default, backwards
#'   compatible) or \code{"staged"} (writes server-local files and returns
#'   descriptors). Arrow provides Parquet; without it the server uses CSV.
#' @return Invisible; the resolved \code{out} symbol mapping (for chaining).
#'   The produced symbols are also recorded on the session so subsequent
#'   manipulation wrappers (\code{\link{ds.omop.merge}},
#'   \code{\link{ds.omop.filter}}, \code{\link{ds.omop.select}},
#'   \code{\link{ds.omop.bind_rows}}) can default to the last one.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#'
#' # Simplest case: single-output plan, bind to "D".
#' ds.omop.plan.execute(plan, out = "D")
#'
#' # Or omit out entirely to auto-derive D_<name> for every output.
#' ds.omop.plan.execute(plan)
#'
#' # Advanced: multiple outputs, each mapped explicitly.
#' plan <- ds.omop.plan.events(plan, "conditions",
#'   "condition_occurrence", concept_set = c(201826))
#' ds.omop.plan.execute(plan,
#'   out = c(baseline = "D_base", conditions = "D_cond")
#' )
#'
#' # Staged mode for large extractions
#' ds.omop.plan.execute(plan,
#'   out = c(features = "D_features"),
#'   output_mode = "staged"
#' )
#' # D_features is now a FlowerDatasetDescriptor on the server
#' }
#' @seealso \code{\link{ds.omop.plan.validate}},
#'   \code{\link{ds.omop.plan.preview}}
#' @export
ds.omop.plan.execute <- function(plan, out = NULL,
                                 symbol = "omop",
                                 conns = NULL,
                                 output_mode = "memory") {
  output_mode <- match.arg(output_mode, c("memory", "staged"))
  if (identical(output_mode, "staged")) {
    # Standard concept identifiers are the stable OHDSI interchange encoding
    # and allow the server to stream fact tables without per-chunk vocabulary
    # lookups on an active DB cursor.
    if (!identical(plan$options$translate_concepts, FALSE)) {
      plan$options$translate_concepts <- FALSE
      # Translation changes the output schema covered by a pre-existing
      # federation signature. Force the normal preparation path to rebuild the
      # contract for the staged representation instead of rejecting it as a
      # post-harmonization mutation or, worse, reusing stale schema evidence.
      plan$harmonization <- NULL
    }
  }
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  plan <- .prepare_plan_for_federation(plan, symbol, conns)
  contract <- .session_harmonization_for_connections(session, conns)
  if (!is.null(contract)) {
    plan <- .apply_plan_harmonization(plan, contract)
    .validate_plan_harmonization(plan, contract)
  }

  # Resolve the out mapping: NULL -> auto-derive D_<name> for every output;
  # a bare unnamed string -> bind the single output; a named vector -> as-is.
  out <- .resolve_plan_out(
    plan, out,
    reserved_symbols = c(symbol, session$res_symbol)
  )
  owned <- .plan_expected_output_symbols(plan, out)
  cleanup_family <- .plan_output_symbol_families(out)
  reserved_targets <- unique(unlist(cleanup_family, use.names = FALSE))

  # Recipe-level scope: a cohort reference and/or omop.table symbol(s) the server
  # folds into ONE cohort and intersects into every population. Validate symbol
  # collisions locally before asking any server for its workspace inventory.
  scope_args <- NULL
  combine <- "union"
  if (!is.null(plan$scope) &&
      (!is.null(plan$scope$cohort) || length(plan$scope$tables) > 0)) {
    scope_args <- .analysis_scope_expr(cohort = plan$scope$cohort,
                                       tables = plan$scope$tables)
    combine <- plan$scope$combine %||% "union"
  }

  scope_tables <- as.character(unlist(
    plan$scope$tables %||% character(0), use.names = FALSE
  ))
  collisions <- intersect(reserved_targets, scope_tables)
  if (length(collisions) > 0L) {
    stop("Output symbols collide with execution-scope input symbol(s): ",
         paste(collisions, collapse = ", "), ". Choose different outputs.",
         call. = FALSE)
  }

  # A distributed overwrite cannot be rolled back if one node fails after old
  # objects have been removed. Require fresh destinations instead, including
  # every reserved composite sibling, and prove absence federation-wide before
  # the server is allowed to create anything.
  preflight_inventory <- .plan_symbol_inventory(
    conns, "plan output reservation"
  )
  occupied <- unlist(lapply(names(preflight_inventory), function(server) {
    found <- intersect(reserved_targets, preflight_inventory[[server]])
    if (length(found) == 0L) character(0) else paste0(server, ":", found)
  }), use.names = FALSE)
  if (length(occupied) > 0L) {
    stop("Plan output destinations must be fresh on every server; occupied: ",
         paste(occupied, collapse = ", "),
         ". Choose different out names or explicitly remove obsolete objects.",
         call. = FALSE)
  }

  # Single assign call: server assigns each output directly into session
  exec_symbol <- NULL
  for (attempt in seq_len(10L)) {
    candidate <- .generate_symbol("dsOexec")
    if (!candidate %in% reserved_targets &&
        all(!vapply(preflight_inventory, function(x) candidate %in% x,
                    logical(1)))) {
      exec_symbol <- candidate
      break
    }
  }
  if (is.null(exec_symbol)) {
    stop("Could not reserve a fresh plan execution status symbol.",
         call. = FALSE)
  }

  exec_args <- list(
    as.name("omopPlanExecuteDS"),
    session$res_symbol, .ds_encode(plan), .ds_encode(out),
    output_mode)
  if (!is.null(scope_args)) {
    # Splice the closed named scope arguments. Cohorts remain literal `scope`;
    # tables are bare `scope_table_<n>` symbols, never nested list()/c() calls.
    exec_args <- c(exec_args, scope_args, list(combine = combine))
  }

  expected_servers <- names(conns)
  assign_success <- character(0)
  assign_errors <- character(0)
  assign_condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns,
      symbol = exec_symbol,
      expr = as.call(exec_args),
      success = function(server) {
        assign_success <<- c(assign_success, server)
      },
      error = function(server, message) {
        assign_errors[[server]] <<- message
      }
    )
    NULL
  }, error = identity)

  # exec_symbol holds TRUE (return value); clean up
  tryCatch(
    DSI::datashield.rm(conns, exec_symbol),
    error = function(e) NULL
  )

  failed_servers <- unique(c(
    names(assign_errors), setdiff(expected_servers, assign_success)
  ))
  if (!is.null(assign_condition) || length(failed_servers) > 0L) {
    .plan_remove_output_symbols(conns, cleanup_family, verify = TRUE)
    detail <- if (!is.null(assign_condition)) {
      conditionMessage(assign_condition)
    } else {
      paste(failed_servers, collapse = ", ")
    }
    stop("Plan execution failed or was incomplete; freshly assigned outputs ",
         "were removed. Affected server(s): ", detail, ".", call. = FALSE)
  }

  missing <- .plan_missing_output_symbols(conns, owned)
  if (length(missing) > 0L) {
    .plan_remove_output_symbols(conns, cleanup_family, verify = TRUE)
    stop("Plan execution did not materialize every requested output ",
         "component; freshly assigned outputs were removed: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }

  # Coordination layer: harmonize concept-id columns into factors with one
  # shared level ordering across the federation. Only meaningful in memory
  # mode (staged outputs are Parquet descriptors, not in-R data frames).
  if (identical(output_mode, "memory") &&
        isTRUE(plan$options$factor_concepts %||% TRUE)) {
    .harmonizeConceptFactors(owned, conns)
  }

  # Record the produced symbols on the session so manipulation wrappers can
  # default to the most recently created one (no need to re-type symbols).
  .record_session_outputs(symbol, owned)

  invisible(out)
}

#' Resolve the out argument of ds.omop.plan.execute into a named mapping
#'
#' Normalises the three accepted \code{out} forms into the named
#' \code{output -> symbol} character vector the server expects. \code{NULL}
#' auto-derives a symbol for every plan output exactly as
#' \code{\link{recipe_execute}} does (an output's \code{result_symbol} when
#' set, else \code{D_<name>}). A bare unnamed string is bound to the plan's
#' sole output, or stops with an instructive error when the plan has several
#' outputs. A named vector is validated and returned unchanged.
#'
#' @param plan An \code{omop_plan} object.
#' @param out \code{NULL}, a bare unnamed string, or a named character vector.
#' @param reserved_symbols Optional server symbols that outputs may not replace.
#' @return A named character vector mapping output names to server symbols.
#' @keywords internal
.resolve_plan_out <- function(plan, out, reserved_symbols = character(0)) {
  out_names <- names(plan$outputs)
  if (length(out_names) == 0L) {
    stop("plan has no outputs to execute; add an output first (e.g. ",
         "ds.omop.plan.baseline / ds.omop.plan.events).", call. = FALSE)
  }

  # (a) Missing / NULL: auto-derive a symbol for every output, mirroring
  # recipe_execute (result_symbol when present, else D_<name>).
  if (is.null(out)) {
    symbols <- vapply(out_names, function(nm) {
      rs <- plan$outputs[[nm]]$result_symbol
      if (!is.null(rs)) as.character(rs)[1L] else paste0("D_", nm)
    }, character(1))
    out <- stats::setNames(symbols, out_names)
  } else if (!is.character(out) || length(out) == 0L) {
    stop("out must be NULL, a single symbol name, or a named character ",
         "vector mapping output names to symbols.", call. = FALSE)
  } else if (is.null(names(out))) {
    # (b) Bare unnamed string: only valid for a single-output plan.
    if (length(out) != 1L) {
      stop("out must be NULL, a single symbol name, or a named character ",
           "vector mapping output names to symbols.", call. = FALSE)
    }
    if (length(out_names) != 1L) {
      stop("plan has ", length(out_names), " outputs (",
           paste(out_names, collapse = ", "),
           "); a bare out=\"symbol\" is only allowed for a single-output ",
           "plan. Use the named form, e.g. out = c(",
           paste0(out_names, " = \"D_", out_names, "\"", collapse = ", "),
           ").", call. = FALSE)
    }
    out <- stats::setNames(out, out_names)
  } else {
    # (c) Named vector: keep backward compatible, but flag unknown outputs.
    if (anyNA(names(out)) || any(!nzchar(names(out))) ||
        anyDuplicated(names(out))) {
      stop("out must have unique, non-empty output names.", call. = FALSE)
    }
    unknown <- setdiff(names(out), out_names)
    if (length(unknown) > 0L) {
      stop("out names not among plan outputs (",
           paste(out_names, collapse = ", "), "): ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
  }

  if (anyNA(out) || any(!nzchar(out)) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9._]*$", out))) {
    stop("Every output symbol must be one simple non-reserved R name.",
         call. = FALSE)
  }
  if (anyDuplicated(unname(out)) ||
      any(out %in% reserved_symbols) ||
      any(grepl("^(\\.dsomop_|handle_)", out))) {
    stop("Output symbols must be unique and cannot target OMOP resources or ",
         "reserved handle names.", call. = FALSE)
  }
  .plan_output_symbol_families(out)
  out
}

#' Derive the exact server symbols owned by a plan execution
#'
#' Composite OHDSI-style outputs are split by the server into a fixed set of
#' symbols. Deriving that set from the requested output type/format avoids
#' treating stale, merely prefix-matching symbols as part of the current run.
#'
#' @param plan An \code{omop_plan}.
#' @param out Named output-to-symbol mapping.
#' @return Named list, one exact character vector per requested output.
#' @keywords internal
.plan_expected_output_symbols <- function(plan, out) {
  stats::setNames(lapply(names(out), function(output_name) {
    spec <- plan$outputs[[output_name]]
    base <- unname(out[[output_name]])
    type <- tolower(spec$type %||% "event_level")
    format <- if (identical(type, "event_level")) {
      tolower(spec$representation$format %||% "long")
    } else {
      ""
    }
    if (identical(type, "person_period")) {
      return(paste0(base, c(
        ".temporalCovariates", ".covariateRef", ".timeRef", ".personRef",
        ".personPeriods"
      )))
    }
    if (identical(type, "temporal_covariates")) {
      return(paste0(base, c(
        ".temporalCovariates", ".covariateRef", ".timeRef", ".personRef"
      )))
    }
    if (identical(format, "sparse")) {
      return(paste0(base, c(".covariates", ".covariateRef", ".personRef")))
    }
    if (identical(type, "survival") &&
        identical(tolower(spec$format %||% "survival"),
                  "recurrent_events")) {
      return(paste0(base, c(".events", ".riskSets")))
    }
    if (identical(type, "survival") &&
        identical(tolower(spec$format %||% "survival"), "multi_state")) {
      return(paste0(base, c(".msdata", ".transitionRef")))
    }
    base
  }), names(out))
}

#' Derive the exact reserved symbol family for an output mapping
#'
#' A base symbol can be reused across ordinary, sparse, temporal and
#' person-period executions. Clearing this finite reserved family prevents a
#' component from a previous representation being mistaken for the current
#' result while leaving unrelated prefix-sharing user objects untouched.
#'
#' @param out Named output-to-base-symbol mapping.
#' @return Named list of exact reserved symbols per requested output.
#' @keywords internal
.plan_output_symbol_families <- function(out) {
  suffixes <- c(
    "", ".covariates", ".covariateRef", ".personRef",
    ".temporalCovariates", ".timeRef", ".personPeriods", ".events",
    ".riskSets", ".msdata", ".transitionRef"
  )
  families <- stats::setNames(lapply(unname(out), function(base) {
    paste0(base, suffixes)
  }), names(out))
  flat <- unlist(families, use.names = FALSE)
  if (anyDuplicated(flat)) {
    stop("Requested output symbols have overlapping reserved component ",
         "families; choose distinct base symbols.", call. = FALSE)
  }
  families
}

.plan_symbol_inventory <- function(conns, context) {
  expected_servers <- names(conns)
  if (is.null(expected_servers) || length(expected_servers) == 0L ||
      anyNA(expected_servers) || any(!nzchar(expected_servers)) ||
      anyDuplicated(expected_servers)) {
    stop("DataSHIELD connections must have unique server names before ",
         context, ".", call. = FALSE)
  }
  inventory <- tryCatch(
    DSI::datashield.symbols(conns),
    error = function(e) {
      stop("Cannot verify server symbols during ", context, ": ",
           conditionMessage(e), call. = FALSE)
    }
  )
  missing_servers <- setdiff(expected_servers, names(inventory))
  invalid_servers <- intersect(expected_servers, names(inventory))[
    !vapply(inventory[intersect(expected_servers, names(inventory))],
            is.character, logical(1))
  ]
  if (length(missing_servers) > 0L || length(invalid_servers) > 0L) {
    stop("Cannot verify server symbols during ", context, " on: ",
         paste(unique(c(missing_servers, invalid_servers)), collapse = ", "),
         ".", call. = FALSE)
  }
  inventory[expected_servers]
}

.plan_remove_output_symbols <- function(conns, owned, verify = FALSE) {
  targets <- unique(unlist(owned, use.names = FALSE))
  for (server in names(conns)) {
    for (target in targets) {
      tryCatch(
        DSI::datashield.rm(conns[server], target),
        error = function(e) NULL
      )
    }
  }
  if (isTRUE(verify)) {
    remaining <- .plan_symbol_inventory(conns, "failed-execution cleanup")
    lingering <- unlist(lapply(names(remaining), function(server) {
      found <- intersect(targets, remaining[[server]])
      if (length(found) == 0L) character(0) else paste0(server, ":", found)
    }), use.names = FALSE)
    if (length(lingering) > 0L) {
      stop("Plan execution failed and cleanup could not be proven for: ",
           paste(lingering, collapse = ", "), ". Remove these server-side ",
           "symbols before continuing.", call. = FALSE)
    }
  }
  invisible(NULL)
}

.plan_missing_output_symbols <- function(conns, owned) {
  expected <- unique(unlist(owned, use.names = FALSE))
  inventory <- .plan_symbol_inventory(conns, "post-execution verification")
  unlist(lapply(names(inventory), function(server) {
    absent <- setdiff(expected, inventory[[server]])
    if (length(absent) == 0L) character(0) else paste0(server, ":", absent)
  }), use.names = FALSE)
}

#' Record execute-produced symbols on the stored session
#'
#' After a plan or recipe execution, stamps the produced server-side symbols
#' onto the \code{omop_session} held in \code{.dsomop_client_env} so the
#' manipulation wrappers can default their target symbol to the most recent
#' output. Updates \code{session$outputs} (accumulated, de-duplicated) and
#' \code{session$last_output} (the final symbol of this execution), then
#' persists the session back into the registry. Never throws: a missing
#' session simply skips recording.
#'
#' @param symbol Character; the session symbol used for the execution.
#' @param out Named list of exact produced component symbols, or a character
#'   vector for backwards-compatible internal use.
#' @return \code{NULL} invisibly.
#' @keywords internal
.record_session_outputs <- function(symbol, out) {
  produced <- unname(unlist(out, use.names = FALSE))
  produced <- produced[nzchar(produced)]
  if (length(produced) == 0L) return(invisible(NULL))
  if (!exists(symbol, envir = .dsomop_client_env)) return(invisible(NULL))

  session <- get(symbol, envir = .dsomop_client_env)
  session$outputs <- unique(c(session$outputs, produced))
  session$last_output <- if (is.list(out)) {
    last <- out[[length(out)]]
    unname(last[[1L]])
  } else produced[[length(produced)]]
  assign(symbol, session, envir = .dsomop_client_env)
  invisible(NULL)
}

#' Resolve a manipulation wrapper's target symbol, defaulting to the session
#'
#' The data-manipulation verbs (\code{\link{ds.omop.merge}},
#' \code{\link{ds.omop.filter}}, \code{\link{ds.omop.select}},
#' \code{\link{ds.omop.bind_rows}}) operate on the NAME of a server-side
#' \code{omop.table} symbol. When the caller omits it, fall back to the
#' session's \code{last_output} (the symbol most recently produced by
#' \code{\link{ds.omop.plan.execute}} / \code{\link{recipe_execute}}), so a
#' user need not re-type it. An explicit value always wins.
#'
#' @param x The caller-supplied symbol name, or \code{NULL} to use the
#'   session default.
#' @param session The \code{omop_session} object.
#' @param arg Character; the argument name, used only in error messages.
#' @return A single character symbol name.
#' @keywords internal
.resolve_target_symbol <- function(x, session, arg = "x") {
  if (is.null(x)) {
    x <- session$last_output
    if (is.null(x)) {
      stop(arg, " not supplied and the session has no recorded output yet. ",
           "Run ds.omop.plan.execute() / recipe_execute() first, or pass ",
           arg, " explicitly.", call. = FALSE)
    }
  }
  if (!is.character(x) || length(x) != 1L) {
    stop(arg, " must be the name of a server-side omop.table symbol.",
         call. = FALSE)
  }
  x
}

#' Harmonize concept-id columns into federation-wide factors
#'
#' Cross-server coordination layer invoked after a memory-mode plan
#' execution. For each freshly assigned output symbol it collects every
#' server's disclosure-safe \code{_concept_id} levels, computes their union
#' in one deterministic order client-side, and broadcasts that ordering back
#' so each server recodes the columns as factors that share identical level
#' coding. This is what makes pooled \code{ds.glm}, \code{ds.glmSLMA}, and
#' \code{ds.table} behave correctly on the federated factor.
#'
#' A value present on only some sites becomes an empty level on the sites
#' that lack it (valid base R; the modelling functions tolerate it). Every
#' expected output component must exist on every server. Any discovery,
#' level-collection, or recoding error fails closed and removes the freshly
#' assigned output symbols rather than leaving inconsistent factors available.
#'
#' @param owned Named list mapping plan outputs to their exact server symbols.
#' @param conns DSI connections object.
#' @return \code{NULL} invisibly; the server symbols are modified in place.
#' @keywords internal
.harmonizeConceptFactors <- function(owned, conns) {
  symbols <- unique(unlist(owned, use.names = FALSE))
  symbols <- symbols[nzchar(symbols)]
  if (length(symbols) == 0L) {
    return(invisible(NULL))
  }
  expected_servers <- names(conns)
  cleanup <- function() {
    .plan_remove_output_symbols(conns, owned, verify = TRUE)
  }
  existing <- tryCatch(
    .plan_symbol_inventory(conns, "concept-factor harmonization"),
    error = function(e) {
      cleanup()
      stop("Cannot verify output components before concept-factor ",
           "harmonization: ", conditionMessage(e), call. = FALSE)
    }
  )
  for (output_name in names(owned)) {
    expected <- sort(unique(owned[[output_name]]))
    for (server in expected_servers) {
      landed <- sort(intersect(existing[[server]], expected))
      if (!identical(landed, expected)) {
        cleanup()
        stop("Output '", output_name,
             "' did not produce its exact component set on server '", server,
             "'; concept factors were not harmonized.", call. = FALSE)
      }
    }
  }
  tryCatch({
    for (sym in symbols) .harmonizeOneSymbol(sym, conns)
  }, error = function(e) {
    cleanup()
    stop("Concept-factor harmonization failed; freshly assigned outputs were ",
         "removed: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

#' Harmonize the concept-id columns of one server-side symbol
#'
#' Implements the three-phase coordination for a single symbol: (1) aggregate
#' each server's safe levels via \code{omopFactorLevelsDS}; (2) union them
#' client-side into one deterministic ordering (numeric ids sorted
#' numerically, character names sorted lexically), dropping any column flagged
#' unsafe on \emph{any} server and any union exceeding the smallest server
#' cap; (3) broadcast the shared spec back via \code{omopAsFactorColumnsDS}.
#'
#' @param sym Character; the server-side symbol to harmonize.
#' @param conns DSI connections object restricted to servers holding
#'   \code{sym}.
#' @return \code{NULL} invisibly.
#' @keywords internal
.harmonizeOneSymbol <- function(sym, conns) {
  expected_servers <- names(conns)
  aggregate_success <- character(0)
  aggregate_errors <- character(0)
  per_server <- DSI::datashield.aggregate(
    conns,
    call("omopFactorLevelsDS", as.symbol(sym)),
    success = function(server, value) {
      aggregate_success <<- c(aggregate_success, server)
    },
    error = function(server, message) {
      aggregate_errors[[server]] <<- message
    }
  )
  returned <- intersect(expected_servers, names(per_server))
  missing_results <- setdiff(expected_servers, names(per_server))
  null_results <- returned[vapply(per_server[returned], is.null, logical(1))]
  candidates <- setdiff(returned, null_results)
  invalid_results <- candidates[!vapply(per_server[candidates], function(result) {
    is.list(result) && is.list(result$levels) &&
      is.character(result$unsafe) &&
      is.numeric(result$nfilter_levels_max) &&
      length(result$nfilter_levels_max) == 1L &&
      !is.na(result$nfilter_levels_max) &&
      is.finite(result$nfilter_levels_max)
  }, logical(1))]
  failed <- unique(c(
    names(aggregate_errors), missing_results, null_results, invalid_results,
    setdiff(expected_servers, aggregate_success)
  ))
  if (length(failed) > 0L) {
    stop("Concept-factor level collection failed or was incomplete on: ",
         paste(failed, collapse = ", "), ".", call. = FALSE)
  }
  per_server <- per_server[expected_servers]
  spec <- .unionConceptLevels(per_server)
  if (length(spec) == 0L) {
    return(invisible(NULL))
  }
  assign_success <- character(0)
  assign_errors <- character(0)
  DSI::datashield.assign.expr(
    conns,
    symbol = sym,
    expr = call("omopAsFactorColumnsDS", as.symbol(sym), .ds_encode(spec)),
    success = function(server) {
      assign_success <<- c(assign_success, server)
    },
    error = function(server, message) {
      assign_errors[[server]] <<- message
    }
  )
  failed <- unique(c(names(assign_errors),
                     setdiff(expected_servers, assign_success)))
  if (length(failed) > 0L) {
    stop("Concept-factor recoding failed or was incomplete on: ",
         paste(failed, collapse = ", "), ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Merge per-server concept levels into one shared ordered spec
#'
#' Pure reduction at the heart of the coordination layer: given each server's
#' \code{omopFactorLevelsDS} report, it computes, per concept-id column, the
#' union of safe levels in one deterministic order. A column flagged unsafe on
#' \emph{any} server is dropped entirely (left raw everywhere), and a union
#' exceeding the smallest reported server cap is dropped (no server would
#' accept it). Numeric-looking ids sort numerically so the shared coding is
#' intuitive; other labels use a locale-independent radix sort so every client
#' derives the identical ordering.
#'
#' Kept side-effect-free (no DSI calls) so the union semantics are unit
#' testable in isolation.
#'
#' @param per_server List of per-server results, each a list with
#'   \code{levels} (named list of column -> character levels), \code{unsafe}
#'   (character vector of disclosive columns), and \code{nfilter_levels_max}
#'   (numeric server cap). \code{NULL} entries are rejected because a
#'   federation cannot be harmonized from partial results.
#' @return A named list mapping each harmonizable column to its shared,
#'   ordered character levels. Empty list when nothing is harmonizable.
#' @keywords internal
.unionConceptLevels <- function(per_server) {
  if (length(per_server) == 0L) {
    return(list())
  }
  if (any(vapply(per_server, is.null, logical(1)))) {
    stop("Concept-factor level collection is incomplete across servers.",
         call. = FALSE)
  }
  all_cols <- unique(unlist(
    lapply(per_server, function(r) c(names(r$levels), as.character(r$unsafe))),
    use.names = FALSE
  ))
  if (length(all_cols) == 0L) {
    return(list())
  }
  caps <- vapply(per_server,
                 function(r) as.numeric(r$nfilter_levels_max %||% NA_real_),
                 numeric(1))
  cap <- suppressWarnings(min(caps, na.rm = TRUE))
  if (!is.finite(cap)) {
    cap <- 40
  }
  spec <- list()
  for (col in all_cols) {
    # If any server deemed this column disclosive, leave it raw everywhere.
    flagged_unsafe <- any(vapply(
      per_server,
      function(r) col %in% as.character(r$unsafe), logical(1)
    ))
    if (flagged_unsafe) {
      next
    }
    lv <- unique(unlist(
      lapply(per_server, function(r) as.character(r$levels[[col]])),
      use.names = FALSE
    ))
    lv <- lv[!is.na(lv) & nzchar(lv)]
    if (length(lv) == 0L) {
      next
    }
    # Deterministic ordering: numeric-looking ids sort numerically so the
    # shared coding is intuitive; otherwise a locale-independent radix sort.
    if (all(grepl("^-?[0-9]+$", lv))) {
      lv <- lv[order(as.numeric(lv))]
    } else {
      lv <- sort(lv, method = "radix")
    }
    if (length(lv) > cap) {
      next
    }
    spec[[col]] <- lv
  }
  spec
}

.plan_domain_concept_column <- function(table) {
  columns <- c(
    condition_occurrence = "condition_concept_id",
    drug_exposure = "drug_concept_id",
    procedure_occurrence = "procedure_concept_id",
    device_exposure = "device_concept_id",
    measurement = "measurement_concept_id",
    observation = "observation_concept_id",
    visit_occurrence = "visit_concept_id",
    visit_detail = "visit_detail_concept_id",
    specimen = "specimen_concept_id",
    condition_era = "condition_concept_id",
    drug_era = "drug_concept_id",
    dose_era = "drug_concept_id",
    episode = "episode_concept_id",
    death = "cause_concept_id"
  )
  unname(columns[[tolower(table)]])
}

.plan_filter_columns <- function(filter) {
  if (is.null(filter) || !is.list(filter)) return(character(0))
  direct <- if (!is.null(filter$var) && is.character(filter$var)) {
    tolower(filter$var)
  } else {
    character(0)
  }
  nested <- unlist(lapply(filter, .plan_filter_columns), use.names = FALSE)
  unique(c(direct, nested))
}

.plan_feature_columns <- function(specs, table) {
  specs <- specs %||% list()
  types <- vapply(specs, function(spec) {
    tolower(as.character(spec$type %||% "boolean")[[1L]])
  }, character(1))
  value_types <- c(
    "mean_value", "min_value", "max_value", "first_value", "latest_value",
    "sum_value", "sd_value", "cv_value", "slope_value"
  )
  date_types <- c(
    "first_value", "latest_value", "time_since", "slope_value",
    "gap_max_days", "gap_mean_days"
  )
  default_concept <- .plan_domain_concept_column(table)
  concept_columns <- vapply(specs, function(spec) {
    as.character(spec$concept_col %||% default_concept %||% "")[[1L]]
  }, character(1))
  value_columns <- vapply(seq_along(specs), function(i) {
    spec <- specs[[i]]
    if (!is.null(spec$value_column) || types[[i]] %in% value_types) {
      as.character(spec$value_column %||% "value_as_number")[[1L]]
    } else {
      ""
    }
  }, character(1))
  filter_columns <- unlist(lapply(specs, function(spec) {
    .plan_filter_columns(spec$filter)
  }), use.names = FALSE)
  fixed <- character(0)
  if (any(types == "abnormal_high")) {
    fixed <- c(fixed, "value_as_number", "range_high")
  }
  if (any(types == "abnormal_low")) {
    fixed <- c(fixed, "value_as_number", "range_low")
  }
  if (any(types %in% c("drug_duration", "duration_sum"))) {
    stem <- sub("_start_date$", "", .default_omop_date_column(table) %||% "")
    if (nzchar(stem)) fixed <- c(fixed, paste0(stem, "_start_date"),
                                  paste0(stem, "_end_date"))
  }
  has_time_windows <- any(vapply(specs, function(spec) {
    !is.null(spec$time_window)
  }, logical(1)))
  if (any(types %in% date_types) || has_time_windows) {
    fixed <- c(fixed, .default_omop_date_column(table) %||% character(0))
  }
  if (length(specs) == 0L && !is.null(default_concept)) {
    concept_columns <- default_concept
  }
  unique(tolower(c(
    "person_id", concept_columns[nzchar(concept_columns)],
    value_columns[nzchar(value_columns)], filter_columns, fixed
  )))
}

.plan_end_date_column <- function(table) {
  columns <- c(
    observation_period = "observation_period_end_date",
    visit_occurrence = "visit_end_date",
    visit_detail = "visit_detail_end_date",
    condition_occurrence = "condition_end_date",
    drug_exposure = "drug_exposure_end_date",
    device_exposure = "device_exposure_end_date",
    payer_plan_period = "payer_plan_period_end_date",
    drug_era = "drug_era_end_date",
    dose_era = "dose_era_end_date",
    condition_era = "condition_era_end_date",
    episode = "episode_end_date"
  )
  unname(columns[[tolower(table)]])
}

.plan_primary_key_column <- function(table) {
  columns <- c(
    person = "person_id", observation_period = "observation_period_id",
    visit_occurrence = "visit_occurrence_id",
    visit_detail = "visit_detail_id",
    condition_occurrence = "condition_occurrence_id",
    drug_exposure = "drug_exposure_id",
    procedure_occurrence = "procedure_occurrence_id",
    device_exposure = "device_exposure_id", measurement = "measurement_id",
    observation = "observation_id", death = "person_id", note = "note_id",
    specimen = "specimen_id", payer_plan_period = "payer_plan_period_id",
    drug_era = "drug_era_id", dose_era = "dose_era_id",
    condition_era = "condition_era_id", episode = "episode_id"
  )
  unname(columns[[tolower(table)]])
}

#' Build the exact schema and semantic dependency manifest for a plan
#'
#' The manifest is deliberately data-independent. It captures every source
#' table/column used implicitly by filters, cohort materialisation, longitudinal
#' grains, feature reductions, derived variables, vocabulary expansion and
#' output formatting. It is the contract bound by plan harmonization.
#'
#' @param plan An \code{omop_plan}.
#' @return A list with \code{tables}, \code{needs_vocabulary_identity}, and
#'   semantic \code{issues} that cannot be harmonized safely.
#' @keywords internal
.plan_dependency_manifest <- function(plan) {
  dependencies <- list()
  reasons <- list()
  issues <- character(0)
  cohort_required <- character(0)
  filter_materialization <- FALSE
  needs_vocabulary_identity <- FALSE

  add <- function(table, columns, reason) {
    table <- tolower(as.character(table %||% "")[[1L]])
    columns <- unique(tolower(as.character(columns %||% character(0))))
    columns <- columns[!is.na(columns) & nzchar(columns)]
    if (!nzchar(table)) return(invisible(NULL))
    dependencies[[table]] <<- unique(c(dependencies[[table]], columns))
    reasons[[table]] <<- unique(c(reasons[[table]], reason))
    invisible(NULL)
  }
  add_cohort_table <- function(reason) {
    add("cohort", c("subject_id", "cohort_definition_id",
                    "cohort_start_date", "cohort_end_date"), reason)
  }
  add_vocab_translation <- function(reason, dictionary = FALSE) {
    columns <- c("concept_id", "concept_name")
    if (dictionary) {
      columns <- c(columns, "domain_id", "vocabulary_id",
                   "concept_class_id", "standard_concept", "invalid_reason")
    }
    add("concept", columns, reason)
    needs_vocabulary_identity <<- TRUE
  }
  add_concept_set <- function(spec, reason) {
    if (is.null(spec) || !is.list(spec) || is.null(spec$concepts)) {
      return(invisible(NULL))
    }
    if (isTRUE(spec$include_descendants)) {
      add("concept_ancestor",
          c("ancestor_concept_id", "descendant_concept_id",
            "min_levels_of_separation"), reason)
      add("concept", c("concept_id", "concept_name", "domain_id",
                       "vocabulary_id"), reason)
      needs_vocabulary_identity <<- TRUE
    }
    if (isTRUE(spec$include_mapped)) {
      add("concept_relationship",
          c("concept_id_1", "concept_id_2", "relationship_id",
            "invalid_reason"), reason)
      needs_vocabulary_identity <<- TRUE
    }
    invisible(NULL)
  }
  add_window_date <- function(table, window, reason) {
    if (!is.null(window)) {
      add(table, .default_omop_date_column(table), reason)
    }
  }
  add_index_event <- function(spec, reason) {
    if (is.null(spec)) return(invisible(NULL))
    table <- tolower(spec$table %||% "")
    start <- .default_omop_date_column(table)
    end <- .plan_end_date_column(table) %||% start
    cols <- c("person_id", start, end, .plan_primary_key_column(table))
    if (!is.null(spec$concept_set)) {
      cols <- c(cols, .plan_domain_concept_column(table))
      add_concept_set(spec$concept_set, reason)
    }
    add(table, cols, reason)
    invisible(NULL)
  }

  walk_population_filter <- function(node, reason) {
    if (is.null(node) || !is.list(node)) return(invisible(NULL))
    type <- tolower(as.character(node$type %||% "")[[1L]])
    params <- node$params %||% node
    if (nzchar(type)) add("person", "person_id", reason)
    if (type == "sex") add("person", "gender_concept_id", reason)
    if (type %in% c("age_range", "age_group")) {
      add("person", "year_of_birth", reason)
      if (is.null(params$reference_date)) cohort_required <<- c(
        cohort_required, paste0(reason, " (age reference)"))
    }
    if (type == "cohort") add_cohort_table(reason)
    if (type %in% c("has_concept", "not_has_concept", "concept_count",
                    "condition", "drug", "procedure")) {
      table <- params$table %||% switch(type,
        condition = "condition_occurrence", drug = "drug_exposure",
        procedure = "procedure_occurrence", "condition_occurrence")
      add(table, c("person_id", .plan_domain_concept_column(table)), reason)
      add_window_date(table, params$window, reason)
      if (!is.null(params$window) && is.null(params$reference_date)) {
        cohort_required <<- c(cohort_required, paste0(reason, " (window)"))
      }
      add_concept_set(params$concept_set, reason)
    }
    if (type %in% c("prior_observation", "followup")) {
      add("observation_period",
          c("person_id", "observation_period_start_date",
            "observation_period_end_date"), reason)
      if (is.null(params$reference_date)) cohort_required <<- c(
        cohort_required, paste0(reason, " (observation reference)"))
    }
    if (type == "visit_count") {
      add("visit_occurrence", c(
        "person_id",
        if (!is.null(params$visit_concept_id) ||
            !is.null(params$visit_concept_ids)) "visit_concept_id"
      ), reason)
      add_window_date("visit_occurrence", params$window, reason)
      if (!is.null(params$window) && is.null(params$reference_date)) {
        cohort_required <<- c(cohort_required, paste0(reason, " (window)"))
      }
    }
    if (type %in% c("has_measurement", "missing_measurement", "measurement")) {
      numeric_range <- !is.null(params$min_value) || !is.null(params$max_value)
      add("measurement", c(
        "person_id", "measurement_concept_id",
        if (numeric_range || type == "missing_measurement") "value_as_number"
      ), reason)
      add_window_date("measurement", params$window, reason)
      if (!is.null(params$window) && is.null(params$reference_date)) {
        cohort_required <<- c(cohort_required, paste0(reason, " (window)"))
      }
    }
    lapply(node[intersect(names(node), c("and", "or", "filters", "filter_tree",
                                        "criteria", "spec"))],
           function(child) {
             if (is.list(child) && is.null(child$type)) {
               lapply(child, walk_population_filter, reason = reason)
             } else {
               walk_population_filter(child, reason)
             }
           })
    invisible(NULL)
  }

  plan_cohort_source <- FALSE
  if (!is.null(plan$cohort)) {
    plan_cohort_source <- TRUE
    if (identical(tolower(plan$cohort$type %||% ""), "cohort_table") ||
        !is.null(plan$cohort$cohort_definition_id)) {
      add_cohort_table("plan cohort")
    }
    if (!is.null(plan$cohort$filter_tree) || !is.null(plan$cohort$spec)) {
      filter_materialization <- TRUE
      walk_population_filter(plan$cohort$filter_tree %||% plan$cohort$spec,
                             "plan cohort")
    }
  }
  if (!is.null(plan$scope) &&
      (!is.null(plan$scope$cohort) || length(plan$scope$tables %||% list()) > 0L)) {
    plan_cohort_source <- TRUE
    if (is.numeric(plan$scope$cohort)) add_cohort_table("plan scope cohort")
  }

  populations <- plan$populations %||% list()
  population_has_source <- function(id, seen = character(0)) {
    if (id %in% seen) return(FALSE)
    pop <- populations[[id]]
    inherited <- identical(id, "base") && plan_cohort_source
    if (is.null(pop)) return(inherited)
    if (!is.null(pop$cohort_definition_id) || !is.null(pop$filter_tree) ||
        !is.null(pop$index_event)) return(TRUE)
    if (!is.null(pop$setop$members)) {
      members <- as.character(unlist(pop$setop$members, use.names = FALSE))
      return(length(members) > 0L && all(vapply(
        members, population_has_source, logical(1), seen = c(seen, id)
      )))
    }
    inherited
  }
  for (population_name in names(populations)) {
    pop <- populations[[population_name]]
    reason <- paste0("population '", population_name, "'")
    if (!is.null(pop$cohort_definition_id)) add_cohort_table(reason)
    if (!is.null(pop$filter_tree)) {
      filter_materialization <- TRUE
      walk_population_filter(pop$filter_tree, reason)
    }
    add_index_event(pop$index_event, reason)
  }

  derived_dependencies <- function(specs, reason) {
    specs <- specs %||% list()
    kinds <- tolower(vapply(specs, function(spec) {
      as.character(spec$kind %||% "")[[1L]]
    }, character(1)))
    if (length(kinds) == 0L) return(invisible(NULL))
    add("person", "person_id", reason)
    if (any(kinds %in% c("age", "demo_missingness"))) {
      add("person", "year_of_birth", reason)
    }
    if (any(kinds %in% c("sex_mf", "demo_missingness", "chads2",
                         "chadsvasc"))) {
      add("person", "gender_concept_id", reason)
    }
    if (any(kinds %in% c("chads2", "chadsvasc"))) {
      add("person", "year_of_birth", reason)
    }
    if ("demo_missingness" %in% kinds) {
      add("person", c("month_of_birth", "day_of_birth", "race_concept_id",
                       "ethnicity_concept_id"), reason)
    }
    if (any(kinds %in% c("obs_duration", "prior_obs", "followup"))) {
      add("observation_period",
          c("person_id", "observation_period_start_date",
            "observation_period_end_date"), reason)
    }
    index_age <- vapply(specs, function(spec) {
      identical(tolower(spec$kind %||% ""), "age") &&
        identical(tolower(spec$reference %||% "today"), "index") &&
        is.null(spec$reference_date)
    }, logical(1))
    if (any(index_age)) cohort_required <<- c(cohort_required, reason)
    scores <- intersect(kinds, c("charlson", "chads2", "chadsvasc",
                                 "dcsi", "hfrs"))
    if (length(scores) > 0L) {
      add("condition_occurrence",
          c("person_id", "condition_concept_id"), reason)
      hierarchy_scores <- intersect(scores,
                                    c("charlson", "chads2", "chadsvasc"))
      if (length(hierarchy_scores) > 0L) {
        add("concept_ancestor",
            c("ancestor_concept_id", "descendant_concept_id",
              "min_levels_of_separation"), reason)
        add("concept", c("concept_id", "concept_name", "domain_id",
                         "vocabulary_id"), reason)
        needs_vocabulary_identity <<- TRUE
      }
      if (any(scores %in% c("dcsi", "hfrs"))) {
        add("concept", c("concept_id", "concept_code", "vocabulary_id",
                         "standard_concept", "invalid_reason"), reason)
        add("concept_relationship",
            c("concept_id_1", "concept_id_2", "relationship_id",
              "invalid_reason"), reason)
        needs_vocabulary_identity <<- TRUE
      }
    }
    invisible(NULL)
  }

  translate <- isTRUE(plan$options$translate_concepts %||% TRUE)
  for (output_name in names(plan$outputs %||% list())) {
    out <- plan$outputs[[output_name]]
    type <- tolower(out$type %||% "event_level")
    reason <- paste0("output '", output_name, "'")
    population_id <- out$population_id %||% "base"
    has_cohort <- plan_cohort_source || population_has_source(population_id)
    require_cohort <- function(detail = reason) {
      cohort_required <<- c(cohort_required, detail)
      if (!has_cohort) {
        issues <<- c(issues, paste0(detail, " requires a cohort/index source"))
      }
    }

    if (type == "person_level") {
      for (table_name in names(out$tables %||% list())) {
        entry <- out$tables[[table_name]]
        if (is.null(entry)) {
          issues <- c(issues, paste0(
            reason, " requests schema-dependent default columns from table '",
            table_name, "'; list columns explicitly for federated execution"
          ))
          next
        }
        if (is.list(entry) && !is.null(entry$features)) {
          if (length(entry$features) == 0L) {
            issues <- c(issues, paste0(
              reason, " has data-discovered empty feature specs for table '",
              table_name, "'"
            ))
          }
          add(table_name, .plan_feature_columns(entry$features, table_name),
              reason)
          lapply(entry$features, function(spec) {
            add_concept_set(spec$concept_set, reason)
          })
        } else {
          add(table_name, c("person_id", unlist(entry, use.names = FALSE)),
              reason)
          if (translate && (is.null(entry) ||
              any(grepl("_concept_id$", unlist(entry, use.names = FALSE))))) {
            add_vocab_translation(reason)
          }
        }
      }
      derived_dependencies(out$derived_columns, reason)
      if (length(out$tables %||% list()) == 0L &&
          length(out$derived_columns %||% list()) == 0L) {
        issues <- c(issues, paste0(reason, " has no executable source"))
      }
    } else if (type == "event_level") {
      table <- tolower(out$table %||% "")
      format <- tolower(out$representation$format %||% "long")
      columns <- c("person_id", unlist(out$columns %||% character(0),
                                       use.names = FALSE),
                   .plan_filter_columns(out$filters$custom))
      concept_set <- out$filters$concept_set$ids %||% out$concept_set
      if (!is.null(concept_set) || format %in% c("wide", "sparse", "features")) {
        columns <- c(columns, out$filters$concept_col %||% out$concept_col %||%
                       .plan_domain_concept_column(table))
      }
      add_concept_set(concept_set, reason)
      temporal <- out$temporal %||% list()
      if (!is.null(out$filters$time_window) || length(temporal) > 0L) {
        columns <- c(columns,
          out$filters$time_window$date_column %||%
            .default_omop_date_column(table))
      }
      if (!is.null(temporal$min_gap) || !is.null(temporal$event_select)) {
        columns <- c(columns, .plan_primary_key_column(table))
      }
      if (format == "features") {
        specs <- out$representation$features %||% list()
        if (length(specs) == 0L) {
          issues <- c(issues, paste0(
            reason, " uses data-discovered automatic features; provide an ",
            "explicit named feature specification"
          ))
        }
        columns <- c(columns, .plan_feature_columns(specs, table))
        feature_types <- tolower(vapply(specs, function(spec) {
          as.character(spec$type %||% "boolean")[[1L]]
        }, character(1)))
        if (any(feature_types %in% c("first_value", "latest_value"))) {
          columns <- c(columns, .plan_primary_key_column(table))
        }
        lapply(specs, function(spec) add_concept_set(spec$concept_set, reason))
      }
      if (format == "wide") {
        flat <- suppressWarnings(as.integer(unlist(
          if (is.list(concept_set) && !is.null(concept_set$concepts)) {
            concept_set$concepts
          } else concept_set, use.names = FALSE
        )))
        if (length(flat) == 0L || anyNA(flat)) {
          issues <- c(issues, paste0(
            reason, " wide format requires a closed integer concept_set"
          ))
        }
        if (translate) {
          issues <- c(issues, paste0(
            reason, " wide labels depend on vocabulary names; set ",
            "translate_concepts = FALSE for a stable federated schema"
          ))
        }
      }
      grain <- tolower(out$representation$grain %||% "person")
      date_mode <- tolower(if (is.list(out$date_handling)) {
        out$date_handling$mode %||% ""
      } else out$date_handling %||% "")
      if (grain == "episode" || !is.null(temporal$index_window) ||
          date_mode %in% c("relative", "relative_to_index")) {
        require_cohort(reason)
      }
      add(table, columns, reason)
      if (!is.null(out$filters$visit)) {
        add(table, "visit_occurrence_id", reason)
        add("visit_occurrence", c("visit_occurrence_id", "visit_concept_id"),
            reason)
      }
      if (translate && format %in% c("long", "wide")) {
        add_vocab_translation(reason)
      }
    } else if (type == "baseline") {
      require_cohort(reason)
      add("person", c("person_id", unlist(out$columns %||% character(0),
                                           use.names = FALSE),
                      if ("age_at_index" %in% (out$derived %||% character(0)))
                        "year_of_birth"), reason)
      add("observation_period",
          c("person_id", "observation_period_start_date",
            "observation_period_end_date"), reason)
      if (translate) add_vocab_translation(reason)
    } else if (type == "survival") {
      require_cohort(reason)
      outcomes <- out$outcomes %||% list(outcome = out$outcome)
      for (endpoint in outcomes) {
        table <- tolower(endpoint$table %||% "")
        add(table, c(
          "person_id", .plan_domain_concept_column(table),
          .default_omop_date_column(table),
          .plan_filter_columns(endpoint$filters),
          .plan_filter_columns(out$filters$custom)
        ), reason)
        add_concept_set(endpoint$concept_set, reason)
      }
      censoring <- out$censoring %||% if (is.null(out$outcomes)) {
        list(observation_period_end = TRUE, death = FALSE)
      } else {
        list()
      }
      if (!identical(censoring$observation_period_end, FALSE)) {
        add("observation_period", c(
          "person_id", "observation_period_start_date",
          "observation_period_end_date"
        ), reason)
      }
      if (!identical(censoring$death, FALSE)) {
        add("death", c("person_id", "death_date"), reason)
      }
    } else if (type == "concept_dictionary") {
      add_vocab_translation(reason, dictionary = TRUE)
      sources <- out$source_outputs
      if (!is.null(sources) &&
          length(setdiff(sources, names(plan$outputs))) > 0L) {
        issues <- c(issues, paste0(reason, " references missing source output(s)"))
      }
    } else if (type == "cohort_membership") {
      require_cohort(reason)
    } else if (type == "intervals_long") {
      require_cohort(reason)
      for (table in tolower(out$tables %||% character(0))) {
        filter_index <- match(table, tolower(names(out$source_filters)))
        source_filter <- if (is.na(filter_index)) {
          out$filters$custom
        } else {
          out$source_filters[[filter_index]]
        }
        add(table, c("person_id", .default_omop_date_column(table),
                     .plan_end_date_column(table),
                     .plan_primary_key_column(table),
                     if (!is.null(out$concept_filter[[table]]))
                       .plan_domain_concept_column(table),
                     .plan_filter_columns(source_filter)), reason)
        add_concept_set(out$concept_filter[[table]], reason)
      }
    } else if (type %in% c("temporal_covariates", "person_period")) {
      require_cohort(reason)
      table <- tolower(out$table %||% "")
      add(table, c("person_id", .plan_domain_concept_column(table),
                   .default_omop_date_column(table),
                   .plan_filter_columns(out$filters$custom)), reason)
      add_concept_set(out$concept_set, reason)
    }
  }

  if (filter_materialization && length(cohort_required) > 0L) {
    add("observation_period",
        c("person_id", "observation_period_start_date",
          "observation_period_end_date"), "cohort materialization")
  }
  list(
    tables = lapply(dependencies, sort),
    reasons = reasons,
    needs_vocabulary_identity = needs_vocabulary_identity,
    issues = unique(issues),
    cohort_required = unique(cohort_required)
  )
}

.plan_harmonization_signature <- function(plan) {
  copy <- plan
  copy$harmonization <- NULL
  as.character(jsonlite::toJSON(
    .ds_coerce_names(.plan_strip_classes(copy)),
    auto_unbox = TRUE, null = "null",
    digits = NA
  ))
}

.plan_semantic_snapshot <- function(comparison, needs_vocabulary_identity) {
  if (!isTRUE(needs_vocabulary_identity)) return(list())
  servers <- sort(comparison$servers %||% character(0))
  versions <- comparison$semantic_versions %||% list()
  if (length(versions) > 0L) versions <- versions[servers]
  missing <- setdiff(servers, names(versions))
  vocabulary <- vapply(versions, function(entry) {
    value <- entry$vocabulary_version %||% NA_character_
    if (length(value) != 1L) NA_character_ else as.character(value)
  }, character(1))
  invalid <- names(vocabulary)[is.na(vocabulary) | !nzchar(vocabulary)]
  if (length(missing) > 0L || length(invalid) > 0L ||
      length(unique(vocabulary)) != 1L) {
    stop("The plan uses vocabulary-dependent semantics, but connected ",
         "servers do not report one identical non-missing vocabulary ",
         "version. Disable concept translation/expansion or harmonize the ",
         "vocabulary snapshots first.", call. = FALSE)
  }
  versions
}

.plan_schema_snapshot <- function(comparison, requirements,
                                  needs_vocabulary_identity = FALSE) {
  if (is.character(requirements)) {
    requirements <- stats::setNames(rep(list(character(0)),
                                         length(requirements)), requirements)
  }
  requirements <- requirements %||% list()
  names(requirements) <- tolower(names(requirements))
  requirements <- lapply(requirements, function(x) sort(unique(tolower(x))))
  tables <- sort(unique(names(requirements)))
  columns <- comparison$common_columns %||% list()
  types <- comparison$common_column_types %||% list()
  take <- function(values, table, requested) {
    key <- names(values)[tolower(names(values)) == table]
    if (length(key) == 0L) return(NULL)
    value <- values[[key[[1L]]]]
    if (is.null(names(value))) {
      return(sort(intersect(tolower(value), requested)))
    }
    value <- value[tolower(names(value)) %in% requested]
    value <- value[order(tolower(names(value)))]
    names(value) <- tolower(names(value))
    as.list(value)
  }
  list(
    servers = sort(comparison$servers %||% character(0)),
    tables = tables,
    requirements = requirements[tables],
    present = as.list(stats::setNames(
      tables %in% tolower(comparison$common_tables %||% character(0)), tables
    )),
    columns = stats::setNames(lapply(tables, function(x) {
      take(columns, x, requirements[[x]])
    }),
                              tables),
    types = stats::setNames(lapply(tables, function(x) {
      take(types, x, requirements[[x]])
    }), tables),
    semantic_versions = .plan_semantic_snapshot(
      comparison, needs_vocabulary_identity
    )
  )
}

.verify_plan_schema_harmonization <- function(plan, symbol, conns) {
  contract <- plan$harmonization
  if (is.null(contract)) return(invisible(TRUE))
  if (!identical(contract$version, 2L) ||
      !identical(contract$mode, "intersection")) {
    stop("Unsupported or malformed plan harmonization contract; harmonize the ",
         "plan again before execution.", call. = FALSE)
  }
  if (!identical(contract$plan_signature,
                 .plan_harmonization_signature(plan))) {
    stop("The plan changed after schema harmonization; call ",
         "ds.omop.plan.harmonize() again.", call. = FALSE)
  }
  comparison <- ds.omop.compare(
    symbol = symbol, conns = conns,
    tables = names(contract$manifest$tables %||% list())
  )
  if (length(comparison$column_errors %||% character(0)) > 0L) {
    stop("Cannot revalidate the harmonized plan because schema introspection ",
         "is incomplete.", call. = FALSE)
  }
  current <- .plan_schema_snapshot(
    comparison, contract$manifest$tables,
    contract$manifest$needs_vocabulary_identity
  )
  if (!identical(current, contract$schema)) {
    stop("Servers or relevant OMOP schemas changed after plan harmonization; ",
         "call ds.omop.plan.harmonize() again.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Harmonize a plan for multi-server execution
#'
#' Inspects table and column availability across connected servers. In
#' \code{"intersection"} mode the default strict policy rejects a plan whose
#' requested tables, compatible column types, implicit feature dependencies,
#' or output source contracts are not common to every server. With
#' \code{strict = FALSE}, only semantics-preserving column intersections are
#' attempted; an output is removed when its required dependencies are absent.
#' Population/cohort dependencies always fail closed. The returned plan is
#' bound to the compared servers and relevant schema snapshot and is rechecked
#' by validate, preview, and execute. No unsupported strategy or output type is
#' accepted silently.
#'
#' @param plan An \code{omop_plan} object.
#' @param mode Character; harmonization strategy. Only
#'   \code{"intersection"} is currently executable. The former
#'   \code{"union_with_missing"} placeholder is rejected until typed missing
#'   columns can be synthesized consistently on every backend.
#' @param strict Logical; fail when any requested dependency is not common
#'   (default). If false, trim raw optional columns/tables or remove whole
#'   incompatible outputs with explicit warnings; population semantics are
#'   never weakened.
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
                                   strict = TRUE,
                                   symbol = "omop",
                                   conns = NULL) {
  if (!is.character(mode) || length(mode) != 1L || is.na(mode)) {
    stop("mode must be 'intersection'.", call. = FALSE)
  }
  mode <- tolower(mode)
  if (identical(mode, "union_with_missing")) {
    stop("mode = 'union_with_missing' is not implemented: dsOMOPClient will ",
         "not return an unchanged, non-harmonized plan. Use 'intersection'.",
         call. = FALSE)
  }
  mode <- match.arg(mode, "intersection")
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("strict must be TRUE or FALSE.", call. = FALSE)
  }
  allowed_types <- c(
    "person_level", "event_level", "baseline", "survival",
    "concept_dictionary", "cohort_membership", "intervals_long",
    "temporal_covariates", "person_period"
  )
  output_types <- vapply(plan$outputs %||% list(), function(out) {
    tolower(out$type %||% "event_level")
  }, character(1))
  unknown_types <- setdiff(unique(output_types), allowed_types)
  if (length(unknown_types) > 0L) {
    stop("Unsupported plan output type(s): ",
         paste(unknown_types, collapse = ", "), ".", call. = FALSE)
  }
  manifest <- .plan_dependency_manifest(plan)
  if (length(manifest$issues) > 0L) {
    stop("Plan has semantics that cannot be safely harmonized: ",
         paste(manifest$issues, collapse = "; "), ".", call. = FALSE)
  }
  comparison <- ds.omop.compare(
    symbol, conns, tables = names(manifest$tables)
  )
  column_errors <- comparison$column_errors %||% character(0)
  if (length(column_errors) > 0L) {
    stop("Cannot establish a common column contract for table(s): ",
         paste(names(column_errors), collapse = ", "), ". ",
         "Resolve schema introspection errors before harmonizing the plan.",
         call. = FALSE)
  }

  common_tables <- tolower(comparison$common_tables %||% character(0))
  common_columns <- comparison$common_columns %||% list()
  column_diffs <- comparison$column_diffs %||% list()
  column_type_diffs <- comparison$column_type_diffs %||% list()
  common_for <- function(table) {
    key <- names(common_columns)[tolower(names(common_columns)) == table]
    if (length(key) == 0L) return(NULL)
    tolower(common_columns[[key[[1L]]]])
  }
  differs <- function(table) {
    has_entry <- function(values) {
      key <- names(values)[tolower(names(values)) == table]
      length(key) > 0L && length(values[[key[[1L]]]]) > 0L
    }
    has_entry(column_diffs) || has_entry(column_type_diffs)
  }
  incompatible <- character(0)
  contract_tables <- character(0)
  add_issue <- function(label) {
    incompatible <<- c(incompatible, label)
  }
  require_table <- function(table, label) {
    table <- tolower(table %||% "")
    if (!nzchar(table)) {
      add_issue(paste0(label, " has no source table"))
      return(FALSE)
    }
    contract_tables <<- c(contract_tables, table)
    if (!table %in% common_tables) {
      add_issue(paste0(label, " table '", table, "'"))
      return(FALSE)
    }
    if (length(comparison$servers %||% character(0)) >= 2L &&
        is.null(common_for(table))) {
      add_issue(paste0(label, " table '", table,
                       "' has no complete column contract"))
      return(FALSE)
    }
    TRUE
  }
  sources <- function(spec) {
    tolower(as.character(unlist(spec, use.names = FALSE)))
  }
  trim_spec <- function(spec, allowed) {
    spec[sources(spec) %in% allowed]
  }
  missing_columns <- function(table, requested, label) {
    requested <- unique(sources(requested))
    if (length(requested) == 0L) return(character(0))
    common <- common_for(table)
    if (is.null(common)) {
      add_issue(paste0(label, " has no complete column contract"))
      return(requested)
    }
    missing <- setdiff(requested, common)
    if (length(missing) > 0L) {
      add_issue(paste0(label, " column(s) ",
                       paste(missing, collapse = ", ")))
    }
    missing
  }

  # Cohort/population criteria define who is analysed. Never drop or rewrite
  # them in permissive mode: absent source tables must stop harmonization.
  scope_tables <- character(0)
  walk_scope <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    type <- tolower(as.character(x$type %||% "")[[1L]])
    params <- x$params %||% list()
    table <- params$table %||% x$table
    mapped <- switch(type,
      sex =, age_range =, age_group = "person",
      prior_observation =, followup = "observation_period",
      visit_count = "visit_occurrence",
      has_measurement =, missing_measurement = "measurement",
      has_concept =, not_has_concept =, concept_count =
        table %||% "condition_occurrence",
      condition = "condition_occurrence",
      drug = "drug_exposure",
      procedure = "procedure_occurrence",
      measurement = "measurement",
      NULL
    )
    if (!is.null(mapped)) scope_tables <<- c(scope_tables, mapped)
    if (!is.null(x$index_event$table)) {
      scope_tables <<- c(scope_tables, x$index_event$table)
    }
    lapply(x, walk_scope)
    invisible(NULL)
  }
  walk_scope(plan$cohort)
  walk_scope(plan$populations)
  scope_tables <- unique(tolower(scope_tables))
  scope_failures <- scope_tables[!vapply(scope_tables, function(table) {
    require_table(table, "cohort/population dependency")
  }, logical(1))]
  if (length(scope_failures) > 0L) {
    stop("Plan population semantics are not executable on every server: ",
         paste(unique(incompatible), collapse = "; "), ".", call. = FALSE)
  }
  incompatible <- character(0)

  for (out_name in names(plan$outputs)) {
    out <- plan$outputs[[out_name]]
    out_type <- tolower(out$type %||% "event_level")
    if (identical(out_type, "person_level") && !is.null(out$tables)) {
      kept <- list()
      for (table_name in names(out$tables)) {
        table <- tolower(table_name)
        label <- paste0("output '", out_name, "'")
        if (!require_table(table, label)) {
          next
        }
        entry <- out$tables[[table_name]]
        if (is.list(entry) && !is.null(entry$features)) {
          required <- .plan_feature_columns(entry$features, table)
          missing <- missing_columns(table, required, label)
          if (length(missing) == 0L) kept[[table_name]] <- entry
        } else {
          missing <- missing_columns(table, entry, label)
          retained <- if (strict) entry else trim_spec(entry, common_for(table))
          if (length(retained) > 0L) kept[[table_name]] <- retained
        }
      }
      if (!strict) {
        if (length(kept) == 0L) plan$outputs[[out_name]] <- NULL else
          plan$outputs[[out_name]]$tables <- kept
      }
      next
    }

    if (identical(out_type, "event_level")) {
      table <- tolower(out$table %||% "")
      label <- paste0("output '", out_name, "'")
      if (!require_table(table, label)) {
        if (!strict) plan$outputs[[out_name]] <- NULL
        next
      }
      common <- common_for(table)
      format <- tolower(out$representation$format %||% "long")
      required <- c("person_id", .plan_filter_columns(out$filters$custom))
      if (!is.null(out$concept_set) || !is.null(out$filters$concept_set) ||
          format %in% c("wide", "sparse")) {
        required <- c(required, out$filters$concept_col %||%
                        .plan_domain_concept_column(table))
      }
      if (!is.null(out$filters$time_window) || !is.null(out$temporal)) {
        required <- c(required,
          out$filters$time_window$date_column %||%
            .default_omop_date_column(table))
      }
      auxiliary_missing <- FALSE
      if (!is.null(out$filters$visit)) {
        required <- c(required, "visit_occurrence_id")
        if (!require_table("visit_occurrence", label)) {
          auxiliary_missing <- TRUE
        } else if (length(missing_columns(
          "visit_occurrence",
          c("visit_occurrence_id", "visit_concept_id"), label
        )) > 0L) {
          auxiliary_missing <- TRUE
        }
      }
      if (identical(format, "features")) {
        required <- c(required, .plan_feature_columns(
          out$representation$features, table
        ))
      }
      required <- unique(required[!is.na(required) & nzchar(required)])
      required_missing <- missing_columns(table, required, label)
      requested <- out$columns
      requested_missing <- if (is.null(requested)) character(0) else
        missing_columns(table, requested, label)
      requires_identical_defaults <- is.null(requested) &&
        format %in% c("long", "wide")
      if (requires_identical_defaults && differs(table)) {
        add_issue(paste0(label, " requests schema-dependent default columns ",
                         "from table '", table, "'"))
        if (!strict) plan$outputs[[out_name]]$columns <- common
      } else if (!strict && length(requested_missing) > 0L) {
        plan$outputs[[out_name]]$columns <- trim_spec(requested, common)
      }
      if (!strict && (length(required_missing) > 0L || auxiliary_missing)) {
        plan$outputs[[out_name]] <- NULL
      }
      next
    }

    label <- paste0("output '", out_name, "'")
    required_tables <- character(0)
    required_by_table <- list()
    if (identical(out_type, "baseline")) {
      required_tables <- "person"
      required_by_table$person <- unique(c(
        "person_id", sources(out$columns %||% character(0)),
        if ("age_at_index" %in% (out$derived %||% character(0)))
          "year_of_birth" else character(0)
      ))
      if (any((out$derived %||% character(0)) %in%
              c("prior_observation", "future_observation"))) {
        required_tables <- c(required_tables, "observation_period")
        required_by_table$observation_period <- c(
          "person_id", "observation_period_start_date",
          "observation_period_end_date"
        )
      }
    } else if (identical(out_type, "survival")) {
      outcomes <- out$outcomes %||% list(outcome = out$outcome)
      for (endpoint in outcomes) {
        table <- tolower(endpoint$table %||% "")
        if (!nzchar(table)) next
        required_tables <- unique(c(required_tables, table))
        required_by_table[[table]] <- unique(c(
          required_by_table[[table]], "person_id",
          .plan_domain_concept_column(table),
          .default_omop_date_column(table),
          .plan_filter_columns(endpoint$filters),
          .plan_filter_columns(out$filters$custom)
        ))
      }
      censoring <- out$censoring %||% if (is.null(out$outcomes)) {
        list(observation_period_end = TRUE, death = FALSE)
      } else {
        list()
      }
      if (!identical(censoring$observation_period_end, FALSE)) {
        required_tables <- unique(c(required_tables, "observation_period"))
        required_by_table$observation_period <- c(
          "person_id", "observation_period_start_date",
          "observation_period_end_date"
        )
      }
      if (!identical(censoring$death, FALSE)) {
        required_tables <- unique(c(required_tables, "death"))
        required_by_table$death <- c("person_id", "death_date")
      }
    } else if (identical(out_type, "concept_dictionary")) {
      required_tables <- "concept"
      required_by_table$concept <- c(
        "concept_id", "concept_name", "domain_id", "vocabulary_id",
        "concept_class_id", "standard_concept", "invalid_reason"
      )
    } else if (identical(out_type, "intervals_long")) {
      required_tables <- tolower(out$tables %||% character(0))
      for (table in required_tables) {
        start <- .default_omop_date_column(table)
        end <- if (!is.null(start)) sub("_start_date$", "_end_date", start)
          else NULL
        filter_index <- match(table, tolower(names(out$source_filters)))
        source_filter <- if (is.na(filter_index)) {
          out$filters$custom
        } else {
          out$source_filters[[filter_index]]
        }
        required_by_table[[table]] <- unique(c(
          "person_id", start, end,
          if (!is.null(out$concept_filter[[table]]))
            .plan_domain_concept_column(table) else character(0),
          .plan_filter_columns(source_filter)
        ))
      }
    } else if (out_type %in% c("temporal_covariates", "person_period")) {
      required_tables <- tolower(out$table %||% "")
      if (nzchar(required_tables[[1L]])) {
        required_by_table[[required_tables[[1L]]]] <- c(
          "person_id", .plan_domain_concept_column(required_tables[[1L]]),
          .default_omop_date_column(required_tables[[1L]])
        )
      }
    }

    failed_tables <- character(0)
    for (table in required_tables) {
      if (!require_table(table, label)) {
        failed_tables <- c(failed_tables, table)
        next
      }
      missing <- missing_columns(
        table, required_by_table[[table]] %||% character(0), label
      )
      if (length(missing) > 0L) failed_tables <- c(failed_tables, table)
    }
    if (!strict && length(failed_tables) > 0L) {
      if (identical(out_type, "intervals_long")) {
        kept_tables <- setdiff(required_tables, failed_tables)
        if (length(kept_tables) == 0L) {
          plan$outputs[[out_name]] <- NULL
        } else {
          plan$outputs[[out_name]]$tables <- kept_tables
          if (!is.null(out$concept_filter)) {
            plan$outputs[[out_name]]$concept_filter <-
              out$concept_filter[intersect(names(out$concept_filter),
                                           kept_tables)]
          }
        }
      } else {
        plan$outputs[[out_name]] <- NULL
      }
    }
  }

  incompatible <- unique(incompatible)
  if (length(incompatible) > 0L) {
    message <- paste(incompatible, collapse = "; ")
    if (strict) {
      stop("Plan is not executable with one common schema: ", message, ".",
           call. = FALSE)
    }
    warning("Intersection harmonization removed incompatible inputs: ",
            message, ".", call. = FALSE)
  }

  if (length(plan$outputs %||% list()) == 0L) {
    stop("Harmonization removed every output; no common executable plan ",
         "remains.", call. = FALSE)
  }

  # Rebuild after non-strict trimming, then validate the exact implicit and
  # explicit dependency manifest. Dependencies are never silently weakened.
  manifest <- .plan_dependency_manifest(plan)
  if (length(manifest$issues) > 0L) {
    stop("Harmonized plan still has non-portable semantics: ",
         paste(manifest$issues, collapse = "; "), ".", call. = FALSE)
  }
  manifest_failures <- character(0)
  for (table in names(manifest$tables)) {
    if (!table %in% common_tables) {
      manifest_failures <- c(
        manifest_failures, paste0("table '", table, "' is not common")
      )
      next
    }
    common <- common_for(table)
    if (is.null(common)) {
      manifest_failures <- c(
        manifest_failures, paste0("table '", table,
                                  "' has no complete column contract")
      )
      next
    }
    missing <- setdiff(manifest$tables[[table]], common)
    if (length(missing) > 0L) {
      manifest_failures <- c(manifest_failures, paste0(
        "table '", table, "' lacks compatible column(s) ",
        paste(missing, collapse = ", ")
      ))
    }
  }
  if (length(manifest_failures) > 0L) {
    stop("Plan dependency manifest is not executable on every server: ",
         paste(unique(manifest_failures), collapse = "; "), ".",
         call. = FALSE)
  }

  contract_tables <- intersect(unique(c(
    tolower(contract_tables), names(manifest$tables)
  )), common_tables)
  manifest$tables <- manifest$tables[intersect(names(manifest$tables),
                                               contract_tables)]
  schema <- .plan_schema_snapshot(
    comparison, manifest$tables, manifest$needs_vocabulary_identity
  )
  plan$harmonization <- list(
    version = 2L,
    mode = "intersection",
    strict = strict,
    manifest = manifest,
    schema = schema,
    plan_signature = NULL
  )
  plan$harmonization$plan_signature <- .plan_harmonization_signature(plan)

  plan
}

# --- Plan save / load (YAML + JSON) -------------------------------------------

#' Strip S3 classes from a plan for clean serialization
#'
#' Recursively drops every S3 class (e.g. \code{omop_plan},
#' \code{omop_feature_spec}, \code{omop_temporal_spec}) so the structure
#' serializes to plain JSON/YAML mappings and arrays. The data itself is
#' untouched. Mirrors \code{.recipe_strip_classes}.
#'
#' @param x Any object.
#' @return \code{x} with nested lists reduced to plain lists.
#' @keywords internal
.plan_strip_classes <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, .plan_strip_classes)
    class(x) <- "list"
  }
  x
}

#' Portable plain-list representation of a plan
#'
#' Produces the version-tagged, class-free list that
#' \code{\link{ds.omop.plan.save}} serializes. Preserves every field an
#' \code{omop_plan} carries (\code{cohort}, \code{anchor}, \code{outputs}
#' with their nested \code{filters$custom} and/or trees, \code{concept_set},
#' \code{time_window}, representation \code{format}s, and \code{options}).
#'
#' @param plan An \code{omop_plan} object.
#' @return A plain list with a \code{version} tag and the plan fields.
#' @keywords internal
.plan_plain <- function(plan) {
  if (!inherits(plan, "omop_plan"))
    stop("plan must be an omop_plan object", call. = FALSE)
  fields <- .ds_coerce_names(.plan_strip_classes(unclass(plan)))
  c(list(version = "1.1"), fields)
}

#' Integer field names that must survive a plan round-trip
#'
#' JSON/YAML parsing loses the integer/double distinction and turns short
#' atomic vectors into lists of scalars. These are the plan fields the
#' builders store as integers (concept ids, cohort ids, offsets, bin
#' geometry); \code{.plan_restore} coerces them back so a save/load/execute
#' round-trip sends the server the identical payload.
#' @keywords internal
.plan_int_fields <- c(
  "concept_set", "ids", "cohort_definition_id", "start_offset", "end_offset",
  "offset", "bin_width", "window_start", "window_end", "min_count",
  "min_days", "days", "visit_concept_id", "index_window", "calendar",
  "age_breaks", "version"
)

#' Restore atomic vectors and integer types in a parsed plan
#'
#' \code{jsonlite::fromJSON(simplifyVector = FALSE)} (and \code{yaml.load})
#' turn atomic vectors into unnamed lists of scalars and read every number as
#' a double. This walks the parsed structure, collapses unnamed all-scalar
#' lists back to atomic vectors, and coerces the known integer fields
#' (\code{.plan_int_fields}) to integer, so the reconstructed plan re-encodes
#' to byte-identical transport JSON. Mirrors \code{.recipe_restore_params}.
#'
#' @param x A parsed plan substructure.
#' @param key Character; the name this node was stored under (drives integer
#'   coercion). \code{NULL} at the top level.
#' @return The normalized substructure.
#' @keywords internal
.plan_restore <- function(x, key = NULL) {
  # Unnamed list of atomic scalars -> atomic vector.
  if (is.list(x) && length(x) > 0 && is.null(names(x)) &&
      all(vapply(x, function(e) is.atomic(e) && length(e) == 1L,
                 logical(1)))) {
    x <- unlist(x, use.names = FALSE)
  }
  if (is.list(x)) {
    nms <- names(x)
    x <- lapply(seq_along(x), function(i) {
      .plan_restore(x[[i]], key = if (!is.null(nms)) nms[[i]] else NULL)
    })
    names(x) <- nms
    return(x)
  }
  if (!is.null(key) && key %in% .plan_int_fields &&
      is.numeric(x) && !is.integer(x)) {
    if (any(!is.finite(x)) || any(x != trunc(x)) ||
        any(x < -.Machine$integer.max | x > .Machine$integer.max)) {
      stop("Plan field '", key,
           "' must contain exact integers; refusing to truncate imported data.",
           call. = FALSE)
    }
    x <- as.integer(x)
  }
  x
}

#' Reconstruct an omop_plan from its plain representation
#'
#' Inverse of \code{\link{.plan_plain}}: applies \code{\link{.plan_restore}}
#' to recover atomic vectors and integer types, then re-stamps the
#' \code{omop_plan} class so \code{\link{ds.omop.plan.execute}} accepts the
#' result unchanged.
#'
#' @param data A parsed plain plan (from JSON or YAML).
#' @return An \code{omop_plan} object.
#' @keywords internal
.plan_from_plain <- function(data) {
  plan <- .plan_restore(data)
  plan$version <- NULL
  plan$anchor <- plan$anchor %||%
    list(table = "person", id_col = "person_id")
  plan$outputs <- plan$outputs %||% list()
  plan$options <- plan$options %||% list(
    translate_concepts = TRUE,
    block_sensitive = TRUE,
    factor_concepts = TRUE
  )
  class(plan) <- c("omop_plan", "list")
  plan
}

#' Resolve a plan file's serialization format
#'
#' Picks \code{"json"} or \code{"yaml"} from an explicit \code{format} or the
#' file extension (\code{.json} / \code{.yml} / \code{.yaml}). Mirrors
#' \code{.recipe_file_format}.
#'
#' @param file Character; the file path.
#' @param format Character or \code{NULL}; an explicit format override.
#' @return \code{"json"} or \code{"yaml"}.
#' @keywords internal
.plan_file_format <- function(file, format = NULL) {
  fmt <- format
  if (is.null(fmt)) {
    ext <- tolower(tools::file_ext(file))
    fmt <- switch(ext, json = "json", yml = "yaml", yaml = "yaml", NULL)
  } else {
    fmt <- tolower(fmt)
    if (length(fmt) == 1L && fmt == "yml") fmt <- "yaml"
  }
  if (is.null(fmt) || length(fmt) != 1L || !fmt %in% c("json", "yaml")) {
    stop("Plan format must be 'json' or 'yaml', or file must end in ",
         ".json, .yml, or .yaml.", call. = FALSE)
  }
  fmt
}

#' Save an extraction plan to JSON or YAML
#'
#' Serializes an \code{omop_plan} to a file so it can be version-controlled,
#' shared, and re-run later. The on-disk format is a faithful, class-free copy
#' of the plan (all outputs, nested \code{filters$custom} and/or trees,
#' \code{concept_set}s, \code{time_window}s, cohort, and representation
#' formats), tagged with a schema version. The format is chosen from the file
#' extension unless given explicitly: \code{.json} uses \pkg{jsonlite};
#' \code{.yaml}/\code{.yml} uses \pkg{yaml}. A plan reloaded with
#' \code{\link{ds.omop.plan.load}} executes identically to the original
#' (the round-trip is lossless with respect to what is sent to the server).
#'
#' @param plan An \code{omop_plan} object.
#' @param file Character; destination path ending in \code{.json},
#'   \code{.yml}, or \code{.yaml}.
#' @param format Character or \code{NULL}; optional explicit format
#'   (\code{"json"} or \code{"yaml"}) overriding the extension.
#' @return The file path, invisibly.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#' plan <- ds.omop.plan.events(plan, "conditions",
#'   "condition_occurrence", concept_set = c(201826))
#'
#' ds.omop.plan.save(plan, "extraction.json")
#' ds.omop.plan.save(plan, "extraction.yaml")
#' }
#' @seealso \code{\link{ds.omop.plan.load}}, \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.save <- function(plan, file, format = NULL) {
  if (!inherits(plan, "omop_plan"))
    stop("plan must be an omop_plan object", call. = FALSE)
  if (missing(file) || length(file) != 1L || !nzchar(file)) {
    stop("file must be a single non-empty path.", call. = FALSE)
  }
  fmt <- .plan_file_format(file, format)
  plain <- .plan_plain(plan)
  if (fmt == "json") {
    json <- jsonlite::toJSON(plain, auto_unbox = TRUE, pretty = TRUE,
                             null = "null")
    writeLines(as.character(json), file)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE))
      stop("Package 'yaml' is required for YAML plans. Install it with ",
           "install.packages(\"yaml\").", call. = FALSE)
    writeLines(yaml::as.yaml(plain), file)
  }
  invisible(file)
}

#' Load an extraction plan from JSON or YAML
#'
#' Reconstructs an \code{omop_plan} previously written by
#' \code{\link{ds.omop.plan.save}}. The parser is selected from the file
#' extension (\code{.json} via \pkg{jsonlite}; \code{.yaml}/\code{.yml} via
#' \pkg{yaml}). Atomic vectors and integer concept/offset fields are restored
#' so the returned plan is accepted unchanged by
#' \code{\link{ds.omop.plan.execute}} and produces an identical execution.
#'
#' @param file Character; source path ending in \code{.json}, \code{.yml}, or
#'   \code{.yaml}.
#' @return An \code{omop_plan} object.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan.load("extraction.json")
#' ds.omop.plan.execute(plan, out = "D")
#' }
#' @seealso \code{\link{ds.omop.plan.save}}, \code{\link{ds.omop.plan.execute}}
#' @export
ds.omop.plan.load <- function(file) {
  if (missing(file) || length(file) != 1L || !nzchar(file)) {
    stop("file must be a single non-empty path.", call. = FALSE)
  }
  if (!file.exists(file)) {
    stop("Plan file not found: ", file, call. = FALSE)
  }
  fmt <- .plan_file_format(file)
  if (fmt == "json") {
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    data <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  } else {
    if (!requireNamespace("yaml", quietly = TRUE))
      stop("Package 'yaml' is required for YAML plans. Install it with ",
           "install.packages(\"yaml\").", call. = FALSE)
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    data <- .yaml_load_safe(text)
  }
  .plan_from_plain(data)
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
      tar_end <- out$tar$end_offset %||% "cohort_end"
      if (is.null(out$outcomes)) {
        n_concepts <- length(out$outcome$concept_set %||% integer(0))
        cat("  [survival] ", name, ": ",
            out$outcome$table %||% "?", " (",
            n_concepts, " concepts), TAR 0-", tar_end, " days\n")
      } else {
        n_concepts <- sum(vapply(out$outcomes, function(endpoint) {
          length(endpoint$concept_set %||% integer(0))
        }, integer(1)))
        cat("  [survival:", out$format %||% "survival", "] ", name, ": ",
            length(out$outcomes), " endpoints (", n_concepts,
            " concepts), TAR ", out$tar$start_offset %||% 0L, "-",
            tar_end, " days\n")
      }
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
    } else if (otype %in% c("temporal_covariates", "person_period")) {
      bw <- out$bin_width %||% 30L
      ws <- out$window_start %||% -365L
      we <- out$window_end %||% 0L
      label <- if (identical(otype, "person_period")) "panel" else "temporal"
      cat("  [", label, "] ", name, ": ", out$table,
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
      x$options$translate_concepts %||% TRUE,
      " block_sensitive=",
      x$options$block_sensitive %||% TRUE,
      " factor_concepts=",
      x$options$factor_concepts %||% TRUE, "\n")
  invisible(x)
}

# Internal: one-line, human-readable label for a single plan output. Shared by
# summary.omop_plan and plot.omop_plan so the two stay consistent.
.plan_output_label <- function(out, name) {
  otype <- out$type %||% "event_level"
  detail <- switch(otype,
    person_level = paste0(length(out$tables %||% character(0)), " tables"),
    baseline = paste0(length(out$columns %||% character(0)), " cols, ",
                      length(out$derived %||% character(0)), " derived"),
    survival = if (is.null(out$outcomes)) {
      paste0(out$outcome$table %||% "?", ", ",
             length(out$outcome$concept_set %||% integer(0)), " concepts")
    } else {
      paste0(length(out$outcomes), " endpoints, ",
             out$format %||% "survival")
    },
    concept_dictionary = paste0("from ",
                      paste(out$source_outputs %||% "all", collapse = ", ")),
    cohort_membership = "OHDSI cohort format",
    intervals_long = paste0(length(out$tables %||% character(0)), " tables"),
    temporal_covariates = paste0(out$table %||% "?", " bins=",
                      out$bin_width %||% 30L, "d"),
    person_period = paste0(out$table %||% "?", " bins=",
                      out$bin_width %||% 30L, "d, episode@index"),
    {
      n_concepts <- length(out$filters$concept_set$ids %||% out$concept_set)
      paste0(out$table %||% "?",
             if (n_concepts > 0) paste0(" (", n_concepts, " concepts)") else "")
    }
  )
  list(type = otype, name = name, detail = detail)
}

# Internal: short label for the cohort node, used by summary and plot.
.plan_cohort_label <- function(x) {
  if (is.null(x$cohort)) return("all persons")
  if (!is.null(x$cohort$cohort_definition_id))
    paste("cohort ID", x$cohort$cohort_definition_id)
  else "custom cohort spec"
}

#' Summarise an extraction plan
#'
#' Produces a compact, human-readable overview of an \code{omop_plan}: the
#' cohort it targets, one row per configured output (type, name, and key
#' parameters), and the plan-wide disclosure options. This is the headless
#' equivalent of inspecting a plan interactively.
#'
#' @param object An \code{omop_plan} object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly, a data frame with one row per output (columns
#'   \code{type}, \code{name}, \code{detail}); printed as a formatted summary
#'   as a side effect.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#' summary(plan)
#' }
#' @export
#' @method summary omop_plan
summary.omop_plan <- function(object, ...) {
  labels <- lapply(names(object$outputs), function(nm)
    .plan_output_label(object$outputs[[nm]], nm))
  df <- if (length(labels) == 0) {
    data.frame(type = character(0), name = character(0),
               detail = character(0), stringsAsFactors = FALSE)
  } else {
    data.frame(
      type = vapply(labels, function(l) l$type, character(1)),
      name = vapply(labels, function(l) l$name, character(1)),
      detail = vapply(labels, function(l) l$detail, character(1)),
      stringsAsFactors = FALSE
    )
  }

  cat("dsOMOP Extraction Plan\n")
  cat("  Cohort : ", .plan_cohort_label(object), "\n", sep = "")
  cat("  Outputs: ", nrow(df), "\n", sep = "")
  for (i in seq_len(nrow(df))) {
    cat(sprintf("    - [%s] %s: %s\n", df$type[i], df$name[i], df$detail[i]))
  }
  cat("  Options: translate=", object$options$translate_concepts %||% TRUE,
      " block_sensitive=", object$options$block_sensitive %||% TRUE,
      " factor_concepts=", object$options$factor_concepts %||% TRUE, "\n",
      sep = "")
  invisible(df)
}

#' Plot an extraction plan as a dependency graph
#'
#' Renders the structure of an \code{omop_plan} as a small directed graph
#' linking the cohort node to each output node. By default the graph is emitted
#' as Graphviz DOT text (printed via \code{cat}) which can be piped to
#' \code{dot}, pasted into any Graphviz viewer, or rendered with the
#' \pkg{DiagrammeR} package. A base-graphics fallback (\code{engine = "base"})
#' draws a simple cohort-to-outputs diagram using only base \pkg{graphics},
#' requiring no additional packages. This replaces the interactive plan DAG.
#'
#' @param x An \code{omop_plan} object.
#' @param engine Character; \code{"dot"} (default) to emit Graphviz DOT text,
#'   or \code{"base"} to draw a base-graphics diagram.
#' @param ... Additional arguments (ignored).
#' @return Invisibly: the DOT string when \code{engine = "dot"}, otherwise
#'   \code{x}. Output is produced as a side effect.
#' @examples
#' \dontrun{
#' plan <- ds.omop.plan()
#' plan <- ds.omop.plan.baseline(plan)
#' plot(plan)                 # Graphviz DOT text
#' plot(plan, engine = "base")
#' }
#' @export
#' @method plot omop_plan
plot.omop_plan <- function(x, engine = c("dot", "base"), ...) {
  engine <- match.arg(engine)
  cohort_lbl <- .plan_cohort_label(x)
  out_labels <- lapply(names(x$outputs), function(nm)
    .plan_output_label(x$outputs[[nm]], nm))

  esc <- function(s) gsub("\"", "'", s, fixed = TRUE)

  if (engine == "dot") {
    lines <- c(
      "digraph omop_plan {",
      "  rankdir=LR;",
      "  node [shape=box, style=rounded];",
      sprintf("  cohort [label=\"Cohort\\n%s\", style=\"rounded,filled\", fillcolor=\"#d6eaf8\"];",
              esc(cohort_lbl))
    )
    if (length(out_labels) == 0) {
      lines <- c(lines,
        "  empty [label=\"(no outputs)\", style=\"rounded,dashed\"];",
        "  cohort -> empty;")
    } else {
      for (i in seq_along(out_labels)) {
        l <- out_labels[[i]]
        nid <- paste0("out", i)
        lines <- c(lines,
          sprintf("  %s [label=\"[%s]\\n%s\\n%s\"];",
                  nid, esc(l$type), esc(l$name), esc(l$detail)),
          sprintf("  cohort -> %s;", nid))
      }
    }
    lines <- c(lines, "}")
    dot <- paste(lines, collapse = "\n")
    cat(dot, "\n", sep = "")
    return(invisible(dot))
  }

  # Base-graphics fallback: cohort box on the left, outputs stacked on the
  # right, arrows from cohort to each output. No external dependency.
  n <- length(out_labels)
  op <- graphics::par(mar = c(0, 0, 1, 0))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 10), ylim = c(0, max(1, n) + 1))
  graphics::title(main = "dsOMOP Extraction Plan")

  cy <- (max(1, n) + 1) / 2
  draw_box <- function(xc, yc, txt, fill) {
    w <- 2.6; h <- 0.6
    graphics::rect(xc - w, yc - h, xc + w, yc + h, col = fill, border = "grey40")
    graphics::text(xc, yc, txt, cex = 0.8)
  }
  draw_box(2, cy, paste0("Cohort\n", cohort_lbl), "#d6eaf8")

  if (n == 0) {
    draw_box(8, cy, "(no outputs)", "white")
    graphics::arrows(4.6, cy, 5.4, cy, length = 0.1)
  } else {
    ys <- rev(seq_len(n))
    for (i in seq_len(n)) {
      l <- out_labels[[i]]
      draw_box(8, ys[i], paste0("[", l$type, "] ", l$name, "\n", l$detail),
               "#fef9e7")
      graphics::arrows(4.6, cy, 5.4, ys[i], length = 0.1)
    }
  }
  invisible(x)
}
