# Module: Sticky privacy releases
# Client construction, federation preflight, and pooling for the dedicated
# server-owned sticky-noise API.  The client deliberately has no seed, nonce,
# epsilon, epoch, reset, or force controls.

.DP_PRIVACY_GUARANTEE <- paste0(
  "sticky_person_bounded_noise_with_authenticated_lineage_",
  "and_nominal_accounting"
)

.dp_normalize_legacy_attestation <- function(value) {
  if (!is.list(value)) return(value)
  if (identical(
    value$privacy_guarantee,
    "sticky_noise_not_formally_certified_dp"
  )) {
    value$privacy_guarantee <- .DP_PRIVACY_GUARANTEE
  }
  value[c(
    "formal_dp", "sampler_certified", "epsilon_semantics", "delta_semantics",
    "bounded_composition"
  )] <- NULL
  value
}

.dp_scalar_character <- function(value, name, nullable = FALSE) {
  if (is.null(value) && nullable) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop(name, " must be one non-empty character value.", call. = FALSE)
  }
  enc2utf8(value)
}

.dp_scalar_number <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value)) {
    stop(name, " must be one finite number.", call. = FALSE)
  }
  as.numeric(value)
}

.dp_choice <- function(value, choices, name) {
  value <- .dp_scalar_character(value, name)
  if (!value %in% choices) {
    stop(name, " must be one of: ", paste(choices, collapse = ", "), ".",
         call. = FALSE)
  }
  value
}

.dp_assert_unused <- function(values, statistic) {
  used <- names(values)[!vapply(values, is.null, logical(1L))]
  if (length(used) > 0L) {
    stop("`", paste(used, collapse = "`, `"), "` ",
         if (length(used) == 1L) "is" else "are",
         " not valid for statistic='", statistic, "'.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_variable <- function(variable) {
  variable <- .dp_scalar_character(variable, "variable")
  if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", variable) ||
      grepl("^\\.[0-9]", variable)) {
    stop("variable must be one bare column name.", call. = FALSE)
  }
  variable
}

.dp_public_values <- function(value, name) {
  if (is.object(value) || !is.atomic(value) || length(value) < 1L ||
      anyNA(value) || !typeof(value) %in% c("logical", "integer", "double",
                                            "character") ||
      (is.numeric(value) && any(!is.finite(value)))) {
    stop(name, " must contain non-missing logical, finite numeric, or ",
         "character values.", call. = FALSE)
  }
  value <- enc2utf8(as.character(unname(value)))
  if (any(!nzchar(value)) || any(nchar(value, type = "bytes") > 256L)) {
    stop(name, " values must be non-empty and at most 256 bytes.",
         call. = FALSE)
  }
  sort(unique(value), method = "radix")
}

.dp_order_by <- function(order_by) {
  if (is.null(order_by)) return(NULL)
  .dp_variable(order_by)
}

.dp_population_id <- function(population_id, nullable = TRUE) {
  if (is.null(population_id) && isTRUE(nullable)) return(NULL)
  population_id <- .dp_scalar_character(population_id, "population_id")
  if (nchar(population_id, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+/-]*$", population_id)) {
    stop("population_id must be at most 256 characters and use only letters, ",
         "numbers, '.', '_', ':', '@', '+', '/', or '-'; it must start with ",
         "a letter or number.", call. = FALSE)
  }
  population_id
}

.dp_histogram_breaks <- function(breaks) {
  if (is.numeric(breaks)) {
    if (length(breaks) < 2L || anyNA(breaks) || any(!is.finite(breaks)) ||
        any(diff(breaks) <= 0)) {
      stop("breaks must contain at least two strictly increasing finite ",
           "numbers.", call. = FALSE)
    }
    return(unname(as.numeric(breaks)))
  }
  if (!is.character(breaks) || length(breaks) < 2L || anyNA(breaks)) {
    stop("breaks must be finite numeric values, canonical ISO dates, or ",
         "canonical UTC datetimes.", call. = FALSE)
  }
  breaks <- enc2utf8(unname(breaks))
  date_pattern <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  datetime_pattern <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T",
    "[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  )
  if (all(grepl(date_pattern, breaks))) {
    parsed <- as.Date(breaks)
    valid <- !anyNA(parsed) && identical(format(parsed, "%Y-%m-%d"), breaks)
    numeric <- as.numeric(parsed)
  } else if (all(grepl(datetime_pattern, breaks))) {
    parsed <- as.POSIXct(breaks, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    valid <- !anyNA(parsed) && identical(
      format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), breaks
    )
    numeric <- as.numeric(parsed)
  } else {
    stop("Character breaks must all use canonical YYYY-MM-DD dates or ",
         "YYYY-MM-DDTHH:MM:SSZ UTC datetimes.", call. = FALSE)
  }
  if (!isTRUE(valid) || any(diff(numeric) <= 0)) {
    stop("breaks contain invalid or non-increasing dates/times.",
         call. = FALSE)
  }
  breaks
}

#' Define a server-owned sticky privacy release
#'
#' Constructs the public semantic specification for a dedicated dsOMOP
#' sticky-noise release. Noise parameters and state are deliberately
#' absent: epsilon, seeds, nonces, privacy epochs, and ledger controls are owned
#' by each data custodian and cannot be supplied by the analyst.
#' The specification alone does not claim bounded lifetime privacy; that
#' depends on the server accounting mode reported by \code{ds.omop.dp.status()}.
#'
#' Repeated longitudinal records are reduced or capped per person. Public
#' categorical levels are sorted canonically. Numeric histogram breaks may be
#' finite numbers, ISO dates, or canonical UTC datetimes. The \code{"first"}
#' and \code{"last"} reducers require an explicit public \code{order_by}
#' column; row order is never treated as longitudinal time.
#'
#' @param statistic One of \code{"count"}, \code{"bounded_record_count"},
#'   \code{"categorical_histogram"}, \code{"numeric_histogram"},
#'   \code{"bounded_distinct"}, \code{"bounded_mean"}, or
#'   \code{"binary_rate"}.
#' @param variable Bare column name except for \code{"count"} and
#'   \code{"bounded_record_count"}.
#' @param levels Public, fixed character domain for a categorical histogram.
#'   The server returns every requested level, including zero-count levels.
#' @param breaks Public, fixed, strictly increasing finite numbers, canonical
#'   \code{YYYY-MM-DD} dates, or \code{YYYY-MM-DDTHH:MM:SSZ} UTC datetimes.
#' @param lower,upper Public finite bounds for a bounded mean.
#' @param reducer Per-person reducer. Categorical histograms accept
#'   \code{"presence"}, \code{"mode"}, \code{"first"}, \code{"last"}, and
#'   \code{"records"};
#'   numeric histograms also accept \code{"min"}, \code{"max"},
#'   \code{"mean"}, \code{"median"}, and \code{"records"}; bounded means
#'   accept the numeric one-value reducers; binary rates accept \code{"any"},
#'   \code{"all"}, \code{"first"}, and \code{"last"}. For compatibility,
#'   categorical \code{"any"} becomes \code{"presence"}, numeric and
#'   bounded-mean \code{"any"} becomes \code{"mean"}, and the two scalar
#'   bounded primitives canonicalize it to their sole reducer.
#' @param max_contributions Positive integer person-level contribution cap.
#'   Values above one apply to bounded record counts, bounded distinct counts,
#'   categorical \code{"presence"}/\code{"records"} histograms, and numeric
#'   \code{"records"} histograms.
#' @param positive Non-empty public value vector defining positive binary-rate
#'   records. It is canonicalized to sorted, unique character labels.
#' @param order_by Optional bare column defining longitudinal order. Required
#'   for \code{"first"} and \code{"last"}.
#' @param denominator Binary-rate denominator: all persons or only persons with
#'   a non-missing value.
#' @param population_id Optional public compatibility label for the population
#'   represented by \code{x}. When omitted, \code{ds.omop.dp.release()} derives
#'   it from the bare server symbol \code{x}. This label is metadata, not sticky
#'   release identity: changing it does not request or guarantee fresh noise.
#'   Do not put secrets or personal data in this public identifier.
#' @return A strictly validated \code{omop_privacy} specification.
#' @examples
#' omop_privacy("count")
#' omop_privacy("categorical_histogram", variable = "sex",
#'              levels = c("Female", "Male", "Unknown"))
#' omop_privacy("bounded_mean", variable = "value_as_number",
#'              lower = 0, upper = 300, reducer = "mean")
#' @export
omop_privacy <- function(statistic, variable = NULL, levels = NULL,
                         breaks = NULL, lower = NULL, upper = NULL,
                         reducer = "any", max_contributions = 1L,
                         positive = NULL, order_by = NULL,
                         denominator = c("all_persons", "nonmissing"),
                         population_id = NULL) {
  denominator_supplied <- !missing(denominator)
  statistics <- c(
    "count", "bounded_record_count", "categorical_histogram",
    "numeric_histogram", "bounded_distinct", "bounded_mean", "binary_rate"
  )
  statistic <- .dp_choice(statistic, statistics, "statistic")
  reducer <- .dp_scalar_character(reducer, "reducer")
  if (!is.numeric(max_contributions) || length(max_contributions) != 1L ||
      is.na(max_contributions) || !is.finite(max_contributions) ||
      max_contributions != floor(max_contributions) ||
      max_contributions < 1 || max_contributions > 10000) {
    stop("max_contributions must be one integer from 1 to 10000.",
         call. = FALSE)
  }
  max_contributions <- as.integer(max_contributions)
  order_by <- .dp_order_by(order_by)
  population_id <- .dp_population_id(population_id)

  if (identical(statistic, "count")) {
    .dp_assert_unused(list(variable = variable, levels = levels,
                           breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, "any", "reducer")
    if (max_contributions != 1L) {
      stop("count has one contribution per person; max_contributions must be 1.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, reducer = reducer,
                 max_contributions = 1L)
  } else if (identical(statistic, "bounded_record_count")) {
    .dp_assert_unused(list(variable = variable, levels = levels,
                           breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, c("any", "records"), "reducer")
    if (identical(reducer, "any")) reducer <- "records"
    spec <- list(statistic = statistic, reducer = reducer,
                 max_contributions = max_contributions)
  } else if (identical(statistic, "categorical_histogram")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(breaks = breaks, lower = lower, upper = upper,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer,
      c("any", "presence", "mode", "first", "last", "records"),
      "reducer"
    )
    if (identical(reducer, "any")) reducer <- "presence"
    if (!is.character(levels) || length(levels) < 1L || anyNA(levels) ||
        any(!nzchar(levels)) ||
        any(nchar(levels, type = "bytes") > 256L) ||
        anyDuplicated(enc2utf8(levels))) {
      stop("levels must be a non-empty vector of unique, non-missing ",
           "character values.", call. = FALSE)
    }
    levels <- sort(enc2utf8(unname(levels)), method = "radix")
    if (reducer %in% c("mode", "first", "last") &&
        max_contributions != 1L) {
      stop("mode/first/last categorical reducers have one contribution per ",
           "person; max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last", "records") && is.null(order_by)) {
      stop("first/last/records reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("order_by is only valid for first/last/records categorical ",
           "reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 levels = levels, reducer = reducer,
                 max_contributions = max_contributions)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else if (identical(statistic, "numeric_histogram")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, lower = lower, upper = upper,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer, c("any", "min", "max", "mean", "median", "first", "last",
                 "records"), "reducer"
    )
    if (identical(reducer, "any")) reducer <- "mean"
    breaks <- .dp_histogram_breaks(breaks)
    if (!identical(reducer, "records") && max_contributions != 1L) {
      stop("A reduced numeric histogram has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) &&
        !reducer %in% c("first", "last", "records")) {
      stop("order_by is only valid for first/last/records numeric reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 breaks = breaks, reducer = reducer,
                 max_contributions = max_contributions)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else if (identical(statistic, "bounded_distinct")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(breaks = breaks, lower = lower, upper = upper,
                           positive = positive, order_by = order_by,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(reducer, c("any", "distinct"), "reducer")
    if (identical(reducer, "any")) reducer <- "distinct"
    levels <- .dp_public_values(levels, "levels")
    spec <- list(
      statistic = statistic, variable = variable, levels = levels,
      reducer = reducer, max_contributions = max_contributions
    )
  } else if (identical(statistic, "bounded_mean")) {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, breaks = breaks,
                           positive = positive,
                           denominator = if (denominator_supplied) {
                             denominator
                           } else NULL), statistic)
    reducer <- .dp_choice(
      reducer, c("any", "min", "max", "mean", "median", "first", "last"),
      "reducer"
    )
    if (identical(reducer, "any")) reducer <- "mean"
    lower <- .dp_scalar_number(lower, "lower")
    upper <- .dp_scalar_number(upper, "upper")
    if (lower >= upper || !is.finite(upper - lower)) {
      stop("lower and upper must define one finite positive span.",
           call. = FALSE)
    }
    if (max_contributions != 1L) {
      stop("bounded_mean has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) && !reducer %in% c("first", "last")) {
      stop("order_by is only valid for first/last bounded-mean reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 lower = lower, upper = upper, reducer = reducer,
                 max_contributions = 1L)
    if (!is.null(order_by)) spec$order_by <- order_by
  } else {
    variable <- .dp_variable(variable)
    .dp_assert_unused(list(levels = levels, breaks = breaks, lower = lower,
                           upper = upper), statistic)
    reducer <- .dp_choice(reducer, c("any", "all", "first", "last"),
                          "reducer")
    if (length(denominator) > 1L &&
        identical(denominator, c("all_persons", "nonmissing"))) {
      denominator <- denominator[[1L]]
    }
    denominator <- .dp_choice(
      denominator, c("all_persons", "nonmissing"), "denominator"
    )
    positive <- .dp_public_values(positive, "positive")
    if (max_contributions != 1L) {
      stop("binary_rate has one contribution per person; ",
           "max_contributions must be 1.", call. = FALSE)
    }
    if (reducer %in% c("first", "last") && is.null(order_by)) {
      stop("first/last reducers require order_by.", call. = FALSE)
    }
    if (!is.null(order_by) && !reducer %in% c("first", "last")) {
      stop("order_by is only valid for first/last binary-rate reducers.",
           call. = FALSE)
    }
    spec <- list(statistic = statistic, variable = variable,
                 positive = positive, reducer = reducer,
                 max_contributions = 1L, denominator = denominator)
    if (!is.null(order_by)) spec$order_by <- order_by
  }

  if (!is.null(population_id)) spec$population_id <- population_id

  structure(spec, class = c("omop_privacy", "list"))
}

.dp_datasources <- function(datasources) {
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!is.list(datasources) || length(datasources) < 1L ||
      is.null(names(datasources)) || anyNA(names(datasources)) ||
      any(!nzchar(names(datasources))) || anyDuplicated(names(datasources))) {
    stop("datasources must be a non-empty, uniquely named list of DataSHIELD ",
         "connections.", call. = FALSE)
  }
  datasources
}

.dp_complete_aggregate <- function(datasources, expr, context) {
  expected <- names(datasources)
  result <- .ds_safe_aggregate(datasources, expr)
  errors <- attr(result, "ds_errors") %||% list()
  missing <- setdiff(expected, names(result))
  unexpected <- setdiff(names(result), expected)
  if (length(errors) > 0L || length(missing) > 0L ||
      length(unexpected) > 0L) {
    failed <- unique(c(names(errors), missing, unexpected))
    detail <- if (length(errors) > 0L) {
      paste(names(errors), unlist(errors, use.names = FALSE), sep = ": ",
            collapse = "; ")
    } else paste(failed, collapse = ", ")
    stop(context, " requires a complete federation; no partial result was ",
         "published. Affected server(s): ", paste(failed, collapse = ", "),
         if (nzchar(detail)) paste0(" (", detail, ")") else "", ".",
         call. = FALSE)
  }
  result[expected]
}

.dp_status_shape <- function(status, server) {
  if (!is.list(status) || is.null(names(status)) || anyNA(names(status)) ||
      anyDuplicated(names(status))) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  status <- .dp_normalize_legacy_attestation(status)
  required <- c("enabled", "ready", "sticky_noise", "protocol", "mechanism")
  if (!all(required %in% names(status))) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  for (field in c("enabled", "ready", "sticky_noise")) {
    if (!is.logical(status[[field]]) || length(status[[field]]) != 1L ||
        is.na(status[[field]])) {
      stop("Server '", server, "' returned an invalid DP status field '",
           field, "'.", call. = FALSE)
    }
  }
  for (field in c("protocol", "mechanism")) {
    if (!is.character(status[[field]]) || length(status[[field]]) != 1L ||
        is.na(status[[field]]) || !nzchar(status[[field]])) {
      stop("Server '", server, "' returned an invalid DP status field '",
           field, "'.", call. = FALSE)
    }
  }
  if (!isTRUE(status$enabled)) {
    if (isTRUE(status$ready) || isTRUE(status$sticky_noise)) {
      stop("Server '", server, "' returned an incoherent disabled DP status.",
           call. = FALSE)
    }
    return(status)
  }
  if (!"snapshot_id" %in% names(status) ||
      !is.character(status$snapshot_id) || length(status$snapshot_id) != 1L ||
      is.na(status$snapshot_id) || !nzchar(status$snapshot_id)) {
    stop("Server '", server, "' returned a malformed DP status.",
         call. = FALSE)
  }
  if (nchar(status$snapshot_id, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]*$", status$snapshot_id)) {
    stop("Server '", server, "' returned an invalid public snapshot_id.",
         call. = FALSE)
  }
  status
}

#' Inspect sticky privacy-release services
#'
#' Queries every selected DataSHIELD server. Unlike permissive exploration
#' helpers, this function never returns a partial federation: each requested
#' node must provide a well-formed status.
#'
#' \code{"bounded_accounted"} uses a summable, non-blocking nominal allocation
#' and may eventually return data-independent degraded releases. The
#' compatibility mode \code{"sticky_unbounded"} remains sticky for an exact
#' authenticated canonical lineage and typed statistic, but does not identify
#' every mathematically equivalent alternate query construction and does not
#' bound global composition over unlimited distinct queries.
#' The \code{privacy_guarantee} field names the implemented sticky,
#' person-bounded mechanism and nominal accounting contract.
#' Eligible input frames must also carry the server's authenticated
#' person-local provenance capsule; a copied class or plain attribute is not
#' sufficient.
#' Each status contains the custodian's public \code{snapshot_id}. Federated
#' sites may legitimately report different snapshot identifiers.
#' Release preflight rejects duplicate noise domains, ledgers, or
#' domain-scoped ledger authentication keys so the same logical privacy node
#' cannot be pooled twice, including while replicas converge on a rotated noise
#' root or when durable state was accidentally forked.
#'
#' @param datasources Named DataSHIELD connection list. \code{NULL} uses
#'   \code{DSI::datashield.connections_find()}.
#' @return A complete named list of per-server DP status records.
#' @examples
#' \dontrun{ds.omop.dp.status()}
#' @export
ds.omop.dp.status <- function(datasources = NULL) {
  datasources <- .dp_datasources(datasources)
  statuses <- .dp_complete_aggregate(
    datasources, call("omopDpStatusDS"), "DP status preflight"
  )
  for (server in names(statuses)) {
    statuses[[server]] <- .dp_status_shape(statuses[[server]], server)
  }
  statuses
}

.dp_status_contract <- function(statuses, privacy) {
  for (server in names(statuses)) {
    status <- statuses[[server]]
    if (!identical(status$enabled, TRUE) || !identical(status$ready, TRUE) ||
        !identical(status$sticky_noise, TRUE)) {
      stop("Server '", server, "' does not provide an enabled, ready sticky ",
           "DP service.", call. = FALSE)
    }
  }

  required_fields <- c(
    "protocol", "canonical_protocol", "mechanism", "sampler",
    "privacy_guarantee", "person_local_provenance_required",
    "provenance_protocol", "adjacency", "accounting_mode", "allocator",
    "total_epsilon", "total_delta", "release_epsilon", "release_delta",
    "max_levels", "max_contributions", "numeric_grid", "bounded_accounting",
    "never_budget_blocked", "budget_behavior",
    "supported_statistics", "longitudinal_contract", "privacy_epoch",
    "next_release_epsilon", "next_release_degraded", "domain",
    "privacy_instance_id",
    "noise_domain_id", "ledger_id", "ledger_key_id", "noise_key_id"
  )
  for (server in names(statuses)) {
    missing <- required_fields[
      !required_fields %in% names(statuses[[server]])
    ]
    if (length(missing) > 0L) {
      stop("Server '", server, "' omitted DP contract field(s): ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    character_fields <- c(
      "protocol", "canonical_protocol", "mechanism", "sampler",
      "privacy_guarantee", "provenance_protocol", "adjacency",
      "accounting_mode", "allocator",
      "budget_behavior", "longitudinal_contract", "domain",
      "privacy_instance_id",
      "noise_domain_id", "ledger_id", "ledger_key_id", "noise_key_id"
    )
    if (any(!vapply(statuses[[server]][character_fields], function(value) {
      is.character(value) && length(value) == 1L && !is.na(value) &&
        nzchar(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid textual DP contract fields.",
           call. = FALSE)
    }
    numeric_fields <- c("total_epsilon", "total_delta", "release_epsilon",
                        "release_delta", "max_levels", "max_contributions",
                        "numeric_grid", "privacy_epoch",
                        "next_release_epsilon")
    if (any(!vapply(statuses[[server]][numeric_fields], function(value) {
      is.numeric(value) && length(value) == 1L && !is.na(value) &&
        is.finite(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid numeric DP contract fields.",
           call. = FALSE)
    }
    boolean_fields <- c(
      "bounded_accounting", "never_budget_blocked",
      "person_local_provenance_required", "next_release_degraded"
    )
    if (any(!vapply(statuses[[server]][boolean_fields], function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }, logical(1L)))) {
      stop("Server '", server, "' returned invalid DP accounting flags.",
           call. = FALSE)
    }
    if (!statuses[[server]]$accounting_mode %in%
          c("bounded_accounted", "sticky_unbounded") ||
        statuses[[server]]$total_epsilon <= 0 ||
        statuses[[server]]$total_delta < 0 ||
        statuses[[server]]$total_delta >= 1 ||
        statuses[[server]]$release_epsilon <= 0 ||
        statuses[[server]]$release_epsilon >
          statuses[[server]]$total_epsilon ||
        statuses[[server]]$release_delta < 0 ||
        statuses[[server]]$release_delta > statuses[[server]]$total_delta ||
        statuses[[server]]$next_release_epsilon < 0 ||
        statuses[[server]]$next_release_epsilon >
          statuses[[server]]$release_epsilon ||
        any(vapply(statuses[[server]][c("max_levels", "max_contributions",
                                       "numeric_grid")], function(value) {
          value < 1 || value != floor(value)
        }, logical(1L))) || statuses[[server]]$privacy_epoch < 1 ||
        statuses[[server]]$privacy_epoch !=
          floor(statuses[[server]]$privacy_epoch)) {
      stop("Server '", server, "' returned out-of-range DP contract fields.",
           call. = FALSE)
    }
    if ((isTRUE(statuses[[server]]$next_release_degraded) &&
         statuses[[server]]$next_release_epsilon != 0) ||
        (!isTRUE(statuses[[server]]$next_release_degraded) &&
         statuses[[server]]$next_release_epsilon <= 0)) {
      stop("Server '", server, "' returned an incoherent next DP allocation.",
           call. = FALSE)
    }
    expected_budget_behavior <- if (identical(
      statuses[[server]]$accounting_mode, "bounded_accounted"
    )) {
      "degrade_to_data_independent_zero_no_error"
    } else {
      "fixed_epsilon_no_budget_exhaustion_error_unbounded_composition"
    }
    if (!identical(statuses[[server]]$budget_behavior,
                   expected_budget_behavior)) {
      stop("Server '", server, "' returned an incoherent budget behavior.",
           call. = FALSE)
    }
    if (!identical(statuses[[server]]$privacy_guarantee,
                   .DP_PRIVACY_GUARANTEE)) {
      stop("Server '", server, "' returned an unsupported privacy guarantee.",
           call. = FALSE)
    }
    identifiers <- statuses[[server]]
    valid_identifiers <-
      grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", identifiers$domain) &&
      grepl("^dpi_[0-9a-f]{40}$", identifiers$privacy_instance_id) &&
      grepl("^dpn_[0-9a-f]{40}$", identifiers$noise_domain_id) &&
      grepl("^[0-9a-f]{64}$", identifiers$ledger_id) &&
      grepl("^dpl_[0-9a-f]{40}$", identifiers$ledger_key_id) &&
      grepl("^dpk_[0-9a-f]{40}$", identifiers$noise_key_id)
    if (!isTRUE(valid_identifiers)) {
      stop("Server '", server, "' returned invalid DP continuity ",
           "identifiers.", call. = FALSE)
    }
    statistics <- statuses[[server]]$supported_statistics
    if (!is.character(statistics) || length(statistics) < 1L ||
        anyNA(statistics) || any(!nzchar(statistics)) ||
        anyDuplicated(statistics) || !privacy$statistic %in% statistics) {
      stop("Server '", server, "' does not support the requested bounded DP ",
           "statistic.", call. = FALSE)
    }
    expected_accounted <- identical(
      statuses[[server]]$accounting_mode, "bounded_accounted"
    )
    if (!identical(statuses[[server]]$bounded_accounting,
                   expected_accounted) ||
        !identical(statuses[[server]]$person_local_provenance_required, TRUE) ||
        !identical(statuses[[server]]$never_budget_blocked, TRUE)) {
      stop("Server '", server, "' returned an incoherent non-blocking DP ",
           "accounting contract.", call. = FALSE)
    }
  }
  noise_domains <- vapply(
    statuses, `[[`, character(1L), "noise_domain_id"
  )
  duplicated_domains <- unique(noise_domains[duplicated(noise_domains)])
  if (length(duplicated_domains) > 0L) {
    affected <- names(noise_domains)[noise_domains %in% duplicated_domains]
    stop("Selected servers share a DP noise domain (",
         paste(affected, collapse = ", "), "); refusing a duplicate ",
         "federated release from one logical privacy node.", call. = FALSE)
  }
  ledger_ids <- vapply(statuses, `[[`, character(1L), "ledger_id")
  duplicated_ledgers <- unique(ledger_ids[duplicated(ledger_ids)])
  if (length(duplicated_ledgers) > 0L) {
    affected <- names(ledger_ids)[ledger_ids %in% duplicated_ledgers]
    stop("Selected servers share a DP ledger (",
         paste(affected, collapse = ", "), "); refusing a duplicate ",
         "federated release from one logical privacy node.", call. = FALSE)
  }
  ledger_domains <- vapply(statuses, function(status) {
    paste(status$domain, status$ledger_key_id, sep = "\u001f")
  }, character(1L))
  duplicated_ledger_domains <- unique(
    ledger_domains[duplicated(ledger_domains)]
  )
  if (length(duplicated_ledger_domains) > 0L) {
    affected <- names(ledger_domains)[
      ledger_domains %in% duplicated_ledger_domains
    ]
    stop("Selected servers share a domain-scoped DP ledger key (",
         paste(affected, collapse = ", "), "); refusing a duplicate ",
         "federated release from forked privacy state.", call. = FALSE)
  }
  common_fields <- c(
    "protocol", "canonical_protocol", "mechanism", "sampler",
    "privacy_guarantee", "person_local_provenance_required",
    "provenance_protocol", "adjacency", "accounting_mode", "allocator",
    "bounded_accounting", "never_budget_blocked",
    "budget_behavior", "longitudinal_contract"
  )
  for (field in common_fields) {
    values <- lapply(statuses, `[[`, field)
    first <- values[[1L]]
    if (!all(vapply(values[-1L], identical, logical(1L), first))) {
      stop("DP contract field '", field, "' differs across servers.",
           call. = FALSE)
    }
  }

  reference <- statuses[[1L]]
  common_max_contributions <- min(vapply(
    statuses, `[[`, numeric(1L), "max_contributions"
  ))
  common_max_levels <- min(vapply(
    statuses, `[[`, numeric(1L), "max_levels"
  ))
  if (privacy$max_contributions > common_max_contributions) {
    stop("The privacy specification exceeds the common server ",
         "max_contributions cap of ", common_max_contributions, ".",
         call. = FALSE)
  }
  if (privacy$statistic %in% c("categorical_histogram", "bounded_distinct") &&
      length(privacy$levels) > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "numeric_histogram") &&
      length(privacy$breaks) - 1L > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "binary_rate") &&
      length(privacy$positive) > common_max_levels) {
    stop("The privacy specification exceeds the common server max_levels cap ",
         "of ", common_max_levels, ".", call. = FALSE)
  }
  if (identical(privacy$statistic, "bounded_mean")) {
    grids <- vapply(statuses, `[[`, numeric(1L), "numeric_grid")
    if (length(unique(grids)) != 1L) {
      stop("DP contract field 'numeric_grid' differs across servers for a ",
           "bounded mean.", call. = FALSE)
    }
  }
  reference$max_contributions <- common_max_contributions
  reference$max_levels <- common_max_levels
  reference
}

.dp_release_shape <- function(value, server, privacy, contract) {
  value <- .dp_normalize_legacy_attestation(value)
  common <- c(
    "protocol", "mechanism", "adjacency", "epsilon", "delta",
    "accounting_mode", "allocator", "sticky", "degraded", "statistic",
    "sampler", "sensitivity"
  )
  statistic_fields <- switch(
    privacy$statistic,
    count = "noisy_count",
    bounded_record_count = c(
      "noisy_count", "reducer", "max_contributions"
    ),
    categorical_histogram = c(
      "levels", "counts", "reducer", "max_contributions", "value_type"
    ),
    numeric_histogram = c(
      "breaks", "counts", "reducer", "max_contributions", "value_type",
      "interval_contract"
    ),
    bounded_distinct = c(
      "noisy_count", "reducer", "max_contributions", "domain_size",
      "selection_order", "value_type"
    ),
    bounded_mean = c(
      "noisy_count", "noisy_sum_grid", "value", "lower", "upper",
      "numeric_grid", "reducer", "value_type"
    ),
    binary_rate = c(
      "noisy_numerator", "noisy_denominator", "value", "reducer",
      "denominator", "value_type"
    )
  )
  allowed <- c(common, statistic_fields)
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value))) {
    stop("Server '", server, "' returned a malformed DP release.",
         call. = FALSE)
  }
  if (any(c("seed", "nonce", "noise_root", "raw_noise") %in% names(value))) {
    stop("Server '", server, "' returned forbidden private noise state.",
         call. = FALSE)
  }
  if (!setequal(names(value), allowed)) {
    stop("Server '", server, "' returned a malformed DP release schema.",
         call. = FALSE)
  }
  expected <- list(
    protocol = contract$protocol, mechanism = contract$mechanism,
    adjacency = contract$adjacency,
    accounting_mode = contract$accounting_mode,
    allocator = contract$allocator, sticky = TRUE,
    sampler = contract$sampler,
    statistic = privacy$statistic
  )
  for (field in names(expected)) {
    observed <- value[[field]]
    target <- expected[[field]]
    if (is.numeric(target)) {
      valid <- is.numeric(observed) && length(observed) == 1L &&
        !is.na(observed) && is.finite(observed) && identical(as.numeric(observed),
                                                               target)
    } else {
      valid <- identical(observed, target)
    }
    if (!isTRUE(valid)) {
      stop("Server '", server, "' returned a DP release whose '", field,
           "' field does not match the preflight contract.", call. = FALSE)
    }
  }
  if (!is.logical(value$degraded) || length(value$degraded) != 1L ||
      is.na(value$degraded) || !is.numeric(value$epsilon) ||
      length(value$epsilon) != 1L || is.na(value$epsilon) ||
      !is.finite(value$epsilon) || value$epsilon < 0 ||
      value$epsilon > contract$release_epsilon ||
      !is.numeric(value$delta) || length(value$delta) != 1L ||
      is.na(value$delta) || !is.finite(value$delta) || value$delta < 0 ||
      value$delta > contract$release_delta ||
      (isTRUE(value$degraded) &&
       (value$epsilon != 0 || value$delta != 0)) ||
      (!isTRUE(value$degraded) && value$epsilon <= 0)) {
    stop("Server '", server, "' returned an invalid effective DP allocation.",
         call. = FALSE)
  }
  value
}

.dp_nonnegative_integer <- function(value, server, field) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0 || value > 2^53 - 1 ||
      value != floor(value)) {
    stop("Server '", server, "' returned invalid DP field '", field, "'.",
         call. = FALSE)
  }
  as.numeric(value)
}

.dp_count_vector <- function(value, server, expected_length) {
  if (!is.numeric(value) || length(value) != expected_length || anyNA(value) ||
      any(!is.finite(value)) || any(value < 0) || any(value > 2^53 - 1) ||
      any(value != floor(value))) {
    stop("Server '", server, "' returned invalid DP histogram counts.",
         call. = FALSE)
  }
  as.numeric(value)
}

.dp_payload_equal <- function(value, expected, server, field) {
  equal <- if (is.numeric(expected)) {
    is.numeric(value) && length(value) == length(expected) && !anyNA(value) &&
      all(is.finite(value)) && identical(as.numeric(value),
                                         as.numeric(expected))
  } else {
    identical(value, expected)
  }
  if (!isTRUE(equal)) {
    stop("Server '", server, "' returned a DP payload contract drift in '",
         field, "'.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_validate_sensitivity <- function(value, server, privacy, contract) {
  expected <- switch(
    privacy$statistic,
    count = list(l1 = 1, unit = "person"),
    bounded_record_count = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    categorical_histogram = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    numeric_histogram = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    bounded_distinct = list(
      l1 = privacy$max_contributions, unit = "person"
    ),
    bounded_mean = list(
      count = 1, sum_grid = as.numeric(contract$numeric_grid),
      allocation = "sequential_half_epsilon", unit = "person"
    ),
    binary_rate = list(
      numerator = 1, denominator = 1,
      allocation = "sequential_half_epsilon", unit = "person"
    )
  )
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value)) || !setequal(names(value), names(expected))) {
    stop("Server '", server, "' returned malformed DP sensitivity metadata.",
         call. = FALSE)
  }
  for (field in names(expected)) {
    .dp_payload_equal(value[[field]], expected[[field]], server,
                      paste0("sensitivity.", field))
  }
  invisible(TRUE)
}

.dp_validate_estimate <- function(value, expected, lower, upper, server,
                                  statistic) {
  valid <- if (is.null(expected)) {
    is.null(value)
  } else {
    is.numeric(value) && length(value) == 1L && !is.na(value) &&
      is.finite(value) && value >= lower && value <= upper &&
      isTRUE(all.equal(as.numeric(value), as.numeric(expected),
                       tolerance = 1e-12))
  }
  if (!isTRUE(valid)) {
    stop("Server '", server, "' returned an incoherent ", statistic,
         " estimate.", call. = FALSE)
  }
  invisible(TRUE)
}

.dp_validate_payload <- function(value, server, privacy, contract) {
  value <- .dp_release_shape(value, server, privacy, contract)
  statistic <- privacy$statistic
  .dp_validate_sensitivity(value$sensitivity, server, privacy, contract)
  if (identical(statistic, "count")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
  } else if (identical(statistic, "bounded_record_count")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
    .dp_payload_equal(value$reducer, "records", server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
  } else if (identical(statistic, "categorical_histogram")) {
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    .dp_payload_equal(value$levels, privacy$levels, server, "levels")
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    value$counts <- .dp_count_vector(value$counts, server,
                                     length(privacy$levels))
  } else if (identical(statistic, "numeric_histogram")) {
    expected_type <- if (is.numeric(privacy$breaks)) {
      "number"
    } else if (grepl("T", privacy$breaks[[1L]], fixed = TRUE)) {
      "datetime_utc"
    } else {
      "date"
    }
    .dp_payload_equal(value$value_type, expected_type, server, "value_type")
    .dp_payload_equal(value$breaks, privacy$breaks, server, "breaks")
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    .dp_payload_equal(
      value$interval_contract, "left_closed_right_open_last_closed",
      server, "interval_contract"
    )
    value$counts <- .dp_count_vector(value$counts, server,
                                     length(privacy$breaks) - 1L)
  } else if (identical(statistic, "bounded_distinct")) {
    value$noisy_count <- .dp_nonnegative_integer(
      value$noisy_count, server, "noisy_count"
    )
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    .dp_payload_equal(value$reducer, "distinct", server, "reducer")
    .dp_payload_equal(value$max_contributions,
                      privacy$max_contributions, server,
                      "max_contributions")
    .dp_payload_equal(value$domain_size, length(privacy$levels), server,
                      "domain_size")
    .dp_payload_equal(value$selection_order,
                      "canonical_utf8_value_radix", server,
                      "selection_order")
  } else if (identical(statistic, "bounded_mean")) {
    .dp_payload_equal(value$value_type, "number", server, "value_type")
    for (field in c("noisy_count", "noisy_sum_grid")) {
      value[[field]] <- .dp_nonnegative_integer(value[[field]], server, field)
    }
    for (field in c("lower", "upper", "numeric_grid")) {
      observed <- value[[field]]
      target <- if (identical(field, "numeric_grid")) {
        as.numeric(contract$numeric_grid)
      } else privacy[[field]]
      if (!is.numeric(observed) || length(observed) != 1L || is.na(observed) ||
          !is.finite(observed) || !identical(as.numeric(observed), target)) {
        stop("Server '", server, "' returned a bounded-mean contract drift in '",
             field, "'.", call. = FALSE)
      }
    }
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    if (value$noisy_sum_grid > value$noisy_count * value$numeric_grid) {
      stop("Server '", server, "' returned infeasible bounded-mean ",
           "sufficient statistics.", call. = FALSE)
    }
    expected_value <- if (value$noisy_count > 0) {
      privacy$lower + (privacy$upper - privacy$lower) *
        value$noisy_sum_grid / (value$noisy_count * value$numeric_grid)
    } else NULL
    .dp_validate_estimate(
      value$value, expected_value, privacy$lower, privacy$upper, server,
      "bounded-mean"
    )
  } else {
    .dp_payload_equal(value$value_type, "categorical_utf8_v1", server,
                      "value_type")
    for (field in c("noisy_numerator", "noisy_denominator")) {
      value[[field]] <- .dp_nonnegative_integer(value[[field]], server, field)
    }
    .dp_payload_equal(value$reducer, privacy$reducer, server, "reducer")
    .dp_payload_equal(value$denominator, privacy$denominator, server,
                      "denominator")
    if (value$noisy_numerator > value$noisy_denominator) {
      stop("Server '", server, "' returned an infeasible binary rate.",
           call. = FALSE)
    }
    expected_value <- if (value$noisy_denominator > 0) {
      value$noisy_numerator / value$noisy_denominator
    } else NULL
    .dp_validate_estimate(
      value$value, expected_value, 0, 1, server, "binary-rate"
    )
  }
  if (!statistic %in% c("count", "bounded_record_count") &&
      (!is.character(value$value_type) || length(value$value_type) != 1L ||
       is.na(value$value_type) || !nzchar(value$value_type))) {
    stop("Server '", server, "' returned an invalid DP value type.",
         call. = FALSE)
  }
  if (isTRUE(value$degraded)) {
    released <- switch(
      statistic,
      count = value$noisy_count,
      bounded_record_count = value$noisy_count,
      categorical_histogram = value$counts,
      numeric_histogram = value$counts,
      bounded_distinct = value$noisy_count,
      bounded_mean = c(value$noisy_count, value$noisy_sum_grid),
      binary_rate = c(value$noisy_numerator, value$noisy_denominator)
    )
    if (any(released != 0)) {
      stop("Server '", server, "' returned a non-constant degraded payload.",
           call. = FALSE)
    }
  }
  value
}

.dp_histogram_labels <- function(breaks) {
  lower <- utils::head(breaks, -1L)
  upper <- utils::tail(breaks, -1L)
  paste0("[", lower, ", ", upper,
         c(rep.int(")", max(0L, length(lower) - 1L)), "]"))
}

.dp_format_histogram <- function(labels, counts, degraded, format,
                                 numeric_breaks = NULL) {
  if (identical(format, "raw")) {
    value <- list(statistic = if (is.null(numeric_breaks)) {
      "categorical_histogram"
    } else "numeric_histogram", counts = counts, degraded = degraded)
    if (is.null(numeric_breaks)) value$levels <- labels else {
      value$breaks <- numeric_breaks
      value$interval_contract <- "left_closed_right_open_last_closed"
    }
    return(value)
  }
  if (identical(format, "vector")) {
    value <- stats::setNames(counts, labels)
    attr(value, "degraded") <- degraded
    return(value)
  }
  if (identical(format, "wide")) {
    value <- as.data.frame(
      as.list(stats::setNames(counts, labels)), check.names = FALSE,
      stringsAsFactors = FALSE
    )
    attr(value, "degraded") <- degraded
    return(value)
  }
  if (is.null(numeric_breaks)) {
    return(data.frame(level = labels, noisy_count = counts,
                      degraded = degraded, stringsAsFactors = FALSE))
  }
  data.frame(lower = utils::head(numeric_breaks, -1L),
             upper = utils::tail(numeric_breaks, -1L),
             noisy_count = counts, degraded = degraded,
             stringsAsFactors = FALSE)
}

.dp_pool_release <- function(per_site, privacy, contract, format = "long") {
  statistic <- privacy$statistic
  degraded <- any(vapply(per_site, `[[`, logical(1L), "degraded"))
  if (statistic %in% c(
    "count", "bounded_record_count", "bounded_distinct"
  )) {
    values <- vapply(per_site, `[[`, numeric(1L), "noisy_count")
    result <- list(statistic = statistic, noisy_count = sum(values),
                   degraded = degraded)
    if (identical(statistic, "bounded_distinct")) {
      result$pooling <- "sum_of_site_local_distinct_cardinalities"
    }
    return(result)
  }
  if (identical(statistic, "categorical_histogram")) {
    counts <- Reduce(`+`, lapply(per_site, `[[`, "counts"))
    return(.dp_format_histogram(
      privacy$levels, counts, degraded, format
    ))
  }
  if (identical(statistic, "numeric_histogram")) {
    counts <- Reduce(`+`, lapply(per_site, `[[`, "counts"))
    return(.dp_format_histogram(
      .dp_histogram_labels(privacy$breaks), counts, degraded, format,
      numeric_breaks = privacy$breaks
    ))
  }
  if (identical(statistic, "bounded_mean")) {
    noisy_count <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_count"))
    noisy_sum <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_sum_grid"))
    estimate <- if (noisy_count > 0) {
      privacy$lower + (privacy$upper - privacy$lower) *
        (noisy_sum / noisy_count) / as.numeric(contract$numeric_grid)
    } else NA_real_
    if (is.finite(estimate)) {
      estimate <- min(privacy$upper, max(privacy$lower, estimate))
    }
    return(list(statistic = statistic, estimate = estimate,
                noisy_count = noisy_count, noisy_sum_grid = noisy_sum,
                lower = privacy$lower, upper = privacy$upper,
                numeric_grid = as.numeric(contract$numeric_grid),
                degraded = degraded))
  }
  numerator <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_numerator"))
  denominator <- sum(vapply(per_site, `[[`, numeric(1L), "noisy_denominator"))
  estimate <- if (denominator > 0) numerator / denominator else NA_real_
  if (is.finite(estimate)) estimate <- min(1, max(0, estimate))
  list(statistic = statistic, estimate = estimate,
       noisy_numerator = numerator, noisy_denominator = denominator,
       degraded = degraded)
}

#' Request a sticky privacy release
#'
#' Performs a complete-federation preflight, requests the same typed release
#' from every node, verifies the returned mechanism contract, and optionally
#' pools only the noisy sufficient statistics. A failure at any site stops the
#' call without publishing another site's value. Servers may already have
#' committed their sticky release; retrying the identical request returns the
#' same noise rather than rerolling it.
#' Sticky release identity is server-owned and bound to authenticated canonical
#' dataset/recipe lineage, the typed statistic, and the custodian-owned public
#' \code{snapshot_id}. A separate private fingerprint detects drift in the
#' bounded sufficient statistic; protected values never select the noise draw.
#' The analyst's \code{population_id} compatibility label and server symbol
#' alias do not participate, so changing either does not request or guarantee
#' fresh noise. The custodian must rotate
#' \code{snapshot_id} when the protected ETL snapshot changes; that controlled
#' rotation intentionally creates a new release identity.
#' For multiple sites, pooling a non-count statistic additionally requires one
#' compatible public dsOMOP harmonization contract for age grids, date
#' semantics, calendar-day granularity, UTC handling, week start, and
#' operational caps. Per-site output and pooled distinct-person counts do not
#' depend on those value semantics and therefore do not require that unrelated
#' contract.
#' Every input must have been produced by an audited person-local server path
#' and carry its authenticated content-bound provenance capsule.
#'
#' No accounting mode hard-blocks a new operation. In
#' \code{"bounded_accounted"} mode, the nominal noise calibration follows a
#' summable server-owned schedule; once an informative allocation is too small,
#' the endpoint returns a marked, data-independent degraded payload at epsilon
#' zero. In
#' \code{"sticky_unbounded"} mode, an exact authenticated canonical lineage and
#' statistic cannot be rerolled. Alternate constructions that happen to be
#' mathematically equivalent may still be distinct releases, and unlimited
#' distinct queries do not have a finite global DP composition guarantee.
#'
#' @param x One bare DataSHIELD symbol containing a server-side
#'   \code{omop.table}.
#' @param privacy An \code{omop_privacy} specification. If it does not contain
#'   an explicit \code{population_id}, the bare symbol \code{x} is used as its
#'   public compatibility label. This label does not control sticky identity.
#' @param datasources Named DataSHIELD connection list. \code{NULL} uses
#'   \code{DSI::datashield.connections_find()}.
#' @param pool Logical; pool the complete set of noisy site releases.
#' @param format Client-only pooled-result format: long data frame, one-row
#'   wide data frame, named vector, or raw list. Histogram releases support all
#'   four forms; other statistics retain their typed list. This argument never
#'   enters the server specification or sticky-release identity.
#' @return A \code{dsomop_result}. The \code{meta$privacy} record reports the
#'   effective population label, a named public snapshot map, named
#'   per-server accounting records, nominal per-site epsilon, degradation, and
#'   conservative cross-site accounting. A pooled payload is marked degraded
#'   if any site returned its data-independent fallback. Parallel accounting is
#'   used only when every server explicitly attests
#'   \code{disjoint_persons = TRUE}. Without that attestation, pooling sums
#'   site-local contributions and may count the same real person once per site;
#'   the privacy loss is composed sequentially. Multi-site results also carry
#'   \code{meta$harmonization} when non-count values were pooled.
#' @examples
#' \dontrun{
#' p <- omop_privacy("count")
#' ds.omop.dp.release("analysis_table", p)
#' }
#' @export
ds.omop.dp.release <- function(x, privacy, datasources = NULL, pool = TRUE,
                               format = c("long", "wide", "vector", "raw")) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", x) || grepl("^\\.[0-9]", x)) {
    stop("x must be one bare DataSHIELD symbol.", call. = FALSE)
  }
  if (!inherits(privacy, "omop_privacy") || !is.list(privacy)) {
    stop("privacy must be created by omop_privacy().", call. = FALSE)
  }
  if (!is.logical(pool) || length(pool) != 1L || is.na(pool)) {
    stop("pool must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(format) > 1L &&
      identical(format, c("long", "wide", "vector", "raw"))) {
    format <- format[[1L]]
  }
  format <- .dp_choice(format, c("long", "wide", "vector", "raw"),
                       "format")
  # Reconstructing also rejects forged objects with the right S3 class.
  rebuilt <- do.call(omop_privacy, unclass(privacy))
  if (!identical(unclass(rebuilt), unclass(privacy))) {
    stop("privacy is not a canonical omop_privacy specification.",
         call. = FALSE)
  }
  population_id <- .dp_population_id(privacy$population_id %||% x,
                                     nullable = FALSE)
  server_privacy <- unclass(privacy)
  server_privacy$population_id <- population_id

  datasources <- .dp_datasources(datasources)
  statuses <- ds.omop.dp.status(datasources)
  contract <- .dp_status_contract(statuses, privacy)
  harmonization <- NULL
  count_like <- privacy$statistic %in% c(
    "count", "bounded_record_count", "bounded_distinct"
  )
  if (length(datasources) > 1L && isTRUE(pool) && !count_like) {
    disclosure <- .dp_complete_aggregate(
      datasources, call("omopDisclosureSettingsDS"),
      "DP harmonization preflight"
    )
    harmonization <- .federated_harmonization_contract(
      disclosure, expected_servers = names(datasources), fail = TRUE
    )
  }
  raw <- .dp_complete_aggregate(
    datasources,
    call("omopDpReleaseDS", as.name(x), .ds_encode(server_privacy)),
    "DP release"
  )
  for (server in names(raw)) {
    raw[[server]] <- .dp_validate_payload(
      raw[[server]], server, privacy, statuses[[server]]
    )
  }
  if (isTRUE(pool) && !privacy$statistic %in% c(
    "count", "bounded_record_count"
  )) {
    value_types <- vapply(raw, `[[`, character(1L), "value_type")
    if (length(unique(value_types)) != 1L) {
      stop("DP release value_type differs across servers; no pooled or ",
           "partial result was published.", call. = FALSE)
    }
  }

  pooled <- if (isTRUE(pool)) {
    .dp_pool_release(raw, privacy, contract, format = format)
  } else NULL
  epsilons <- vapply(raw, function(value) as.numeric(value$epsilon), numeric(1L))
  deltas <- vapply(raw, function(value) as.numeric(value$delta), numeric(1L))
  degraded <- vapply(raw, `[[`, logical(1L), "degraded")
  snapshot_ids <- vapply(statuses, `[[`, character(1L), "snapshot_id")
  accounting <- lapply(names(statuses), function(server) {
    status <- statuses[[server]]
    value <- list(
      accounting_mode = status$accounting_mode,
      allocator = status$allocator,
      sampler = status$sampler,
      provenance_protocol = status$provenance_protocol,
      person_local_provenance_required =
        status$person_local_provenance_required,
      privacy_guarantee = status$privacy_guarantee,
      bounded_accounting = status$bounded_accounting,
      never_budget_blocked = status$never_budget_blocked,
      total_epsilon = as.numeric(status$total_epsilon),
      total_delta = as.numeric(status$total_delta),
      release_epsilon_max = as.numeric(status$release_epsilon),
      release_delta_max = as.numeric(status$release_delta),
      effective_epsilon = unname(epsilons[[server]]),
      effective_delta = unname(deltas[[server]])
    )
    if ("privacy_epoch" %in% names(status)) {
      value$privacy_epoch <- status$privacy_epoch
    }
    value
  })
  names(accounting) <- names(statuses)
  disjoint <- length(statuses) > 0L && all(vapply(
    statuses, function(status) identical(status$disjoint_persons, TRUE),
    logical(1L)
  ))
  composition <- if (disjoint) "parallel_disjoint_persons" else
    "conservative_sequential_across_sites"
  warnings <- character(0)
  if (any(degraded)) {
    warnings <- c(warnings, paste0(
      "Data-independent degraded DP fallback returned by: ",
      paste(names(degraded)[degraded], collapse = ", "),
      "; pooled utility excludes their protected signal."
    ))
  }
  if (!isTRUE(contract$bounded_accounting)) {
    warnings <- c(warnings, paste0(
      "accounting_mode='sticky_unbounded' prevents rerolls only for an exact ",
      "authenticated canonical lineage and statistic. Alternate equivalent ",
      "query constructions may be distinct releases; this mode does not ",
      "provide finite global DP composition for unlimited distinct queries."
    ))
  }
  if (isTRUE(pool) && length(statuses) > 1L && !disjoint) {
    warnings <- c(warnings, paste0(
      "Servers do not jointly attest disjoint persons. Pooled sufficient ",
      "statistics sum site-local contributions, so a person present at ",
      "multiple sites is counted once per site; privacy accounting is ",
      "conservative sequential composition."
    ))
  }
  if (isTRUE(pool) && length(statuses) > 1L &&
      identical(privacy$statistic, "bounded_distinct")) {
    warnings <- c(warnings, paste0(
      "Pooled bounded_distinct is the sum of noisy site-local cardinalities, ",
      "not the cardinality of the cross-site concept union; use per-site ",
      "results when that additive estimand is not intended."
    ))
  }
  result <- dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(
      call_code = .build_code("ds.omop.dp.release", x = x,
                              privacy = "<omop_privacy>", pool = pool,
                              format = format),
      scope = if (isTRUE(pool)) "pooled" else "per_site",
      warnings = warnings
    )
  )
  result[["meta"]]$privacy <- list(
    statistic = privacy$statistic,
    population_id = population_id,
    snapshot_id = snapshot_ids,
    accounting = accounting,
    format = format,
    mechanism = contract$mechanism,
    sampler = contract$sampler,
    provenance_protocol = contract$provenance_protocol,
    person_local_provenance_required =
      contract$person_local_provenance_required,
    privacy_guarantee = contract$privacy_guarantee,
    adjacency = contract$adjacency,
    accounting_mode = contract$accounting_mode,
    allocator = contract$allocator,
    bounded_accounting = contract$bounded_accounting,
    never_budget_blocked = contract$never_budget_blocked,
    sticky = TRUE,
    degraded = any(degraded),
    per_site_degraded = degraded,
    composition = composition,
    per_site_epsilon = epsilons,
    per_site_delta = deltas,
    total_epsilon_per_site = vapply(
      statuses, `[[`, numeric(1L), "total_epsilon"
    ),
    release_epsilon_max = vapply(
      statuses, `[[`, numeric(1L), "release_epsilon"
    ),
    conservative_epsilon = if (disjoint) max(epsilons) else sum(epsilons),
    conservative_delta = if (disjoint) max(deltas) else sum(deltas),
    global_composition = if (isTRUE(contract$bounded_accounting)) {
      "bounded_nominal_nonblocking_schedule"
    } else {
      "unbounded_across_distinct_semantic_queries"
    }
  )
  result[["meta"]]$harmonization <- harmonization
  result
}
