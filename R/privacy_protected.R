# Module: Protected releases of dsOMOP 2.10.3 servers (client side)
#
# dsOMOP 2.10.3 replaces per-release sticky DP with protected components:
# answers are deterministic in a fixed public basis ("fixed_synopsis": a
# declared bounded vector of an immutable data generation) or in secret
# fingerprints of the realised bounded contributions ("realised_sticky": a
# heuristic disclosure control, not a joint differential-privacy guarantee).
# Each payload carries its displayed values and its signed components; pooling
# sums the signed components of compatible releases and derives the views
# once. The client never relabels a heuristic release as differentially
# private and never pools it with per-release servers.

# Euclidean projection onto {h >= 0, sum(h) = total}; the server's algorithm.
.dp_simplex <- function(y, total) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) return(numeric(0))
  total <- max(0, as.numeric(total))
  if (total == 0) return(rep(0, n))
  sorted <- y[order(-y, seq_len(n), method = "radix")]
  cumulative <- cumsum(sorted)
  candidate <- sorted - (cumulative - total) / seq_len(n)
  rho <- max(which(candidate > 0))
  pmax(y - (cumulative[[rho]] - total) / rho, 0)
}

# Projection of normalized moments onto |S| <= N, 0 <= Q <= N, S^2 <= N Q.
.dp_project_moments <- function(S0, Q0, N) {
  if (!is.finite(N) || N <= 0) return(c(S = 0, Q = 0))
  feasible <- function(S, Q) {
    Q >= 0 && Q <= N + 1e-12 * N && S * S <= N * Q + 1e-9 * N
  }
  if (feasible(S0, Q0)) return(c(S = S0, Q = Q0))
  candidates <- list(c(-N, N), c(N, N), c(0, 0))
  if (abs(S0) <= N) candidates[[length(candidates) + 1L]] <- c(S0, N)
  g <- function(S) (S - S0) + 2 * S * (S * S / N - Q0) / N
  lo <- min(0, S0); hi <- max(0, S0)
  if (g(lo) * g(hi) <= 0) {
    for (i in seq_len(200L)) {
      mid <- (lo + hi) / 2
      if (g(lo) * g(mid) <= 0) hi <- mid else lo <- mid
    }
    sp <- max(-N, min(N, (lo + hi) / 2))
    candidates[[length(candidates) + 1L]] <- c(sp, sp * sp / N)
  }
  ok <- vapply(candidates, function(p) feasible(p[[1L]], p[[2L]]), logical(1L))
  d <- vapply(candidates, function(p) (p[[1L]] - S0)^2 + (p[[2L]] - Q0)^2,
              numeric(1L))
  best <- candidates[[which(ok)[which.min(d[ok])]]]
  c(S = best[[1L]], Q = max(0, min(N, best[[2L]])))
}

.dp_protected_number <- function(value, server, field, nonnegative = FALSE) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || (nonnegative && value < 0)) {
    stop("Server '", server, "' returned an invalid protected field '",
         field, "'.", call. = FALSE)
  }
  as.numeric(value)
}

#' Validate one protected typed payload
#'
#' Checks the protected-release protocol, the route and its guarantee scope,
#' the absence of private material, the signed components, and that the
#' displayed values are post-processing of those components.
#'
#' @keywords internal
.dp_validate_protected_payload <- function(value, server, privacy) {
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      anyDuplicated(names(value))) {
    stop("Server '", server, "' returned a malformed protected release.",
         call. = FALSE)
  }
  forbidden <- c("seed", "nonce", "noise_root", "raw_noise", "fingerprint",
                 "token", "person_token")
  if (any(forbidden %in% names(value))) {
    stop("Server '", server, "' returned forbidden private noise state.",
         call. = FALSE)
  }
  if (!identical(value$protocol, .DP_PROTECTED_RELEASE_PROTOCOL) ||
      !identical(value$statistic, privacy$statistic) ||
      !is.character(value$protection) ||
      !value$protection %in% .DP_PROTECTED_ROUTES) {
    stop("Server '", server, "' returned a protected release that does not ",
         "match the request.", call. = FALSE)
  }
  scope <- if (identical(value$protection, "fixed_synopsis")) {
    "fixed_vector_before_exact_support_gate"
  } else "heuristic_realised_contribution_reuse"
  if (!identical(value$guarantee_scope, scope)) {
    stop("Server '", server, "' returned an incoherent guarantee scope.",
         call. = FALSE)
  }
  if (identical(value$protection, "realised_sticky") &&
      !(is.null(value$mechanism_epsilon) || is.na(value$mechanism_epsilon))) {
    stop("Server '", server, "' claimed a mechanism epsilon for a heuristic ",
         "realised-contribution release.", call. = FALSE)
  }
  if (!identical(value$privacy_call_quota, "none") ||
      !identical(value$history_based_admission, FALSE)) {
    stop("Server '", server, "' returned a history-dependent release.",
         call. = FALSE)
  }
  comps <- value$components
  if (!is.list(comps) || !is.character(comps$coordinates) ||
      !is.numeric(comps$signed) ||
      length(comps$signed) != length(comps$coordinates) ||
      any(!is.finite(comps$signed))) {
    stop("Server '", server, "' returned malformed protected components.",
         call. = FALSE)
  }
  statistic <- privacy$statistic
  if (statistic %in% c("count", "bounded_record_count", "bounded_distinct")) {
    value$noisy_count <- .dp_protected_number(value$noisy_count, server,
                                              "noisy_count", TRUE)
  } else if (statistic %in% c("categorical_histogram", "numeric_histogram")) {
    if (!is.numeric(value$counts) || anyNA(value$counts) ||
        any(value$counts < 0)) {
      stop("Server '", server, "' returned invalid protected counts.",
           call. = FALSE)
    }
    if (identical(statistic, "categorical_histogram") &&
        !identical(value$levels, privacy$levels)) {
      stop("Server '", server, "' returned protected levels that differ ",
           "from the request.", call. = FALSE)
    }
  } else if (identical(statistic, "bounded_mean")) {
    value$noisy_count <- .dp_protected_number(value$noisy_count, server,
                                              "noisy_count", TRUE)
  } else {
    value$noisy_numerator <- .dp_protected_number(
      value$noisy_numerator, server, "noisy_numerator", TRUE)
    value$noisy_denominator <- .dp_protected_number(
      value$noisy_denominator, server, "noisy_denominator", TRUE)
    if (value$noisy_numerator > value$noisy_denominator + 1e-9) {
      stop("Server '", server, "' returned an infeasible binary rate.",
           call. = FALSE)
    }
  }
  value
}

#' Pool compatible protected typed releases
#'
#' Sums the signed components of every site (same owner, families and
#' coordinates) and derives the pooled view once: person totals are clipped at
#' zero, partition cells projected onto the pooled total, weighted cells
#' clipped, bounded moments projected. A distinct-label cardinality is not
#' additive across sites and is not pooled.
#'
#' @keywords internal
.dp_pool_protected <- function(per_site, privacy) {
  first <- per_site[[1L]]$components
  for (site in names(per_site)) {
    comps <- per_site[[site]]$components
    if (!identical(comps$owner, first$owner) ||
        !identical(comps$coordinates, first$coordinates) ||
        !identical(comps$families, first$families)) {
      stop("Protected releases of server '", site, "' are not compatible ",
           "with the other sites (different owner, grid or domain).",
           call. = FALSE)
    }
  }
  statistic <- privacy$statistic
  if (identical(statistic, "bounded_distinct")) {
    stop("A distinct-label cardinality is not additive across sites; use ",
         "the per-site results.", call. = FALSE)
  }
  signed <- Reduce(`+`, lapply(per_site, function(p) p$components$signed))
  families <- first$families
  routes <- vapply(per_site, `[[`, character(1L), "protection")
  out <- list(statistic = statistic,
              protection = if (all(routes == "fixed_synopsis"))
                "fixed_synopsis" else "realised_sticky",
              pooling = "sum_of_signed_components_then_derived_once")
  n <- if (identical(families[[1L]], "N")) max(0, signed[[1L]]) else NA_real_
  if (statistic == "count") {
    out$noisy_count <- n
  } else if (statistic == "bounded_record_count") {
    h <- .dp_simplex(signed[-1L], n)
    k <- privacy$max_contributions
    out$noisy_count <- sum(pmin(seq_along(h), k) * h)
  } else if (statistic %in% c("categorical_histogram", "numeric_histogram")) {
    out$counts <- if (identical(families[[1L]], "N")) {
      .dp_simplex(signed[-1L], n)
    } else pmax(0, signed)
    if (identical(statistic, "categorical_histogram")) {
      out$levels <- privacy$levels
    } else {
      out$breaks <- per_site[[1L]]$breaks
    }
  } else if (statistic == "bounded_mean") {
    J <- 65535
    lower <- per_site[[1L]]$lower; upper <- per_site[[1L]]$upper
    out$noisy_count <- n
    out$estimate <- NA_real_
    out$sd <- NA_real_
    if (n >= 1) {
      p <- .dp_project_moments(signed[[2L]] / J, signed[[3L]] / J, n)
      out$estimate <- (lower + upper) / 2 + (upper - lower) / 2 * p[["S"]] / n
      if (n >= 2) {
        out$sd <- sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / n) / (n - 1))) *
          (upper - lower) / 2
      }
    }
    out$lower <- lower; out$upper <- upper
  } else {
    h <- .dp_simplex(signed[-1L], n)
    out$noisy_numerator <- h[[1L]]
    out$noisy_denominator <- n
    out$estimate <- if (n > 0) h[[1L]] / n else NA_real_
  }
  out
}

# The protected-release branch of ds.omop.dp.release().
.dp_protected_release <- function(x, privacy, server_privacy, datasources,
                                  statuses, want_combine, result_type, format,
                                  population_id, type, pool) {
  for (server in names(statuses)) {
    status <- statuses[[server]]
    if (!identical(status$enabled, TRUE) || !identical(status$ready, TRUE)) {
      stop("Server '", server, "' does not provide an enabled, ready ",
           "protected-release service.", call. = FALSE)
    }
    if (!identical(status$privacy_guarantee, .DP_PROTECTED_GUARANTEE) ||
        !identical(status$history_based_admission, FALSE) ||
        !identical(status$privacy_call_quota, "none") ||
        !privacy$statistic %in% (status$supported_statistics %||%
                                   character(0))) {
      stop("Server '", server, "' returned an unsupported protected-release ",
           "contract.", call. = FALSE)
    }
  }
  domains <- vapply(statuses, function(s) s$noise_domain_id %||% "",
                    character(1L))
  if (anyDuplicated(domains[nzchar(domains)])) {
    stop("Selected servers share a DP noise domain; refusing a duplicate ",
         "federated release from one logical privacy node.", call. = FALSE)
  }
  raw <- .dp_complete_aggregate(
    datasources,
    call("omopDpReleaseDS", as.name(x), .ds_encode(server_privacy)),
    "protected release")
  for (server in names(raw)) {
    raw[[server]] <- .dp_validate_protected_payload(raw[[server]], server,
                                                    privacy)
  }
  pooled <- if (want_combine) .dp_pool_protected(raw, privacy) else NULL
  routes <- vapply(raw, `[[`, character(1L), "protection")
  warnings <- character(0)
  if (any(routes == "realised_sticky")) {
    warnings <- c(warnings, paste0(
      "Heuristic realised-contribution protection (",
      paste(names(routes)[routes == "realised_sticky"], collapse = ", "),
      "): equivalent questions return the same answer, but this is not a ",
      "joint differential-privacy guarantee and no pooled epsilon is ",
      "reported."))
  }
  mechanism <- vapply(raw, function(p) {
    as.numeric(p$mechanism_epsilon %||% NA_real_)
  }, numeric(1L))
  result <- dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(
      call_code = .build_code("ds.omop.dp.release", x = x,
                              privacy = "<omop_privacy>",
                              pool = if (is.null(type)) pool else NULL,
                              type = if (is.null(type)) NULL else result_type,
                              format = format),
      scope = if (want_combine) "pooled" else "per_site",
      warnings = warnings))
  all_fixed <- all(routes == "fixed_synopsis")
  result[["meta"]]$privacy <- list(
    statistic = privacy$statistic,
    population_id = population_id,
    contract = .DP_PROTECTED_PRIVACY_CONTRACT,
    protection = routes,
    guarantee_scope = vapply(raw, `[[`, character(1L), "guarantee_scope"),
    noise_reference_epsilon = vapply(raw, function(p) {
      as.numeric(p$noise_reference_epsilon)
    }, numeric(1L)),
    mechanism_epsilon = mechanism,
    # Fixed synopses of disjoint populations compose in parallel (maximum);
    # sites sharing people compose sequentially (sum). Nothing is claimed for
    # heuristic releases.
    pooled_epsilon_disjoint_sites = if (all_fixed) max(mechanism) else NA_real_,
    pooled_epsilon_overlapping_sites = if (all_fixed) sum(mechanism) else
      NA_real_,
    api_transcript_epsilon = NA_real_,
    privacy_call_quota = "none",
    history_based_admission = FALSE,
    snapshot_id = vapply(statuses, `[[`, character(1L), "snapshot_id"))
  .result_type_view(
    result, result_type,
    "No compatible pooled protected release was available.")
}

#' Pool protected analysis-catalog frames from their signed components
#'
#' dsOMOP 2.10.3 protected catalog releases carry a \code{dsomop_release}
#' payload: per owner and stratum the signed noisy person total, histogram
#' cells reconciled to that total, bounded lattice moments, or person-time
#' status and time. Compatible sites (same entry, owners, strata, cells, grids,
#' bounds, caps and allocations) are pooled by summing the signed components;
#' every pooled view is derived once (person totals projected top-down, cells
#' onto their stratum, moments onto their feasible set). Standard and protected
#' frames are never mixed, and nothing is pooled across incompatible grids.
#'
#' @param per_site Named list of released frames.
#' @return List: \code{result} (one row per owner and stratum with the pooled
#'   persons, mean, SD, quantiles, events and time where defined; the pooled
#'   cells as attribute \code{cells}) and \code{warnings}.
#' @keywords internal
.pool_protected_release_frames <- function(per_site) {
  payloads <- lapply(per_site, function(df) {
    attr(df, "dsomop_release", exact = TRUE)
  })
  ok <- vapply(payloads, function(p) {
    is.list(p) && identical(p$protocol, .DP_PROTECTED_RELEASE_PROTOCOL)
  }, logical(1L))
  if (!all(ok)) {
    stop("Protected and standard releases cannot be pooled together: ",
         paste(names(per_site)[!ok], collapse = ", "),
         " returned no protected components.", call. = FALSE)
  }
  signature <- function(p) {
    lapply(p$owners, function(o) {
      list(o$owner, o$family, o$grid, o$bounds, o$cap, o$allocation,
           o$strata_kind, o$cell_keys,
           vapply(o$components, `[[`, character(1L), "stratum"))
    })
  }
  reference <- signature(payloads[[1L]])
  for (site in names(payloads)) {
    if (!identical(signature(payloads[[site]]), reference) ||
        !identical(payloads[[site]]$entry, payloads[[1L]]$entry)) {
      stop("Protected release of server '", site, "' is not compatible with ",
           "the other sites (entry, owner, grid, bounds, strata or cells ",
           "differ).", call. = FALSE)
    }
  }
  J <- 65535
  rows <- list()
  cells <- list()
  for (i in seq_along(payloads[[1L]]$owners)) {
    owners <- lapply(payloads, function(p) p$owners[[i]])
    o <- owners[[1L]]
    sum_field <- function(field, j) {
      Reduce(`+`, lapply(owners, function(x) x$components[[j]][[field]]))
    }
    strata <- vapply(o$components, `[[`, character(1L), "stratum")
    n <- vapply(seq_along(strata), function(j) sum_field("n", j), numeric(1L))
    shown <- if (identical(o$strata_kind, "public") && length(n) > 1L) {
      .dp_simplex(n, max(0, sum(n)))
    } else pmax(0, n)
    frame <- data.frame(owner = o$owner, stratum = strata, persons = shown,
                        stringsAsFactors = FALSE)
    if (identical(o$family, "C6")) {
      status <- lapply(seq_along(strata), function(j) sum_field("status", j))
      time <- vapply(seq_along(strata), function(j) sum_field("time", j),
                     numeric(1L))
      frame$events <- vapply(seq_along(strata), function(j) {
        .dp_simplex(status[[j]], shown[[j]])[[1L]]
      }, numeric(1L))
      frame$person_time <- pmin(pmax(time, 0), shown * o$horizon_days)
      frame$rate <- ifelse(frame$person_time > 0,
                           frame$events / frame$person_time, NA_real_)
    } else {
      if (!is.null(o$components[[1L]]$s) && !is.null(o$bounds)) {
        L <- o$bounds[[1L]]; U <- o$bounds[[2L]]
        frame$mean <- NA_real_; frame$sd <- NA_real_
        for (j in seq_along(strata)) {
          N <- shown[[j]]
          if (N < 2) next
          p <- .dp_project_moments(sum_field("s", j) / J,
                                   sum_field("q", j) / J, N)
          frame$mean[[j]] <- (L + U) / 2 + (U - L) / 2 * p[["S"]] / N
          frame$sd[[j]] <- sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / N) /
                                      (N - 1))) * (U - L) / 2
        }
      }
      if (!is.null(o$components[[1L]]$h)) {
        for (j in seq_along(strata)) {
          h <- .dp_simplex(sum_field("h", j), shown[[j]])
          cells[[length(cells) + 1L]] <- data.frame(
            owner = o$owner, stratum = strata[[j]],
            cell = o$cell_keys %||% seq_along(h), count = h,
            stringsAsFactors = FALSE)
        }
      }
    }
    rows[[length(rows) + 1L]] <- frame
  }
  result <- do.call(rbind, lapply(rows, function(r) {
    for (col in c("mean", "sd", "events", "person_time", "rate")) {
      if (!col %in% names(r)) r[[col]] <- NA_real_
    }
    r[, c("owner", "stratum", "persons", "mean", "sd", "events",
          "person_time", "rate")]
  }))
  attr(result, "cells") <- if (length(cells)) do.call(rbind, cells) else NULL
  routes <- vapply(payloads, `[[`, character(1L), "protection")
  warnings <- paste0(
    "Pooled protected components: per owner and stratum the signed noisy ",
    "components of every site were summed and the views derived once.")
  if (any(routes != "fixed_synopsis")) {
    warnings <- c(warnings, paste0(
      "At least one site used heuristic realised-contribution protection; ",
      "no pooled epsilon is reported."))
  }
  list(result = result, warnings = warnings)
}
