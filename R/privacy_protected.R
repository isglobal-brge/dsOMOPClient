# Module: Protected releases of dsOMOP 2.10.3 servers (client side)
#
# dsOMOP 2.10.3 replaces per-release sticky DP with protected components:
# answers are deterministic in a fixed public basis ("fixed_synopsis": a
# declared bounded vector before exact support gates) or in secret
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

# Projection of normalized moments onto |S| <= N, 0 <= Q <= N, S^2 <= N Q;
# the server's algorithm, with its candidate order (equal distances resolve
# to the same point on both sides).
.dp_project_moments <- function(S0, Q0, N) {
  if (!is.finite(N) || N <= 0) return(c(S = 0, Q = 0))
  feasible <- function(S, Q) {
    Q >= 0 && Q <= N + 1e-12 * N && S * S <= N * Q + 1e-9 * N
  }
  if (feasible(S0, Q0)) return(c(S = S0, Q = Q0))
  candidates <- list(c(-N, N), c(N, N))
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
  candidates[[length(candidates) + 1L]] <- c(0, 0)
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

# Public opaque identifier of one logical release source (dsOMOP >= 2.10.3):
# two connections to one logical node carry the same identifier.
.DP_PROTECTED_SOURCE_PATTERN <- "^dsl_[0-9a-f]{40}$"

# Guarantee scope of each protected route (dsOMOP 2.10.3).
.DP_PROTECTED_SCOPES <- c(
  fixed_synopsis = "fixed_vector_before_exact_support_gate",
  realised_sticky = "heuristic_realised_contribution_reuse"
)

#' Check the guarantee claims of a protected release or protection label
#'
#' One coherence check for typed payloads and catalogue protection labels
#' (B14). A fixed synopsis declares its guarantee scope
#' (\code{fixed_vector_before_exact_support_gate}), the finite epsilon of its
#' bounded vector and delta 0 (before the exact support gates). A heuristic
#' realised-contribution release declares the heuristic scope and claims no
#' finite mechanism epsilon and no delta. A mixed frame names the protection
#' of each block: exactly one protected route among its blocks, whose scope
#' the frame declares, and no frame-level epsilon or delta. Standard,
#' standard-fallback and unavailable labels claim no protected scope, epsilon
#' or delta. No route claims a finite epsilon for the whole API transcript. A
#' missing or contradictory descriptor is refused rather than displayed.
#'
#' @param value Payload or \code{dsomop_protection} label.
#' @param server Server name (for messages).
#' @param what Description of \code{value} (for messages).
#' @return \code{value}, invisibly.
#' @keywords internal
.dp_check_protected_claims <- function(value, server, what = "release") {
  present <- function(x) !is.null(x) && !(length(x) == 1L && is.atomic(x) &&
                                            is.na(x))
  finite_number <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  }
  fail <- function(...) {
    stop("Server '", server, "' returned a protected ", what, " ", ...,
         call. = FALSE)
  }
  if (present(value$api_transcript_epsilon)) {
    fail("that claims an epsilon for the whole API transcript.")
  }
  route <- value$protection
  scope <- value$guarantee_scope
  scope_ok <- function(expected) {
    is.character(scope) && length(scope) == 1L && !is.na(scope) &&
      identical(unname(scope), unname(expected))
  }
  if (is.character(route) && length(route) == 1L &&
      route %in% names(.DP_PROTECTED_SCOPES) &&
      !scope_ok(.DP_PROTECTED_SCOPES[[route]])) {
    fail("whose guarantee scope is missing or incoherent with its ", route,
         " route.")
  }
  if (identical(route, "mixed")) {
    blocks <- value$blocks
    routes <- if (is.list(blocks) && length(blocks)) {
      unique(unlist(lapply(blocks, function(b) {
        if (is.character(b) && length(b) == 1L &&
            b %in% names(.DP_PROTECTED_SCOPES)) b else NULL
      }), use.names = FALSE))
    } else character(0)
    if (length(routes) != 1L) {
      fail("that is mixed without exactly one protected block route.")
    }
    if (!scope_ok(.DP_PROTECTED_SCOPES[[routes]])) {
      fail("whose guarantee scope is missing or incoherent with its ",
           routes, " block.")
    }
  }
  if (is.character(route) && length(route) == 1L &&
      route %in% c("standard", "standard_fallback", "unavailable")) {
    if (is.character(scope) && any(scope %in% .DP_PROTECTED_SCOPES)) {
      fail("that is ", route, " but claims a protected guarantee scope.")
    }
    if (present(value$mechanism_epsilon) || present(value$delta)) {
      fail("that is ", route, " but claims a mechanism epsilon or delta.")
    }
  }
  if (identical(route, "fixed_synopsis")) {
    if (!finite_number(value$mechanism_epsilon) ||
        value$mechanism_epsilon <= 0) {
      fail("that declares a fixed synopsis without a finite positive ",
           "mechanism epsilon.")
    }
    if (!finite_number(value$delta) || !identical(as.numeric(value$delta), 0)) {
      fail("that declares a fixed synopsis without delta 0.")
    }
  } else if (is.character(route) && length(route) == 1L &&
             route %in% c("realised_sticky", "mixed")) {
    if (present(value$mechanism_epsilon)) {
      fail("that claims a mechanism epsilon for a heuristic ", route,
           " release.")
    }
    if (present(value$delta)) {
      fail("that claims a delta for a heuristic ", route, " release.")
    }
  }
  invisible(value)
}

#' Require one distinct logical source per site before pooling
#'
#' Pooling sums the signed components of every site, so a site counted twice
#' (two connections to one logical node) would double its contribution. Every
#' site must report its public logical source identifier and all identifiers
#' must differ; otherwise pooling is refused (the per-site results remain
#' available without pooling). Equal numeric answers never establish or refute
#' source identity. One site has nothing to pool against.
#'
#' @param payloads Named list of payloads (typed releases or catalogue
#'   \code{dsomop_release} attributes).
#' @param what Description for messages.
#' @return The identifiers, invisibly.
#' @keywords internal
.dp_protected_sources <- function(payloads, what = "protected releases") {
  if (length(payloads) < 2L) return(invisible(NULL))
  ids <- vapply(payloads, function(p) {
    id <- if (is.list(p) && is.list(p$source)) p$source$logical_source_id
    if (is.character(id) && length(id) == 1L && !is.na(id) &&
        grepl(.DP_PROTECTED_SOURCE_PATTERN, id)) id else NA_character_
  }, character(1L))
  if (anyNA(ids)) {
    stop("The ", what, " cannot be pooled: server(s) ",
         paste(names(payloads)[is.na(ids)], collapse = ", "),
         " report no logical source identifier, so the independence of the ",
         "sites cannot be established. Use the per-site results.",
         call. = FALSE)
  }
  repeated <- duplicated(ids) | duplicated(ids, fromLast = TRUE)
  if (any(repeated)) {
    groups <- split(names(payloads)[repeated], ids[repeated])
    stop("Duplicate logical source: servers ",
         paste(vapply(groups, paste, character(1L), collapse = " and "),
               collapse = "; "),
         " are connections to the same logical node. Pooling their ", what,
         " would count that node more than once; use the per-site results.",
         call. = FALSE)
  }
  invisible(ids)
}

#' Validate one protected typed payload
#'
#' Checks the protected-release protocol, route and guarantee scope,
#' forbidden top-level private-state fields, signed-component and public-basis
#' structure, view masks, and basic feasibility of displayed counts and rates.
#' It does not recompute displayed values from the signed basis; pooling
#' derives its own display from compatible signed components.
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
      !is.character(value$protection) || length(value$protection) != 1L ||
      is.na(value$protection) ||
      !value$protection %in% .DP_PROTECTED_ROUTES) {
    stop("Server '", server, "' returned a protected release that does not ",
         "match the request.", call. = FALSE)
  }
  # Route, guarantee scope, epsilon and delta coherence (shared with the
  # catalogue protection labels, B14).
  .dp_check_protected_claims(value, server)
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
  # The complete basis, its versioned projection and the view masks are what
  # pooling derives every view from (R04/R05).
  basis <- comps$basis
  ncells <- if (is.list(basis) && is.numeric(basis$signed)) {
    length(basis$signed) - as.integer(length(basis$families) > 0L &&
                                        basis$families[[1L]] %in% c("N", "T"))
  } else NA_integer_
  valid_view <- is.null(comps$view_of_basis) ||
    (is.list(comps$view_of_basis) && all(vapply(comps$view_of_basis,
      function(i) is.numeric(i) && !anyNA(i) && all(is.finite(i)) &&
        !anyDuplicated(i) && all(i == floor(i)) && all(i >= 1) &&
        (is.na(ncells) || all(i <= ncells)), logical(1L))))
  if (!is.list(basis) || !is.character(basis$id) || length(basis$id) != 1L ||
      is.na(basis$id) || !nzchar(basis$id) ||
      !is.character(basis$coordinates) || !is.numeric(basis$signed) ||
      !length(basis$signed) || anyNA(basis$coordinates) ||
      !is.character(basis$families) || anyNA(basis$families) ||
      length(basis$families) != length(basis$signed) ||
      length(basis$signed) != length(basis$coordinates) ||
      anyDuplicated(data.frame(family = basis$families,
                               coordinate = basis$coordinates)) ||
      any(!is.finite(basis$signed)) ||
      !is.character(comps$projection) || length(comps$projection) != 1L ||
      is.na(comps$projection) ||
      !comps$projection %in% .DP_TYPED_PROJECTIONS || !valid_view) {
    stop("Server '", server, "' returned protected components without a ",
         "complete basis, a known projection or a valid view.", call. = FALSE)
  }
  expected_lead <- switch(comps$projection,
    "typed-partition-simplex-v1" = "N",
    "typed-weighted-total-simplex-v1" = "T", NULL)
  if ((!is.null(expected_lead) &&
       (!identical(basis$families[[1L]], expected_lead) || ncells < 1L)) ||
      (identical(comps$projection, "typed-weighted-atomwise-clip-v1") &&
       any(basis$families %in% c("N", "T")))) {
    stop("Server '", server, "' returned a projection incompatible with its ",
         "complete basis total.", call. = FALSE)
  }
  if (identical(privacy$statistic, "binary_rate") &&
      (!identical(comps$view_kind, "binary_rate_numerator_denominator") ||
       length(comps$view_of_basis) != 2L ||
       !identical(comps$projection, "typed-partition-simplex-v1") ||
       !all(comps$view_of_basis[[1L]] %in% comps$view_of_basis[[2L]]))) {
    stop("Server '", server, "' returned a binary rate without its ",
         "numerator and denominator masks.", call. = FALSE)
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

# Versioned complete-basis projections of dsOMOP 2.10.3 typed releases, the
# server's rule (R05): a person partition onto the simplex of its person
# total; capped weighted records onto the simplex of their protected weighted
# total; multi-label presence cells clipped at zero one by one.
.DP_TYPED_PROJECTIONS <- c(
  "typed-partition-simplex-v1", "typed-weighted-total-simplex-v1",
  "typed-weighted-atomwise-clip-v1", "none"
)

.dp_typed_project <- function(projection, total, cells) {
  switch(projection,
    "typed-partition-simplex-v1" = ,
    "typed-weighted-total-simplex-v1" = .dp_simplex(cells, max(0, total)),
    "typed-weighted-atomwise-clip-v1" = pmax(0, cells),
    stop("Unknown typed projection '", projection, "'.", call. = FALSE))
}

# The public basis/view description of a typed payload that every pooled
# site must share: owner, complete basis (identity, registered domain,
# coordinates, families), projection, view kind and view masks, effective
# breaks.
.dp_typed_semantics <- function(p) {
  comps <- p$components
  b <- comps$basis %||% list()
  list(owner = comps$owner, family = comps$family,
       coordinates = comps$coordinates, families = comps$families,
       basis_id = b$id, domain = b$domain,
       basis_coordinates = b$coordinates, basis_families = b$families,
       projection = comps$projection, view_kind = comps$view_kind,
       view_of_basis = comps$view_of_basis, basis_id_top = p$basis_id,
       breaks = p$breaks)
}

#' Pool compatible protected typed releases
#'
#' Every site must report the same complete public basis (owner, basis
#' identity including the registered domain labels, coordinates, families),
#' the same versioned projection and the same view (kind and public masks of
#' basis cells), and be a distinct logical source
#' (\code{.dp_protected_sources}). The signed basis components are summed
#' once, the complete basis is projected once with the server's rule, and
#' every view is derived from those projected cells: histogram bins are sums
#' of cells; a binary rate's numerator and denominator are the sums of its
#' numerator and denominator masks (R04); a one-site pool therefore equals the
#' site's displayed values. A distinct-label cardinality is not additive across
#' sites and is not pooled.
#'
#' @keywords internal
.dp_pool_protected <- function(per_site, privacy) {
  .dp_protected_sources(per_site, "protected releases")
  first <- per_site[[1L]]
  reference <- .dp_typed_semantics(first)
  for (site in names(per_site)) {
    if (!identical(.dp_typed_semantics(per_site[[site]]), reference)) {
      stop("Protected releases of server '", site, "' are not compatible ",
           "with the other sites (different owner, basis, registered domain, ",
           "projection, view or grid).", call. = FALSE)
    }
  }
  statistic <- privacy$statistic
  if (identical(statistic, "bounded_distinct")) {
    stop("A distinct-label cardinality is not additive across sites; use ",
         "the per-site results.", call. = FALSE)
  }
  projection <- reference$projection %||% "none"
  if (!projection %in% .DP_TYPED_PROJECTIONS) {
    stop("The protected releases use a basis projection ('", projection,
         "') this dsOMOPClient does not implement; upgrade dsOMOPClient.",
         call. = FALSE)
  }
  basis_signed <- function(p) {
    p$components$basis$signed %||% p$components$signed
  }
  pooled <- Reduce(`+`, lapply(per_site, basis_signed))
  families <- reference$basis_families %||% reference$families
  routes <- vapply(per_site, `[[`, character(1L), "protection")
  out <- list(statistic = statistic,
              protection = if (all(routes == "fixed_synopsis"))
                "fixed_synopsis" else "realised_sticky",
              pooling = "sum_of_signed_basis_components_then_derived_once",
              projection = projection)
  lead <- length(families) > 0L && families[[1L]] %in% c("N", "T")
  total <- if (lead) pooled[[1L]] else NA_real_
  n <- if (lead && identical(families[[1L]], "N")) max(0, total) else NA_real_
  cells <- if (lead) pooled[-1L] else pooled
  view <- reference$view_of_basis
  projected <- function() {
    if (length(cells) == 0L) return(numeric(0))
    .dp_typed_project(projection, total, cells)
  }
  view_counts <- function(h) {
    if (is.null(view)) h else vapply(view, function(i) sum(h[i]), numeric(1L))
  }
  if (statistic == "count") {
    out$noisy_count <- n
  } else if (statistic == "bounded_record_count") {
    h <- projected()
    k <- privacy$max_contributions
    out$noisy_count <- sum(pmin(seq_along(h), k) * h)
  } else if (statistic %in% c("categorical_histogram", "numeric_histogram")) {
    out$counts <- view_counts(projected())
    if (identical(statistic, "categorical_histogram")) {
      out$levels <- first$levels %||% privacy$levels
    } else {
      out$breaks <- first$breaks
    }
  } else if (statistic == "bounded_mean") {
    J <- 65535
    lower <- first$lower; upper <- first$upper
    out$noisy_count <- n
    out$estimate <- NA_real_
    out$sd <- NA_real_
    if (n >= 1) {
      p <- .dp_project_moments(pooled[[2L]] / J, pooled[[3L]] / J, n)
      out$estimate <- (lower + upper) / 2 + (upper - lower) / 2 * p[["S"]] / n
      if (n >= 2) {
        out$sd <- sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / n) / (n - 1))) *
          (upper - lower) / 2
      }
    }
    out$lower <- lower; out$upper <- upper
  } else {
    # Binary rate: numerator and denominator masks of the one projected basis.
    if (!identical(reference$view_kind, "binary_rate_numerator_denominator") ||
        !is.list(view) || length(view) != 2L) {
      stop("The protected binary releases carry no numerator/denominator ",
           "view of their basis; they cannot be pooled.", call. = FALSE)
    }
    v <- view_counts(projected())
    out$noisy_numerator <- v[[1L]]
    out$noisy_denominator <- v[[2L]]
    out$estimate <- if (v[[2L]] > 0) v[[1L]] / v[[2L]] else NA_real_
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
    logical_source_id = vapply(raw, function(p) {
      id <- if (is.list(p$source)) p$source$logical_source_id
      if (is.character(id) && length(id) == 1L) id else NA_character_
    }, character(1L)),
    snapshot_id = vapply(statuses, function(s) {
      label <- s$snapshot_id
      if (is.character(label) && length(label) == 1L && !is.na(label)) {
        label
      } else NA_character_
    }, character(1L)))
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
#' frames are never mixed, nothing is pooled across incompatible grids, and
#' every site must be a distinct logical source: two connections to one
#' logical node are refused instead of being counted twice.
#'
#' Complete person-time hierarchies are summed and projected from the root
#' down; public view masks select sums of those projected nodes. Nested
#' Achilles histograms use the parent's protected cells as their second
#' margin. First-event and capped recurrent event-time hierarchies project
#' their signed A/B/C nodes from the person or event total down before
#' selecting day, month or year views. Recurrent persons use a separate complete
#' interval-presence vector for distinct contributors among the same K-capped
#' occurrences. Its versioned radial projection enforces public feasibility;
#' retreat toward the event-mass anchor can add bias. Coarse person counts are
#' selected directly, never summed from fine bins.
#'
#' Record-weighted releases (family \code{RM}) are pooled per stratum by
#' \code{.pool_protected_record_moments}. Exit-time x status cells of a
#' grouped-time Cox release (family \code{C8}) are summed and projected onto
#' their pooled C6 event/non-event parents. Interaction C6/C8 exports retain
#' the complete signed atomic family, projected before view coarsening;
#' supplemental rows and cells retain family and view identifiers. C8 uses
#' finite IPF with a feasible margin repair, not an exact KL/L2 claim. Cox
#' cells are never refitted; the event-weighted q x exposure cells of
#' an SCCS release (\code{w}) are not pooled (only its case total).
#'
#' @param per_site Named list of released frames.
#' @param policy \code{"strict"} or \code{"pooled_only_ok"} (record-weighted
#'   strata present on only some servers).
#' @param columns The entry's contracted columns (record-weighted schema).
#' @return List: \code{result} (one row per owner, stratum and interaction view with the pooled
#'   persons, mean, SD, quantiles, events and time where defined; the pooled
#'   cells as attribute \code{cells}) and \code{warnings}.
#' @keywords internal
.pool_protected_release_frames <- function(per_site, policy = "strict",
                                           columns = NULL) {
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
  .dp_protected_sources(payloads, "protected catalogue releases")
  families <- unique(unlist(lapply(payloads, function(p) {
    vapply(p$owners, function(o) as.character(o$family %||% NA_character_),
           character(1L))
  })))
  if ("RM" %in% families) {
    if (!identical(families, "RM")) {
      stop("A record-weighted protected release cannot be pooled with other ",
           "protected components.", call. = FALSE)
    }
    return(.pool_protected_record_moments(payloads, policy = policy,
                                          columns = columns))
  }
  if ("TTE" %in% families) {
    if (!identical(families, "TTE")) {
      stop("Time-to-event hierarchies cannot pool with other protected families.",
           call. = FALSE)
    }
    return(.pool_protected_tte(payloads, columns = columns))
  }
  signature <- function(p) {
    lapply(p$owners, function(o) {
      list(o$owner, o$family, o$grid, o$bounds, o$cap, o$allocation,
           o$strata_kind, o$cell_keys,
           o$horizon_days, o$recurrent_cap,
           .dp_c6_hierarchy_signature(o),
           .dp_survival_signature(o),
           if (!is.null(o$parent_basis)) signature(list(owners = list(o$parent_basis))),
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
    if (identical(o$family, "C8") || !is.null(o$family_basis) ||
        (identical(o$family, "C6") &&
          (!is.null(o$parent_owner) || !is.null(o$parent)))) {
      displayed <- .dp_pool_survival_owner(owners)
      rows[[length(rows) + 1L]] <- displayed$frame
      if (!is.null(displayed$cells)) cells[[length(cells) + 1L]] <- displayed$cells
      next
    }
    if (identical(o$family, "C6") && !is.null(o$hierarchy)) {
      rows[[length(rows) + 1L]] <- .dp_pool_c6_hierarchy(owners)
      next
    }
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
        parent_cells <- NULL
        if (!is.null(o$parent_basis)) {
          parents <- lapply(owners, `[[`, "parent_basis")
          if (any(vapply(parents, function(p) length(p$components) != 1L ||
                          is.null(p$components[[1L]]$h), logical(1L)))) {
            stop("A nested histogram lacks its root's protected basis.",
                 call. = FALSE)
          }
          pn <- sum(vapply(parents, function(p) p$components[[1L]]$n,
                            numeric(1L)))
          ph <- Reduce(`+`, lapply(parents, function(p) p$components[[1L]]$h))
          target <- .dp_simplex(ph, max(0, pn))
          X <- do.call(rbind, lapply(seq_along(strata), function(j) sum_field("h", j)))
          if (ncol(X) != length(target)) {
            stop("A nested histogram has an incompatible parent basis.", call. = FALSE)
          }
          parent_cells <- .dp_transport(X, shown, target)
        }
        for (j in seq_along(strata)) {
          h <- if (is.null(parent_cells)) .dp_simplex(sum_field("h", j), shown[[j]]) else
            parent_cells[j, ]
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
    for (col in c("family", "view", "subgroup_kind")) {
      if (!col %in% names(r)) r[[col]] <- NA_character_
    }
    for (col in c("mean", "sd", "events", "person_time", "rate", "recurrent")) {
      if (!col %in% names(r)) r[[col]] <- NA_real_
    }
    r[, c("owner", "stratum", "persons", "mean", "sd", "events",
          "person_time", "rate", "recurrent", "family", "view", "subgroup_kind")]
  }))
  attr(result, "cells") <- if (length(cells)) do.call(rbind, lapply(cells, function(x) {
    for (col in c("family", "view", "subgroup_kind")) {
      if (!col %in% names(x)) x[[col]] <- NA_character_
    }
    x
  })) else NULL
  routes <- vapply(payloads, `[[`, character(1L), "protection")
  warnings <- paste0(
    "Pooled protected components: per owner and stratum the signed noisy ",
    "components of every site were summed and the views derived once.")
  if ("C8" %in% families) {
    warnings <- c(warnings, paste0(
      "Grouped-time Cox exit-time x status cells (C8) were summed across ",
      "sites and projected onto their pooled C6 event/no-event parents; ",
      "interaction views retain complete atomic family margins. No hazard ratio is ",
      "refitted on them."))
  }
  if ("SCCS" %in% families) {
    warnings <- c(warnings, paste0(
      "The event-weighted SCCS q x exposure cells are not pooled; only the ",
      "case totals are."))
  }
  if (any(routes != "fixed_synopsis")) {
    warnings <- c(warnings, paste0(
      "At least one site used heuristic realised-contribution protection; ",
      "no pooled epsilon is reported."))
  }
  list(result = result, warnings = warnings)
}

# Interval views of a pooled record-value histogram. The public grid has an
# explicit open first/last bin, and tail quantiles report the finite edge
# plus censoring flags, exactly like the server.
.dp_record_histogram_views <- function(h, breaks, quantiles = TRUE) {
  upper <- c(breaks[-1L], NA_real_)
  lower <- c(-Inf, breaks[-1L])
  fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
  labels <- ifelse(is.na(upper), paste0("[", fmt(breaks), ", Inf)"),
                   paste0("[", fmt(breaks), ", ", fmt(upper), ")"))
  labels[[1L]] <- paste0("(-Inf, ", fmt(upper[[1L]]), ")")
  bins <- data.frame(bin = seq_along(h), bin_label = labels,
                     bin_lower = lower, bin_upper = upper,
                     records = signif(h, 12), stringsAsFactors = FALSE)
  q <- NULL
  if (quantiles && sum(h) > 0) {
    probs <- c(.1, .25, .5, .75, .9)
    cdf <- cumsum(h) / sum(h)
    index <- vapply(probs, function(p) which(cdf >= p - 1e-12)[[1L]],
                    integer(1L))
    values <- vapply(seq_along(probs), function(i) {
      b <- index[[i]]
      if (b == 1L) return(upper[[1L]])
      if (is.na(upper[[b]])) return(breaks[[b]])
      before <- if (b > 1L) cdf[[b - 1L]] else 0
      width <- cdf[[b]] - before
      fraction <- if (width > 0) (probs[[i]] - before) / width else 0
      breaks[[b]] + fraction * (upper[[b]] - breaks[[b]])
    }, numeric(1L))
    open_lower <- is.infinite(lower[index])
    open_upper <- is.na(upper[index])
    q <- data.frame(prob = probs, value = signif(values, 12),
                    bin_lower = lower[index],
                    bin_upper = ifelse(open_upper, Inf, upper[index]),
                    open_lower = open_lower, open_upper = open_upper,
                    censored = open_lower | open_upper)
  }
  list(bins = bins, quantiles = q)
}

#' Pool record-weighted protected releases (capped record moments)
#'
#' dsOMOP 2.10.3 releases the record-weighted QueryLibrary summaries as one
#' record-moment owner (family \code{RM}): per stratum the signed person
#' total N, the capped record count R and, on a public scale, the lattice sums
#' S and Q of the clipped, normalized record values. Compatible servers (same
#' entry, owner, cap, record order, lattice, normalization and allocation,
#' distinct logical sources) are pooled per stratum by summing N, R, S and Q;
#' the record-weighted mean and SD are derived once from the pooled R, S and Q
#' exactly as the server derives them (the persons never weight the mean).
#' Moments are pooled only when every contributing server released them on
#' the same public scale. Under \code{"strict"} only strata released by every
#' server are pooled; \code{"pooled_only_ok"} also pools strata released by
#' some servers (sums over those servers).
#'
#' The versioned \code{record-display-v1} projection holds displayed R in
#' [N, K N] before deriving moments. Compatible signed value histograms are
#' summed, then projected onto that displayed R. The result attributes
#' \code{value_histograms} and \code{value_quantiles} contain their cells and
#' interval quantiles, including open-tail censoring flags. A stratum's
#' histogram is available only if every contributing site released it on the
#' same public grid; missing moments never cause an exact-value fallback.
#'
#' @param payloads Named list of \code{dsomop_release} payloads.
#' @param policy \code{"strict"} or \code{"pooled_only_ok"}.
#' @param columns Optional contracted columns (their names are the schema).
#' @return List: \code{result} (the summary's own columns) and
#'   \code{warnings}.
#' @keywords internal
.pool_protected_record_moments <- function(payloads, policy = "strict",
                                           columns = NULL) {
  policy <- match.arg(policy, c("strict", "pooled_only_ok"))
  sites <- names(payloads)
  owners <- lapply(payloads, function(p) p$owners)
  if (any(lengths(owners) != 1L)) {
    stop("A record-weighted protected release must carry exactly one ",
         "record-moment owner.", call. = FALSE)
  }
  os <- lapply(owners, `[[`, 1L)
  o <- os[[1L]]
  display <- o$display
  if (!is.list(display) || !is.character(display$columns)) {
    stop("Server '", sites[[1L]], "' returned a record-weighted release ",
         "without its pooling fields; use the per-site results.",
         call. = FALSE)
  }
  signature <- function(x) {
    list(x$owner, x$population, x$family, x$weighting, x$cap, x$cap_scope,
         x$record_order, x$lattice, x$normalization, x$allocation,
         x$strata_kind, x$display, x$display_projection, x$distribution_floor)
  }
  projection <- o$display_projection
  if (!is.null(projection) && !identical(projection, "record-display-v1")) {
    stop("Unknown protected record display projection; upgrade dsOMOPClient.",
         call. = FALSE)
  }
  cap <- .dp_protected_number(o$cap, sites[[1L]], "cap", TRUE)
  if (cap < 1 || cap != floor(cap)) stop("Invalid protected record cap.",
                                       call. = FALSE)
  floor <- .dp_protected_number(o$distribution_floor %||% 2,
                                 sites[[1L]], "distribution_floor", TRUE)
  floor <- max(2, floor)
  histogram_owners <- Filter(function(x) !is.null(x$value_histogram), os)
  histogram <- NULL
  if (length(histogram_owners)) {
    h_signature <- function(x) list(x$value_histogram, x$value_grid,
      x$value_breaks, x$value_cell_keys, x$value_reconciliation)
    histogram <- histogram_owners[[1L]]
    breaks <- histogram$value_breaks
    if (!identical(histogram$value_histogram, "record-value-histogram-v1") ||
        !is.numeric(breaks) || length(breaks) < 2L ||
        any(!is.finite(breaks)) || any(diff(breaks) <= 0) ||
        !is.character(histogram$value_cell_keys) ||
        length(histogram$value_cell_keys) != length(breaks) ||
        anyNA(histogram$value_cell_keys) ||
        anyDuplicated(histogram$value_cell_keys) ||
        !identical(histogram$value_reconciliation,
                    "cells_sum_to_signed_record_total_R") ||
        any(!vapply(histogram_owners, function(x) {
          identical(h_signature(x), h_signature(histogram))
        }, logical(1L)))) {
      stop("Protected record value histograms have an unknown or incompatible ",
           "public basis.", call. = FALSE)
    }
  }
  for (site in sites) {
    if (!identical(signature(os[[site]]), signature(o)) ||
        !identical(payloads[[site]]$entry, payloads[[1L]]$entry)) {
      stop("Protected record-weighted release of server '", site, "' is not ",
           "compatible with the other sites (entry, owner, cap, record ",
           "order, lattice, allocation or display differ).", call. = FALSE)
    }
  }
  if (!is.null(columns) && !identical(names(columns), display$columns)) {
    stop("The record-weighted release does not match its contracted ",
         "columns.", call. = FALSE)
  }
  J <- as.numeric(o$lattice)
  with_q <- !is.null(o$allocation$Q)
  by_site <- lapply(sites, function(site) {
    comps <- os[[site]]$components
    keys <- vapply(comps, function(c) as.character(c$stratum), character(1L))
    if (anyNA(keys) || anyDuplicated(keys)) {
      stop("Server '", site, "' returned malformed record-weighted strata.",
           call. = FALSE)
    }
    stats::setNames(comps, keys)
  })
  names(by_site) <- sites
  keys <- lapply(by_site, names)
  strata <- if (identical(policy, "strict")) {
    Reduce(intersect, keys)
  } else unique(unlist(keys, use.names = FALSE))
  partial <- FALSE
  no_moments <- character(0)
  no_histograms <- character(0)
  conflicts <- character(0)
  rows <- lapply(strata, function(key) {
    present <- sites[vapply(by_site, function(b) key %in% names(b),
                            logical(1L))]
    if (length(present) < length(sites)) partial <<- TRUE
    comps <- lapply(present, function(site) by_site[[site]][[key]])
    names(comps) <- present
    total <- function(field, nonnegative = FALSE) {
      sum(vapply(present, function(site) {
        .dp_protected_number(comps[[site]][[field]], site, field)
      }, numeric(1L)))
    }
    N <- total("n")
    R <- total("r")
    moments <- all(vapply(comps, function(c) {
      !is.null(c$s) && (!with_q || !is.null(c$q)) && !is.null(c$scale)
    }, logical(1L)))
    if (moments) {
      scales <- unique(lapply(comps, function(c) {
        list(c$scale, as.numeric(c$bounds))
      }))
      moments <- length(scales) == 1L
    }
    shown_n <- max(0, N)
    shown_r <- if (identical(projection, "record-display-v1")) {
      min(cap * shown_n, max(shown_n, R))
    } else max(0, R)
    mean <- sd <- NA_real_
    if (!moments) {
      no_moments <<- c(no_moments, key)
    } else if (shown_n >= floor && shown_r >= 2) {
      bounds <- as.numeric(comps[[1L]]$bounds)
      L <- bounds[[1L]]; U <- bounds[[2L]]
      Su0 <- total("s") / J
      if (with_q) {
        Qu0 <- total("q") / J
        p <- .dp_project_moments(2 * Su0 - shown_r, 4 * Qu0 - 4 * Su0 + shown_r,
                                 shown_r)
        mean <- (L + U) / 2 + (U - L) / 2 * p[["S"]] / shown_r
        sd <- (U - L) / 2 *
          sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / shown_r) / (shown_r - 1)))
      } else {
        mean <- L + (U - L) * min(shown_r, max(0, Su0)) / shown_r
      }
    }
    hist <- NULL
    has_h <- vapply(comps, function(c) !is.null(c$h), logical(1L))
    if (any(has_h)) {
      if (is.null(histogram)) {
        stop("Protected record cells lack their public histogram basis.",
             call. = FALSE)
      }
      for (site in present[has_h]) {
        h <- comps[[site]]$h
        if (is.null(os[[site]]$value_histogram) || !is.numeric(h) ||
            length(h) != length(histogram$value_breaks) ||
            any(!is.finite(h))) {
          stop("Server '", site, "' returned malformed protected record cells.",
               call. = FALSE)
        }
      }
      if (all(has_h)) {
        h <- Reduce(`+`, lapply(comps, `[[`, "h"))
        hist <- .dp_record_histogram_views(
          .dp_simplex(h, shown_r), histogram$value_breaks,
          quantiles = shown_n >= floor)
      } else no_histograms <<- c(no_histograms, key)
    }
    values <- comps[[1L]]$display %||% list()
    for (site in present[-1L]) {
      other <- comps[[site]]$display %||% list()
      for (col in names(values)) {
        if (!identical(values[[col]], other[[col]])) {
          conflicts <<- c(conflicts, col)
          values[col] <- list(NA)
        }
      }
    }
    row <- lapply(display$columns, function(col) {
      if (identical(col, display$persons)) return(signif(shown_n, 12))
      if (identical(col, display$records)) return(signif(shown_r, 12))
      if (identical(col, display$mean)) return(signif(mean, 12))
      if (identical(col, display$sd)) return(signif(sd, 12))
      v <- values[[col]]
      if (is.null(v)) NA else v
    })
    names(row) <- display$columns
    list(row = as.data.frame(row, stringsAsFactors = FALSE, optional = TRUE),
         order_value = suppressWarnings(as.numeric(
           comps[[1L]]$order_value %||% NA_real_)),
         order_unit = suppressWarnings(as.numeric(
           comps[[1L]]$order_unit %||% 0)),
         key = key, hist = hist)
  })
  if (length(rows) == 0L) {
    return(list(result = NULL, warnings = paste0(
      "No record-weighted stratum was released by every server; ",
      "pooling_policy = \"pooled_only_ok\" also pools strata released by ",
      "some servers.")))
  }
  result <- do.call(rbind, lapply(rows, `[[`, "row"))
  names(result) <- display$columns
  order_value <- vapply(rows, `[[`, numeric(1L), "order_value")
  order_unit <- vapply(rows, `[[`, numeric(1L), "order_unit")
  key <- vapply(rows, `[[`, character(1L), "key")
  ord <- if (identical(display$order, "n_persons")) {
    order(-result[[display$persons]], order_value, key, method = "radix",
          na.last = TRUE)
  } else {
    order(order_value, order_unit, key, method = "radix", na.last = TRUE)
  }
  result <- result[ord, , drop = FALSE]
  rownames(result) <- NULL
  key_columns <- setdiff(display$columns,
                         c(display$persons, display$records,
                           display$mean, display$sd))
  attach_views <- function(field) {
    frames <- lapply(ord, function(i) {
      view <- rows[[i]]$hist[[field]]
      if (is.null(view)) return(NULL)
      labels <- rows[[i]]$row[, key_columns, drop = FALSE]
      cbind(labels[rep(1L, nrow(view)), , drop = FALSE], view, row.names = NULL)
    })
    frames <- Filter(Negate(is.null), frames)
    if (length(frames)) do.call(rbind, frames) else NULL
  }
  attr(result, "value_histograms") <- attach_views("bins")
  attr(result, "value_quantiles") <- attach_views("quantiles")
  routes <- vapply(payloads, `[[`, character(1L), "protection")
  warnings <- paste0(
    "Pooled capped record-weighted moments: per stratum the signed person ",
    "totals, capped record counts and lattice sums of every site were summed ",
    "and the record-weighted mean and SD derived once (the persons never ",
    "weight the mean).")
  if (partial) {
    warnings <- c(warnings, paste0(
      "Some strata were released by only some servers and were pooled over ",
      "those servers."))
  }
  if (length(no_moments)) {
    warnings <- c(warnings, paste0(
      "Moments were not pooled for ", length(no_moments), " strata: a ",
      "server released none or used another public scale."))
  }
  if (length(histogram_owners)) {
    warnings <- c(warnings, paste0(
      "Compatible signed record-value cells were summed and projected onto ",
      "the displayed capped record totals; value_histograms and ",
      "value_quantiles attributes contain the pooled cells and intervals."))
  }
  if (length(no_histograms)) {
    warnings <- c(warnings, paste0(
      "Value histograms were not pooled for ", length(no_histograms),
      " strata because a contributing server released none."))
  }
  if (length(conflicts)) {
    warnings <- c(warnings, paste0(
      "Conflicting presentation labels were retained as NA: ",
      paste(unique(conflicts), collapse = ", "), "."))
  }
  if (any(routes != "fixed_synopsis")) {
    warnings <- c(warnings, paste0(
      "At least one site used heuristic realised-contribution protection; ",
      "no pooled epsilon is reported."))
  }
  list(result = result, warnings = warnings)
}

#' Pool the frames of an entry with protected components
#'
#' Frames carrying protected components are pooled from their signed
#' components (\code{.pool_protected_release_frames}). When no server
#' returned protected components (each one's DP service is unavailable), the
#' standard outputs, whose disclosive fields are unavailable, are pooled with
#' each other by the entry's established contract (\code{standard_contract}).
#' An effect estimate (\code{effect_estimate} descriptor) is pooled as in
#' dsOMOP 2.10.2, by inverse-variance meta-analysis of the per-site protected
#' estimates and SEs: post-processing of the protected releases, after the
#' sites were checked to be compatible, distinct logical sources. The pooled
#' components stay available as attribute \code{protected_components}.
#'
#' @param raw Named list of per-site frames.
#' @param release_pooling The entry's \code{release_pooling} descriptor.
#' @param policy Pooling policy.
#' @param harmonization Harmonization contract.
#' @return List: \code{result}, \code{warnings}, \code{harmonization}.
#' @keywords internal
.pool_protected_analysis <- function(raw, release_pooling, policy = "strict",
                                     harmonization = NULL) {
  policy <- match.arg(policy, c("strict", "pooled_only_ok"))
  protected <- vapply(raw, function(df) {
    p <- attr(df, "dsomop_release", exact = TRUE)
    is.list(p) && identical(p$protocol, .DP_PROTECTED_RELEASE_PROTOCOL)
  }, logical(1L))
  standard <- release_pooling$standard_contract
  if (!any(protected) && is.list(standard)) {
    out <- .pool_analysis_contract(raw, standard, policy = policy,
                                   harmonization = harmonization)
    out$warnings <- unique(c(out$warnings, paste0(
      "No server returned protected components (their DP services are ",
      "unavailable): the standard outputs, with their protected fields ",
      "unavailable, were pooled by the established contract.")))
    return(out)
  }
  components <- .pool_protected_release_frames(
    raw, policy = policy, columns = release_pooling$columns)
  effect <- release_pooling$effect_estimate
  if (!is.list(effect)) {
    return(c(components, list(harmonization = harmonization)))
  }
  # The established effect contract sums its count columns too: here they
  # are sums of per-site displayed protected values, never banded, so the
  # count-band compatibility of standard pooling does not apply.
  pooled <- .pool_analysis_contract(raw, effect, policy = policy,
                                    harmonization = NULL)
  result <- pooled$result
  if (!is.null(result)) {
    attr(result, "protected_components") <- components$result
  }
  warnings <- c(
    components$warnings, pooled$warnings, paste0(
      "Pooled effect: inverse-variance meta-analysis of the per-site ",
      "protected estimates and SEs (post-processing of the protected ",
      "releases); its count columns are sums of the per-site displayed ",
      "protected values. The signed-component pooling is attribute ",
      "'protected_components'."))
  list(result = result, warnings = unique(warnings),
       harmonization = harmonization)
}

#' Warnings for pooled frames that mix standard and protected blocks
#'
#' In standard mode a dsOMOP 2.10.3 server with a DP service fills the
#' person-time block (person-days, rates, MDRR, effects) of the C6/C9 entries
#' from protected components and labels the frame \code{mixed}. The
#' established contract then sums or meta-analyses those fields like any
#' other: the pooled values are post-processing of protected (noisy) values,
#' not exact sums, and carry no pooled epsilon.
#'
#' @param raw Named list of per-site frames.
#' @return Character vector of warnings (possibly empty).
#' @keywords internal
.analysis_mixed_pool_warnings <- function(raw) {
  fields <- unique(unlist(lapply(raw, function(df) {
    label <- attr(df, "dsomop_protection", exact = TRUE)
    if (!is.list(label) || !identical(label$protection, "mixed") ||
        !is.data.frame(df)) return(character(0))
    standard <- label$blocks$standard
    if (!is.character(standard)) return(character(0))
    setdiff(names(df), standard)
  }), use.names = FALSE))
  if (length(fields) == 0L) return(character(0))
  paste0(
    "Mixed protected frames: the pooled ", paste(fields, collapse = ", "),
    " are sums or derivations of per-site protected (noisy) values, not ",
    "exact sums; no pooled epsilon is reported.")
}
