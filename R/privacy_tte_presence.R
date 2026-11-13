# Versioned feasible radial projection, not Euclidean projection. The concave
# anchor f(E)=N*(1-(1-E/R)^(R/N)) satisfies the public bounds, containment and
# partition inequalities. A feasible input is preserved exactly. Otherwise
# move from that anchor toward the bounded noisy target as far as every
# inequality permits. Retreat toward the anchor can bias the displayed P.
.dp_tte_presence_project <- function(signed, events, N, R, K, basis) {
  m <- length(events)
  valid_index <- function(i) is.numeric(i) && !anyNA(i) &&
    all(i == floor(i)) && all(i >= 1L & i <= m) && !anyDuplicated(i)
  if (!identical(basis$version, "tte-interval-presence-v1") ||
      !identical(basis$projection, "tte-presence-radial-v1") ||
      !is.numeric(signed) || length(signed) != m || any(!is.finite(signed)) ||
      !is.numeric(events) || any(!is.finite(events)) || any(events < 0) ||
      !all(is.finite(c(N, R, K))) || N < 0 || R < N || R > K * N || K < 1 ||
      !is.matrix(basis$contains) || ncol(basis$contains) != 2L ||
      !all(vapply(seq_len(nrow(basis$contains)), function(i) {
        valid_index(basis$contains[i, ])
      }, logical(1L))) || !is.list(basis$partitions) ||
      !all(vapply(basis$partitions, function(p) {
        length(p$parent) == 1L && is.numeric(p$parent) && !is.na(p$parent) &&
          p$parent == floor(p$parent) && p$parent >= 0L && p$parent <= m &&
          length(p$children) > 1L && valid_index(p$children)
      }, logical(1L)))) {
    stop("Malformed protected event-time presence basis.", call. = FALSE)
  }
  tol <- 1e-9 * max(1, N, R)
  if (any(events > R + tol)) stop("Incoherent event-time presence totals.",
                                  call. = FALSE)
  if (N == 0) return(numeric(m))
  lower <- events / K
  upper <- pmin(N, events)
  anchor <- if (R == N) events else
    -N * expm1((R / N) * log1p(-pmin(1, events / R)))
  anchor <- pmin(upper, pmax(lower, anchor))
  residual <- function(p) {
    parents <- vapply(basis$partitions, function(q) {
      if (q$parent == 0L) N else p[[q$parent]]
    }, numeric(1L))
    sums <- vapply(basis$partitions, function(q) sum(p[q$children]), numeric(1L))
    c(p[basis$contains[, 1L]] - p[basis$contains[, 2L]],
      parents - sums, sums - K * parents)
  }
  target <- pmin(upper, pmax(lower, signed))
  rt <- residual(target)
  if (all(rt <= tol)) return(target)
  ra <- residual(anchor)
  if (any(ra > tol)) stop("Protected presence anchor is not feasible.", call. = FALSE)
  bad <- which(rt > tol)
  fraction <- min(1, pmax(0, -ra[bad]) / (rt[bad] - ra[bad]))
  out <- anchor + fraction * (target - anchor)
  if (any(!is.finite(out)) || any(residual(out) > tol) ||
      any(out < lower - tol | out > upper + tol)) {
    stop("Protected event-time presence projection did not resolve.", call. = FALSE)
  }
  out
}
