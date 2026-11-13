# Client projections for complete protected hierarchies. These algorithms
# match dsOMOP's versioned Dykstra-with-feasible-fallback contract; all inputs
# are released signed components, never raw or hidden support counts. The
# finite-iteration fallback preserves margins but is not an exact L2 projection.

.dp_bounded_simplex <- function(y, total, lower = 0, upper = Inf) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) return(numeric(0))
  lower <- rep_len(as.numeric(lower), n)
  upper <- rep_len(as.numeric(upper), n)
  upper <- pmax(upper, lower)
  total <- min(max(as.numeric(total), sum(lower)), sum(upper))
  f <- function(lambda) sum(pmin(pmax(y - lambda, lower), upper))
  bps <- sort(unique(c(y - upper, y - lower)), method = "radix")
  bps <- bps[is.finite(bps)]
  vals <- vapply(bps, f, numeric(1L))
  m <- length(bps)
  lambda <- if (total >= vals[[1L]]) {
    # Left of every breakpoint only the unbounded-above entries move.
    slope <- sum(is.infinite(upper))
    if (slope == 0L) bps[[1L]] else bps[[1L]] - (total - vals[[1L]]) / slope
  } else if (total <= vals[[m]]) {
    bps[[m]]
  } else {
    k <- max(which(vals >= total))
    if (vals[[k]] == vals[[k + 1L]]) bps[[k]] else
      bps[[k]] + (vals[[k]] - total) * (bps[[k + 1L]] - bps[[k]]) /
        (vals[[k]] - vals[[k + 1L]])
  }
  pmin(pmax(y - lambda, lower), upper)
}


.dp_simplex_rows <- function(Y, totals) {
  n_row <- nrow(Y)
  n_col <- ncol(Y)
  U <- matrix(apply(Y, 1L, sort, decreasing = TRUE), nrow = n_row,
              ncol = n_col, byrow = TRUE)
  CS <- matrix(apply(U, 1L, cumsum), nrow = n_row, ncol = n_col,
               byrow = TRUE)
  J <- matrix(seq_len(n_col), n_row, n_col, byrow = TRUE)
  rho <- pmax(1L, rowSums(U - (CS - totals) / J > 0))
  tau <- (CS[cbind(seq_len(n_row), rho)] - totals) / rho
  pmax(Y - tau, 0)
}

# Match the server's nonnegative column repair without moving row parents.
.dp_repair_columns <- function(Z, target) {
  current <- colSums(Z)
  surplus <- pmax(current - target, 0)
  deficit <- pmax(target - current, 0)
  if (sum(surplus) <= 0 || sum(deficit) <= 0) return(Z)
  fraction <- ifelse(current > 0, pmin(surplus / current, 1), 0)
  removed <- sweep(Z, 2L, fraction, `*`)
  Z - removed + outer(rowSums(removed), deficit / sum(deficit))
}


.dp_transport <- function(Y, rows, cols, tol = 1e-12,
                                    max_iter = 20000L) {
  rows <- pmax(0, as.numeric(rows))
  cols <- pmax(0, as.numeric(cols))
  total <- sum(rows)
  if (total <= 0) return(Y * 0)
  # Exact equal totals (both margins are displays of one parent).
  cols <- if (sum(cols) > 0) cols * (total / sum(cols)) else
    rep(total / length(cols), length(cols))
  # Zero-margin rows/columns are fixed at zero. Removing them is the exact
  # same Euclidean problem and avoids slow alternating-projection tails on
  # sparse public age/year domains.
  live_rows <- rows > 0
  live_cols <- cols > 0
  if (!all(live_rows) || !all(live_cols)) {
    out <- Y * 0
    out[live_rows, live_cols] <- .dp_transport(
      Y[live_rows, live_cols, drop = FALSE], rows[live_rows], cols[live_cols],
      tol = tol, max_iter = max_iter)
    return(out)
  }
  scale <- max(1, total)
  X <- Y
  P <- Y * 0
  Q <- Y * 0
  for (it in seq_len(max_iter)) {
    Z <- .dp_simplex_rows(X + P, rows)
    P <- X + P - Z
    Xn <- t(.dp_simplex_rows(t(Z + Q), cols))
    Q <- Z + Q - Xn
    done <- max(abs(rowSums(Xn) - rows)) <= tol * scale &&
      max(abs(Xn - X)) <= tol * scale
    X <- Xn
    if (done) break
  }
  if (max(abs(rowSums(X) - rows), abs(colSums(X) - cols)) > 1e-8 * scale) {
    # Keep converged Dykstra outputs unchanged. A finite iteration limit is
    # not evidence that the nonempty feasible set is unavailable.
    X <- pmax(X, 0)
    rs <- rowSums(X)
    live <- rs > 0
    X[live, ] <- X[live, , drop = FALSE] * (rows[live] / rs[live])
    if (any(!live)) X[!live, ] <- outer(rows[!live], cols / total)
    X <- .dp_repair_columns(X, cols)
    if (max(abs(rowSums(X) - rows), abs(colSums(X) - cols)) > 1e-8 * scale) {
      stop("Canonical hierarchy projection did not converge.", call. = FALSE)
    }
  }
  X
}



# Only public semantics enter compatibility; signed values differ by site.
.dp_c6_hierarchy_signature <- function(o) {
  list(hierarchy = o$hierarchy,
       nodes = lapply(o$hierarchy_nodes, function(n) n[c("key", "parent")]))
}

# Sum one complete C6 hierarchy and project it from the root down before
# selecting the requested view. Crossed age/year cells use both margins.
.dp_pool_c6_hierarchy <- function(owners) {
  o <- owners[[1L]]
  meta <- o$hierarchy
  if (!identical(meta$version, "c6-tree-gender-age5-year-v1") ||
      !identical(meta$display, "l2-transportation-dykstra-feasible-fallback-v2")) {
    stop("Unknown person-time hierarchy projection; upgrade dsOMOPClient.",
         call. = FALSE)
  }
  signed <- c("N_s", "E_s", "NE_s", "T_s", "R_s")
  nodes <- o$hierarchy_nodes
  if (!is.list(nodes) || !"root" %in% names(nodes) ||
      nrow(nodes$root) != 1L ||
      !all(names(nodes) %in% c("root", "gender", "age", "year", "age_year"))) {
    stop("Malformed complete person-time hierarchy.", call. = FALSE)
  }
  for (level in names(nodes)) {
    sums <- lapply(owners, function(x) {
      n <- x$hierarchy_nodes[[level]]
      if (!is.data.frame(n) || !all(c("key", "parent", signed) %in% names(n)) ||
          anyNA(n$key) || anyDuplicated(n$key) ||
          !all(vapply(n[signed], function(v) is.numeric(v) &&
            all(is.finite(v)), logical(1L)))) {
        stop("Malformed signed person-time hierarchy nodes.", call. = FALSE)
      }
      as.matrix(n[signed])
    })
    nodes[[level]][signed] <- Reduce(`+`, sums)
  }
  horizon <- .dp_protected_number(meta$horizon_days, "pooled", "horizon_days", TRUE)
  cap <- .dp_protected_number(meta$recurrent_cap, "pooled", "recurrent_cap", TRUE)
  if (cap < 1 || horizon <= 0) stop("Invalid person-time hierarchy bounds.",
                                   call. = FALSE)
  root <- nodes$root
  root$N_d <- max(0, root$N_s)
  st <- .dp_simplex(c(root$E_s, root$NE_s), root$N_d)
  root$E_d <- st[[1L]]; root$NE_d <- st[[2L]]
  root$T_d <- min(max(root$T_s, 0), root$N_d * horizon)
  root$R_d <- min(max(root$R_s, root$E_d), cap * root$E_d)
  nodes$root <- root
  displayed <- c("N_d", "E_d", "NE_d", "T_d", "R_d")
  for (level in intersect(c("gender", "age", "year"), names(nodes))) {
    parent <- nodes[[if (level == "gender") "root" else "gender"]]
    n <- nodes[[level]]
    if (is.null(parent) || any(!n$parent %in% parent$key)) {
      stop("A person-time hierarchy lacks its parent nodes.", call. = FALSE)
    }
    n[displayed] <- 0
    for (p in seq_len(nrow(parent))) {
      i <- which(n$parent == parent$key[[p]])
      if (!length(i)) next
      N <- .dp_simplex(n$N_s[i], parent$N_d[[p]])
      E <- .dp_bounded_simplex((n$E_s[i] + N - n$NE_s[i]) / 2,
                                parent$E_d[[p]], 0, N)
      T <- .dp_bounded_simplex(n$T_s[i], parent$T_d[[p]], 0, N * horizon)
      R <- .dp_bounded_simplex(n$R_s[i], parent$R_d[[p]], E, cap * E)
      n[i, displayed] <- cbind(N, E, N - E, T, R)
    }
    nodes[[level]] <- n
  }
  if ("age_year" %in% names(nodes)) {
    if (!all(c("gender", "age", "year") %in% names(nodes))) {
      stop("A crossed person-time hierarchy lacks its two margins.", call. = FALSE)
    }
    n <- nodes$age_year
    n[displayed] <- 0
    for (g in nodes$gender$key) {
      a <- nodes$age[nodes$age$parent == g, , drop = FALSE]
      y <- nodes$year[nodes$year$parent == g, , drop = FALSE]
      i <- which(n$parent == g)
      expected <- as.vector(outer(a$key, sub(paste0(g, "|"), "", y$key,
                                             fixed = TRUE), paste, sep = "|"))
      if (length(i) != nrow(a) * nrow(y) ||
          !identical(n$key[i], expected)) {
        stop("A crossed person-time hierarchy has incompatible cell ordering.",
             call. = FALSE)
      }
      for (field in c("E", "NE", "T", "R")) {
        sc <- paste0(field, "_s"); dc <- paste0(field, "_d")
        X <- matrix(n[[sc]][i], nrow = nrow(a))
        n[[dc]][i] <- as.vector(.dp_transport(X, a[[dc]], y[[dc]]))
      }
      n$N_d[i] <- n$E_d[i] + n$NE_d[i]
    }
    nodes$age_year <- n
  }
  view <- meta$view_of_nodes
  n <- nodes[[meta$view_level]]
  strata <- vapply(o$components, `[[`, character(1L), "stratum")
  if (!is.list(view) || length(view) != length(strata) || is.null(n) ||
      !identical(names(view), strata) ||
      !all(vapply(view, function(i) is.numeric(i) && !anyNA(i) &&
        !anyDuplicated(i) && all(i == floor(i)) &&
        all(i >= 1 & i <= nrow(n)), logical(1L))) ||
      !identical(sort(as.integer(unlist(view, use.names = FALSE))),
                  seq_len(nrow(n)))) {
    stop("A person-time hierarchy lacks a complete public view mapping.",
         call. = FALSE)
  }
  sums <- t(vapply(view, function(i) colSums(n[i, displayed, drop = FALSE]),
                    stats::setNames(numeric(length(displayed)), displayed)))
  frame <- data.frame(owner = o$owner, stratum = strata,
                       persons = sums[, "N_d"], events = sums[, "E_d"],
                       person_time = sums[, "T_d"], recurrent = sums[, "R_d"],
                       stringsAsFactors = FALSE, row.names = NULL)
  frame$rate <- ifelse(frame$person_time > 0,
                        frame$events / frame$person_time, NA_real_)
  frame
}

# Pool the complete first-event or capped-recurrent day hierarchy. The
# weighted recurrent root is R; N is never substituted for event mass.
.pool_protected_tte <- function(payloads, columns = NULL) {
  os <- lapply(payloads, function(p) {
    if (length(p$owners) != 1L) {
      stop("A time-to-event release must carry one complete hierarchy.", call. = FALSE)
    }
    p$owners[[1L]]
  })
  o <- os[[1L]]
  signature <- function(x) list(x$owner, x$population, x$family, x$basis,
    x$estimand_kind, x$cap, x$allocation, x$levels, x$hierarchy_nodes,
    x$view, x$display_projection, x$presence_basis, x$presence_view)
  if (!identical(o$display_projection, if (identical(o$estimand_kind, "recurrent"))
      "tte-hierarchy-presence-v1" else "tte-hierarchy-simplex-v1") ||
      !identical(o$basis, "tte-signed-day-basis-v1") ||
      !identical(o$family, "TTE") ||
      !is.character(o$estimand_kind) || length(o$estimand_kind) != 1L ||
      is.na(o$estimand_kind) || !o$estimand_kind %in% c("first", "recurrent") ||
      !is.character(o$levels) || !length(o$levels) ||
      !identical(o$levels, c("A", "B", "C")[seq_along(o$levels)]) ||
      any(!vapply(os, function(x) identical(signature(x), signature(o)), logical(1L))) ||
      any(!vapply(payloads, function(p) identical(p$entry, payloads[[1L]]$entry),
                   logical(1L)))) {
    stop("Protected time-to-event hierarchies have an unknown or incompatible basis/view.",
         call. = FALSE)
  }
  expected_columns <- c("day_offset", "num_events", "persons")
  if (!is.null(columns) && !identical(names(columns), expected_columns)) {
    stop("Protected time-to-event output does not match its contracted columns.",
         call. = FALSE)
  }
  comps <- lapply(os, function(x) {
    if (length(x$components) != 1L ||
        !identical(x$components[[1L]]$stratum, "root")) {
      stop("Malformed time-to-event root component.", call. = FALSE)
    }
    x$components[[1L]]
  })
  field <- if (o$estimand_kind == "recurrent") "r" else "n"
  total <- max(0, sum(vapply(seq_along(comps), function(i) {
    .dp_protected_number(comps[[i]][[field]],
                         if (is.null(names(payloads))) as.character(i) else
                           names(payloads)[[i]], field)
  }, numeric(1L))))
  N <- max(0, sum(vapply(comps, function(c) {
    .dp_protected_number(c$n, "pooled", "n")
  }, numeric(1L))))
  K <- .dp_protected_number(o$cap, "pooled", "cap", TRUE)
  if (K < 1 || K != floor(K)) stop("Invalid event-time cap.", call. = FALSE)
  if (field == "r") total <- min(K * N, max(N, total))
  nodes <- o$hierarchy_nodes
  displays <- list()
  for (level in o$levels) {
    node <- nodes[[level]]
    if (!is.data.frame(node) || !all(c("key", "parent") %in% names(node)) ||
        !nrow(node) || !is.character(node$key) || anyNA(node$key) ||
        any(!nzchar(node$key)) || anyDuplicated(node$key)) {
      stop("Malformed time-to-event hierarchy nodes.", call. = FALSE)
    }
    signed <- lapply(comps, function(c) {
      x <- c$levels[[level]]
      if (!identical(x$keys, node$key) || !is.numeric(x$signed) ||
          length(x$signed) != nrow(node) || any(!is.finite(x$signed))) {
        stop("Malformed signed time-to-event components.", call. = FALSE)
      }
      x$signed
    })
    pooled <- Reduce(`+`, signed)
    if (level == "A") {
      displays[[level]] <- .dp_simplex(pooled, total)
    } else {
      parent <- displays[[if (level == "B") "A" else "B"]]
      if (!is.numeric(node$parent) || anyNA(node$parent) ||
          any(node$parent != floor(node$parent)) ||
          any(node$parent < 1 | node$parent > length(parent))) {
        stop("Malformed time-to-event parent indices.", call. = FALSE)
      }
      shown <- numeric(length(pooled))
      for (p in unique(node$parent)) {
        kids <- which(node$parent == p)
        shown[kids] <- .dp_simplex(pooled[kids], parent[[p]])
      }
      displays[[level]] <- shown
    }
  }
  view <- o$view
  if (!is.list(view) || !is.numeric(view$day_offset) || anyNA(view$day_offset) ||
      !is.character(view$level) || anyNA(view$level) ||
      !all(view$level %in% o$levels) || !is.list(view$nodes) ||
      length(view$nodes) != length(view$level) ||
      length(view$nodes) != length(view$day_offset) ||
      !all(vapply(seq_along(view$nodes), function(i) {
        idx <- view$nodes[[i]]
        is.numeric(idx) && length(idx) > 0L && !anyNA(idx) &&
          !anyDuplicated(idx) && all(idx == floor(idx)) &&
          all(idx >= 1L & idx <= length(displays[[view$level[[i]]]]))
      }, logical(1L)))) {
    stop("Malformed public time-to-event view mapping.", call. = FALSE)
  }
  # The mixed-level bins must partition the whole hierarchy exactly once.
  covered <- lapply(o$levels, function(l) {
    unlist(view$nodes[view$level == l], use.names = FALSE)
  })
  names(covered) <- o$levels
  for (level in rev(o$levels)) {
    if (anyDuplicated(covered[[level]])) {
      stop("A time-to-event view repeats protected nodes.", call. = FALSE)
    }
    if (level == "A") next
    parent_level <- if (level == "B") "A" else "B"
    for (p in unique(nodes[[level]]$parent)) {
      kids <- which(nodes[[level]]$parent == p)
      hit <- kids %in% covered[[level]]
      if (any(hit)) {
        if (!all(hit) || p %in% covered[[parent_level]]) {
          stop("A time-to-event view overlaps or omits hierarchy children.", call. = FALSE)
        }
        covered[[parent_level]] <- c(covered[[parent_level]], p)
      }
    }
  }
  if (!identical(sort(as.integer(covered$A)), seq_len(nrow(nodes$A)))) {
    stop("A time-to-event view does not cover its complete basis.", call. = FALSE)
  }
  events <- vapply(seq_along(view$nodes), function(i) {
    sum(displays[[view$level[[i]]]][view$nodes[[i]]])
  }, numeric(1L))
  persons <- events
  if (field == "r") {
    basis <- o$presence_basis
    intervals <- basis$intervals
    idx <- o$presence_view
    if (!is.data.frame(intervals) ||
        !all(c("key", "lower", "upper", "level") %in% names(intervals)) ||
        anyNA(intervals$key) || anyDuplicated(intervals$key) ||
        !all(intervals$level %in% o$levels) ||
        !is.list(basis$nodes) || length(basis$nodes) != nrow(intervals) ||
        !is.numeric(idx) || length(idx) != length(events) || anyNA(idx) ||
        any(idx != floor(idx) | idx < 1 | idx > nrow(intervals)) || anyDuplicated(idx) ||
        !identical(intervals$lower[idx], view$day_offset) ||
        !identical(intervals$upper[idx], view$upper)) {
      stop("Malformed public event-time presence mapping.", call. = FALSE)
    }
    ep <- vapply(seq_len(nrow(intervals)), function(i) {
      j <- basis$nodes[[i]]
      d <- displays[[intervals$level[[i]]]]
      if (!is.numeric(j) || !length(j) || anyNA(j) || anyDuplicated(j) ||
          any(j != floor(j) | j < 1 | j > length(d))) {
        stop("Malformed event-time presence event mask.", call. = FALSE)
      }
      sum(d[j])
    }, numeric(1L))
    signed <- lapply(comps, function(c) {
      if (!is.numeric(c$p) || length(c$p) != nrow(intervals) ||
          any(!is.finite(c$p))) {
        stop("Missing or malformed signed recurrent presence vector.", call. = FALSE)
      }
      c$p
    })
    persons <- .dp_tte_presence_project(Reduce(`+`, signed), ep, N, total, K, basis)[idx]
  }
  result <- data.frame(day_offset = view$day_offset,
                        num_events = signif(events, 12), persons = signif(persons, 12))
  list(result = result, warnings = paste0(
    "Pooled protected ", o$estimand_kind, " event-time hierarchy: signed ",
    "nodes were summed, projected from their ", if (field == "r") "capped event" else
      "person", " total down through A/B/C, then coarsened to the requested bins.",
    if (field == "r") paste0(" Distinct contributors among the same K-capped occurrences ",
      "use the complete interval-presence vector and versioned feasible radial projection; ",
      "coarse persons are never sums of fine-bin persons. Projection toward its event-mass ",
      "anchor can add bias.") else ""))
}
