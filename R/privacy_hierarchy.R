# Client projections for complete protected hierarchies. These algorithms
# match dsOMOP's versioned l2-transportation-dykstra-v1 contract; all inputs
# are released signed components, never raw or hidden support counts.

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
    stop("Canonical hierarchy projection did not converge.", call. = FALSE)
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
      !identical(meta$display, "l2-transportation-dykstra-v1")) {
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
