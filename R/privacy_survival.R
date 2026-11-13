# Complete protected survival displays. Signed components pool first; the
# displayed status and subgroup parents then constrain every child cell.
.dp_survival_signature <- function(o) {
  b <- o$family_basis
  list(status_of_cell = o$status_of_cell, display_projection = o$display_projection,
       parent = o$parent, parent_owner = o$parent_owner, horizon_days = o$horizon_days,
       family_basis = if (is.null(b)) NULL else list(
         b$version, b$projection, b$family_id, b$horizon_days, b$status_of_cell,
         names(b$arms), names(b$atoms), b$view_of_atoms, b$view_index,
         lapply(b$arms, `[[`, "stratum"),
         lapply(b$atoms, function(a) lapply(a, `[[`, "stratum"))))
}

.dp_survival_number <- function(x, n) {
  is.numeric(x) && length(x) == n && all(is.finite(x))
}

.dp_survival_close <- function(x, y) {
  length(x) == length(y) && all(is.finite(c(x, y))) &&
    max(abs(x - y), 0) <= 1e-8 * max(1, abs(x), abs(y))
}

# The server's finite IPF plus nonnegative surplus/deficit repair. It is a
# feasible display with fixed parents, not a claim of an exact KL/L2 minimizer.
.dp_c8_ipf <- function(X, r, c, iterations = 2000L, tol = 1e-10) {
  X <- as.matrix(X)
  k <- nrow(X); m <- ncol(X)
  out <- matrix(0, k, m, dimnames = dimnames(X))
  if (k == 0L || m == 0L) return(out)
  r <- pmax(0, as.numeric(r)); c <- pmax(0, as.numeric(c))
  if (sum(r) <= 0 || sum(c) <= 0) return(out)
  c <- c * sum(r) / sum(c)
  Z <- pmax(X, 0) + 1e-9 * max(1, sum(r)) / (k * m)
  Z[r == 0, ] <- 0
  Z[, c == 0] <- 0
  scale_rows <- function(Z) {
    rs <- rowSums(Z)
    Z * ifelse(rs > 0, r / rs, 0)
  }
  for (it in seq_len(iterations)) {
    Z <- scale_rows(Z)
    cs <- colSums(Z)
    Z <- sweep(Z, 2L, ifelse(cs > 0, c / cs, 0), `*`)
    if (max(abs(rowSums(Z) - r)) <= tol * max(1, sum(r))) break
  }
  Z <- .dp_repair_columns(scale_rows(Z), c)
  dimnames(Z) <- dimnames(X)
  Z
}

.dp_pool_survival_owner <- function(owners) {
  o <- owners[[1L]]
  c8 <- identical(o$family, "C8")
  b <- o$family_basis
  fail <- function() stop("Malformed or incompatible protected survival hierarchy.",
                          call. = FALSE)
  if (!c8 && !identical(o$family, "C6")) fail()
  if (is.null(b) && (!is.null(o$parent) || !is.null(o$parent_owner))) fail()
  horizon <- if (is.null(b)) o$horizon_days else b$horizon_days
  if (!c8 && (!.dp_survival_number(horizon, 1L) || horizon != 3650)) fail()
  if (any(!vapply(owners, function(x) {
    identical(.dp_survival_signature(x), .dp_survival_signature(o)) &&
      identical(x$cell_keys, o$cell_keys) && identical(x$family, o$family)
  }, logical(1L)))) fail()
  cells <- o$cell_keys
  event <- NULL
  if (c8) {
    if (!is.character(cells) || !length(cells) || anyNA(cells) ||
        anyDuplicated(cells) || !is.character(o$status_of_cell) ||
        length(o$status_of_cell) != length(cells) ||
        anyNA(o$status_of_cell) ||
        any(!o$status_of_cell %in% c("event", "censored", "admin_censored")) ||
        !identical(sub("^.*\\|status=", "", cells), o$status_of_cell)) fail()
    event <- o$status_of_cell == "event"
    if (!any(event) || all(event)) fail()
    if (!identical(o$display_projection, "c8-status-parent-simplex-v1")) fail()
  }
  fields <- if (c8) c("n", "status", "h") else c("n", "status", "time")
  valid_component <- function(x) {
    is.list(x) && is.character(x$stratum) && length(x$stratum) == 1L &&
      !is.na(x$stratum) && nzchar(x$stratum) &&
      .dp_survival_number(x$n, 1L) && .dp_survival_number(x$status, 2L) &&
      .dp_survival_close(sum(x$status), x$n) &&
      if (c8) .dp_survival_number(x$h, length(cells)) &&
        .dp_survival_close(c(sum(x$h[event]), sum(x$h[!event])), x$status) else
        .dp_survival_number(x$time, 1L)
  }
  add <- function(xs) {
    out <- list(stratum = xs[[1L]]$stratum)
    for (f in fields) out[[f]] <- Reduce(`+`, lapply(xs, `[[`, f))
    out
  }
  validate_components <- function(xs, strata) {
    is.list(xs) && length(xs) == length(strata) &&
      all(vapply(xs, valid_component, logical(1L))) &&
      identical(unname(vapply(xs, `[[`, character(1L), "stratum")), strata)
  }
  if (!is.list(o$components) || !length(o$components) ||
      !all(vapply(o$components, valid_component, logical(1L)))) fail()
  strata <- unname(vapply(o$components, `[[`, character(1L), "stratum"))
  if (anyDuplicated(strata)) fail()
  if (any(!vapply(owners, function(x) validate_components(x$components, strata),
                  logical(1L)))) fail()
  roots <- lapply(seq_along(strata), function(j) {
    add(lapply(owners, function(x) x$components[[j]]))
  })
  atoms <- NULL
  if (!is.null(b)) {
    named <- function(x) is.list(x) && length(x) > 0L &&
      !is.null(names(x)) && !anyNA(names(x)) && all(nzchar(names(x))) &&
      !anyDuplicated(names(x))
    if (!identical(b$version, "interaction-family-v1") ||
        !identical(b$projection, if (c8) "finite-ipf-feasible-repair-v1" else
                     "bounded-simplex-v1") ||
        !is.list(b$family_id) || !is.character(b$family_id$kind) ||
        length(b$family_id$kind) != 1L || is.na(b$family_id$kind) ||
        !b$family_id$kind %in% c("sex", "age5", "concept", "explicit-partition") ||
        !.dp_survival_number(b$horizon_days, 1L) || b$horizon_days != 3650 ||
        (c8 && !identical(b$status_of_cell, o$status_of_cell)) ||
        !named(b$arms) || !named(b$atoms) || !named(b$view_of_atoms) ||
        !identical(names(b$arms), strata) ||
        !.dp_survival_number(b$view_index, 1L) ||
        b$view_index != floor(b$view_index) || b$view_index < 1L ||
        b$view_index > length(b$view_of_atoms)) fail()
    atom_keys <- switch(b$family_id$kind,
      sex = c("gender=8507", "gender=8532", "gender=other-or-unknown"),
      age5 = paste0("age5=", c("neg", seq(0, 110, 5), "unknown")),
      concept = c("concept=present", "concept=absent"),
      `explicit-partition` = b$family_id$atoms)
    if (!identical(names(b$atoms), atom_keys) ||
        !identical(strata, c("arm=target", "arm=comparator"))) fail()
    if (identical(b$family_id$kind, "concept") &&
        !all(vapply(b$family_id[c("concept_id", "domain_code")], function(x) {
          is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
        }, logical(1L)))) fail()
    masks <- b$view_of_atoms
    if (!all(vapply(masks, function(i) is.numeric(i) && length(i) > 0L &&
                    !anyNA(i) && all(i == floor(i)) && !anyDuplicated(i) &&
                    all(i >= 1L & i <= length(b$atoms)), logical(1L))) ||
        !identical(sort(as.integer(unlist(masks, use.names = FALSE))),
                    seq_along(b$atoms))) fail()
    for (site in owners) {
      sb <- site$family_basis
      if (!validate_components(sb$arms, strata) ||
          any(!vapply(sb$atoms, function(a) identical(names(a), strata), logical(1L))) ||
          any(!vapply(sb$atoms, validate_components, logical(1L), strata = strata))) fail()
      for (j in seq_along(strata)) {
        all_atoms <- add(lapply(sb$atoms, `[[`, j))
        selected <- add(lapply(sb$atoms[masks[[b$view_index]]], `[[`, j))
        for (f in fields) {
          if (!.dp_survival_close(all_atoms[[f]], sb$arms[[j]][[f]]) ||
              !.dp_survival_close(selected[[f]], site$components[[j]][[f]])) fail()
        }
      }
    }
    roots <- lapply(seq_along(strata), function(j) {
      add(lapply(owners, function(x) x$family_basis$arms[[j]]))
    })
    atoms <- lapply(seq_along(b$atoms), function(i) {
      lapply(seq_along(strata), function(j) {
        add(lapply(owners, function(x) x$family_basis$atoms[[i]][[j]]))
      })
    })
  }
  N <- E <- T <- numeric(length(strata))
  H <- vector("list", length(strata))
  for (j in seq_along(strata)) {
    root <- roots[[j]]
    N[[j]] <- max(0, root$n)
    E[[j]] <- .dp_simplex(root$status, N[[j]])[[1L]]
    if (c8) {
      H[[j]] <- numeric(length(cells))
      H[[j]][event] <- .dp_simplex(root$h[event], E[[j]])
      H[[j]][!event] <- .dp_simplex(root$h[!event], N[[j]] - E[[j]])
    } else T[[j]] <- min(max(root$time, 0), N[[j]] * horizon)
    if (is.null(atoms)) next
    children <- lapply(atoms, `[[`, j)
    ns <- .dp_simplex(vapply(children, `[[`, numeric(1L), "n"), N[[j]])
    st <- do.call(rbind, lapply(children, `[[`, "status"))
    es <- .dp_bounded_simplex((st[, 1L] + ns - st[, 2L]) / 2,
                               E[[j]], 0, ns)
    idx <- b$view_of_atoms[[b$view_index]]
    if (c8) {
      X <- do.call(rbind, lapply(children, `[[`, "h"))
      DE <- .dp_c8_ipf(X[, event, drop = FALSE], es, H[[j]][event])
      DN <- .dp_c8_ipf(X[, !event, drop = FALSE], ns - es, H[[j]][!event])
      H[[j]][event] <- colSums(DE[idx, , drop = FALSE])
      H[[j]][!event] <- colSums(DN[idx, , drop = FALSE])
    } else {
      ts <- .dp_bounded_simplex(vapply(children, `[[`, numeric(1L), "time"),
                                 T[[j]], 0, ns * b$horizon_days)
      T[[j]] <- sum(ts[idx])
    }
    N[[j]] <- sum(ns[idx]); E[[j]] <- sum(es[idx])
  }
  frame <- data.frame(owner = o$owner, stratum = strata, persons = N, events = E,
                        stringsAsFactors = FALSE)
  frame$family <- o$family
  frame$view <- if (is.null(b)) NA_character_ else names(b$view_of_atoms)[[b$view_index]]
  frame$subgroup_kind <- if (is.null(b)) NA_character_ else b$family_id$kind
  if (!c8) {
    frame$person_time <- T
    frame$rate <- ifelse(T > 0, E / T, NA_real_)
  }
  out_cells <- if (c8) do.call(rbind, lapply(seq_along(strata), function(j) {
    data.frame(owner = o$owner, stratum = strata[[j]], cell = cells,
                 count = H[[j]], family = frame$family[[j]],
                 view = frame$view[[j]], subgroup_kind = frame$subgroup_kind[[j]],
                 stringsAsFactors = FALSE)
  })) else NULL
  list(frame = frame, cells = out_cells)
}
