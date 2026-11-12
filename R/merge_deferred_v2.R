# A prefixed base whose equality depends on parameters cannot be shared yet.
# Retain only namespace correspondence, never recipe snapshots: binding compares
# the current populations and declarations, so subsequent editor changes survive.

.merge_deferred_has_params <- function(x) {
  found <- FALSE
  .v2_walk_params(x, function(p) found <<- TRUE)
  found
}

.merge_deferred_needed <- function(a, b) {
  .merge_deferred_has_params(list(.merge_effective_base(a), .merge_effective_base(b),
    .merge_deferred_nodes(a), .merge_deferred_nodes(b)))
}

.merge_deferred_empty_maps <- function() {
  stats::setNames(rep(list(character(0)), 7L),
                  c("population", "variable", "output", "block", "event", "interval", "concept_set"))
}

.merge_deferred_mark <- function(r, maps, prefix) {
  id <- unname(maps$population["base"])
  if (length(id) != 1L || is.na(id) || is.null(r$populations[[id]])) return(r)
  aliases <- list()
  for (kind in c("event", "interval", "concept_set")) {
    m <- maps[[kind]]
    aliases[[kind]] <- lapply(seq_along(m), function(i) list(from = unname(m[[i]]), to = names(m)[[i]]))
  }
  r$populations[[id]]$simple$merge_base <- list(
    target = "base", aliases = aliases, source_prefix = paste0(prefix, "_"), target_prefix = "")
  r
}

# Existing candidates may themselves be prefixed by a later merge. Both ends
# refer to that recipe's current namespace and must move together.
.merge_deferred_remap <- function(marker, maps, prefix) {
  if (is.null(marker)) return(NULL)
  look <- function(kind, name) {
    value <- maps[[kind]][name]
    if (length(value) && !is.na(value)) unname(value) else name
  }
  marker$target <- look("population", marker$target)
  for (kind in names(marker$aliases)) {
    marker$aliases[[kind]] <- lapply(marker$aliases[[kind]], function(pair) {
      list(from = look(kind, pair$from), to = look(kind, pair$to))
    })
  }
  if (!is.null(prefix)) {
    marker$source_prefix <- paste0(prefix, "_", marker$source_prefix)
    marker$target_prefix <- paste0(prefix, "_", marker$target_prefix)
  }
  marker
}

# Resolved helper observability is not a declaration. In particular, stale
# printing metadata must not hide a deliberate edit to an actual event.
.merge_deferred_nodes <- function(r) {
  for (id in names(r$populations)) {
    if (!isTRUE(r$populations[[id]]$simple$new_users$pending)) {
      r$populations[[id]]$simple <- NULL
    } else {
      r$populations[[id]]$simple$merge_base <- NULL
    }
  }
  r$meta <- NULL
  .merge_named_nodes(r)
}

.merge_deferred_aliases <- function(marker, nodes) {
  aliases <- .merge_deferred_empty_maps()
  for (kind in c("event", "interval", "concept_set")) {
    pairs <- marker$aliases[[kind]] %||% list()
    for (pair in pairs) aliases[[kind]][pair$from] <- pair$to
    # Parameters may introduce a named node only after the merge. Its possible
    # correspondence is still subject to the same current-definition proof.
    head <- paste0(kind, ":")
    keys <- names(nodes) %||% character(0)
    nms <- substring(keys[startsWith(keys, head)], nchar(head) + 1L)
    for (nm in nms[startsWith(nms, marker$source_prefix)]) {
      if (!nm %in% names(aliases[[kind]])) {
        aliases[[kind]][nm] <- paste0(marker$target_prefix,
          substring(nm, nchar(marker$source_prefix) + 1L))
      }
    }
  }
  aliases
}

# Prove source correspondences from leaves upward. A renamed event cannot be
# considered equal merely because its old name agrees: all renamed sources on
# which its current definition depends must already have been proved equal.
.merge_deferred_verified_maps <- function(nodes, aliases) {
  verified <- .merge_deferred_empty_maps()
  repeat {
    changed <- FALSE
    rn <- .merge_renamer(verified)
    for (kind in c("concept_set", "event", "interval")) {
      for (from in setdiff(names(aliases[[kind]]), names(verified[[kind]]))) {
        to <- unname(aliases[[kind]][from])
        left <- nodes[[paste0(kind, ":", from)]]
        right <- nodes[[paste0(kind, ":", to)]]
        if (!length(left) || !length(right) ||
            .merge_deferred_has_params(c(left, right))) next
        target <- unclass(right[[1L]])
        if (!all(vapply(right, function(d) identical(unclass(d), target), logical(1)))) next
        same <- vapply(left, function(d) {
          d$name <- to
          identical(unclass(rn$any(d)), target)
        }, logical(1))
        if (all(same)) {
          verified[[kind]][from] <- to
          changed <- TRUE
        }
      }
    }
    if (!changed) return(verified)
  }
}

# The eager check uses a temporary private namespace for b. That makes a named
# reference compare unequal until its current declaration has been checked,
# even when a and b happen to use the same textual name for different events.
.merge_base_semantics_equal <- function(a, b) {
  left <- .merge_effective_base(a)
  right <- .merge_effective_base(b)
  if (.merge_deferred_has_params(list(left, right))) return(FALSE)
  an <- .merge_deferred_nodes(a)
  bn <- .merge_deferred_nodes(b)
  maps <- .merge_deferred_empty_maps()
  aliases <- .merge_deferred_empty_maps()
  for (kind in c("event", "interval", "concept_set")) {
    head <- paste0(kind, ":")
    keys <- names(bn) %||% character(0)
    nms <- substring(keys[startsWith(keys, head)], nchar(head) + 1L)
    maps[[kind]] <- stats::setNames(vapply(nms, function(nm) paste0(".merge_candidate.", nm), ""), nms)
    aliases[[kind]] <- stats::setNames(nms, unname(maps[[kind]]))
  }
  rn <- .merge_renamer(maps)
  nodes <- an
  for (key in names(bn)) {
    definitions <- lapply(bn[[key]], rn$any)
    kind <- sub(":.*$", "", key)
    nodes[[paste0(kind, ":", definitions[[1L]]$name)]] <- definitions
  }
  renamed <- rn$population(right)
  # Index source identity is observable in index_source and censor tags. A
  # prefix may also change an unnamed source through its named concept set;
  # only an index whose complete source expression is unchanged can share.
  if (!identical(left[["index"]], renamed[["index"]])) return(FALSE)
  inverse <- .merge_renamer(.merge_deferred_verified_maps(nodes, aliases))
  right <- inverse$population(renamed)
  identical(.merge_population_definition(left), .merge_population_definition(right))
}

.merge_deferred_replace_population <- function(r, from, to) {
  maps <- .merge_deferred_empty_maps()
  maps$population <- stats::setNames(to, from)
  rn <- .merge_renamer(maps)
  r$populations[[from]] <- NULL
  r$populations <- lapply(r$populations, rn$population)
  for (id in names(r$blocks %||% list())) {
    r$blocks[[id]]$population_id <- rn$pop(r$blocks[[id]]$population_id %||% "base")
    if (!is.null(r$blocks[[id]]$where)) r$blocks[[id]]$where <- rn$any(r$blocks[[id]]$where)
  }
  r$outputs <- lapply(r$outputs, rn$output)
  for (slot in c("variables", "filters", "events", "intervals", "concept_sets")) {
    if (!is.null(r[[slot]])) r[[slot]] <- lapply(r[[slot]], rn$any)
  }
  r
}

.merge_deferred_reconcile <- function(r) {
  if (!inherits(r, "omop_recipe")) return(r)
  repeat {
    collapsed <- FALSE
    for (id in names(r$populations)) {
      p <- r$populations[[id]]
      marker <- p$simple$merge_base
      if (is.null(marker)) next
      target_id <- marker$target
      target <- r$populations[[target_id]]
      if (identical(id, target_id) || is.null(target)) next
      if (identical(target_id, "base")) target <- .merge_effective_base(r)
      if (.merge_deferred_has_params(list(p, target))) next
      if (!identical(p[["index"]], target[["index"]])) next
      nodes <- .merge_deferred_nodes(r)
      maps <- .merge_deferred_verified_maps(nodes, .merge_deferred_aliases(marker, nodes))
      maps$population <- stats::setNames(target_id, id)
      candidate <- .merge_renamer(maps)$population(p)
      if (!identical(.merge_population_definition(candidate),
                     .merge_population_definition(target))) next
      # A later editor may intentionally use both populations in a set-op.
      # Keeping the equivalent candidates is preferable to creating a
      # duplicate-member set-op that cannot round-trip through its constructor.
      degenerate <- any(vapply(r$populations, function(other) {
        members <- other$setop$members
        if (is.null(members) || !id %in% members) return(FALSE)
        members[members == id] <- target_id
        anyDuplicated(members) > 0L
      }, logical(1)))
      if (degenerate) next
      r <- .merge_deferred_replace_population(r, id, target_id)
      collapsed <- TRUE
      break
    }
    if (!collapsed) return(r)
  }
}
