# Module: Recipe language 2.0 -- recipe_merge() (design-2.10.3 section 2.10)
#
# recipe_merge(a, b) joins two recipes into one. Without a prefix both share
# one namespace: a name used by both must carry the same definition. With a
# prefix, every node of `b` -- concept sets, events and intervals (declared or
# named inline), populations, variables, blocks and outputs -- is renamed
# `<prefix>_<name>` and every reference to it is rewritten: concept-set and
# event references, anchors, censoring sources and their index-source tags,
# index arms, parents, set-operation members, match inputs, output rows and
# columns, output dependencies (joined inputs, dictionary sources,
# carry-forward and source-analysis keys) and publication roots. Block
# metadata is preserved: a renamed block keeps its expansion, its variables
# keep their block_id (the renamed block) and the block records the variable
# prefix so it re-expands to the same names. Parameters are shared by name
# (recipe_params() refuses two definitions of one name).

#' @rdname omop_param
#' @param a,b Recipes to merge (\code{recipe_merge()}).
#' @param prefix \code{NULL} (one shared namespace) or a name: every node of
#'   \code{b} is renamed \code{<prefix>_<name>} and every reference to it is
#'   rewritten.
#' @details \code{recipe_merge(a, b, prefix = NULL)} joins two recipes. Without
#'   a prefix both share one namespace: a name used by both must carry the same
#'   definition, and their base populations, recipe-level filters, scopes,
#'   plan options and coverage defaults must agree. With a prefix every node
#'   of \code{b} (concept sets, events, intervals, populations, variables,
#'   blocks, outputs) is renamed and every reference rewritten -- anchors,
#'   censoring (and its \code{when_index_source} tags), index arms, parents,
#'   set-operation members, match inputs, output rows and columns, output
#'   dependencies and publication roots; \code{b}'s base population keeps its
#'   meaning (its recipe-level filters move onto it) and is shared only when it
#'   equals \code{a}'s. When equality depends on parameters, it is reconsidered
#'   after binding or default resolution against the current population and
#'   source definitions, preserving edits made after merging. Outputs whose
#'   columns were implicit (every variable)
#'   are made explicit first, so each output keeps exactly its own variables.
#'   Block metadata (\code{block_id}, \code{block_version}) is preserved.
#'   Parameters are shared by name.
#' @export
recipe_merge <- function(a, b, prefix = NULL) {
  .v2_recipe_check(a)
  .v2_recipe_check(b)
  a <- .merge_freeze_columns(a)
  b <- .merge_freeze_columns(b)
  if (!is.null(prefix)) {
    prefix <- .v2_name(prefix, "recipe_merge(): prefix", allow_null = FALSE)
    if (nchar(prefix) > 62L) stop("recipe_merge(): prefix is too long.", call. = FALSE)
    share_base <- .merge_same_base(a, b)
    defer_base <- !share_base && .merge_deferred_needed(a, b)
    if (share_base && is.null(a$populations$base$simple)) {
      a$populations$base$simple <- b$populations$base$simple
    }
    b <- .merge_prefix(b, prefix, share_base = share_base, defer_base = defer_base)
    if (share_base) b$filters <- a$filters
  }
  .merge_into(a, b)
}

# --- shared helpers --------------------------------------------------------------------

# Population-level recipe filters (criteria and legacy population filters).
.merge_recipe_pop_filters <- function(r) {
  Filter(function(f) inherits(f, "omop_criterion") ||
           (inherits(f, c("omop_filter", "omop_filter_group")) &&
              length(.extract_filters_by_level(list(f), "population"))),
         r$filters %||% list())
}

.merge_recipe_row_filters <- function(r) {
  .extract_filters_by_level(r$filters %||% list(), "row")
}

# The base population with its recipe-level population filters, as one
# comparable definition.
.merge_effective_base <- function(r) {
  base <- r$populations$base
  base$filters <- c(base$filters %||% list(), unname(.merge_recipe_pop_filters(r)))
  base
}

# Resolved helper choices explain a population but do not change its
# definition. A pending expansion still carries criteria that must be bound.
.merge_population_definition <- function(p) {
  if (!isTRUE(p$simple$new_users$pending)) p$simple <- NULL
  unclass(p)
}

.merge_same_base <- function(a, b) {
  .merge_base_semantics_equal(a, b) &&
    identical(unname(.merge_recipe_row_filters(a)), unname(.merge_recipe_row_filters(b)))
}

# Outputs that take every recipe variable when their columns are omitted get
# their own variables explicitly, so a merge never adds the other recipe's.
.merge_freeze_columns <- function(r) {
  vars <- names(r$variables %||% list())
  if (!length(vars)) return(r)
  for (nm in names(r$outputs %||% list())) {
    o <- r$outputs[[nm]]
    implicit <- is.null(o$variables) && (is.null(o$format) || identical(o$format, "wide"))
    if (implicit) r$outputs[[nm]]$variables <- vars
  }
  r
}

# Every named event / interval / concept set, including parameter defaults.
.merge_named_nodes <- function(x, out = list()) {
  if (.v2_is_param(x)) {
    if (inherits(x, "omop_param_op")) {
      for (a in x$args) out <- .merge_named_nodes(a, out)
      return(out)
    }
    if (!is.null(x$default)) {
      out <- .merge_named_nodes(.v2_param_remap(x, x$default), out)
    }
    return(out)
  }
  if ((.v2_is_event(x) || .v2_is_interval(x) || inherits(x, "omop_concept_set")) &&
      is.character(x$name)) {
    kind <- if (.v2_is_event(x)) "event" else if (.v2_is_interval(x)) "interval" else "concept_set"
    key <- paste0(kind, ":", x$name)
    out[[key]] <- c(out[[key]], list(x))
  }
  if (is.list(x) && !is.data.frame(x)) for (el in x) out <- .merge_named_nodes(el, out)
  out
}

# --- renaming -------------------------------------------------------------------------------

.merge_prefix <- function(r, prefix, share_base, defer_base = FALSE) {
  f <- function(nm) {
    out <- paste0(prefix, "_", nm)
    if (nchar(out) > 64L) {
      stop("recipe_merge(): '", out, "' exceeds the 64-character name grammar; use a ",
           "shorter prefix.", call. = FALSE)
    }
    out
  }
  pops <- setdiff(names(r$populations %||% list()), if (share_base) "base")
  maps <- list(
    population = stats::setNames(vapply(pops, f, ""), pops),
    variable = stats::setNames(vapply(names(r$variables %||% list()), f, ""),
                               names(r$variables %||% list())),
    output = stats::setNames(vapply(names(r$outputs %||% list()), f, ""),
                             names(r$outputs %||% list())),
    block = stats::setNames(vapply(names(r$blocks %||% list()), f, ""),
                            names(r$blocks %||% list())),
    concept_set = stats::setNames(vapply(names(r$concept_sets %||% list()), f, ""),
                                  names(r$concept_sets %||% list())))
  nodes <- names(.merge_named_nodes(r))
  ev <- unique(c(sub("^event:", "", grep("^event:", nodes, value = TRUE)),
                 names(r$events %||% list())))
  iv <- unique(c(sub("^interval:", "", grep("^interval:", nodes, value = TRUE)),
                 names(r$intervals %||% list())))
  ev <- ev[nzchar(ev)]
  iv <- iv[nzchar(iv)]
  maps$event <- stats::setNames(vapply(ev, f, ""), ev)
  maps$interval <- stats::setNames(vapply(iv, f, ""), iv)
  cs <- unique(c(sub("^concept_set:", "", grep("^concept_set:", nodes, value = TRUE)),
                 names(r$concept_sets %||% list())))
  maps$concept_set <- stats::setNames(vapply(cs, f, ""), cs)
  rewrite_names <- function(x) {
    if (!length(x)) return(x)
    replacements <- c(maps$variable, maps$event, maps$interval)
    replacements <- replacements[!duplicated(names(replacements))]
    stems <- names(replacements)[order(nchar(names(replacements)), decreasing = TRUE)]
    keys <- names(x)
    for (i in seq_along(keys)) for (stem in stems) {
      if (identical(keys[[i]], stem) || startsWith(keys[[i]], paste0(stem, "_"))) {
        keys[[i]] <- paste0(replacements[[stem]], substring(keys[[i]], nchar(stem) + 1L))
        break
      }
    }
    names(x) <- keys
    x
  }
  r$options$column_names <- rewrite_names(r$options$column_names)
  for (vn in names(r$variables)) r$variables[[vn]]$column_names <- rewrite_names(r$variables[[vn]]$column_names)
  rn <- .merge_renamer(maps, prefix = prefix)

  # b's recipe-level filters keep constraining b's base: they move onto it.
  if (!share_base) {
    rows <- .merge_recipe_row_filters(r)
    if (length(rows)) {
      stop("recipe_merge(): recipe-level row filters of the recipe being prefixed apply ",
           "to every output of the merged recipe; put them on its variables or events ",
           "(where =) first.", call. = FALSE)
    }
    r$populations$base$filters <- c(r$populations$base$filters %||% list(),
                                    unname(.merge_recipe_pop_filters(r)))
    r$filters <- list()
  }

  out <- r
  out$populations <- list()
  for (id in names(r$populations)) {
    if (share_base && identical(id, "base")) next
    p <- rn$population(r$populations[[id]])
    out$populations[[p$id]] <- p
  }
  out$blocks <- list()
  for (bid in names(r$blocks %||% list())) {
    blk <- r$blocks[[bid]]
    blk$id <- maps$block[[bid]]
    blk$population_id <- rn$pop(blk$population_id %||% "base")
    # The expansion keeps the prefixed variable names (recipe_add_block()).
    blk$name_prefix <- paste0(prefix, if (!is.null(blk$name_prefix)) paste0("_", blk$name_prefix))
    if (!is.null(blk$where)) blk$where <- rn$any(blk$where)
    out$blocks[[blk$id]] <- blk
  }
  out$variables <- list()
  for (vn in names(r$variables %||% list())) {
    v <- rn$any(r$variables[[vn]])
    v$name <- maps$variable[[vn]]
    if (!is.null(v$block_id) && !is.null(maps$block[[v$block_id]])) {
      v$block_id <- maps$block[[v$block_id]]
    }
    out$variables[[v$name]] <- v
  }
  out$filters <- lapply(r$filters %||% list(), rn$any)
  out$outputs <- list()
  for (on in names(r$outputs %||% list())) {
    o <- rn$output(r$outputs[[on]])
    out$outputs[[o$name]] <- o
  }
  if (length(r$concept_sets %||% list())) {
    out$concept_sets <- list()
    for (cn in names(r$concept_sets)) {
      cs <- rn$any(r$concept_sets[[cn]])
      cs$name <- maps$concept_set[[cn]]
      out$concept_sets[[cs$name]] <- cs
    }
  }
  if (length(r$events %||% list())) {
    out$events <- lapply(r$events, rn$any)
    names(out$events) <- vapply(out$events, function(e) e$name %||% "", "")
  }
  if (length(r$intervals %||% list())) {
    out$intervals <- lapply(r$intervals, rn$any)
    names(out$intervals) <- vapply(out$intervals, function(e) e$name %||% "", "")
  }
  if (!is.null(r$publish)) {
    out$publish <- if (.v2_is_param(r$publish)) rn$out(r$publish) else lapply(r$publish, rn$out)
    out <- recipe_set_publish(out, out$publish)
  }
  if (defer_base) out <- .merge_deferred_mark(out, maps, prefix)
  out
}

# Reference rewriting for one renaming. Each function handles one kind of
# object; plain strings are renamed only where the field is a reference.
.merge_renamer <- function(maps, prefix = NULL, deferred = FALSE) {
  look <- function(kind, x) {
    if (!is.character(x) || length(x) != 1L || is.na(x)) return(x)
    hit <- maps[[kind]][x]
    if (length(hit) && !is.na(hit)) return(unname(hit))
    if (deferred && !is.null(prefix) && kind %in% c("event", "interval", "concept_set")) {
      return(.v2_name(paste0(prefix, "_", x), "recipe_merge(): deferred reference",
                      allow_null = FALSE))
    }
    x
  }
  self <- new.env(parent = emptyenv())
  self$defer <- function(x, role = x$type) {
    # A reconciliation rewrites known aliases without introducing a namespace.
    # Do not decorate unrelated placeholders with an empty transformation.
    kinds <- switch(role, population = "population", output = "output",
      variable = "variable", tag = c("population", "event", "interval"),
      c("event", "interval", "concept_set"))
    if (is.null(prefix) && !any(vapply(maps[kinds], length, integer(1)) > 0L)) return(x)
    attr(x, "omop_namespace") <- c(attr(x, "omop_namespace", exact = TRUE) %||% list(),
                                   list(list(prefix = prefix, maps = maps, role = role)))
    x
  }
  self$pop <- function(x) if (.v2_is_param(x)) self$defer(x, "population") else look("population", x)
  self$out <- function(x) if (.v2_is_param(x)) self$defer(x, "output") else look("output", x)
  self$var <- function(x) if (.v2_is_param(x)) self$defer(x, "variable") else look("variable", x)
  # A whole placeholder is one value, even though its storage is a list.
  refs <- function(x, fn) {
    if (.v2_is_param(x)) return(fn(x))
    if (is.list(x)) return(lapply(x, fn))
    unname(vapply(x, fn, ""))
  }
  sources <- function(x) {
    if (.v2_is_param(x) || .v2_is_event(x) || .v2_is_interval(x)) return(self$src(x))
    refs(x, self$src)
  }
  # A reference by name to an event or an interval (sources accept both).
  self$src <- function(x) {
    if (.v2_is_param(x)) return(self$defer(x, "event"))
    if (is.character(x) && length(x) == 1L) {
      ev <- maps$event[x]
      iv <- maps$interval[x]
      if (length(ev) && !is.na(ev)) return(unname(ev))
      if (length(iv) && !is.na(iv)) return(unname(iv))
      return(if (deferred) look("event", x) else x)
    }
    self$any(x)
  }
  self$tag <- function(t) {
    if (.v2_is_param(t)) return(self$defer(t, "tag"))
    if (startsWith(t, "cohort:")) return(t)
    if (startsWith(t, "match:")) return(paste0("match:", self$pop(sub("^match:", "", t))))
    self$src(t)
  }
  self$censor <- function(x) {
    if (.v2_is_param(x)) return(self$defer(x, "censor"))
    if (is.character(x) && length(x) == 1L &&
        x %in% c("observation_end", "death", "index_end")) return(x)
    if (is.list(x) && identical(names(x), "admin_date")) return(x)
    self$src(x)
  }
  # Apply fn to field k of x when it is set (never turning a NULL into a
  # removed field or vice versa).
  upd <- function(x, k, fn) {
    if (!is.null(x[[k]])) x[[k]] <- fn(x[[k]])
    x
  }
  self$any <- function(x) {
    if (is.null(x)) return(x)
    if (.v2_is_param(x)) {
      if (x$type %in% c("event", "concept_set", "window")) {
        x <- self$defer(x)
      }
      return(x)
    }
    if (inherits(x, "omop_concept_set")) {
      if (is.character(x$name)) x$name <- look("concept_set", x$name)
      return(upd(x, "exclude", self$any))
    }
    if (.v2_is_event(x)) {
      if (is.character(x$name)) x$name <- look("event", x$name)
      kind <- x$kind %||% "table"
      if (identical(kind, "table")) {
        x <- upd(x, "concepts", function(cs) {
          if (.v2_is_param(cs)) return(self$defer(cs, "concept_set"))
          if (is.character(cs)) look("concept_set", cs) else self$any(cs)
        })
      }
      if (identical(kind, "episode_members")) x <- upd(x, "episode", self$src)
      if (identical(kind, "cost")) x <- upd(x, "event", self$src)
      x <- upd(x, "simple", self$any)
      return(upd(x, "where", self$any))
    }
    if (.v2_is_interval(x)) {
      if (is.character(x$name)) x$name <- look("interval", x$name)
      return(upd(x, "event", self$src))
    }
    if (inherits(x, "omop_criterion")) {
      x <- upd(x, "event", self$src)
      for (k in c("window", "criterion", "where")) x <- upd(x, k, self$any)
      return(upd(x, "criteria", function(cr) lapply(cr, self$any)))
    }
    if (inherits(x, "omop_anchor")) {
      x <- upd(x, "event", self$src)
      return(upd(x, "search", self$any))
    }
    if (inherits(x, "omop_window")) {
      for (k in c("start", "end", "anchor")) x <- upd(x, k, self$any)
      return(x)
    }
    if (inherits(x, "omop_bound")) return(upd(x, "anchor", self$any))
    if (inherits(x, "omop_index")) {
      src <- x$source
      if (inherits(src, "omop_any_of")) {
        x$source <- self$any(src)
      } else if (!(is.list(src) && identical(names(src), "cohort_definition_id"))) {
        x$source <- self$src(src)
      }
      return(x)
    }
    if (inherits(x, "omop_any_of")) return(upd(x, "arms", function(a) lapply(a, self$src)))
    if (inherits(x, "omop_censor")) {
      x <- upd(x, "source", self$src)
      x <- upd(x, "search", self$any)
      if (!is.null(x$when_index_source)) x$when_index_source <- refs(x$when_index_source, self$tag)
      return(x)
    }
    if (inherits(x, "omop_match")) {
      x <- upd(x, "cases", self$pop)
      x <- upd(x, "controls", self$pop)
      x <- upd(x, "outcome", self$src)
      return(upd(x, "match", function(m) lapply(m, self$any)))
    }
    if (inherits(x, "omop_match_item")) {
      x <- upd(x, "event", self$src)
      return(upd(x, "window", self$any))
    }
    if (inherits(x, "omop_reducer")) {
      for (k in c("window", "reference", "simple")) x <- upd(x, k, self$any)
      return(x)
    }
    if (inherits(x, "omop_variable") && .v2_is_v2_variable(x)) {
      x <- upd(x, "event", self$src)
      for (k in c("reducer", "where")) x <- upd(x, k, self$any)
      if (!is.null(x$derived$window)) x$derived$window <- self$any(x$derived$window)
      return(x)
    }
    if (inherits(x, c("omop_predicate", "omop_concept_set", "omop_unit", "omop_end",
                      "omop_coverage", "omop_filter", "omop_filter_group", "omop_variable",
                      "omop_index_event"))) {
      return(x)
    }
    if (is.list(x) && !is.data.frame(x)) {
      for (i in seq_along(x)) if (!is.null(x[[i]])) x[[i]] <- self$any(x[[i]])
      return(x)
    }
    x
  }
  self$population <- function(p) {
    p$id <- self$pop(p$id)
    if (!is.null(p$parent_id)) p$parent_id <- self$pop(p$parent_id)
    if (!is.null(p$setop)) p$setop$members <- refs(p$setop$members, self$pop)
    p$filters <- lapply(p$filters %||% list(), self$any)
    if (!is.null(p[["index"]])) p[["index"]] <- self$any(p[["index"]])
    if (!is.null(p$match)) p$match <- self$any(p$match)
    if (!is.null(p$simple)) {
      p$simple <- self$any(p$simple)
      if (!is.null(p$simple$merge_base)) {
        p$simple$merge_base <- .merge_deferred_remap(p$simple$merge_base, maps, prefix)
      }
    }
    if (!is.null(p$circe_v2)) {
      p$circe_v2$index <- self$any(p$circe_v2$index)
      p$circe_v2$filters <- lapply(p$circe_v2$filters %||% list(), self$any)
    }
    p
  }
  self$output <- function(o) {
    old <- o$name
    o$name <- self$out(old)
    if (identical(o$result_symbol, paste0("D_", old))) o$result_symbol <- paste0("D_", o$name)
    o$population_id <- self$pop(o$population_id %||% "base")
    if (!is.null(o$variables)) o$variables <- refs(o$variables, self$var)
    if (!is.null(o$sources)) o$sources <- sources(o$sources)
    opts <- o$options %||% list()
    named_refs <- function(x) {
      if (is.null(x)) return(x)
      if (.v2_is_param(x)) return(self$src(x))
      nms <- names(x)
      out <- lapply(x, self$src)
      names(out) <- nms
      out
    }
    if (!is.null(opts$outcomes)) opts$outcomes <- named_refs(opts$outcomes)
    if (!is.null(opts$exposures)) opts$exposures <- named_refs(opts$exposures)
    if (!is.null(opts$sources)) opts$sources <- sources(opts$sources)
    for (k in c("tar", "entry", "window", "time_window", "anchor")) opts <- upd(opts, k, self$any)
    if (!is.null(opts$tars)) opts$tars <- lapply(opts$tars, self$any)
    if (!is.null(opts$anchors)) opts$anchors <- lapply(opts$anchors, self$any)
    if (!is.null(opts$censor)) {
      cz <- opts$censor
      opts$censor <- if (.v2_is_param(cz) ||
                        inherits(cz, c("omop_censor", "omop_event", "omop_interval"))) {
        self$censor(cz)
      } else lapply(if (is.character(cz)) as.list(cz) else cz, self$censor)
    }
    if (!is.null(opts$relations)) {
      opts$relations <- lapply(opts$relations, function(rel) {
        if (!is.null(rel$event)) rel$event <- self$src(rel$event)
        if (!is.null(rel$source)) rel$source <- self$src(rel$source)
        rel
      })
    }
    if (!is.null(opts$partition$boundaries)) {
      opts$partition$boundaries <- sources(opts$partition$boundaries)
    }
    for (k in intersect(names(opts), c("carry_forward", "source_analyses"))) {
      if (length(opts[[k]])) names(opts[[k]]) <- unname(vapply(names(opts[[k]]), self$src, ""))
    }
    if (!is.null(opts$from)) {
      from <- function(fr) {
        if (.v2_is_param(fr) || is.character(fr)) return(self$out(fr))
        fr$output <- self$out(fr$output)
        fr
      }
      opts$from <- if (.v2_is_param(opts$from)) from(opts$from) else lapply(opts$from, from)
    }
    if (!is.null(opts$source_outputs)) {
      opts$source_outputs <- refs(opts$source_outputs, self$out)
    }
    o$options <- opts
    o
  }
  self
}

# --- joining ------------------------------------------------------------------------------

.merge_into <- function(a, b) {
  conflicts <- character(0)
  join <- function(slot, what) {
    x <- a[[slot]] %||% list()
    y <- b[[slot]] %||% list()
    ynm <- names(y) %||% rep("", length(y))
    for (i in seq_along(y)) {
      nm <- ynm[[i]]
      if (!nzchar(nm)) {
        # An unnamed registry event is interned by its definition.
        x <- c(x, y[i])
        next
      }
      if (is.null(x[[nm]])) {
        x[[nm]] <- y[[nm]]
      } else {
        definition <- if (identical(slot, "populations")) .merge_population_definition else unclass
        if (!identical(definition(x[[nm]]), definition(y[[nm]]))) {
          conflicts <<- c(conflicts, paste0(what, " '", nm, "'"))
        } else if (identical(slot, "populations") && is.null(x[[nm]]$simple) &&
                   !is.null(y[[nm]]$simple)) {
          x[[nm]]$simple <- y[[nm]]$simple
        }
      }
    }
    x
  }
  out <- a
  out$populations <- join("populations", "population")
  out$blocks <- join("blocks", "block")
  out$variables <- join("variables", "variable")
  out$outputs <- join("outputs", "output")
  for (slot in c("concept_sets", "events", "intervals")) {
    joined <- join(slot, sub("s$", "", sub("_", " ", slot)))
    out[[slot]] <- if (length(joined)) joined else NULL
  }
  # Named inline events / intervals / concept sets share their kind namespace.
  na <- .merge_named_nodes(a)
  nb <- .merge_named_nodes(b)
  for (key in intersect(names(na), names(nb))) {
    if (!identical(unclass(na[[key]][[1L]]), unclass(nb[[key]][[1L]]))) {
      conflicts <- c(conflicts, sub(":", " '", paste0(key, "'")))
    }
  }
  # Recipe-level filters, scope, plan options and the coverage default apply
  # to the whole recipe: they must agree.
  fa <- unname(a$filters %||% list())
  fb <- unname(b$filters %||% list())
  if (length(fb) && !identical(fa, fb)) {
    conflicts <- c(conflicts, "recipe-level filters (they constrain the shared base population)")
  } else if (length(fa) && !length(fb) && !is.null(b$populations$base) &&
             any(vapply(b$outputs %||% list(), function(o) identical(o$population_id %||% "base", "base"),
                        logical(1)))) {
    conflicts <- c(conflicts, "recipe-level filters (they would constrain the second recipe's base population)")
  }
  if (!identical(a$scope, b$scope)) conflicts <- c(conflicts, "the recipe scope (cohort/tables)")
  for (key in c("labels", "column_names")) {
    ma <- a$options[[key]]; mb <- b$options[[key]]
    common <- intersect(names(ma), names(mb))
    if (length(common) && !identical(unname(ma[common]), unname(mb[common]))) {
      conflicts <- c(conflicts, paste0("options.", key))
    }
    combined <- .conceptMapOverlay(ma, mb)
    if (length(combined)) a$options[[key]] <- b$options[[key]] <- combined
  }
  out$options <- a$options
  if (!identical(a$options %||% list(), b$options %||% list())) conflicts <- c(conflicts, "the plan options")
  if (!identical(a$coverage, b$coverage)) conflicts <- c(conflicts, "the recipe coverage default")
  if (length(conflicts)) {
    stop("recipe_merge(): the recipes disagree on ", paste(unique(conflicts), collapse = "; "),
         ". Rename (prefix = ) or align them first.", call. = FALSE)
  }
  if (!is.null(a$publish) || !is.null(b$publish)) {
    roots <- function(r) {
      x <- r$publish %||% names(r$outputs %||% list())
      if (.v2_is_param(x)) list(x) else as.list(x)
    }
    out <- recipe_set_publish(out, unique(c(roots(a), roots(b))))
  }
  out$meta <- list(created = Sys.time(), modified = Sys.time())
  # Parameters are shared by name: two definitions of one name are refused.
  recipe_params(out)
  out
}
