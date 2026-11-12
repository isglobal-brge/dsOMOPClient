# Module: Recipe language 2.0 -- parameters (design-2.10.3 section 2.10)
#
# A parameter placeholder (omop_param()) stands for a value anywhere a
# language-2 constructor expects one: concept sets, events, integers, numbers,
# strings, dates or windows. Constructors accept placeholders without
# validating them (R/lang_v2.R); recipe_bind() substitutes type-checked values
# and rebuilds every object that held a placeholder through its own
# constructor, so a bound object is validated exactly like a hand-written one.
# recipe_to_plan() binds the remaining placeholders to their defaults and
# refuses any parameter left without one (design 2.10: NULL default =
# unbound). Integer and number placeholders admit + - * / with numeric
# literals and other numeric placeholders (the frozen variants of acceptance
# recipe 13 write `-omop_param(...)` and `omop_param(...) * -1L`); the
# expression is evaluated when its parameters are bound.

.V2_PARAM_TYPES <- c("concept_set", "event", "integer", "number", "string", "date",
                     "window")

#' Recipe parameters: placeholders, binding and merging
#'
#' \code{omop_param()} declares a named placeholder of a fixed type that may
#' stand for a value in any language-2 constructor (an event's concepts, a
#' window bound, an era gap, a matching seed, a predicate literal inside
#' \code{\link{omop_where}}, ...). The same name may appear several times; every
#' occurrence must carry the same definition.
#'
#' \code{recipe_params()} lists the parameters a recipe declares.
#' \code{recipe_bind()} binds values by name: each value is checked against
#' the declared type and substituted, and every object that held a placeholder
#' is rebuilt through its constructor, so a bound recipe is validated exactly
#' like a hand-written one. Parameters left unbound keep their placeholder (a
#' recipe may be bound in stages). \code{\link{recipe_to_plan}} binds the
#' remaining parameters to their defaults and refuses a parameter without a
#' default.
#'
#' Integer and number parameters admit the arithmetic operators \code{+},
#' \code{-}, \code{*} and \code{/} (also unary minus) with numeric literals and
#' other numeric parameters, e.g. \code{omop_window(-omop_param("washout",
#' type = "integer"), -1)}; the expression is evaluated when its parameters are
#' bound and its value is then validated where it is used.
#'
#' Types and the values they accept: \code{"integer"} (an exact integer),
#' \code{"number"} (a finite number), \code{"string"} (one string),
#' \code{"date"} (an ISO date or a \code{Date}), \code{"concept_set"} (an
#' \code{\link{omop_concept_set}}, concept IDs, or the name of a registry
#' concept set), \code{"event"} (an \code{\link{omop_event}}, an
#' \code{\link{omop_era}}, or the name of a declared one) and \code{"window"}
#' (an \code{\link{omop_window}}).
#'
#' @param name Parameter name (a letter, then letters, digits or underscores).
#' @param type Parameter type (see Details).
#' @param default Optional default value of the declared type; \code{NULL}
#'   leaves the parameter unbound until \code{recipe_bind()}.
#' @param description Optional description.
#' @param recipe An \code{omop_recipe}.
#' @param ... \code{recipe_bind()}: values by parameter name
#'   (\code{name = value}).
#' @return \code{omop_param()}: a parameter placeholder.
#'   \code{recipe_params()}: a named list of the declared parameters, in order
#'   of first appearance. \code{recipe_bind()}: the bound recipe.
#' @examples
#' rec <- omop_recipe(
#'   populations = list(omop_population("c1", index = omop_index(
#'     list(cohort_definition_id = omop_param("cohort", type = "integer", default = 1)),
#'     select = "all"))),
#'   variables = list(dm = omop_variable(
#'     event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set")),
#'     reducer = omop_reduce("binary", window = omop_window(-omop_param("lookback",
#'       type = "integer", default = 365L), 0)))),
#'   outputs = list(omop_output("w", rows = "c1", format = "wide")))
#' names(recipe_params(rec))
#' bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE))
#' @seealso \code{\link{recipe_merge}}, \code{\link{recipe_to_plan}}
#' @export
omop_param <- function(name, type = c("concept_set", "event", "integer", "number", "string",
                                      "date", "window"),
                       default = NULL, description = NULL) {
  if (missing(name)) stop("omop_param(): name is required.", call. = FALSE)
  if (.v2_is_param(name)) stop("omop_param(): name must be a string.", call. = FALSE)
  name <- .v2_name(name, "omop_param(): name", allow_null = FALSE)
  type <- .v2_choice(type, .V2_PARAM_TYPES, "omop_param(): type")
  if (!is.null(description) &&
      (!is.character(description) || length(description) != 1L || is.na(description))) {
    stop("omop_param(): description must be one string.", call. = FALSE)
  }
  if (!is.null(default)) {
    default <- .v2_param_check(type, default, paste0("omop_param(\"", name, "\"): default"))
  }
  structure(list(name = name, type = type, default = default, description = description),
            class = c("omop_param", "list"))
}

# A value of a declared parameter type, checked and in canonical form.
.v2_param_check <- function(type, value, field) {
  if (is.null(value)) {
    stop(field, ": NULL leaves a parameter unbound; give a value of type ", type, ".",
         call. = FALSE)
  }
  if (.v2_is_param(value)) {
    stop(field, ": a parameter's value cannot be another parameter.", call. = FALSE)
  }
  switch(type,
    integer = .v2_int(value, field),
    number = .v2_number(value, field),
    string = {
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop(field, " must be one string.", call. = FALSE)
      }
      value
    },
    date = .v2_date(value, field, allow_null = FALSE),
    concept_set = {
      if (inherits(value, "omop_concept_set")) {
        value
      } else if (is.character(value)) {
        .v2_name(value, field, allow_null = FALSE)
      } else if (is.numeric(value) || (is.list(value) && !is.object(value))) {
        .v2_ids(value, field, allow_empty = TRUE)
      } else {
        stop(field, " must be an omop_concept_set(), concept IDs or the name of a ",
             "registry concept set.", call. = FALSE)
      }
    },
    event = {
      if (.v2_is_event(value) || .v2_is_interval(value)) {
        value
      } else if (is.character(value)) {
        .v2_name(value, field, allow_null = FALSE)
      } else {
        stop(field, " must be an omop_event(), an omop_era() or the name of a declared ",
             "one.", call. = FALSE)
      }
    },
    window = {
      if (!inherits(value, "omop_window")) {
        stop(field, " must be an omop_window().", call. = FALSE)
      }
      value
    },
    stop(field, ": unknown parameter type '", type, "'.", call. = FALSE))
}

#' Arithmetic on numeric parameter placeholders
#'
#' \code{+}, \code{-}, \code{*} and \code{/} (and unary minus) combine an
#' integer or number \code{\link{omop_param}} with numeric literals or other
#' numeric parameters into a placeholder expression, evaluated when its
#' parameters are bound. No other operator applies to a parameter.
#'
#' @param e1,e2 Operands.
#' @return A parameter expression (class \code{"omop_param_op"}).
#' @keywords internal
#' @export
Ops.omop_param <- function(e1, e2) {
  op <- .Generic
  if (!op %in% c("+", "-", "*", "/")) {
    stop("omop_param(): only + - * / apply to a parameter (got '", op, "').", call. = FALSE)
  }
  if (nargs() == 1L) {
    if (identical(op, "+")) return(e1)
    if (!identical(op, "-")) stop("omop_param(): unary '", op, "' is not defined.", call. = FALSE)
    return(.v2_param_op("-", e1))
  }
  .v2_param_op(op, e1, e2)
}

# A parameter expression: op over one (unary minus) or two operands, each a
# numeric parameter (or expression) or a finite numeric literal. Its type is
# "number" when any operand is a number or the operator is a division,
# "integer" otherwise.
.v2_param_op <- function(op, e1, e2 = NULL) {
  if (!is.character(op) || length(op) != 1L || !op %in% c("+", "-", "*", "/")) {
    stop("omop_param(): unknown parameter operator.", call. = FALSE)
  }
  operands <- if (is.null(e2)) list(e1) else list(e1, e2)
  if (length(operands) == 1L && !identical(op, "-")) {
    stop("omop_param(): only '-' is a unary parameter operator.", call. = FALSE)
  }
  types <- vapply(operands, function(a) {
    if (.v2_is_param(a)) {
      t <- if (is.character(a$type) && length(a$type) == 1L) a$type else ""
      if (!t %in% c("integer", "number")) {
        stop("omop_param(): arithmetic applies to integer and number parameters only ",
             "(parameter '", a$name %||% "?", "' is ", if (nzchar(t)) t else "untyped", ").",
             call. = FALSE)
      }
      return(t)
    }
    if (!is.numeric(a) || length(a) != 1L || is.na(a) || !is.finite(a)) {
      stop("omop_param(): a parameter combines with finite numeric literals only.",
           call. = FALSE)
    }
    # A whole-number literal keeps an integer expression integer (-1 and -1L
    # alike, so the type survives a JSON round trip).
    if (a == trunc(a) && abs(a) <= .Machine$integer.max) "integer" else "number"
  }, character(1))
  if (!any(vapply(operands, .v2_is_param, logical(1)))) {
    stop("omop_param(): a parameter expression needs a parameter.", call. = FALSE)
  }
  type <- if (identical(op, "/") || any(types == "number")) "number" else "integer"
  structure(list(op = op, args = operands, type = type),
            class = c("omop_param_op", "omop_param", "list"))
}

# Evaluate a parameter expression over bound operand values.
.v2_param_arith <- function(op, vals, type, context) {
  res <- suppressWarnings(if (length(vals) == 1L) -vals[[1L]] else
    switch(op, "+" = vals[[1L]] + vals[[2L]], "-" = vals[[1L]] - vals[[2L]],
           "*" = vals[[1L]] * vals[[2L]], "/" = vals[[1L]] / vals[[2L]]))
  if (length(res) != 1L || is.na(res) || !is.finite(res)) {
    stop(context, ": the parameter expression has no finite value.", call. = FALSE)
  }
  if (identical(type, "integer") && is.numeric(res) && res == trunc(res) &&
      abs(res) <= .Machine$integer.max) {
    res <- as.integer(res)
  }
  res
}

#' @export
print.omop_param <- function(x, ...) {
  if (inherits(x, "omop_param_op")) {
    cat("omop_param expression:", .v2_param_label(x), "\n")
    return(invisible(x))
  }
  cat("omop_param '", x$name, "' (", x$type, if (!is.null(x$default)) {
    paste0(", default ", .v2_param_value_text(x$default))
  }, ")", if (!is.null(x$description)) paste0(": ", x$description), "\n", sep = "")
  invisible(x)
}

.v2_param_value_text <- function(v) {
  if (inherits(v, "omop_concept_set")) {
    return(paste0("concept set ", paste(utils::head(v$concepts, 5L), collapse = ", ")))
  }
  if (inherits(v, "omop_window")) return(.v2_window_text(v))
  if (.v2_is_event(v) || .v2_is_interval(v)) return(.v2_source_label(v))
  paste(format(v), collapse = ", ")
}

# A short label of a placeholder or expression (print, explain).
.v2_param_label <- function(x) {
  if (!inherits(x, "omop_param_op")) return(paste0("<param ", x$name %||% "?", ">"))
  arg <- function(a) {
    if (!.v2_is_param(a)) return(format(a))
    if (inherits(a, "omop_param_op")) paste0("(", .v2_param_label(a), ")") else .v2_param_label(a)
  }
  if (length(x$args) == 1L) return(paste0("-", arg(x$args[[1L]])))
  paste(arg(x$args[[1L]]), x$op, arg(x$args[[2L]]))
}

# --- binding -------------------------------------------------------------------------

# Resolve a placeholder through `lookup` (a function of a leaf parameter
# returning its value, or NULL to leave it unbound). A partly bound expression
# keeps its placeholder with the bound operands substituted.
.v2_param_resolve <- function(p, lookup, context) {
  if (inherits(p, "omop_param_op")) {
    args <- lapply(p$args, function(a) {
      if (.v2_is_param(a)) .v2_param_resolve(a, lookup, context) else a
    })
    if (any(vapply(args, .v2_is_param, logical(1)))) {
      p$args <- args
      return(p)
    }
    return(.v2_param_arith(p$op, args, p$type, context))
  }
  value <- lookup(p)
  if (is.null(value)) p else value
}

# Substitute placeholders throughout `x`; every language-2 object that held
# one is rebuilt through its constructor (bottom up), so bound values are
# validated where they are used.
.v2_bind_walk <- function(x, lookup, context) {
  if (.v2_is_param(x)) {
    value <- .v2_param_resolve(x, lookup, context)
    return(list(value = value, changed = !identical(value, x)))
  }
  if (!is.list(x) || is.data.frame(x)) return(list(value = x, changed = FALSE))
  changed <- FALSE
  out <- x
  for (i in seq_along(x)) {
    el <- x[[i]]
    if (is.null(el)) next
    r <- .v2_bind_walk(el, lookup, context)
    if (isTRUE(r$changed)) {
      changed <- TRUE
      out[[i]] <- r$value
    }
  }
  if (changed && .v2_is_tagged(out)) out <- .v2_rebuild(out)
  list(value = out, changed = changed)
}

# Rebuild a language-2 object through its constructor (re-validating it).
.v2_rebuild <- function(x) {
  spec <- .v2_ctor_spec(x)
  args <- spec$args
  # A NULL stored argument always equals its constructor default.
  if (!is.null(names(args))) args <- args[!vapply(args, is.null, logical(1))]
  obj <- do.call(spec$ctor, args)
  if (!is.null(spec$name)) obj$name <- spec$name
  obj
}

# Bind every placeholder to its default; refuse a parameter without one.
.v2_resolve_params <- function(x, context = "the recipe") {
  lookup <- function(p) {
    if (!is.null(p$default)) return(p$default)
    .v2_stop("Unbound parameter '", p$name %||% "?", "' in ", context,
             "; bind it with recipe_bind() before recipe_to_plan().")
  }
  .v2_bind_walk(x, lookup, context)$value
}

# The value of one placeholder (or expression) bound to its defaults.
.v2_param_value <- function(x, context) .v2_resolve_params(x, context)

# Calls fn on every leaf parameter of x (inside expressions too).
.v2_walk_params <- function(x, fn) {
  if (inherits(x, "omop_param_op")) {
    for (a in x$args) .v2_walk_params(a, fn)
    return(invisible(NULL))
  }
  if (.v2_is_param(x)) {
    fn(x)
    return(invisible(NULL))
  }
  if (is.list(x) && !is.data.frame(x)) for (el in x) .v2_walk_params(el, fn)
  invisible(NULL)
}

.v2_param_definition <- function(p) {
  list(name = p$name, type = p$type, default = p$default, description = p$description)
}

#' @rdname omop_param
#' @export
recipe_params <- function(recipe) {
  .v2_recipe_check(recipe)
  found <- list()
  .v2_walk_params(recipe[setdiff(names(recipe), "meta")], function(p) {
    nm <- p$name
    if (!is.character(nm) || length(nm) != 1L) {
      stop("recipe_params(): a parameter placeholder has no name.", call. = FALSE)
    }
    prev <- found[[nm]]
    if (is.null(prev)) {
      found[[nm]] <<- p
    } else if (!identical(.v2_param_definition(prev), .v2_param_definition(p))) {
      stop("Parameter '", nm, "' is declared with different definitions (type, default ",
           "or description); use one omop_param() definition per name.", call. = FALSE)
    }
  })
  found
}

#' @rdname omop_param
#' @export
recipe_bind <- function(recipe, ...) {
  .v2_recipe_check(recipe)
  values <- list(...)
  if (length(values) == 0L) return(recipe)
  nms <- names(values)
  if (is.null(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    stop("recipe_bind(): give each parameter value once, by name (name = value).",
         call. = FALSE)
  }
  params <- recipe_params(recipe)
  unknown <- setdiff(nms, names(params))
  if (length(unknown)) {
    stop("recipe_bind(): ", paste0("'", unknown, "'", collapse = ", "),
         if (length(unknown) == 1L) " is not a parameter" else " are not parameters",
         " of the recipe (its parameters: ",
         if (length(params)) paste(names(params), collapse = ", ") else "none", ").",
         call. = FALSE)
  }
  for (nm in nms) {
    values[nm] <- list(.v2_param_check(params[[nm]]$type, values[[nm]],
                                       paste0("recipe_bind(): parameter '", nm, "'")))
  }
  lookup <- function(p) values[[p$name]]
  out <- .v2_bind_walk(recipe, lookup, "recipe_bind()")$value
  out$meta$modified <- Sys.time()
  out
}
