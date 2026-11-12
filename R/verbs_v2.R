# Module: Recipe language 2.0 -- standalone row verbs (design-2.10.3 sections
# 2.11 and 4.6): ds.omop.mutate() and the expr form of ds.omop.filter().
#
# The client translates R expressions into the closed mutation AST of the
# vendored inst/schema/mutate-v1.json (or the plan-v2 predicate grammar for a
# filter) and never evaluates them: bare names are columns, literals are
# numbers, strings, logicals, NA and as.Date("YYYY-MM-DD"), and only the
# listed operators and functions are recognised. The request is validated
# against the vendored schema before it is sent; the server validates it again
# and type-checks it against the frame before any row is touched. Both verbs
# need the servers to advertise their operation (verb.mutate,
# verb.filter_expr; design 4.4).

.MUTATE_NAME_PATTERN <- "^[A-Za-z][A-Za-z0-9_]{0,63}$"

.mutate_stop <- function(...) stop("ds.omop.mutate(): ", ..., call. = FALSE)

# Every server must advertise the verb's operation (dsOMOP 2.10.3).
.verb_assert_supported <- function(session, servers, op, what) {
  lang <- .v2_server_language(session, servers)
  ok <- vapply(lang, function(rl) !is.null(rl) && op %in% rl$operations, logical(1))
  if (!all(ok)) {
    stop(what, " needs the '", op, "' operation, which ",
         paste(servers[!ok], collapse = ", "), if (sum(!ok) == 1L) " does" else " do",
         " not advertise (it is part of dsOMOP 2.10.3). Upgrade dsOMOP on ",
         if (sum(!ok) == 1L) "that server." else "those servers.", call. = FALSE)
  }
  invisible(TRUE)
}

.mutate_column <- function(s) {
  name <- as.character(s)
  if (!grepl(.MUTATE_NAME_PATTERN, name)) {
    .mutate_stop("'", name, "' is not a column name (a letter, then letters, digits or ",
                 "underscores).")
  }
  list(col = name)
}

# A literal value of an expression or predicate (no evaluation).
.mutate_literal <- function(e) {
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  if (is.call(e) && identical(e[[1L]], as.name("-")) && length(e) == 2L && is.numeric(e[[2L]])) {
    e <- -e[[2L]]
  }
  if (is.call(e) && identical(e[[1L]], as.name("as.Date"))) {
    if (length(e) != 2L || !is.character(e[[2L]])) {
      .mutate_stop("as.Date() takes one literal ISO date.")
    }
    return(list(lit = .plan_iso_date(e[[2L]], "ds.omop.mutate(): as.Date()"), as = "date"))
  }
  if (!is.atomic(e) || length(e) != 1L) return(NULL)
  if (is.na(e)) {
    as <- if (is.double(e)) "numeric" else if (is.integer(e)) "integer" else
      if (is.character(e)) "text" else NULL
    return(c(list(lit = NULL), if (!is.null(as)) list(as = as)))
  }
  if (is.integer(e)) return(list(lit = e, as = "integer"))
  if (is.numeric(e)) {
    if (!is.finite(e)) .mutate_stop("numeric literals must be finite.")
    return(list(lit = e))
  }
  if (is.character(e) || is.logical(e)) return(list(lit = e))
  NULL
}

# A scalar literal of a predicate leaf.
.mutate_pred_value <- function(e) {
  lit <- .mutate_literal(e)
  if (is.null(lit) || is.null(lit$lit)) {
    .mutate_stop("a comparison compares one column with one literal (number, string, ",
                 "TRUE/FALSE or as.Date(\"YYYY-MM-DD\")); assign an expression to a column ",
                 "first to compare it.")
  }
  lit$lit
}

# R call -> plan-v2 predicate (the omop_where() grammar over assigned names).
.mutate_pred <- function(e) {
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  if (!is.call(e)) .mutate_stop("expected a condition (==, <, %in%, is.na(), &, |, !).")
  fn <- as.character(e[[1L]])[[1L]]
  if (fn %in% c("&", "|")) {
    op <- if (fn == "&") "and" else "or"
    return(.v2_pred_flatten(op, list(.mutate_pred(e[[2L]]), .mutate_pred(e[[3L]]))))
  }
  if (fn %in% c("&&", "||")) .mutate_stop("use & and | (vectorised), not ", fn, ".")
  if (identical(fn, "!")) {
    inner <- e[[2L]]
    while (is.call(inner) && identical(inner[[1L]], as.name("("))) inner <- inner[[2L]]
    if (is.call(inner) && identical(inner[[1L]], as.name("is.na"))) {
      return(list(var = .mutate_column(inner[[2L]])$col, op = "not_null"))
    }
    if (is.call(inner) && identical(inner[[1L]], as.name("%in%"))) {
      leaf <- .mutate_pred(inner)
      leaf$op <- "not_in"
      return(leaf)
    }
    return(list(not = .mutate_pred(inner)))
  }
  if (identical(fn, "is.na")) {
    if (length(e) != 2L || !is.symbol(e[[2L]])) .mutate_stop("is.na() takes one column.")
    return(list(var = .mutate_column(e[[2L]])$col, op = "is_null"))
  }
  if (fn %in% .V2_CMP_OPS) {
    lhs <- e[[2L]]; rhs <- e[[3L]]
    if (is.symbol(lhs) && !is.symbol(rhs)) {
      return(list(var = .mutate_column(lhs)$col, op = fn, value = .mutate_pred_value(rhs)))
    }
    if (is.symbol(rhs) && !is.symbol(lhs)) {
      return(list(var = .mutate_column(rhs)$col, op = unname(.V2_CMP_FLIP[[fn]]),
                  value = .mutate_pred_value(lhs)))
    }
    .mutate_stop("a comparison compares one column with one literal; assign an expression to ",
                 "a column first to compare it.")
  }
  if (identical(fn, "%in%")) {
    if (!is.symbol(e[[2L]])) .mutate_stop("the left side of %in% must be a column.")
    rhs <- e[[3L]]
    items <- if (is.call(rhs) && identical(rhs[[1L]], as.name("c"))) as.list(rhs)[-1L] else list(rhs)
    values <- lapply(items, .mutate_pred_value)
    if (!length(values) || length(values) > 10000L) .mutate_stop("%in% needs 1 to 10000 values.")
    return(list(var = .mutate_column(e[[2L]])$col, op = "in", value = values))
  }
  .mutate_stop("'", fn, "' is not a condition of the grammar (== != < <= > >= %in% is.na ! & |).")
}

.mutate_is_condition <- function(e) {
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  is.call(e) && as.character(e[[1L]])[[1L]] %in%
    c(.V2_CMP_OPS, "%in%", "is.na", "!", "&", "|", "&&", "||")
}

# Breaks or labels of cut(): a literal c(...) vector, or the name of a vector
# in the caller's environment (these positions are never columns; the server
# receives the values only).
.mutate_cut_vector <- function(e, env, what, type) {
  value <- if (is.call(e) && identical(e[[1L]], as.name("c"))) {
    lapply(as.list(e)[-1L], function(a) {
      lit <- .mutate_literal(a)
      if (is.null(lit) || is.null(lit$lit)) .mutate_stop("cut() ", what, " must be literals.")
      lit$lit
    })
  } else if (is.symbol(e) && exists(as.character(e), envir = env)) {
    as.list(get(as.character(e), envir = env))
  } else {
    lit <- .mutate_literal(e)
    if (is.null(lit) || identical(what, "breaks")) {
      .mutate_stop("cut() ", what, " must be a literal vector c(...)",
                   if (identical(what, "breaks")) " of at least two public breaks", ".")
    }
    list(lit$lit)
  }
  ok <- if (identical(type, "number")) {
    all(vapply(value, function(v) is.numeric(v) && length(v) == 1L && is.finite(v), logical(1)))
  } else {
    all(vapply(value, function(v) is.character(v) && length(v) == 1L && !is.na(v), logical(1)))
  }
  if (!ok) .mutate_stop("cut() ", what, " must be ", if (type == "number") "finite numbers" else
    "strings", ".")
  unname(value)
}

# R expression -> mutation AST node (design 4.6).
.mutate_expr <- function(e, env) {
  while (is.call(e) && identical(e[[1L]], as.name("("))) e <- e[[2L]]
  if (is.symbol(e)) return(.mutate_column(e))
  lit <- .mutate_literal(e)
  if (!is.null(lit)) return(lit)
  if (!is.call(e)) .mutate_stop("unsupported expression.")
  if (.mutate_is_condition(e)) return(list(pred = .mutate_pred(e)))
  fn <- as.character(e[[1L]])[[1L]]
  if (fn %in% c("+", "-", "*", "/")) {
    if (length(e) == 2L) {
      if (identical(fn, "+")) return(.mutate_expr(e[[2L]], env))
      if (identical(fn, "-")) {
        # Unary minus: multiply by the integer -1 (an integer stays integer).
        return(list(op = "*", args = list(.mutate_expr(e[[2L]], env),
                                          list(lit = -1L, as = "integer"))))
      }
    }
    return(list(op = fn, args = list(.mutate_expr(e[[2L]], env), .mutate_expr(e[[3L]], env))))
  }
  if (identical(fn, "ifelse")) {
    m <- match.call(function(test, yes, no) NULL, e)
    if (is.null(m$test) || is.null(m$yes) || is.null(m$no)) .mutate_stop("ifelse() needs test, yes and no.")
    if (!.mutate_is_condition(m$test)) .mutate_stop("the test of ifelse() must be a condition.")
    return(list(ifelse = list(test = .mutate_pred(m$test), yes = .mutate_expr(m$yes, env),
                              no = .mutate_expr(m$no, env))))
  }
  if (identical(fn, "case_when")) {
    args <- as.list(e)[-1L]
    nms <- names(args) %||% rep("", length(args))
    default <- NULL
    cases <- list()
    for (i in seq_along(args)) {
      a <- args[[i]]
      if (identical(nms[[i]], ".default")) {
        default <- .mutate_expr(a, env)
        next
      }
      if (nzchar(nms[[i]])) .mutate_stop("case_when() takes condition ~ value formulas and .default.")
      if (!is.call(a) || !identical(a[[1L]], as.name("~")) || length(a) != 3L) {
        .mutate_stop("case_when() takes condition ~ value formulas.")
      }
      if (isTRUE(a[[2L]]) && i == length(args)) {
        default <- .mutate_expr(a[[3L]], env)
        next
      }
      if (!.mutate_is_condition(a[[2L]])) .mutate_stop("each case_when() case needs a condition.")
      cases[[length(cases) + 1L]] <- list(when = .mutate_pred(a[[2L]]),
                                          then = .mutate_expr(a[[3L]], env))
    }
    if (!length(cases)) .mutate_stop("case_when() needs at least one condition ~ value case.")
    out <- list(cases = cases)
    if (!is.null(default)) out[["else"]] <- default
    return(list(case_when = out))
  }
  if (identical(fn, "coalesce")) {
    args <- as.list(e)[-1L]
    if (length(args) < 2L || length(args) > 16L) .mutate_stop("coalesce() takes 2 to 16 values.")
    if (!is.null(names(args)) && any(nzchar(names(args)))) .mutate_stop("coalesce() takes unnamed values.")
    return(list(coalesce = lapply(unname(args), .mutate_expr, env = env)))
  }
  if (identical(fn, "cut")) {
    m <- match.call(function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE,
                             dig.lab = 3L, ordered_result = FALSE) NULL, e)
    if (is.null(m$x) || is.null(m$breaks)) .mutate_stop("cut() needs x and breaks.")
    for (unsupported in c("include.lowest", "dig.lab", "ordered_result")) {
      if (!is.null(m[[unsupported]])) .mutate_stop("cut() does not take ", unsupported, ".")
    }
    right <- if (is.null(m$right)) TRUE else m$right
    if (!is.logical(right) || length(right) != 1L || is.na(right)) {
      .mutate_stop("cut() right must be TRUE or FALSE.")
    }
    out <- list(arg = .mutate_expr(m$x, env),
                breaks = .mutate_cut_vector(m$breaks, env, "breaks", "number"),
                # R's cut() default (intervals closed on the right) is kept.
                right = right)
    if (!is.null(m$labels)) out$labels <- .mutate_cut_vector(m$labels, env, "labels", "string")
    return(list(cut = out))
  }
  .mutate_stop("'", fn, "' is not part of the mutation grammar (+ - * /, conditions, ifelse, ",
               "case_when, coalesce, cut, as.Date).")
}

# The vendored schema validates exactly what the server will parse.
.mutate_schema_check <- function(x, schema, field, root = schema) {
  wire <- jsonlite::fromJSON(as.character(jsonlite::toJSON(.v2_plain(x), auto_unbox = TRUE,
                                                           null = "null", digits = NA)),
                             simplifyVector = FALSE)
  res <- .jsonSchemaValidate(schema, wire, root = root)
  if (!res$valid) {
    stop(field, " does not validate against the frozen schema: ",
         paste(utils::head(res$errors, 4L), collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

#' The mutation request of ds.omop.mutate() (mutate-v1.json)
#' @keywords internal
.mutate_request <- function(exprs, env) {
  nms <- names(exprs)
  if (!length(exprs)) .mutate_stop("give at least one name = expression.")
  if (length(exprs) > 64L) .mutate_stop("give at most 64 expressions.")
  if (is.null(nms) || any(!nzchar(nms))) .mutate_stop("name every expression (name = expression).")
  bad <- nms[!grepl(.MUTATE_NAME_PATTERN, nms)]
  if (length(bad)) {
    .mutate_stop("'", bad[[1L]], "' is not a column name (a letter, then letters, digits or ",
                 "underscores, at most 64 characters).")
  }
  req <- list(version = 1L, assign = lapply(seq_along(exprs), function(i) {
    list(name = nms[[i]], expr = .mutate_expr(exprs[[i]], env))
  }))
  .mutate_schema_check(req, .jsonSchemaPackaged("mutate-v1", package = "dsOMOPClient"),
                       "ds.omop.mutate(): the request")
  req
}

#' Derive columns of a server-side omop.table
#'
#' Adds or replaces columns of a server-side, token-keyed data frame with R
#' expressions over its columns, evaluated on every server by the closed
#' mutation grammar of \code{omopMutateDS} (design section 2.11): bare names
#' are columns (of the input or assigned earlier in the same call); literals
#' are numbers (\code{5L} is an integer), strings, \code{TRUE}/\code{FALSE},
#' \code{NA} and \code{as.Date("YYYY-MM-DD")}; operators are \code{+ - * /}
#' (division by zero or a non-finite result gives \code{NA}; \code{date -
#' date} gives days, \code{date +/- integer} a date), conditions (\code{== !=
#' < <= > >= \%in\% is.na() ! & |}, three-valued: \code{NA} when unknown, each
#' comparing one column with one literal), \code{ifelse(test, yes, no)},
#' \code{case_when(cond ~ value, ..., .default = )} (an unknown condition
#' gives \code{NA}), \code{coalesce(...)} and \code{cut(x, breaks, labels,
#' right)} (R's \code{right = TRUE} default; text labels, \code{NA} outside
#' the breaks; \code{breaks}/\code{labels} may name vectors of your session).
#' Nothing is evaluated as R code, on the client or on the server.
#'
#' Person, episode and set keys and protected columns can be neither read nor
#' written. The result is a released object: its distinct persons must reach
#' the server's \code{nfilter.subset} (the server refuses it otherwise, with
#' its constant message) and it carries no differential-privacy provenance.
#' With \code{newobj = x} the target is replaced in place on every server; a
#' rollback copy is kept until every server has succeeded, so a failure on
#' one server leaves the target unchanged everywhere.
#'
#' @param x Name of the server-side \code{omop.table} symbol (default: the
#'   session's most recent output).
#' @param ... Named expressions, \code{name = expression}.
#' @param newobj Name of the symbol to create (default: a fresh name);
#'   \code{newobj = x} replaces \code{x}.
#' @param symbol Session symbol (default \code{"omop"}).
#' @param conns DSI connections (default: the session's).
#' @return Invisibly, the name of the server-side symbol.
#' @examples
#' \dontrun{
#' ds.omop.mutate("D_cov", pdc = covered / observed,
#'                adherent = ifelse(pdc >= 0.8, 1L, 0L),
#'                age_band = cut(age, c(0, 18, 65, 120), right = FALSE))
#' }
#' @seealso \code{\link{ds.omop.filter}}, \code{\link{ds.omop.select}}
#' @export
ds.omop.mutate <- function(x = NULL, ..., newobj = NULL, symbol = "omop", conns = NULL) {
  exprs <- as.list(substitute(list(...)))[-1L]
  req <- .mutate_request(exprs, parent.frame())
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  .verb_assert_supported(session, names(conns), "verb.mutate", "ds.omop.mutate()")
  x <- .resolve_target_symbol(x, session, "x")
  expr <- call("omopMutateDS", as.name(x), .ds_encode_lossless(req))
  if (!is.null(newobj) && identical(newobj, x)) {
    .assign_in_place(conns, x, expr, context = "OMOP mutate")
    return(invisible(x))
  }
  newobj <- newobj %||% .generate_symbol("omop.mutate")
  .assign_expr_atomic(conns, newobj, expr = expr, context = "OMOP mutate",
                      session_symbol = symbol, required_symbols = x)
  invisible(newobj)
}

# Replace `target` on every server with the value of `expr` (an authorised
# in-place verb, design 7.3): a rollback copy of the target is made first;
# when any server fails, the servers that succeeded get the copy back; the
# copies are removed in both cases. A plain symbol copy releases nothing new.
.assign_in_place <- function(conns, target, expr, context) {
  inventory <- .plan_symbol_inventory(conns, paste0(context, " preflight"))
  backup <- .fresh_symbol_from_inventory(inventory, "omop.rollback", context)
  .assign_expr_atomic(conns, backup, expr = as.name(target),
                      context = paste0(context, " rollback copy"), required_symbols = target)
  succeeded <- character(0)
  failures <- list()
  condition <- tryCatch({
    DSI::datashield.assign.expr(conns, symbol = target, expr = expr,
                                success = function(server) succeeded <<- c(succeeded, server),
                                error = function(server, message) failures[[server]] <<- message)
    NULL
  }, error = identity)
  incomplete <- unique(c(names(failures), setdiff(names(conns), succeeded)))
  if (!is.null(condition) || length(incomplete)) {
    restored <- intersect(succeeded, names(conns))
    restore_error <- NULL
    if (length(restored)) {
      restore_error <- tryCatch({
        DSI::datashield.assign.expr(conns[restored], symbol = target, expr = as.name(backup))
        NULL
      }, error = function(e) conditionMessage(e))
    }
    .plan_remove_output_symbols(conns, list(backup), verify = is.null(restore_error))
    if (!is.null(restore_error)) {
      stop(context, " failed on ", paste(incomplete, collapse = ", "), " and the target could not ",
           "be restored on ", paste(restored, collapse = ", "), ": ", restore_error, call. = FALSE)
    }
    detail <- .ds_failure_detail(failures, condition, servers = incomplete)
    stop(context, " failed and '", target, "' was left unchanged: ", detail, call. = FALSE)
  }
  .plan_remove_output_symbols(conns, list(backup), verify = TRUE)
  invisible(target)
}

# The predicate of ds.omop.filter(expr = ): an omop_where() predicate (or its
# tree form), parameter placeholders bound to their defaults.
.verb_predicate_tree <- function(expr) {
  pred <- if (inherits(expr, "omop_predicate")) expr else
    if (is.list(expr)) omop_where_tree(expr) else
      stop("ds.omop.filter(): expr must be a predicate from omop_where().", call. = FALSE)
  pred <- .v2_resolve_params(pred, "ds.omop.filter(expr = )")
  if (.v2_is_param(pred)) {
    stop("ds.omop.filter(): expr must be a predicate from omop_where().", call. = FALSE)
  }
  .mutate_schema_check(pred$tree, .v2_schema_def("predicate"), "ds.omop.filter(): expr",
                       root = .v2_plan_schema())
  pred$tree
}
