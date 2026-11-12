# Module: Client Utilities
# Internal utility functions for session management and symbol generation.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' First non-NULL element of a list
#'
#' Returns the first element of \code{x} that is not \code{NULL}, or \code{NULL}
#' if every element is \code{NULL}. Used to pick a single value (e.g. a visit
#' filter or concept-scope column) from a set of variables, "first one set wins".
#'
#' @param x A list.
#' @return The first non-NULL element, or \code{NULL}.
#' @keywords internal
.first_non_null <- function(x) {
  for (el in x) if (!is.null(el)) return(el)
  NULL
}

#' Internal environment for storing dsOMOPClient session state
#' @keywords internal
.dsomop_client_env <- new.env(parent = emptyenv())

#' Retrieve a stored OMOP session
#'
#' Looks up a previously created \code{omop_session} object by its symbol
#' name in the internal client environment. Stops with an informative error
#' if no session with that symbol exists.
#'
#' @param symbol Character; the session symbol to look up.
#' @return The \code{omop_session} object.
#' @keywords internal
.get_session <- function(symbol = "omop") {
  if (!exists(symbol, envir = .dsomop_client_env)) {
    stop("No OMOP session '", symbol,
         "'. Call ds.omop.connect() first.",
         call. = FALSE)
  }
  get(symbol, envir = .dsomop_client_env)
}

#' Generate a unique temporary symbol name
#'
#' Creates a random symbol by appending six alphanumeric characters to the
#' given prefix, separated by a dot. Used to create unique server-side
#' variable names that avoid collisions across sessions.
#'
#' @param prefix Character; prefix for the generated symbol.
#' @return Character; a unique symbol string (e.g., \code{"dsO.aB3xZq"}).
#' @keywords internal
.generate_symbol <- function(prefix = "dsO") {
  paste0(prefix, ".",
         paste(sample(c(letters, LETTERS, 0:9), 6,
                      replace = TRUE),
               collapse = ""))
}

# Return a random workspace symbol that is absent from every supplied server
# inventory. The caller is responsible for obtaining the inventory immediately
# before use; this keeps reservation checks consistent across lifecycle helpers.
.fresh_symbol_from_inventory <- function(inventory, prefix, context,
                                         attempts = 20L) {
  for (attempt in seq_len(attempts)) {
    candidate <- .generate_symbol(prefix)
    occupied <- vapply(inventory, function(symbols) candidate %in% symbols,
                       logical(1))
    if (!any(occupied)) return(candidate)
  }
  stop("Could not reserve a collision-free server symbol during ", context,
       ".", call. = FALSE)
}

#' Parse YAML without evaluating embedded R expressions
#'
#' The yaml package can be configured globally to evaluate `!expr` tags.  The
#' client never accepts executable YAML, irrespective of that process-wide
#' option.  A sentinel handler lets us reject such tags explicitly while still
#' relying on yaml's parser (instead of attempting to recognise tags with a
#' regular expression).
#'
#' @param text A length-one YAML document.
#' @return The parsed R object.
#' @keywords internal
.yaml_load_safe <- function(text, sequences_as_lists = FALSE) {
  blocked <- function(value) {
    structure(list(value = value), class = "dsomop_blocked_yaml_expr")
  }
  handlers <- list(expr = blocked)
  # JSON-shaped documents (v2 plans) keep one-element arrays as arrays.
  if (isTRUE(sequences_as_lists)) handlers$seq <- function(x) as.list(x)
  parsed <- yaml::yaml.load(
    text,
    handlers = handlers,
    eval.expr = FALSE
  )
  contains_blocked <- function(x) {
    if (inherits(x, "dsomop_blocked_yaml_expr")) return(TRUE)
    is.list(x) && any(vapply(x, contains_blocked, logical(1)))
  }
  if (contains_blocked(parsed)) {
    stop("Executable YAML tags (!expr) are not permitted.", call. = FALSE)
  }
  parsed
}

#' Assign one server-side symbol atomically across a federation
#'
#' @param conns Named DataSHIELD connections.
#' @param newobj Destination symbol, which must not already exist.
#' @param expr Unevaluated assign expression.
#' @param context Human-readable operation label.
#' @param session_symbol Optional local OMOP session name to update after commit.
#' @param required_symbols Source symbols that must exist on every server.
#' @return `newobj`, invisibly.
#' @keywords internal
.assign_expr_atomic <- function(conns, newobj, expr, context,
                                session_symbol = NULL,
                                required_symbols = character(0)) {
  if (!is.character(newobj) || length(newobj) != 1L || is.na(newobj) ||
      !grepl("^[A-Za-z.][A-Za-z0-9._]*$", newobj) ||
      grepl("^\\.[0-9]", newobj)) {
    stop(context, " output must be one safe R symbol.", call. = FALSE)
  }
  required_symbols <- unique(as.character(required_symbols))
  if (length(required_symbols) > 0L &&
      (anyNA(required_symbols) || any(!nzchar(required_symbols)) ||
       any(!grepl("^[A-Za-z.][A-Za-z0-9._]*$", required_symbols)) ||
       any(grepl("^\\.[0-9]", required_symbols)))) {
    stop(context, " source symbols must be safe R symbols.", call. = FALSE)
  }

  inventory <- .plan_symbol_inventory(conns, paste0(context, " preflight"))
  occupied <- names(inventory)[vapply(
    inventory, function(x) newobj %in% x, logical(1)
  )]
  if (length(occupied) > 0L) {
    stop(context, " output symbol '", newobj, "' already exists on: ",
         paste(occupied, collapse = ", "), ". Choose a fresh newobj.",
         call. = FALSE)
  }
  missing_sources <- unlist(lapply(names(inventory), function(server) {
    missing <- setdiff(required_symbols, inventory[[server]])
    if (length(missing) == 0L) character(0) else
      paste0(server, ":", missing)
  }), use.names = FALSE)
  if (length(missing_sources) > 0L) {
    stop(context, " source symbol(s) are unavailable: ",
         paste(missing_sources, collapse = ", "), ".", call. = FALSE)
  }

  succeeded <- character(0)
  failures <- list()
  condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns, symbol = newobj, expr = expr,
      success = function(server) succeeded <<- c(succeeded, server),
      error = function(server, message) failures[[server]] <<- message
    )
    incomplete <- unique(c(names(failures), setdiff(names(conns), succeeded)))
    if (length(incomplete)) {
      stop(.ds_failure_detail(failures, servers = incomplete), call. = FALSE)
    }
    # An assignment acknowledgement is not the end of the transaction. A
    # missing output or a failed inventory request must clean the fresh name
    # everywhere it might have been assigned, just like assignment failure.
    committed <- .plan_symbol_inventory(conns, paste0(context, " commit"))
    missing_output <- names(committed)[!vapply(
      committed, function(x) newobj %in% x, logical(1)
    )]
    if (length(missing_output)) {
      stop("Could not prove ", context, " commit on: ",
           paste(missing_output, collapse = ", "), ".", call. = FALSE)
    }
    NULL
  }, error = identity, interrupt = identity)
  if (!is.null(condition)) {
    detail <- .assignment_error_detail(failures, condition, setdiff(names(conns), succeeded))
    cleanup <- .assignment_cleanup(conns, newobj)
    if (length(cleanup)) {
      stop(context, " failed: ", detail, "; cleanup could not be proven for '",
           newobj, "' on ", .assignment_site_errors(cleanup),
           ". The output may remain on these sites.", call. = FALSE)
    }
    stop(context, " failed and was rolled back: ", detail,
         if (!grepl("[.]$", detail)) ".", call. = FALSE)
  }

  if (!is.null(session_symbol)) {
    .record_session_owned_symbol(session_symbol, newobj)
  }
  invisible(newobj)
}

# Attempt every site's cleanup independently; a transport failure at one site
# must not prevent verified disposal elsewhere. Only call for symbols whose
# ownership was established before writing them.
.assignment_cleanup <- function(conns, symbol) {
  failures <- list()
  for (server in names(conns)) {
    condition <- tryCatch({
      .plan_remove_output_symbols(conns[server], list(symbol), verify = TRUE)
      NULL
    }, error = identity, interrupt = identity)
    if (!is.null(condition)) failures[[server]] <- conditionMessage(condition)
  }
  failures
}

# DSI can throw a generic aggregate exception after delivering site callbacks.
# Keep those site reasons as well as the original transport/verification error.
.assignment_error_detail <- function(errors, condition, incomplete = character(0)) {
  original <- conditionMessage(condition)
  detail <- tryCatch(.ds_failure_detail(errors, condition, servers = incomplete),
                     error = function(e) original, interrupt = function(e) original)
  if (nzchar(original) && !grepl(original, detail, fixed = TRUE)) {
    paste0(detail, "; ", original)
  } else detail
}

.assignment_site_errors <- function(errors) {
  paste(vapply(names(errors), function(server) {
    paste0(server, ": ", errors[[server]])
  }, character(1)), collapse = "; ")
}

#' Record an owned server symbol without changing the default manipulation input
#'
#' @param session_symbol Local OMOP session name.
#' @param symbol Newly committed server-side symbol.
#' @return `NULL`, invisibly.
#' @keywords internal
.record_session_owned_symbol <- function(session_symbol, symbol) {
  if (!exists(session_symbol, envir = .dsomop_client_env, inherits = FALSE)) {
    return(invisible(NULL))
  }
  session <- get(session_symbol, envir = .dsomop_client_env, inherits = FALSE)
  session$outputs <- unique(c(session$outputs, symbol))
  assign(session_symbol, session, envir = .dsomop_client_env)
  invisible(NULL)
}

# --- Code generation helpers --------------------------------------------------

#' Format an R value for code generation
#' @param x An R value to format as code
#' @return Character string of valid R code
#' @keywords internal
.format_r_value <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.character(x) && length(x) == 1) return(paste0('"', x, '"'))
  if (is.numeric(x) && length(x) == 1) return(as.character(x))
  if (is.logical(x) && length(x) == 1) return(as.character(x))
  if (is.integer(x) && length(x) == 1) return(paste0(x, "L"))
  if (is.numeric(x)) return(paste0("c(", paste(x, collapse = ", "), ")"))
  if (is.character(x)) return(paste0('c("', paste(x, collapse = '", "'), '")'))
  deparse(x, width.cutoff = 500L)
}

#' Build an R code string for a function call
#' @param fn_name Character; fully qualified function name
#' @param ... Named arguments to include in the call
#' @return Character string of the R call
#' @keywords internal
.build_code <- function(fn_name, ...) {
  args <- list(...)
  parts <- vapply(names(args), function(nm) {
    val <- args[[nm]]
    if (is.null(val)) return(NA_character_)
    paste0(nm, " = ", .format_r_value(val))
  }, character(1))
  parts <- parts[!is.na(parts)]
  paste0(fn_name, "(", paste(parts, collapse = ", "), ")")
}
