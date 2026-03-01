# Module: Client Utilities
# Internal utility functions for session management and symbol generation.

`%||%` <- function(x, y) if (is.null(x)) y else x

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
