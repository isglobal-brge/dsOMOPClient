# ==============================================================================
# dsOMOPClient v2 - Internal Utilities
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

.dsomop_client_env <- new.env(parent = emptyenv())

.get_session <- function(symbol = "omop") {
  if (!exists(symbol, envir = .dsomop_client_env)) {
    stop("No OMOP session '", symbol,
         "'. Call ds.omop.connect() first.",
         call. = FALSE)
  }
  get(symbol, envir = .dsomop_client_env)
}

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
