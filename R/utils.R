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
