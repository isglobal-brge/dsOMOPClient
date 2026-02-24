# ==============================================================================
# dsOMOPClient v2 - Connect, Disconnect, Status
# ==============================================================================

#' Connect to an OMOP CDM resource on DataSHIELD servers
#'
#' @param resource Character or named list; resource name(s)
#' @param symbol Character; server-side symbol name (default: "omop")
#' @param cdm_schema Character; CDM schema override
#' @param vocab_schema Character; vocabulary schema override
#' @param results_schema Character; results schema override
#' @param temp_schema Character; temp schema override
#' @param strict Logical; fail on any server error
#' @param conns DSI connections object
#' @return An omop_session object (invisibly)
#' @export
ds.omop.connect <- function(resource,
                            symbol = "omop",
                            cdm_schema = NULL,
                            vocab_schema = NULL,
                            results_schema = NULL,
                            temp_schema = NULL,
                            strict = TRUE,
                            conns = NULL) {
  conns <- conns %||% DSI::datashield.connections_default()
  if (is.null(conns)) {
    stop("No DataSHIELD connections available.", call. = FALSE)
  }

  server_names <- DSI::datashield.connections_find(conns)

  if (is.character(resource) && length(resource) == 1) {
    resource_map <- stats::setNames(
      rep(resource, length(server_names)),
      server_names
    )
  } else if (is.list(resource)) {
    resource_map <- resource
  } else {
    resource_map <- stats::setNames(
      as.character(resource), server_names)
  }

  res_symbol <- .generate_symbol("dsO")

  errors <- list()
  for (srv in server_names) {
    res_name <- resource_map[[srv]]
    if (is.null(res_name)) {
      if (strict) {
        stop("No resource specified for server '",
             srv, "'.", call. = FALSE)
      }
      next
    }
    tryCatch({
      DSI::datashield.assign.resource(
        conns, symbol = res_symbol,
        resource = res_name
      )
    }, error = function(e) {
      errors[[srv]] <<- e$message
    })
  }

  if (strict && length(errors) > 0) {
    stop("Failed to assign resource on server(s): ",
         paste(names(errors), collapse = ", "), "\n",
         paste(errors, collapse = "\n"),
         call. = FALSE)
  }

  init_call <- paste0(
    "omopInitDS('", res_symbol, "'",
    if (!is.null(cdm_schema))
      paste0(", cdm_schema='", cdm_schema, "'") else "",
    if (!is.null(vocab_schema))
      paste0(", vocab_schema='", vocab_schema, "'") else "",
    if (!is.null(results_schema))
      paste0(", results_schema='", results_schema, "'") else "",
    if (!is.null(temp_schema))
      paste0(", temp_schema='", temp_schema, "'") else "",
    ")"
  )

  tryCatch({
    DSI::datashield.assign.expr(
      conns, symbol = symbol,
      expr = as.symbol(init_call)
    )
  }, error = function(e) {
    if (strict) {
      stop("Failed to initialize OMOP handle: ",
           e$message, call. = FALSE)
    }
    warning("OMOP init failed on some servers: ", e$message)
  })

  caps <- tryCatch(
    DSI::datashield.aggregate(
      conns,
      expr = call("omopGetCapabilitiesDS", res_symbol)
    ),
    error = function(e) NULL
  )

  tryCatch(
    DSI::datashield.rm(conns, res_symbol),
    error = function(e) NULL
  )

  session <- list(
    symbol = symbol,
    res_symbol = res_symbol,
    resource_map = resource_map,
    conns = conns,
    capabilities = caps,
    server_names = server_names,
    errors = errors
  )
  class(session) <- "omop_session"

  assign(symbol, session, envir = .dsomop_client_env)

  invisible(session)
}

#' Disconnect an OMOP session
#'
#' @param symbol Character; the session symbol to disconnect
#' @param conns DSI connections
#' @export
ds.omop.disconnect <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  tryCatch({
    DSI::datashield.aggregate(
      conns,
      expr = call("omopCleanupDS", session$res_symbol)
    )
  }, error = function(e) NULL)

  tryCatch(
    DSI::datashield.rm(conns, symbol),
    error = function(e) NULL
  )

  if (exists(symbol, envir = .dsomop_client_env)) {
    rm(list = symbol, envir = .dsomop_client_env)
  }

  invisible(TRUE)
}

#' Get OMOP session status
#'
#' @param symbol Character; session symbol
#' @return Named list with per-server status
#' @export
ds.omop.status <- function(symbol = "omop") {
  session <- .get_session(symbol)

  ping <- tryCatch(
    DSI::datashield.aggregate(
      session$conns,
      expr = call("omopPingDS")
    ),
    error = function(e) list(error = e$message)
  )

  list(
    symbol = symbol,
    servers = session$server_names,
    capabilities = session$capabilities,
    ping = ping,
    errors = session$errors
  )
}
