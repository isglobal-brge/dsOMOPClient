# Module: Session Management
# Connect, disconnect, and query status of OMOP CDM DataSHIELD sessions.

#' Connect to an OMOP CDM resource on DataSHIELD servers
#'
#' Establishes a connection to one or more OMOP CDM databases via DataSHIELD.
#' Assigns the resource server-side, initializes the OMOP handle, retrieves
#' capabilities, and returns an \code{omop_session} object. This is the entry
#' point for all dsOMOPClient operations.
#'
#' @param resource Character or named list; resource name(s). A single string
#'   applies to all servers; a named list maps server names to resource names.
#' @param symbol Character; server-side symbol name (default: "omop").
#' @param cdm_schema Character; CDM schema override (NULL uses server default).
#' @param vocab_schema Character; vocabulary schema override (NULL uses server default).
#' @param results_schema Character; results schema override (NULL uses server default).
#' @param temp_schema Character; temp schema override (NULL uses server default).
#' @param strict Logical; if TRUE, fail immediately on any server error.
#' @param conns DSI connections object (NULL uses default connections).
#' @return An \code{omop_session} object (invisibly).
#' @examples
#' \dontrun{
#' library(DSI)
#' builder <- newDSLoginBuilder()
#' builder$append(server = "server1", url = "https://opal.example.org",
#'                resource = "project.omop_cdm", driver = "OpalDriver")
#' conns <- datashield.login(builder$build())
#' session <- ds.omop.connect(resource = "project.omop_cdm", conns = conns)
#' }
#' @seealso \code{\link{ds.omop.disconnect}}, \code{\link{ds.omop.status}}
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
#' Cleans up server-side temporary tables and removes the OMOP handle symbol.
#' Should be called when the session is no longer needed to free resources.
#'
#' @param symbol Character; the session symbol to disconnect (default: "omop").
#' @param conns DSI connections (NULL uses the session's stored connections).
#' @return Invisible TRUE on success.
#' @examples
#' \dontrun{
#' ds.omop.disconnect("omop")
#' }
#' @seealso \code{\link{ds.omop.connect}}
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
#' Pings each connected server and returns the current session status
#' including capabilities, server versions, and any connection errors.
#'
#' @param symbol Character; session symbol (default: "omop").
#' @return Named list with symbol, servers, capabilities, ping results,
#'   and errors.
#' @examples
#' \dontrun{
#' status <- ds.omop.status("omop")
#' status$ping
#' }
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
