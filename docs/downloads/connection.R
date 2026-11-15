# Set these values once before connecting. Re-sourcing keeps existing settings
# and the active session, including an Opal session.
if (!exists("book_driver", inherits = TRUE)) book_driver <- "DSLite"
if (!exists("opal_url", inherits = TRUE)) opal_url <- Sys.getenv("BOOK_OPAL_URL")
if (!exists("opal_project", inherits = TRUE)) opal_project <- "practice"
if (!exists("opal_resources", inherits = TRUE))
  opal_resources <- c(gibleed = "gibleed", numeric = "numeric", questionnaire = "questionnaire")
if (!exists("opal_profile", inherits = TRUE)) opal_profile <- "default"
if (!exists(".book_connection_state", inherits = FALSE))
  .book_connection_state <- new.env(parent = emptyenv())

book_resource_names <- function(driver, project, resources) {
  stopifnot(driver %in% c("DSLite", "Opal"), !is.null(names(resources)),
    !anyDuplicated(names(resources)), all(nzchar(resources)),
    length(project) == 1L, nzchar(project))
  if (driver == "Opal") setNames(paste(project, resources, sep = "."), names(resources))
  else resources
}
# Retained for existing setup blocks. book_connect resolves current settings
# at call time, so changing the configuration never requires re-sourcing.
if (!exists("resource_names", inherits = TRUE))
  resource_names <- book_resource_names(book_driver, opal_project, opal_resources)

book_connect <- function(name = "gibleed", env = parent.frame(), symbol = "omop",
                         two_sites = FALSE, session = "default",
                         conns = NULL, resources = NULL, ...) {
  force(env)
  stopifnot(length(name) == 1L, book_driver %in% c("DSLite", "Opal"))
  stopifnot(length(session) == 1L, nzchar(session), session != "practice",
    is.logical(two_sites), length(two_sites) == 1L, !is.na(two_sites))
  if (!is.null(.book_connection_state[[session]])) book_disconnect(session)
  if (!is.null(conns)) {
    # Use real analyst connections and their configured resource names as given.
    if (is.null(resources) || is.null(names(resources)) ||
        anyDuplicated(names(resources)) || anyNA(resources) || length(resources) != length(conns) ||
        !identical(names(resources), names(conns)) || any(!nzchar(resources)) ||
        (two_sites && length(conns) != 2L))
      stop("Supply one named resource for each connection (two for two_sites = TRUE).")
    complete <- FALSE
    on.exit(if (!complete) DSI::datashield.logout(conns), add = TRUE)
    dsOMOPClient::ds.omop.connect(resource = as.list(resources),
      conns = conns, symbol = symbol)
    practice <- list(conns = conns, path = character())
    complete <- TRUE
  } else if (book_driver == "DSLite") {
    if (name == "questionnaire") {
      source("tools/book/fixture.R", local = TRUE)
      practice <- book_fixture(env = env, symbol = symbol, two_sites = two_sites, ...)
    } else {
      source("tools/book/practice.R", local = TRUE)
      practice <- if (two_sites) book_numeric_pair(resource = name, env = env, symbol = symbol, ...)
      else book_practice(name, env = env, symbol = symbol, ...)
    }
  } else {
    if (two_sites) stop("For two Opal sites, supply conns and matching named resources.")
    library(DSOpal)
    resources <- book_resource_names(book_driver, opal_project, opal_resources)
    if (!name %in% names(resources)) stop("No Opal resource configured for: ", name)
    stopifnot(nzchar(opal_url))
    builder <- DSI::newDSLoginBuilder()
    token <- Sys.getenv("BOOK_OPAL_TOKEN")
    if (nzchar(token)) {
      builder$append(server = "site", url = opal_url, token = token,
        driver = "OpalDriver", profile = opal_profile)
    } else {
      stopifnot(nzchar(Sys.getenv("BOOK_OPAL_USER")), nzchar(Sys.getenv("BOOK_OPAL_PASSWORD")))
      builder$append(server = "site", url = opal_url,
        user = Sys.getenv("BOOK_OPAL_USER"), password = Sys.getenv("BOOK_OPAL_PASSWORD"),
        driver = "OpalDriver", profile = opal_profile)
    }
    conns <- DSI::datashield.login(builder$build(), assign = FALSE)
    connected <- FALSE
    on.exit(if (!connected) DSI::datashield.logout(conns), add = TRUE)
    dsOMOPClient::ds.omop.connect(resource = resources[[name]], conns = conns, symbol = symbol)
    connected <- TRUE
    practice <- list(conns = conns, path = character())
  }
  practice$resource <- name
  practice$symbol <- symbol
  practice$session <- session
  .book_connection_state[[session]] <- practice
  if (session == "default") .book_connection_state$practice <- practice
  practice
}

# Named sessions allow a lesson to keep earlier assigned tables while opening
# a second pair. Switching or closing one session leaves the others available.
book_disconnect <- function(session = "default") {
  previous <- .book_connection_state[[session]]
  if (!is.null(previous)) {
    tryCatch(dsOMOPClient::ds.omop.disconnect(conns = previous$conns,
      symbol = previous$symbol), finally = {
        tryCatch(DSI::datashield.logout(previous$conns), finally = {
          unlink(previous$path)
          .book_connection_state[[session]] <- NULL
          if (session == "default") .book_connection_state$practice <- NULL
        })
      })
  }
  invisible(NULL)
}

book_switch <- function(name, env = parent.frame(), session = "default", ...) {
  book_disconnect(session)
  book_connect(name, env = env, session = session, ...)
}
