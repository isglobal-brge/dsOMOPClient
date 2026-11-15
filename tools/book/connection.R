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

book_connect <- function(name = "gibleed", env = parent.frame(), symbol = "omop", ...) {
  force(env)
  stopifnot(length(name) == 1L, book_driver %in% c("DSLite", "Opal"))
  if (book_driver == "DSLite") {
    if (name == "questionnaire") {
      source("tools/book/fixture.R", local = TRUE)
      practice <- book_fixture(env = env, symbol = symbol, ...)
    } else {
      source("tools/book/practice.R", local = TRUE)
      practice <- book_practice(name, env = env, symbol = symbol, ...)
    }
  } else {
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
  .book_connection_state$practice <- practice
  practice
}

book_switch <- function(name, env = parent.frame(), ...) {
  previous <- .book_connection_state$practice
  if (!is.null(previous)) {
    # Logout still runs if detaching the resource fails. Never remove permanent
    # practice databases: only helpers' temporary paths are recorded in $path.
    tryCatch(dsOMOPClient::ds.omop.disconnect(conns = previous$conns,
      symbol = previous$symbol), finally = {
        DSI::datashield.logout(previous$conns)
        unlink(previous$path)
        .book_connection_state$practice <- NULL
      })
  }
  book_connect(name, env = env, ...)
}
