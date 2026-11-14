# Edit only this configuration when moving the public practice lessons to Opal.
book_driver <- "DSLite"  # or "Opal"
opal_url <- Sys.getenv("BOOK_OPAL_URL")
opal_project <- "practice"
opal_resources <- c(gibleed = "gibleed", numeric = "numeric")
opal_profile <- "default"

book_resource_names <- function(driver, project, resources) {
  stopifnot(driver %in% c("DSLite", "Opal"),
            identical(sort(names(resources)), c("gibleed", "numeric")),
            all(nzchar(resources)), length(project) == 1L, nzchar(project))
  if (driver == "Opal") setNames(paste(project, resources, sep = "."), names(resources))
  else c(gibleed = "gibleed", numeric = "numeric")
}
resource_names <- book_resource_names(book_driver, opal_project, opal_resources)
book_connect <- function(resource = c("gibleed", "numeric"), env = parent.frame()) {
  force(env)
  resource <- match.arg(resource)
  if (book_driver == "DSLite") {
    source("tools/book/practice.R", local = TRUE)
    return(book_practice(resource, env = env))
  }
  library(DSOpal)
  stopifnot(nzchar(opal_url), nzchar(Sys.getenv("BOOK_OPAL_USER")),
            nzchar(Sys.getenv("BOOK_OPAL_PASSWORD")))
  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "site", url = opal_url,
    user = Sys.getenv("BOOK_OPAL_USER"), password = Sys.getenv("BOOK_OPAL_PASSWORD"),
    driver = "OpalDriver", profile = opal_profile)
  conns <- DSI::datashield.login(builder$build(), assign = FALSE)
  dsOMOPClient::ds.omop.connect(resource = resource_names[[resource]], conns = conns)
  list(conns = conns, path = character())
}
