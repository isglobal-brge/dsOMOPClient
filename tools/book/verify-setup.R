# Executed on the pod, using only the two public synthetic databases.
library(dsOMOPClient)
source("tools/book/connection.R")
source("tools/book/practice.R")
source("tools/book/display.R")
local({
  # Register the actual resources under changed Opal-style names in DSLite.
  # This tests the exact chapter attach/switch code without claiming a live Opal.
  resource_names <- book_resource_names("Opal", "renamed_project",
    c(gibleed="intro_cdm", numeric="lab_cdm"))
  stopifnot(identical(resource_names,
    c(gibleed="renamed_project.intro_cdm", numeric="renamed_project.lab_cdm")))
  practice <- book_practice("gibleed", resource_names=resource_names)
  conns <- practice$conns
  on.exit({try(ds.omop.disconnect(conns=conns),silent=TRUE);DSI::datashield.logout(conns)})
  lines <- readLines("vignettes/book-02-connect.Rmd.orig")
  for (label in c("attach", "handle", "switch-to-numeric", "switch-back")) {
    start <- match(paste0("```{r ",label,"}"), lines)
    end <- start + which(lines[(start+1):length(lines)] == "```")[1]
    eval(parse(text=lines[(start+1):(end-1)]))
    expected <- if (label == "switch-to-numeric") 25 else 2690
    stopifnot(ds.omop.table.stats("person", conns=conns)$per_site$site$persons==expected)
  }
  message("PASS: exact chapter 2 attach/switch chunks with renamed project and both resources")
})

# Validate the unevaluated analyst example without installing packages or logging in.
local({
  lines <- readLines("vignettes/book-00-start.Rmd.orig")
  chunk <- function(label) {
    start <- grep(paste0("^```\\{r ", label, ", eval = FALSE\\}$"), lines)
    stopifnot(length(start) == 1L)
    end <- start + which(lines[(start + 1L):length(lines)] == "```")[1L]
    parse(text=lines[(start + 1L):(end - 1L)])
  }
  installation <- chunk("install-prerequisites")
  stopifnot(sum(grepl("^install.packages", lines)) == 1L,
    !any(grepl("auth_token|easy-opal|dsadmin[.]|opal-admin|check-opal|book-compose|book-rock", lines)))
  needed <- c("DSI", "DSOpal", "DSLite", "DBI", "RSQLite", "resourcer", "withr",
              "openssl", "jsonlite", "knitr", "rmarkdown", "duckdb", "lme4", "remotes")
  stopifnot(all(needed %in% eval(installation[[1L]][[2L]])))
  stopifnot(identical(vapply(as.list(installation)[-1L], function(x) x[[2L]], ""),
    c("datashield/dsBase@6.3.5", "datashield/dsBaseClient@6.3.5",
      "isglobal-brge/dsOMOP@2.10.5", "isglobal-brge/dsOMOPClient@2.10.5")))
  withr::local_envvar(c(BOOK_OPAL_USER="", BOOK_OPAL_PASSWORD="", BOOK_OPAL_TOKEN=""))
  example <- new.env(parent=globalenv())
  for (expr in chunk("opal-login")) {
    # Stop before the first network call; placeholders must never be sent.
    if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name("conns"))) break
    eval(expr, example)
  }
  login <- example$builder$build()
  stopifnot(login$server == "site", login$url == "<server-url>",
            login$user == "<user>", login$password == "<password>",
            login$driver == "OpalDriver", login$profile == "<profile>",
            identical(unname(example$resource_names),
              c("<project>.<gibleed-resource>", "<project>.<numeric-resource>")))
  # Check the same builder settings when later lessons reconnect via the helper,
  # for both passwords and tokens. The helper's login/attach calls remain unrun.
  for (token in c("", "example-token")) {
    Sys.setenv(BOOK_OPAL_TOKEN=token)
    helper <- new.env(parent=example)
    helper$resource <- "numeric"
    helper$env <- helper
    for (expr in as.list(body(example$book_connect))[-1L]) {
      if (is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
          identical(expr[[2L]], as.name("conns"))) break
      eval(expr, helper)
    }
    config <- helper$builder$build()
    stopifnot(config$url == login$url, config$profile == login$profile,
              config$driver == "OpalDriver")
    if (nzchar(token)) stopifnot(config$token == token)
    else stopifnot(config$user == login$user, config$password == login$password)
  }
  message("PASS: install chunk parsed only; analyst password/token login builders checked without network calls")
})
