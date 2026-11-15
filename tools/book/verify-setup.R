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
# Check administrator scripts parse and use installed opalr exported API signatures.
for (path in c("tools/book/opal-admin.R", "tools/book/check-opal.R")) {
  ast <- parse(path)
  check <- function(x) {
    if (is.call(x)) {
      name <- as.character(x[[1]])[1]
      if (grepl("^(opal[.]|oadmin[.]|dsadmin[.])", name)) {
        fun <- getExportedValue("opalr", name)
        if (name == "dsadmin.install_github_package" && identical(as.list(x)[[3]], "dsOMOP"))
          stopifnot(identical(x$ref, as.character(packageVersion("dsOMOP"))))
        supplied <- names(as.list(x)[-1])
        stopifnot(all(supplied[nzchar(supplied)] %in% names(formals(fun))))
      }
    }
    if (is.call(x) || is.expression(x) || is.pairlist(x))
      for (child in as.list(x)) check(child)
  }
  check(ast)
}
message("PASS: Opal scripts parsed; named arguments match installed opalr API (no live deployment)")
