# Run from package root. Execute each chapter in a fresh synthetic session.
stopifnot(as.character(packageVersion("dsOMOP")) == "2.10.4",
          as.character(packageVersion("dsOMOPClient")) == "2.10.4")
library(dsOMOPClient)
source("tools/book/fixture.R")
source("tools/book/practice.R")
source("tools/book/verify-book2.R")
source("tools/book/verify.R")
options(width=88)
knitr::opts_knit$set(root.dir = normalizePath("."))
inputs <- sort(list.files("vignettes", "\\.Rmd\\.orig$", full.names=TRUE))
args <- if (sys.nframe() == 0L) commandArgs(trailingOnly=TRUE) else character()
if (length(args)) inputs <- inputs[grepl(args[[1L]], basename(inputs))]
stopifnot(length(inputs) > 0L)
for (input in inputs) {
  message("PRECOMPUTE ", input)
  render <- function() {
    if (!grepl("book-00", input)) {
      f <- if (grepl("book-0[1234]-", input)) book_practice("gibleed",env=environment()) else
           if (grepl("book-06-events|book-08-outputs|book-11-parameters",input))
             book_practice("numeric",env=environment()) else book_fixture(env=environment())
      conns <- f$conns
    }
    on.exit({
      if (exists("conns", inherits=FALSE)) {
        try(ds.omop.disconnect(conns=conns), silent=TRUE)
        try(DSI::datashield.logout(conns), silent=TRUE)
      }
      if (exists("f", inherits=FALSE)) unlink(f$path)
      if (exists("practice", inherits=FALSE)) unlink(practice$path)
      if (exists("two_conns", inherits=FALSE)) {
        try(ds.omop.disconnect(conns=two_conns), silent=TRUE)
        try(DSI::datashield.logout(two_conns), silent=TRUE)
      }
      if (exists("pair", inherits=FALSE)) unlink(pair$path)
    })
    knitr::opts_chunk$set(error=FALSE, warning=TRUE, message=FALSE, comment="#>", collapse=FALSE)
    tryCatch(knitr::knit(input, output=sub("\\.orig$", "", input), envir=environment(), quiet=TRUE), error=function(e) { print(DSI::datashield.errors()); stop(e) })
    verify_chapter(input, environment())
  }
  render()
}
message("All chapter chunks executed successfully")
writeLines(capture.output(sessionInfo()),"tools/book/evidence/session-info.txt")
