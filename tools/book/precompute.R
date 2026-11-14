# Run from package root. Execute each chapter in a fresh synthetic session.
stopifnot(as.character(packageVersion("dsOMOP")) == "2.10.5",
          as.character(packageVersion("dsOMOPClient")) == "2.10.5")
library(dsOMOPClient)
source("tools/book/verify-book2.R")
source("tools/book/verify.R")
source("tools/book/verify-book3.R")
source("tools/book/verify-longitudinal.R")
source("tools/book/verify-standalone-showcase.R")
options(width=88)
knitr::opts_knit$set(root.dir = normalizePath("."))
inputs <- sort(list.files("vignettes", "\\.Rmd\\.orig$", full.names=TRUE))
args <- if (sys.nframe() == 0L) commandArgs(trailingOnly=TRUE) else character()
if (length(args)) inputs <- inputs[grepl(args[[1L]], basename(inputs))]
stopifnot(length(inputs) > 0L)
if (!"--chapter-worker" %in% args) {
  for (input in inputs) {
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c("tools/book/precompute.R", shQuote(basename(input)), "--chapter-worker"))
    if (status != 0L) stop("Fresh-process chapter failed: ", input)
  }
  message("All chapters passed in separate R processes using visible setup")
} else {
  for (input in inputs) {
    message("PRECOMPUTE ", input)
    render <- function() {
      on.exit({
        if (exists("conns", inherits=FALSE)) {
          try(ds.omop.disconnect(conns=conns), silent=TRUE)
          try(DSI::datashield.logout(conns), silent=TRUE)
        }
        if (exists("practice", inherits=FALSE)) unlink(practice$path)
        if (exists("two_conns", inherits=FALSE)) {
          try(ds.omop.disconnect(conns=two_conns, symbol=if (grepl("book-15-labs|book-16-longitudinal",input)) "book_pool" else "omop"), silent=TRUE)
          try(DSI::datashield.logout(two_conns), silent=TRUE)
        }
        if (exists("pair", inherits=FALSE)) unlink(pair$path)
      })
      knitr::opts_chunk$set(error=FALSE, warning=TRUE, message=FALSE, comment="#>", collapse=FALSE)
      chapter_env <- environment()
      knitr::opts_hooks$set(label = function(options) {
        if (grepl("book-11-parameters", input) && options$label == "unknown-function")
          chapter_env$rollback_snapshot <- DSLite::getDSLiteData(chapter_env$conns, "D_bmi")$site
        if (grepl("book-00-showcase", input) && options$label == "showcase-numeric-connection") {
          symbols <- DSI::datashield.symbols(chapter_env$conns)$site
          symbols <- symbols[grepl("^D_tour_", symbols)]
          chapter_env$showcase_snapshot <- setNames(lapply(symbols, function(symbol)
            DSLite::getDSLiteData(chapter_env$conns, symbol)$site), symbols)
        }
        options
      })
      tryCatch(knitr::knit(input, output=sub("\\.orig$", "", input), envir=environment(), quiet=TRUE), error=function(e) { print(DSI::datashield.errors()); stop(e) })
      verify_chapter(input, environment())
    }
    render()
  }
  message("All chapter chunks executed successfully")
  writeLines(capture.output(sessionInfo()),"tools/book/evidence/session-info.txt")
}
