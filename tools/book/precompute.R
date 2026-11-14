# Run from package root. Execute each chapter in a fresh synthetic session.
stopifnot(as.character(packageVersion("dsOMOP")) == "2.10.4",
          as.character(packageVersion("dsOMOPClient")) == "2.10.4")
library(dsOMOPClient)
source("tools/book/fixture.R")
options(width=88)
for (input in sort(list.files("vignettes/articles", "\\.Rmd\\.orig$", full.names=TRUE))) {
  message("PRECOMPUTE ", input)
  local({
    f <- book_fixture(env=environment())
    conns <- f$conns
    on.exit({ds.omop.disconnect(conns=conns); DSI::datashield.logout(conns); unlink(f$path)})
    knitr::opts_chunk$set(error=FALSE, warning=TRUE, message=FALSE, comment="#>", collapse=TRUE)
    knitr::knit(input, output=sub("\\.orig$", "", input), envir=environment(), quiet=TRUE)
  })
}
message("All chapter chunks executed successfully")
source("tools/book/verify.R")
writeLines(capture.output(sessionInfo()),"tools/book/evidence/session-info.txt")
