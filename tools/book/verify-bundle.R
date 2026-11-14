# Smoke-test the downloadable helpers in the documented no-checkout layout.
library(dsOMOPClient)
root <- normalizePath(".")
local({
  scratch <- tempfile("book-reader-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive=TRUE), add=TRUE)
  work <- file.path(scratch, "book-work")
  dir.create(work)
  # Reuse the checksum-verified public databases instead of downloading again.
  stopifnot(file.symlink(normalizePath("../practice-data"),
                        file.path(scratch, "practice-data")))
  unzip("docs/downloads/book-helpers.zip", exdir=work)
  old <- setwd(work)
  on.exit(setwd(old), add=TRUE, after=FALSE)
  stopifnot(all(file.exists(file.path("tools/book",
    c("practice.R", "fixture.R", "connection.R", "display.R", "load-practice.R",
      "opal-admin.R", "check-opal.R", "book-compose.sh", "book-rock.yml")))))
  for (chapter in c("book-00-start", "book-05-recipe", "book-06-events")) {
    run <- function() {
      lines <- readLines(file.path(root,"vignettes",paste0(chapter,".Rmd.orig")))
      label <- if (chapter=="book-00-start") "practice-login" else "chapter-setup"
      start <- match(paste0("```{r ", label, "}"), lines)
      end <- start + which(lines[(start+1):length(lines)] == "```")[1]
      eval(parse(text=lines[(start+1):(end-1)]))
      on.exit({try(ds.omop.disconnect(conns=conns),silent=TRUE)
               DSI::datashield.logout(conns); unlink(practice$path)})
      stopifnot(identical(names(conns), "site"),
        nrow(ds.omop.columns("person", conns=conns)$site)>0)
      message("PASS: downloaded helper bundle + visible ",chapter," setup without checkout")
    }
    run()
  }
})
