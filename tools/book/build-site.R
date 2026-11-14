# The site consumes committed, precomputed Markdown; no connection is opened.
stopifnot(length(list.files("vignettes", "^book-.*\\.Rmd$")) == 16L)
for (path in list.files("vignettes", "^book-.*\\.Rmd$", full.names=TRUE)) {
  stopifnot(!any(grepl("^```\\{r", readLines(path))))
}
pkgdown::build_site(new_process = FALSE, examples = FALSE, install = FALSE, lazy = FALSE)

dir.create("docs/downloads",showWarnings=FALSE)
helpers <- c("load-practice.R", "practice.R", "fixture.R", "connection.R",
             "display.R", "opal-admin.R", "check-opal.R", "book-compose.sh", "book-rock.yml")
helpers <- c(file.path("tools/book", helpers), "tools/dates/va-native.R")
stopifnot(all(file.copy(helpers, "docs/downloads", overwrite=TRUE)))
# Recreate the archive so removed helpers cannot linger from an earlier build.
unlink("docs/downloads/book-helpers.zip")
utils::zip("docs/downloads/book-helpers.zip", helpers)
