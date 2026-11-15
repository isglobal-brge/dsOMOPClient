# The site consumes committed, precomputed Markdown; no connection is opened.
stopifnot(length(list.files("vignettes", "^book-.*\\.Rmd$")) == 25L)
for (path in list.files("vignettes", "^book-.*\\.Rmd$", full.names=TRUE)) {
  stopifnot(!any(grepl("^```\\{r", readLines(path))))
}
# With article names, refresh only changed pages after a complete site build.
articles <- commandArgs(trailingOnly=TRUE)
# Drop obsolete numbered book output only for a complete article build.
if (!length(articles)) unlink(list.files("docs/articles", "^book-.*[.](html|md)$", full.names=TRUE))
if (length(articles)) {
  for (article in articles) pkgdown::build_article(article, new_process=FALSE, lazy=FALSE)
  pkgdown::build_llm_docs()
} else pkgdown::build_site(new_process=FALSE, examples=FALSE, install=FALSE, lazy=FALSE)

dir.create("docs/downloads",showWarnings=FALSE)
helpers <- c("load-practice.R", "load-numeric.R", "practice.R", "fixture.R", "connection.R",
             "display.R", "case-helpers.R", "opal-admin.R", "check-opal.R", "book-compose.sh", "book-rock.yml")
helpers <- c(file.path("tools/book", helpers), "tools/book/capstone.R")
stopifnot(all(file.copy(helpers, "docs/downloads", overwrite=TRUE)))
# Recreate the archive so removed helpers cannot linger from an earlier build.
unlink("docs/downloads/book-helpers.zip")
utils::zip("docs/downloads/book-helpers.zip", helpers)

# Some pkgdown stages restore cached assets after init_site(). Ship the exact
# reviewed book assets, including new capability-card styles, at the end.
stopifnot(all(file.copy(c("pkgdown/extra.css", "pkgdown/extra.js"), "docs", overwrite=TRUE)))
file.create("docs/.nojekyll")
