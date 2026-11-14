# The site consumes committed, precomputed Markdown; no connection is opened.
stopifnot(length(list.files("vignettes", "^book-.*\\.Rmd$")) == 16L)
for (path in list.files("vignettes", "^book-.*\\.Rmd$", full.names=TRUE)) {
  stopifnot(!any(grepl("^```\\{r", readLines(path))))
}
pkgdown::build_site(new_process = FALSE, examples = FALSE, install = FALSE, lazy = TRUE)

dir.create("docs/downloads",showWarnings=FALSE)
file.copy(c("tools/book/load-practice.R","tools/book/practice.R","tools/book/fixture.R"),
          "docs/downloads",overwrite=TRUE)
