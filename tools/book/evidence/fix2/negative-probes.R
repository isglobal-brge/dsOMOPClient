# Regression probes: each former packaging defect must fail full-chapter validation.
root <- normalizePath(".")
local({
  scratch <- tempfile("book-missing-dependency-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive=TRUE), add=TRUE)
  Sys.unsetenv("BOOK_PRACTICE_DIR")
  for (chapter in c("book-07-verbal-autopsy", "book-09-reproduce")) {
    work <- file.path(scratch, chapter)
    dir.create(work)
    unzip("docs/downloads/book-helpers.zip", exdir=work)
    input <- file.path(work, paste0(chapter, ".Rmd.orig"))
    file.copy(file.path("vignettes", basename(input)), input)
    expected <- normalizePath(file.path("vignettes", paste0(chapter, ".Rmd")))
    if (chapter == "book-07-verbal-autopsy") {
      unlink(file.path(work, "tools/dates/va-native.R"))
      marker <- "solution"
    } else {
      lines <- readLines(input)
      lines[grepl('^manifest <-', lines)] <-
        'manifest <- jsonlite::read_json("tools/book/evidence/book2/practice-manifest.json")'
      writeLines(lines, input)
      marker <- "practice-manifest"
    }
    logfile <- file.path(root, "tools/book/evidence/fix2", paste0("negative-", chapter, ".log"))
    status <- local({
      old <- setwd(work)
      on.exit(setwd(old))
      system2(file.path(R.home("bin"), "Rscript"), c("--vanilla",
        shQuote(file.path(root, "tools/book/verify-bundle.R")), "--chapter-worker",
        shQuote(basename(input)), shQuote(expected)), stdout=logfile, stderr=logfile)
    })
    log <- readLines(logfile)
    stopifnot(status != 0L, any(grepl(marker, log)),
              any(grepl("cannot open the connection", log)))
    message("PASS: full-chapter verifier rejects former defect in ", chapter,
            " at chunk ", marker)
  }
})
