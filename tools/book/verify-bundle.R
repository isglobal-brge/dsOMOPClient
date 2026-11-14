# Execute complete lessons with only the published helpers as runtime sources.
# Run from the client checkout after precompute.R and build-site.R.
args <- commandArgs(trailingOnly=TRUE)
if (length(args) && args[[1L]] == "--chapter-worker") {
  input <- args[[2L]]
  expected <- args[[3L]]
  options(width=88)
  knitr::opts_knit$set(root.dir=normalizePath("."))
  knitr::opts_chunk$set(error=FALSE, warning=TRUE, message=FALSE,
                       comment="#>", collapse=FALSE)
  run <- function() {
    on.exit({
      if (exists("conns", inherits=FALSE)) {
        try(dsOMOPClient::ds.omop.disconnect(conns=conns), silent=TRUE)
        try(DSI::datashield.logout(conns), silent=TRUE)
      }
      if (exists("practice", inherits=FALSE)) unlink(practice$path)
      if (exists("two_conns", inherits=FALSE)) {
        try(dsOMOPClient::ds.omop.disconnect(conns=two_conns), silent=TRUE)
        try(DSI::datashield.logout(two_conns), silent=TRUE)
      }
      if (exists("pair", inherits=FALSE)) unlink(pair$path)
    })
    output <- sub("\\.orig$", "", input)
    knitr::knit(input, output=output, envir=environment(), quiet=TRUE)
    # Compare the entire lesson, including exercises, solutions and all outputs.
    stopifnot(identical(unname(tools::md5sum(output)),
                        unname(tools::md5sum(expected))))
    if (grepl("book-07-verbal-autopsy", input)) {
      stopifnot(identical(recipe_to_plan(va), recipe_to_plan(reference)))
      # These are public synthetic fixtures: verify the actual assigned tables too.
      original <- DSLite::getDSLiteData(conns, "D_va")$site
      rebuilt <- DSLite::getDSLiteData(conns, "D_va_reference")$site
      stopifnot(nrow(rebuilt) == 24L, identical(original, rebuilt),
        sum(rebuilt$cause_of_death == "Myocardial infarction", na.rm=TRUE) == 12L,
        sum(rebuilt$cause_of_death == "Pneumonia", na.rm=TRUE) == 6L)
      message("PASS: capstone native plan, assigned table and cause categories")
    }
    if (grepl("book-09-reproduce", input)) {
      stopifnot(identical(manifest, jsonlite::read_json(file.path(
        Sys.getenv("BOOK_PRACTICE_DIR", "../practice-data"), "manifest.json"))))
      message("PASS: Appendix A reads the loader manifest")
    }
  }
  run()
  message("PASS: all chunks and byte-identical output: ", basename(input))
  quit(save="no", status=0L)
}

root <- normalizePath(".")
practice_dir <- normalizePath(Sys.getenv("BOOK_PRACTICE_DIR", "../practice-data"),
                              mustWork=TRUE)
stopifnot(all(file.exists(file.path(practice_dir,
  c("gibleed.sqlite", "numeric.sqlite", "manifest.json")))))
inputs <- sort(list.files("vignettes", "^book-.*\\.Rmd\\.orig$", full.names=TRUE))
stopifnot(length(inputs) == 16L)
local({
  scratch <- tempfile("book-reader-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive=TRUE), add=TRUE)
  # The child receives staged lesson text and this runner, never checkout paths.
  worker <- file.path(scratch, "chapter-worker.R")
  stopifnot(file.copy("tools/book/verify-bundle.R", worker))
  old_practice <- Sys.getenv("BOOK_PRACTICE_DIR", unset=NA_character_)
  Sys.unsetenv("BOOK_PRACTICE_DIR")
  on.exit({
    if (is.na(old_practice)) Sys.unsetenv("BOOK_PRACTICE_DIR")
    else Sys.setenv(BOOK_PRACTICE_DIR=old_practice)
  }, add=TRUE)
  run_chapter <- function(input, custom=FALSE) {
    chapter <- basename(input)
    case <- file.path(scratch, paste0(chapter, if (custom) "-custom" else ""))
    work <- file.path(case, "book-work")
    dir.create(work, recursive=TRUE)
    # Only the loader-produced databases and manifest are shared, never a checkout.
    data_dir <- file.path(case, if (custom) "custom-data" else "practice-data")
    stopifnot(file.symlink(practice_dir, data_dir))
    if (custom) Sys.setenv(BOOK_PRACTICE_DIR=data_dir)
    else Sys.unsetenv("BOOK_PRACTICE_DIR")
    unzip(file.path(root, "docs/downloads/book-helpers.zip"), exdir=work)
    stopifnot(!dir.exists(file.path(work, "tools/book/evidence")))
    stopifnot(file.copy(file.path(root, input), file.path(work, chapter)),
      file.copy(file.path(root, sub("\\.orig$", "", input)),
                file.path(case, "expected.Rmd")))
    old <- setwd(work)
    on.exit(setwd(old), add=TRUE)
    status <- system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(worker), "--chapter-worker", shQuote(chapter),
        shQuote(file.path(case, "expected.Rmd"))))
    if (status != 0L) stop("Published bundle chapter failed: ", chapter)
  }
  for (input in inputs) run_chapter(input)
  # No ../practice-data exists in this case: the override must actually be used.
  run_chapter(inputs[grepl("book-09-reproduce", inputs)], custom=TRUE)
  message("PASS: 16 complete chapters in fresh R processes from the final ZIP; ",
          "Appendix A also passed with BOOK_PRACTICE_DIR and no default data path")
})
