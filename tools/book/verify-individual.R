# Reproduce the documented individual-download route in an empty reader directory.
# Run after build-site.R; retain its log and remove only this scratch directory.
local({
  root <- normalizePath('.')
  scratch <- tempfile('book3-fix1-individual-'); dir.create(scratch)
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  reader <- file.path(scratch,'book-work');dir.create(file.path(reader,'tools/book'),recursive=TRUE)
  dir.create(file.path(reader,'tools/dates'),recursive=TRUE)
  helpers <- c('practice.R','fixture.R','connection.R','display.R','load-practice.R','load-numeric.R','case-helpers.R')
  # Each helper must be linked in the actual alternative list, before the capstone.
  setup <- readLines('vignettes/book-00-start.Rmd.orig')
  alternative <- paste(setup[grep('Alternatively, save',setup):grep('chapter 18.s solution uses it',setup)],collapse='\n')
  stopifnot(all(vapply(helpers,function(h) grepl(paste0('../downloads/',h),alternative,fixed=TRUE),logical(1))))
  stopifnot(all(file.copy(file.path('docs/downloads',helpers),file.path(reader,'tools/book'))),
    file.copy('docs/downloads/va-native.R',file.path(reader,'tools/dates')))
  data <- file.path(scratch,'practice-data');dir.create(data)
  # Reuse checksum-pinned downloads; the loader still verifies each archive hash.
  archives <- list.files('../practice-data','[.]zip$',full.names=TRUE)
  stopifnot(length(archives)==2L,all(file.copy(archives,data)))
  withr::with_dir(reader, {
    stopifnot(all(file.exists(file.path('tools/book',helpers))),file.exists('tools/dates/va-native.R'))
    status <- system2(file.path(R.home('bin'),'Rscript'),c('tools/book/load-practice.R','../practice-data'))
    stopifnot(status==0L,all(file.exists(file.path(data,c('gibleed.sqlite','numeric.sqlite','manifest.json')))))
  })
  status <- system2(file.path(R.home('bin'),'Rscript'),c('tools/book/verify-numeric-loader.R',shQuote(data)))
  stopifnot(status==0L)
  cat('PASS: documented individual-file placement, setup checks, fresh two-resource loader and all 39 numeric tables/scalars\n')
})
