root <- normalizePath('.')
inputs <- list.files('vignettes', '^book-(00-showcase|11-parameters).*Rmd.orig$', full.names=TRUE)
stopifnot(length(inputs)==2L)
local({
 scratch<-tempfile('book-fix2-reader-');dir.create(scratch)
 on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
 for(input in inputs){
  case<-file.path(scratch,basename(input));work<-file.path(case,'book-work')
  dir.create(work,recursive=TRUE)
  stopifnot(file.symlink(normalizePath('../practice-data'),file.path(case,'practice-data')))
  unzip('docs/downloads/book-helpers.zip',exdir=work)
  stopifnot(file.copy(input,file.path(work,basename(input))),
    file.copy(sub('.orig$','',input),file.path(case,'expected.Rmd')))
  status<-withr::with_dir(work,system2(file.path(R.home('bin'),'Rscript'),
    c('--vanilla',shQuote(file.path(root,'tools/book/verify-bundle.R')),'--chapter-worker',
      shQuote(basename(input)),shQuote(file.path(case,'expected.Rmd')))))
  stopifnot(status==0L)
 }
 message('PASS: both final edited chapters reproduce byte for byte using only published helpers')
})
