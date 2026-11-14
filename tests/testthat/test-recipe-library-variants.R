# Select cases/backends explicitly for economical reruns; the default covers
# the whole new boundary matrix on SQLite. Vendor jobs select their backends.
for(.library_spec in .library_variant_specs()) local({
  spec<-.library_spec
  selected<-Sys.getenv("DSOMOP_LIBRARY_CASES","")
  if(nzchar(selected)&&!grepl(selected,paste(spec$id,spec$case,sep="/"))) return(invisible(NULL))
  for(.backend in strsplit(Sys.getenv("DSOMOP_LIBRARY_VARIANT_BACKENDS","sqlite"),",",fixed=TRUE)[[1]]) local({
    backend<-.backend
    test_that(paste(spec$id,spec$case,"boundary artifacts and public execution on",backend), {
      tryCatch(.library_variant_execute(spec,backend,record=Sys.getenv("DSOMOP_LIBRARY_RECORD_VARIANTS")=="true"),
        error=function(e) {cat("\nVARIANT_ERROR",spec$id,spec$case,conditionMessage(e),"\n");stop(e)})
    })
  })
})

if (!nzchar(Sys.getenv("DSOMOP_LIBRARY_CASES", ""))) test_that("measurement variants pin one vocabulary-defined dense schema even without site events", {
  root <- dirname(.library_test_dir("panel.measurements"))
  schemas <- lapply(c("two_concepts", "empty", "unavailable"), function(case)
    .library_test_json(file.path(root, case, "schema.json")))
  expect_identical(schemas[[1]], schemas[[2]])
  expect_identical(schemas[[1]], schemas[[3]])
})
