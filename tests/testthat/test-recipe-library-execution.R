for (.library_backend in strsplit(Sys.getenv("DSOMOP_LIBRARY_BACKENDS", "sqlite,postgresql,mariadb,mysql"), ",", fixed=TRUE)[[1L]]) local({
  backend <- .library_backend
  test_that(paste("all 24 templates execute with independent values and memory/staged parity on",backend), {
    if (backend == "postgresql") skip_if(Sys.getenv("DSOMOP_TEST_POSTGRES_HOST") == "", "PostgreSQL test host not configured")
    ctx <- .library_cdm_setup(backend)
    ids <- recipe_library_list()$id
    for (i in seq_along(ids)) {
      cat("\nLIBRARY_CASE",backend,ids[[i]],"\n")
      tryCatch(.library_execute_case(ctx,ids[[i]],i), error = function(e) {
        fail(paste(ids[[i]],conditionMessage(e)))
      })
    }
  })
})
