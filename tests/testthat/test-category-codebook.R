test_that("C53 C54 C55 codebooks are local and published types match release", {
  r<-omop_recipe(variables=list(flag=omop_category(omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("binary")),omop_category_spec(binary=omop_category_binary("logical")))))
  prospective<-recipe_codebook(r)
  expect_true(length(prospective)>0L)
  expect_true(all(vapply(prospective,function(x)x$status=="prospective",logical(1))))
  ctx<-.clf_setup();.numeric_lexer()
  result<-recipe_execute(r,out="Book",symbol="labels",conns=ctx$conns)
  # Any network call from the local builder is a regression.
  testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
  book<-recipe_codebook(result)
  expect_identical(book[[1L]]$schema_version,"codebook-1")
  expect_identical(book[[1L]]$status,"published")
  cols<-book[[1L]]$columns
  expect_identical(cols$r_type[cols$name=="flag"],"logical")
  expect_false(any(c("count","n","observed","missingness","selected_date","event_id") %in% names(cols)))
  expect_error(recipe_codebook(result,output="absent"),"Unknown exact")
  expect_error(recipe_codebook(list()),"codebook_metadata_missing")
})
