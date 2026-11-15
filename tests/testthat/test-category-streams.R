test_that("C36 variable-local row exclusion cannot create a paired candidate stream", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("measurement",3000963L,name="lab")
  a<-omop_number_rows(e,number_spec=omop_number_spec(unit=omop_unit(8713L,unknown="missing")))
  b<-omop_number_rows(e,number_spec=omop_number_spec(unit=omop_unit(8713L,unknown="exclude")))
  r<-omop_recipe(variables=list(x=omop_repeated(e,list(a=a,b=b),timepoints=omop_timepoints(list(now=omop_timepoint()),anchor="2020-01-01"))))
  expect_error(recipe_execute(r,out="NoDifferentStreams",symbol="labels",conns=ctx$conns),"repeated_grain_incompatible|identical event|candidate stream")
})
