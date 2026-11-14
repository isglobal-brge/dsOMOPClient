test_that("C45 output category copies bind original bins and flags independently", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L)
  r<-omop_recipe(variables=list(n=omop_variable(event=e,reducer=omop_reduce("count")),
    band=omop_number_bins(omop_variable(event=e,reducer=omop_reduce("count")),breaks=c(0,1,2,Inf)),
    flag=omop_variable(event=e,reducer=omop_reduce("binary"))),
    outputs=omop_output("o",format="wide",category_columns=list(
      nominal=omop_category_column("band",category_spec=omop_category_spec(ordered=FALSE)),
      yesno=omop_category_column("flag",category_spec=omop_category_spec(binary=omop_category_binary("factor"))),
      indicators=omop_category_indicators(omop_category_column("band"),c(middle="bin:2")))))
  recipe_execute(r,out=c(o="Copies"),symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Copies")) {
    expect_true(is.ordered(f$band));expect_false(is.ordered(f$nominal))
    expect_identical(as.character(f$band),as.character(f$nominal))
    expect_type(f$n,"integer");expect_type(f$flag,"integer");expect_true(all(f$yesno=="yes"));expect_true(all(f$indicators_middle==1L))
  }
})
