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
    expect_true(all(c("nominal","yesno","indicators_middle") %in% names(f)))
    expect_true(is.ordered(f$band));expect_false(is.ordered(f$nominal))
    expect_identical(as.character(f$band),as.character(f$nominal))
    expect_type(f$n,"integer");expect_type(f$flag,"integer");expect_true(all(f$yesno=="yes"));expect_true(all(f$indicators_middle==1L))
  }
})

test_that("C45 C47 generated panel flags and matrix reference copies retain typed roles", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L,name="e")
  base<-function(output)omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),outputs=output)
  panel<-omop_output("panel",format="person_period",rows="p",columns=c("e_binary","yesno"),options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count")),category_columns=list(yesno=omop_category_column("e_binary",category_spec=omop_category_spec(binary=omop_category_binary("factor")))))
  recipe_execute(base(panel),out="PanelCopy",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"PanelCopy")) {
    expect_type(f$e_binary,"integer");expect_s3_class(f$yesno,"factor")
    expect_equal(as.character(f$yesno),ifelse(f$e_binary==1L,"yes","no"))
  }
  sparse<-function(copies)omop_output("matrix",format="sparse",rows="p",columns=character(),options=list(sources=list(e),analyses="binary"),category_columns=copies)
  r<-base(sparse(list(bad=omop_category_column("covariate_value",component="matrix",category_spec=omop_category_spec(binary=omop_category_binary("factor"))))))
  expect_error(recipe_execute(r,out="NoMatrixFactor",symbol="labels",conns=ctx$conns),"category_type_unsupported")
  r<-base(sparse(list(code=omop_category_column("concept_id",component="covariateRef",category_spec=omop_category_spec(label_style="code")))))
  result<-recipe_execute(r,out="MatrixReference",symbol="labels",conns=ctx$conns)
  columns<-recipe_codebook(result)[["matrix/covariateRef"]]$columns
  expect_identical(columns$r_type[columns$name=="code"],"factor")
  r<-base(omop_output("w",category_columns=list(a=omop_category_column("b"),b=omop_category_column("a"))))
  expect_error(recipe_execute(r,out="NoSiblingCopies",symbol="labels",conns=ctx$conns),"category_spec_invalid|Unknown|unknown")
})
