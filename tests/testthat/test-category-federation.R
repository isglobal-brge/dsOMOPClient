test_that("C01 C08 C48 custom order and reference survive three-site memory and Parquet", {
  skip_if_not_installed("arrow")
  ctx<-.clf_setup();.numeric_lexer()
  keys<-paste0("concept:",c(45000124L,45000123L,45000125L,45000126L))
  r<-.clf_recipe()
  r$variables$education<-omop_category(r$variables$education,omop_category_spec(
    levels=keys,order=omop_category_order(keys),ordered=FALSE,reference="concept:45000123"))
  p<-recipe_to_plan(r,version=2)
  previous<-NULL
  for(mode in c("memory","staged")) {
    symbol<-paste0("Cats_",mode)
    ds.omop.plan.execute(p,out=setNames(symbol,names(p$outputs)),symbol="labels",conns=ctx$conns,output_mode=mode)
    if(mode=="staged")for(server in ctx$servers) {
      descriptor<-server$getSessionData(server$getSessionIds()[[1L]],symbol)
      expect_identical(descriptor$metadata$format,"parquet")
      dsOMOP:::.publishReadChunks(descriptor,function(chunk)expect_equal(unname(contrasts(chunk$education)[2,]),rep(0,3)))
    }
    frames<-.clf_frames(ctx,symbol)
    for(f in frames) {
      expect_false(is.ordered(f$education))
      expect_identical(levels(f$education),c("Same [concept:45000124]","New education","Same [concept:45000125]","declared only"))
      if(mode=="memory")expect_equal(unname(contrasts(f$education)[2,]),rep(0,3))
    }
    values<-lapply(frames,function(f)as.character(f$education))
    if(!is.null(previous))expect_identical(values,previous)
    previous<-values
  }
})
test_that("C29 semantic flags keep their declared binary representations", {
  ctx<-.clf_setup();.numeric_lexer()
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("binary"))
  r<-omop_recipe(variables=list(i=omop_category(v,omop_category_spec(binary=omop_category_binary())),
    l=omop_category(v,omop_category_spec(binary=omop_category_binary("logical"))),
    f=omop_category(v,omop_category_spec(binary=omop_category_binary("factor")))))
  p<-recipe_to_plan(r,version=2)
  ds.omop.plan.execute(p,out=setNames("Flags",names(p$outputs)),symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Flags")) {
    expect_type(f$i,"integer");expect_type(f$l,"logical")
    expect_identical(levels(f$f),c("no","yes"))
    expect_true(all(f$i==1L));expect_true(all(f$l));expect_true(all(f$f=="yes"))
  }
})
