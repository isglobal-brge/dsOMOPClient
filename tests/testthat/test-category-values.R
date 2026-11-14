test_that("C05 C26 C27 C65 merges and zero sentinel semantics stay distinct", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_concept_id = CASE WHEN person_id<=6 THEN 0 WHEN person_id<=12 THEN 45000123 ELSE NULL END WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id",missing="keep"))
  for(unknown in c("keep","level","missing"))for(na in c("keep","level")) {
    r<-omop_recipe(variables=list(answer=omop_category(v,omop_category_spec(missing=omop_category_missing(na,unknown)))),labels=c(`45000123`="Missing"))
    p<-recipe_to_plan(r,version=2)
    symbol<-paste0("Sentinels_",unknown,"_",na)
    ds.omop.plan.execute(p,out=setNames(symbol,names(p$outputs)),symbol="labels",conns=ctx$conns)
    for(f in .clf_frames(ctx,symbol)) {
      expect_identical(attr(f,"omop_concepts")$answer$category$spec$missing$na,na)
      expect_true("Missing" %in% levels(f$answer))
      if(na=="level")expect_true("Missing [missing]" %in% levels(f$answer))
      if(unknown=="level")expect_true("Unknown" %in% levels(f$answer))
      expect_equal(sum(is.na(f$answer)),if(na=="level")0L else if(unknown=="missing")12L else 6L)
    }
  }
  for(unknown in c("level","missing")) {
    r<-omop_recipe(variables=list(answer=omop_category(v,omop_category_spec(missing=omop_category_missing("level",unknown)))),labels=c(`0`="shared",`45000123`="shared"))
    p<-recipe_to_plan(r,version=2)
    expect_error(ds.omop.plan.execute(p,out=setNames("Mixed",names(p$outputs)),symbol="labels",conns=ctx$conns),"category_spec_invalid")
  }
})
test_that("C06 bins admit an explicit subsequent nominal recode", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L)
  v<-omop_number_bins(omop_variable(event=e,reducer=omop_reduce("count")),breaks=c(0,1,2,Inf))
  spec<-omop_category_spec(recode=omop_category_recode(c(`bin:1`="category:outside",`bin:2`="category:inside",`bin:3`="category:outside"),levels=c("category:outside","category:inside")))
  r<-omop_recipe(variables=list(band=omop_category(v,spec)))
  p<-recipe_to_plan(r,version=2)
  ds.omop.plan.execute(p,out=setNames("Nominal",names(p$outputs)),symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Nominal")) {
    expect_false(is.ordered(f$band));expect_identical(levels(f$band),c("outside","inside"))
    expect_true(all(f$band=="inside"))
  }
})
