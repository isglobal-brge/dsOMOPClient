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

test_that("C02 C05 C65 effective order and inherited aliases survive grouping", {
  ctx<-.clf_setup();.numeric_lexer()
  keys<-paste0("concept:",c(45000124L,45000123L,45000125L))
  hash<-as.character(openssl::sha256(charToRaw(.jcsCanonical(as.list(keys)))))
  e<-omop_event("observation",45000120L)
  v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  declarations<-list(explicit=omop_category_order(keys),metadata=omop_category_order(metadata=list(id="education",version="1",sha256=hash,keys=keys)))
  r<-omop_recipe(variables=lapply(declarations,function(order)omop_category(v,omop_category_spec(order=order,ordered=FALSE,reference="concept:45000124"))),labels=c(`45000123`="merged",`45000124`="merged"))
  result<-recipe_execute(r,out="Aliases",symbol="labels",conns=ctx$conns)
  book<-recipe_codebook(result)[[1L]]$columns
  for(f in .clf_frames(ctx,"Aliases")) {
    a<-attr(f$explicit,"omop_category_contract");b<-attr(f$metadata,"omop_category_contract")
    expect_identical(a$category,b$category)
    expect_identical(a$reference,"concept:45000123")
    expect_identical(unname(a$aliases["concept:45000124"]),"concept:45000123")
  }
  expect_true(any(vapply(book$aliases,function(x)"concept:45000124" %in% names(x),logical(1))))
  r$variables$explicit<-omop_category(v,omop_category_spec(order=omop_category_order(keys[c(1,3,2)])))
  expect_error(recipe_execute(r,out="SeparatedAliases",symbol="labels",conns=ctx$conns),"contiguous")
  rows<-omop_category_rows(e,"value_as_concept_id")
  grouped<-omop_category_reduce(rows,omop_reduce("mode"))
  recode<-omop_category_spec(recode=omop_category_recode(c(`concept:45000124`="category:joined"),c("category:joined","concept:45000125")))
  r<-omop_recipe(variables=list(x=omop_category(grouped,recode)),labels=c(`45000123`="merged",`45000124`="merged"))
  recipe_execute(r,out="InheritedAlias",symbol="labels",conns=ctx$conns)
  for(i in 1:3)expect_equal(sum(as.character(.clf_frames(ctx,"InheritedAlias")[[i]]$x)=="joined"),if(i==1L)12L else 18L)
  r$variables$x<-omop_category(grouped,omop_category_spec(recode=omop_category_recode(c(`concept:45000123`="category:a",`concept:45000124`="category:b"),c("category:a","category:b","concept:45000125"))))
  expect_error(recipe_execute(r,out="ConflictingAliases",symbol="labels",conns=ctx$conns),"category_spec_invalid")
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_concept_id=0 WHERE value_as_concept_id=45000123");DBI::dbDisconnect(con)
  }
  r<-omop_recipe(variables=list(x=omop_category(grouped,omop_category_spec(missing=omop_category_missing("level","level")))),labels=c(`0`="merged",`45000124`="merged"))
  expect_error(recipe_execute(r,out="InheritedZero",symbol="labels",conns=ctx$conns),"category_spec_invalid")
  r$variables$x<-omop_category(grouped,omop_category_spec(recode=omop_category_recode(c(`concept:45000124`="category:ordinary"),c("category:ordinary","concept:45000125")),missing=omop_category_missing("level","level")))
  recipe_execute(r,out="ConsumedZero",symbol="labels",conns=ctx$conns)
  for(i in 1:3)expect_equal(sum(as.character(.clf_frames(ctx,"ConsumedZero")[[i]]$x)=="ordinary"),if(i==1L)12L else 18L)
})
