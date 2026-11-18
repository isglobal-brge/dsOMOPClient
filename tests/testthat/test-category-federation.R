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

test_that("C04 C07 public period ranges stay open and integer grids keep unused levels", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET observation_date=CASE WHEN person_id<=6 THEN '2020-01-01' WHEN person_id<=12 THEN '2020-03-01' ELSE '2020-04-01' END WHERE observation_concept_id=45000120")
    DBI::dbExecute(con,"UPDATE observation SET value_as_number=CASE WHEN person_id<=9 THEN 1 ELSE 10 END WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  source<-omop_anchor(e)
  open<-omop_date(source,omop_date_spec(precision="month"))
  declared<-omop_date(source,omop_date_spec(precision="month",level_range=c("2020-01-01","2020-03-31")))
  scale<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_number"))
  r<-omop_recipe(variables=list(open=omop_category(open,omop_category_spec(ordered=TRUE)),declared=omop_category(declared,omop_category_spec(ordered=TRUE)),scale=scale,
    ordinal=omop_category(omop_number_bins(scale,breaks=seq(-.5,10.5,1)),omop_category_spec(ordered=TRUE))))
  recipe_execute(r,out="OpenPeriods",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"OpenPeriods")) {
    expect_identical(levels(f$open),c("2020-01","2020-03","2020-04"))
    expect_identical(levels(f$declared),c("2020-01","2020-02","2020-03","2020-04"))
    expect_length(levels(f$ordinal),11L);expect_true(is.ordered(f$ordinal));expect_true(is.numeric(f$scale))
  }
  r$variables$declared<-omop_category(declared,omop_category_spec(order=omop_category_order(paste0("period:2020-0",1:3))))
  expect_error(recipe_execute(r,out="IncompleteOrder",symbol="labels",conns=ctx$conns),"category_domain_incompatible")
  for(server in ctx$servers)expect_error(server$getSessionData(server$getSessionIds()[[1L]],"IncompleteOrder"))
})

test_that("C49 staged category metadata tampering fails before materialisation", {
  ctx<-.clf_setup();.numeric_lexer()
  r<-.clf_recipe();r$variables$education<-omop_category(r$variables$education,omop_category_spec(ordered=TRUE))
  recipe_execute(r,out="Tamper",symbol="labels",conns=ctx$conns,output_mode="staged")
  server<-ctx$servers[[1L]];d<-server$getSessionData(server$getSessionIds()[[1L]],"Tamper")
  spec<-.conceptUnionReports(DSI::datashield.aggregate(ctx$conns,call("omopFactorLevelsDS",as.symbol("Tamper"))))
  original<-d$metadata$concept_presentation
  meta<-dsOMOP:::.ds_arg(original)
  meta$contracts$education$category$spec$ordered<-FALSE
  d$metadata$concept_presentation<-dsOMOP:::.conceptEncode(meta)
  expect_error(dsOMOP::omopAsFactorColumnsDS(d,.ds_encode(spec)),"not committed|revoked")
})

test_that("C52 C61 label merges and grouped families follow the DataSHIELD level rule", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_concept_id=CASE WHEN person_id=1 THEN 45000125 ELSE 45000124 END WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L,name="e")
  v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  r<-omop_recipe(variables=list(x=omop_category(v,omop_category_spec(ordered=FALSE))),labels=c(`45000124`="merged",`45000125`="merged"))
  # Since 2.10.6 a rare value needs no per-level minimum: two observed values
  # pass DataSHIELD's level rule, merged or not, and the factor is shared.
  recipe_execute(r,out="RareMerge",symbol="labels",conns=ctx$conns)
  for (f in .clf_frames(ctx,"RareMerge")) expect_true(is.factor(f$x))
  r$variables$x<-omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  recipe_execute(r,out="RareGrouped",symbol="labels",conns=ctx$conns)
  for (f in .clf_frames(ctx,"RareGrouped")) expect_true(is.factor(f$x))
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM observation WHERE person_id<>1");DBI::dbDisconnect(con)
  }
  r<-omop_recipe(events=list(e),outputs=omop_output("sparse",format="sparse",columns=character(),options=list(sources=list(e),analyses="binary")),category_spec=omop_category_spec(ordered=FALSE))
  expect_error(recipe_execute(r,out="SparseRare",symbol="labels",conns=ctx$conns),"insufficient_support|insufficient support")
})

test_that("C50 C55 category labels and codebooks use only each component's retained sites", {
  for(strict in c(TRUE,FALSE))local({
    ctx<-.clf_setup(strict=strict,excluded=TRUE);.numeric_lexer()
    r<-.clf_recipe()
    r$variables$education<-omop_category(r$variables$education,omop_category_spec(ordered=FALSE))
    few<-omop_event("observation",45000125L,name="few")
    r$events<-list(few=few)
    r$populations<-list(base=omop_population("base"),few=omop_population("few",index=omop_index(few)))
    r$outputs<-list(output=omop_output("output",rows="base"),small=omop_output("small",rows="few",grain="episode"))
    for(policy in c("error","skip")) {
      symbols<-c(output=paste0("CategoryRetained_",policy),small=paste0("CategorySmall_",policy))
      execute<-function()recipe_execute(r,out=symbols,symbol="labels",conns=ctx$conns,on_small_site=policy)
      if(strict&&policy=="error") {
        expect_error(execute(),"insufficient_support")
        expect_true(all(vapply(DSI::datashield.symbols(ctx$conns),function(x)!any(symbols %in% x),logical(1))))
      } else {
        result<-suppressWarnings(execute())
        book<-recipe_codebook(result)
        expect_true("small/small" %in% names(book))
        cols<-book[["small/small"]]$columns
        labels<-cols$levels[[which(cols$name=="education")]]$labels
        expect_false(any(grepl("Excluded",labels)))
        expect_false(any(grepl("45000127",unlist(book[["small/small"]]$columns$levels))))
      }
    }
  })
})
