test_that("C18 C19 C57 C63 identity reduction follows mapping and preserves typed selectors", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM observation")
    rows<-expand.grid(position=1:10,person_id=1:18)
    rows$observation_id<-seq_len(nrow(rows));rows$observation_concept_id<-45000120L
    rows$observation_date<-as.character(as.Date("2020-01-01")+rows$position)
    rows$observation_type_concept_id<-0L
    rows$value_as_concept_id<-c(rep(45000123L,4),rep(45000124L,3),rep(45000125L,3))[rows$position]
    rows$position<-NULL
    DBI::dbWriteTable(con,"observation",rows,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  s<-omop_category_spec(recode=omop_category_recode(c(`concept:45000123`="category:x",`concept:45000124`="category:y",`concept:45000125`="category:y"),levels=c("category:x","category:y")))
  raw<-omop_category_rows(e,"value_as_concept_id")
  grouped<-omop_category(raw,s)
  yes<-omop_category_indicators(grouped,c(y="category:y"))[["y"]]
  variables<-list(post=omop_category(omop_variable(event=e,reducer=omop_reduce("mode",value="value_as_concept_id")),s),
    before=omop_category_reduce(grouped,omop_reduce("mode")),
    count=omop_category_reduce(grouped,omop_reduce("count")),
    distinct=omop_category_reduce(grouped,omop_reduce("n_distinct")),
    first=omop_category_reduce(grouped,omop_reduce("first_value")),
    last=omop_category_reduce(grouped,omop_reduce("last_value")),
    nth=omop_category_reduce(grouped,omop_reduce("nth_value",n=c(1L,3L))),
    any=omop_category_reduce(yes,omop_reduce("any")),
    all=omop_category_reduce(yes,omop_reduce("all")),
    total=omop_category_reduce(yes,omop_reduce("sum")))
  r<-omop_recipe(variables=variables)
  p<-recipe_to_plan(r,version=2)
  expect_type(p,"list")
  for(mode in c("memory","staged")) {
    symbol<-paste0("Reduced_",mode)
    ds.omop.plan.execute(p,out=setNames(symbol,names(p$outputs)),symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(f$post=="x"));expect_true(all(f$before=="y"))
      expect_true(all(f$count==10L));expect_true(all(f$distinct==2L))
      expect_true(all(f$first=="x"));expect_true(all(f$last=="y"))
      expect_true(all(f$any==1L));expect_true(all(f$all==0L));expect_true(all(f$total==6L))
      nth<-grep("^nth",names(f),value=TRUE);expect_length(nth,2L)
      expect_true(all(vapply(f[nth],function(x)all(x=="x"),logical(1))))
    }
  }
})
