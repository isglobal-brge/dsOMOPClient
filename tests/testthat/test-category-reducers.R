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
    rows$value_as_number<-c(rep(1,4),5:10)[rows$position]
    rows$position<-NULL
    DBI::dbWriteTable(con,"observation",rows,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  s<-omop_category_spec(recode=omop_category_recode(c(`concept:45000123`="category:x",`concept:45000124`="category:y",`concept:45000125`="category:y"),levels=c("category:x","category:y")))
  raw<-omop_category_rows(e,"value_as_concept_id")
  grouped<-omop_category(raw,s)
  yes<-omop_category_indicators(grouped,c(y="category:y"))[["y"]]
  variables<-list(numeric_post=omop_number_bins(omop_variable(event=e,reducer=omop_reduce("mode",value="value_as_number")),breaks=c(-Inf,4,Inf)),numeric_grouped=omop_category_reduce(omop_number_bins(omop_number_rows(e),breaks=c(-Inf,4,Inf)),omop_reduce("mode")),post=omop_category(omop_variable(event=e,reducer=omop_reduce("mode",value="value_as_concept_id")),s),
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
      expect_true(all(f$numeric_post=="(-Inf,4)"));expect_true(all(f$numeric_grouped=="[4,Inf)"))
      expect_true(all(f$count==10L));expect_true(all(f$distinct==2L))
      expect_true(all(f$first=="x"));expect_true(all(f$last=="y"))
      expect_true(all(f$any==1L));expect_true(all(f$all==0L));expect_true(all(f$total==6L))
      nth<-grep("^nth",names(f),value=TRUE);expect_length(nth,2L)
      expect_true(all(vapply(f[nth],function(x)all(x=="x"),logical(1))))
    }
  }
})

test_that("C05 C63 category reducers use declared equivalence, sentinel identity and three-valued flags", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM observation")
    rows<-expand.grid(position=1:3,person_id=1:18)
    rows$observation_id<-seq_len(nrow(rows));rows$observation_concept_id<-45000120L
    rows$observation_date<-as.character(as.Date("2020-01-01")+rows$position);rows$observation_type_concept_id<-0L
    rows$value_as_concept_id<-c(NA_integer_,45000124L,45000125L)[rows$position];rows$position<-NULL
    DBI::dbWriteTable(con,"observation",rows,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);empty<-omop_event("observation",999999L)
  raw<-omop_category_rows(e,"value_as_concept_id")
  miss<-omop_category_indicators(omop_category(raw,omop_category_spec(missing=omop_category_missing("level"))),c(m="special:missing"))[["m"]]
  known<-omop_category_indicators(raw,c(k="concept:45000124"))[["k"]]
  never<-omop_category_indicators(raw,c(k="concept:45000126"))[["k"]]
  empty_flag<-omop_category_indicators(omop_category_rows(empty,"value_as_concept_id"),c(k="concept:45000124"))[["k"]]
  reduce<-function(v,fn)omop_category_reduce(v,omop_reduce(fn))
  r<-omop_recipe(variables=list(distinct=reduce(raw,"n_distinct"),count=reduce(raw,"count"),missing=reduce(miss,"sum"),any=reduce(known,"any"),all=reduce(known,"all"),unknown=reduce(never,"any"),false_all=reduce(never,"all"),empty_count=reduce(omop_category_rows(empty,"value_as_concept_id"),"count"),empty_distinct=reduce(omop_category_rows(empty,"value_as_concept_id"),"n_distinct"),empty_any=reduce(empty_flag,"any"),empty_all=reduce(empty_flag,"all"),empty_sum=reduce(empty_flag,"sum")),labels=c(`45000124`="shared",`45000125`="shared"))
  recipe_execute(r,out="ThreeValued",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"ThreeValued")) {
    expect_true(all(f$distinct==1L));expect_true(all(f$count==3L));expect_true(all(f$missing==1L))
    expect_true(all(f$any==1L));expect_true(all(is.na(f$all)));expect_true(all(is.na(f$unknown)))
    expect_true(all(f$empty_any==0L));expect_true(all(is.na(f$empty_all)));expect_true(all(f$empty_sum==0L))
    expect_true(all(f$false_all==0L));expect_true(all(f$empty_count==0L));expect_true(all(f$empty_distinct==0L))
  }
})

test_that("C63 mode ties use typed identity rather than display order", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path);DBI::dbExecute(con,"DELETE FROM observation")
    rows<-expand.grid(position=1:2,person_id=1:18)
    rows$observation_id<-seq_len(nrow(rows));rows$observation_concept_id<-45000120L
    rows$observation_date<-"2020-01-01";rows$observation_type_concept_id<-0L
    rows$value_as_concept_id<-c(2L,10L)[rows$position];rows$value_as_number<-c(1.5,9.5)[rows$position];rows$position<-NULL
    DBI::dbWriteTable(con,"observation",rows[nrow(rows):1,],append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  concepts<-omop_category_rows(e,"value_as_concept_id")
  bins<-omop_category(omop_number_bins(omop_number_rows(e),breaks=0:10),omop_category_spec(order=omop_category_order(paste0("bin:",10:1))))
  mixed<-omop_category(concepts,omop_category_spec(recode=omop_category_recode(c(`concept:2`="category:alpha"),c("category:alpha","concept:10"))))
  r<-omop_recipe(variables=list(concept=omop_category_reduce(concepts,omop_reduce("mode")),bin=omop_category_reduce(bins,omop_reduce("mode")),mixed=omop_category_reduce(mixed,omop_reduce("mode"))),labels=c(`2`="Z",`10`="A"))
  recipe_execute(r,out="CanonicalTies",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"CanonicalTies")) {
    expect_true(all(f$concept=="Z"));expect_true(all(f$bin=="[1,2)"));expect_true(all(f$mixed=="10"))
  }
})
