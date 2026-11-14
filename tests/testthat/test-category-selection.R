test_that("C34 C36 C37 C38 canonical paired missingness and reuse determine one shared row", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM observation")
    rows<-expand.grid(position=1:3,person_id=1:18)
    rows$observation_id<-rows$person_id*100L+c(2L,10L,20L)[rows$position]
    rows$observation_concept_id<-45000120L;rows$observation_type_concept_id<-0L
    rows$observation_date<-as.character(as.Date("2020-01-01")+c(0,0,1)[rows$position])
    rows$value_as_number<-c(2,10,20)[rows$position]
    rows$value_as_concept_id<-c(NA_integer_,45000124L,45000124L)[rows$position]
    rows$position<-NULL;DBI::dbWriteTable(con,"observation",rows,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  category<-omop_category_rows(e,"value_as_concept_id")
  flag<-omop_category_indicators(category,c(known="concept:45000124"),missing="zero")
  values<-list(value=omop_number_rows(e),flags=flag)
  points<-omop_timepoints(list(a=omop_timepoint(tolerance=c(-1,1)),b=omop_timepoint(tolerance=c(-1,1))),anchor="2020-01-01",reuse=FALSE)
  bundle<-omop_repeated(e,values,timepoints=points,missing="keep")
  skip<-omop_repeated(e,values,timepoints=points,missing="skip_any")
  empty<-omop_repeated(e,list(value=omop_number_rows(e),present=omop_number_present(omop_number_rows(e)),missing=omop_number_missing(omop_number_rows(e))),timepoints=omop_timepoints(list(later=omop_timepoint(6,"months")),anchor="2020-01-01"))
  r<-omop_recipe(variables=list(keep=bundle,skip=skip,empty=empty))
  for(mode in c("memory","staged")) {
    symbol<-paste0("Selection_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(f$keep_value_a==2));expect_true(all(f$keep_value_b==10))
      expect_true(all(f$skip_value_a==10));expect_true(all(f$skip_value_b==20))
      expect_true(all(f$keep_flags_known_a==0L));expect_true(all(f$keep_flags_known_b==1L))
      expect_true(all(is.na(f$empty_value_later)));expect_true(all(f$empty_present_later==0L));expect_true(all(f$empty_missing_later==1L))
    }
  }
})

test_that("C35 C38 own anchors, overflow and empty targets retain explicit representation semantics", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_number=5 WHERE observation_concept_id=45000120");DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);empty<-omop_event("observation",999999L)
  raw<-omop_number_rows(e)
  cat<-omop_category_rows(e,"value_as_concept_id",omop_category_spec(missing=omop_category_missing("level")))
  values<-list(raw=raw,flag=omop_number_flag(raw,"gt",3),present=omop_number_present(raw),missing=omop_number_missing(raw),sentinel=omop_category_indicators(cat,c(missing="special:missing")),zero=omop_category_indicators(omop_category_rows(e,"value_as_concept_id"),c(known="concept:45000124"),missing="zero"))
  grid<-omop_timepoints(list(own=omop_timepoint(anchor="2020-01-01",select="last",tolerance=c(-7,0)),undefined=omop_timepoint(anchor=omop_anchor(empty)),overflow=omop_timepoint(10000,"years"),empty=omop_timepoint()),anchor="2090-01-01")
  r<-omop_recipe(variables=list(x=omop_repeated(e,values,timepoints=grid)))
  for(mode in c("memory","staged")) {
    symbol<-paste0("Targets_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(f$x_raw_own==5));expect_true(all(f$x_present_own==1L))
      expect_true(all(vapply(f[grep("_(undefined|overflow)$",names(f))],function(x)all(is.na(x)),logical(1))))
      expect_true(all(is.na(f$x_raw_empty)));expect_true(all(is.na(f$x_flag_empty)))
      expect_true(all(f$x_present_empty==0L));expect_true(all(f$x_missing_empty==1L));expect_true(all(f$x_sentinel_missing_empty==1L));expect_true(all(f$x_zero_known_empty==0L))
    }
  }
})

test_that("C43 repeated scalars keep episodes and explicit long broadcasts", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_number=5 WHERE observation_concept_id=45000120")
    DBI::dbExecute(con,"INSERT INTO observation (observation_id,person_id,observation_concept_id,observation_date,observation_type_concept_id,value_as_number) SELECT observation_id+1000,person_id,observation_concept_id,'2020-02-01',0,9 FROM observation WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L,name="e")
  grid<-omop_timepoints(list(baseline=omop_timepoint()))
  bundle<-omop_repeated(e,list(value=omop_number_rows(e)),timepoints=grid,broadcast=TRUE)
  other<-omop_number(bundle$value_baseline,broadcast=TRUE)
  r<-omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e,select="all")),variables=list(x=bundle,y=other),outputs=list(omop_output("wide",format="wide",rows="p",grain="episode"),omop_output("long",format="long",rows="p",sources=list(e),columns=c("x_value_baseline","y"))))
  result<-recipe_execute(r,out=c(wide="Episodes",long="Broadcast"),symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Episodes")) {expect_equal(nrow(f),36L);expect_setequal(unique(f$x_value_baseline),c(5,9));expect_equal(f$x_value_baseline,f$y)}
  for(f in .clf_frames(ctx,"Broadcast")) {expect_setequal(unique(f$x_value_baseline),c(5,9));expect_equal(f$x_value_baseline,f$y)}
  r$outputs<-list(panel=omop_output("panel",format="person_period",rows="p",columns="x_value_baseline",options=list(sources=list(e),window=omop_window(0,30),bins=list(width=10))))
  expect_error(recipe_execute(r,out="NoImplicitPeriods",symbol="labels",conns=ctx$conns),"repeated_grain_incompatible")
})
