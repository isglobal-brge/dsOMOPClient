test_that("C39 C40 C41 C42 visit ranking precedes the measurement join", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM observation");DBI::dbExecute(con,"DELETE FROM visit_occurrence")
    visits<-expand.grid(position=1:3,person_id=1:18)
    visits$visit_occurrence_id<-visits$person_id*10L+visits$position
    visits$visit_concept_id<-9201L;visits$visit_type_concept_id<-0L
    visits$visit_start_date<-as.character(as.Date("2020-01-01")+visits$position-1L)
    visits$visit_end_date<-visits$visit_start_date
    observations<-visits[visits$position!=2,c("person_id","visit_occurrence_id")]
    observations$observation_id<-seq_len(nrow(observations));observations$observation_concept_id<-45000120L
    observations$observation_date<-visits$visit_start_date[visits$position!=2]
    observations$observation_type_concept_id<-0L;observations$value_as_number<-visits$position[visits$position!=2]*10
    observations$value_as_concept_id<-NA_integer_
    later<-observations;later$observation_id<-later$observation_id+1000L
    later$observation_date<-as.character(as.Date(later$observation_date)+1)
    later$value_as_number<-later$value_as_number+1;later$value_as_concept_id<-45000124L
    visits$position<-NULL
    DBI::dbWriteTable(con,"visit_occurrence",visits,append=TRUE);DBI::dbWriteTable(con,"observation",rbind(observations,later),append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);visits<-omop_event("visit_occurrence")
  bundle<-omop_repeated(e,list(value=omop_number_rows(e),present=omop_number_present(omop_number_rows(e))),visits=omop_visits(c(1L,2L,3L,5L),visits,select="closest"))
  last<-omop_repeated(e,list(value=omop_number_rows(e)),visits=omop_visits(c(1L,2L,3L),visits,select="last"))
  skipped<-omop_repeated(e,list(value=omop_number_rows(e),answer=omop_category_rows(e,"value_as_concept_id")),visits=omop_visits(c(1L,2L,3L),visits,select="closest"),missing="skip_any")
  r<-omop_recipe(variables=list(lab=bundle,last=last,skipped=skipped,nth=omop_variable(event=e,reducer=omop_reduce("nth_value",n=2L))))
  for(mode in c("memory","staged")) {
    symbol<-paste0("Visits_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_equal(nrow(f),18L);expect_true(all(f$lab_value_visit_1==10));expect_true(all(f$lab_value_visit_3==30))
      expect_true(all(is.na(f$lab_value_visit_2)));expect_true(all(is.na(f$lab_value_visit_5)))
      expect_true(all(f$lab_present_visit_2==0L));expect_true(all(f$lab_present_visit_5==0L));expect_true(all(f$nth==11))
      expect_true(all(f$last_value_visit_1==11));expect_true(all(f$last_value_visit_3==31))
      expect_true(all(f$skipped_value_visit_1==11));expect_true(all(f$skipped_value_visit_3==31));expect_true(all(is.na(f$skipped_value_visit_2)))
    }
  }
})

test_that("C40 C41 visit keys, missing starts and conflicting duplicates have bounded semantics", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM visit_occurrence");DBI::dbExecute(con,"DELETE FROM observation")
    visits<-expand.grid(position=1:3,person_id=1:18)
    visits$visit_occurrence_id<-visits$person_id*100+c(2,10,20)[visits$position]
    visits$visit_concept_id<-9201L;visits$visit_type_concept_id<-0L
    visits$visit_start_date<-c("2020-01-01","2020-01-01",NA_character_)[visits$position]
    visits$visit_end_date<-visits$visit_start_date
    rows<-visits[,c("person_id","visit_occurrence_id")]
    rows$observation_id<-seq_len(nrow(rows));rows$observation_concept_id<-45000120L
    rows$observation_date<-"2020-01-01";rows$observation_type_concept_id<-0L
    rows$value_as_number<-c(2,10,20)[visits$position]
    unlinked<-rows[seq_len(18),];unlinked$observation_id<-unlinked$observation_id+1000L;unlinked$visit_occurrence_id<-NA;unlinked$value_as_number<-999
    visits$position<-NULL
    DBI::dbWriteTable(con,"visit_occurrence",visits,append=TRUE)
    DBI::dbWriteTable(con,"observation",rbind(unlinked,rows[nrow(rows):1,]),append=TRUE)
    DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);v<-omop_event("visit_occurrence")
  r<-omop_recipe(variables=list(x=omop_repeated(e,list(value=omop_number_rows(e)),visits=omop_visits(c(1L,2L,3L),v))))
  recipe_execute(r,out="VisitKeys",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"VisitKeys")) {
    expect_true(all(f$x_value_visit_1==2));expect_true(all(f$x_value_visit_2==10));expect_true(all(is.na(f$x_value_visit_3)))
  }
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    old<-DBI::dbReadTable(con,"visit_occurrence");extra<-old[1L,,drop=FALSE];extra$visit_start_date<-"2020-02-01"
    DBI::dbWriteTable(con,"visit_occurrence",rbind(old,extra),overwrite=TRUE);DBI::dbDisconnect(con)
  }
  expect_error(recipe_execute(r,out="DuplicateVisit",symbol="labels",conns=ctx$conns),"repeated_spec_invalid|duplicate|identity")
})
