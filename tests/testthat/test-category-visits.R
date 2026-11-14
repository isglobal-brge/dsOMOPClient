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
    visits$position<-NULL
    DBI::dbWriteTable(con,"visit_occurrence",visits,append=TRUE);DBI::dbWriteTable(con,"observation",observations,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);visits<-omop_event("visit_occurrence")
  bundle<-omop_repeated(e,list(value=omop_number_rows(e),present=omop_number_present(omop_number_rows(e))),visits=omop_visits(c(1L,2L,3L,5L),visits,select="closest"))
  r<-omop_recipe(variables=list(lab=bundle,nth=omop_variable(event=e,reducer=omop_reduce("nth_value",n=2L))))
  for(mode in c("memory","staged")) {
    symbol<-paste0("Visits_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_equal(nrow(f),18L);expect_true(all(f$lab_value_visit_1==10));expect_true(all(f$lab_value_visit_3==30))
      expect_true(all(is.na(f$lab_value_visit_2)));expect_true(all(is.na(f$lab_value_visit_5)))
      expect_true(all(f$lab_present_visit_2==0L));expect_true(all(f$lab_present_visit_5==0L));expect_true(all(f$nth==30))
    }
  }
})
