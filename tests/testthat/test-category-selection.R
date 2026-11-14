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
