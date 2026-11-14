test_that("C32 C38 C42 repeated public bundles retain requested missing columns", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L)
  points<-omop_timepoints(list(baseline=omop_timepoint(),m6=omop_timepoint(6,"months")),anchor="2020-01-01")
  bundle<-omop_repeated(e,list(answer=omop_category_rows(e,"value_as_concept_id")),timepoints=points)
  expect_s3_class(bundle$answer_m6,"omop_variable")
  r<-omop_recipe(variables=list(education=bundle),labels=c(`45000123`="primary",`45000124`="secondary",`45000125`="tertiary"))
  p<-recipe_to_plan(r,version=2)
  expect_true("layout.repeated" %in% .plan_v2_operations(p))
  ds.omop.plan.execute(p,out=setNames("Repeated",names(p$outputs)),symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Repeated")) {
    expect_equal(nrow(f),18L)
    expect_true(all(c("education_answer_baseline","education_answer_m6") %in% names(f)))
    expect_false(anyNA(f$education_answer_baseline))
    expect_true(all(is.na(f$education_answer_m6)))
  }
  code<-recipe_to_code(r)
  expect_silent(parse(text=code))
})
test_that("C44 time grids reject ambiguity and keep declaration order", {
  e<-omop_event("measurement")
  expect_error(omop_timepoint(tolerance=c(1,2)),"containing zero")
  expect_error(omop_timepoints(list(x=omop_timepoint(),x=omop_timepoint())),"unique")
  expect_error(omop_repeated(e,list(x=omop_number_rows(e))),"exactly")
  b<-omop_repeated(e,list(x=omop_number_rows(e)),timepoints=omop_timepoints(list(m12=omop_timepoint(12,"months"),baseline=omop_timepoint())))
  expect_identical(names(b),c("x_m12","x_baseline"))
})

test_that("C44 flattened indicator and point names cannot overwrite siblings", {
  e<-omop_event("observation",45000120L)
  v<-omop_category_rows(e,"value_as_concept_id")
  flags<-omop_category_indicators(v,c(a="concept:45000123"))
  expect_error(omop_repeated(e,list(x=flags,x_a=v),timepoints=omop_timepoints(list(p=omop_timepoint()))),"collide")
  expect_error(omop_repeated(e,list(x_a=v,x=v),timepoints=omop_timepoints(list(b=omop_timepoint(),a_b=omop_timepoint()))),"collide")
})

test_that("C32 C33 C34 C35 public grids use original calendar anchors and inclusive tolerances", {
  ctx<-.clf_setup();.numeric_lexer()
  dates<-c("2024-08-31","2025-02-21","2025-02-28","2025-03-01","2025-03-07","2025-08-31")
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path);DBI::dbExecute(con,"DELETE FROM observation")
    rows<-expand.grid(position=seq_along(dates),person_id=1:18)
    rows$observation_id<-seq_len(nrow(rows));rows$observation_concept_id<-45000120L
    rows$observation_type_concept_id<-0L;rows$observation_date<-dates[rows$position]
    rows$value_as_number<-rows$position;rows$position<-NULL
    DBI::dbWriteTable(con,"observation",rows[nrow(rows):1,],append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  grid<-omop_timepoints(list(baseline=omop_timepoint(),m6=omop_timepoint(6,"months",c(-7,7)),m12=omop_timepoint(12,"months"),first=omop_timepoint(6,"months",c(-7,7),select="first"),last=omop_timepoint(6,"months",c(-7,7),select="last")),anchor="2024-08-31")
  leap<-omop_timepoints(list(year=omop_timepoint(1,"years"),months=omop_timepoint(12,"months")),anchor="2024-02-29")
  r<-omop_recipe(variables=list(calendar=omop_repeated(e,list(value=omop_number_rows(e)),timepoints=grid),leap=omop_repeated(e,list(value=omop_number_rows(e)),timepoints=leap)))
  recipe_execute(r,out="CalendarPoints",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"CalendarPoints")) {
    expected<-c(calendar_value_baseline=1,calendar_value_m6=3,calendar_value_m12=6,calendar_value_first=2,calendar_value_last=5,leap_value_year=4,leap_value_months=3)
    for(n in names(expected))expect_true(all(f[[n]]==expected[[n]]),info=n)
  }
})
