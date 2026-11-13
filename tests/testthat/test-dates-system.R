test_that("D43 native verbal-autopsy wide table is complete on DSLite", {
  f<-dates_va_fixture();conns<-f$conns
  on.exit({DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  visit<-omop_anchor(omop_event("visit_occurrence"),which="first")
  death<-omop_anchor(omop_event("death"),which="first")
  va<-omop_recipe(variables=list(
    age_at_death=omop_age_at("death"),age_at_first_visit=omop_age_at("first_visit"),
    education=omop_variable(event=omop_event("observation",concepts=900001L),reducer=omop_reduce("last_value",value="value_as_concept_id")),
    fever=omop_variable(event=omop_event("observation",concepts=900002L),reducer=omop_reduce("last_value",value="value_as_concept_id")),
    visit_date=omop_date(visit),visit_month=omop_date(visit,omop_date_spec(precision="month")),
    visit_quarter=omop_date(visit,omop_date_spec(precision="quarter")),visit_year=omop_date(visit,omop_date_spec(precision="year",representation="number")),
    visit_parts=omop_date_parts(visit,c("year","month")),death_date=omop_date(death),
    cause_of_death=omop_variable(event=omop_event("death"),reducer=omop_reduce("first_value",value="cause_concept_id"))),
    outputs=omop_output(name="people",format="wide",grain="person"),options=list(translate_concepts=TRUE))
  expect_identical(recipe_execute(va,out=c(people="D_va"),conns=conns),c(people="D_va"))
  # Test harness only: synthetic fixture rows never enter the analyst snippet.
  d<-DSLite::getDSLiteData(conns,"D_va")$site
  expect_equal(nrow(d),24L);expect_equal(length(unique(d$person_id)),24L)
  expect_true(all(grepl("^p2",d$person_id)))
  expect_identical(d$age_at_death,rep(c(43L,33L,NA_integer_,NA_integer_),6))
  expect_identical(d$age_at_first_visit,rep(c(39L,29L,NA_integer_,NA_integer_),6))
  expect_identical(d$visit_date,rep(as.Date(c("2020-06-15","2020-03-14","2020-07-20",NA)),6))
  expect_true(is.ordered(d$visit_month));expect_true(is.ordered(d$visit_quarter))
  expect_identical(d$visit_year,rep(c(2020L,2020L,2020L,NA_integer_),6))
  expect_identical(as.character(d$education),rep(c("University education","Secondary education",NA,NA),6))
  expect_identical(as.character(d$cause_of_death),rep(c("Myocardial infarction","Pneumonia","Myocardial infarction",NA),6))
})
