# Synthetic-only assertions, deliberately kept out of analyst chapters.
# Chapter environments are checked before teardown by verify_chapter().
verify_chapter <- function(input, env) {
  get_data <- function(symbol) DSLite::getDSLiteData(env$conns, symbol)$site
  if (grepl("book-01|book-03", input)) {
    stopifnot(nrow(ds.omop.tables(conns=env$conns)$site) > 0,
              nrow(ds.omop.columns("observation",conns=env$conns)$site) > 0)
  }
  if (grepl("book-05", input)) {
    d <- get_data("D_people")
    stopifnot(nrow(d)==24L, length(unique(d$person_id))==24L,
      identical(as.character(d$education),rep(c("University education","Secondary education",NA,NA),6)),
      identical(as.character(d$cause_of_death),rep(c("Myocardial infarction","Pneumonia","Myocardial infarction",NA),6)))
    a <- get_data("D_answers")
    stopifnot(identical(as.character(a$fever),rep(c("yes","no",NA,NA),6)))
  }
  if (grepl("book-13-visits", input)) {
    d <- get_data("D_offsets")
    stopifnot(identical(d$first_days,rep(c(166L,73L,201L,NA_integer_),6)),
              identical(d$last_days,rep(c(1369L,439L,201L,NA_integer_),6)))
    date <- get_data("D_calendar")$visit_date
    stopifnot(inherits(date,"Date"),
      identical(date,rep(as.Date(c("2020-06-15","2020-03-14","2020-07-20",NA)),6)))
    long <- get_data("D_visit_dates")
    stopifnot(nrow(long)==30L,"visit_start_date" %in% names(long), inherits(long$visit_start_date,"Date"),
              sum(as.character(long$concept_id)=="Inpatient Visit")==12,
              sum(as.character(long$concept_id)=="Outpatient Visit")==18)
    message("Long visit_start_date class: ",paste(class(long$visit_start_date),collapse=","))
    message("Long visit_start_date synthetic first five: ",paste(head(long$visit_start_date,5),collapse=", "))
    stopifnot(nrow(get_data("D_visit_study"))==18,
              all(get_data("D_visit_study")$first_days==0))
  }
  if (grepl("book-18-capstone", input)) {
    d <- get_data("D_va")
    stopifnot(nrow(d)==24L, length(unique(d$person_id))==24L,
      all(grepl("^p2",d$person_id)),
      identical(d$age_at_death,rep(c(43L,33L,NA_integer_,NA_integer_),6)),
      identical(d$age_at_first_visit,rep(c(39L,29L,NA_integer_,NA_integer_),6)),
      identical(as.character(d$visit_month),rep(c("2020-06","2020-03","2020-07",NA),6)),
      identical(as.character(d$visit_quarter),rep(c("2020-Q2","2020-Q1","2020-Q3",NA),6)),
      identical(d$visit_year,rep(c(2020L,2020L,2020L,NA_integer_),6)),
      identical(recipe_to_plan(env$va),recipe_to_plan(env$reference)),
      identical(d,get_data("D_va_reference")))
  }
  verify_book2(input, env, get_data)
  verify_book3(input, env, get_data)
  verify_longitudinal(input, env, get_data)
  verify_standalone_showcase(input, env, get_data)
  verify_book4(input, env)
  # No native recipe accidentally executes through the legacy path.
  for(name in ls(env)) {
    obj <- get(name,env)
    if(inherits(obj,"omop_recipe") && !name %in% c("template","harmonised","blocked")) {
      stopifnot(recipe_explain(obj)$selected=="v2")
    }
  }
  message("ASSERTIONS PASS ",basename(input))
}
