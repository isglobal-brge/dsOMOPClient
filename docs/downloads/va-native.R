# A-28 native colleague recipe. Question IDs below are synthetic fixture IDs;
# replace them with the corresponding questions in the study CDM.
# First visit means the earliest visit; latest answers are independent per
# question across available history. Missing anchors retain the person.
# All patient values stay in the assigned table on each server.
va_native_recipe <- function(education_question_id = 900001L,
                             fever_question_id = 900002L) {
  visit<-omop_anchor(omop_event("visit_occurrence"),which="first")
  death<-omop_anchor(omop_event("death"),which="first")
  omop_recipe(variables=list(
    age_at_death=omop_age_at("death"),age_at_first_visit=omop_age_at("first_visit"),
    education=omop_variable(event=omop_event("observation",concepts=education_question_id),reducer=omop_reduce("last_value",value="value_as_concept_id")),
    fever=omop_variable(event=omop_event("observation",concepts=fever_question_id),reducer=omop_reduce("last_value",value="value_as_concept_id")),
    visit_date=omop_date(visit),visit_month=omop_date(visit,omop_date_spec(precision="month")),
    visit_quarter=omop_date(visit,omop_date_spec(precision="quarter")),visit_year=omop_date(visit,omop_date_spec(precision="year",representation="number")),
    visit_parts=omop_date_parts(visit,c("year","month")),death_date=omop_date(death),
    cause_of_death=omop_variable(event=omop_event("death"),reducer=omop_reduce("first_value",value="cause_concept_id"))),
    outputs=omop_output(name="people",format="wide",grain="person"),options=list(translate_concepts=TRUE))
}

# With initialized conns, returns only the symbol mapping. Example:
# library(dsOMOPClient)
# source("tools/dates/va-native.R")
# va_native_execute(conns)
va_native_execute <- function(conns, education_question_id = 900001L,
                              fever_question_id = 900002L) {
  recipe_execute(va_native_recipe(education_question_id, fever_question_id),
                 out = c(people = "D_va"), conns = conns)
}
