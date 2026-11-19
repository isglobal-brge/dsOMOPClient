# Cause-of-death teaching recipe. Question IDs below are synthetic fixture IDs;
# replace them and the education answer keys with those in the study CDM.
# This is the recipe defined step by step in chapter 18.
# First visit means the earliest visit; latest answers are independent per
# question across available history. Missing anchors retain the person.
# All patient values stay in the assigned table on each server.
book_capstone_recipe <- function(education_question_id = 900001L,
                             fever_question_id = 900002L) {
  first_visit <- omop_anchor(omop_event("visit_occurrence"), which = "first")
  age_at_death <- omop_age_at("death")
  education <- omop_variable(event = omop_event("observation", education_question_id),
    reducer = omop_reduce("last_value", value = "value_as_concept_id"))
  fever <- omop_variable(event = omop_event("observation", fever_question_id),
    reducer = omop_reduce("last_value", value = "value_as_concept_id"))
  cause <- omop_variable(event = omop_event("death"),
    reducer = omop_reduce("first_value", value = "cause_concept_id"))
  answer_keys <- c("concept:900101", "concept:900102")
  omop_recipe(variables = list(
    age_at_death = age_at_death,
    age_band = omop_number_bins(age_at_death, breaks = c(0, 40, 121),
      labels = c("under 40", "40 and over")),
    age_at_first_visit = omop_age_at(first_visit),
    education = omop_category(education, omop_category_spec(levels = answer_keys,
      order = omop_category_order(c(answer_keys, "special:missing")), ordered = TRUE,
      missing = omop_category_missing(na = "level"))),
    fever = fever, cause = cause,
    visit_date = omop_date(first_visit),
    visit_month = omop_date(first_visit, omop_date_spec(precision = "month")),
    visit_quarter = omop_date(first_visit, omop_date_spec(precision = "quarter")),
    visit_year = omop_date(first_visit, omop_date_spec(precision = "year", representation = "number"))),
    outputs = omop_output("people", format = "wide", grain = "person"))
}

# With initialized conns, returns only the symbol mapping. Example:
# library(dsOMOPClient)
# source("tools/book/capstone.R")
# book_capstone_execute(conns)
book_capstone_execute <- function(conns, education_question_id = 900001L,
                              fever_question_id = 900002L) {
  recipe_execute(book_capstone_recipe(education_question_id, fever_question_id),
                 out = c(people = "D_va"), datasources = conns)
}
