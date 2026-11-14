education <- omop_variable(
  event = omop_event("observation", concepts = 900001L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
cause <- omop_variable(
  event = omop_event("death"),
  reducer = omop_reduce("first_value", value = "cause_concept_id"))
people <- omop_recipe(variables = list(education = education, cause_of_death = cause),
  outputs = omop_output("people", format = "wide", grain = "person"))
