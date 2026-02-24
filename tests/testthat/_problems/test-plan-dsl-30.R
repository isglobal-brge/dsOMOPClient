# Extracted from test-plan-dsl.R:30

# test -------------------------------------------------------------------------
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan,
    tables = list(person = c("person_id", "gender_concept_id")),
    name = "demo"
  )
