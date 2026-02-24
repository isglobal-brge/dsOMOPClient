# Extracted from test-plan-dsl.R:125

# test -------------------------------------------------------------------------
plan <- ds.omop.plan()
plan <- ds.omop.plan.cohort(plan, cohort_definition_id = 1)
plan <- ds.omop.plan.baseline(plan,
    tables = list(person = c("person_id")), name = "demo")
