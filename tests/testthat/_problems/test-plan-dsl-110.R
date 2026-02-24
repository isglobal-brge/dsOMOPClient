# Extracted from test-plan-dsl.R:110

# test -------------------------------------------------------------------------
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan,
    tables = list(person = c("person_id")), name = "demo")
