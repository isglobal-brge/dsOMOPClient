# B2: nested criteria require the same capabilities as direct leaves. The
# protocol is present at both sites, so a missing operation is the refusal.
test_that("nested observed gaps match the server inventory and refuse before federation assignment", {
  make_recipe <- function(gap) omop_recipe(
    populations = omop_population("p", filters = list(omop_all(
      omop_observed(before = 30), omop_not(omop_any(
        omop_observed(before = 365, gap = gap), omop_observed(after = 1)))))),
    variables = list(sex = omop_sex()),
    outputs = omop_output("w", rows = "p", format = "wide"))
  for (gap in list(NULL, 0, 30)) {
    r <- make_recipe(gap)
    plan <- recipe_to_plan(r)
    ops <- dsOMOPClient:::.plan_v2_operations(plan)
    expect_identical("population.coverage.union" %in% ops, !is.null(gap))
    state <- .legacy_federation(new = c("one", "two"),
                                ops = setdiff(.V2_ALL_OPS, "population.coverage.union"))
    if (is.null(gap)) {
      recipe_execute(r)
      expect_identical(unname(state$assigns), c(1L, 1L))
    } else {
      expect_error(recipe_execute(r),
                   "one lacks population.coverage.union; two lacks population.coverage.union")
      expect_identical(unname(state$assigns), c(0L, 0L))
    }
    if (requireNamespace("dsOMOP", quietly = TRUE)) {
      server_ops <- dsOMOP:::.irOperations(dsOMOP:::.planNormalizeV2(dsOMOPClient:::.v2_plain(plan)))
      expect_identical(as.character(ops), sort(unique(server_ops)))
    }
  }
})
