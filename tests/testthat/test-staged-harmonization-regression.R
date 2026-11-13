# Every site publishes the current methods, so the execution preflight (review
# B1) selects the publication protocol; test-v2-protocol.R covers old sites.
.mock_current_methods()

test_that("staged execution invalidates harmonization before preparation", {
  captured <- NULL
  conns <- list(site_a = "A", site_b = "B")
  plan <- ds.omop.plan()
  plan$harmonization <- list(stale = TRUE)

  testthat::local_mocked_bindings(
    .get_session = function(...) {
      list(conns = conns, res_symbol = "dsO_res")
    },
    .prepare_plan_for_federation = function(plan, ...) {
      captured <<- plan
      stop("captured staged plan", call. = FALSE)
    },
    .package = "dsOMOPClient"
  )

  expect_error(
    ds.omop.plan.execute(plan, output_mode = "staged"),
    "captured staged plan"
  )
  expect_identical(captured$options$translate_concepts, FALSE)
  expect_null(captured$harmonization)
})
