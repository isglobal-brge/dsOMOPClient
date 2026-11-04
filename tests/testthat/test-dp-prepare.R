# DP preparation (dsOMOP 2.9.1): a tools-only path that never touches loading.

test_that("DP preparation and table loading use separate server methods", {
  calls <- list()
  testthat::local_mocked_bindings(
    .plan_execute_impl = function(plan, out = NULL, symbol = "omop",
                                  conns = NULL, output_mode = "memory",
                                  server_method = "omopPlanExecuteDS",
                                  harmonize_factors = TRUE) {
      calls[[length(calls) + 1L]] <<- list(
        out = out, output_mode = output_mode, server_method = server_method,
        harmonize_factors = harmonize_factors
      )
      invisible(out)
    },
    .package = "dsOMOPClient"
  )
  recipe <- omop_recipe(
    variables = omop_variable_age(name = "age_at_2025", year = 2025),
    outputs = omop_output(name = "ages", type = "wide")
  )
  ds.omop.dp.prepare(recipe)
  ds.omop.plan.execute(recipe_to_plan(recipe), out = c(ages = "D_ages"))
  expect_identical(calls[[1L]]$server_method, "omopDpPlanExecuteDS")
  expect_false(calls[[1L]]$harmonize_factors)
  expect_identical(calls[[1L]]$output_mode, "memory")
  expect_identical(unname(calls[[1L]]$out), "D_ages")
  expect_identical(calls[[2L]]$server_method, "omopPlanExecuteDS")
  expect_true(calls[[2L]]$harmonize_factors)
  expect_error(ds.omop.dp.prepare(list()), "omop_recipe or omop_plan")
})

test_that("a sticky release prepares, releases and removes its input", {
  recipe <- omop_recipe(
    variables = omop_variable_age(name = "age_at_2025", year = 2025),
    outputs = omop_output(name = "ages", type = "wide")
  )
  redesign <- omop_querylibrary_sticky(
    "PE12", variable = "age_at_2025", breaks = seq(0, 120, by = 10),
    preparation = recipe
  )
  prepared <- NULL
  released <- NULL
  removed <- NULL
  testthat::local_mocked_bindings(
    .dp_datasources = function(datasources) datasources,
    .querylibrary_sticky_server_preflight = function(redesign, datasources) {
      invisible(TRUE)
    },
    ds.omop.dp.prepare = function(x, out = NULL, symbol = "omop",
                                  conns = NULL) {
      prepared <<- list(x = x, out = out, symbol = symbol)
      invisible(out)
    },
    ds.omop.dp.release = function(x, privacy, datasources, pool, format) {
      released <<- list(x = x, privacy = privacy)
      structure(list(ok = TRUE), class = "dsomop_result")
    },
    .plan_remove_output_symbols = function(conns, owned, verify = FALSE) {
      removed <<- unlist(owned, use.names = FALSE)
      invisible(NULL)
    },
    .package = "dsOMOPClient"
  )
  result <- ds.omop.querylibrary.sticky.release(
    redesign, datasources = list(a = "A")
  )
  expect_s3_class(result, "dsomop_result")
  expect_identical(prepared$x, recipe)
  symbol <- unname(prepared$out)
  expect_match(symbol, "^dsOdp\\.")
  expect_identical(names(prepared$out), "ages")
  expect_identical(released$x, symbol)
  expect_identical(released$privacy, redesign$privacy)
  expect_true(symbol %in% removed)
  expect_true(paste0(symbol, ".events") %in% removed)
})

test_that("a sticky release without input explains both routes", {
  testthat::local_mocked_bindings(
    .dp_datasources = function(datasources) datasources,
    .querylibrary_sticky_server_preflight = function(redesign, datasources) {
      invisible(TRUE)
    },
    .package = "dsOMOPClient"
  )
  expect_error(
    ds.omop.querylibrary.sticky.release(
      omop_querylibrary_sticky("PE12", variable = "age_at_2025",
                               breaks = seq(0, 120, by = 10)),
      datasources = list(a = "A")),
    "dsomop:person.year_of_birth_distribution"
  )
  expect_error(
    ds.omop.querylibrary.sticky.release(omop_querylibrary_sticky("CO11"),
                                        datasources = list(a = "A")),
    "ds.omop.dp.prepare"
  )
})

test_that("sticky questions point to their automatic catalog answers", {
  catalog <- omop_querylibrary_sticky_catalog()
  expect_true("automatic_equivalent" %in% names(catalog))
  automatic <- stats::setNames(catalog$automatic_equivalent,
                               catalog$upstream_id)
  expect_identical(automatic[["PE12"]],
                   "dsomop:person.year_of_birth_distribution")
  expect_identical(automatic[["CO01"]], "dsomop:condition.duration_stats")
  expect_true(is.na(automatic[["CO11"]]))
  expect_true(all(names(.querylibrary_sticky_automatic()) %in%
                    catalog$upstream_id))
  printed <- capture.output(print(omop_querylibrary_sticky(
    "PE12", variable = "age_at_2025", breaks = seq(0, 120, by = 10))))
  expect_true(any(grepl("Automatic equivalent: ds.omop.analysis.run",
                        printed, fixed = TRUE)))
})
