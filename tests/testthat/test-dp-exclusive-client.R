.exclusive_status <- function(enabled = TRUE, exclusive = TRUE) {
  status <- list(
    enabled = enabled, ready = enabled, sticky_noise = enabled,
    protocol = "dsomop-dp-release-v2",
    mechanism = "dsomop-sticky-discrete-laplace-prf-v2",
    snapshot_id = "snapshot-v1"
  )
  if (!is.null(exclusive)) status$exclusive <- exclusive
  status
}

test_that("DP status validates and prints exclusivity with legacy compatibility", {
  statuses <- list(
    exclusive = .exclusive_status(),
    opted_out = .exclusive_status(exclusive = FALSE),
    legacy = .exclusive_status(exclusive = NULL),
    disabled = .exclusive_status(enabled = FALSE)
  )
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) statuses[names(conns)],
    .package = "DSI"
  )
  value <- ds.omop.dp.status(as.list(stats::setNames(names(statuses),
                                                    names(statuses))))
  expect_s3_class(value, "omop_dp_status")
  expect_true(value$exclusive$exclusive)
  expect_false(value$opted_out$exclusive)
  expect_false(value$legacy$exclusive)
  expect_true(value$disabled$exclusive)
  output <- capture.output(returned <- print(value))
  expect_identical(returned, value)
  expect_match(paste(output, collapse = "\n"),
               "exclusive: enabled=TRUE ready=TRUE exclusive=TRUE", fixed = TRUE)
  expect_match(paste(output, collapse = "\n"),
               "legacy: enabled=TRUE ready=TRUE exclusive=FALSE", fixed = TRUE)
  for (invalid in list(NA, NULL, 1, "TRUE", logical(0), c(TRUE, FALSE))) {
    bad <- .exclusive_status()
    bad["exclusive"] <- list(invalid)
    expect_error(.dp_status_shape(bad, "bad"), "invalid DP status field 'exclusive'")
  }
})

test_that("memory execution never requests DP status and keeps factors", {
  conns <- list(a = "A", b = "B")
  session <- list(res_symbol = "resource", conns = conns)
  prepared <- NULL
  harmonized <- FALSE
  assigned <- character(0)
  testthat::local_mocked_bindings(
    .get_session = function(...) session,
    .prepare_plan_for_federation = function(plan, ...) {
      prepared <<- plan
      plan
    },
    .record_session_outputs = function(...) invisible(NULL),
    .session_harmonization_for_connections = function(...) NULL,
    .harmonizePublicationFactors = function(...) { harmonized <<- TRUE }
  )
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      expect_identical(as.character(expr[[1L]]), "omopPlanStatusDS")
      stats::setNames(rep(list(.publication_status(c(baseline = "D"))), length(conns)),
                      names(conns))
    },
    datashield.symbols = function(conns, ...) {
      stats::setNames(rep(list(assigned), length(conns)), names(conns))
    },
    datashield.assign.expr = function(conns, symbol, expr, success, ...) {
      assigned <<- union(assigned, c(symbol, "D"))
      for (server in names(conns)) success(server)
      invisible(NULL)
    },
    datashield.rm = function(conns, symbol, ...) {
      assigned <<- setdiff(assigned, symbol)
      invisible(NULL)
    },
    .package = "DSI"
  )
  plan <- ds.omop.plan.baseline(ds.omop.plan())
  expect_message(out <- ds.omop.plan.execute(plan, out = "D"), NA)
  expect_identical(unname(out), "D")
  expect_true(prepared$options$factor_concepts)
  expect_true(harmonized)
})

test_that("shared aggregates keep successful sites and the original errors", {
  conns <- list(a = "A", b = "B")
  for (message in c("DP-exclusive mode blocks standard statistical releases.",
                    "table not present")) {
    for (throw_generic in c(TRUE, FALSE)) {
      testthat::local_mocked_bindings(
        datashield.aggregate = function(conns, expr, error, ...) {
          if (identical(names(conns), "b")) {
            error("b", message)
            if (throw_generic) stop("There are some DataSHIELD errors")
            return(list(b = NULL))
          }
          list(a = data.frame(statistic = "rows", value = 100L))
        },
        .package = "DSI"
      )
      result <- .ds_safe_aggregate(conns, quote(omopTableStatsDS("omop")))
      expect_identical(names(result), "a")
      expect_identical(attr(result, "ds_errors")$b, message)
    }
  }
})

test_that("disabled community tools are reported with their reason", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error, ...) {
      error("a", paste("The OHDSI community tools (Achilles, OHDSI results,",
                       "the analysis catalog and the query library) are",
                       "disabled on this server."))
      stop("There are some DataSHIELD errors")
    },
    .package = "DSI"
  )
  expect_error(
    .ds_safe_aggregate(list(a = "A"), quote(omopAchillesStatusDS("omop"))),
    "community tools .* are disabled on: a.*dsomop.tools.enabled"
  )
})

test_that("factor discovery failures keep the server's reason", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error, ...) {
      error("a", "level discovery refused by the server")
      stop("There are some DataSHIELD errors")
    },
    datashield.assign.expr = function(...) stop("must not recode"),
    .package = "DSI"
  )
  expect_error(
    .harmonizeOneSymbol("D", list(a = "A")),
    "level collection failed.*level discovery refused by the server"
  )
})
