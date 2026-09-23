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

test_that("memory execution adapts factor discovery to every server's DP status", {
  conns <- list(a = "A", b = "B")
  statuses <- list(a = .exclusive_status(exclusive = NULL),
                   b = .exclusive_status())
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
    .harmonizeConceptFactors = function(...) { harmonized <<- TRUE }
  )
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      expect_identical(as.character(expr[[1L]]), "omopDpStatusDS")
      statuses[names(conns)]
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
  plan$harmonization <- list(old_contract = TRUE)
  expect_message(out <- ds.omop.plan.execute(plan, out = "D"),
                 "Exclusive DP is active on b.*factor-level discovery is disabled")
  expect_identical(unname(out), "D")
  expect_false(prepared$options$factor_concepts)
  expect_null(prepared$harmonization)
  expect_false(harmonized)
  expect_true(plan$options$factor_concepts)
  expect_true(plan$harmonization$old_contract)

  for (status in list(.exclusive_status(exclusive = NULL),
                       .exclusive_status(exclusive = FALSE),
                       .exclusive_status(enabled = FALSE))) {
    statuses$b <- status
    assigned <- character(0)
    harmonized <- FALSE
    expect_message(ds.omop.plan.execute(plan, out = "D"), NA)
    expect_true(prepared$options$factor_concepts)
    expect_true(harmonized)
  }
})

test_that("factor preflight fails before execution when status is unavailable", {
  testthat::local_mocked_bindings(
    .get_session = function(...) list(res_symbol = "resource",
                                      conns = list(a = "A"))
  )
  testthat::local_mocked_bindings(
    datashield.aggregate = function(...) stop("offline"),
    datashield.assign.expr = function(...) stop("must not assign"),
    .package = "DSI"
  )
  expect_error(ds.omop.plan.execute(ds.omop.plan.baseline(ds.omop.plan())),
               "DP status preflight.*no partial result.*offline")
})

test_that("standard helpers surface an exclusive refusal instead of partial data", {
  testthat::local_mocked_bindings(
    .get_session = function(...) list(res_symbol = "resource",
                                      conns = list(a = "A", b = "B"))
  )
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, ...) {
      if (identical(names(conns), "b")) {
        stop("DP-exclusive mode blocks standard statistical releases. ",
             "Use the typed DP channel omopDpReleaseDS ",
             "(ds.omop.dp.release on the client).", call. = FALSE)
      }
      list(a = data.frame(statistic = "rows", value = 100L))
    },
    .package = "DSI"
  )
  for (type in c("split", "combine", "both")) {
    expect_error(ds.omop.table.stats("person", type = type,
                                     pooling_policy = "pooled_only_ok"),
                 "Exclusive DP.*b.*ds.omop.dp.release.*No partial result")
  }
  expect_error(ds.omop.plan.preview(ds.omop.plan.baseline(ds.omop.plan()),
                                    conns = list(b = "B")),
               "Exclusive DP.*ds.omop.dp.release")
})


test_that("DSI error callbacks preserve exclusive refusals through generic failures", {
  conns <- list(a = "A", b = "B")
  testthat::local_mocked_bindings(
    .get_session = function(...) list(res_symbol = "resource", conns = conns)
  )
  refusal <- paste("DP-exclusive mode blocks standard statistical releases.",
                    "Use the typed DP channel omopDpReleaseDS",
                    "(ds.omop.dp.release on the client).")
  for (throw_generic in c(TRUE, FALSE)) {
    testthat::local_mocked_bindings(
      datashield.aggregate = function(conns, expr, error, ...) {
        if (identical(names(conns), "b")) {
          error("b", refusal)
          if (throw_generic) stop("There are some DataSHIELD errors")
          return(list(b = NULL))
        }
        list(a = data.frame(statistic = "rows", value = 100L))
      },
      .package = "DSI"
    )
    expect_error(ds.omop.table.stats("person", type = "both",
                                     pooling_policy = "pooled_only_ok"),
                 "Exclusive DP.*b.*ds.omop.dp.release.*No partial result")
  }
})


test_that("factor discovery preserves a policy refusal arriving after preflight", {
  testthat::local_mocked_bindings(
    datashield.aggregate = function(conns, expr, error, ...) {
      error("a", paste("DP-exclusive mode blocks standard statistical releases.",
                         "Use ds.omop.dp.release on the client."))
      stop("There are some DataSHIELD errors")
    },
    datashield.assign.expr = function(...) stop("must not recode"),
    .package = "DSI"
  )
  expect_error(.harmonizeOneSymbol("D", list(a = "A")),
               "level collection failed.*DP-exclusive.*ds.omop.dp.release")
})
