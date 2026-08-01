test_that("table filtering preserves per-server introspection errors", {
  session_name <- "dictionary_errors"
  assign(
    session_name,
    list(res_symbol = "dsO_res", conns = list(a = "A", b = "B")),
    envir = dsOMOPClient:::.dsomop_client_env
  )
  on.exit(rm(list = session_name,
             envir = dsOMOPClient:::.dsomop_client_env), add = TRUE)

  raw <- list(a = data.frame(
    table_name = "person", schema_category = "CDM",
    stringsAsFactors = FALSE
  ))
  attr(raw, "ds_errors") <- list(b = "catalog unavailable")
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = function(...) raw,
    .package = "dsOMOPClient"
  )

  result <- ds.omop.tables(
    schema_category = "CDM", symbol = session_name
  )
  expect_identical(attr(result, "ds_errors"),
                   list(b = "catalog unavailable"))
})

test_that("schema snapshots include only complete servers and retain errors", {
  session_name <- "snapshot_errors"
  assign(
    session_name,
    list(res_symbol = "dsO_res", conns = list(a = "A", b = "B")),
    envir = dsOMOPClient:::.dsomop_client_env
  )
  on.exit(rm(list = session_name,
             envir = dsOMOPClient:::.dsomop_client_env), add = TRUE)

  calls <- 0L
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        list(
          a = list(tables = "person", cdm_info = list(cdm_version = "5.4")),
          b = list(tables = "person", cdm_info = list(cdm_version = "5.4"))
        )
      } else {
        value <- list(a = data.frame(from_table = "person"))
        attr(value, "ds_errors") <- list(b = "graph unavailable")
        value
      }
    },
    .package = "dsOMOPClient"
  )

  result <- ds.omop.snapshot(symbol = session_name)
  expect_identical(names(result), "a")
  expect_match(attr(result, "ds_errors")$b,
               "relationships: graph unavailable", fixed = TRUE)
})
