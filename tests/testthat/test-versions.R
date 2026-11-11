# Versions, server readiness and readable server errors (2.10.1)

# One row per published method, in the DSI contract the client verifies.
.published_inventory <- function(server, names, version, package = "dsOMOP",
                                 type = "aggregate") {
  if (length(names) == 0L) {
    return(data.frame(server = character(0), name = character(0),
                      type = character(0), class = character(0),
                      value = character(0), package = character(0),
                      version = character(0), stringsAsFactors = FALSE))
  }
  data.frame(server = server, name = names, type = type, class = "function",
             value = paste0(package, "::", names), package = package,
             version = version, stringsAsFactors = FALSE)
}

.mock_server <- function(published_version, ping_version = published_version,
                         drop = character(0), legacy = FALSE) {
  aggregate <- setdiff(dsOMOPClient:::.DSOMOP_SERVER_METHODS$aggregate, drop)
  assign <- setdiff(dsOMOPClient:::.DSOMOP_SERVER_METHODS$assign, drop)
  if (legacy) {
    aggregate <- character(0)
    assign <- "getOMOPCDMTableDS"
  }
  testthat::local_mocked_bindings(
    datashield.methods = function(conns, type = "aggregate") {
      .published_inventory(names(conns), if (type == "aggregate") aggregate else assign,
                           published_version, type = type)
    },
    datashield.aggregate = function(conns, expr, error = NULL, ...) {
      server <- names(conns)
      if (!"omopPingDS" %in% c(aggregate)) {
        message <- paste0("[Client error: (400) Bad Request] No such DataSHIELD ",
                          "'AGGREGATE' method with name: omopPingDS")
        if (is.function(error)) error(server, message)
        stop("There are some DataSHIELD errors, list them with datashield.errors()",
             call. = FALSE)
      }
      stats::setNames(list(list(alive = TRUE, version = ping_version)), server)
    },
    .package = "DSI",
    .env = parent.frame()
  )
}

test_that("the client lists exactly the server methods its code calls", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources are not available")
  named <- unlist(lapply(list.files(r_dir, "\\.R$", full.names = TRUE), function(f) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    gsub("[\"']", "", pd$text[pd$token %in% c("STR_CONST", "SYMBOL", "SYMBOL_FUNCTION_CALL") &
                                grepl("^[\"']?omop[A-Za-z0-9]*DS[\"']?$", pd$text)])
  }))
  listed <- unlist(dsOMOPClient:::.DSOMOP_SERVER_METHODS, use.names = FALSE)
  expect_setequal(setdiff(unique(named), listed), character(0))
  expect_setequal(listed, unique(named))
  expect_length(intersect(dsOMOPClient:::.DSOMOP_SERVER_METHODS$aggregate,
                          dsOMOPClient:::.DSOMOP_SERVER_METHODS$assign), 0L)
})

test_that("ds.omop.client.version reports the installed client", {
  expect_identical(ds.omop.client.version(),
                   as.character(utils::packageVersion("dsOMOPClient")))
})

test_that("server errors are reported by their own cause", {
  ssl <- paste0(
    "Command 'omopInitDS(\"dsO.6AvjEF\", cdm_schema = NULL, vocab_schema = NULL, \n",
    "    results_schema = NULL, temp_schema = NULL)' failed on 'Ethiopia': Error ",
    "while evaluating 'is.null(base::assign('omop', value={dsOMOP::omopInitDS(",
    "\"dsO.6AvjEF\", cdm_schema = NULL)}))' -> Error : connection to server at ",
    "\"broadsea-atlasdb\" (172.20.0.3), port 5432 failed: server does not support ",
    "SSL, but SSL was required\n\n"
  )
  expect_identical(
    dsOMOPClient:::.ds_error_core(ssl),
    paste0("connection to server at \"broadsea-atlasdb\" (172.20.0.3), port ",
           "5432 failed: server does not support SSL, but SSL was required")
  )

  missing <- paste0("[Client error: (400) Bad Request] No such DataSHIELD ",
                    "'ASSIGN' method with name: omopInitDS")
  expect_identical(dsOMOPClient:::.ds_error_core(missing), missing)
  detail <- dsOMOPClient:::.ds_failure_detail(list(Ethiopia = missing))
  expect_match(detail, "^Ethiopia: \\[Client error")
  expect_match(detail, "does not publish dsOMOP's omopInitDS")
  expect_match(detail, "Packages > dsOMOP > Publish")
  expect_null(dsOMOPClient:::.ds_failure_hint("timeout"))
})

test_that("ds.omop.server.version says when a server is ready", {
  .mock_server("2.10.1")
  v <- ds.omop.server.version(list(site = "FAKE"))
  expect_identical(v$server, "site")
  expect_identical(v$published, "2.10.1")
  expect_identical(v$running, "2.10.1")
  expect_identical(v$missing, "")
  expect_true(v$ready)
  expect_identical(attr(v, "client"), ds.omop.client.version())
})

test_that("ds.omop.server.version explains a profile left on dsOMOP 1.x", {
  .mock_server("1.1.1", legacy = TRUE)
  v <- ds.omop.server.version(list(Ethiopia = "FAKE"))
  expect_false(v$ready)
  expect_true(is.na(v$running))
  expect_match(v$advice, "methods of dsOMOP 1.1.1")
  expect_match(v$advice, "Publish")
})

test_that("ds.omop.server.version explains an upgrade that was not published", {
  .mock_server("2.5.1", ping_version = "2.10.1")
  v <- ds.omop.server.version(list(site = "FAKE"))
  expect_false(v$ready)
  expect_match(v$advice, "dsOMOP 2.10.1 is installed but the profile publishes the methods of 2.5.1")
})

test_that("ds.omop.server.version lists methods the profile lacks", {
  .mock_server("2.10.1", drop = c("omopDpPlanExecuteDS", "omopFactorLevelsDS"))
  v <- ds.omop.server.version(list(site = "FAKE"))
  expect_false(v$ready)
  expect_match(v$missing, "omopFactorLevelsDS")
  expect_match(v$missing, "omopDpPlanExecuteDS")
  expect_match(v$advice, "lacks 2 dsOMOP method")
})

test_that("ds.omop.connect stops before assigning anything when dsOMOP is not published", {
  assigned <- FALSE
  testthat::local_mocked_bindings(
    datashield.methods = function(conns, type = "aggregate") {
      # The profile publishes dsBase and the methods of dsOMOP 1.1.1.
      if (type == "aggregate") {
        .published_inventory(names(conns), "meanDS", "6.3.5", package = "dsBase")
      } else {
        .published_inventory(names(conns), "getOMOPCDMTableDS", "1.1.1",
                             type = "assign")
      }
    },
    datashield.symbols = function(conns, ...) stats::setNames(list(character(0)), names(conns)),
    datashield.aggregate = function(conns, expr, error = NULL, ...) {
      message <- paste0("[Client error: (400) Bad Request] No such DataSHIELD ",
                        "'AGGREGATE' method with name: omopPingDS")
      if (is.function(error)) error(names(conns), message)
      stop("There are some DataSHIELD errors, list them with datashield.errors()",
           call. = FALSE)
    },
    datashield.assign.resource = function(...) {
      assigned <<- TRUE
      invisible(NULL)
    },
    .package = "DSI"
  )
  expect_error(
    ds.omop.connect(resource = "VA_data.hararghe_hdss", symbol = "not_ready",
                    conns = list(Ethiopia = "FAKE")),
    "dsOMOP is not ready on Ethiopia:\nEthiopia: \\[Client error: \\(400\\) Bad Request\\] No such DataSHIELD 'AGGREGATE' method with name: omopPingDS The server's DataSHIELD profile does not publish dsOMOP's omopPingDS"
  )
  expect_false(assigned)
})
