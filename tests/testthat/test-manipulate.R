# ==============================================================================
# Tests: client wrappers for the disclosure-gated manipulation verbs
# (R/manipulate.R: ds.omop.merge / ds.omop.filter / ds.omop.select /
#  ds.omop.bind_rows)
#
# These run with no live DataSHIELD backend: a fake session is registered and
# DSI::datashield.assign.expr is mocked so the generated call expression can be
# captured and asserted. We verify each wrapper (a) validates its arguments
# client-side and (b) builds the right omop*DS() assign expression, with string
# operators / values routed through the B64 transport (so the DataSHIELD lexer
# never sees a bare "<>=").
# ==============================================================================

# Decode a B64: transport token exactly as the server's .ds_arg does.
.ds_arg_decode <- function(x) {
  if (!is.character(x) || length(x) != 1L || !startsWith(x, "B64:")) return(x)
  b64 <- substring(x, 5)
  b64 <- gsub("-", "+", b64)
  b64 <- gsub("_", "/", b64)
  pad <- (4 - nchar(b64) %% 4) %% 4
  if (pad > 0) b64 <- paste0(b64, strrep("=", pad))
  jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(b64)), simplifyVector = FALSE)
}

# Register a fake session so .get_session() resolves and conns is non-NULL.
.with_fake_session <- function(symbol = "omop", code) {
  assign(symbol, list(conns = list(srv = "FAKE")),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
    }
  )
}

# Run `expr_fn()` (a wrapper call) with assign.expr mocked; return the captured
# call expression passed to DSI::datashield.assign.expr.
.capture_assign <- function(expr_fn) {
  captured <- NULL
  testthat::local_mocked_bindings(
    datashield.assign.expr = function(conns, symbol, expr, ...) {
      captured <<- list(conns = conns, symbol = symbol, expr = expr)
      invisible(NULL)
    },
    .package = "DSI"
  )
  ret <- expr_fn()
  list(captured = captured, ret = ret)
}

# ------------------------------------------------------------------------------
# ds.omop.merge
# ------------------------------------------------------------------------------

test_that("ds.omop.merge builds omopMergeDS(x, y, by, type) and returns newobj", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.merge("cohort_a", "cohort_b", type = "left", newobj = "j1"))

  expect_equal(out$ret, "j1")
  expect_equal(out$captured$symbol, "j1")

  call_expr <- out$captured$expr
  expect_equal(as.character(call_expr[[1]]), "omopMergeDS")
  # x, y travel as bare symbols (as.name), not strings
  expect_equal(call_expr[[2]], as.name("cohort_a"))
  expect_equal(call_expr[[3]], as.name("cohort_b"))
  # by = "person_id" (scalar -> .ds_encode leaves it bare); type bare
  expect_equal(call_expr[[4]], "person_id")
  expect_equal(call_expr[[5]], "left")
})

test_that("ds.omop.merge validates type via match.arg", {
  .with_fake_session()
  expect_error(ds.omop.merge("a", "b", type = "outer"), "should be one of")
})

test_that("ds.omop.merge rejects non-scalar-character symbols", {
  .with_fake_session()
  expect_error(ds.omop.merge(c("a", "b"), "c"), "omop.table symbols")
  expect_error(ds.omop.merge("a", 42), "omop.table symbols")
})

test_that("ds.omop.merge generates a newobj when none supplied", {
  .with_fake_session()
  out <- .capture_assign(function() ds.omop.merge("a", "b"))
  expect_match(out$ret, "^omop\\.merge\\.")
  expect_equal(out$captured$symbol, out$ret)
})

# ------------------------------------------------------------------------------
# ds.omop.filter
# ------------------------------------------------------------------------------

test_that("ds.omop.filter B64-encodes the operator and a scalar string value", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.filter("feat", var = "sex", op = "==", value = "M", newobj = "f1"))

  expect_equal(out$ret, "f1")
  call_expr <- out$captured$expr
  expect_equal(as.character(call_expr[[1]]), "omopFilterDS")
  expect_equal(call_expr[[2]], as.name("feat"))
  # var is a bare validated identifier
  expect_equal(call_expr[[3]], "sex")
  # op and the scalar string value are B64-wrapped (lexer-safe) and decode back
  expect_true(startsWith(call_expr[[4]], "B64:"))
  expect_true(startsWith(call_expr[[5]], "B64:"))
  expect_equal(.ds_arg_decode(call_expr[[4]]), "==")
  expect_equal(.ds_arg_decode(call_expr[[5]]), "M")
})

test_that("ds.omop.filter passes a numeric scalar value bare", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.filter("feat", var = "age", op = ">=", value = 18, newobj = "f2"))

  call_expr <- out$captured$expr
  # operator still B64 (contains '>' '='); decodes to ">="
  expect_equal(.ds_arg_decode(call_expr[[4]]), ">=")
  # numeric scalar travels bare (no B64 wrapper)
  expect_equal(call_expr[[5]], 18)
})

test_that("ds.omop.filter B64-encodes a vector value for 'in'", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.filter("feat", var = "sex", op = "in", value = c("M", "F"),
                   newobj = "f3"))

  call_expr <- out$captured$expr
  expect_equal(.ds_arg_decode(call_expr[[4]]), "in")
  expect_true(startsWith(call_expr[[5]], "B64:"))
  expect_equal(unlist(.ds_arg_decode(call_expr[[5]])), c("M", "F"))
})

test_that("ds.omop.filter validates op via match.arg and checks arg shapes", {
  .with_fake_session()
  expect_error(ds.omop.filter("feat", var = "age", op = "~=", value = 1),
               "should be one of")
  expect_error(ds.omop.filter(c("a", "b"), var = "age", op = "==", value = 1),
               "omop.table symbol")
  expect_error(ds.omop.filter("feat", var = c("a", "b"), op = "==", value = 1),
               "single column name")
})

# ------------------------------------------------------------------------------
# ds.omop.select
# ------------------------------------------------------------------------------

test_that("ds.omop.select builds omopSelectDS(x, cols) with B64 cols", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.select("feat", cols = c("age", "sex"), newobj = "s1"))

  expect_equal(out$ret, "s1")
  call_expr <- out$captured$expr
  expect_equal(as.character(call_expr[[1]]), "omopSelectDS")
  expect_equal(call_expr[[2]], as.name("feat"))
  expect_true(startsWith(call_expr[[3]], "B64:"))
  expect_equal(unlist(.ds_arg_decode(call_expr[[3]])), c("age", "sex"))
})

test_that("ds.omop.select validates its arguments", {
  .with_fake_session()
  expect_error(ds.omop.select(42, cols = "age"), "omop.table symbol")
  expect_error(ds.omop.select("feat", cols = character(0)),
               "non-empty character vector")
})

# ------------------------------------------------------------------------------
# ds.omop.bind_rows
# ------------------------------------------------------------------------------

test_that("ds.omop.bind_rows builds omopBindRowsDS(x, y) with bare symbols", {
  .with_fake_session()
  out <- .capture_assign(function()
    ds.omop.bind_rows("wave1", "wave2", newobj = "b1"))

  expect_equal(out$ret, "b1")
  call_expr <- out$captured$expr
  expect_equal(as.character(call_expr[[1]]), "omopBindRowsDS")
  expect_equal(call_expr[[2]], as.name("wave1"))
  expect_equal(call_expr[[3]], as.name("wave2"))
  # no extra arguments beyond the two symbols
  expect_length(call_expr, 3L)
})

test_that("ds.omop.bind_rows rejects non-scalar-character symbols", {
  .with_fake_session()
  expect_error(ds.omop.bind_rows("a", c("b", "c")), "omop.table symbols")
})

# ------------------------------------------------------------------------------
# .ds_encode_scalar transport: round-trips operators the lexer would choke on
# ------------------------------------------------------------------------------

test_that(".ds_encode_scalar round-trips comparison operators (lexer-safe)", {
  for (op in c("==", "!=", ">", ">=", "<", "<=", "in", "not_in")) {
    enc <- dsOMOPClient:::.ds_encode_scalar(op)
    expect_true(startsWith(enc, "B64:"))
    # URL-safe base64: no raw +, /, or = that could confuse the R expr parser
    expect_false(grepl("[+/=]", substring(enc, 5)))
    expect_equal(.ds_arg_decode(enc), op)
  }
})
