.precision_decode <- function(x) {
  encoded <- dsOMOPClient:::.ds_encode_lossless(x)
  payload <- chartr("-_", "+/", sub("^B64:", "", encoded))
  payload <- paste0(payload, strrep("=", (4L - nchar(payload) %% 4L) %% 4L))
  json <- rawToChar(jsonlite::base64_dec(payload))
  list(json = json, value = jsonlite::fromJSON(json, simplifyVector = FALSE))
}

test_that("lossless transport preserves adjacent large public numeric boundaries", {
  breaks <- 2^53 + c(0, 2, 4)
  exprs <- alist(band = cut(big, breaks, labels = c("lower", "upper"), right = FALSE),
                 exact = 9007199254740994, decimal = 0.12345678901234566,
                 flag = big < 9007199254740994)
  request <- dsOMOPClient:::.mutate_request(exprs, environment())
  decoded <- .precision_decode(request)$value
  expect_identical(unlist(decoded$assign[[1L]]$expr$cut$breaks), breaks)
  expect_length(unique(unlist(decoded$assign[[1L]]$expr$cut$breaks)), 3L)
  expect_identical(decoded$assign[[2L]]$expr$lit, 9007199254740994)
  expect_identical(decoded$assign[[3L]]$expr$lit, 0.12345678901234566)
  expect_identical(decoded$assign[[4L]]$expr$pred$value, 9007199254740994)
  expect_silent(dsOMOPClient:::.mutate_schema_check(
    request, dsOMOPClient:::.jsonSchemaPackaged("mutate-v1", package = "dsOMOPClient"), "request"))
})

test_that("lossless transport keeps objects arrays nulls and UTF-8 strings distinct", {
  value <- list(empty_object = structure(list(), names = character()),
                empty_array = list(), null = NULL, singleton = list(2^53 + 2),
                text = "dose \"quoted\" \\ \n \u03bcg", boolean = TRUE)
  decoded <- .precision_decode(value)
  expect_identical(decoded$value$empty_object, value$empty_object)
  expect_identical(decoded$value$empty_array, value$empty_array)
  expect_true("null" %in% names(decoded$value))
  expect_null(decoded$value$null)
  expect_identical(decoded$value$singleton, value$singleton)
  expect_identical(decoded$value$text, value$text)
  expect_identical(decoded$value$boolean, TRUE)
  expect_match(decoded$json, '"empty_object":{}', fixed = TRUE)
  expect_match(decoded$json, '"empty_array":[]', fixed = TRUE)
})

test_that("transport preserves plan semantics with large numeric predicates", {
  event <- omop_event("measurement", 3004410,
                      where = omop_where(value_as_number >= 9007199254740994), name = "lab")
  plan <- recipe_to_plan(omop_recipe(
    variables = list(value = omop_variable(event = event, reducer = omop_reduce("mean"))),
    outputs = omop_output("w", format = "wide")))
  decoded <- .precision_decode(plan)$value
  expect_identical(decoded$events$lab$where$value, 9007199254740994)
  expect_identical(dsOMOPClient:::.planNormalizedDigest(decoded)$sha256,
                   dsOMOPClient:::.planNormalizedDigest(plan)$sha256)
})
