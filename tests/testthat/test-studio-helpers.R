# Tests for new dsOMOPClient helpers: per-server resource mapping, scope-aware
# table listing, and Studio port resolution.

# --- .resolve_resource_map -------------------------------------------------

test_that("a single resource string is applied to every server", {
  m <- .resolve_resource_map("proj.omop", c("nairobi", "douala", "dakar"))
  expect_equal(names(m), c("nairobi", "douala", "dakar"))
  expect_true(all(unlist(m) == "proj.omop"))
})

test_that("a named list maps each server to its own resource", {
  m <- .resolve_resource_map(
    list(nairobi = "a.mimic", douala = "b.omop", dakar = "c.cdm"),
    c("nairobi", "douala", "dakar"))
  expect_equal(m$nairobi, "a.mimic")
  expect_equal(m$douala, "b.omop")
  expect_equal(m$dakar, "c.cdm")
})

test_that("a NAMED VECTOR maps by name, not position (regression)", {
  # Order intentionally differs from server order.
  m <- .resolve_resource_map(c(dakar = "c", nairobi = "a", douala = "b"),
                             c("nairobi", "douala", "dakar"))
  expect_equal(m$nairobi, "a")
  expect_equal(m$douala, "b")
  expect_equal(m$dakar, "c")
})

test_that("an unnamed vector matches positionally to the servers", {
  m <- .resolve_resource_map(c("a", "b"), c("nairobi", "douala"))
  expect_equal(m$nairobi, "a")
  expect_equal(m$douala, "b")
})

test_that("strict mode errors on unknown or missing server names", {
  expect_error(
    .resolve_resource_map(list(mars = "x", nairobi = "a"),
                          c("nairobi", "douala"), strict = TRUE),
    "not among connected servers")
  expect_error(
    .resolve_resource_map(list(nairobi = "a"),
                          c("nairobi", "douala"), strict = TRUE),
    "no resource specified")
  expect_error(
    .resolve_resource_map(c("a", "b", "c"), c("nairobi", "douala")),
    "entries but there are")
})

# --- .get_person_tables (scope-aware) --------------------------------------

mk <- function(...) {
  tn <- c(...)
  data.frame(table_name = tn, schema_category = rep("CDM", length(tn)),
             has_person_id = rep(TRUE, length(tn)), stringsAsFactors = FALSE)
}

test_that("pooled scope unions present tables across servers", {
  tb <- list(nairobi = mk("person", "measurement"),
             douala  = mk("person", "drug_exposure"))
  expect_equal(.get_person_tables(tb, "pooled", c("nairobi", "douala")),
               c("drug_exposure", "measurement", "person"))
})

test_that("per_site scope restricts to the focus server's tables", {
  tb <- list(nairobi = mk("person", "measurement"),
             douala  = mk("person", "drug_exposure"))
  expect_equal(.get_person_tables(tb, "per_site", "nairobi"),
               c("measurement", "person"))
})

test_that("tables absent from every server are hidden", {
  tb <- list(nairobi = mk("person"), douala = mk("person"))
  out <- .get_person_tables(tb, "pooled", c("nairobi", "douala"))
  expect_false("measurement" %in% out)
  expect_equal(out, "person")
})

test_that("NULL/empty input is handled", {
  expect_equal(.get_person_tables(NULL), character(0))
  expect_equal(.get_person_tables(list()), character(0))
})

# --- .studio_resolve_port --------------------------------------------------

test_that("an invalid port is rejected", {
  expect_error(.studio_resolve_port(0), "between 1 and 65535")
  expect_error(.studio_resolve_port(70000), "between 1 and 65535")
  expect_error(.studio_resolve_port("nope"), "between 1 and 65535")
})

test_that("a free port is returned as-is (or a valid fallback)", {
  free <- tryCatch({
    s <- base::serverSocket(0L)            # 0 => OS picks a free port
    p <- as.integer(sub(".*:", "", summary(s)$description))
    close(s); p
  }, error = function(e) NA_integer_)
  skip_if(is.na(free), "could not obtain a free port")
  out <- .studio_resolve_port(free)
  expect_true(is.null(out) || (is.numeric(out) && out >= 1 && out <= 65535))
})
