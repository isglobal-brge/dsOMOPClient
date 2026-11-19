# dsOMOPClient 2.10.9: dsBaseClient conventions (datasources, type, pooling
# policy, newobj) and the pooled views they produce.

.cons_session <- function(symbol = "cons", conns = list(a = "A", b = "B")) {
  assign(symbol,
         list(conns = conns, res_symbol = "dsO.cons",
              server_names = names(conns)),
         envir = dsOMOPClient:::.dsomop_client_env)
  withr::defer_parent(
    if (exists(symbol, envir = dsOMOPClient:::.dsomop_client_env)) {
      rm(list = symbol, envir = dsOMOPClient:::.dsomop_client_env)
    })
  symbol
}

# Fake aggregate answers per server method; records the servers each call saw.
.cons_aggregate <- function(answers, seen = NULL) {
  function(conns, expr) {
    method <- as.character(expr[[1L]])
    if (!is.null(seen)) seen$calls <- c(seen$calls, list(list(
      method = method, servers = names(conns), expr = expr)))
    out <- answers[[method]][intersect(names(conns), names(answers[[method]]))]
    missing <- setdiff(names(conns), names(out))
    if (length(missing) > 0L) {
      attr(out, "ds_errors") <- stats::setNames(
        as.list(rep("no answer", length(missing))), missing)
    }
    out
  }
}

.cons_reset_notes <- function() {
  notes <- dsOMOPClient:::.dsomop_alias_notes
  rm(list = ls(notes, all.names = TRUE), envir = notes)
}

test_that("datasources selects the connections; conns stays an alias", {
  sym <- .cons_session()
  seen <- new.env()
  stats <- list(omopTableStatsDS = list(a = list(rows = 10, persons = 5),
                                        b = list(rows = 20, persons = 10)))
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(stats, seen),
    .package = "dsOMOPClient")
  .cons_reset_notes()

  ds.omop.table.stats("person", symbol = sym)
  expect_identical(seen$calls[[1L]]$servers, c("a", "b"))
  ds.omop.table.stats("person", symbol = sym, datasources = list(a = "A"))
  expect_identical(seen$calls[[2L]]$servers, "a")
  expect_message(
    ds.omop.table.stats("person", symbol = sym, conns = list(b = "B")),
    "'conns' is now called 'datasources'")
  expect_identical(seen$calls[[3L]]$servers, "b")
  # One message per session.
  expect_message(
    ds.omop.value.counts("person", "x", symbol = sym, conns = list(b = "B"),
                         execute = FALSE), NA)
  expect_error(
    ds.omop.table.stats("person", symbol = sym, conns = list(a = "A"),
                        datasources = list(b = "B")),
    "both given")
})

test_that("NULL connections resolve to the session, else to every connection", {
  sym <- .cons_session()
  expect_identical(dsOMOPClient:::.resolve_datasources(NULL, sym),
                   list(a = "A", b = "B"))
  testthat::local_mocked_bindings(
    datashield.connections_find = function(...) list(z = "Z"),
    .package = "DSI")
  expect_identical(
    dsOMOPClient:::.resolve_datasources(NULL, "no_such_session"),
    list(z = "Z"))
  expect_identical(
    dsOMOPClient:::.resolve_datasources(list(q = "Q"), sym), list(q = "Q"))
})

test_that("signatures carry their real type defaults", {
  both <- list(ds.omop.table.stats, ds.omop.domain.coverage,
               ds.omop.date.counts, ds.omop.value.counts, ds.omop.crosstab,
               ds.omop.concept.locate, ds.omop.achilles.results)
  for (f in both) expect_identical(formals(f)$type, "both")
  split <- list(ds.omop.column.stats, ds.omop.value.quantiles,
                ds.omop.achilles.distribution, ds.omop.value.histogram,
                ds.omop.missingness)
  for (f in split) expect_identical(formals(f)$type, "split")
  expect_identical(dsOMOPClient:::.normalize_result_type("pooled"), "both")
  dry <- ds.omop.crosstab("person", "a", "b", execute = FALSE)
  expect_identical(dry$meta$type, "both")
  expect_output(print(dry), "Type: both")
})

test_that("call code records the pooling policy and the view", {
  code <- ds.omop.value.counts("person", "gender_concept_id",
                               pooling_policy = "pooled_only_ok",
                               execute = FALSE)$meta$call_code
  expect_match(code, 'pooling_policy = "pooled_only_ok"', fixed = TRUE)
  expect_match(code, 'type = "both"', fixed = TRUE)
  expect_error(ds.omop.table.stats("person", pooling_policy = "loose"),
               "should be one of")
})

test_that("a site-specific output stops for combine and explains itself for both", {
  sym <- .cons_session()
  quantiles <- list(omopNumericQuantilesDS = list(
    a = data.frame(probability = 0.5, value = 1),
    b = data.frame(probability = 0.5, value = 2)))
  seen <- new.env()
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(quantiles, seen),
    .package = "dsOMOPClient")
  expect_error(ds.omop.value.quantiles("m", "v", symbol = sym,
                                       type = "combine"),
               "cannot be safely pooled")
  expect_null(seen$calls)
  both <- ds.omop.value.quantiles("m", "v", symbol = sym, type = "both")
  expect_named(both$per_site, c("a", "b"))
  expect_null(both$pooled)
  expect_match(both$meta$pooling_reason, "cannot be safely pooled")
})

test_that("missingness pools banded counts and names older servers", {
  sym <- .cons_session()
  new_frames <- list(
    a = data.frame(column_name = c("x", "y"), missing_rate = c(0.1, NA),
                   n_total = c(100, 100), n_missing = c(10, NA)),
    b = data.frame(column_name = c("x", "y"), missing_rate = c(0.25, 0.5),
                   n_total = c(200, 200), n_missing = c(50, 100)))
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(list(omopMissingnessDS = new_frames)),
    .package = "dsOMOPClient")
  res <- ds.omop.missingness("measurement", symbol = sym, type = "both")
  x <- res$pooled[res$pooled$column_name == "x", ]
  expect_equal(x$n_total, 300)
  expect_equal(x$n_missing, 60)
  expect_equal(x$missing_rate, 0.2)
  # A suppressed count stays unknown in the pool.
  expect_true(is.na(res$pooled$missing_rate[res$pooled$column_name == "y"]))
  expect_identical(res$meta$pooling$method, "ratio of summed banded counts")

  old <- lapply(new_frames, function(df) df[, c("column_name", "missing_rate")])
  out <- dsOMOPClient:::.pool_result(old, "missingness", "strict")
  expect_null(out$result)
  expect_match(out$warnings, "dsOMOP 2.10.8 or older")
  mixed <- list(a = old$a, b = new_frames$b)
  partial <- dsOMOPClient:::.pool_result(mixed, "missingness", "pooled_only_ok")
  expect_equal(partial$result$n_total[partial$result$column_name == "x"], 200)
  expect_true(any(grepl("server(s) a", partial$warnings, fixed = TRUE)))
})

test_that("pooled value counts keep every count column", {
  per_site <- list(
    a = data.frame(value = c("1", "2"), n = c(10, 20), n_persons = c(5, 10),
                   concept_name = c("A", "B")),
    b = data.frame(value = c("2", "3"), n = c(30, 5), n_persons = c(15, 5),
                   concept_name = c("B", "C")))
  out <- dsOMOPClient:::.pool_result(per_site, "value_counts", "strict")$result
  expect_identical(out$value, "2")
  expect_equal(out$n, 50)
  expect_equal(out$n_persons, 25)
  expect_identical(out$concept_name, "B")
})

test_that("pooled prevalence follows metric and top_n and sums both counts", {
  frame <- data.frame(concept_id = 1:3, concept_name = c("x", "y", "z"),
                      n_persons = c(50, 40, 30), n_records = c(10, 100, 60))
  per_site <- list(a = frame, b = frame)
  by_records <- dsOMOPClient:::.pool_result(
    per_site, "concept_prevalence", "strict",
    options = list(metric = "records", top_n = 2))$result
  expect_identical(by_records$concept_id, c(2L, 3L))
  expect_equal(by_records$n_records, c(200, 120))
  expect_equal(by_records$n_persons, c(80, 60))
  by_persons <- dsOMOPClient:::.pool_result(
    per_site, "concept_prevalence", "strict",
    options = list(metric = "persons", top_n = 1))$result
  expect_identical(by_persons$concept_id, 1L)
  expect_equal(by_persons$n_records, 20)
})

test_that("pooled table statistics keep date_range and requested stats", {
  per_site <- list(
    a = list(rows = 100, persons = 50,
             date_range = list(column = "d", min_month = "2010-01",
                               max_month = "2015-02")),
    b = list(rows = 200, persons = 80,
             date_range = list(column = "d", min_month = "2009-05",
                               max_month = "2014-01")))
  out <- dsOMOPClient:::.pool_result(per_site, "table_stats", "strict")$result
  expect_equal(out$rows, 300)
  expect_identical(out$date_range$min_month, "2009-05")
  expect_identical(out$date_range$max_month, "2015-02")
  persons_only <- dsOMOPClient:::.pool_result(
    list(a = list(persons = 5), b = list(persons = 7)), "table_stats",
    "strict")$result
  expect_identical(names(persons_only), "persons")
  expect_equal(persons_only$persons, 12)
})

test_that("pooled drilldowns keep the date range and say what is not pooled", {
  site <- function(n, months, rates) list(
    summary = list(concept_id = 1L, concept_name = "C", n_records = n,
                   n_persons = n / 2, records_per_person_mean = 2,
                   pct_persons_multi = 10),
    numeric_summary = NULL, categorical_values = NULL,
    date_range = list(column = "d", min_month_safe = months[[1L]],
                      max_month_safe = months[[2L]],
                      date_counts = data.frame(period = c("2010", "2011"),
                                               n_records = c(n, n),
                                               n_persons = c(n, n))),
    missingness = data.frame(column_name = "v", missing_rate = rates))
  per_site <- list(a = site(20, c("2010-01", "2011-06"), 0.1),
                   b = site(40, c("2010-03", "2011-12"), 0.2))
  out <- dsOMOPClient:::.pool_result(per_site, "concept_drilldown", "strict")
  pooled <- out$result
  expect_equal(pooled$summary$n_records, 60)
  expect_equal(pooled$summary$records_per_person_mean, 2)
  expect_true(is.na(pooled$summary$pct_persons_multi))
  expect_identical(pooled$date_range$min_month_safe, "2010-01")
  expect_identical(pooled$date_range$max_month_safe, "2011-12")
  expect_equal(pooled$date_range$date_counts$n_records, c(60, 60))
  expect_true(is.na(pooled$missingness$missing_rate))
  expect_true(any(grepl("without the counts", out$warnings)))
})

test_that("pooled cross-tabs keep their axis metadata", {
  ct <- function(m) list(row_col = "r", col_col = "c", count_mode = "persons",
                         counts = m)
  m <- matrix(c(10, 20, 30, 40), 2, dimnames = list(c("F", "M"), c("x", "y")))
  out <- dsOMOPClient:::.pool_result(list(a = ct(m), b = ct(m)), "crosstab",
                                     "strict")$result
  expect_identical(out$row_col, "r")
  expect_identical(out$count_mode, "persons")
  expect_identical(out$row_levels, c("F", "M"))
  expect_equal(out$counts[["M", "y"]], 80)
})

test_that("recipe_preview_stats returns server frames with a table column", {
  sym <- .cons_session()
  stats <- list(omopTableStatsDS = list(a = list(rows = 10, persons = 5),
                                        b = list(rows = 20, persons = 10)))
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(stats),
    .package = "dsOMOPClient")
  recipe <- omop_recipe()
  recipe <- recipe_add_variable(recipe, name = "v", table = "person")
  recipe <- recipe_add_variable(recipe, name = "w", table = "measurement")
  res <- recipe_preview_stats(recipe, symbol = sym)
  expect_named(res$per_site, c("a", "b"))
  expect_identical(res$per_site$a$table, c("person", "measurement"))
  expect_equal(res$per_site$b$rows, c(20, 20))
  expect_equal(res$pooled$persons, c(15, 15))
  expect_s3_class(res$meta$by_table$person, "dsomop_result")
  .cons_reset_notes()
  expect_message(split <- recipe_preview_stats(recipe, scope = "per_site",
                                               symbol = sym),
                 "'scope' is now called 'type'")
  expect_null(split$pooled)
  expect_identical(split$meta$type, "split")
})

test_that("comorbidity refuses two tables and uses the table's concept column", {
  expect_error(ds.omop.comorbidity(1, 2, tableA = "condition_occurrence",
                                   tableB = "drug_exposure"),
               "tableB must equal")
  res <- ds.omop.comorbidity(1, 2, tableA = "drug_exposure", execute = FALSE)
  expect_match(res$meta$call_code, 'row = "drug_concept_id"', fixed = TRUE)
  expect_error(ds.omop.comorbidity(1, 2, tableA = "person"),
               "domain concept column")
})

test_that("date counts accept a cohort", {
  sym <- .cons_session()
  seen <- new.env()
  counts <- list(omopDateCountsDS = list(
    a = data.frame(period = "2010", n_records = 10, n_persons = 5),
    b = data.frame(period = "2010", n_records = 20, n_persons = 10)))
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(counts, seen),
    .package = "dsOMOPClient")
  res <- ds.omop.date.counts("condition_occurrence", symbol = sym,
                             cohort = 7L)
  expect_identical(seen$calls[[1L]]$expr[[6L]], 7L)
  expect_equal(res$pooled$n_records, 30)
})

test_that("status keeps one ping entry per server", {
  sym <- .cons_session()
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = .cons_aggregate(list(
      omopPingDS = list(a = list(version = "2.10.9")))),
    .package = "dsOMOPClient")
  status <- ds.omop.status(sym)
  expect_named(status$ping, c("a", "b"))
  expect_identical(status$ping$a$version, "2.10.9")
  expect_identical(status$ping$b$error, "no answer")
})

test_that("renamed arguments keep their old names", {
  .cons_reset_notes()
  sym <- .cons_session()
  sent <- NULL
  testthat::local_mocked_bindings(
    .conceptAssertSupport = function(...) invisible(TRUE),
    .assign_expr_atomic = function(conns, newobj, expr, ...) {
      sent <<- expr
      invisible(newobj)
    },
    .package = "dsOMOPClient")
  expect_message(ds.omop.merge("x", "y", type = "left", newobj = "m",
                               symbol = sym),
                 "'type' is now called 'join'")
  expect_identical(sent[[5L]], "left")
  ds.omop.merge("x", "y", join = "left", newobj = "m", symbol = sym)
  expect_identical(sent[[5L]], "left")
  expect_error(ds.omop.merge("x", "y", type = "inner", join = "left",
                             symbol = sym), "both given")

  expect_identical(dsOMOPClient:::.cohort_id_from_newobj("dsomop_cohort_12"),
                   12L)
  expect_identical(dsOMOPClient:::.cohort_id_from_newobj(3), 3L)
  expect_error(dsOMOPClient:::.cohort_id_from_newobj("my_cohort"),
               "dsomop_cohort_<id>")
})

test_that("as.data.frame of both views never falls back to one server", {
  r <- dsomop_result(per_site = list(a = data.frame(x = 1)), pooled = NULL,
                     meta = list(type = "both"))
  expect_equal(nrow(as.data.frame(r)), 0L)
  r2 <- dsomop_result(per_site = list(a = data.frame(x = 1),
                                      b = data.frame(x = 2)),
                      meta = list(type = "split"))
  expect_identical(as.data.frame(r2)$server, c("a", "b"))
  expect_identical(r2[["a"]], data.frame(x = 1))
})

test_that("OHDSI results keep the per-server frames when they cannot be combined", {
  sym <- .cons_session(conns = list(site_a = "A", site_b = "B"))
  contract <- function(tool) list(
    contract_version = 1L, tool_id = tool, table_name = "cohort_count",
    pooling_contract = list(version = 1L, strategy = "tabular",
                            columns = list(key = list(role = "key"),
                                           n = list(role = "sum"))))
  frames <- list(site_a = data.frame(key = "x", n = 10),
                 site_b = data.frame(key = "x", n = 15))
  testthat::local_mocked_bindings(
    .ds_safe_aggregate = function(conns, expr) {
      if (identical(as.character(expr[[1L]]), "omopOhdsiResultContractDS")) {
        return(list(site_a = contract("cohort_diagnostics"),
                    site_b = contract("cohort_incidence")))
      }
      frames[names(conns)]
    },
    .package = "dsOMOPClient")
  expect_error(ds.omop.ohdsi.results("cohort_count", symbol = sym,
                                     type = "combine"),
               "differs across servers")
  both <- ds.omop.ohdsi.results("cohort_count", symbol = sym, type = "both")
  expect_named(both$per_site, c("site_a", "site_b"))
  expect_null(both$pooled)
  expect_identical(both$meta$type, "both")
  expect_match(both$meta$pooling_reason, "differs across servers")
})
