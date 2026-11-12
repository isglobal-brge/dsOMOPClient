# WS-9 verbs end to end (design-2.10.3 sections 2.11, 7.3): ds.omop.mutate()
# and ds.omop.filter(expr =) from the analyst's call through DSLite to the
# installed dsOMOP server over the SQLite test CDM, on two sites. The server
# frames are inspected only through DSLite's session store (test-only); the
# analyst sees symbols and refusals, never rows.

.v9e_fixtures <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(dir.exists, file.path(tp, "..", "..", "..", "dsOMOP", "tests", "fixtures"))
}

.v9e_setup <- function(env = parent.frame()) {
  fixtures <- .v9e_fixtures()
  testthat::skip_if(is.null(fixtures), "dsOMOP fixture builders not reachable (sibling source tree)")
  builders <- new.env()
  sys.source(file.path(fixtures, "create_test_db.R"), envir = builders)
  db <- tempfile("ws9-verbs-", fileext = ".sqlite")
  conn <- builders$create_test_omop_db(db, n_persons = 15)
  DBI::dbDisconnect(conn)
  withr::defer(unlink(db), envir = env)
  resource <- resourcer::newResource(name = "omop_v9", url = paste0("omop+dbi:sqlite://", db),
                                     format = "omop.dbi.db")
  server <- DSLite::newDSLiteServer(resources = list(omop_v9 = resource),
                                    config = DSLite::defaultDSConfiguration(include = "dsOMOP"))
  assign("v9e_dslite", server, envir = globalenv())
  withr::defer(rm("v9e_dslite", envir = globalenv()), envir = env)
  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "site1", url = "v9e_dslite", driver = "DSLiteDriver")
  builder$append(server = "site2", url = "v9e_dslite", driver = "DSLiteDriver")
  conns <- DSI::datashield.login(builder$build())
  withr::defer(try(DSI::datashield.logout(conns), silent = TRUE), envir = env)
  ds.omop.connect(resource = "omop_v9", symbol = "omop_v9", conns = conns)
  recipe_execute(omop_recipe(variables = list(omop_variable_age(name = "age"), omop_variable_sex()),
                             outputs = omop_output("w", type = "wide")),
                 out = c(w = "D_w"), symbol = "omop_v9", conns = conns)
  list(conns = conns, server = server)
}

# The server-side frame of `symbol` on every session (test-only inspection).
.v9e_frames <- function(server, symbol) {
  lapply(server$getSessionIds(), function(sid) {
    if (symbol %in% server$symbols(sid)) server$getSessionData(sid, symbol) else NULL
  })
}

test_that("ds.omop.mutate() and ds.omop.filter(expr) run through DSLite on the installed server", {
  skip_if_not_installed("DSLite")
  skip_if_not_installed("dsOMOP")
  skip_if_not_installed("resourcer")
  skip_if_not_installed("RSQLite")
  ctx <- .v9e_setup()
  conns <- ctx$conns
  before <- .v9e_frames(ctx$server, "D_w")
  expect_true(all(vapply(before, function(f) inherits(f, "omop.table"), logical(1))))
  out <- ds.omop.mutate("D_w", age10 = age * 10L, adult = ifelse(age >= 18, 1L, 0L),
                        band = cut(age, c(0, 18, 65, 150), right = FALSE),
                        newobj = "D_m", symbol = "omop_v9", conns = conns)
  expect_identical(out, "D_m")
  for (f in .v9e_frames(ctx$server, "D_m")) {
    expect_s3_class(f, "omop.table")
    expect_true(all(c("age10", "adult", "band") %in% names(f)))
    expect_equal(f$age10, f$age * 10L)
    expect_null(attr(f, "dsomop_dp_provenance"))
  }
  # the closed predicate form of the filter
  kept <- ds.omop.filter("D_m", expr = omop_where(adult == 1, !is.na(sex)), newobj = "D_f",
                         symbol = "omop_v9", conns = conns)
  expect_identical(kept, "D_f")
  for (f in .v9e_frames(ctx$server, "D_f")) {
    expect_true(all(f$adult == 1L))
  }
  # a filter isolating too few persons is refused with the constant message, nothing assigned
  err <- tryCatch(suppressWarnings(ds.omop.filter("D_m", expr = omop_where(age10 < 0),
                                                  newobj = "D_none", symbol = "omop_v9",
                                                  conns = conns)), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "insufficient individuals", fixed = TRUE)
  expect_false(any(vapply(DSI::datashield.symbols(conns), function(s) "D_none" %in% s, logical(1))))
  # in place: replaced on every site, the rollback copy removed
  expect_identical(ds.omop.mutate("D_m", age10 = age10 + 1L, newobj = "D_m", symbol = "omop_v9",
                                  conns = conns), "D_m")
  for (f in .v9e_frames(ctx$server, "D_m")) expect_equal(f$age10, f$age * 10L + 1L)
  syms <- unlist(DSI::datashield.symbols(conns))
  expect_false(any(grepl("^omop\\.rollback\\.", syms)))
  # an in-place refusal leaves the target unchanged everywhere
  err <- tryCatch(ds.omop.mutate("D_m", person_id = 1L, newobj = "D_m", symbol = "omop_v9",
                                 conns = conns), error = function(e) e)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "left unchanged", fixed = TRUE)
  for (f in .v9e_frames(ctx$server, "D_m")) expect_equal(f$age10, f$age * 10L + 1L)
  expect_false(any(grepl("^omop\\.rollback\\.", unlist(DSI::datashield.symbols(conns)))))
})
