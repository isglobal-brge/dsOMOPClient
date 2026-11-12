# Amendment A-18 end to end, from the analyst's call: a real language-2.0
# adapter plan sent with ds.omop.plan.execute() through DSLite to the
# installed dsOMOP server, over the extended reach fixture loaded into
# PostgreSQL. The section 11 adapter refusals reach the analyst as an error
# naming the site, the output, the code and the closed reason, under both
# on_small_site settings (they are never skippable), and nothing is published.
# Needs the sibling dsOMOP source tree for the fixture builders, DSLite and a
# reachable PostgreSQL (DSOMOP_TEST_POSTGRES_HOST, or the WS-0 test host).

.ws4e_fixtures <- function() {
  tp <- tryCatch(testthat::test_path(), error = function(e) ".")
  Find(dir.exists, file.path(tp, "..", "..", "..", "dsOMOP", "tests", "fixtures"))
}

.ws4e_pg <- function() {
  if (!requireNamespace("RPostgres", quietly = TRUE)) return(NULL)
  host <- Sys.getenv("DSOMOP_TEST_POSTGRES_HOST")
  cfg <- if (nzchar(host)) {
    list(host = host, port = as.integer(Sys.getenv("DSOMOP_TEST_POSTGRES_PORT", "5432")),
         db = Sys.getenv("DSOMOP_TEST_POSTGRES_DATABASE", "dsomop"),
         user = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_USER", "dsomop_admin"),
         password = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_PASSWORD", "dsomop"))
  } else {
    list(host = Sys.getenv("DSOMOP_TEST_PG_HOST", "omopdb"),
         port = as.integer(Sys.getenv("DSOMOP_TEST_PG_PORT", "5432")),
         db = Sys.getenv("DSOMOP_TEST_PG_DB", "omop"),
         user = Sys.getenv("DSOMOP_TEST_PG_USER", "omop"),
         password = Sys.getenv("DSOMOP_TEST_PG_PASSWORD", "omop"))
  }
  conn <- tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = cfg$host, port = cfg$port,
                                  dbname = cfg$db, user = cfg$user, password = cfg$password,
                                  sslmode = "disable", connect_timeout = 5L),
                   error = function(e) NULL)
  if (is.null(conn)) return(NULL)
  cfg$conn <- conn
  cfg
}

.ws4e_meta <- list(language_version = "2.0", semantic_version = "2.0.0",
                   generated_by = "ws4-a18-e2e")

.ws4e_plans <- list(
  adapter_person_mismatch = list(
    defects = TRUE, root = "episodes_out",
    plan = list(
      version = 2L, meta = .ws4e_meta,
      events = list(
        episodes = list(table = "episode", concept_set = list(concepts = list(32531))),
        members = list(episode_members = list(episode = list(event = "episodes")))),
      populations = list(onc = list(kind = "index",
                                    index = list(source = list(event = "episodes"), select = "all",
                                                 end = list(strategy = "event_end")))),
      variables = list(n_members = list(source = list(event = "members"),
                                        reducer = list(fn = "count"))),
      outputs = list(episodes_out = list(rows = "onc", format = "wide",
                                         columns = list("n_members"))))),
  cost_currency_mixed = list(
    defects = FALSE, root = "costs",
    plan = list(
      version = 2L, meta = .ws4e_meta,
      events = list(visits = list(table = "visit_occurrence"),
                    visit_cost = list(cost = list(event = list(event = "visits"),
                                                  field = "total_cost"))),
      populations = list(all = list(kind = "criteria")),
      variables = list(cost = list(source = list(event = "visit_cost"),
                                   reducer = list(fn = "sum", value = "value"))),
      outputs = list(costs = list(rows = "all", format = "wide", columns = list("cost"))))))

test_that("A-18 end to end: adapter refusals reach the analyst with their reason, never published", {
  skip_if_not_installed("DSLite")
  skip_if_not_installed("dsOMOP")
  skip_if_not_installed("resourcer")
  skip_if_not_installed("RSQLite")
  fixtures <- .ws4e_fixtures()
  skip_if(is.null(fixtures), "dsOMOP fixture builders not reachable (sibling source tree)")
  pg <- .ws4e_pg()
  skip_if(is.null(pg), "PostgreSQL test host not reachable")
  on.exit(try(DBI::dbDisconnect(pg$conn), silent = TRUE), add = TRUE)
  env <- new.env()
  for (f in c("create_reach_db.R", "extend_reach_db.R", "load_reach_pg.R")) {
    sys.source(file.path(fixtures, f), envir = env)
  }
  for (reason in names(.ws4e_plans)) {
    case <- .ws4e_plans[[reason]]
    schema <- paste0("ws4e_", Sys.getpid(), "_", if (case$defects) "d" else "c")
    lite <- env$build_extended_reach_handle(100L, with_defects = case$defects)
    env$load_reach_fixture_pg(pg$conn, schema = schema, handle = lite)
    DBI::dbDisconnect(lite$conn)
    resource <- resourcer::newResource(
      name = "omop_ws4e",
      url = paste0("omop+dbi:postgresql://", pg$host, ":", pg$port, "/", pg$db,
                   "?cdm_schema=", schema, "&vocabulary_schema=", schema,
                   "&results_schema=", schema),
      identity = pg$user, secret = pg$password, format = "omop.dbi.db")
    assign("ws4e_dslite", DSLite::newDSLiteServer(
      resources = list(omop_ws4e = resource),
      config = DSLite::defaultDSConfiguration(include = "dsOMOP")), envir = globalenv())
    builder <- DSI::newDSLoginBuilder()
    builder$append(server = "site1", url = "ws4e_dslite", driver = "DSLiteDriver")
    conns <- DSI::datashield.login(builder$build())
    ds.omop.connect(resource = "omop_ws4e", symbol = "omop_ws4e", conns = conns)
    out <- stats::setNames("D_ws4e", case$root)
    for (on_small_site in c("error", "skip")) {
      err <- tryCatch(
        withr::with_options(list(nfilter.subset = 3L),
          ds.omop.plan.execute(case$plan, out = out, symbol = "omop_ws4e", conns = conns,
                               on_small_site = on_small_site)),
        error = function(e) e)
      info <- paste(reason, on_small_site)
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err),
                   paste0("site1:", case$root, " [validation_error] (", reason, ")"),
                   fixed = TRUE, info = info)
      # the server's raw message never reaches the analyst
      expect_false(grepl("belongs to another person|mix currencies|/events/",
                         conditionMessage(err)), info = info)
      symbols <- unlist(DSI::datashield.symbols(conns), use.names = FALSE)
      expect_false("D_ws4e" %in% symbols, info = info)
    }
    try(ds.omop.disconnect(symbol = "omop_ws4e", conns = conns), silent = TRUE)
    try(DSI::datashield.logout(conns), silent = TRUE)
    rm("ws4e_dslite", envir = globalenv())
    try(DBI::dbExecute(pg$conn, paste0("DROP SCHEMA ", schema, " CASCADE")), silent = TRUE)
  }
})
