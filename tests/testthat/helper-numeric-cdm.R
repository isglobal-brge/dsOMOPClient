.numeric_definitions <- function(env = parent.frame()) {
  path <- testthat::test_path("..", "..", "..", "dsOMOP", "tests", "fixtures", "numeric", "matrix.R")
  if (!file.exists(path)) stop("Numeric tests need the matching dsOMOP sibling source")
  sys.source(path, env)
}
.numeric_sites <- function(n = 3L, env = parent.frame()) {
  if (!nzchar(Sys.getenv("DSOMOP_TEST_NUMERIC_CDM"))) testthat::skip("opt-in numeric CDM matrix")
  for (pkg in c("dsOMOP", "DSLite", "dsBase", "dsBaseClient", "RPostgres", "arrow")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Numeric matrix requires ", pkg)
  }
  base <- withr::local_tempdir(.local_envir = env); Sys.chmod(base, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = base, DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_PROVIDER = "file", DSOMOP_PSEUDONYM_ROOT = "",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_KEY = ""), .local_envir = env)
  withr::local_options(list(dsstaging.base_dir = base), .local_envir = env)
  pg <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("DSOMOP_TEST_POSTGRES_HOST", "omopdb"),
    dbname = Sys.getenv("PGDATABASE", "omop"), user = Sys.getenv("PGUSER", "omop"),
    password = Sys.getenv("PGPASSWORD", "omop"), sslmode = "disable")
  withr::defer(DBI::dbDisconnect(pg), envir = env)
  tables <- DBI::dbGetQuery(pg, "SELECT table_name FROM information_schema.tables WHERE table_schema='numeric_cdm'")$table_name
  servers <- list(); login <- DSI::newDSLoginBuilder(); schemas <- character()
  for (i in seq_len(n)) {
    schema <- paste0("numeric_site_", Sys.getpid(), "_", i)
    DBI::dbExecute(pg, paste0("CREATE SCHEMA ", schema)); schemas <- c(schemas, schema)
    local({ s <- schema; withr::defer(DBI::dbExecute(pg, paste0("DROP SCHEMA ", s, " CASCADE")), envir = env) })
    for (table in tables) {
      cols <- DBI::dbListFields(pg, DBI::Id(schema = "numeric_cdm", table = table))
      restriction <- if ("person_id" %in% cols) paste0(" WHERE MOD(person_id, ", n, ") = ", i - 1L) else ""
      DBI::dbExecute(pg, paste0("CREATE VIEW ", schema, ".", table, " AS SELECT * FROM numeric_cdm.", table, restriction))
    }
    resource <- resourcer::newResource(name = "numeric", format = "omop.dbi.db",
      url = paste0("omop+dbi:postgresql://", Sys.getenv("DSOMOP_TEST_POSTGRES_HOST", "omopdb"),
        ":5432/", Sys.getenv("PGDATABASE", "omop"), "?cdm_schema=", schema, "&sslmode=disable"),
      identity = Sys.getenv("PGUSER", "omop"), secret = Sys.getenv("PGPASSWORD", "omop"))
    config <- DSLite::defaultDSConfiguration(include = c("dsOMOP", "dsBase"))
    config$Options[["nfilter.subset"]] <- "5"; config$Options[["nfilter.tab"]] <- "3"
    server <- DSLite::newDSLiteServer(resources = list(numeric = resource), config = config)
    name <- paste0("numeric_server_", i)
    assign(name, server, globalenv())
    local({ nm <- name; withr::defer(rm(list = nm, envir = globalenv()), envir = env) })
    servers[[paste0("site", i)]] <- server
    login$append(server = paste0("site", i), url = name, driver = "DSLiteDriver")
  }
  conns <- DSI::datashield.login(login$build())
  withr::defer(DSI::datashield.logout(conns), envir = env)
  ds.omop.connect(resource = "numeric", symbol = "numeric", conns = conns)
  withr::defer(ds.omop.disconnect("numeric", conns = conns), envir = env)
  list(conns = conns, servers = servers, pg = pg, schemas = schemas)
}
.numeric_frames <- function(ctx, symbol, mode = "memory") lapply(ctx$servers, function(server) {
  x <- server$getSessionData(server$getSessionIds()[[1L]], symbol)
  testthat::expect_s3_class(x, if (mode == "staged") "FlowerDatasetDescriptor" else "omop.table")
  if (inherits(x, "FlowerDatasetDescriptor")) {
    testthat::expect_identical(x$metadata$format, "parquet")
    chunks <- list(); dsOMOP:::.publishReadChunks(x, function(c) chunks[[length(chunks) + 1L]] <<- c)
    x <- do.call(rbind, chunks)
  }
  x
})
.numeric_lexer <- function(env = parent.frame()) {
  # DSLite 1.5 quoted-string lexer workaround, as in concept-label-analysis.
  lexer <- get("Lexer", asNamespace("DSLite"))
  d <- lexer$public_fields$t_DBLSTRING; s <- lexer$public_fields$t_STRING
  withr::defer({lexer$public_fields$t_DBLSTRING <- d; lexer$public_fields$t_STRING <- s}, envir = env)
  lexer$public_fields$t_DBLSTRING <- r"("([^"\\]|\\.)*")"
  lexer$public_fields$t_STRING <- r"('([^'\\]|\\.)*')"
}
