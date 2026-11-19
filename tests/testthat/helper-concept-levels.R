# Three-site DSLite federation for concept levels: site1 holds 60 causes of a
# 68-cause public vocabulary, sites 2 and 3 eight. Shared by the vocabulary
# (2.10.6) and automatic (2.10.7) concept level tests.
.cvl_root <- 910000L
.cvl_causes <- c(910001:910008, 910101:910160)

.cvl_setup <- function(env = parent.frame(), levels_max = "40", auto_max = NULL, deaths = NULL) {
  for (pkg in c("DSLite", "DSI", "dsOMOP", "resourcer", "RSQLite")) testthat::skip_if_not_installed(pkg)
  source <- testthat::test_path("..", "..", "..", "dsOMOP", "tests", "fixtures", "create_test_db.R")
  testthat::skip_if_not(file.exists(source), "server test fixtures are not available")
  builders <- new.env(); sys.source(source, builders)
  base <- withr::local_tempdir(.local_envir = env)
  Sys.chmod(base, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = base, DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_PROVIDER = "file",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_KEY = ""), .local_envir = env)
  withr::local_options(list(dsstaging.base_dir = base), .local_envir = env)
  # site1: 60 distinct causes, four deaths each; sites 2 and 3: eight causes.
  deaths <- deaths %||% list(rep(910101:910160, each = 4L), rep(910001:910008, each = 5L), rep(910001:910008, each = 5L))
  concepts <- c(.cvl_root, .cvl_causes)
  login <- DSI::newDSLoginBuilder()
  servers <- list(); paths <- character()
  for (i in seq_along(deaths)) {
    db <- file.path(base, paste0("site", i, ".sqlite"))
    ids <- deaths[[i]]
    conn <- builders$create_test_omop_db(db, n_persons = length(ids))
    for (table in c("death", "observation_period")) {
      ddl <- DBI::dbGetQuery(conn, paste0("SELECT sql FROM sqlite_master WHERE name = '", table, "'"))$sql[[1L]]
      ddl <- gsub("([a-z_]+_date) TEXT", "\\1 DATE", ddl, perl = TRUE)
      DBI::dbExecute(conn, paste0("ALTER TABLE ", table, " RENAME TO cvl_old"))
      DBI::dbExecute(conn, ddl)
      DBI::dbExecute(conn, paste0("INSERT INTO ", table, " SELECT * FROM cvl_old"))
      DBI::dbExecute(conn, "DROP TABLE cvl_old")
    }
    DBI::dbExecute(conn, "DELETE FROM death")
    DBI::dbWriteTable(conn, "death", data.frame(person_id = seq_along(ids), death_date = "2021-01-01",
      death_type_concept_id = 0L, cause_concept_id = ids), append = TRUE)
    DBI::dbWriteTable(conn, "concept", data.frame(concept_id = concepts,
      concept_name = c("Cause of death", paste("Cause", .cvl_causes)), domain_id = "Condition",
      vocabulary_id = "Fixture", concept_class_id = "Fixture", standard_concept = "S",
      concept_code = as.character(concepts), valid_start_date = "2000-01-01",
      valid_end_date = "2099-12-31", invalid_reason = NA_character_), append = TRUE)
    DBI::dbWriteTable(conn, "concept_ancestor", data.frame(
      ancestor_concept_id = c(rep(.cvl_root, length(concepts)), .cvl_causes),
      descendant_concept_id = c(concepts, .cvl_causes),
      min_levels_of_separation = c(0L, rep(1L, length(.cvl_causes)), rep(0L, length(.cvl_causes))),
      max_levels_of_separation = c(0L, rep(1L, length(.cvl_causes)), rep(0L, length(.cvl_causes)))), append = TRUE)
    DBI::dbDisconnect(conn)
    resource <- resourcer::newResource(name = "cvl", url = paste0("omop+dbi:sqlite://", db), format = "omop.dbi.db")
    # dsBase too when installed, for the tables and summaries of the copies.
    config <- DSLite::defaultDSConfiguration(include = c(if (requireNamespace("dsBase", quietly = TRUE)) "dsBase", "dsOMOP"))
    config$Options[["nfilter.tab"]] <- "3"; config$Options[["nfilter.subset"]] <- "3"
    config$Options[["nfilter.levels.max"]] <- levels_max; config$Options[["nfilter.levels.density"]] <- "0.33"
    # DSLite sessions set options for the whole R process: always set it.
    config$Options[["dsomop.auto_declared_levels_max"]] <- auto_max %||% "10000"
    server <- DSLite::newDSLiteServer(resources = list(cvl = resource), config = config)
    global <- paste0("cvl_site_", i)
    assign(global, server, globalenv())
    local({ name <- global; withr::defer(rm(list = name, envir = globalenv()), envir = env) })
    login$append(server = paste0("site", i), url = global, driver = "DSLiteDriver")
    servers[[paste0("site", i)]] <- server; paths[[i]] <- db
  }
  conns <- DSI::datashield.login(login$build())
  withr::defer(DSI::datashield.logout(conns), envir = env)
  ds.omop.connect(resource = "cvl", symbol = "cvl", conns = conns)
  withr::defer(ds.omop.disconnect("cvl", conns = conns), envir = env)
  list(conns = conns, servers = servers, paths = paths)
}

.cvl_frames <- function(ctx, symbol) lapply(ctx$servers, function(server) {
  value <- tryCatch(server$getSessionData(server$getSessionIds()[[1L]], symbol), error = function(e) NULL)
  # A staged output is a descriptor; read its private rows back.
  if (inherits(value, "FlowerDatasetDescriptor")) {
    frame <- NULL
    dsOMOP:::.publishReadChunks(value, function(chunk) frame <<- rbind(frame, chunk))
    value <- frame
  }
  value
})

# The published column has one shared level set; returns it.
.cvl_shared <- function(ctx, symbol, column) {
  frames <- .cvl_frames(ctx, symbol)
  expect_true(all(vapply(frames, is.data.frame, logical(1))), info = symbol)
  sets <- lapply(frames, function(frame) {
    expect_true(is.factor(frame[[column]]), info = paste(symbol, column))
    levels(frame[[column]])
  })
  for (set in sets) expect_identical(set, sets[[1L]], info = symbol)
  sets[[1L]]
}

# Declared domains report the same levels at every site, whatever each site holds.
.cvl_reports_identical <- function(ctx, symbol) {
  reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol(symbol)))
  for (report in reports) {
    expect_length(report$unsafe, 0L)
    expect_identical(report$levels, reports[[1L]]$levels, info = symbol)
  }
}

.cvl_event <- omop_event(table = "death", name = "cause_of_death")
.cvl_set_event <- omop_event(table = "death", name = "cause_of_death",
  concepts = omop_concept_set(.cvl_root, descendants = TRUE))
.cvl_first <- omop_reduce("first_value", value = "cause_concept_id")
.cvl_wide <- function(event, spec = NULL, ...) {
  variable <- omop_variable(event = event, reducer = .cvl_first)
  if (!is.null(spec)) variable <- omop_category(variable, spec)
  omop_recipe(variables = list(variable), outputs = omop_output(name = "study", type = "wide"), ...)
}
