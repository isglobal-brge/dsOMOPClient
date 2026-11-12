# Release gate for design sections 10.7 and 12.3: execute the frozen public
# recipes, including Circe import and parameter binding, through DSLite and
# the real population/reducer/output/publication stack at k = 5. Inspection
# of the server's session store below is test-only; the analyst receives only
# published symbols. No roster or compiler is replaced by a test double.
# Set DSOMOP_TEST_WS7_ACCEPTANCE_REQUIRED=true for the integrated WS-7 run:
# missing compilers, APIs, fixtures or either database then fail the gate.

.ws7e_required <- function() identical(tolower(Sys.getenv("DSOMOP_TEST_WS7_ACCEPTANCE_REQUIRED")), "true")

.ws7e_need <- function(ok, message) {
  if (isTRUE(ok)) return(invisible(NULL))
  if (.ws7e_required()) stop(message, call. = FALSE)
  testthat::skip(message)
}

.ws7e_paths <- function() {
  server <- normalizePath(testthat::test_path("..", "..", "..", "dsOMOP"), mustWork = FALSE)
  list(fixtures = file.path(server, "tests", "fixtures"),
       recipes = file.path(server, "inst", "plans", "acceptance"))
}

.ws7e_dependencies <- function() {
  .ws7e_need(requireNamespace("dsOMOP", quietly = TRUE), "WS-7 acceptance needs dsOMOP")
  .ws7e_need(exists(".compilePopulation", asNamespace("dsOMOP"), inherits = FALSE),
             "WS-7 acceptance awaits the coordinated WS-6 population compiler merge")
  api <- c("omop_event", "omop_index", "omop_reduce", "recipe_bind", "omop_param")
  .ws7e_need(all(api %in% getNamespaceExports("dsOMOPClient")),
             "WS-7 acceptance needs the WS-8/WS-9 public client API")
  for (pkg in c("DSLite", "DSI", "resourcer", "RSQLite", "DBI", "jsonlite")) {
    .ws7e_need(requireNamespace(pkg, quietly = TRUE), paste("WS-7 acceptance needs", pkg))
  }
  paths <- .ws7e_paths()
  .ws7e_need(dir.exists(paths$fixtures) && dir.exists(paths$recipes),
             "WS-7 acceptance needs the matching sibling server fixtures")
  paths
}

.ws7e_pg_config <- function() {
  configured <- nzchar(Sys.getenv("DSOMOP_TEST_POSTGRES_HOST"))
  list(host = if (configured) Sys.getenv("DSOMOP_TEST_POSTGRES_HOST") else Sys.getenv("DSOMOP_TEST_PG_HOST", "omopdb"),
       port = as.integer(Sys.getenv(if (configured) "DSOMOP_TEST_POSTGRES_PORT" else "DSOMOP_TEST_PG_PORT", "5432")),
       db = Sys.getenv(if (configured) "DSOMOP_TEST_POSTGRES_DATABASE" else "DSOMOP_TEST_PG_DB", if (configured) "dsomop" else "omop"),
       user = Sys.getenv(if (configured) "DSOMOP_TEST_POSTGRES_ADMIN_USER" else "DSOMOP_TEST_PG_USER", if (configured) "dsomop_admin" else "omop"),
       password = Sys.getenv(if (configured) "DSOMOP_TEST_POSTGRES_ADMIN_PASSWORD" else "DSOMOP_TEST_PG_PASSWORD", if (configured) "dsomop" else "omop"))
}

.ws7e_setup <- function(backend, .local_envir) {
  paths <- .ws7e_dependencies()
  builders <- new.env(parent = globalenv())
  for (file in c("create_reach_db.R", "extend_reach_db.R", "load_reach_pg.R")) {
    sys.source(file.path(paths$fixtures, file), envir = builders)
  }
  fixture <- builders$build_extended_reach_handle(100L, with_defects = FALSE)
  on.exit(DBI::dbDisconnect(fixture$conn), add = TRUE)
  base <- withr::local_tempdir(.local_envir = .local_envir)
  Sys.chmod(base, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = base, DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
                        DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_PROVIDER = "file",
                        DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_KEY = ""),
                     .local_envir = .local_envir)
  withr::local_options(list(dsstaging.base_dir = base, dsomop.staged_chunk_size = 7L,
                            nfilter.subset = 5L), .local_envir = .local_envir)
  symbol <- paste0("omop_ws7_", backend)
  if (identical(backend, "sqlite")) {
    db <- file.path(base, "reach.sqlite")
    dest <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
    for (table in DBI::dbListTables(fixture$conn)) {
      DBI::dbWriteTable(dest, table, DBI::dbReadTable(fixture$conn, table), overwrite = TRUE)
    }
    DBI::dbDisconnect(dest)
    resource <- resourcer::newResource(name = symbol, url = paste0("omop+dbi:sqlite://", db),
                                       format = "omop.dbi.db")
  } else {
    .ws7e_need(requireNamespace("RPostgres", quietly = TRUE), "WS-7 acceptance needs RPostgres")
    pg <- .ws7e_pg_config()
    conn <- tryCatch(DBI::dbConnect(RPostgres::Postgres(), host = pg$host, port = pg$port,
                                    dbname = pg$db, user = pg$user, password = pg$password,
                                    sslmode = "disable", connect_timeout = 5L), error = function(e) NULL)
    .ws7e_need(!is.null(conn), "WS-7 acceptance needs the PostgreSQL test host")
    schema <- paste0("ws7_acceptance_", Sys.getpid(), "_", sample.int(1000000L, 1L))
    withr::defer({
      DBI::dbExecute(conn, paste0("DROP SCHEMA IF EXISTS ", schema, " CASCADE"))
      DBI::dbDisconnect(conn)
    }, envir = .local_envir)
    builders$load_reach_fixture_pg(conn, schema = schema, handle = fixture)
    resource <- resourcer::newResource(name = symbol,
      url = paste0("omop+dbi:postgresql://", pg$host, ":", pg$port, "/", pg$db,
                   "?cdm_schema=", schema, "&vocabulary_schema=", schema, "&results_schema=", schema),
      identity = pg$user, secret = pg$password, format = "omop.dbi.db")
  }
  server <- DSLite::newDSLiteServer(resources = stats::setNames(list(resource), symbol),
                                    config = DSLite::defaultDSConfiguration(include = "dsOMOP"))
  server_name <- paste0("ws7_acceptance_", backend, "_", Sys.getpid())
  assign(server_name, server, envir = globalenv())
  withr::defer(rm(list = server_name, envir = globalenv()), envir = .local_envir)
  login <- DSI::newDSLoginBuilder()
  login$append(server = "site1", url = server_name, driver = "DSLiteDriver")
  conns <- DSI::datashield.login(login$build())
  connected <- FALSE
  withr::defer({
    if (connected) ds.omop.disconnect(symbol = symbol, conns = conns)
    DSI::datashield.logout(conns)
  }, envir = .local_envir)
  ds.omop.connect(resource = symbol, symbol = symbol, conns = conns)
  connected <- TRUE
  sid <- server$getSessionIds()
  stopifnot(length(sid) == 1L)
  session <- .get_session(symbol)
  handle <- server$getSessionData(sid[[1L]], paste0(".dsomop_handle_", session$res_symbol))
  list(server = server, sid = sid[[1L]], conns = conns, symbol = symbol,
       handle = handle, paths = paths)
}

.ws7e_frame <- function(ctx, symbol) {
  frame <- ctx$server$getSessionData(ctx$sid, symbol)
  if (inherits(frame, "FlowerDatasetDescriptor")) {
    chunks <- list()
    dsOMOP:::.publishReadChunks(frame, function(chunk) chunks[[length(chunks) + 1L]] <<- chunk)
    frame <- do.call(rbind, chunks)
  }
  frame
}

.ws7e_expected <- function(output) {
  cols <- unique(unlist(lapply(output$rows, names)))
  result <- lapply(cols, function(col) unlist(lapply(output$rows, function(row) {
    if (is.null(row[[col]])) NA else row[[col]]
  })))
  names(result) <- cols
  as.data.frame(result, stringsAsFactors = FALSE)
}

# Reconstruct the chronological roster key from independent oracle episode
# identities, never from the production compiler's roster. All non-matching
# corpus populations have one episode per (person, start), except recipe 05's
# two same-day MI records whose ends are equal and whose source keys break the
# tie. Union the outputs so survival omissions cannot renumber the baseline.
.ws7e_episode_catalog <- function(ctx, recipe, frozen, expected, component) {
  root <- strsplit(component, ".", fixed = TRUE)[[1L]][[1L]]
  pop <- frozen$outputs[[root]]$rows
  roots <- names(frozen$outputs)[vapply(frozen$outputs, function(x) identical(x$rows, pop), logical(1))]
  components <- names(expected)[vapply(strsplit(names(expected), ".", fixed = TRUE),
                                       function(x) x[[1L]] %in% roots, logical(1))]
  rows <- lapply(expected[components], .ws7e_expected)
  # Recipe 09 excludes the episode whose follow-up ends before TAR entry.
  # Recipe 01 independently defines the same complete c1 population.
  if (identical(recipe, "09-recurrent-events")) {
    complete <- jsonlite::fromJSON(file.path(ctx$paths$recipes, "01-characterisation", "expected.json"),
                                  simplifyVector = FALSE)
    rows <- list(.ws7e_expected(complete$outputs$table1))
  }
  keys <- c("person_id", "index_date", intersect("index_key", names(rows[[1L]])))
  catalog <- unique(do.call(rbind, lapply(rows, function(x) x[keys])))
  ordering <- list(as.numeric(catalog$person_id), catalog$index_date)
  if ("index_key" %in% keys) ordering <- c(ordering, list(as.character(catalog$index_key)))
  catalog <- catalog[do.call(order, c(ordering, list(method = "radix"))), , drop = FALSE]
  catalog$cohort_row_id <- seq_len(nrow(catalog))
  catalog
}

.ws7e_episode_key <- function(rows, keys) {
  do.call(paste, c(lapply(rows[keys], as.character), sep = "|"))
}

.ws7e_compare <- function(frame, expected, handle, info, episodes = NULL) {
  want <- .ws7e_expected(expected)
  if (!is.null(episodes)) {
    keys <- setdiff(names(episodes), "cohort_row_id")
    at <- match(.ws7e_episode_key(want, keys), .ws7e_episode_key(episodes, keys))
    expect_false(anyNA(at), info = info)
    want$cohort_row_id <- episodes$cohort_row_id[at]
  } else {
    # Matching publishes set_id/is_case, which bind each person's values to
    # the oracle case time directly. Each membership must have its own key.
    expect_identical(anyDuplicated(as.character(frame$cohort_row_id)), 0L, info = info)
  }
  expect_identical(as.character(frame$row_id), as.character(frame$cohort_row_id), info = info)
  released <- as.character(unlist(expected$released_columns))
  expect_setequal(names(frame), released)
  expect_identical(nrow(frame), nrow(want), info = info)
  cols <- intersect(names(want), released)
  want <- want[cols]
  want <- dsOMOP:::.pseudonymizeIdentifiers(want, dsOMOP:::.personKey(handle),
                                           dsOMOP:::.personKeyPublicContract(handle))
  got <- as.data.frame(frame[cols], stringsAsFactors = FALSE)
  for (col in cols) {
    if (is.numeric(want[[col]])) got[[col]] <- as.numeric(got[[col]]) else {
      got[[col]] <- as.character(got[[col]])
      want[[col]] <- as.character(want[[col]])
    }
  }
  ordered <- function(x) {
    x <- x[do.call(order, c(unname(x), list(na.last = TRUE, method = "radix"))), , drop = FALSE]
    attributes(x)[setdiff(names(attributes(x)), c("names", "row.names", "class"))] <- NULL
    class(x) <- "data.frame"
    rownames(x) <- NULL
    x
  }
  expect_equal(ordered(got), ordered(want), tolerance = 1e-9, info = info)
  expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$", frame$person_id)), info = info)
  expect_gte(length(unique(frame$person_id)), 5L)
  expect_false(any(c("subject_id", "index_key", "index_visit_id", "op_component_id") %in% names(frame)), info = info)
}

.ws7e_canonical <- function(frame) {
  frame <- as.data.frame(frame, stringsAsFactors = FALSE)
  attributes(frame)[setdiff(names(attributes(frame)), c("names", "row.names", "class"))] <- NULL
  class(frame) <- "data.frame"
  for (col in names(frame)) {
    value <- frame[[col]]
    frame[[col]] <- if (is.numeric(value)) format(value, digits = 17, trim = TRUE,
                                                 scientific = FALSE) else as.character(value)
    frame[[col]][is.na(value)] <- NA_character_
  }
  rownames(frame) <- NULL
  frame
}

.ws7e_run <- function(ctx, recipe) {
  path <- file.path(ctx$paths$recipes, recipe)
  env <- new.env(parent = asNamespace("dsOMOPClient"))
  withr::with_dir(path, sys.source("recipe.R", envir = env))
  rec <- if (identical(recipe, "13-parametrised")) env$bound else env$rec
  frozen <- jsonlite::fromJSON(file.path(path, "plan.json"), simplifyVector = FALSE)
  expect_identical(.plan_v2_digest(recipe_to_plan(rec))$sha256,
                   .plan_v2_digest(frozen)$sha256, info = recipe)
  expected <- jsonlite::fromJSON(file.path(path, "expected.json"), simplifyVector = FALSE)$outputs
  frames <- list()
  for (mode in c("memory", "staged")) {
    prefix <- paste0("D_ws7_", substr(recipe, 1L, 2L), "_", mode, "_")
    roots <- names(frozen$outputs)
    out <- stats::setNames(paste0(prefix, roots), roots)
    before <- unlist(DSI::datashield.symbols(ctx$conns), use.names = FALSE)
    recipe_execute(rec, out = out, symbol = ctx$symbol, conns = ctx$conns, output_mode = mode)
    after <- unlist(DSI::datashield.symbols(ctx$conns), use.names = FALSE)
    added <- setdiff(after, before)
    seen <- character(0)
    for (component in names(expected)) {
      root <- strsplit(component, ".", fixed = TRUE)[[1L]][[1L]]
      suffix <- substring(component, nchar(root) + 2L)
      candidates <- if (!nzchar(suffix)) out[[root]] else paste0(out[[root]], ".", suffix)
      if (identical(suffix, "risk_sets")) candidates <- c(candidates, paste0(out[[root]], ".riskSets"))
      published <- intersect(candidates, added)
      expect_length(published, 1L)
      if (length(published) != 1L) next
      seen <- c(seen, published)
      raw <- ctx$server$getSessionData(ctx$sid, published)
      expect_identical(inherits(raw, "FlowerDatasetDescriptor"), identical(mode, "staged"))
      frame <- .ws7e_frame(ctx, published)
      episodes <- if (!identical(recipe, "10-matching")) {
        .ws7e_episode_catalog(ctx, recipe, frozen, expected, component)
      }
      .ws7e_compare(frame, expected[[component]], ctx$handle, paste(recipe, mode, component), episodes)
      if (identical(mode, "memory")) frames[[component]] <- .ws7e_canonical(frame) else {
        expect_identical(.ws7e_canonical(frame), frames[[component]], info = paste(recipe, component))
      }
    }
    expect_setequal(added, seen)
    expect_false(any(grepl("^dsOexec", after)))
    expect_length(ctx$handle$temp_tables, 0L)
  }
  invisible(NULL)
}

for (.ws7e_backend in c("sqlite", "postgresql")) local({
  backend <- .ws7e_backend
  test_that(paste("all 15 public acceptance recipes reproduce the oracle in memory and staged on", backend), {
    ctx <- .ws7e_setup(backend, environment())
    recipes <- sort(list.dirs(ctx$paths$recipes, recursive = FALSE, full.names = FALSE))
    recipes <- recipes[grepl("^[0-9][0-9]-", recipes)]
    expect_identical(length(recipes), 15L)
    for (recipe in recipes) expect_error(.ws7e_run(ctx, recipe), NA, info = recipe)
  })
})
