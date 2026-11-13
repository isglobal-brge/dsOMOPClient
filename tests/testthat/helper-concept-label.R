# Real, three-site DSLite execution; direct session inspection is test-only.
.clf_setup <- function(env = parent.frame(), strict = TRUE, excluded = FALSE, parser_strict = TRUE) {
  for (pkg in c('DSLite','DSI','dsOMOP','resourcer','RSQLite')) testthat::skip_if_not_installed(pkg)
  source <- testthat::test_path('..','..','..','dsOMOP','tests','fixtures','create_test_db.R')
  builders <- new.env(); sys.source(source, builders)
  base <- withr::local_tempdir(.local_envir = env)
  Sys.chmod(base, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = base, DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = '1',
    DSOMOP_PSEUDONYM_ROOT = '', DSOMOP_PSEUDONYM_PROVIDER = 'file',
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = 'false', DSOMOP_PSEUDONYM_KEY = ''), .local_envir = env)
  withr::local_options(list(dsstaging.base_dir = base), .local_envir = env)
  paths <- character(); servers <- list(); login <- DSI::newDSLoginBuilder()
  for (i in 1:3) {
    db <- file.path(base, paste0('site',i,'.sqlite'))
    paths[[paste0('site',i)]] <- db
    conn <- builders$create_test_omop_db(db, n_persons = 18)
    # v1 federation validates the declared database date type as well as names.
    for(table in c('observation','death','cohort','observation_period')) {
      ddl <- DBI::dbGetQuery(conn,paste0("SELECT sql FROM sqlite_master WHERE name = '",table,"'"))$sql[[1L]]
      ddl <- gsub('([a-z_]+_date) TEXT','\\1 DATE',ddl,perl=TRUE)
      DBI::dbExecute(conn,paste0('ALTER TABLE ',table,' RENAME TO label_fixture_old'))
      DBI::dbExecute(conn,ddl)
      DBI::dbExecute(conn,paste0('INSERT INTO ',table,' SELECT * FROM label_fixture_old'))
      DBI::dbExecute(conn,'DROP TABLE label_fixture_old')
    }
    DBI::dbExecute(conn, 'UPDATE person SET gender_concept_id = 8507')
    DBI::dbExecute(conn, 'DELETE FROM cohort')
    DBI::dbWriteTable(conn,'cohort',data.frame(cohort_definition_id=1L,subject_id=1:18,
      cohort_start_date='2020-01-01',cohort_end_date='2021-12-31'),append=TRUE)
    ids <- if (i == 1L) rep(c(45000123L,45000124L,45000125L), each = 6L) else rep(c(45000123L,45000124L), each = 9L)
    if (excluded && i == 3L) ids[1:6] <- 45000127L
    DBI::dbExecute(conn, 'DELETE FROM observation')
    DBI::dbWriteTable(conn, 'observation', data.frame(observation_id = 1:18, person_id = 1:18,
      observation_concept_id = 45000120L, observation_date = '2020-01-01',
      observation_type_concept_id = 0L, value_as_concept_id = ids), append = TRUE)
    DBI::dbWriteTable(conn, 'observation', data.frame(observation_id = 19:36, person_id = 1:18,
      observation_concept_id = ids, observation_date = '2020-01-02', observation_type_concept_id = 0L,
      value_as_number = 1), append = TRUE)
    DBI::dbExecute(conn, 'DELETE FROM death')
    DBI::dbWriteTable(conn, 'death', data.frame(person_id = 1:18, death_date = '2021-01-01', cause_concept_id = ids), append = TRUE)
    labels <- c('Education level', if (i == 1L) 'Old education' else 'New education', 'Same', 'Same')
    vocab <- data.frame(concept_id = c(45000120L,45000123L,45000124L,45000125L), concept_name = labels,
      domain_id = 'Observation', vocabulary_id = 'A27', concept_class_id = 'Finding', standard_concept = 'S',
      concept_code = as.character(c(45000120L,45000123L,45000124L,45000125L)),
      valid_start_date = '2000-01-01', valid_end_date = '2099-01-01', invalid_reason = NA_character_)
    if (excluded && i == 3L) {
      vocab$concept_name[vocab$concept_id == 45000123L] <- 'Excluded candidate'
      extra <- vocab[2L,,drop=FALSE]; extra$concept_id <- 45000127L; extra$concept_name <- 'Excluded category'
      vocab <- rbind(vocab,extra)
    }
    if (i == 3L && !excluded) vocab <- vocab[vocab$concept_id != 45000123L,]
    DBI::dbWriteTable(conn, 'concept', vocab, append = TRUE)
    DBI::dbWriteTable(conn, 'vocabulary', data.frame(vocabulary_id = 'A27', vocabulary_version = if(i == 1L) '2025-01-01' else '2026-01-01'), overwrite = TRUE)
    DBI::dbDisconnect(conn)
    resource <- resourcer::newResource(name = 'labels', url = paste0('omop+dbi:sqlite://',db), format = 'omop.dbi.db')
    config <- DSLite::defaultDSConfiguration(include = if(requireNamespace('dsBase',quietly=TRUE)) c('dsOMOP','dsBase') else 'dsOMOP')
    config$Options[['dsomop.query_strict']] <- if(strict) 'TRUE' else 'FALSE'
    config$Options[['nfilter.subset']] <- '5'; config$Options[['nfilter.tab']] <- '3'
    config$Options[['nfilter.levels.max']] <- '40'; config$Options[['nfilter.levels.density']] <- '1'
    server <- DSLite::newDSLiteServer(resources = list(labels = resource), config = config, strict = parser_strict)
    global <- paste0('a27_site_',i)
    assign(global, server, globalenv())
    local({ name <- global; withr::defer(rm(list = name, envir = globalenv()), envir = env) })
    login$append(server = paste0('site',i), url = global, driver = 'DSLiteDriver')
    servers[[paste0('site',i)]] <- server
  }
  conns <- DSI::datashield.login(login$build())
  withr::defer(DSI::datashield.logout(conns), envir = env)
  ds.omop.connect(resource = 'labels', symbol = 'labels', conns = conns)
  withr::defer(ds.omop.disconnect('labels', conns = conns), envir = env)
  list(conns = conns, servers = servers, paths = paths)
}

.clf_frames <- function(ctx, symbol) lapply(ctx$servers, function(server) {
  x <- server$getSessionData(server$getSessionIds()[[1L]], symbol)
  if (inherits(x, 'FlowerDatasetDescriptor')) {
    chunks <- list(); dsOMOP:::.publishReadChunks(x, function(c) chunks[[length(chunks)+1L]] <<- c)
    x <- do.call(rbind,chunks)
  }
  x
})

.clf_recipe <- function() omop_recipe(variables = list(
  omop_variable('education', event = omop_event('observation', 45000120L),
    reducer = omop_reduce('last_value', value = 'value_as_concept_id')),
  omop_variable('cause', table = 'death', column = 'cause_concept_id')),
  labels = c('45000126' = 'declared only'))

