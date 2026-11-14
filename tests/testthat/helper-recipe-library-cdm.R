# Isolated synthetic CDM, shared by all 24 templates. Expectations are hand
# calculated below; neither the plan compiler nor executor creates them.
.library_cdm_tables <- function(conn) {
  if (!"days_supply" %in% DBI::dbListFields(conn, "drug_exposure"))
    DBI::dbExecute(conn, "ALTER TABLE drug_exposure ADD COLUMN days_supply INTEGER")
  people <- 1:24
  date <- function(day) as.character(as.Date("2020-01-01") + day)
  DBI::dbExecute(conn, "DELETE FROM person")
  DBI::dbWriteTable(conn, "person", data.frame(person_id = people,
    gender_concept_id = 8507L, year_of_birth = 1980L, month_of_birth = 12L,
    day_of_birth = 31L, race_concept_id = 0L, ethnicity_concept_id = 0L), append = TRUE)
  for (tab in c("observation_period", "visit_occurrence", "condition_occurrence", "drug_exposure",
    "measurement", "observation", "death", "cohort")) DBI::dbExecute(conn, paste("DELETE FROM", tab))
  DBI::dbWriteTable(conn, "observation_period", data.frame(observation_period_id = people,
    person_id = people, observation_period_start_date = "2018-01-01",
    observation_period_end_date = "2022-12-31", period_type_concept_id = 0L), append = TRUE)
  DBI::dbWriteTable(conn, "visit_occurrence", data.frame(visit_occurrence_id = people,
    person_id = people, visit_concept_id = 9201L, visit_start_date = date(0),
    visit_end_date = date(0), visit_type_concept_id = 0L), append = TRUE)
  obs <- do.call(rbind, lapply(seq_along(c(-20,-5,-2)), function(i) data.frame(
    observation_id = people + (i-1L)*24L, person_id = people,
    observation_concept_id = if (i == 3L) 990002L else 990001L,
    observation_date = date(c(-20,-5,-2)[[i]]), observation_type_concept_id = 0L,
    value_as_concept_id = c(990101L, NA_integer_, 0L)[[i]])))
  DBI::dbWriteTable(conn, "observation", obs, append = TRUE)
  measure <- do.call(rbind, lapply(1:3, function(i) data.frame(measurement_id = people + (i-1L)*24L,
    person_id = people, measurement_concept_id = 990003L, measurement_date = date(c(-10,0,10)[i]),
    measurement_type_concept_id = 0L, value_as_number = c(4,6,8)[i], unit_concept_id = 8840L)))
  DBI::dbWriteTable(conn, "measurement", measure, append = TRUE)
  conditions <- rbind(data.frame(condition_occurrence_id = people, person_id = people,
    condition_concept_id = 990004L, condition_start_date = date(-10),
    condition_end_date = date(-10), condition_type_concept_id = 0L),
    do.call(rbind, lapply(1:3, function(i) data.frame(condition_occurrence_id = (1:12) + i*24L,
      person_id = 1:12, condition_concept_id = if (i == 3L) 990008L else 990005L,
      condition_start_date = date(c(10,20,15)[i]), condition_end_date = date(c(10,20,15)[i]),
      condition_type_concept_id = 0L))))
  DBI::dbWriteTable(conn, "condition_occurrence", conditions, append = TRUE)
  drugs <- do.call(rbind, lapply(1:3, function(i) data.frame(drug_exposure_id = people + (i-1L)*24L,
    person_id = people, drug_concept_id = if (i == 3L) 990007L else 990006L,
    drug_exposure_start_date = date(c(0,14,100)[i]), drug_exposure_end_date = date(c(29,43,129)[i]),
    drug_type_concept_id = 0L, days_supply = 30L, quantity = 30)))
  DBI::dbWriteTable(conn, "drug_exposure", drugs, append = TRUE)
  # All death records are outside the default 365-day horizon. This tests
  # zero/NA death output without censoring the survival oracle.
  DBI::dbWriteTable(conn, "death", data.frame(person_id = people, death_date = date(500),
    death_type_concept_id = 0L, cause_concept_id = 0L), append = TRUE)
  vocab <- data.frame(concept_id = c(990001:990008,990101),
    concept_name = c("Education question", "Employment question", "Numeric test", "History test",
      "Outcome test", "Drug A test", "Drug B test", "Cause B test", "Answer test"),
    domain_id = c("Observation", "Observation", "Measurement", "Condition", "Condition", "Drug", "Drug", "Condition", "Meas Value"),
    vocabulary_id = "LibraryFixture", concept_class_id = "Clinical Finding", standard_concept = "S",
    concept_code = as.character(c(990001:990008,990101)), valid_start_date = "1970-01-01",
    valid_end_date = "2099-12-31", invalid_reason = NA_character_)
  DBI::dbWriteTable(conn, "concept", vocab, append = TRUE)
  invisible(NULL)
}

.library_cdm_setup <- function(backend = "sqlite", env = parent.frame(), two_sites = FALSE, second_site = "empty_observation", mutate = NULL) {
  for (pkg in c("dsOMOP", "DSLite", "DSI", "resourcer", "RSQLite")) testthat::skip_if_not_installed(pkg)
  builders <- new.env(parent = globalenv())
  source <- testthat::test_path("..", "..", "..", "dsOMOP", "tests", "fixtures")
  sys.source(file.path(source, "create_test_db.R"), builders)
  base <- withr::local_tempdir(.local_envir = env)
  Sys.chmod(base, "0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR = base, DSOMOP_TEST_ALLOW_EPHEMERAL_STATE = "1",
    DSOMOP_PSEUDONYM_ROOT = "", DSOMOP_PSEUDONYM_PROVIDER = "file",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING = "false", DSOMOP_PSEUDONYM_KEY = ""), .local_envir = env)
  withr::local_options(list(dsstaging.base_dir = base), .local_envir = env)
  path <- file.path(base, "fixture.sqlite")
  conn <- builders$create_test_omop_db(path, n_persons = 24)
  on.exit(if (DBI::dbIsValid(conn)) DBI::dbDisconnect(conn), add = TRUE)
  .library_cdm_tables(conn)
  if (!is.null(mutate)) mutate(conn)
  tables <- sort(DBI::dbListTables(conn))
  fixture_hash <- paste(openssl::sha256(charToRaw(paste(vapply(tables, function(tab)
    jsonlite::toJSON(DBI::dbReadTable(conn, tab), na = "null", digits = NA), ""), collapse = "\n"))))
  vocabulary_hash <- paste(openssl::sha256(charToRaw(jsonlite::toJSON(DBI::dbReadTable(conn, "concept"), na = "null", digits = NA))))
  # Hash the fully mutated source fixture before vendor copying/connection.
  # PostgreSQL execution uses qualified isolated schemas, not search_path.
  variant_fixture_hash <- paste(openssl::sha256(charToRaw(jsonlite::toJSON(
    lapply(tables, function(tab) DBI::dbReadTable(conn, tab)),
    na = "null", null = "null", digits = NA))))
  if (backend == "sqlite") {
    resource <- resourcer::newResource(name = "library", url = paste0("omop+dbi:sqlite://", path), format = "omop.dbi.db")
  } else if (backend == "postgresql") {
    testthat::skip_if_not_installed("RPostgres")
    pg <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("DSOMOP_TEST_POSTGRES_HOST", "omopdb"),
      port = as.integer(Sys.getenv("DSOMOP_TEST_POSTGRES_PORT", "5432")),
      dbname = Sys.getenv("DSOMOP_TEST_POSTGRES_DATABASE", "omop"),
      user = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_USER", "omop"),
      password = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_PASSWORD", "omop"), sslmode = "disable")
    schema <- paste0("library_", Sys.getpid(), "_", sample.int(1000000, 1))
    withr::defer({DBI::dbExecute(pg, paste("DROP SCHEMA IF EXISTS", schema, "CASCADE")); DBI::dbDisconnect(pg)}, envir = env)
    sys.source(file.path(source, "load_reach_pg.R"), builders)
    builders$load_reach_fixture_pg(pg, schema, handle = list(conn = conn, person_key = "person_id"))
    resource <- resourcer::newResource(name = "library", url = paste0("omop+dbi:postgresql://",
      Sys.getenv("DSOMOP_TEST_POSTGRES_HOST", "omopdb"), ":", Sys.getenv("DSOMOP_TEST_POSTGRES_PORT", "5432"), "/",
      Sys.getenv("DSOMOP_TEST_POSTGRES_DATABASE", "omop"), "?cdm_schema=", schema,
      "&vocabulary_schema=", schema, "&results_schema=", schema), format = "omop.dbi.db",
      identity = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_USER", "omop"), secret = Sys.getenv("DSOMOP_TEST_POSTGRES_ADMIN_PASSWORD", "omop"))
  } else if (backend %in% c("mysql", "mariadb")) {
    sys.source(file.path(source, "..", "testthat", "helper-vendor-db.R"), builders)
    sys.source(file.path(source, "..", "testthat", "helper-ws4.R"), builders)
    cfg <- builders$.dsomopVendorTestConfig(backend)
    if (is.null(cfg)) testthat::skip(paste(backend, "not configured"))
    dest <- builders$.dsomopVendorAdminConnection(cfg)
    schema <- builders$.dsomopVendorNamespaceToken()
    DBI::dbExecute(dest, paste0("CREATE DATABASE `",schema,"`"))
    withr::defer({DBI::dbExecute(dest,paste0("DROP DATABASE `",schema,"`"));DBI::dbDisconnect(dest)},envir=env)
    builders$.ws4_copy_tables(conn,dest,schema)
    resource <- resourcer::newResource(name="library",format="omop.dbi.db",
      url=paste0("omop+dbi:",backend,"://",cfg$host,":",cfg$port,"/",schema,
        "?cdm_schema=",schema,"&vocabulary_schema=",schema,"&results_schema=",schema),
      identity=cfg$admin_user,secret=cfg$admin_password)
  } else stop("Unknown fixture backend")
  DBI::dbDisconnect(conn)
  second_resource <- NULL
  if (two_sites) {
    stopifnot(backend == "sqlite")
    second <- file.path(base,"second.sqlite")
    file.copy(path,second)
    second_conn <- DBI::dbConnect(RSQLite::SQLite(),second)
    for (tab in DBI::dbListTables(second_conn)) {
      rows <- DBI::dbReadTable(second_conn,tab)
      if (nrow(rows)) {
        DBI::dbExecute(second_conn,paste("DELETE FROM",tab))
        DBI::dbWriteTable(second_conn,tab,rows[rev(seq_len(nrow(rows))),,drop=FALSE],append=TRUE)
      }
    }
    if (second_site == "empty_observation") DBI::dbExecute(second_conn,"DELETE FROM observation")
    if (second_site == "tiny_roster") DBI::dbExecute(second_conn,"DELETE FROM visit_occurrence WHERE person_id > 4")
    if (second_site == "missing_measurement") DBI::dbExecute(second_conn,"DROP TABLE measurement")
    DBI::dbDisconnect(second_conn)
    second_resource <- resourcer::newResource(name="library",url=paste0("omop+dbi:sqlite://",second),format="omop.dbi.db")
  }
  ctx <- .library_connect(resource, base, env, two_sites, fixture_hash, vocabulary_hash, second_resource)
  ctx$variant_fixture_hash <- variant_fixture_hash
  ctx
}

.library_connect <- function(resource, base, env, two_sites = FALSE, fixture_hash = NULL, vocabulary_hash = NULL, second_resource = NULL) {
  config <- DSLite::defaultDSConfiguration(include = "dsOMOP")
  config$Options[["nfilter.subset"]] <- "5"
  servers <- list(); login <- DSI::newDSLoginBuilder()
  for (i in seq_len(if (two_sites) 2L else 1L)) {
    server <- DSLite::newDSLiteServer(resources = list(library = if (i == 2L && !is.null(second_resource)) second_resource else resource), config = config)
    name <- paste0("library_fixture_", i, "_", Sys.getpid())
    assign(name, server, globalenv())
    local({nm <- name; withr::defer(rm(list = nm, envir = globalenv()), envir = env)})
    servers[[paste0("site", i)]] <- server
    login$append(server = paste0("site", i), url = name, driver = "DSLiteDriver")
  }
  conns <- DSI::datashield.login(login$build())
  withr::defer(DSI::datashield.logout(conns), envir = env)
  ds.omop.connect(resource = "library", symbol = "library", conns = conns)
  withr::defer(ds.omop.disconnect("library", conns = conns), envir = env)
  list(conns = conns, servers = servers, symbol = "library", fixture_hash = fixture_hash,
    vocabulary_hash = vocabulary_hash)
}

.library_frames <- function(ctx, symbol) lapply(ctx$servers, function(server) {
  frame <- server$getSessionData(server$getSessionIds()[[1L]], symbol)
  if (inherits(frame, "FlowerDatasetDescriptor")) {
    chunks <- list()
    dsOMOP:::.publishReadChunks(frame, function(x) chunks[[length(chunks)+1L]] <<- x)
    frame <- do.call(rbind, chunks)
  }
  frame
})

.library_assert_values <- function(id, frames) {
  f <- frames[[1L]]
  # Independent arithmetic over the synthetic fixture, not executor outputs.
  if (id == "demographics.baseline") {
    expect_equal(nrow(f), 24L); expect_equal(f$age, rep(40L,24)); expect_equal(f$prior_observed_days, rep(365L,24))
    expect_true(all(as.character(f$sex) == "M"))
  } else if (id == "demographics.death_cause") {
    expect_equal(f$death_recorded, rep(0L,24)); expect_true(all(is.na(f$death_days))); expect_true(all(is.na(f$cause_concept_id)))
  } else if (id == "socioeconomic.education") {
    expect_true(all(is.na(f$education_concept_id))); expect_equal(f$education_records, rep(2L,24))
  } else if (id == "socioeconomic.employment") {
    expect_equal(.library_oracle_frame(f)$employment_concept_id, rep("0",24)); expect_equal(f$employment_records, rep(1L,24))
  } else if (id == "socioeconomic.numeric_observation") {
    expect_equal(f$numeric_value, rep(4,24)); expect_equal(f$source_records, rep(1L,24))
  } else if (id == "comorbidity.condition_history") {
    expect_equal(f$history_present, rep(1L,24)); expect_equal(f$history_days, rep(1L,24))
  } else if (startsWith(id, "comorbidity.")) {
    expect_equal(f[[sub("comorbidity.", "", id, fixed = TRUE)]], rep(0,24))
  } else if (id == "drug.era_summary") {
    expect_equal(f$era_count, rep(1L,24)); expect_equal(f$era_days, rep(44L,24))
    expect_equal(f$first_era_days, rep(44L,24)); expect_equal(f$covered_days, rep(44L,24))
  } else if (id == "drug.adherence_components") {
    expect_equal(f$observed_days, rep(365L,24)); expect_equal(f$recorded_covered_days, rep(44L,24))
    expect_equal(f$effective_covered_days, rep(60L,24)); expect_equal(f$supply_days, rep(60L,24))
  } else if (id == "cohort.new_users") {
    expect_equal(nrow(f),24L); expect_equal(f$age,rep(40L,24)); expect_length(unique(f$index_source),1L)
  } else if (id == "outcome.incident_event") {
    expect_equal(nrow(f),12L); expect_equal(f$age,rep(40L,12)); expect_equal(f$prior_observed_days,rep(365L,12))
  } else if (id %in% c("survival.first_event", "survival.competing_risk", "cohort.active_comparator")) {
    for (nm in names(frames)) {
      x <- frames[[nm]]
      cap <- if (nm == "on_treatment_analysis") 73L else 365L
      expect_equal(nrow(x),24L); expect_equal(sort(x$event),rep(0:1,each=12))
      expect_equal(sort(x$exit_days_from_index),rep(c(10L,cap),each=12))
      expect_equal(x$follow_up_days,x$exit_days_from_index-1L)
      if (id == "survival.competing_risk") {
        expect_true(all(as.character(x$outcome_name[x$event == 1L]) == "cause_a")); expect_true(all(is.na(x$outcome_name[x$event == 0L])))
      }
      if (id == "cohort.active_comparator") expect_true(all(as.character(x$arm) == "era_a"))
    }
  } else if (id == "survival.recurrent_events") {
    e <- frames[["recurrent.events"]]; rr <- frames[["recurrent.risk_sets"]]
    expect_equal(nrow(e),36L); expect_equal(sum(e$event == 0),12L)
    expect_equal(sort(e$event_days_from_index[!is.na(e$event_days_from_index)]),rep(c(10L,20L),each=12))
    expect_equal(nrow(rr),24L); expect_equal(rr$follow_up_days,rep(364L,24))
  } else if (id == "panel.measurements") {
    expect_equal(nrow(f),24L*13L)
    expect_equal(sum(f$measure_count),48L)
    expect_equal(f$measure_mean[f$period_start_days == 0],rep(7,24))
    expect_equal(f$measure_last[f$period_start_days == 0],rep(8,24))
    expect_equal(f$measure_carry[f$period_start_days == 0],rep(6,24))
    expect_true(all(is.na(f$measure_mean[f$period_start_days > 0])))
  } else if (id == "panel.incidence") {
    expect_equal(nrow(f),24L); expect_equal(f$days,rep(366L,24)); expect_equal(sum(f$n_outcome),24L)
  } else if (id == "panel.sccs") {
    expect_equal(sum(f$days),24L*366L); expect_equal(sum(f$exposed_drug),24L*44L); expect_equal(sum(f$n_outcome),24L)
    expect_equal(unique(f$age),40L)
  } else if (id == "longitudinal.event_log") {
    expect_equal(nrow(f),96L); expect_setequal(as.character(f$source),c("measure","history"))
    expect_equal(sum(is.na(f$value)),24L)
  } else if (id == "matching.nested_case_control") {
    expect_equal(nrow(f),60L); expect_equal(sum(f$is_case),12L)
    expect_equal(f$sampling_weight[f$is_case == 1L],rep(1,12))
    expect_equal(f$sampling_weight[f$is_case == 0L],rep(3,48))
    expect_equal(f$exposure_history,rep(1L,60))
    expect_equal(length(unique(f$set_id)),12L)
  }
}

.library_expected_columns <- function(id, component, recipe) {
  keys <- c("row_id", "cohort_row_id", "person_id")
  s <- c("outcome_name", "event", "entry_days_from_index", "exit_days_from_index", "follow_up_days")
  if (id == "cohort.active_comparator") return(c(keys,s,"arm","age","sex"))
  if (id %in% c("survival.first_event","survival.competing_risk")) return(c(keys,s))
  if (id == "survival.recurrent_events") return(c(keys, if (component == "recurrent.events")
    c("outcome_name","event","event_number","outcome_event_number","event_days_from_index","entry_days_from_index","exit_days_from_index") else
    c("entry_days_from_index","exit_days_from_index","follow_up_days")))
  pp <- c(keys,"period_start_days","period_end_days","days")
  if (id == "panel.measurements") return(c(pp,"measure_count","measure_mean","measure_last","measure_carry"))
  if (id == "panel.incidence") return(c(pp,"calendar_year","n_outcome"))
  if (id == "panel.sccs") return(c(pp,"age","age_band","calendar_year","exposed_drug","active_drug","n_outcome"))
  if (id == "longitudinal.event_log") return(c(keys,"source","concept_id","start_days","end_days","value","unit_concept_id","event_number"))
  c(keys,names(recipe$variables))
}

.library_execute_case <- function(ctx, id, counter) {
  r <- .library_test_recipe(id)
  frames <- list()
  components <- if (id == "survival.recurrent_events") c("recurrent.events","recurrent.risk_sets") else r$publish
  for (mode in c("memory","staged")) {
    out <- setNames(paste0("D_lib_",counter,"_",mode,"_",r$publish),r$publish)
    before <- unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE)
    result <- recipe_execute(r, out = out, symbol = ctx$symbol, conns = ctx$conns, output_mode = mode)
    after <- unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE)
    added <- setdiff(after,before)
    got <- list()
    for (component in components) {
      root <- strsplit(component,".",fixed=TRUE)[[1L]][1L]
      suffix <- substring(component,nchar(root)+1L)
      candidates <- paste0(out[[root]],suffix)
      if (suffix == ".risk_sets") candidates <- c(candidates,paste0(out[[root]],".riskSets"))
      symbol <- intersect(candidates,added)
      expect_length(symbol,1L)
      f <- .library_frames(ctx,symbol[[1L]])[[1L]]
      expect_identical(names(f),.library_expected_columns(id,component,r))
      expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",f$person_id)))
      expect_gte(length(unique(f$person_id)),5L)
      expect_false(any(c("subject_id","index_key","index_visit_id") %in% names(f)))
      got[[component]] <- f
      if (mode == "memory") .library_check_schema(id,component,f)
      if (mode == "staged") {
        for (col in names(f)) {
          expect_identical(is.na(f[[col]]),is.na(frames[[component]][[col]]))
          expect_equal(as.character(f[[col]]),as.character(frames[[component]][[col]]))
        }
      }
    }
    expect_equal(length(added),length(components))
    .library_assert_values(id,got)
    .library_compare_oracle(id,got)
    if (mode == "memory") frames <- got
  }
  cat("\nLIBRARY_EXECUTION",id,"fixture",ctx$fixture_hash,"vocabulary",ctx$vocabulary_hash,"\n")
  invisible(frames)
}


.library_oracle_frame <- function(frame) {
  contracts <- attr(frame,"omop_concepts")
  out <- as.data.frame(frame)
  for (col in names(out)) {
    if (!is.null(contracts[[col]]$ids)) out[[col]] <- contracts[[col]]$ids else if (!is.null(contracts[[col]]$resolved)) {
      resolved <- contracts[[col]]$resolved
      out[[col]] <- as.character(unlist(resolved$ids))[match(as.character(out[[col]]),as.character(unlist(resolved$labels)))]
    }
    out[[col]] <- as.character(out[[col]])
    # Staged chunk concatenation need not retain the private concept capsule.
    # These labels/IDs are independently fixed by .library_cdm_tables(), not
    # inferred from the executor or from clinical rows.
    if (col %in% c("concept_id","education_concept_id","employment_concept_id","cause_concept_id")) {
      vocabulary <- c("History test"="990004","Numeric test"="990003","Answer test"="990101")
      at <- match(out[[col]],names(vocabulary))
      out[[col]][!is.na(at)] <- unname(vocabulary[at[!is.na(at)]])
    }
  }
  out
}

.library_compare_oracle <- function(id, frames) {
  expected <- .library_test_json(file.path(.library_test_dir(id),"expected.json"))
  expect_setequal(names(frames),names(expected$components))
  for (component in names(expected$components)) {
    rows <- expected$components[[component]]
    cols <- names(rows[[1L]])
    want <- as.data.frame(setNames(lapply(cols,function(col) vapply(rows,function(row)
      if (is.null(row[[col]])) NA_character_ else as.character(row[[col]]),"")),cols),stringsAsFactors=FALSE)
    got <- .library_oracle_frame(frames[[component]])[cols]
    sorted <- function(x) {
      x <- x[do.call(order,c(unname(x),list(na.last=TRUE,method="radix"))),,drop=FALSE]
      attributes(x) <- list(names=names(x),row.names=seq_len(nrow(x)),class="data.frame")
      x
    }
    expect_identical(sorted(got),sorted(want),info=paste(id,component))
  }
}

.library_check_schema <- function(id, component, frame) {
  columns <- attr(frame,"omop_column_contract")
  expect_named(columns,names(frame))
  schema <- list(columns = lapply(columns, function(x) list(type=x$type,
    nullable=x$nullable,role=x$role)),
    protection = list(person_id="existing pseudonymisation",row_id="native episode key",
      cohort_row_id="native episode key",set_id=if(id=="matching.nested_case_control") "native matched-set key" else NULL))
  file <- file.path(.library_test_dir(id),"schema.json")
  if (Sys.getenv("DSOMOP_LIBRARY_RECORD_SCHEMA") == "true") {
    existing <- if (file.exists(file)) .library_test_json(file) else list()
    existing[[component]] <- schema
    writeLines(as.character(jsonlite::toJSON(existing,auto_unbox=TRUE,pretty=TRUE,null="null",digits=NA)),file)
  }
  expect_true(file.exists(file),info=paste(id,"released schema fixture"))
  if (file.exists(file)) expect_identical(.library_test_canonical(schema),
    .library_test_canonical(.library_test_json(file)[[component]]),info=paste(id,component))
}
