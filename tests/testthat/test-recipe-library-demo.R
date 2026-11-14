test_that("every template takes the public DSLite path on the read-only demo CDM", {
  skip_if(Sys.getenv("DSOMOP_TEST_LIBRARY_DEMO") != "true", "Opt in to the shared demo CDM")
  base <- withr::local_tempdir(); Sys.chmod(base,"0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR=base,DSOMOP_TEST_ALLOW_EPHEMERAL_STATE="1",
    DSOMOP_PSEUDONYM_ROOT="",DSOMOP_PSEUDONYM_PROVIDER="file",
    DSOMOP_PSEUDONYM_REQUIRE_EXISTING="false",DSOMOP_PSEUDONYM_KEY=""))
  withr::local_options(list(dsstaging.base_dir=base))
  resource <- resourcer::newResource(name="library",format="omop.dbi.db",
    url="omop+dbi:postgresql://omopdb:5432/omop?cdm_schema=demo_cdm&vocabulary_schema=demo_cdm&results_schema=demo_cdm",
    identity="omop",secret="omop")
  ctx <- .library_connect(resource,base,environment())
  session <- dsOMOPClient:::.get_session(ctx$symbol)
  server <- ctx$servers[[1L]]
  handle <- server$getSessionData(server$getSessionIds()[[1L]],paste0(".dsomop_handle_",session$res_symbol))
  demo_tables <- c("person","observation_period","visit_occurrence","condition_occurrence",
    "drug_exposure","measurement","observation","death","concept","concept_ancestor","concept_relationship","cdm_source")
  checksums <- setNames(vapply(demo_tables,function(tab) paste(openssl::sha256(charToRaw(
    jsonlite::toJSON(DBI::dbReadTable(handle$conn,DBI::Id(schema="demo_cdm",table=tab)),na="null",digits=NA)))),""),demo_tables)
  cat("\nLIBRARY_DEMO_CHECKSUMS",jsonlite::toJSON(as.list(checksums),auto_unbox=TRUE),"\n")
  ids <- recipe_library_list()$id
  bindings <- list(index=omop_event("visit_occurrence",9201,name="entry"),
    education=4005823L,employment=4005823L,conditions=201820L,
    drug=c(1124300L,1154029L),drug_a=1124300L,drug_b=1154029L,
    measure=omop_event("measurement",3025315,value="value_as_number",name="measure"),
    outcome=omop_event("condition_occurrence",201820,name="outcome"),
    outcome_a=omop_event("condition_occurrence",201820,name="cause_a"),
    outcome_b=omop_event("condition_occurrence",255573,name="cause_b"),
    exposure=omop_event("drug_exposure",c(1124300,1154029),name="exposure"),
    source_a=omop_event("measurement",3025315,name="measure"),
    source_b=omop_event("condition_occurrence",201820,name="history"))
  successes <- character(); refusals <- character()
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    required <- recipe_library_list()$required_parameters[[i]]
    r <- do.call(recipe_library_instantiate,c(list(id),bindings[required]))
    before <- unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE)
    status <- tryCatch({
      recipe_execute(r,out=setNames(paste0("D_demo_",i,"_",r$publish),r$publish),symbol=ctx$symbol,conns=ctx$conns)
      "published"
    },error=function(e) conditionMessage(e))
    cat("\nLIBRARY_DEMO",id,status,"\n")
    if (status == "published") successes <- c(successes,id) else {
      if (id %in% c("drug.adherence_components","drug.era_summary")) {
        # The shared demo predates the days_supply role. Do not alter shared
        # data or silently drop the requested supply/fallback semantics.
        expect_match(status,"validation_error")
        expect_false("days_supply" %in% DBI::dbListFields(handle$conn,DBI::Id(schema="demo_cdm",table="drug_exposure")))
      } else if (id %in% c("panel.incidence","panel.sccs")) {
        expect_match(status,"factor-level disclosure policy|insufficient_support")
      } else expect_match(status,"insufficient_support")
      expect_setequal(unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE),before)
      refusals <- c(refusals,id)
    }
  }
  expect_gte(length(successes),1L)
  expect_equal(length(successes)+length(refusals),24L)
})
