test_that("two sites retain the same library schema with reordered and empty event sources", {
  ctx <- .library_cdm_setup(two_sites=TRUE)
  r <- .library_test_recipe("socioeconomic.education")
  recipe_execute(r,out=c(education="D_edu_fed"),symbol=ctx$symbol,conns=ctx$conns)
  frames <- .library_frames(ctx,"D_edu_fed")
  expect_identical(names(frames$site1),names(frames$site2))
  expect_equal(frames$site1$education_records,rep(2L,24))
  expect_equal(frames$site2$education_records,rep(0L,24))
  expect_true(all(is.na(frames$site1$education_concept_id)))
  expect_true(all(is.na(frames$site2$education_concept_id)))
})

test_that("library final support failure rolls back every site; explicit skip is retained", {
  ctx <- .library_cdm_setup(two_sites=TRUE,second_site="tiny_roster")
  r <- .library_test_recipe("demographics.baseline")
  before <- DSI::datashield.symbols(ctx$conns)
  expect_error(recipe_execute(r,out=c(baseline="D_small"),symbol=ctx$symbol,conns=ctx$conns),"insufficient_support")
  expect_equal(DSI::datashield.symbols(ctx$conns),before)
  result <- recipe_execute(r,out=c(baseline="D_skip"),symbol=ctx$symbol,conns=ctx$conns,on_small_site="skip")
  expect_null(result$site2)
  expect_true("D_skip" %in% DSI::datashield.symbols(ctx$conns)$site1)
  expect_false("D_skip" %in% DSI::datashield.symbols(ctx$conns)$site2)
})

test_that("a missing required event table is never a small-site skip", {
  ctx <- .library_cdm_setup(two_sites=TRUE,second_site="missing_measurement")
  r <- .library_test_recipe("panel.measurements")
  before <- DSI::datashield.symbols(ctx$conns)
  expect_error(recipe_execute(r,out=c(measurements="D_missing"),symbol=ctx$symbol,conns=ctx$conns,on_small_site="skip"))
  expect_equal(DSI::datashield.symbols(ctx$conns),before)
})

test_that("no-event features keep final support and an empty event-log root refuses", {
  ctx <- .library_cdm_setup()
  r <- recipe_library_instantiate("comorbidity.condition_history",
    index=omop_event("visit_occurrence",9201),conditions=numeric())
  recipe_execute(r,out=c(history="D_empty_history"),symbol=ctx$symbol,conns=ctx$conns)
  f <- .library_frames(ctx,"D_empty_history")[[1L]]
  expect_equal(nrow(f),24L);expect_equal(f$history_present,rep(0L,24))
  r <- recipe_library_instantiate("longitudinal.event_log",index=omop_event("visit_occurrence",9201),
    source_a=omop_event("measurement",numeric(),name="measure"),
    source_b=omop_event("condition_occurrence",numeric(),name="history"))
  expect_error(recipe_execute(r,out=c(event_log="D_empty_events"),symbol=ctx$symbol,conns=ctx$conns),"insufficient_support")
  empty <- recipe_set_publish(.library_test_recipe("demographics.baseline"),character())
  before <- DSI::datashield.symbols(ctx$conns)
  recipe_execute(empty,symbol=ctx$symbol,conns=ctx$conns)
  expect_equal(DSI::datashield.symbols(ctx$conns),before)
})

test_that("risk-set sampling retains seed sensitivity, row-order reproducibility and cap refusal", {
  ctx <- .library_cdm_setup(two_sites=TRUE)
  r <- .library_test_recipe("matching.nested_case_control")
  recipe_execute(r,out=c(matched="D_match_one"),symbol=ctx$symbol,conns=ctx$conns)
  frames <- .library_frames(ctx,"D_match_one")
  membership <- function(frame,server) {
    session <- dsOMOPClient:::.get_session(ctx$symbol)
    handle <- server$getSessionData(server$getSessionIds()[[1L]],paste0(".dsomop_handle_",session$res_symbol))
    tokens <- dsOMOP:::.pseudonymizeIdentifiers(data.frame(person_id=1:24),
      dsOMOP:::.personKey(handle),dsOMOP:::.personKeyPublicContract(handle))$person_id
    person <- match(frame$person_id,tokens)
    cases <- setNames(person[frame$is_case==1],as.character(frame$set_id[frame$is_case==1]))
    sort(paste(cases[as.character(frame$set_id)],person,frame$is_case))
  }
  controls <- frames$site1[frames$site1$is_case == 0L, , drop = FALSE]
  expect_true(anyDuplicated(controls$person_id) > 0L)
  expect_equal(length(unique(controls$cohort_row_id)), nrow(controls))
  first <- membership(frames$site1,ctx$servers$site1)
  expect_identical(first,membership(frames$site2,ctx$servers$site2))
  r2 <- recipe_bind(r,seed=2L)
  recipe_execute(r2,out=c(matched="D_match_two"),symbol=ctx$symbol,conns=ctx$conns)
  second <- .library_frames(ctx,"D_match_two")
  expect_false(identical(first,membership(second$site1,ctx$servers$site1)))
  r$populations$matched$match$max_candidates <- 1L
  expect_error(recipe_execute(r,out=c(matched="D_match_cap"),symbol=ctx$symbol,conns=ctx$conns),"candidate|validation_error")
  expect_false(any(vapply(DSI::datashield.symbols(ctx$conns),function(x) "D_match_cap" %in% x,logical(1))))
})

test_that("the five score templates retain indexed weights, duplicates and future exclusion", {
  ctx <- .library_cdm_setup()
  session <- dsOMOPClient:::.get_session(ctx$symbol)
  server <- ctx$servers[[1L]]
  handle <- server$getSessionData(server$getSessionIds()[[1L]],paste0(".dsomop_handle_",session$res_symbol))
  DBI::dbExecute(handle$conn,"UPDATE person SET year_of_birth=1940, gender_concept_id=8532")
  # Independently pinned contributions: diabetes=1; CHADS2 age>=75=1;
  # CHA2DS2-VASc age>=75=2 and female=1; DCSI retinopathy=1 + CKD=2;
  # HFRS F05->373995 delirium weight=3.2. Duplicate rows never double score.
  scenarios <- list(charlson=list(concepts=201820L,score=1),
    chads2=list(concepts=201820L,score=2),chadsvasc=list(concepts=201820L,score=4),
    dcsi=list(concepts=c(4174977L,443611L),score=3),hfrs=list(concepts=373995L,score=3.2))
  for (kind in names(scenarios)) {
    DBI::dbExecute(handle$conn,"DELETE FROM condition_occurrence")
    concepts <- rep(scenarios[[kind]]$concepts,each=2L)
    rows <- do.call(rbind,lapply(seq_along(concepts),function(i) data.frame(
      condition_occurrence_id=(1:24)+(i-1L)*24L,person_id=1:24,condition_concept_id=concepts[[i]],
      condition_start_date="2020-01-01",condition_end_date="2020-01-01",condition_type_concept_id=0L)))
    DBI::dbWriteTable(handle$conn,"condition_occurrence",rows,append=TRUE)
    r <- .library_test_recipe(paste0("comorbidity.",kind))
    symbol <- paste0("D_score_",kind)
    recipe_execute(r,out=c(score=symbol),symbol=ctx$symbol,conns=ctx$conns)
    expect_equal(.library_frames(ctx,symbol)[[1L]][[kind]],rep(scenarios[[kind]]$score,24))
    DBI::dbExecute(handle$conn,"UPDATE condition_occurrence SET condition_start_date='2020-01-02', condition_end_date='2020-01-02'")
    recipe_execute(r,out=c(score=paste0(symbol,"_future")),symbol=ctx$symbol,conns=ctx$conns)
    expected <- switch(kind,chads2=1,chadsvasc=3,0)
    expect_equal(.library_frames(ctx,paste0(symbol,"_future"))[[1L]][[kind]],rep(expected,24))
  }
})
