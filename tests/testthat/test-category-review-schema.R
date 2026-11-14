# R6/R7: public original inventories and final projection for panel/survival families.
.schema_books <- function(recipe, ctx, symbol, mode="memory") {
  plan <- recipe_to_plan(recipe,version=2)
  symbol <- paste0(symbol,"_",mode,"_",substr(.planNormalizedDigest(plan)$sha256,1L,8L))
  symbols <- setNames(paste0(symbol,"_",names(plan$outputs)),names(plan$outputs))
  result <- ds.omop.plan.execute(plan,out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode)
  books <- local({
    testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
      datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
    list(prospective=recipe_codebook(recipe),published=recipe_codebook(result))
  })
  expect_identical(names(books$prospective),names(books$published))
  for (key in names(books$prospective)) {
    expected <- books$prospective[[key]]$columns$name
    expect_identical(expected,books$published[[key]]$columns$name,info=key)
    parts <- strsplit(key,"/",fixed=TRUE)[[1L]]
    target <- paste0(symbols[[parts[[1L]]]],if(parts[[2L]]!=parts[[1L]])paste0(".",parts[[2L]]))
    dates <- books$prospective[[key]]$columns$semantic_type == "date"
    date_keys <- expected[dates]
    date_types <- books$prospective[[key]]$columns$r_type[dates]
    expect_identical(date_types, books$published[[key]]$columns$r_type[match(date_keys, books$published[[key]]$columns$name)], info=key)
    for (frame in .clf_frames(ctx,target)) {
      expect_identical(expected,names(frame),info=key)
      actual_types <- vapply(frame[date_keys],function(x) {
        if(inherits(x,"Date")) "Date" else if(is.ordered(x)) "ordered_factor" else if(is.factor(x)) "factor" else typeof(x)
      }, "")
      expect_identical(date_types,unname(actual_types),info=key)
    }
  }
  books$prospective
}

.schema_recipe <- function(e, outputs, variables=NULL, date_spec=NULL) {
  omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),
    variables=variables,outputs=outputs,date_spec=date_spec)
}

test_that("R6 S1 / R7 D1 panel generated originals and copy-only selections agree locally", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  for (date in list(NULL,omop_date_spec(precision="month"))) for (mode in c("memory","staged")) for (selected in list(c("yesno","e_binary"),"yesno",c("e_count","yesno","e_binary"))) {
    output <- omop_output("panel",format="person_period",rows="p",columns=selected,
      options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count")),
      category_columns=list(yesno=omop_category_column("e_binary",
        category_spec=omop_category_spec(binary=omop_category_binary("factor")))))
    book <- .schema_books(.schema_recipe(e,output,date_spec=date),ctx,"Panel",mode)
    expect_identical(book[["panel/panel"]]$columns$name,c("row_id","cohort_row_id","person_id",if(!is.null(date))c("period_start_date","period_end_date"),selected))
  }
})

test_that("R6 S2 / R7 D2 survival mode copies obey required roles across every patient component", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  answer <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  for (style in c("survival","competing_risk","counting_process","recurrent_events","multi_state")) {
    opts <- list(format=style,outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end"))
    components <- if(style=="recurrent_events")c("events","risk_sets") else "surv"
    if(style=="multi_state") {
      opts$transitions <- list(list(from="index",to="lab")); opts$initial_state <- "index"
    }
    copies <- setNames(lapply(components,function(component)omop_category_column("answer",component=component)),paste0("copy",seq_along(components)))
    # Preserve the exact S2 declaration for the single primary component.
    if(length(components)==1L)copies <- list(copy=omop_category_column("answer",component=if(style!="survival")components[[1L]]))
    for(date in list(NULL,omop_date_spec(precision="month"))) for(mode in c("memory","staged")) for(original in c(TRUE,FALSE)) {
      selected <- c(names(copies),if(original)"answer")
      output <- omop_output("surv",format="survival",rows="p",columns=selected,options=opts,category_columns=copies)
      book <- .schema_books(.schema_recipe(e,output,list(answer=answer),date),ctx,"Survival",mode)
      for(i in seq_along(components)) {
        structural <- c("row_id","cohort_row_id","person_id",switch(style,
          recurrent_events=if(components[[i]]=="events")c("outcome_name","event","event_number","outcome_event_number"),
          multi_state=c("from","to","trans","status","from_name","to_name","state_visit_number"),
          counting_process=c("outcome_name","event","interval_number"),c("outcome_name","event")))
        if(!is.null(date)) structural <- c(structural,switch(style,
          multi_state=character(),counting_process=c("interval_start_date","interval_end_date"),
          recurrent_events=c("entry_date","exit_date",if(components[[i]]=="events")"event_date"),
          c("entry_date","exit_date")))
        expect_identical(book[[paste("surv",components[[i]],sep="/")]]$columns$name,
          c(structural,names(copies)[[i]],if(original)"answer"))
      }
      if(style=="multi_state")expect_false(any(c("copy","answer") %in% book[["surv/transitionRef"]]$columns$name))
    }
  }
})

test_that("R6 multi-TAR and joined dependents retain each component's final schema", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  answer <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  components <- c("early_events","early_risk_sets","late_events","late_risk_sets")
  copies <- setNames(lapply(components,function(component)omop_category_column("answer",component=component)),paste0("copy",seq_along(components)))
  output <- omop_output("surv",format="survival",rows="p",columns=c(names(copies),"answer"),
    options=list(format="recurrent_events",outcomes=list(lab=e),
      tars=list(early=omop_window(0,100),late=omop_window(0,365)),censor=list("observation_end")),category_columns=copies)
  for(date in list(NULL,omop_date_spec(precision="month"))) for(mode in c("memory","staged"))
    .schema_books(.schema_recipe(e,output,list(answer=answer),date),ctx,"MultiTar",mode)
  panel <- omop_output("panel",format="person_period",rows="p",columns=c("yesno","e_binary"),
    options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count")),
    category_columns=list(yesno=omop_category_column("e_binary",category_spec=omop_category_spec(binary=omop_category_binary("factor")))))
  wide <- omop_output("wide",rows="p",columns="answer")
  joined <- omop_output("joined",format="joined",rows="p",grain="episode",columns=c("copy","e_binary"),
    options=list(from=list(list(output="panel"),list(output="wide")),cardinality="many_to_one"),
    category_columns=list(copy=omop_category_column("e_binary")))
  for(date in list(NULL,omop_date_spec(precision="month"))) for(mode in c("memory","staged"))
    .schema_books(.schema_recipe(e,list(panel,wide,joined),list(answer=answer),date),ctx,"JoinedPanel",mode)
})

test_that("R6 unprojected panels enumerate closed source analyses and generated measurements", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",c(45000120L,45000121L),name="e")
  output <- omop_output("panel",format="person_period",rows="p",columns=character(),
    options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("count","binary","mean"),
      source_analyses=list(e=c("binary","count")),outcomes=list(lab=e),exposures=list(active=omop_era(e,end="start",fallback="none")),
      carry_forward=list(e=list(lookback=30)),partition=list(age_bands=10,calendar="month")))
  recipe <- .schema_recipe(e,output)
  recipe$options$translate_concepts <- FALSE
  book <- .schema_books(recipe,ctx,"FullPanel")
  expect_true(all(c("period_start_days","period_end_days","days","age","age_band","calendar_month",
    "exposed_active","active_active","n_lab","e_45000120_binary","e_45000121_count","e_45000121_carry") %in% book[[1L]]$columns$name))
})

test_that("R6 panel public inventories handle closed exclusions and inactive analyses", {
  plan <- list(events=list(e=list(table="condition_occurrence",concept_set="codes")),
    concept_sets=list(codes=list(concepts=list(1L,2L,3L),exclude=list(concepts=list(2L)))))
  node <- list(options=list(sources=list(list(event="e")),analyses=list("count","binary","mean")))
  expect_identical(names(.codebookPanelColumns(plan,node)),c("period_start_days","period_end_days","days",
    "e_1_binary","e_1_count","e_3_binary","e_3_count"))
  node$options$analyses <- list("mean")
  expect_identical(names(.codebookPanelColumns(plan,node)),c("period_start_days","period_end_days","days"))
  node$options$sources <- list(list(interval="i")); node$options$analyses <- list("exposed_days","binary")
  expect_identical(names(.codebookPanelColumns(plan,node)),c("period_start_days","period_end_days","days","i_binary","i_exposed_days"))
})


test_that("R7 endpoint presentation keeps declared types and append order before projection", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  answer <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  for(date in list(omop_date_spec(precision="day"),omop_date_spec(precision="month"),
                  omop_date_spec(precision="year",representation="number"),
                  omop_date_spec(precision="relative_days",anchor="index"))) {
    for(copies in c(FALSE,TRUE)) {
      output <- omop_output("surv",format="survival",rows="p",columns=if(copies)c("copy","answer") else "answer",
        options=list(outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end")),
        category_columns=if(copies)list(copy=omop_category_column("answer")))
      for(mode in c("memory","staged")) {
        book <- .schema_books(.schema_recipe(e,output,list(answer=answer),date),ctx,"DateTypes",mode)
        expected <- c("row_id","cohort_row_id","person_id","outcome_name","event",
          if(!copies)c("entry_days_from_index","exit_days_from_index","follow_up_days","answer"),
          "entry_date","exit_date",if(copies)c("copy","answer"))
        expect_identical(book[[1L]]$columns$name,expected)
      }
    }
  }
})
