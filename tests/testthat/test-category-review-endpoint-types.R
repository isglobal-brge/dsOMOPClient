# R8: generated date factors consume recipe category defaults without a variable.
.endpoint_books <- function(recipe, ctx, symbol, mode, ordered, public_domain=FALSE, whole_schema=TRUE, schema_types=FALSE) {
  plan <- recipe_to_plan(recipe,version=2)
  symbols <- setNames(paste0(symbol,"_",names(plan$outputs)),names(plan$outputs))
  result <- ds.omop.plan.execute(plan,out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode)
  books <- local({
    testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
      datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
    list(prospective=recipe_codebook(recipe),published=recipe_codebook(result))
  })
  expect_identical(names(books$prospective),names(books$published))
  checked <- 0L
  for (key in names(books$prospective)) {
    a <- books$prospective[[key]]$columns; b <- books$published[[key]]$columns
    published_names <- b$name
    if (whole_schema) expect_identical(a$name,b$name,info=key)
    else {
      # Long source-specific columns remain open prospectively. Its declared
      # dates must still match, and every actual column must match publication.
      expect_true(all(a$name %in% b$name),info=key)
      b <- b[match(a$name,b$name),,drop=FALSE]
    }
    if (schema_types) expect_identical(a$r_type,b$r_type,info=key)
    dates <- which(a$semantic_type=="date")
    checked <- checked+length(dates)
    parts <- strsplit(key,"/",fixed=TRUE)[[1L]]
    target <- paste0(symbols[[parts[[1L]]]],if(parts[[2L]]!=parts[[1L]])paste0(".",parts[[2L]]))
    for (i in dates) {
      expect_identical(a$r_type[[i]],if(ordered)"ordered_factor" else "factor",info=key)
      expect_identical(a$r_type[[i]],b$r_type[[i]],info=key)
      expect_identical(a$ordered[[i]],ordered,info=key)
      expect_identical(a$contrasts[[i]]$kind,if(ordered)"polynomial" else "treatment",info=key)
      # Legacy date-only metadata omits ordered/contrasts. Category publication
      # declares them; its open period domain remains unresolved prospectively.
      if (!is.null(recipe$options$category_spec)) {
        expect_identical(a$ordered[[i]],b$ordered[[i]],info=key)
        expect_identical(a$contrasts[[i]]$kind,b$contrasts[[i]]$kind,info=key)
      }
      if (public_domain) {
        for (field in c("keys","labels"))
          expect_identical(unlist(a$levels[[i]][[field]],use.names=FALSE),
            unlist(b$levels[[i]][[field]],use.names=FALSE),info=paste(key,field))
        expect_identical(a$reference[[i]],b$reference[[i]],info=key)
        expect_identical(a$contrasts[[i]],b$contrasts[[i]],info=key)
      } else expect_null(a$levels[[i]],info=key)
    }
    for (site in names(ctx$servers)) {
      server <- ctx$servers[[site]]
      value <- server$getSessionData(server$getSessionIds()[[1L]],target)
      if (mode=="staged") {
        expect_s3_class(value,"FlowerDatasetDescriptor")
        expect_identical(value$metadata$format,"parquet")
        # Inspect the publication reader's chunks directly. rbind.data.frame
        # discards factor contrast attributes even when binding a single chunk.
        frames <- list()
        dsOMOP:::.publishReadChunks(value,function(frame)frames[[length(frames)+1L]] <<- frame)
      } else frames <- list(value)
      expect_gt(length(frames),0L)
      for (frame in frames) {
        expect_identical(names(frame),published_names,info=paste(key,site))
        if (schema_types) {
          types <- vapply(frame,function(column) {
            if (inherits(column,"integer64")) "integer64" else if (inherits(column,"Date")) "Date" else
              if (is.ordered(column)) "ordered_factor" else if (is.factor(column)) "factor" else typeof(column)
          },"")
          expect_identical(unname(types),a$r_type,info=paste(key,site))
        }
        for (i in dates) {
          column <- frame[[a$name[[i]]]]
          expect_true(is.factor(column),info=paste(key,site))
          expect_identical(is.ordered(column),ordered,info=paste(key,site))
          expect_identical(levels(column),unlist(b$levels[[i]]$labels,use.names=FALSE),info=paste(key,site))
          if (public_domain) {
            keys <- unlist(a$levels[[i]]$keys,use.names=FALSE)
            expected <- if(ordered)stats::contr.poly(length(keys)) else
              stats::contr.treatment(length(keys),base=match(a$reference[[i]],keys))
            expect_equal(unname(stats::contrasts(column)),unname(expected),info=paste(key,site))
          }
        }
      }
    }
  }
  expect_gt(checked,0L)
  books$prospective
}

.endpoint_panel <- function(e) omop_output("panel",format="person_period",rows="p",columns=c("yesno","e_binary"),
  options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count")),
  category_columns=list(yesno=omop_category_column("e_binary",
    category_spec=omop_category_spec(binary=omop_category_binary("factor")))))

test_that("R8 exact D1 unordered defaults and date-only controls match both transports", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  for (mode in c("memory","staged")) for (unordered in c(FALSE,TRUE)) {
    recipe <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),
      outputs=.endpoint_panel(e),date_spec=omop_date_spec(precision="month"),
      category_spec=if(unordered)omop_category_spec(ordered=FALSE))
    book <- .endpoint_books(recipe,ctx,paste0("Endpoint_",mode,"_",unordered),mode,!unordered)
    expect_identical(book[[1L]]$columns$name,c("row_id","cohort_row_id","person_id",
      "period_start_date","period_end_date","yesno","e_binary"))
  }
})

test_that("R8 nested survival and joined dependents inherit endpoint factor contracts", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  answer <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  components <- c("early_events","early_risk_sets","late_events","late_risk_sets")
  copies <- setNames(lapply(components,function(component)omop_category_column("answer",component=component)),paste0("copy",seq_along(components)))
  survival <- omop_output("surv",format="survival",rows="p",columns=c(names(copies),"answer"),
    options=list(format="recurrent_events",outcomes=list(lab=e),
      tars=list(early=omop_window(0,100),late=omop_window(0,365)),censor=list("observation_end")),category_columns=copies)
  wide <- omop_output("wide",rows="p",columns="answer")
  joined <- omop_output("joined",format="joined",rows="p",grain="episode",columns=c("copy","e_binary"),
    options=list(from=list(list(output="panel"),list(output="wide")),cardinality="many_to_one"),
    category_columns=list(copy=omop_category_column("e_binary")))
  for (mode in c("memory","staged")) for (unordered in c(FALSE,TRUE)) {
    for (outputs in list(survival,list(.endpoint_panel(e),wide,joined))) {
      recipe <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),
        variables=list(answer=answer),outputs=outputs,date_spec=omop_date_spec(precision="month"),
        category_spec=if(unordered)omop_category_spec(ordered=FALSE))
      .endpoint_books(recipe,ctx,paste0("Dependent_",mode,"_",unordered),mode,!unordered)
    }
  }
})

test_that("R8 generated public date domains apply order, recode, reference and contrasts", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  keys <- paste0("period:2020-0",1:3)
  specs <- list(
    omop_category_spec(ordered=FALSE,order=omop_category_order(rev(keys)),reference=keys[[2L]]),
    omop_category_spec(recode=omop_category_recode(setNames(c("category:early","category:late","category:late"),keys),
      levels=c("category:early","category:late"),labels=c("Early","Late")),reference="category:late"),
    omop_category_spec(ordered=TRUE,recode=omop_category_recode(setNames(c("category:early","category:late","category:late"),keys),
      levels=c("category:early","category:late"),labels=c("Early","Late")),reference="category:late"))
  # No factor copies here: these defaults describe the generated date domain.
  output <- omop_output("panel",format="person_period",rows="p",columns=character(),
    options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses="count"))
  for (mode in c("memory","staged")) for (i in seq_along(specs)) {
    recipe <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),outputs=output,
      date_spec=omop_date_spec(precision="month",level_range=c("2020-01-01","2020-03-31")),category_spec=specs[[i]])
    .endpoint_books(recipe,ctx,paste0("PublicEndpoint_",mode,"_",i),mode,i==3L,public_domain=TRUE)
  }
})


test_that("R9 R10 membership and long dates consume defaults in both transports", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  for (fmt in c("membership","long")) for (mode in c("memory","staged")) for (unordered in c(FALSE,TRUE)) {
    recipe <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),
      outputs=omop_output("panel",format=fmt,rows="p",sources=if(fmt=="long")list(e),columns=character()),
      date_spec=omop_date_spec(precision="month"),
      category_spec=if(unordered)omop_category_spec(ordered=FALSE))
    book <- .endpoint_books(recipe,ctx,paste0("Existing_",fmt,"_",mode,"_",unordered),mode,!unordered,
      whole_schema=fmt=="membership",schema_types=fmt=="membership")
    if (fmt=="membership") expect_identical(book[[1L]]$columns$name,
      c("row_id","cohort_row_id","person_id","cohort_start_date","cohort_end_date","index_source"))
  }
})

test_that("R9 existing date domains apply public order recode and inactive defaults", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  keys <- paste0("period:2020-0",1:3)
  specs <- list(
    omop_category_spec(ordered=FALSE,order=omop_category_order(rev(keys)),reference=keys[[2L]]),
    omop_category_spec(recode=omop_category_recode(setNames(c("category:early","category:late","category:late"),keys),
      levels=c("category:early","category:late"),labels=c("Early","Late")),reference="category:late"),
    omop_category_spec(ordered=TRUE,recode=omop_category_recode(setNames(c("category:early","category:late","category:late"),keys),
      levels=c("category:early","category:late"),labels=c("Early","Late")),reference="category:late"),
    omop_category_spec(ordered=FALSE,label_style="code",language=omop_category_language(4180186L),
      rollup=omop_category_rollup("snomed",separation=0L,domain="concept:45000124",multiple="all"),
      binary=omop_category_binary("logical")))
  for (fmt in c("membership","long")) for (mode in c("memory","staged")) for (i in seq_along(specs)) {
    recipe <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e,end=omop_end("fixed",days=20))),
      outputs=omop_output("panel",format=fmt,rows="p",sources=if(fmt=="long")list(e),columns=character()),
      date_spec=omop_date_spec(precision="month",level_range=c("2020-01-01","2020-03-31")),category_spec=specs[[i]])
    # Period order/recode defaults describe dates. Keep long's concept IDs
    # numeric so those declarations do not apply to a second, unrelated domain.
    if (fmt=="long") recipe$options$translate_concepts <- FALSE
    book <- .endpoint_books(recipe,ctx,paste0("PublicExisting_",fmt,"_",mode,"_",i),mode,i==3L,
      public_domain=TRUE,whole_schema=fmt=="membership",schema_types=fmt=="membership")
    dates <- book[[1L]]$columns
    for (column in dates$derivation[dates$semantic_type=="date"]) {
      expect_false(any(c("label_style","language","rollup","binary") %in% names(column$category_spec)))
    }
  }
})

test_that("R10 membership provenance follows native rosters and person collapse", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  populations <- list(omop_population("p",index=omop_index(e)),
    omop_population("child",parent_id="p"),omop_population("people"),
    omop_population("cohort",index=omop_index(list(cohort_definition_id=1L))),
    omop_population("combined",union=c("p","child")))
  for (mode in c("memory","staged")) for (grain in c("episode","person")) {
    outputs <- lapply(populations,function(p)omop_output(p$id,format="membership",rows=p$id,
      grain=if(p$id=="people")"person" else grain,columns=character(),
      collapse=if(grain=="person" && p$id!="people")"first_episode"))
    recipe <- omop_recipe(events=list(e),populations=populations,outputs=outputs,
      date_spec=omop_date_spec(precision="month"))
    book <- .endpoint_books(recipe,ctx,paste0("Roster_",mode,"_",grain),mode,TRUE,schema_types=TRUE)
    for (table in book) expect_identical(table$columns$name,c(if(table$row_grain=="episode")c("row_id","cohort_row_id"),
      "person_id","cohort_start_date","cohort_end_date","index_source"))
  }
})
