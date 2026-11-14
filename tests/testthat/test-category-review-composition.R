# Review R4/R5: output-local composition of declared schemas and reduced cells.
.composition_books <- function(recipe, ctx, symbol, mode="memory") {
  result <- recipe_execute(recipe,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
  testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
    datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
  list(prospective=recipe_codebook(recipe)[[1L]]$columns,
    published=recipe_codebook(result)[[1L]]$columns)
}

test_that("R4 final selection composes copies with ordinary and expanded columns locally", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L)
  count <- omop_variable(event=e,reducer=omop_reduce("count"))
  variables <- list(band=omop_number_bins(count,breaks=c(0,2,10)), n=count,
    answer=omop_variable(event=e,reducer=omop_reduce("nth_value",value="value_as_concept_id",n=c(1L,3L))))
  cases <- list(excluded="band",only="nominal",ordinary=c("nominal","band"),
    expanded=c("copy","answer_3"),interleaved=c("answer_3","nominal","n","copy","answer_1"),
    whole=c("copy","answer","nominal"))
  for (key in names(cases)) {
    selected <- cases[[key]]
    r <- omop_recipe(variables=variables,outputs=omop_output("w",format="wide",columns=selected,
      number_columns=list(ncopy=omop_number_column("n")),
      category_columns=list(nominal=omop_category_column("band",category_spec=omop_category_spec(ordered=FALSE)),
        copy=omop_category_column("answer_1"))))
    books <- .composition_books(r,ctx,paste0("Composition_",key))
    expected <- c("person_id",unlist(lapply(selected,function(k)if(k=="answer")c("answer_1","answer_3") else k)))
    expect_identical(books$prospective$name,expected,info=key)
    expect_identical(books$prospective$name,books$published$name,info=key)
  }
  r$outputs$w$columns <- c("ncopy","nominal","n")
  books <- .composition_books(r,ctx,"Composition_numeric")
  expect_identical(books$prospective$name,c("person_id","ncopy","nominal","n"))
  expect_identical(books$prospective$name,books$published$name)
})

test_that("R5 mode and vector nth copies preserve consumed identity across sites and storage", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L)
  spec <- omop_category_spec(recode=omop_category_recode(
    c(`concept:45000123`="category:first",`concept:45000124`="category:second",`concept:45000125`="category:third"),
    c("category:first","category:second","category:third"),labels=c("First","Second","Third")),
    missing=omop_category_missing("level","level"))
  rows <- omop_category_rows(e,"value_as_concept_id",spec)
  for (fn in c("mode","nth_value")) for (mode in c("memory","staged")) {
    reducer <- if(fn=="mode")omop_reduce(fn) else omop_reduce(fn,n=c(1L,3L))
    v <- omop_category_reduce(rows,reducer)
    source <- if(fn=="mode")"answer" else "answer_1"
    copies <- list(copy=omop_category_column(source),
      styled=omop_category_column(source,category_spec=omop_category_spec(ordered=TRUE,
        order=omop_category_order(c("category:third","category:second","category:first","special:missing","special:unknown")))))
    if(fn=="nth_value")copies$absent <- omop_category_column("answer_3")
    r <- omop_recipe(variables=list(answer=v),outputs=omop_output("w",format="wide",category_columns=copies))
    symbol <- paste0("ReducedCopy_",fn,"_",mode)
    books <- .composition_books(r,ctx,symbol,mode)
    expect_identical(books$prospective$name,books$published$name)
    frames <- .clf_frames(ctx,symbol)
    for (f in frames) {
      expect_identical(f[[source]],f$copy)
      expect_identical(attr(f,"omop_concepts")[[source]],attr(f,"omop_concepts")$copy)
      expect_identical(as.character(f[[source]]),as.character(f$styled))
      expect_true(is.ordered(f$styled)); expect_false(is.ordered(f[[source]]))
      expect_identical(head(levels(f$styled),3L),c("Third","Second","First"))
      if(fn=="nth_value") {
        expect_identical(f$answer_3,f$absent)
        expect_identical(attr(f,"omop_concepts")$answer_3,attr(f,"omop_concepts")$absent)
        expect_true(all(is.na(f$absent)))
      }
    }
    expect_identical(levels(frames[[1L]]$copy),levels(frames[[3L]]$copy))
    expect_identical(attr(frames[[1L]]$copy,"omop_category_contract"),attr(frames[[3L]]$copy,"omop_category_contract"))
  }
})

test_that("R5 copies retain missing sentinels and upstream category admission", {
  ctx <- .clf_setup(); .numeric_lexer()
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_concept_id=CASE WHEN person_id<=6 THEN NULL WHEN person_id<=12 THEN 0 ELSE 45000124 END WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  rows <- omop_category_rows(omop_event("observation",45000120L),"value_as_concept_id",
    omop_category_spec(missing=omop_category_missing("level","level")))
  v <- omop_category_reduce(rows,omop_reduce("mode"))
  r <- omop_recipe(variables=list(answer=v),outputs=omop_output("w",category_columns=list(copy=omop_category_column("answer"))))
  .composition_books(r,ctx,"MissingReducedCopy")
  for (f in .clf_frames(ctx,"MissingReducedCopy")) {
    expect_identical(f$answer,f$copy)
    expect_equal(sum(as.character(f$copy)=="Missing"),6L)
    expect_equal(sum(as.character(f$copy)=="Unknown"),6L)
    expect_identical(attr(f,"omop_concepts")$answer,attr(f,"omop_concepts")$copy)
  }
  # A copy cannot bypass the reduced input's support gate by selecting only it.
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_concept_id=CASE WHEN person_id=1 THEN 45000123 ELSE 45000124 END WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  r$outputs$w$columns <- "copy"
  expect_error(recipe_execute(r,out="UnsafeReducedCopy",symbol="labels",conns=ctx$conns),"unsafe|Unsafe|disclosure|levels|admi")
})
