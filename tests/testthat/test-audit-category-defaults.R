# Audit prerequisite composition: generated date endpoints and category defaults.
.audit_schema_books <- function(recipe, ctx, symbol, mode="memory") {
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

test_that("generated date endpoints apply effective category ordering in every transport", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="e")
  panel <- omop_output("panel",format="person_period",rows="p",columns=c("yesno","e_binary"),
    options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count")),
    category_columns=list(yesno=omop_category_column("e_binary",
      category_spec=omop_category_spec(binary=omop_category_binary("factor")))))
  survival <- omop_output("surv",format="survival",rows="p",columns="answer",
    options=list(format="recurrent_events",outcomes=list(lab=e),
      tars=list(early=omop_window(0,100),late=omop_window(0,365)),censor=list("observation_end")))
  answer <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"))
  wide <- omop_output("wide",rows="p",columns="answer")
  joined <- omop_output("joined",format="joined",rows="p",grain="episode",columns=c("copy","e_binary"),
    options=list(from=list(list(output="panel"),list(output="wide")),cardinality="many_to_one"),
    category_columns=list(copy=omop_category_column("e_binary")))
  for(default in list(NULL,omop_category_spec(ordered=FALSE))) for(mode in c("memory","staged")) {
    for(outputs in list(panel,survival,list(panel,wide,joined))) {
      r <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),
        variables=list(answer=answer),outputs=outputs,date_spec=omop_date_spec(precision="month"),category_spec=default)
      book <- .audit_schema_books(r,ctx,"DateCategoryDefaults",mode)
      for(component in book) {
        dates <- which(component$columns$semantic_type=="date")
        expect_true(all(component$columns$r_type[dates] == if(is.null(default))"ordered_factor" else "factor"))
        for(i in dates) {
          expect_identical(component$columns$ordered[[i]],is.null(default))
          expect_identical(component$columns$contrasts[[i]]$kind,if(is.null(default))"polynomial" else "treatment")
        }
      }
    }
  }
})
