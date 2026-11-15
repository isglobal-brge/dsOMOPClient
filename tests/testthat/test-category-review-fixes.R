# Review R1-R3: public constructors, real native execution, and local metadata.
.category_review_books <- function(recipe, ctx, symbol) {
  plan <- recipe_to_plan(recipe, version=2)
  result <- ds.omop.plan.execute(plan,out=setNames(symbol,names(plan$outputs)),symbol="labels",conns=ctx$conns)
  testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
    datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
  list(prospective=recipe_codebook(recipe)[[1L]]$columns,published=recipe_codebook(result)[[1L]]$columns)
}

test_that("R1 prospective nth scalar, vector and projected columns match publication", {
  ctx <- .clf_setup(); .numeric_lexer()
  rows <- omop_category_rows(omop_event("observation",45000120L),"value_as_concept_id")
  cases <- list(scalar=list(n=3L,select=NULL,expected="answer"),
    vector=list(n=c(3L,1L),select=NULL,expected=c("answer_3","answer_1")),
    selected=list(n=c(1L,3L),select="answer_3",expected="answer_3"),
    reordered=list(n=c(1L,3L),select=c("answer_3","answer_1"),expected=c("answer_3","answer_1")))
  for (name in names(cases)) {
    case <- cases[[name]]
    r <- omop_recipe(variables=list(answer=omop_category_reduce(rows,omop_reduce("nth_value",n=case$n))),
      outputs=omop_output("wide",format="wide",columns=case$select))
    books <- .category_review_books(r,ctx,paste0("Nth_",name))
    expect_identical(books$prospective$name,c("person_id",case$expected),info=name)
    expect_identical(books$prospective$name,books$published$name,info=name)
    expect_identical(books$prospective$r_type,books$published$r_type,info=name)
  }
})

test_that("R2 effective numeric defaults, overrides and resets match publication", {
  ctx <- .clf_setup(); .numeric_lexer()
  v <- omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("count"))
  r <- omop_recipe(variables=list(inherited=v,
    override=omop_number(v,omop_number_spec(representation="bins",breaks=c(0,5,10),labels=c("low","high"))),
    reset=omop_number(v,omop_number_spec(representation="number"))),
    number_spec=omop_number_spec(representation="bins",breaks=c(0,2,10),labels=c("few","many")),
    category_spec=omop_category_spec(ordered=FALSE))
  books <- .category_review_books(r,ctx,"NumericDefaults")
  expect_identical(books$prospective$name,books$published$name)
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],c("factor","factor","integer"))
  for (key in c("inherited","override")) {
    i <- match(key,books$prospective$name); j <- match(key,books$published$name)
    expect_equal(unlist(books$prospective$levels[[i]]$keys),unlist(books$published$levels[[j]]$keys))
    expect_equal(unlist(books$prospective$levels[[i]]$labels),unlist(books$published$levels[[j]]$labels))
    expect_identical(books$prospective$ordered[[i]],books$published$ordered[[j]])
  }
  expect_null(books$prospective$levels[[match("reset",books$prospective$name)]])
  # Original review P4, without a category override.
  r <- omop_recipe(variables=list(n=v),number_spec=omop_number_spec(representation="bins",breaks=c(0,2,10)))
  books <- .category_review_books(r,ctx,"NumericBins")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],"ordered_factor")
  expect_equal(unlist(books$prospective$levels[[2]]$keys),unlist(books$published$levels[[2]]$keys))
  expect_equal(unlist(books$prospective$levels[[2]]$labels),unlist(books$published$levels[[2]]$labels))
  r <- omop_recipe(variables=list(inherited=v,reset=omop_number(v,omop_number_spec(rounding=omop_number_round("none")))),number_spec=omop_number_spec(rounding=omop_number_round("floor")))
  books <- .category_review_books(r,ctx,"NumericRounding")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],c("double","integer"))
})

test_that("R2 date and category defaults, overrides and resets describe public domains", {
  ctx <- .clf_setup(); .numeric_lexer()
  source <- omop_anchor(omop_event("observation",45000120L))
  r <- omop_recipe(variables=list(inherited=omop_date(source),
    start=omop_date(source,omop_date_spec(representation="start")),
    reset=omop_date(source,omop_date_spec(precision="day",representation="auto")),
    nominal=omop_category(omop_date(source),omop_category_spec(ordered=FALSE))),
    date_spec=omop_date_spec(precision="month",level_range=c("2020-01-01","2020-03-31")),
    category_spec=omop_category_spec(ordered=TRUE))
  books <- .category_review_books(r,ctx,"DateDefaults")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],c("ordered_factor","Date","Date","factor"))
  for (key in c("inherited","nominal")) {
    i <- match(key,books$prospective$name)
    expect_equal(unlist(books$prospective$levels[[i]]$keys),unlist(books$published$levels[[i]]$keys))
    expect_equal(unlist(books$prospective$levels[[i]]$labels),unlist(books$published$levels[[i]]$labels))
  }
  expect_true(all(books$prospective$completeness=="unresolved"))
})

test_that("R2 native untranslated concepts have no factor contract", {
  ctx <- .clf_setup(); .numeric_lexer()
  r <- omop_recipe(variables=list(gender=omop_variable(name="gender",table="person",column="gender_concept_id")),options=list(translate_concepts=FALSE))
  books <- .category_review_books(r,ctx,"Untranslated")
  i <- match("gender",books$prospective$name); j <- match("gender",books$published$name)
  expect_false(is.na(i)); expect_false(is.na(j))
  expect_identical(books$prospective$r_type[i],"integer")
  expect_identical(books$prospective$r_type[i],books$published$r_type[j])
  for (field in c("levels","ordered","reference","contrasts")) expect_null(books$prospective[[field]][[i]])
})

test_that("R3 hierarchy defaults are inactive for flags and numeric/date categories", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L)
  flag <- omop_variable(name="flag",event=e,reducer=omop_reduce("binary"))
  defaults <- omop_category_spec(rollup=omop_category_rollup("snomed",separation=0L,domain="concept:45000124",multiple="all"))
  for (as in c("default","integer","logical","factor")) {
    v <- if(as=="default")flag else omop_category(flag,omop_category_spec(binary=omop_category_binary(as)))
    r <- omop_recipe(variables=list(flag=v),category_spec=defaults)
    books <- .category_review_books(r,ctx,paste0("Inactive_",as))
    expect_identical(books$prospective$r_type,books$published$r_type)
    for (f in .clf_frames(ctx,paste0("Inactive_",as))) {
      expect_true(all(if(as=="factor")as.character(f$flag)=="yes" else f$flag==1L))
      expect_identical(is.factor(f$flag),as=="factor")
    }
  }
  count <- omop_variable(event=e,reducer=omop_reduce("count"))
  bins <- omop_number_bins(count,breaks=c(0,2,10))
  date <- omop_date(omop_anchor(e),omop_date_spec(precision="month"))
  r <- omop_recipe(variables=list(n=count,bins=bins,date=date),category_spec=defaults)
  books <- .category_review_books(r,ctx,"InactiveCategories")
  expect_identical(books$prospective$r_type,books$published$r_type)
  for (f in .clf_frames(ctx,"InactiveCategories")) {
    expect_true(all(f$n==1L))
    expect_true(all(as.character(f$bins)=="[0,2)"))
    expect_true(all(as.character(f$date)=="2020-01"))
  }
  # Even an unavailable vocabulary domain must not activate an irrelevant default.
  defaults$rollup$domain <- "concept:999999999"
  r <- omop_recipe(variables=list(n=count,bins=bins,date=date),category_spec=defaults)
  books <- .category_review_books(r,ctx,"InactiveDomain")
  expect_identical(books$prospective$r_type,books$published$r_type)
  for (v in list(flag,bins,date)) {
    r <- omop_recipe(variables=setNames(list(omop_category(v,defaults)),v$name %||% "x"))
    expect_error(recipe_execute(r,out="ExplicitIncompatible",symbol="labels",conns=ctx$conns),"concept identities|category_spec_invalid")
  }
  explicit <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("mode"),
    category_spec=omop_category_spec(rollup=omop_category_rollup("snomed",separation=0L,domain="concept:45000124",multiple="all")))
  expect_error(recipe_execute(omop_recipe(variables=list(x=explicit)),out="ExplicitMultiple",symbol="labels",conns=ctx$conns),"Multiple hierarchy targets|category_type_unsupported")
})


test_that("R2 binary recipe defaults and explicit resets retain typed domains", {
  ctx <- .clf_setup(); .numeric_lexer()
  v <- omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("binary"))
  r <- omop_recipe(variables=list(inherited=v,
    logical=omop_category(v,omop_category_spec(binary=omop_category_binary("logical"))),
    reset=omop_category(v,omop_category_spec(binary=omop_category_binary("integer")))),
    category_spec=omop_category_spec(binary=omop_category_binary("factor",c("absent","present")),ordered=TRUE))
  books <- .category_review_books(r,ctx,"BinaryDefaults")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],c("ordered_factor","logical","integer"))
  expect_equal(unlist(books$prospective$levels[[2]]$keys),unlist(books$published$levels[[2]]$keys))
  expect_equal(unlist(books$prospective$levels[[2]]$labels),unlist(books$published$levels[[2]]$labels))
  expect_null(books$prospective$levels[[3]]); expect_null(books$prospective$levels[[4]])
})

test_that("R2 date result typing uses execution semantics before numeric defaults", {
  ctx <- .clf_setup(); .numeric_lexer()
  r <- omop_recipe(variables=list(days=omop_time_between("2020-01-01","2020-01-02"),
    cyclic=omop_date_parts("2020-01-01",parts=c(cyclic="month_sin"))))
  books <- .category_review_books(r,ctx,"DateNumericTypes")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],c("integer","double"))
  r <- omop_recipe(variables=list(days=omop_time_between("2020-01-01","2020-01-02")),
    number_spec=omop_number_spec(representation="bins",breaks=c(0,2,10)))
  books <- .category_review_books(r,ctx,"DateNumericBins")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_identical(books$prospective$r_type[-1],"ordered_factor")
})


test_that("R2 category ordering, recodes and resets use the public producer domain", {
  ctx <- .clf_setup(); .numeric_lexer()
  v <- omop_number_bins(omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("count")),breaks=c(0,2,10),labels=c("few","many"))
  r <- omop_recipe(variables=list(inherited=v,reset=omop_category(v,omop_category_spec(order=omop_category_order(metadata="producer"),ordered=FALSE))),
    category_spec=omop_category_spec(order=omop_category_order(c("bin:2","bin:1")),ordered=TRUE,reference="bin:1"))
  books <- .category_review_books(r,ctx,"CategoryOrderDefaults")
  expect_identical(books$prospective$r_type,books$published$r_type)
  for (i in 2:3) {
    expect_equal(unlist(books$prospective$levels[[i]]$keys),unlist(books$published$levels[[i]]$keys))
    expect_equal(unlist(books$prospective$levels[[i]]$labels),unlist(books$published$levels[[i]]$labels))
    expect_equal(books$prospective$reference[[i]],books$published$reference[[i]])
    expect_equal(books$prospective$contrasts[[i]],books$published$contrasts[[i]])
  }
  spec <- omop_category_spec(recode=omop_category_recode(c(`bin:1`="category:few",`bin:2`="category:many"),c("category:few","category:many"),labels=c("Few","Many")),missing=omop_category_missing(na="level"))
  r <- omop_recipe(variables=list(n=omop_category(v,spec)))
  books <- .category_review_books(r,ctx,"CategoryRecode")
  expect_identical(books$prospective$r_type,books$published$r_type)
  expect_equal(unlist(books$prospective$levels[[2]]$keys),unlist(books$published$levels[[2]]$keys))
  expect_equal(unlist(books$prospective$levels[[2]]$labels),unlist(books$published$levels[[2]]$labels))
})


test_that("R1 expanded-column projection also preserves long output structure", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L,name="answers")
  v <- omop_category_reduce(omop_category_rows(e,"value_as_concept_id"),omop_reduce("nth_value",n=c(1L,3L)),broadcast=TRUE)
  r <- omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e,select="all")),
    variables=list(answer=v),outputs=omop_output("long",format="long",rows="p",sources=list(e),columns="answer_3"))
  books <- .category_review_books(r,ctx,"NthLong")
  expect_identical(books$prospective$name[startsWith(books$prospective$name,"answer")],"answer_3")
  expect_identical(books$published$name[startsWith(books$published$name,"answer")],"answer_3")
  for (f in .clf_frames(ctx,"NthLong")) {
    expect_true(all(c("row_id","cohort_row_id","person_id","concept_id") %in% names(f)))
    expect_equal(nrow(f),18L)
  }
})
