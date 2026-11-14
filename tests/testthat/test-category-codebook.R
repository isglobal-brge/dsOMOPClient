test_that("C53 C54 C55 codebooks are local and published types match release", {
  r<-omop_recipe(variables=list(flag=omop_category(omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("binary")),omop_category_spec(binary=omop_category_binary("logical")))))
  prospective<-recipe_codebook(r)
  expect_true(length(prospective)>0L)
  expect_true(all(vapply(prospective,function(x)x$status=="prospective",logical(1))))
  ctx<-.clf_setup();.numeric_lexer()
  result<-recipe_execute(r,out="Book",symbol="labels",conns=ctx$conns)
  # Any network call from the local builder is a regression.
  testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
  book<-recipe_codebook(result)
  expect_identical(book[[1L]]$schema_version,"codebook-1")
  expect_identical(book[[1L]]$status,"published")
  cols<-book[[1L]]$columns
  expect_identical(cols$r_type[cols$name=="flag"],"logical")
  expect_false(any(c("count","n","observed","missingness","selected_date","event_id") %in% names(cols)))
  expect_error(recipe_codebook(result,output="absent"),"Unknown exact")
  expect_error(recipe_codebook(list()),"codebook_metadata_missing")
})

test_that("C53 prospective books contain fixed keys, declared types, domains and all families", {
  r<-.category_format_recipe(3000963L)
  b<-recipe_codebook(r)
  expect_identical(b[["wide/wide"]]$columns$name[1:3],c("row_id","cohort_row_id","person_id"))
  expect_identical(b[["wide/wide"]]$columns$r_type[b[["wide/wide"]]$columns$name=="flag"],"logical")
  expect_identical(b[["wide/wide"]]$columns$r_type[b[["wide/wide"]]$columns$name=="repeated_value_m6"],"factor")
  expect_identical(b[["sparse/covariateRef"]]$columns$name,c("covariate_id","source","concept_id","concept_name","analysis"))
  expect_true(all(b[["wide/wide"]]$columns$name %in% b[["joined/joined"]]$columns$name))
  p<-recipe_to_plan(r,version=2)
  p$outputs$survival$options$format<-"recurrent_events"
  p$outputs$survival$options$tars<-list(early=list(start=1,end=30),late=list(start=31,end=365))
  schemas<-.codebookPlanSchemas(p)
  expect_true(all(paste0("survival/",c("early_events","early_risk_sets","late_events","late_risk_sets")) %in% names(schemas)))
})

test_that("C51 hierarchy indicator mapping contracts cannot silently diverge", {
  make<-function(map)list(concept_protocol=1L,levels=list(),concepts=list(),unsafe=character(),nfilter_levels_max=40,keys="flag",generated=list(),schema=list(columns=list(list(stable_key="flag",name="flag",r_type="integer",derivation=list(mapping=map))),row_keys=list()))
  expect_error(.conceptUnionReports(list(a=make("concept:10"),b=make("concept:20"))),"category_domain_incompatible")
})

test_that("C48 empty category metadata encodes null without changing plan NA validation", {
  spec<-list(numbers=list(x=list(unit="1")),columns=list(x=list(reference=NA_character_,aliases=c(a=NA_character_,b="flag:1"))))
  got<-dsOMOP:::.ds_arg(.ds_encode(spec))
  expect_null(got$columns$x$reference)
  expect_identical(names(got$columns$x$aliases),c("a","b"))
  expect_null(got$columns$x$aliases$a)
  expect_identical(got$columns$x$aliases$b,"flag:1")
  expect_error(.ds_encode_lossless(list(value=NA_real_)),"NA is not a JSON|Non-finite numbers")
})

test_that("C53 C54 C59 plain native numeric and untranslated releases have local codebooks", {
  ctx<-.clf_setup();.numeric_lexer()
  r<-omop_recipe(variables=list(n=omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("count"))),options=list(translate_concepts=FALSE))
  p<-recipe_to_plan(r,version=2)
  expect_true("output.codebook_metadata" %in% .plan_v2_operations(p))
  for(mode in c("memory","staged")) {
    result<-recipe_execute(r,out=paste0("PlainBook_",mode),symbol="labels",conns=ctx$conns,output_mode=mode)
    book<-recipe_codebook(result)
    expect_length(book,1L);expect_identical(book[[1L]]$status,"published")
    expect_identical(book[[1L]]$columns$r_type[book[[1L]]$columns$name=="n"],"integer")
  }
  session<-.get_session("labels")
  session$capabilities[[1L]]$recipe_language$operations<-setdiff(session$capabilities[[1L]]$recipe_language$operations,"output.codebook_metadata")
  expect_error(.v2_assert_supported(p,session,names(ctx$conns)),"output.codebook_metadata")
})

test_that("C53 codebook units describe declared pipelines without exposing source inventories", {
  e<-omop_event("measurement",3000963L,unit=omop_unit(8713L))
  v<-omop_variable(event=e,reducer=omop_reduce("mean"))
  r<-omop_recipe(variables=list(raw=v,converted=omop_number(v,omop_number_spec(unit=omop_number_unit("unit:8713","coordinate:test",2))),flag=omop_number_flag(v,"gt",12)))
  columns<-recipe_codebook(r)[[1L]]$columns
  expect_identical(columns$unit[match(c("raw","converted","flag"),columns$name)],c("unit:8713","coordinate:test","1"))
  expect_identical(columns$r_type[match(c("raw","converted","flag"),columns$name)],c("double","double","integer"))
})
