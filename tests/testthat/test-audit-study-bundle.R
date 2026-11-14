test_that("Q26 bundles contain only public allowlisted metadata and exact bound plans", {
  r<-omop_recipe(variables=list(age=omop_age_at("2020-01-01")))
  study<-list(id="demo",protocol_version="1.0",roles=list(target=list(population="base",cohort_definition_id="9007199254740993")))
  r$meta$password<-"SECRET_SENTINEL"
  testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("network"),datashield.assign.expr=function(...)stop("network"),.package="DSI")
  path<-tempfile(fileext=".json");on.exit(unlink(path))
  b<-recipe_study_bundle(r,study,path)
  saved<-jsonlite::fromJSON(path,simplifyVector=FALSE)
  expect_identical(b$snapshot$status,"unknown")
  expect_identical(saved$study$roles$target$cohort_definition_id,"9007199254740993")
  expect_identical(.planNormalizedDigest(saved$bound_recipe$plan)$sha256,b$semantic_digest)
  expect_false(grepl("SECRET_SENTINEL",paste(readLines(path),collapse="")))
  expect_identical(.numberJson(b),.numberJson(recipe_study_bundle(r,study)))
  bad<-study;bad$roles$target$population<-"absent"
  expect_error(recipe_study_bundle(r,bad),"study_bundle_invalid.*roles")
  bad<-study;bad$protocol_uri<-"https://user:SECRET_SENTINEL@example.org"
  expect_error(recipe_study_bundle(r,bad),"remove credentials")
  bad<-study;bad$password<-"SECRET_SENTINEL"
  expect_error(recipe_study_bundle(r,bad),"documented fields")
  bad<-study;bad$roles$target$cohort_definition_id<-9007199254740992
  expect_error(recipe_study_bundle(r,bad),"exact nonnegative")
})
test_that("Q07 native interoperability scopes identities to public semantic definitions", {
  r<-omop_recipe(variables=list(x=omop_age_at("2020-01-01")))
  books<-recipe_codebook(r)
  decl<-books[[1L]]$columns$provenance[[1L]]$interoperability
  expect_identical(decl$schema_version,"ohdsi-declaration-1")
  expect_identical(decl$compatibility,"native")
  expect_identical(decl$identity$scope_digest,books[[1L]]$plan_digest)
  p<-recipe_to_plan(r,version=2)
  other<-p;other$variables$x$derived$reference<-"2021-01-01"
  expect_false(identical(.ohdsiDeclaration(p,key="covariate_id")$identity,
    .ohdsiDeclaration(other,key="covariate_id")$identity))
})
