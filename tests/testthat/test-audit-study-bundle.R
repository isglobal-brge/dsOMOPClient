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
test_that("Q26 partial publication does not copy private receipt inventories", {
  r<-omop_recipe(variables=list(age=omop_age_at("2020-01-01")))
  p<-recipe_to_plan(r,version=2);schemas<-.codebookPlanSchemas(p)
  schemas[[1]]$schema$columns[[1]]$provenance$patient_rows<-"PRIVATE_SENTINEL"
  schemas[[1]]$private_components<-"PRIVATE_SENTINEL"
  x<-structure(list(),omop_codebook=list(plan=p,components=schemas,
    private_components="PRIVATE_SENTINEL",credentials="SECRET_SENTINEL",
    publication_sites=list(siteA=list(status="ok",components=names(schemas)),siteB=list(status="refused",components=list()))))
  b<-recipe_study_bundle(x,list(id="partial",protocol_version="1",roles=list(target=list(population="base"))))
  expect_identical(b$publication$status,"published")
  expect_identical(b$publication$sites$siteB$status,"refused")
  expect_false(grepl("PRIVATE_SENTINEL|SECRET_SENTINEL",.numberJson(b)))
  expect_identical(b$publication$components,as.list(names(schemas)))
})
test_that("Q26 bundles retain customized template ancestry and original phenotype identity", {
  r<-.library_test_recipe("demographics.baseline")
  study<-list(id="custom",protocol_version="2",roles=list(target=list(population=names(recipe_to_plan(r)$populations)[1])))
  before<-recipe_study_bundle(r,study)
  r$variables[[1L]]<-omop_age_at("2021-01-01")
  after<-recipe_study_bundle(r,study)
  expect_identical(before$library,after$library)
  expect_false(identical(before$semantic_digest,after$semantic_digest))
  item<-list(concept=list(CONCEPT_ID=201820))
  cs<-list(id=0,expression=list(items=list(item)))
  definition<-list(ConceptSets=list(cs),PrimaryCriteria=list(CriteriaList=list(list(ConditionOccurrence=list(CodesetId=0))),PrimaryCriteriaLimit=list(Type="First")))
  pop<-recipe_import_circe(definition,id="phenotype",origin=list(phenotype_id="public-example",release="pinned"))
  imported<-omop_recipe(populations=pop)
  imported$populations$phenotype$index_event$concept_id<-201826L
  b<-recipe_study_bundle(imported,list(id="phenotype",protocol_version="1",roles=list(target=list(population="phenotype"))))
  expect_identical(b$phenotype_origins$phenotype$original_sha256,pop$circe_provenance$original_sha256)
  expect_true(b$phenotype_origins$phenotype$derivative)
  expect_false("original_json" %in% names(b$phenotype_origins$phenotype))
})
