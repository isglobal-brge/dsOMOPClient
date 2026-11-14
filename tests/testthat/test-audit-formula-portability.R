test_that("Q14 builders validate units, versions, mapping and calendar-age association", {
  x<-.audit_formula_inputs();u<-x$coordinate
  expect_s3_class(omop_formula_bmi(u("coordinate:kg"),u("coordinate:metres")),"omop_variable")
  expect_s3_class(omop_formula_map(u("coordinate:mmHg"),u("coordinate:mmHg")),"omop_variable")
  expect_error(omop_formula_bmi(x$raw,x$raw),"formula_unit_mismatch")
  expect_error(omop_formula_ratio(x$raw,x$raw,"1",version="latest"),"formula_version_unsupported")
  scr<-u("coordinate:idms_creatinine_mg_dl")
  expect_error(omop_formula_egfr_ckdepi2021(scr,omop_age_at("index"),x$sex,x$map),"formula_age_anchor")
  expect_error(omop_formula_egfr_ckdepi2021(scr,x$age,x$sex,list(female="concept:0",male="concept:8507")),"formula_sex_map")
  expect_error(omop_formula_egfr_ckdepi2021(scr,x$age,x$sex,list(female="concept:8507",male="concept:8507")),"formula_sex_map")
})
test_that("Q14 closed log-exp expansion and provenance survive JSON YAML and generated R", {
  x<-.audit_formula_inputs()
  v<-omop_formula_egfr_ckdepi2021(x$coordinate("coordinate:idms_creatinine_mg_dl"),x$age,x$sex,x$map)
  r<-omop_recipe(variables=list(egfr=v))
  p<-recipe_to_plan(r,version=2)
  expect_true(all(c("number.expression","number.presentation","category.indicators","date.elapsed") %in% .plan_v2_operations(p)))
  walk<-function(x) {if(is.list(x)) {if(!is.null(x$stage))expect_identical(x$stage,"presented");lapply(x,walk)};NULL}
  walk(p$variables$egfr)
  for(format in c("json","yaml")) {
    path<-tempfile(fileext=paste0(".",format));on.exit(unlink(path),add=TRUE)
    get(paste0("recipe_export_",format))(r,path)
    restored<-get(paste0("recipe_import_",format))(path)
    expect_identical(.planNormalizedDigest(recipe_to_plan(restored,version=2))$sha256,.planNormalizedDigest(p)$sha256)
    expect_identical(.numberJson(.formulaInventory(restored)),.numberJson(.formulaInventory(r)))
  }
  env<-new.env(parent=asNamespace("dsOMOPClient"));eval(parse(text=recipe_to_code(r)),env)
  restored<-tail(Filter(function(x)inherits(x,"omop_recipe"),as.list(env)),1)[[1]]
  expect_identical(.planNormalizedDigest(recipe_to_plan(restored,version=2))$sha256,.planNormalizedDigest(p)$sha256)
  expect_identical(.numberJson(.formulaInventory(restored)),.numberJson(.formulaInventory(r)))
})

test_that("formula definitions retain exact binary64 coefficients in YAML", {
  a<-.formulaDefinitions()
  b<-.yaml_load_safe(yaml::as.yaml(a,precision=17L,handlers=list(numeric=.numberYamlNumeric)))
  expect_identical(.numberJson(a),.numberJson(b))
})

test_that("Q14 edited expansions retain immutable origin and identify derivatives", {
  x<-.audit_formula_inputs()
  v<-omop_formula_ratio(x$coordinate("1"),x$coordinate("1"),"1")
  before<-.formulaProvenance(v)
  expect_false(before$derivative)
  v$number_spec<-omop_number_spec(round=omop_number_round("decimals",digits=2L))
  after<-.formulaProvenance(v)
  expect_true(after$derivative)
  expect_identical(after$original_expansion_sha256,before$original_expansion_sha256)
  expect_identical(after$sha256,before$sha256)
  expect_false(identical(after$expansion_sha256,before$expansion_sha256))
  r<-omop_recipe(variables=list(ratio=v))
  restored<-recipe_import_json(recipe_export_json(r))
  expect_identical(.numberJson(.formulaInventory(restored)),.numberJson(.formulaInventory(r)))
  after$equation<-"changed"
  expect_error(.formulaRestore(v,after),"formula_provenance_invalid")
})
