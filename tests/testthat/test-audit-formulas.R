.audit_formula_inputs <- function() {
  e<-omop_event("measurement",3000963L,name="scr",unit=omop_unit(8554L))
  raw<-omop_variable(event=e,reducer=omop_reduce("first_value",missing="keep"))
  coordinate<-function(unit)omop_number(raw,omop_number_spec(unit=omop_number_unit("unit:8554",unit,1)))
  age<-omop_age_at(omop_anchor(e,which="first",scope="episode"))
  sex<-omop_variable(table="person",column="gender_concept_id")
  list(event=e,raw=raw,coordinate=coordinate,age=age,sex=sex,
    map=list(female="concept:8532",male="concept:8507"))
}
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
test_that("Q14 independent power oracle agrees with actual private published formulas", {
  ctx<-.clf_setup();.numeric_lexer();x<-.audit_formula_inputs()
  original<-dsOMOP:::.rejectionEntry
  testthat::local_mocked_bindings(.rejectionEntry=function(e){message("PRIVATE SYNTHETIC DEBUG: ",conditionMessage(e));original(e)},.package="dsOMOP")
  # Synthetic source values and protocol units are test declarations.
  scr<-rep(c(.35,.7,1.4,.45,.9,1.8),3)
  sex<-rep(c(8532L,8532L,8532L,8507L,8507L,8507L),3)
  for(db in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),db)
    DBI::dbExecute(con,"DELETE FROM measurement")
    DBI::dbWriteTable(con,"measurement",data.frame(measurement_id=seq_along(scr),person_id=seq_along(scr),measurement_concept_id=3000963L,measurement_date="2020-01-01",measurement_type_concept_id=0L,value_as_number=scr,unit_concept_id=8554L),append=TRUE)
    DBI::dbExecute(con,"UPDATE person SET year_of_birth=1980, month_of_birth=1, day_of_birth=1")
    for(i in seq_along(scr)) {
      DBI::dbExecute(con,"UPDATE person SET gender_concept_id=? WHERE person_id=?",params=list(sex[i],i))
    }
    DBI::dbDisconnect(con)
  }
  egfr<-omop_formula_egfr_ckdepi2021(x$coordinate("coordinate:idms_creatinine_mg_dl"),x$age,x$sex,x$map)
  constant<-function(value,unit) .formulaDerive(list(x=x$raw),.formulaLit(value),unit)
  r<-omop_recipe(variables=list(egfr=egfr,
    bmi=omop_formula_bmi(constant(80,"coordinate:kg"),constant(2,"coordinate:metres")),
    map=omop_formula_map(constant(120,"coordinate:mmHg"),constant(80,"coordinate:mmHg")),
    ratio=omop_formula_ratio(constant(10,"1"),constant(2,"1"),"1"),
    zero=omop_formula_ratio(constant(10,"1"),constant(0,"1"),"1")),
    populations=omop_population("p",index=omop_index(x$event)),outputs=omop_output("o",rows="p"))
  # Independently written published equation, not log/exp expansion.
  reference<-function(scr,age,female)142*pmin(scr/ifelse(female,.7,.9),1)^ifelse(female,-.241,-.302)*
    pmax(scr/ifelse(female,.7,.9),1)^(-1.2)*.9938^age*ifelse(female,1.012,1)
  pinned<-c(132.427073658545,112.054306798798,48.774469951725,136.508312959617,110.725599603555,48.196116553088)
  expect_equal(reference(scr[1:6],40,sex[1:6]==8532),pinned,tolerance=1e-12)
  for(mode in c("memory","staged")) {
    symbols<-c(o=paste0("Formula_",mode))
    result<-ds.omop.plan.execute(recipe_to_plan(r,version=2),out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbols[[1L]])) {
      expect_equal(sort(f$egfr),sort(reference(scr,40,sex==8532)),tolerance=1e-10)
      expect_equal(f$bmi,rep(20,nrow(f)));expect_equal(f$map,rep(280/3,nrow(f)))
      expect_equal(f$ratio,rep(5,nrow(f)));expect_true(all(is.na(f$zero)))
      expect_identical(attr(f,"omop_numbers")$egfr$privacy_class,2L)
    }
    expect_true(length(recipe_codebook(result)[[1L]]$formulas)>0L)
  }
})
