test_that("Q14 independent power oracle agrees with actual private published formulas", {
  ctx<-.clf_setup();.numeric_lexer();x<-.audit_formula_inputs()
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
      if(mode=="memory") expect_identical(attr(f,"omop_numbers")$egfr$privacy_class,2L)
    }
    # A staged output's chunks are plain data: its number contracts travel in
    # the descriptor's presentation metadata.
    if(mode=="staged") for(server in ctx$servers) {
      descriptor<-server$getSessionData(server$getSessionIds()[[1L]],symbols[[1L]])
      meta<-dsOMOP:::.ds_arg(descriptor$metadata$concept_presentation)
      expect_identical(meta$numbers$egfr$privacy_class,2L)
    }
    expect_true(length(recipe_codebook(result)[[1L]]$formulas)>0L)
  }
})
