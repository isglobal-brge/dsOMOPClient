if(nzchar(Sys.getenv("DSOMOP_TEST_NUMERIC_CDM"))) .numeric_definitions()
test_that("N41 N42 N43 N65 N66 numeric and complete public domains reconcile at retained sites", {
  ctx<-.numeric_sites();.numeric_lexer()
  e<-omop_event("measurement",numeric_lab,unit=omop_unit(8713L))
  v<-omop_variable(event=e,reducer=omop_reduce("mean"))
  r<-omop_recipe(variables=list(value=omop_number(v),first=omop_number(omop_variable(event=e,reducer=omop_reduce("first_value"))),
    band=omop_number_bins(v,breaks=c(-Inf,0,12,15,Inf)),table_band=omop_number_bins(v,breaks=c(-Inf,0,100,Inf)),cat=omop_number_category(v,thresholds=c(12,15))),
    populations=omop_population("p",index=omop_index(e)),outputs=omop_output("o",format="wide",rows="p"))
  previous<-NULL
  for(mode in c("memory","staged")) {
    destination<-paste0("Number_",mode)
    recipe_execute(r,out=c(o=destination),symbol="numeric",conns=ctx$conns,output_mode=mode)
    frames<-.numeric_frames(ctx,destination,mode)
    for(i in seq_along(frames)) {
      expect_equal(ncol(stats::model.matrix(~band,data=frames[[i]])),4L)
      expect_true(is.numeric(frames[[i]]$value));expect_true(is.ordered(frames[[i]]$band));expect_true(is.ordered(frames[[i]]$cat))
      expect_identical(levels(frames[[i]]$band),c("(-Inf,0)","[0,12)","[12,15)","[15,Inf)"))
      expect_identical(levels(frames[[i]]$cat),c("low","normal","high"))
      if(!is.null(previous))for(n in c("value","first","band","cat"))expect_equal(frames[[i]][[n]],previous[[i]][[n]])
    }
    if(mode=="memory")for(n in 2:3) {
      conns<-ctx$conns[seq_len(n)];f<-do.call(rbind,lapply(frames[seq_len(n)],as.data.frame))
      tab<-ds.omop.table("Number_memory","table_band",conns=conns)
      expect_equal(as.numeric(tab),as.numeric(table(f$table_band)))
      expect_identical(dimnames(tab)[[1]],levels(f$table_band))
      mean<-dsBaseClient::ds.mean("Number_memory$value",type="combine",datasources=conns)
      expect_equal(as.numeric(mean$Global.Mean[1,1]),mean(f$value),tolerance=1e-9)
      quantiles<-dsBaseClient::ds.quantileMean("Number_memory$value",type="combine",datasources=conns)
      oracle<-Reduce(`+`,lapply(frames[seq_len(n)],function(f)stats::quantile(f$value,c(.05,.10,.25,.5,.75,.90,.95))*nrow(f)))/nrow(f)
      expect_equal(unname(quantiles[1:7]),unname(oracle))
      fit<-dsBaseClient::ds.glm(value~first,data="Number_memory",family="gaussian",datasources=conns)
      expect_equal(unname(fit$coefficients[,1]),unname(coef(stats::glm(value~first,data=f))),tolerance=1e-6)
    }
    previous<-frames
  }
})
test_that("N43 public numeric contracts cannot pool by matching labels alone", {
  record<-function(unit)list(kind="number",id="N1",label="same",number=list(unit=unit),order=1L)
  expect_error(.numberResolveRecords(list(record("days"),record("weeks"))),"differ")
  expect_error(.numberValidateShared(list(value=list(unit="days")),list(value=list(unit="weeks"))),"differ")
})


test_that("N43 N46 strict and error/skip use retained numeric components only", {
  for(strict in c(TRUE,FALSE)) local({
    ctx<-.clf_setup(strict=strict,excluded=TRUE)
    p<-recipe_to_plan(.clf_recipe())
    p$variables<-list(n=list(source=list(event=names(p$events)[1L]),reducer=list(fn="count"),number_spec=list(representation="bins",breaks=list("-Inf",0,1,5,"Inf"))))
    p$outputs$output$columns<-list("n")
    p$events$few<-list(table="observation",concept_set=list(concepts=list(45000125L)))
    p$populations$few<-list(kind="index",index=list(source=list(event="few"),select="all",end=list(strategy="event_end")))
    p$outputs$small<-list(rows="few",format="wide",grain="episode")
    p$publish<-list("output","small")
    for(policy in c("error","skip")) {
      symbols<-c(output=paste0("NumericRetained_",policy),small=paste0("NumericSmall_",policy))
      execute<-function()ds.omop.plan.execute(p,out=symbols,symbol="labels",conns=ctx$conns,on_small_site=policy)
      if(strict&&policy=="error") {
        expect_error(execute(),"insufficient_support")
        inventories<-DSI::datashield.symbols(ctx$conns)
        expect_true(all(vapply(inventories,function(x)!any(symbols %in% x),TRUE)))
      } else {
        suppressWarnings(execute())
        frame<-ctx$servers$site1$getSessionData(ctx$servers$site1$getSessionIds()[[1L]],symbols[["small"]])
        expect_true(is.ordered(frame$n));expect_identical(levels(frame$n),c("(-Inf,0)","[0,1)","[1,5)","[5,Inf)"))
      }
    }
  })
})
test_that("N43 N66 corrupted numeric contracts roll back rather than skip sites", {
  ctx<-.clf_setup()
  p<-recipe_to_plan(.clf_recipe())
  p$variables<-list(n=list(source=list(event=names(p$events)[1L]),reducer=list(fn="count"),number_spec=list(representation="bins",breaks=list(0,1,5,"Inf"))))
  p$outputs$output$columns<-list("n")
  original<-.conceptUnionReports
  testthat::local_mocked_bindings(.conceptUnionReports=function(reports) {
    spec<-original(reports);spec$columns$n$levels<-list("changed");spec
  })
  expect_error(ds.omop.plan.execute(p,out=c(output="NumericRollback"),symbol="labels",conns=ctx$conns,on_small_site="skip"),"recoding failed")
  inventories<-DSI::datashield.symbols(ctx$conns)
  expect_true(all(vapply(inventories,function(x)!"NumericRollback" %in% x,TRUE)))
})
