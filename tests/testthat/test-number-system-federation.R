if(nzchar(Sys.getenv("DSOMOP_TEST_NUMERIC_CDM"))) .numeric_definitions()
test_that("N41 N42 N43 N65 N66 numeric and complete public domains reconcile at retained sites", {
  ctx<-.numeric_sites();.numeric_lexer()
  e<-omop_event("measurement",numeric_lab,unit=omop_unit(8713L))
  v<-omop_variable(event=e,reducer=omop_reduce("mean"))
  twice<-list(version=1L,assign=list(list(name="twice",expr=list(op="*",args=list(list(col="x"),list(lit=2))))))
  r<-omop_recipe(variables=list(double=omop_number_derive(list(x=v),twice,"coordinate:doubled"),total=omop_number_derive(list(x=omop_number_rows(e)),twice,"coordinate:doubled",reducer=omop_reduce("sum")),projection=omop_number_rows(e,number_spec=omop_number_spec(rounding=omop_number_round("decimals",1))),event_double=omop_number_derive(list(x=omop_number_rows(e)),twice,"coordinate:doubled"),value=omop_number(v),first=omop_number(omop_variable(event=e,reducer=omop_reduce("first_value"))),
    band=omop_number_bins(v,breaks=c(-Inf,0,12,15,Inf)),table_band=omop_number_bins(v,breaks=c(-Inf,0,100,Inf)),cat=omop_number_category(v,thresholds=c(12,15))),
    populations=omop_population("p",index=omop_index(e)),outputs=list(omop_output("o",format="wide",rows="p",columns=c("value","first","double","total","band","table_band","cat")),omop_output("events",format="long",rows="p",sources=list(e),columns=c("projection","event_double"),options=list(columns_derived=c("value")))))
  rate<-list(version=1L,assign=list(list(name="rate",expr=list(op="/",args=list(list(op="*",args=list(list(lit=365250),list(col="n"))),list(col="d"))))))
  r$outputs$periods<-omop_output("periods",format="person_period",rows="p",sources=list(e),columns=c("days","n_case","days_raw","days_weeks","days_band","period_rate"),number_columns=list(days_raw=omop_number_column("days"),days_weeks=omop_number_column("days",number_spec=omop_number_spec(unit=omop_number_unit("days","weeks",1/7))),days_band=omop_number_bins(omop_number_column("days"),breaks=c(0,7,14,Inf)),period_rate=omop_number_derive(list(n=omop_number_column("n_case"),d=omop_number_column("days")),rate,"coordinate:per_1000_person_years")),options=list(bins=list(width=10L),window=omop_window(0L,20L,anchor="index"),outcomes=list(case=e)))
  previous<-NULL
  for(mode in c("memory","staged")) {
    destination<-paste0("Number_",mode)
    recipe_execute(r,out=c(o=destination,events=paste0(destination,"Events"),periods=paste0(destination,"Periods")),symbol="numeric",conns=ctx$conns,output_mode=mode)
    frames<-.numeric_frames(ctx,destination,mode)
    events<-.numeric_frames(ctx,paste0(destination,"Events"),mode)
    periods<-.numeric_frames(ctx,paste0(destination,"Periods"),mode)
    for(f in periods) {
      expect_equal(f$days_raw,f$days)
      expect_equal(f$days_weeks,f$days/7)
      expect_equal(f$period_rate,365250*f$n_case/f$days)
      expect_true(is.ordered(f$days_band))
      expect_identical(levels(f$days_band),c("[0,7)","[7,14)","[14,Inf)"))
    }
    for(i in seq_along(events)) {
      expect_equal(events[[i]]$event_double,2*events[[i]]$value)
      expect_equal(frames[[i]]$double,2*frames[[i]]$value)
      expect_equal(sum(frames[[i]]$total),sum(events[[i]]$event_double))
      expect_true(is.numeric(events[[i]]$projection))
    }
    for(i in seq_along(frames)) {
      expect_equal(ncol(stats::model.matrix(~band,data=frames[[i]])),4L)
      expect_true(is.numeric(frames[[i]]$value));expect_true(is.ordered(frames[[i]]$band));expect_true(is.ordered(frames[[i]]$cat))
      expect_identical(levels(frames[[i]]$band),c("(-Inf,0)","[0,12)","[12,15)","[15,Inf)"))
      expect_identical(levels(frames[[i]]$cat),c("low","normal","high"))
      if(!is.null(previous))for(n in c("value","first","double","total","band","cat"))expect_equal(frames[[i]][[n]],previous[[i]][[n]])
    }
    if(mode=="memory")for(n in 2:3) {
      conns<-ctx$conns[seq_len(n)];f<-do.call(rbind,lapply(frames[seq_len(n)],as.data.frame))
      tab<-as.table(ds.omop.table("Number_memory","table_band",conns=conns))
      expect_equal(as.numeric(tab),as.numeric(table(f$table_band)))
      expect_identical(dimnames(tab)[[1]],levels(f$table_band))
      mean<-dsBaseClient::ds.mean("Number_memory$value",type="combine",datasources=conns)
      expect_equal(as.numeric(mean$Global.Mean[1,1]),mean(f$value),tolerance=1e-9)
      doubled<-dsBaseClient::ds.mean("Number_memory$double",type="combine",datasources=conns)
      expect_equal(as.numeric(doubled$Global.Mean[1,1]),2*mean(f$value),tolerance=1e-9)
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
