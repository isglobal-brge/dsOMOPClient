test_that("C46 C48 C53 C54 nested survival covariates retain category contracts", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L,name="e")
  v<-omop_category(omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id")),omop_category_spec(ordered=TRUE))
  r<-omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),variables=list(education=v),outputs=omop_output("rec",format="survival",rows="p",columns="education",options=list(format="recurrent_events",outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end"))))
  prospective<-recipe_codebook(r)
  expect_setequal(names(prospective),c("rec/events","rec/risk_sets"))
  for(entry in prospective)expect_true("education" %in% entry$columns$name)
  for(mode in c("memory","staged")) {
    base<-paste0("Nested_",mode)
    result<-recipe_execute(r,out=base,symbol="labels",conns=ctx$conns,output_mode=mode)
    book<-recipe_codebook(result)
    expect_setequal(names(book),names(prospective))
    inventory<-DSI::datashield.symbols(ctx$conns)
    for(component in c("events","risk_sets")) {
      candidates<-c(paste0(base,".",component),if(component=="risk_sets")paste0(base,".riskSets"))
      symbol<-intersect(candidates,unlist(inventory,use.names=FALSE));expect_length(symbol,1L)
      for(f in .clf_frames(ctx,symbol)) {
        expect_true(is.ordered(f$education));expect_identical(book[[paste("rec",component,sep="/")]]$columns$name,names(f))
      }
    }
  }
})

test_that("C46 C54 multi-state covariates and transition references have separate schemas", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L,name="e")
  v<-omop_category(omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id")),omop_category_spec(ordered=FALSE))
  output<-omop_output("course",format="survival",rows="p",columns="education",options=list(format="multi_state",outcomes=list(lab=e),transitions=list(list(from="index",to="lab")),initial_state="index",tar=omop_window(0,365),censor=list("observation_end")))
  r<-omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),variables=list(education=v),outputs=output)
  expected<-recipe_codebook(r)
  expect_setequal(names(expected),c("course/course","course/transitionRef"))
  for(mode in c("memory","staged")) {
    base<-paste0("States_",mode)
    result<-recipe_execute(r,out=base,symbol="labels",conns=ctx$conns,output_mode=mode)
    book<-recipe_codebook(result)
    expect_setequal(names(book),names(expected))
    inventory<-unique(unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE))
    symbol<-intersect(c(base,paste0(base,".msdata")),inventory);expect_length(symbol,1L)
    for(f in .clf_frames(ctx,symbol)) {
      expect_s3_class(f$education,"factor");expect_type(f$status,"integer")
      expect_identical(book[["course/course"]]$columns$name,names(f))
    }
    expect_false("education" %in% book[["course/transitionRef"]]$columns$name)
  }
})

test_that("C46 C48 C54 competing-risk and counting-process tables retain typed covariates", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L,name="e")
  education<-omop_category(omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id")),omop_category_spec(ordered=TRUE))
  outputs<-lapply(c("competing_risk","counting_process"),function(format)omop_output(format,format="survival",rows="p",columns="education",options=list(format=format,outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end"))))
  r<-omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),variables=list(education=education),outputs=outputs)
  prospective<-recipe_codebook(r)
  for(mode in c("memory","staged")) {
    symbols<-setNames(paste0("OtherSurvival_",names(r$outputs),"_",mode),names(r$outputs))
    result<-recipe_execute(r,out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode)
    book<-recipe_codebook(result)
    expect_setequal(names(book),names(prospective))
    for(output in names(symbols))for(f in .clf_frames(ctx,symbols[[output]])) {
      expect_true(is.ordered(f$education));expect_type(f$event,"integer")
      expect_identical(book[[paste(output,output,sep="/")]]$columns$name,names(f))
      expect_identical(prospective[[paste(output,output,sep="/")]]$columns$name,names(f))
    }
  }
})
