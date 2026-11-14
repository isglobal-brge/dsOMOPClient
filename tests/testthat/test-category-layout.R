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
