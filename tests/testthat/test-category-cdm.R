.category_format_recipe <- function(id) {
  lab<-omop_event("measurement",id,name="lab")
  base<-omop_variable(event=lab,reducer=omop_reduce("count"))
  binary<-omop_variable(event=lab,reducer=omop_reduce("binary"))
  repeat_values<-omop_number_bins(omop_number_rows(lab),breaks=c(-Inf,12,15,Inf))
  repeated<-omop_repeated(lab,list(value=repeat_values),timepoints=omop_timepoints(list(baseline=omop_timepoint(),m6=omop_timepoint(6,"months",c(-7,7)))))
  outputs<-list(
    omop_output("wide",format="wide",rows="p",grain="episode"),
    omop_output("long",format="long",rows="p",sources=list(lab),options=list(columns_derived=c("value","unit","days_from_index"))),
    omop_output("intervals",format="intervals",rows="p",options=list(sources=list(lab))),
    omop_output("membership",format="membership",rows="p"),
    omop_output("dictionary",format="dictionary",rows="p",options=list(source_outputs=c("long"))),
    omop_output("panel",format="person_period",rows="p",columns=character(),options=list(sources=list(lab),window=omop_window(0,3650),bins=list(width=365L),analyses=c("binary","count"))),
    omop_output("sparse",format="sparse",rows="p",columns=character(),options=list(sources=list(lab),analyses=c("binary","count"))),
    omop_output("temporal",format="temporal_covariates",rows="p",options=list(sources=list(lab),analyses=c("binary","count"),bin_width=3651L,window=omop_window(0,3650))),
    omop_output("survival",format="survival",rows="p",options=list(outcomes=list(lab=lab),tar=omop_window(1,3650),censor=list("observation_end"))),
    omop_output("joined",format="joined",rows="p",grain="episode",options=list(from=list(list(output="survival"),list(output="wide")),cardinality="one_to_one")))
  omop_recipe(events=list(lab),populations=omop_population("p",index=omop_index(lab)),variables=list(
    count=base,band=omop_number_bins(base,breaks=c(-Inf,5,10,Inf)),
    flag=omop_category(binary,omop_category_spec(binary=omop_category_binary("logical"))),
    repeated=repeated),outputs=outputs,category_spec=omop_category_spec(ordered=FALSE))
}

for(dataset in c("GiBleed","Synthea27Nj"))local({
  dataset<-dataset
  test_that(paste("C46 C48 C53 C54 C56 C64",dataset,"runs all native formats and codebooks at three sites"), {
    ctx<-if(dataset=="GiBleed").category_gibleed_sites() else .numeric_sites()
    .numeric_lexer()
    original_relevel<-dsOMOP:::.conceptRelevel
    original_compile<-dsOMOP:::.compileOutput
    testthat::local_mocked_bindings(
      .conceptRelevel=function(...)withCallingHandlers(original_relevel(...),error=function(e)message("CDM relevel: ",conditionMessage(e))),
      .compileOutput=function(...)withCallingHandlers(original_compile(...),error=function(e){message("CDM compile: ",conditionMessage(e));print(tail(sys.calls(),12L))}),.package="dsOMOP")
    recipe<-.category_format_recipe(if(dataset=="GiBleed")3006322L else 3000963L)
    plan<-recipe_to_plan(recipe,version=2)
    expect_setequal(vapply(plan$outputs,function(x)x$format,""),c("wide","long","intervals","membership","dictionary","person_period","sparse","temporal_covariates","survival","joined"))
    expect_length(recipe_codebook(recipe),13L)
    prior<-NULL
    for(mode in c("memory","staged")) {
      destinations<-setNames(paste0("Categories_",names(plan$outputs),"_",mode),names(plan$outputs))
      result<-ds.omop.plan.execute(plan,out=destinations,symbol="numeric",conns=ctx$conns,output_mode=mode)
      book<-recipe_codebook(result)
      expect_gte(length(book),length(plan$outputs))
      frames<-lapply(destinations,function(symbol).numeric_frames(ctx,symbol,mode))
      for(root in names(frames))for(i in seq_along(frames[[root]])) {
        f<-frames[[root]][[i]]
        expect_gt(nrow(f),0L,info=paste(dataset,root,mode))
        if(!is.null(prior))for(column in names(f))expect_equal(f[[column]],prior[[root]][[i]][[column]],info=paste(dataset,root,column))
        if(root=="wide") {
          expect_type(f$flag,"logical");expect_true(is.factor(f$band))
          expect_true(is.factor(f$repeated_value_baseline));expect_true(is.factor(f$repeated_value_m6))
        }
        key<-paste(root,root,sep="/")
        if(key %in% names(book))expect_identical(book[[key]]$columns$name,names(f))
      }
      prior<-frames
    }
  })
})
