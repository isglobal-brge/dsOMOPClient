
for(dataset in c("GiBleed","Synthea27Nj"))local({
  dataset<-dataset
  test_that(paste("C46 C48 C53 C54 C56 C64",dataset,"runs all native formats and codebooks at three sites"), {
    .numeric_lexer()
    ctx<-if(dataset=="GiBleed").category_gibleed_sites() else .numeric_sites()
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
      frames<-lapply(names(destinations),function(root) {
        symbol<-destinations[[root]]
        if(root!="dictionary")return(.numeric_frames(ctx,symbol,mode))
        lapply(ctx$servers,function(server) {
          f<-server$getSessionData(server$getSessionIds()[[1L]],symbol)
          if(inherits(f,"FlowerDatasetDescriptor")) {chunks<-list();dsOMOP:::.publishReadChunks(f,function(x)chunks[[length(chunks)+1L]]<<-x);f<-do.call(rbind,chunks)}
          expect_s3_class(f,"data.frame");f
        })
      });names(frames)<-names(destinations)
      for(root in names(frames))for(i in seq_along(frames[[root]])) {
        f<-frames[[root]][[i]]
        expect_true(nrow(f)>0L,info=paste(dataset,root,mode))
        if(!is.null(prior))for(column in names(f))expect_equal(f[[column]],prior[[root]][[i]][[column]],info=paste(dataset,root,column))
        if(root=="wide") {
          expect_type(f$flag,"logical");expect_true(is.factor(f$band))
          expect_true(is.factor(f$repeated_value_baseline));expect_true(is.factor(f$repeated_value_m6))
        }
        key<-paste(root,root,sep="/")
        if(key %in% names(book))expect_identical(book[[key]]$columns$name,names(f))
      }
      if(mode=="memory") {
        symbol<-destinations[["wide"]]
        expected<-Reduce(`+`,lapply(frames$wide,function(f)table(f$flag_factor)))
        names(dimnames(expected))<-paste0(symbol,"$flag_factor")
        expect_equal(ds.omop.table(symbol,"flag_factor",ctx$conns),expected)
        expect_equal(ds.omop.table(symbol,"flag_factor",rev(ctx$conns)),expected)
        expected_mean<-mean(unlist(lapply(frames$wide,function(f)f$count),use.names=FALSE))
        fit<-dsBaseClient::ds.glm(formula="count ~ 1",data=symbol,family="gaussian",datasources=ctx$conns)
        expect_equal(unname(fit$coefficients[1L,1L]),expected_mean,tolerance=1e-6)
      }
      prior<-frames
    }
  })
})
