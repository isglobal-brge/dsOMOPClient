
for(dataset in c("GiBleed","Synthea27Nj"))local({
  dataset<-dataset
  test_that(paste("C46 C48 C53 C54 C56 C64",dataset,"runs all native formats and codebooks at three sites"), {
    .numeric_lexer()
    reconcile<-.harmonizeOneSymbol
    testthat::local_mocked_bindings(.harmonizeOneSymbol=function(sym,conns,...) {
      message("CDM reconciliation: ",unique(unname(sym)))
      reconcile(sym,conns,...)
    },.package="dsOMOPClient")
    ctx<-if(dataset=="GiBleed").category_gibleed_sites() else .numeric_sites()
    recipe<-.category_format_recipe(if(dataset=="GiBleed")3006322L else 3000963L)
    plan<-recipe_to_plan(recipe,version=2)
    expect_setequal(vapply(plan$outputs,function(x)x$format,""),c("wide","long","intervals","membership","dictionary","person_period","sparse","temporal_covariates","survival","joined"))
    expect_length(recipe_codebook(recipe),15L)
    prior<-NULL
    for(mode in c("memory","staged")) {
      destinations<-setNames(paste0("Categories_",names(plan$outputs),"_",mode),names(plan$outputs))
      result<-ds.omop.plan.execute(plan,out=destinations,symbol="numeric",conns=ctx$conns,output_mode=mode)
      book<-recipe_codebook(result)
      expect_gte(length(book),length(plan$outputs))
      if(mode=="staged")for(root in names(destinations))for(i in seq_along(ctx$servers)) {
        server<-ctx$servers[[i]];descriptor<-server$getSessionData(server$getSessionIds()[[1L]],destinations[[root]])
        if(inherits(descriptor,"FlowerDatasetDescriptor"))dsOMOP:::.publishReadChunks(descriptor,function(chunk) {
          for(n in names(chunk))if(is.factor(prior[[root]][[i]][[n]])) {
            expect_identical(levels(chunk[[n]]),levels(prior[[root]][[i]][[n]]))
            expect_identical(is.ordered(chunk[[n]]),is.ordered(prior[[root]][[i]][[n]]))
            expect_equal(attr(chunk[[n]],"contrasts"),attr(prior[[root]][[i]][[n]],"contrasts"))
          }
        })
      }
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
        if(!is.null(prior))for(column in names(f)) {
          if(is.factor(f[[column]]))expect_identical(as.character(f[[column]]),as.character(prior[[root]][[i]][[column]]))
          else expect_equal(f[[column]],prior[[root]][[i]][[column]],info=paste(dataset,root,column))
        }
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
        expect_equal(as.table(ds.omop.table(symbol,"flag_factor",ctx$conns)),expected)
        expect_equal(as.table(ds.omop.table(symbol,"flag_factor",rev(ctx$conns))),expected)
        expected_mean<-mean(unlist(lapply(frames$wide,function(f)f$count),use.names=FALSE))
        fit<-dsBaseClient::ds.glm(formula="count ~ 1",data=symbol,family="gaussian",datasources=ctx$conns)
        expect_equal(unname(fit$coefficients[1L,1L]),expected_mean,tolerance=1e-6)
      }
      prior<-frames
    }
  })
})
