# Compare independent published/actual oracles, not one descriptor helper to itself.
# Fail at the first mismatched component so a large matrix does not repeat one
# defect hundreds of times before reporting its exact declared/actual schema.
.equivalence_identical <- function(expected,actual,info=NULL) {
  expect_identical(expected,actual,info=info)
  if(!identical(expected,actual)) {
    diagnostic <- paste(info,"expected",paste(capture.output(dput(expected)),collapse=" "),
      "actual",paste(capture.output(dput(actual)),collapse=" "))
    message(diagnostic); stop(diagnostic)
  }
}
.equivalence_books <- function(recipe,ctx,mode,label) {
  plan <- recipe_to_plan(recipe,version=2)
  server <- ctx$servers[[1L]]
  handle <- server$getSessionData(server$getSessionIds()[[1L]],paste0(".dsomop_handle_",.get_session("labels")$res_symbol))
  validation <- dsOMOP:::.planValidate(handle,plan)
  if(!isTRUE(validation$valid))stop(paste(validation$errors,collapse="; "))
  symbols <- setNames(paste0("Eq_",substr(.planNormalizedDigest(plan)$sha256,1L,8L),"_",mode,"_",names(plan$outputs)),names(plan$outputs))
  rejection <- character()
  reject_entry <- dsOMOP:::.rejectionEntry
  testthat::local_mocked_bindings(.rejectionEntry=function(e) {
    rejection <<- unique(c(rejection,conditionMessage(e)))
    reject_entry(e)
  },.package="dsOMOP")
  assign_expr <- DSI::datashield.assign.expr
  testthat::local_mocked_bindings(datashield.assign.expr=function(...) {
    args <- list(...); callback <- args$error
    args$error <- function(server,message) {
      rejection <<- unique(c(rejection,message))
      if(is.function(callback))callback(server,message)
    }
    do.call(assign_expr,args,quote=TRUE)
  },.package="DSI")
  result <- tryCatch(ds.omop.plan.execute(plan,out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode),
    error=function(e)stop(paste(c(conditionMessage(e),rejection),collapse="\n")))
  books <- local({
    testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
      datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
    list(recipe_codebook(recipe),recipe_codebook(result))
  })
  .equivalence_identical(names(books[[1L]]),names(books[[2L]]),info=label)
  for(key in names(books[[1L]])) {
    a <- books[[1L]][[key]]$columns; b <- books[[2L]][[key]]$columns
    info <- paste(label,key)
    # Long has an explicitly open source schema; all declared columns must be
    # correct, while its complete published inventory must match every frame.
    if(startsWith(key,"long/"))expect_true(all(a$name %in% b$name),info=info)
    else .equivalence_identical(a$name,b$name,info=info)
    at <- match(a$name,b$name)
    expect_false(anyNA(at),info=info)
    .equivalence_identical(a$r_type,b$r_type[at],info=info)
    parts <- strsplit(key,"/",fixed=TRUE)[[1L]]
    target <- paste0(symbols[[parts[1L]]],if(parts[2L]!=parts[1L])paste0(".",parts[2L]))
    for(site in names(ctx$servers)) {
      server <- ctx$servers[[site]]
      object <- server$getSessionData(server$getSessionIds()[[1L]],target)
      if(mode=="staged") {
        expect_s3_class(object,"FlowerDatasetDescriptor")
        .equivalence_identical(object$metadata$format,"parquet")
        frames <- list()
        dsOMOP:::.publishReadChunks(object,function(x)frames[[length(frames)+1L]] <<- x)
      } else frames <- list(object)
      for(frame in frames) {
        .equivalence_identical(names(frame),b$name,info=info)
        for(i in seq_len(nrow(a))) {
          x <- frame[[a$name[i]]]
          type <- if(is.ordered(x))"ordered_factor" else if(is.factor(x))"factor" else
            if(inherits(x,"Date"))"Date" else if(inherits(x,"integer64"))"integer64" else typeof(x)
          .equivalence_identical(a$r_type[i],type,info=paste(info,a$name[i],site))
          if(is.factor(x)) {
            .equivalence_identical(a$ordered[[i]],is.ordered(x),info=info)
            .equivalence_identical(levels(x),unlist(b$levels[[at[i]]]$labels,use.names=FALSE),info=info)
            if(!is.null(a$levels[[i]])) {
              .equivalence_identical(unlist(a$levels[[i]]$keys,use.names=FALSE),unlist(b$levels[[at[i]]]$keys,use.names=FALSE),info=info)
              .equivalence_identical(unlist(a$levels[[i]]$labels,use.names=FALSE),levels(x),info=info)
            }
            if(!is.null(a$reference[[i]])&&!is.na(a$reference[[i]])).equivalence_identical(a$reference[[i]],b$reference[[at[i]]],info=info)
            .equivalence_identical(a$contrasts[[i]]$kind,if(is.ordered(x))"polynomial" else "treatment",info=info)
            if(length(levels(x))>1L) {
              ref <- match(a$reference[[i]],unlist(a$levels[[i]]$keys,use.names=FALSE))
              expected <- if(is.ordered(x))stats::contr.poly(nlevels(x)) else
                stats::contr.treatment(nlevels(x),base=if(length(ref)==1L&&!is.na(ref))ref else 1L)
              expect_equal(unname(contrasts(x)),unname(expected),info=info)
            }
          }
        }
      }
    }
  }
}

.equivalence_recipe <- function(date,number,category,scope,matrices=FALSE) {
  e <- omop_event("observation",45000120L,name="e")
  count <- omop_variable(event=e,reducer=omop_reduce("count"),broadcast=TRUE)
  flag <- omop_variable(event=e,reducer=omop_reduce("binary"),broadcast=TRUE)
  when <- omop_date(omop_anchor(e),broadcast=TRUE)
  if(scope=="variable") {
    count <- omop_number(count,number)
    if(!is.null(category$binary))flag <- omop_category(flag,category)
    when <- omop_date(omop_anchor(e),date,broadcast=TRUE)
    if(!is.null(category$ordered) && date$precision=="month")when <- omop_category(when,category)
    if(!is.null(category$ordered) && identical(number$representation,"bins"))count <- omop_category(count,category)
  }
  event_value <- omop_number_rows(e)
  if(scope=="variable") {
    event_value <- omop_number(event_value,number)
    if(!is.null(category$ordered) && identical(number$representation,"bins"))event_value <- omop_category(event_value,category)
  }
  vars <- list(n=count,flag=flag,when=when,event_value=event_value)
  selected <- c("n","flag","when")
  outputs <- list(
    omop_output("wide",format="wide",rows="p",columns=selected),
    omop_output("long",format="long",rows="p",sources=list(e),columns=selected),
    omop_output("intervals",format="intervals",rows="p",columns=c("event_value","when"),options=list(sources=list(e))),
    omop_output("membership",format="membership",rows="p",columns=character()),
    omop_output("dictionary",format="dictionary",rows="p",options=list(source_outputs="long")),
    omop_output("panel",format="person_period",rows="p",columns="when",
      options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count"))))
  for(style in c("survival","competing_risk","counting_process","recurrent_events","multi_state")) {
    opts <- list(format=style,outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end"))
    if(style=="multi_state") {opts$transitions<-list(list(from="index",to="lab"));opts$initial_state<-"index"}
    outputs[[length(outputs)+1L]] <- omop_output(style,format="survival",rows="p",columns=selected,options=opts)
  }
  outputs[[length(outputs)+1L]] <- omop_output("joined",format="joined",rows="p",grain="episode",
    options=list(from=list(list(output="membership"),list(output="wide")),cardinality="one_to_one"))
  if(matrices) {
    outputs[[length(outputs)+1L]] <- omop_output("sparse",format="sparse",rows="p",columns=character(),options=list(sources=list(e),analyses=c("binary","count")))
    outputs[[length(outputs)+1L]] <- omop_output("temporal",format="temporal_covariates",rows="p",
      options=list(sources=list(e),analyses=c("binary","count"),bin_width=365L,window=omop_window(0,365)))
  }
  omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),variables=vars,outputs=outputs,
    date_spec=if(scope=="recipe")date,number_spec=if(scope=="recipe")number,
    category_spec=if(scope=="recipe")category,options=list(translate_concepts=FALSE))
}

# Full product of type-changing presentation axes; matrices participate in all
# numeric-compatible cells (bins are explicitly forbidden by their contract).
.equivalence_matrix <- function(dn,categories=c("none","nominal","flags")) for(scope in c("recipe","variable")) local({
  scope <- scope
  test_that(paste("prospective execution equivalence matrix",scope,dn), {
    ctx <- .clf_setup(); .numeric_lexer()
    # The full product is site-local; the independent endpoint regressions
    # exercise all three heterogeneous sites and both transport implementations.
    ctx$conns <- ctx$conns[1L]; ctx$servers <- ctx$servers[1L]
    for(nn in c("identity","round","bins"))
      for(cn in categories) {
        label <- paste(scope,dn,nn,cn)
        date <- switch(dn,day=omop_date_spec(precision="day"),
          # Include lagged counting-process starts and the observation-end
          # anchor within the fixture's unchanged 40-level cap.
          month=omop_date_spec(precision="month",level_range=c("2019-01-01","2021-12-31"),
            labels=c("2024-12"="2024-12")),
          relative=omop_date_spec(precision="relative_days",anchor="index"))
        number <- switch(nn,identity=omop_number_spec(),round=omop_number_spec(rounding=omop_number_round("floor")),
          bins=omop_number_spec(representation="bins",breaks=c(-Inf,2,Inf),labels=c("few","many")))
        category <- switch(cn,none=NULL,nominal=omop_category_spec(ordered=FALSE),
          flags=omop_category_spec(binary=omop_category_binary("factor")),
          logical=omop_category_spec(binary=omop_category_binary("logical")))
        recipe <- .equivalence_recipe(date,number,category,scope,matrices=nn!="bins"||scope=="variable")
        tryCatch(.equivalence_books(recipe,ctx,"memory",paste(label,"memory")),
          error=function(e) {message(label,": ",conditionMessage(e)); stop(e)})
      }
  })
})
