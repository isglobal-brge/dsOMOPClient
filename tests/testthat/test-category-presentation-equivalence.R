# Compare independent published/actual oracles, not one descriptor helper to itself.
.equivalence_books <- function(recipe,ctx,mode,label) {
  plan <- recipe_to_plan(recipe,version=2)
  symbols <- setNames(paste0("Eq_",names(plan$outputs)),names(plan$outputs))
  result <- ds.omop.plan.execute(plan,out=symbols,symbol="labels",conns=ctx$conns,output_mode=mode)
  books <- local({
    testthat::local_mocked_bindings(datashield.aggregate=function(...)stop("unexpected network"),
      datashield.assign.expr=function(...)stop("unexpected network"),.package="DSI")
    list(recipe_codebook(recipe),recipe_codebook(result))
  })
  expect_identical(names(books[[1L]]),names(books[[2L]]),info=label)
  for(key in names(books[[1L]])) {
    a <- books[[1L]][[key]]$columns; b <- books[[2L]][[key]]$columns
    info <- paste(label,key)
    # Long has an explicitly open source schema; all declared columns must be
    # correct, while its complete published inventory must match every frame.
    if(startsWith(key,"long/"))expect_true(all(a$name %in% b$name),info=info)
    else expect_identical(a$name,b$name,info=info)
    at <- match(a$name,b$name)
    expect_false(anyNA(at),info=info)
    expect_identical(a$r_type,b$r_type[at],info=info)
    parts <- strsplit(key,"/",fixed=TRUE)[[1L]]
    target <- paste0(symbols[[parts[1L]]],if(parts[2L]!=parts[1L])paste0(".",parts[2L]))
    for(site in names(ctx$servers)) {
      server <- ctx$servers[[site]]
      object <- server$getSessionData(server$getSessionIds()[[1L]],target)
      if(mode=="staged") {
        expect_s3_class(object,"FlowerDatasetDescriptor")
        expect_identical(object$metadata$format,"parquet")
        frames <- list()
        dsOMOP:::.publishReadChunks(object,function(x)frames[[length(frames)+1L]] <<- x)
      } else frames <- list(object)
      for(frame in frames) {
        expect_identical(names(frame),b$name,info=info)
        for(i in seq_len(nrow(a))) {
          x <- frame[[a$name[i]]]
          type <- if(is.ordered(x))"ordered_factor" else if(is.factor(x))"factor" else
            if(inherits(x,"Date"))"Date" else if(inherits(x,"integer64"))"integer64" else typeof(x)
          expect_identical(a$r_type[i],type,info=paste(info,a$name[i],site))
          if(is.factor(x)) {
            expect_identical(a$ordered[[i]],is.ordered(x),info=info)
            expect_identical(levels(x),unlist(b$levels[[at[i]]]$labels,use.names=FALSE),info=info)
            if(!is.null(a$levels[[i]])) {
              expect_identical(unlist(a$levels[[i]]$keys,use.names=FALSE),unlist(b$levels[[at[i]]]$keys,use.names=FALSE),info=info)
              expect_identical(unlist(a$levels[[i]]$labels,use.names=FALSE),levels(x),info=info)
            }
            if(!is.null(a$reference[[i]])&&!is.na(a$reference[[i]]))expect_identical(a$reference[[i]],b$reference[[at[i]]],info=info)
            expect_identical(a$contrasts[[i]]$kind,if(is.ordered(x))"polynomial" else "treatment",info=info)
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
  count <- omop_variable(event=e,reducer=omop_reduce("count"))
  flag <- omop_variable(event=e,reducer=omop_reduce("binary"))
  when <- omop_date(omop_anchor(e))
  if(scope=="variable") {
    count <- omop_number(count,number)
    flag <- omop_category(flag,category)
    when <- omop_category(omop_date(omop_anchor(e),date),category)
  }
  vars <- list(n=count,flag=flag,when=when)
  outputs <- list(
    omop_output("wide",format="wide",rows="p",columns=names(vars)),
    omop_output("long",format="long",rows="p",sources=list(e),columns=names(vars)),
    omop_output("intervals",format="intervals",rows="p",columns=names(vars),options=list(sources=list(e))),
    omop_output("membership",format="membership",rows="p",columns=character()),
    omop_output("dictionary",format="dictionary",rows="p",options=list(source_outputs="long")),
    omop_output("panel",format="person_period",rows="p",columns=names(vars),
      options=list(sources=list(e),window=omop_window(0,20),bins=list(width=10),analyses=c("binary","count"))))
  for(style in c("survival","competing_risk","counting_process","recurrent_events","multi_state")) {
    opts <- list(format=style,outcomes=list(lab=e),tar=omop_window(0,365),censor=list("observation_end"))
    if(style=="multi_state") {opts$transitions<-list(list(from="index",to="lab"));opts$initial_state<-"index"}
    outputs[[length(outputs)+1L]] <- omop_output(style,format="survival",rows="p",columns=names(vars),options=opts)
  }
  outputs[[length(outputs)+1L]] <- omop_output("joined",format="joined",rows="p",grain="episode",columns=names(vars),
    options=list(from=list(list(output="survival"),list(output="wide")),cardinality="one_to_one"))
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
for(scope in c("recipe","variable")) for(date_name in c("day","month","relative"))
  for(number_name in c("identity","round","bins")) for(category_name in c("none","nominal","flags")) {
    local({
      scope <- scope; dn <- date_name; nn <- number_name; cn <- category_name
      label <- paste(scope,dn,nn,cn)
      test_that(paste("prospective execution equivalence",label), {
        ctx <- .clf_setup(); .numeric_lexer()
        date <- switch(dn,day=omop_date_spec(precision="day"),
          month=omop_date_spec(precision="month",level_range=c("2020-01-01","2021-12-31")),
          relative=omop_date_spec(precision="relative_days",anchor="index"))
        number <- switch(nn,identity=omop_number_spec(),round=omop_number_spec(rounding=omop_number_round("floor")),
          bins=omop_number_spec(representation="bins",breaks=c(-Inf,2,Inf),labels=c("few","many")))
        category <- switch(cn,none=NULL,nominal=omop_category_spec(ordered=FALSE),
          flags=omop_category_spec(binary=omop_category_binary("factor")))
        recipe <- .equivalence_recipe(date,number,category,scope,matrices=nn!="bins"||scope=="variable")
        .equivalence_books(recipe,ctx,"memory",paste(label,"memory"))
      })
    })
  }
