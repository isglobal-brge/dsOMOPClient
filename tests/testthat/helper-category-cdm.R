.category_format_recipe <- function(id) {
  lab<-omop_event("measurement",id,name="lab")
  base<-omop_variable(event=lab,reducer=omop_reduce("count"))
  binary<-omop_variable(event=lab,reducer=omop_reduce("binary"))
  repeat_values<-omop_number_bins(omop_number_rows(lab),breaks=c(-Inf,12,15,Inf))
  repeated<-omop_repeated(lab,list(value=repeat_values),timepoints=omop_timepoints(list(baseline=omop_timepoint(),m6=omop_timepoint(6,"months",c(-7,7)))))
  outputs<-list(
    omop_output("wide",format="wide",rows="p",grain="episode"),
    omop_output("baseline",type="baseline",rows="p",grain="episode"),
    omop_output("features",type="features",rows="p",grain="episode"),
    omop_output("long",format="long",rows="p",sources=list(lab),options=list(columns_derived=c("value","unit","days_from_index"))),
    omop_output("intervals",format="intervals",rows="p",options=list(sources=list(lab))),
    omop_output("membership",format="membership",rows="p"),
    omop_output("dictionary",format="dictionary",rows="p",options=list(source_outputs=c("long"))),
    omop_output("panel",format="person_period",rows="p",columns=character(),options=list(sources=list(lab),window=omop_window(0,30),bins=list(width=31L),analyses=c("binary","count"))),
    omop_output("sparse",format="sparse",rows="p",columns=character(),options=list(sources=list(lab),analyses=c("binary","count"))),
    omop_output("temporal",format="temporal_covariates",rows="p",options=list(sources=list(lab),analyses=c("binary","count"),bin_width=3651L,window=omop_window(0,3650))),
    omop_output("survival",format="survival",rows="p",options=list(outcomes=list(lab=lab),tar=omop_window(1,3650),censor=list("observation_end"))),
    omop_output("joined",format="joined",rows="p",grain="episode",options=list(from=list(list(output="survival"),list(output="wide")),cardinality="one_to_one")))
  omop_recipe(events=list(lab),populations=omop_population("p",index=omop_index(lab)),variables=list(
    count=base,band=omop_number_bins(base,breaks=c(-Inf,5,10,Inf)),
    flag=omop_category(binary,omop_category_spec(binary=omop_category_binary("logical"))),
    flag_factor=omop_category(binary,omop_category_spec(binary=omop_category_binary("factor"))),
    repeated=repeated),outputs=outputs,category_spec=omop_category_spec(ordered=FALSE))
}

.category_gibleed_sites <- function(n=3L,env=parent.frame()) {
  source<-Sys.getenv("DSOMOP_TEST_CATEGORY_GIBLEED")
  if(!nzchar(source))testthat::skip("opt-in retained GiBleed CDM")
  if(!file.exists(source))stop("Missing pinned GiBleed fixture")
  base<-withr::local_tempdir(.local_envir=env);Sys.chmod(base,"0700")
  withr::local_envvar(c(DSOMOP_STATE_DIR=base,DSOMOP_TEST_ALLOW_EPHEMERAL_STATE="1",DSOMOP_PSEUDONYM_PROVIDER="file",DSOMOP_PSEUDONYM_ROOT="",DSOMOP_PSEUDONYM_REQUIRE_EXISTING="false",DSOMOP_PSEUDONYM_KEY=""),.local_envir=env)
  withr::local_options(list(dsstaging.base_dir=base),.local_envir=env)
  servers<-list();login<-DSI::newDSLoginBuilder();paths<-character()
  for(i in seq_len(n)) {
    path<-file.path(base,paste0("site",i,".sqlite"));stopifnot(file.copy(source,path))
    db<-DBI::dbConnect(RSQLite::SQLite(),path)
    # Fixed roster-only cohort keeps this all-format transport test targeted;
    # it never selects on a category, measurement, outcome or missingness.
    keep<-DBI::dbGetQuery(db,"SELECT person_id FROM person ORDER BY person_id LIMIT 108")$person_id
    for(table in DBI::dbListTables(db))if("person_id" %in% DBI::dbListFields(db,table))DBI::dbExecute(db,paste0("DELETE FROM ",DBI::dbQuoteIdentifier(db,table)," WHERE person_id % ",n," != ",i-1L," OR person_id NOT IN (",paste(keep,collapse=","),")"))
    DBI::dbDisconnect(db);paths<-c(paths,path)
    resource<-resourcer::newResource(name="numeric",url=paste0("omop+dbi:sqlite://",path),format="omop.dbi.db")
    config<-DSLite::defaultDSConfiguration(include=c("dsOMOP","dsBase"))
    config$Options[["nfilter.subset"]]<-"5";config$Options[["nfilter.tab"]]<-"3"
    server<-DSLite::newDSLiteServer(resources=list(numeric=resource),config=config)
    name<-paste0("cats_gibleed_",i);assign(name,server,globalenv())
    local({nm<-name;withr::defer(rm(list=nm,envir=globalenv()),envir=env)})
    servers[[paste0("site",i)]]<-server;login$append(server=paste0("site",i),url=name,driver="DSLiteDriver")
  }
  conns<-DSI::datashield.login(login$build());withr::defer(DSI::datashield.logout(conns),envir=env)
  ds.omop.connect(resource="numeric",symbol="numeric",conns=conns)
  withr::defer(ds.omop.disconnect("numeric",conns=conns),envir=env)
  list(conns=conns,servers=servers,paths=paths)
}
