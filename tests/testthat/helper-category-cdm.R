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
    for(table in DBI::dbListTables(db))if("person_id" %in% DBI::dbListFields(db,table))DBI::dbExecute(db,paste0("DELETE FROM ",DBI::dbQuoteIdentifier(db,table)," WHERE person_id % ",n," != ",i-1L))
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
