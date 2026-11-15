# Two public, synthetic Eunomia resources on one real DSLite server.
# Run the administrator loader first; analyst lessons never read DB rows.
book_practice <- function(resource=c("gibleed","numeric"), env=parent.frame(),
                          resource_names=c(gibleed="gibleed", numeric="numeric"), symbol="omop") {
  force(env)
  resource<-match.arg(resource)
  library(dsOMOP); library(dsOMOPClient); library(DSLite)
  directory<-Sys.getenv("BOOK_PRACTICE_DIR",file.path("..","practice-data"))
  paths<-setNames(file.path(directory,c("gibleed.sqlite","numeric.sqlite")),c("gibleed","numeric"))
  stopifnot(all(file.exists(paths)))
  paths<-setNames(normalizePath(paths),names(paths))
  lexer<-get("Lexer",asNamespace("DSLite"))
  old_double<-lexer$public_fields$t_DBLSTRING;old_single<-lexer$public_fields$t_STRING
  withr::defer({lexer$public_fields$t_DBLSTRING<-old_double;lexer$public_fields$t_STRING<-old_single},envir=env)
  lexer$public_fields$t_DBLSTRING<-r"("([^"\\]|\\.)*")"
  lexer$public_fields$t_STRING<-r"('([^'\\]|\\.)*')"
  withr::local_options(list(nfilter.subset=5,default.nfilter.subset=5,
    dsomop.pseudonym_provider="injected",
    dsomop.pseudonym_root=as.raw(openssl::sha256(charToRaw("public-book-practice")))),.local_envir=env)
  resources<-lapply(names(paths),function(name) resourcer::newResource(name=resource_names[[name]],
    url=paste0("omop+dbi:sqlite://",paths[[name]]),format="omop.dbi.db"))
  names(resources)<-unname(resource_names[names(paths)])
  config<-DSLite::defaultDSConfiguration(include=c("dsOMOP","dsBase"))
  config$Options$nfilter.subset<-5;config$Options$default.nfilter.subset<-5
  server<-DSLite::newDSLiteServer(resources=resources,config=config)
  withr::local_options(list(datashield.env=environment()),.local_envir=env)
  builder<-DSI::newDSLoginBuilder()
  builder$append(server="site",url="server",driver="DSLiteDriver")
  conns<-DSI::datashield.login(builder$build(),assign=FALSE)
  ds.omop.connect(resource=resource_names[[resource]],conns=conns,symbol=symbol)
  # The permanent public database is not a temporary per-chapter fixture.
  list(conns=conns,server=server,path=character(),database=paths[[resource]],resource=resource)
}

# Offline-only administrator fixture: disjoint halves of public synthetic people.
# No records are invented. The permanent practice resources are never modified.
book_numeric_pair <- function(env=parent.frame(), resource=c("numeric", "gibleed")) {
  resource <- match.arg(resource)
  force(env)
  base <- book_practice(resource, env=env, symbol="book_pool")
  ds.omop.disconnect(conns=base$conns, symbol="book_pool")
  DSI::datashield.logout(base$conns)
  source_db <- DBI::dbConnect(RSQLite::SQLite(), base$database)
  ids <- sort(DBI::dbReadTable(source_db, "person")$person_id)
  DBI::dbDisconnect(source_db)
  groups <- split(ids, rep(c("north", "south"), length.out=length(ids)))
  paths <- setNames(vapply(groups, function(x) tempfile("book-numeric-", fileext=".sqlite"), ""), names(groups))
  complete <- FALSE
  on.exit(if (!complete) unlink(paths), add=TRUE)
  servers <- lapply(names(groups), function(site) {
    stopifnot(file.copy(base$database, paths[[site]]))
    db <- DBI::dbConnect(RSQLite::SQLite(), paths[[site]])
    tryCatch({
      for (table in DBI::dbListTables(db)) {
        if ("person_id" %in% DBI::dbListFields(db, table)) {
          DBI::dbExecute(db, paste0("DELETE FROM ", DBI::dbQuoteIdentifier(db, table),
            " WHERE person_id NOT IN (", paste(groups[[site]], collapse=","), ") OR person_id IS NULL"))
        }
      }
    }, finally=DBI::dbDisconnect(db))
    resource <- resourcer::newResource(name="numeric",
      url=paste0("omop+dbi:sqlite://", paths[[site]]), format="omop.dbi.db")
    config <- DSLite::defaultDSConfiguration(include=c("dsOMOP", "dsBase"))
    config$Options$nfilter.subset <- 5
    config$Options$default.nfilter.subset <- 5
    DSLite::newDSLiteServer(resources=list(numeric=resource), config=config)
  })
  north <- servers[[1L]]; south <- servers[[2L]]
  withr::local_options(list(datashield.env=environment()), .local_envir=env)
  builder <- DSI::newDSLoginBuilder()
  for (site in names(groups)) builder$append(server=site, url=site, driver="DSLiteDriver")
  conns <- DSI::datashield.login(builder$build(), assign=FALSE)
  ds.omop.connect(resource="numeric", conns=conns, symbol="book_pool")
  complete <- TRUE
  list(conns=conns, path=paths, groups=groups)
}
