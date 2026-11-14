# Two public, synthetic Eunomia resources on one real DSLite server.
# Run the administrator loader first; analyst lessons never read DB rows.
book_practice <- function(resource=c("gibleed","numeric"), env=parent.frame(),
                          resource_names=c(gibleed="gibleed", numeric="numeric")) {
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
  ds.omop.connect(resource=resource_names[[resource]],conns=conns)
  # The permanent public database is not a temporary per-chapter fixture.
  list(conns=conns,server=server,path=character(),database=paths[[resource]],resource=resource)
}
