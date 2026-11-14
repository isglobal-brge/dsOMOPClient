test_that("Q31 native-v2 lesson renders actual two-site published workflows", {
  expect_true(requireNamespace("rmarkdown",quietly=TRUE));expect_true(rmarkdown::pandoc_available())
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"DELETE FROM measurement")
    x<-expand.grid(record=1:4,person_id=1:18)
    DBI::dbWriteTable(con,"measurement",data.frame(measurement_id=seq_len(nrow(x)),person_id=x$person_id,
      measurement_concept_id=3000963L,measurement_date=ifelse(x$record<4,"2020-01-01","2020-01-02"),
      measurement_type_concept_id=0L,value_as_number=c(3,3,9,15)[x$record],unit_concept_id=8554L),append=TRUE)
    DBI::dbDisconnect(con)
  }
  out<-Sys.getenv("DSOMOP_AUDIT_ARTIFACTS",tempdir());dir.create(out,recursive=TRUE,showWarnings=FALSE)
  result<-rmarkdown::render(system.file("book","audit-native-v2.Rmd",package="dsOMOPClient"),
    output_file="audit-native-v2.html",output_dir=out,
    params=list(conns=ctx$conns[1:2],resource="labels"),envir=new.env(parent=globalenv()),quiet=TRUE)
  expect_true(file.exists(result))
  text<-paste(readLines(result,warn=FALSE),collapse="\n")
  expect_false(grepl("SECRET_SENTINEL|PRIVATE_SENTINEL",text))
  cat("\nRENDERED_LESSON_SHA256",paste(openssl::sha256(file(result,"rb"))),"\n")
})
