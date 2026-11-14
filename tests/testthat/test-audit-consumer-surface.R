test_that("Q23 records the installed published consumer interfaces", {
  for(package in c("dsOMOP","dsOMOPClient","dsBase","dsBaseClient","DSLite","arrow")) {
    expect_true(requireNamespace(package,quietly=TRUE))
    cat("\nCONSUMER_VERSION",package,as.character(utils::packageVersion(package)),"\n")
  }
  for(name in c("ds.mean","ds.quantileMean","ds.glm","ds.reShape","ds.lmerSLMA","ds.glmerSLMA","ds.getWGSR")) {
    f<-getExportedValue("dsBaseClient",name)
    expect_true(is.function(f))
    cat("\nCONSUMER_FORMALS",name,paste(deparse(formals(f)),collapse=" "),"\n")
  }
})
