test_that("B1 native reducer policies survive public recipe assignment", {
  f <- dates_va_fixture(); conns <- f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  for (scope in c("variable","recipe")) {
    spec <- omop_date_spec(future="na",as_of="2020-06-15")
    local_spec <- if(scope=="variable") spec else NULL
    event <- omop_event("visit_occurrence")
    vars <- list(helper=omop_date("first_visit",local_spec),
      first=omop_variable(event=event,reducer=omop_reduce("first_date"),date_spec=local_spec),
      last=omop_variable(event=event,reducer=omop_reduce("last_date"),date_spec=local_spec),
      value=omop_variable(event=event,reducer=omop_reduce("first_value",value="visit_start_date"),date_spec=local_spec))
    r <- omop_recipe(variables=vars,outputs=omop_output("o",format="wide",grain="person"),
                     date_spec=if(scope=="recipe") spec else NULL)
    recipe_execute(r,out=c(o="D_future"),conns=conns)
    d <- DSLite::getDSLiteData(conns,"D_future")$site
    for (col in c("helper","first","value")) {
      expect_identical(d[[col]],rep(as.Date(c("2020-06-15","2020-03-14",NA,NA)),6))
    }
    expect_s3_class(d$last,"Date");expect_true(all(is.na(d$last)))
  }
})

test_that("B2 reference calendar order survives federation, contrasts and staging", {
  directory <- withr::local_tempdir();Sys.chmod(directory,"0700")
  withr::local_options(list(dsstaging.base_dir=directory))
  f <- dates_va_fixture(two_sites=TRUE);conns <- f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  expected <- list(abbr=c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"),
    full=c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"),
    weekday=c("6","7","1","2","3","4","5"),
    escaped=paste("%Y",c("Sat","Sun","Mon","Tue","Wed","Thu","Fri")))
  patterns <- c(abbr="%a",full="%A",weekday="%u",escaped="%%Y %a")
  vars <- lapply(patterns,function(pattern) omop_date("2020-01-01",
    omop_date_spec(precision="custom",pattern=pattern,level_range=c("2020-01-01","2020-01-07"))))
  r <- omop_recipe(variables=vars,outputs=omop_output("o",format="wide",grain="person"))
  recipe_execute(r,out=c(o="D_weekday"),conns=conns)
  memory <- DSLite::getDSLiteData(conns,"D_weekday")
  recipe_execute(r,out=c(o="D_weekday_stage"),conns=rev(conns),output_mode="staged")
  staged <- DSLite::getDSLiteData(conns,"D_weekday_stage")
  for (site in names(memory)) {
    chunks <- list()
    dsOMOP:::.publishReadChunks(staged[[site]],function(c) chunks[[length(chunks)+1L]]<<-c)
    restored <- do.call(rbind,chunks)
    for (col in names(expected)) {
      expect_true(is.ordered(memory[[site]][[col]]))
      expect_identical(levels(memory[[site]][[col]]),expected[[col]])
      expect_identical(contrasts(memory[[site]][[col]]),contrasts(ordered(expected[[col]],levels=expected[[col]])))
      expect_equal(restored[[col]],memory[[site]][[col]])
    }
    expect_identical(as.character(memory[[site]]$abbr),rep("Wed",24))
  }
  # Exercise the client's shared calendar implementation independently as well.
  for (col in names(patterns)) {
    spec <- .dateEffective(list(precision="custom",pattern=patterns[[col]],level_range=c("2020-01-01","2020-01-07")))
    value <- .datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec)
    expect_identical(levels(value),expected[[col]])
  }
})

test_that("B3 client patterns distinguish escaped and active adjacent directives", {
  patterns <- c("%%Y %G-W%V","%%V %Y","%%%Y","%%%%%Y","%%Y %a","%Y%%","%%G%%V%Y")
  expected <- c("%Y 2020-W01","%V 2020","%2020","%%2020","%Y Wed","2020%","%G%V2020")
  f <- dates_va_fixture();conns <- f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  vars <- setNames(lapply(patterns,function(pattern) omop_date("2020-01-01",
    omop_date_spec(precision="custom",pattern=pattern))),paste0("d",seq_along(patterns)))
  recipe_execute(omop_recipe(variables=vars,outputs=omop_output("o",format="wide",grain="person")),
                 out=c(o="D_escaped"),conns=conns)
  d <- DSLite::getDSLiteData(conns,"D_escaped")$site
  for (i in seq_along(patterns)) {
    expect_identical(as.character(d[[paste0("d",i)]]),rep(expected[i],24))
    spec <- .dateEffective(omop_date_spec(precision="custom",pattern=patterns[i],labels=setNames("Public",expected[i])))
    value <- .datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec)
    expect_identical(as.character(value),"Public")
    spec$labels <- setNames("Bad",paste0(expected[i],"x"))
    expect_error(.datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec),"Label keys")
  }
  for (pattern in c("%Y-W%V","%%%Y-W%V","%G-%%V","%%G-W%V","%Y%")) {
    expect_error(omop_date_spec(precision="custom",pattern=pattern),"date_pattern_invalid")
  }
})
