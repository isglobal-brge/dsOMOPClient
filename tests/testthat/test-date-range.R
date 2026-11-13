test_that("B4 custom public ranges order all years independently of observed dates", {
  range <- c("2000-01-01", "2010-12-31")
  days <- seq(as.Date(range[1]), as.Date(range[2]), by="day")
  observations <- list(empty=as.Date(character()), missing=as.Date(NA_character_),
    endpoint=as.Date("2000-01-01"), middle=as.Date("2005-06-15"),
    disjoint=as.Date(c("2030-01-01", "1980-12-31")))
  for (pattern in c("%Y", "%m/%Y", "%G-W%V")) {
    spec <- .dateEffective(list(precision="custom", pattern=pattern, level_range=range))
    public <- unique(format(days, pattern))
    for (observed in observations) {
      value <- .datePresent(list(lo=observed, hi=observed), spec)
      expected <- unique(format(sort(unique(c(days, observed[!is.na(observed)]))), pattern))
      expect_true(is.ordered(value))
      expect_identical(as.character(value), format(observed, pattern))
      expect_identical(levels(value), expected)
      contract <- attr(value, "omop_date_contract")
      expect_identical(contract$declared, public)
      expect_true(all(diff(contract$order)>0))
      # Adding observations must not change the public levels or their order.
      expect_identical(levels(value)[levels(value) %in% public], public)
    }
  }
  expect_identical(unique(format(days, "%Y")), as.character(2000:2010))
  expect_identical(unique(format(days, "%m/%Y")),
    sprintf("%02d/%04d", rep(1:12, 11), rep(2000:2010, each=12)))
  expect_identical(head(unique(format(days, "%G-W%V")), 1), "1999-W52")
  expect_true("2004-W53" %in% unique(format(days, "%G-W%V")))
  expect_identical(tail(unique(format(days, "%G-W%V")), 1), "2010-W52")
})

test_that("B4 custom ranges retain shared unused levels through final publication", {
  f <- dates_va_fixture(two_sites=TRUE); conns <- f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  range <- c("2000-01-01", "2010-12-31")
  days <- seq(as.Date(range[1]), as.Date(range[2]), by="day")
  patterns <- c(year="%Y", month="%m/%Y", week="%G-W%V")
  sources <- list(endpoint="2000-01-01", disjoint="first_visit",
    missing=omop_date_components(2024L,2L,30L))
  vars <- list()
  for (name in names(patterns)) for (source in names(sources)) {
    vars[[paste(name,source,sep="_")]] <- omop_date(sources[[source]],
      omop_date_spec(precision="custom",pattern=patterns[[name]],level_range=range))
  }
  vars$builtin <- omop_date("2000-01-01",omop_date_spec(precision="year",level_range=range))
  r <- omop_recipe(variables=vars,outputs=omop_output("o",format="wide",grain="person"))
  expect_identical(recipe_execute(r,out=c(o="D_range"),conns=conns),c(o="D_range"))
  frames <- DSLite::getDSLiteData(conns,"D_range")
  visits <- list(site=as.Date(c("2020-06-15","2020-03-14","2020-07-20",NA)),
    site2=as.Date(c("2020-06-15","2020-06-15","2020-07-20",NA)))
  all_visits <- do.call(c,visits)
  for (site in names(frames)) {
    d <- frames[[site]]
    expect_equal(nrow(d),24L)
    expect_identical(levels(d$year_endpoint),as.character(2000:2010))
    expect_identical(as.character(d$year_endpoint),rep("2000",24))
    expect_identical(levels(d$year_endpoint),levels(d$builtin))
    for (name in names(patterns)) for (source in names(sources)) {
      col <- paste(name,source,sep="_"); pattern <- patterns[[name]]
      observed <- switch(source,endpoint=as.Date("2000-01-01"),
        disjoint=visits[[site]],missing=as.Date(NA_character_))
      domain <- if(source=="disjoint") c(days,all_visits[!is.na(all_visits)]) else days
      expected <- unique(format(sort(unique(domain)),pattern))
      expect_true(is.ordered(d[[col]]))
      expect_identical(as.character(d[[col]]),rep(format(observed,pattern),length.out=24))
      expect_identical(levels(d[[col]]),expected)
      expect_identical(levels(d[[col]]),levels(frames$site[[col]]))
    }
  }
  expect_identical(recipe_execute(r,out=c(o="D_range_reverse"),conns=rev(conns)),c(o="D_range_reverse"))
  reversed <- DSLite::getDSLiteData(conns,"D_range_reverse")
  for (site in names(frames)) for (col in names(vars)) {
    expect_identical(levels(reversed[[site]][[col]]),levels(frames[[site]][[col]]))
  }
})
