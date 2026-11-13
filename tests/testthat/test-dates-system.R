test_that("D43 native verbal-autopsy wide table is complete on DSLite", {
  f<-dates_va_fixture();conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  snippet<-new.env(parent=environment())
  sys.source(testthat::test_path("..","..","tools","dates","va-native.R"),snippet)
  va<-snippet$va_native_recipe()
  expect_identical(recipe_execute(va,out=c(people="D_va"),conns=conns),c(people="D_va"))
  # Test harness only: synthetic fixture rows never enter the analyst snippet.
  d<-DSLite::getDSLiteData(conns,"D_va")$site
  expect_equal(nrow(d),24L);expect_equal(length(unique(d$person_id)),24L)
  expect_true(all(grepl("^p2",d$person_id)))
  expect_identical(d$age_at_death,rep(c(43L,33L,NA_integer_,NA_integer_),6))
  expect_identical(d$age_at_first_visit,rep(c(39L,29L,NA_integer_,NA_integer_),6))
  expect_identical(d$visit_date,rep(as.Date(c("2020-06-15","2020-03-14","2020-07-20",NA)),6))
  expect_true(is.ordered(d$visit_month));expect_true(is.ordered(d$visit_quarter))
  expect_identical(attr(d,"omop_concepts")$visit_month$kind,"date")
  expect_identical(levels(d$visit_parts_month),sprintf("%02d",1:12))
  expect_identical(d$visit_year,rep(c(2020L,2020L,2020L,NA_integer_),6))
  expect_identical(as.character(d$fever),rep(c("Yes",NA,NA,NA),6))
  expect_identical(d$death_date,rep(as.Date(c("2024-06-01","2024-03-14","2024-05-10",NA)),6))
  expect_identical(as.character(d$education),rep(c("University education","Secondary education",NA,NA),6))
  expect_identical(as.character(d$cause_of_death),rep(c("Myocardial infarction","Pneumonia","Myocardial infarction",NA),6))
})

test_that("D39 D42 API binding, closed wire, conflict and capability contracts", {
  mk<-function(vars,...) omop_recipe(variables=vars,outputs=omop_output(name="o",format="wide",grain="person"),...)
  r<-mk(list(d=omop_date("0004-02-29"),visit=omop_date_parts("first_visit")),date_spec=omop_date_spec(precision="month"))
  p<-recipe_to_plan(r,version=2L);n<-.planNormalizeV2(p)
  expect_identical(names(p$variables),c("d","visit_year","visit_month","visit_day"))
  expect_null(p$outputs$o$options$date_handling)
  expect_identical(.planNormalizeV2(n),n)
  expect_true("date.presentation" %in% .plan_v2_operations(p))
  expect_error(recipe_to_plan(r,version=1L),"v1|version 1|no v1")
  p$outputs$o$options$date_handling<-list(mode="absolute")
  expect_error(.planNormalizeV2(p),"date_policy_conflict")
  expect_error(omop_date_spec(precision="garbage"),"date_spec_invalid")
  expect_error(omop_date("today"),"date_source_invalid")
  expect_error(omop_date_spec(pattern="%Y-W%V"),"%G-W%V")
  expect_error(mk(list(v=omop_date_parts("birth",c(a="year",a="month")))),"date_parts_names")
  expect_error(mk(list(v=omop_date_parts("birth",c("year","year")))),"unique")
  expect_identical(tail(names(formals(omop_variable)),3),c("labels","column_names","date_spec"))
  expect_identical(tail(names(formals(omop_recipe)),3),c("labels","column_names","date_spec"))
  expect_match(paste(recipe_explain(r)$text,collapse=" "),"partial|Partial")
  expect_true(any(recipe_lint(r)$code=="quasi_identifier"))
})

test_that("D40 date factors pool through real ds.table with missing public periods", {
  f<-dates_va_fixture(two_sites=TRUE);conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  r<-omop_recipe(variables=list(month=omop_date("first_visit",omop_date_spec(precision="month",level_range=c("2020-03-01","2020-07-31")))),outputs=omop_output(name="o",format="wide",grain="person"))
  recipe_execute(r,out=c(o="D_month"),conns=conns)
  d<-DSLite::getDSLiteData(conns,"D_month")
  expect_identical(levels(d$site$month),c("2020-03","2020-04","2020-05","2020-06","2020-07"))
  expect_identical(levels(d$site$month),levels(d$site2$month))
  expect_identical(as.integer(table(d$site2$month)),c(0L,0L,0L,12L,6L))
  pooled<-ds.omop.table("D_month","month",conns)
  expect_equal(as.numeric(pooled),c(6,0,0,18,12))
  expect_identical(dimnames(pooled)[[1L]],levels(d$site$month))
  expect_identical(contrasts(d$site$month),contrasts(d$site2$month))
  recipe_execute(r,out=c(o="D_reverse"),conns=rev(conns))
  reversed<-DSLite::getDSLiteData(conns,"D_reverse")
  expect_identical(lapply(reversed,function(x)levels(x$month)),lapply(d,function(x)levels(x$month)))
})

test_that("D43 final threshold still refuses and rolls back the VA population", {
  f<-dates_va_fixture(threshold=25L);conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  r<-omop_recipe(variables=list(age=omop_age_at("death")),outputs=omop_output(name="o",format="wide",grain="person"))
  expect_error(recipe_execute(r,out=c(o="D_refused"),conns=conns),"insufficient_support")
  expect_false("D_refused" %in% DSI::datashield.symbols(conns)$site)
})

test_that("D41 staged date types and levels match private memory publication", {
  directory<-withr::local_tempdir();Sys.chmod(directory,"0700")
  withr::local_options(list(dsstaging.base_dir=directory))
  f<-dates_va_fixture();conns<-f$conns
  withr::defer({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)})
  r<-omop_recipe(variables=list(day=omop_date("first_visit"),quarter=omop_date("first_visit",omop_date_spec(precision="quarter")),age=omop_age_at("death")),outputs=omop_output(name="o",format="wide",grain="person"))
  recipe_execute(r,out=c(o="D_memory"),conns=conns)
  recipe_execute(r,out=c(o="D_staged"),conns=conns,output_mode="staged")
  memory<-DSLite::getDSLiteData(conns,"D_memory")$site
  descriptor<-DSLite::getDSLiteData(conns,"D_staged")$site
  chunks<-list();dsOMOP:::.publishReadChunks(descriptor,function(c)chunks[[length(chunks)+1L]]<<-c)
  staged<-do.call(rbind,chunks)
  for(col in c("day","quarter","age")) expect_equal(staged[[col]],memory[[col]])
})

test_that("D31 D32 D36 date age policies and public bands use real assignment", {
  f<-dates_va_fixture();conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  r<-omop_recipe(variables=list(annual=omop_age_at("death",unit="annual_years"),estimated=omop_age_at("death",date_spec=omop_date_spec(partial="mid")),bands=omop_age_at("death",bands=TRUE),custom=omop_age_at("death",bands=TRUE,age_breaks=c(0L,18L,19L,65L)),negative=omop_age_at("1900-01-01")),outputs=omop_output("o",format="wide",grain="person"),options=list(translate_concepts=FALSE))
  recipe_execute(r,out=c(o="D_age"),conns=conns)
  d<-DSLite::getDSLiteData(conns,"D_age")$site
  expect_identical(d$annual,rep(c(44L,34L,24L,NA_integer_),6))
  expect_identical(d$estimated,rep(c(43L,33L,23L,NA_integer_),6))
  expect_identical(levels(d$bands),c(paste0(seq(0,95,5),"-",seq(4,99,5)),"100+"))
  expect_identical(levels(d$custom),c("0-17","18-18","19-64","65+"))
  expect_true(all(is.na(d$negative)))
})

test_that("D42 date recipe JSON YAML and code preserve native semantics", {
  r<-omop_recipe(variables=list(day=omop_date(omop_date_components(2024L,2L,29L)),parts=omop_date_parts("first_visit",c("year","month")),age=omop_age_at("death")),outputs=omop_output("o",format="wide",grain="person"),date_spec=omop_date_spec(partial="none"))
  expected<-recipe_to_plan(r)
  restored<-list(recipe_import_json(recipe_export_json(r)))
  if(requireNamespace("yaml",quietly=TRUE))restored<-c(restored,list(recipe_import_yaml(recipe_export_yaml(r))))
  env<-new.env(parent=asNamespace("dsOMOPClient"));eval(parse(text=recipe_to_code(r)),env)
  restored<-c(restored,list(env$recipe))
  for(x in restored) expect_identical(recipe_to_plan(x),expected)
  expect_error(omop_recipe(variables=list(v=omop_date_parts("birth",c(person_id="year")))),"date_parts_names")
  expect_error(omop_recipe(variables=list(v=omop_date("birth",omop_date_spec(precision="day",pattern="%Y")))),"Irrelevant")
})

test_that("D40 protected ds.glm uses identical ordered date contrasts", {
  lexer<-get("Lexer",asNamespace("DSLite"));old_d<-lexer$public_fields$t_DBLSTRING;old_s<-lexer$public_fields$t_STRING
  withr::defer({lexer$public_fields$t_DBLSTRING<-old_d;lexer$public_fields$t_STRING<-old_s})
  lexer$public_fields$t_DBLSTRING<-r"("([^"\\]|\\.)*")";lexer$public_fields$t_STRING<-r"('([^'\\]|\\.)*')"
  f<-dates_va_fixture(two_sites=TRUE);conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  r<-omop_recipe(variables=list(age=omop_age_at("2024-06-01",unit="annual_years"),quarter=omop_date("first_visit",omop_date_spec(precision="quarter"))),outputs=omop_output("o",format="wide",grain="person"))
  recipe_execute(r,out=c(o="D_model"),conns=conns)
  frames<-DSLite::getDSLiteData(conns,"D_model")
  expect_true(all(vapply(frames,inherits,logical(1),"omop.table")))
  matrices<-lapply(frames,function(d)stats::model.matrix(~quarter,stats::model.frame(~quarter,d,drop.unused.levels=TRUE)))
  expect_identical(colnames(matrices[[1]]),colnames(matrices[[2]]))
  fit<-dsBaseClient::ds.glm(formula="age ~ quarter",data="D_model",family="gaussian",datasources=conns)
  reference<-stats::glm(age~quarter,data=do.call(rbind,lapply(frames,as.data.frame)))
  expect_equal(unname(fit$coefficients[,1L]),unname(stats::coef(reference)),tolerance=1e-6)
})

test_that("D40 all-missing date factors and aliases preserve typed calendar contracts", {
  f<-dates_va_fixture(two_sites=TRUE);conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  for(fmt in c("baseline","features")) {
    r<-omop_recipe(variables=list(empty=omop_date(omop_date_components(2024L,2L,30L),omop_date_spec(precision="month")),day=omop_date("first_visit")),outputs=omop_output("o",type=fmt,grain="person"))
    recipe_execute(r,out=c(o=paste0("D_",fmt)),conns=conns)
    d<-DSLite::getDSLiteData(conns,paste0("D_",fmt))
    expect_true(all(vapply(d,function(x)is.ordered(x$empty)&&all(is.na(x$empty))&&inherits(x$day,"Date"),logical(1))))
    expect_identical(levels(d[[1]]$empty),character())
  }
})

test_that("D42 merge rewrites dates' event references and component sources", {
  mk<-function(id) omop_recipe(events=list(omop_event("observation",concepts=id,name="e")),variables=list(date=omop_date(omop_anchor("e")),gap=omop_time_between(omop_anchor("e"),"2024-01-01"),comp=omop_date(omop_date_components(2024L,1L,1L,event="e"))),outputs=omop_output("o",format="wide"))
  p<-recipe_to_plan(recipe_merge(mk(900001L),mk(900002L),prefix="b"))
  expect_identical(p$variables$b_date$derived$reference$source$event,"b_e")
  expect_identical(p$variables$b_gap$derived$from$source$event,"b_e")
  expect_identical(p$variables$b_comp$derived$reference$event$event,"b_e")
})

test_that("D40 exact custom factors cannot bypass metadata support or rollback", {
  f<-dates_va_fixture();conns<-f$conns
  on.exit({ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns);unlink(f$path)},add=TRUE)
  db<-DBI::dbConnect(RSQLite::SQLite(),f$path)
  DBI::dbExecute(db,"UPDATE visit_occurrence SET visit_start_date=printf('2020-01-%02d',person_id),visit_end_date=printf('2020-01-%02d',person_id)")
  DBI::dbDisconnect(db)
  r<-omop_recipe(variables=list(day=omop_date("first_visit",omop_date_spec(precision="custom",pattern="%Y-%m-%d"))),outputs=omop_output("o",format="wide",grain="person"))
  expect_error(recipe_execute(r,out=c(o="D_unsafe"),conns=conns),"factor-level disclosure policy|certify complete")
  expect_false("D_unsafe" %in% DSI::datashield.symbols(conns)$site)
  r$variables$day$date_spec<-omop_date_spec(precision="day")
  expect_identical(recipe_execute(r,out=c(o="D_exact"),conns=conns),c(o="D_exact"))
  expect_s3_class(DSLite::getDSLiteData(conns,"D_exact")$site$day,"Date")
})


test_that("D39 D42 effective native policies and public map order bind digests", {
  r<-omop_recipe(variables=list(d=omop_date("2024-01-01")),outputs=omop_output("o",format="wide",grain="person"),date_spec=omop_date_spec(precision="month",labels=c("2024-01"="Jan","2024-02"="Feb")))
  a<-recipe_to_plan(r);b<-a
  b$options$date_spec$labels<-rev(b$options$date_spec$labels)
  expect_identical(.planNormalizedDigest(a)$sha256,.planNormalizedDigest(b)$sha256)
  b$variables$d$date_spec<-list(precision="month")
  expect_identical(.planNormalizedDigest(a)$sha256,.planNormalizedDigest(b)$sha256)
  b$variables$d$date_spec$partial<-"mid"
  expect_false(identical(.planNormalizedDigest(a)$sha256,.planNormalizedDigest(b)$sha256))
})


test_that("D42 native date operation negotiation matches normalized server requirements", {
  r<-omop_recipe(variables=list(age=omop_age_at("2024-01-01"),gap=omop_time_between(omop_anchor(omop_event("death")),"2024-01-01")),outputs=omop_output("o",format="wide",grain="person"))
  p<-recipe_to_plan(r)
  expect_identical(as.character(.plan_v2_operations(p)),as.character(dsOMOP:::.irOperations(dsOMOP:::.irNormalize(p))))
  expect_true(all(c("date.elapsed","date.presentation","anchor.event") %in% .plan_v2_operations(p)))
})
