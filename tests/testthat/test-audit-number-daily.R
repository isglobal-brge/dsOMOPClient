test_that("Q15 daily declarations preserve both stages through portable recipes", {
  e<-omop_event("measurement",3000963L,name="lab",unit=omop_unit(8554L))
  v<-omop_number_daily(omop_number_rows(e),omop_reduce("mean"),missing="propagate")
  r<-omop_recipe(events=list(e),variables=list(daily=v))
  p<-recipe_to_plan(r,version=2)
  expect_identical(p$variables$daily$derived$kind,"number_daily")
  expect_true("number.daily" %in% .plan_v2_operations(p))
  for(format in c("json","yaml")) {
    path<-tempfile(fileext=paste0(".",format));on.exit(unlink(path),add=TRUE)
    get(paste0("recipe_export_",format))(r,path)
    restored<-get(paste0("recipe_import_",format))(path)
    expect_identical(.planNormalizedDigest(recipe_to_plan(restored,version=2))$sha256,.planNormalizedDigest(p)$sha256)
  }
  env<-new.env(parent=asNamespace("dsOMOPClient"));eval(parse(text=recipe_to_code(r)),env)
  restored<-tail(Filter(function(x)inherits(x,"omop_recipe"),as.list(env)),1)[[1]]
  expect_identical(.planNormalizedDigest(recipe_to_plan(restored,version=2))$sha256,.planNormalizedDigest(p)$sha256)
  merged<-recipe_to_plan(recipe_merge(r,r,prefix="b"),version=2)
  expect_identical(merged$variables$b_daily$derived$input$derived$source$event,"b_lab")
  expect_error(omop_number_daily(omop_variable(event=e,reducer=omop_reduce("mean")),omop_reduce("mean")),"number_daily_input")
  expect_error(omop_number_daily(omop_number_rows(e),omop_reduce("count")),"number_expression_invalid")
})

test_that("Q15 missing capability refuses before assignment", {
  v<-omop_number_daily(omop_number_rows(omop_event("measurement",3000963L,unit=omop_unit(8554L))),omop_reduce("mean"))
  r<-omop_recipe(variables=list(daily=v))
  required<-.plan_v2_operations(recipe_to_plan(r,version=2))
  state<-.legacy_federation(new=c("one","two"),ops=setdiff(union(.V2_ALL_OPS,required),"number.daily"))
  expect_error(recipe_execute(r),"lacks number.daily")
  expect_identical(unname(state$assigns),c(0L,0L))
})
