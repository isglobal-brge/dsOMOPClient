test_that("C01 C03 C57 category declarations lower through the public API", {
  order <- c("concept:20","concept:10","concept:30")
  s <- omop_category_spec(levels=order,order=omop_category_order(order),ordered=TRUE)
  v <- omop_variable(name="education",table="person",column="race_concept_id")
  r <- omop_recipe(variables=list(education=omop_category(v,s)),
    category_spec=omop_category_spec(ordered=FALSE))
  p <- recipe_to_plan(r,version=2)
  expect_true(p$variables$education$category_spec$ordered)
  expect_false(p$options$category_spec$ordered)
  expect_null(v$category_spec)
  expect_true("category.presentation" %in% .plan_v2_operations(p))
  expect_error(recipe_to_plan(r,version=1),"category_spec")
  expect_error(omop_category_order(c("concept:1","concept:1")),"unique")
  expect_error(omop_category_order(c("concept:01")),"canonical")
  expect_error(omop_category_spec(binary=list(kind="category_binary",as="factor",labels=c("a","a"))),"unique")
  expect_error(.categorySpecCheck(list(sql="SELECT *")),"closed")
  expect_error(omop_category_rollup("none",level=1),"no other")
  expect_error(omop_category_rollup("snomed",ancestors="concept:1",domain="concept:2",distance="max"),"separation")
})
test_that("C02 ordinal metadata binds the exact canonical key array", {
  keys<-c("concept:20","concept:10","concept:30")
  sha<-as.character(openssl::sha256(charToRaw(.jcsCanonical(as.list(keys)))))
  m<-omop_category_order(metadata=list(id="education",version="1",sha256=sha,keys=keys))
  expect_identical(.categoryOrderCheck(m),.categoryOrderCheck(omop_category_order(keys)))
  expect_error(omop_category_order(metadata=list(id="education",version="1",sha256=paste(rep("0",64),collapse=""),keys=keys)),"hash")
})
test_that("C58 category constructors survive executable code and recipe interchange", {
  r<-omop_recipe(variables=list(x=omop_category(omop_race(),omop_category_spec(
    order=omop_category_order(metadata="identity"),ordered=FALSE,
    missing=omop_category_missing(na="level")))))
  p<-recipe_to_plan(r,version=2)
  env<-new.env(parent=asNamespace("dsOMOPClient"))
  eval(parse(text=recipe_to_code(r)),env)
  recipes<-Filter(function(x)inherits(x,"omop_recipe"),as.list(env))
  expect_true(length(recipes)>0)
  expect_equal(.planNormalizedDigest(recipe_to_plan(tail(recipes,1)[[1]],version=2))$sha256,.planNormalizedDigest(p)$sha256)
  expect_silent(recipe_to_plan(recipe_merge(r,r,prefix="copy"),version=2))
})

test_that("C57 C58 nested category and repeated declarations round trip executable code", {
  e<-omop_event("observation",45000120L,name="answer")
  rows<-omop_category_rows(e,"value_as_concept_id")
  times<-omop_timepoints(list(now=omop_timepoint(),later=omop_timepoint(6,"months")),anchor="2020-01-01")
  r<-omop_recipe(variables=list(nth=omop_category_reduce(rows,omop_reduce("nth_value",n=c(1L,3L))),repeated=omop_repeated(e,list(x=rows),timepoints=times)))
  env<-new.env(parent=asNamespace("dsOMOPClient"));eval(parse(text=recipe_to_code(r)),env)
  recipes<-Filter(function(x)inherits(x,"omop_recipe"),as.list(env))
  expect_length(recipes,1L)
  expect_equal(.planNormalizedDigest(recipe_to_plan(recipes[[1L]],version=2))$sha256,.planNormalizedDigest(recipe_to_plan(r,version=2))$sha256)
  expect_error(omop_category_reduce(rows,omop_reduce("last_value",value="value_as_concept_id")),"omit value")
  expect_error(omop_category_reduce(rows,omop_reduce("last_value",unit="month")),"only with time_since")
})


test_that("C03 C59 explain preserves public category policies without data access", {
  e<-omop_event("observation",45000120L)
  r<-omop_recipe(variables=list(x=omop_category(omop_variable(event=e,reducer=omop_reduce("binary")),omop_category_spec(binary=omop_category_binary("factor")))))
  text<-paste(recipe_explain(r,detail="semantics")$text,collapse="\n")
  expect_match(text,"Inapplicable recipe defaults are inactive",fixed=TRUE)
  expect_match(text,"Semantic flags retain false/true identity",fixed=TRUE)
  expect_match(text,"Open concept and period domains remain unresolved",fixed=TRUE)
})

test_that("C59 every missing category capability refuses before assignment", {
  e<-omop_event("observation",45000120L,name="e")
  rows<-omop_category_rows(e,"value_as_concept_id",omop_category_spec(rollup=omop_category_rollup("snomed",separation=1L,domain="concept:45000124")))
  flag<-omop_category_indicators(rows,c(yes="concept:45000124"))[["yes"]]
  r<-omop_recipe(events=list(e),variables=list(any=omop_category_reduce(flag,omop_reduce("any")),time=omop_repeated(e,list(value=rows),timepoints=omop_timepoints(list(now=omop_timepoint()),anchor="2020-01-01"))))
  required<-grep("^(category[.]|layout[.]|output[.]codebook)",.plan_v2_operations(recipe_to_plan(r,version=2)),value=TRUE)
  expect_setequal(required,c("category.presentation","category.rollup","category.indicators","category.reduce","layout.repeated","output.codebook_metadata"))
  for(missing in required) {
    state<-.legacy_federation(new=c("one","two"),ops=setdiff(union(.V2_ALL_OPS,required),missing))
    expect_error(recipe_execute(r),paste0("lacks ",missing))
    expect_identical(unname(state$assigns),c(0L,0L))
  }
  expect_error(recipe_to_plan(r,version=1),"repeated")
})


test_that("category reducer input context is scoped to its typed consumer", {
  event <- omop_event("observation", 45000120L)
  rows <- omop_category_rows(event, "value_as_concept_id")
  flag <- omop_category_indicators(rows, c(yes = "concept:45000124"))[[1L]]
  make_any <- function() omop_reduce("any")
  expect_error(make_any(), "needs a predicate")
  reduced <- omop_category_reduce(flag, make_any())
  expect_identical(reduced$derived$reducer$fn, "any")
  expect_error(make_any(), "needs a predicate")
  expect_error(omop_reduce("all"), "needs a predicate")
})
