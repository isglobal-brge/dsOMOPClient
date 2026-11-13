.cl_recipe <- function(...) {
  omop_recipe(variables = list(omop_variable('education', event = omop_event('observation', 1L),
    reducer = omop_reduce('last_value', value = 'value_as_concept_id'), labels = c('1' = 'variable'))), ...)
}

test_that('A27 appended maps survive native rendering and binding', {
  expect_identical(tail(names(formals(omop_variable)), 3L), c('broadcast','labels','column_names'))
  expect_identical(tail(names(formals(omop_recipe)), 3L), c('publish','labels','column_names'))
  r <- .cl_recipe(labels = c('2' = 'recipe', '1' = 'recipe'), column_names = c(education_1 = 'educated'))
  p <- recipe_to_plan(r)
  expect_identical(p$variables$education$labels, list('1' = 'variable'))
  expect_identical(p$options$labels, list('1' = 'recipe', '2' = 'recipe'))
  expect_error(recipe_to_plan(r, version = 1), 'labels/column_names')
  expect_true('output.concept_labels' %in% .plan_v2_operations(p))
  r2 <- .cl_recipe(labels = c('1' = 'recipe', '2' = 'recipe'), column_names = c(education_1 = 'educated'))
  expect_identical(.planNormalizedDigest(p)$sha256, .planNormalizedDigest(recipe_to_plan(r2))$sha256)
  r2$options$labels[['2']] <- 'different'
  expect_false(identical(.planNormalizedDigest(p)$sha256, .planNormalizedDigest(recipe_to_plan(r2))$sha256))
  expect_match(paste(recipe_explain(r)$text, collapse = '\n'), 'merge')
  expect_error(recipe_to_plan(.cl_recipe(options = list(factor_concepts = FALSE))), 'translate_concepts = FALSE')
  disabled <- .cl_recipe(options = list(translate_concepts = FALSE, factor_concepts = FALSE))
  expect_match(paste(recipe_explain(disabled)$text, collapse = '\n'), 'inactive')
})

test_that('A27 capability negotiation refuses maps on older servers', {
  p <- recipe_to_plan(.cl_recipe())
  ops <- .plan_v2_operations(p)
  session <- list(capabilities = list(a = list(recipe_language = list(version = 2L,
    operations = setdiff(ops, 'output.concept_labels')))))
  expect_error(.v2_assert_supported(p, session, 'a'), 'output.concept_labels')
})

test_that('A27 federation resolves names by ID and retains unused declarations', {
  rec <- function(id, name, version = '2026-01-01', origin = 'vocabulary', label = name) {
    list(id = id, name = name, version = version, vocabulary = 'SNOMED', origin = origin, label = label)
  }
  report <- function(rows) list(concept_protocol = 1L, levels = list(education = vapply(rows, function(r) r$id, '')),
    unsafe = character(), nfilter_levels_max = 40, concepts = list(education = rows), keys = c('person_id','education'), generated = list())
  sites <- list(a = report(list(rec('1','Old','2025-01-01'), rec('2','Same'), rec('4', NULL, origin = 'recipe', label = 'unused'))),
                b = report(list(rec('1','New'), rec('3','Same'), rec('4', NULL, origin = 'recipe', label = 'unused'))),
                c = report(list(rec('1',NULL), rec('2','Same'), rec('4', NULL, origin = 'recipe', label = 'unused'))))
  spec <- .conceptUnionReports(sites)
  expect_identical(spec$columns$education$levels, c('New','Same [2]','Same [3]','unused'))
  expect_identical(.conceptUnionReports(rev(sites)), spec)
  sites$c$unsafe <- 'education'
  expect_error(.conceptUnionReports(sites), 'disclosure policy')
})

test_that('A27 merge prefixes stable keys while targets and concept recodes stay literal', {
  a <- omop_recipe(variables=list(omop_variable('age',table='person',column='year_of_birth')))
  b <- .cl_recipe(column_names=c(education_1='educated'),labels=c('2'='other'))
  combined <- recipe_merge(a,b,prefix='study')
  expect_identical(combined$options$column_names,c(study_education_1='educated'))
  expect_identical(combined$options$labels,c('2'='other'))
  expect_identical(combined$variables$study_education$labels,c('1'='variable'))
  expect_identical(recipe_to_plan(combined)$options$column_names,list(study_education_1='educated'))
})

test_that('A27 client checks final component names including numeric-only reports', {
  record <- function(id,name) list(id=id,name=name,vocabulary='v',version='1',origin='vocabulary',label=name)
  generated <- list(a_1=list(id='1',prefix='a_',suffix='',owner='a',explicit='same',candidates=list(record('1','One'))),
    b_2=list(id='2',prefix='b_',suffix='',owner='b',explicit='same',candidates=list(record('2','Two'))))
  report <- list(concept_protocol=1L,levels=list(),concepts=list(),unsafe=character(),nfilter_levels_max=40,
    keys=c('person_id','a_1','b_2'),generated=generated)
  expect_error(.conceptUnionReports(list(a=report,b=report)),'explicit name')
  report$generated$a_1$explicit <- 'education'
  expect_identical(unlist(.conceptUnionReports(list(a=report,b=report))$names),c(person_id='person_id',a_1='education',b_2='same'))
})
