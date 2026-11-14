test_that("C09 C10 ATC levels use classes and preflight every declared input", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"INSERT INTO concept SELECT 45000123, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id=45000124 AND NOT EXISTS (SELECT 1 FROM concept WHERE concept_id=45000123)")
    targets<-data.frame(concept_id=45000201:45000205,concept_name=paste0("ATC",1:5),domain_id="Drug",vocabulary_id="ATC",concept_class_id=paste("ATC",c("1st","2nd","3rd","4th","5th")),standard_concept="C",concept_code=paste0("A",1:5),valid_start_date="2000-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_)
    DBI::dbWriteTable(con,"concept",targets,append=TRUE)
    ca<-expand.grid(ancestor_concept_id=45000201:45000205,descendant_concept_id=45000123:45000125)
    ca$min_levels_of_separation<-8L;ca$max_levels_of_separation<-9L
    ca<-rbind(ca,data.frame(ancestor_concept_id=45000201L,descendant_concept_id=45000205L,min_levels_of_separation=1L,max_levels_of_separation=1L))
    DBI::dbWriteTable(con,"concept_ancestor",ca,append=TRUE);DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  for(level in 1:5) {
    r<-omop_recipe(variables=list(atc=omop_category(v,omop_category_spec(rollup=omop_category_rollup("atc",level=level,domain=paste0("concept:",45000123:45000125))))))
    if(level==1L) {
      input<-omop_category_rows(omop_event("observation",45000120L),"value_as_concept_id",omop_category_spec(rollup=omop_category_rollup("atc",level=5,domain=paste0("concept:",45000123:45000125))))
      reduced<-omop_category_reduce(input,omop_reduce("mode"))
      r$variables$second<-omop_category(reduced,omop_category_spec(rollup=omop_category_rollup("atc",level=1,domain="concept:45000205")))
      r$variables$styled<-omop_category(reduced,omop_category_spec(label_style="code_name"))
    }
    symbol<-paste0("ATC",level)
    recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(as.character(f$atc)==paste0("ATC",level)))
      if(level==1L) {
        expect_true(all(as.character(f$second)=="ATC1"))
        expect_true(all(as.character(f$styled)=="A5 - ATC5"))
      }
    }
  }
})
test_that("C20 C23 code styles and deterministic language candidates preserve zeros", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE concept SET concept_code='001A' WHERE concept_id=45000124")
    DBI::dbWriteTable(con,"concept_synonym",data.frame(concept_id=rep(45000124L,2),concept_synonym_name=c("Zulu","Alpha"),language_concept_id=4180186L),append=TRUE)
    DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  r<-omop_recipe(variables=list(code=omop_category(v,omop_category_spec(label_style="code")),
    translated=omop_category(v,omop_category_spec(language=omop_category_language(4180186L))),
    named=omop_category(v,omop_category_spec(label_style="code_name",language=omop_category_language(4180186L)))))
  recipe_execute(r,out="Styled",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Styled")) {
    expect_true("001A" %in% levels(f$code));expect_true("Alpha" %in% levels(f$translated));expect_true("001A - Alpha" %in% levels(f$named))
  }
})

test_that("C10 C15 C16 C19 C60 hierarchy ambiguity and exact separation are public contracts", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"INSERT INTO concept SELECT 45000123, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id=45000124 AND NOT EXISTS (SELECT 1 FROM concept WHERE concept_id=45000123)")
    for(id in 45000301:45000302)DBI::dbExecute(con,paste0("INSERT INTO concept SELECT ",id,", 'Ancestor",id,"', domain_id, 'SNOMED', concept_class_id, standard_concept, '",id,"', valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id=45000124"))
    ca<-expand.grid(ancestor_concept_id=45000301:45000302,descendant_concept_id=45000123:45000125)
    ca$min_levels_of_separation<-1L;ca$max_levels_of_separation<-3L
    DBI::dbWriteTable(con,"concept_ancestor",ca,append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  domain<-omop_concept_set(45000123:45000125)
  roll<-function(...)omop_category_spec(rollup=omop_category_rollup("snomed",domain=domain,...))
  ambiguous<-omop_recipe(variables=list(x=omop_category(v,roll(separation=1))))
  ap<-recipe_to_plan(ambiguous,version=2)
  expect_error(ds.omop.plan.execute(ap,out=setNames("Ambiguous",names(ap$outputs)),symbol="labels",conns=ctx$conns),"category_rollup_ambiguous")
  r<-omop_recipe(variables=list(
    min2=omop_category(v,roll(separation=2,distance="min")),
    max2=omop_category(v,roll(separation=2,distance="max")),
    priority=omop_category(v,roll(separation=1,multiple="priority",priority=c("concept:45000302","concept:45000301"))),
    flags=omop_category_indicators(omop_category(v,roll(separation=3,distance="max",multiple="all")),c(a="concept:45000301",b="concept:45000302"))))
  recipe_execute(r,out="Hierarchy",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Hierarchy")) {
    expect_identical(as.character(f$min2),as.character(f$max2))
    expect_false(any(grepl("Ancestor",as.character(f$min2))))
    expect_true(all(f$priority=="Ancestor45000302"))
    expect_true(all(f$flags_a==1L));expect_true(all(f$flags_b==1L));expect_equal(nrow(f),18L)
  }
  flags<-omop_category_indicators(omop_category_rows(e,"value_as_concept_id",roll(separation=3,distance="max",multiple="all")),c(a="concept:45000301",b="concept:45000302"))
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"INSERT INTO observation (observation_id,person_id,observation_concept_id,observation_date,observation_type_concept_id,value_as_concept_id) SELECT observation_id+1000,person_id,observation_concept_id,observation_date,observation_type_concept_id,value_as_concept_id FROM observation WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  bool<-function(x)omop_category(omop_category_reduce(x,omop_reduce("any")),omop_category_spec(binary=omop_category_binary("factor")))
  pooled<-omop_recipe(variables=list(a=bool(flags$a),b=bool(flags$b),total=omop_category_reduce(flags$a,omop_reduce("sum"))))
  recipe_execute(pooled,out="Overlapping",symbol="labels",conns=ctx$conns)
  for(column in c("a","b")) {
    tab<-ds.omop.table("Overlapping",column,ctx$conns)
    expect_equal(as.integer(tab),c(0L,54L))
  }
  fit<-dsBaseClient::ds.glm(formula="total ~ 1",data="Overlapping",family="gaussian",datasources=ctx$conns)
  expect_equal(unname(fit$coefficients[1L,1L]),2,tolerance=1e-6)
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE concept SET vocabulary_id='SNOMED' WHERE concept_id IN (45000123,45000124,45000125)");DBI::dbDisconnect(con)
  }
  self<-omop_recipe(variables=list(zero=omop_category(v,roll(separation=0)),named=omop_category(v,roll(ancestors=paste0("concept:",45000123:45000125)))))
  recipe_execute(self,out="NamedSelf",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"NamedSelf"))expect_identical(as.character(f$zero),as.character(f$named))
})

test_that("C12 C13 C14 ICD10 rollup deduplicates reverse maps and follows exact class edges", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"INSERT INTO concept SELECT 45000123, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id=45000124 AND NOT EXISTS (SELECT 1 FROM concept WHERE concept_id=45000123)")
    concepts<-data.frame(concept_id=c(45000401L,45000402L,45000410L,45000420L),concept_name=c("A00","A01","Block","Chapter"),domain_id=c("Condition","Condition","Condition","Observation"),vocabulary_id="ICD10",concept_class_id=c("ICD10 Hierarchy","ICD10 Hierarchy","ICD10 SubChapter","ICD10 Chapter"),standard_concept="C",concept_code=c("A00","A01","A00-A09","I"),valid_start_date="2000-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_)
    DBI::dbWriteTable(con,"concept",concepts,append=TRUE)
    maps<-expand.grid(concept_id_1=45000401:45000402,concept_id_2=45000123:45000125);maps$relationship_id<-"Maps to"
    edges<-data.frame(concept_id_1=c(45000401L,45000410L,45000410L),concept_id_2=c(45000410L,45000402L,45000420L),relationship_id=c("Is a","Subsumes","Is a"))
    rel<-rbind(maps,edges);rel$valid_start_date<-"2000-01-01";rel$valid_end_date<-"2099-12-31";rel$invalid_reason<-NA_character_
    bad<-concepts[4L,];bad$concept_id<-45000430L;bad$concept_name<-"Excluded chapter"
    DBI::dbWriteTable(con,"concept",bad,append=TRUE)
    ignored<-rel[rep(1L,6L),];ignored$concept_id_1<-45000430L;ignored$concept_id_2<-rep(45000123:45000125,2L)
    ignored$relationship_id<-rep(c("Maps to","Maps to value"),each=3L);ignored$invalid_reason<-rep(c("D",NA_character_),each=3L)
    DBI::dbWriteTable(con,"concept_relationship",rbind(rel,ignored),append=TRUE);DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L);v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  variables<-setNames(lapply(c("block","chapter"),function(level)omop_category(v,omop_category_spec(rollup=omop_category_rollup("icd10",level=level,vocabulary="ICD10",domain=paste0("concept:",45000123:45000125))))),c("block","chapter"))
  recipe_execute(omop_recipe(variables=variables),out="ICD",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"ICD")) {expect_true(all(f$block=="Block"));expect_true(all(f$chapter=="Chapter"));expect_equal(nrow(f),18L)}
})

test_that("C11 C17 hierarchy cycles outside the declared input nodes fail structurally", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    ca<-data.frame(ancestor_concept_id=c(45000124L,45000125L,45000124L),descendant_concept_id=c(45000123L,45000124L,45000125L),min_levels_of_separation=1L,max_levels_of_separation=1L)
    DBI::dbWriteTable(con,"concept_ancestor",ca,append=TRUE);DBI::dbDisconnect(con)
  }
  v<-omop_category_rows(omop_event("observation",999999L),"value_as_concept_id",omop_category_spec(rollup=omop_category_rollup("snomed",separation=1L,domain="concept:45000123")))
  r<-omop_recipe(variables=list(x=omop_category_reduce(v,omop_reduce("mode"))))
  expect_error(recipe_execute(r,out="Cycle",symbol="labels",conns=ctx$conns),"category_domain_incompatible")
})

test_that("C21 C24 C25 language candidates beat fallbacks and literal labels remain unchanged", {
  ctx<-.clf_setup();.numeric_lexer()
  for(i in seq_along(ctx$paths)) {
    con<-DBI::dbConnect(RSQLite::SQLite(),ctx$paths[[i]])
    DBI::dbExecute(con,"UPDATE concept SET concept_code='' WHERE concept_id=45000124")
    if(i==2L)DBI::dbWriteTable(con,"concept_synonym",data.frame(concept_id=45000124L,concept_synonym_name=c("Éducation","Alpha local"),language_concept_id=4180186L),append=TRUE)
    DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  language<-omop_category_spec(language=omop_category_language(4180186L))
  custom<-v;custom$labels<-c(`45000124`="Exact override")
  r<-omop_recipe(variables=list(translated=omop_category(v,language),literal=omop_category(custom,language),code=omop_category(v,omop_category_spec(label_style="code")),vocab=omop_category(v,omop_category_spec(label_style="vocabulary_code"))))
  recipe_execute(r,out="Fallbacks",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"Fallbacks")) {
    expect_true("Alpha local" %in% levels(f$translated));expect_true("Exact override" %in% levels(f$literal))
    expect_true("45000124" %in% levels(f$code));expect_true("A27:45000124" %in% levels(f$vocab))
  }
})

test_that("C11 C22 missing hierarchy tables and source-code requests fail without a source-value route", {
  ctx<-.clf_setup(drop_tables="concept_ancestor");.numeric_lexer()
  v<-omop_category_rows(omop_event("observation",999999L),"value_as_concept_id")
  roll<-omop_category(v,omop_category_spec(rollup=omop_category_rollup("atc",level=5,domain="concept:45000123")))
  r<-omop_recipe(variables=list(x=omop_category_reduce(roll,omop_reduce("mode"))))
  expect_error(recipe_execute(r,out="MissingHierarchy",symbol="labels",conns=ctx$conns),"category_domain_incompatible")
  r$variables$x<-omop_category(omop_variable(event=omop_event("observation",999999L),reducer=omop_reduce("last_value",value="concept_id")),omop_category_spec(label_style="source_code"))
  expect_error(recipe_execute(r,out="SourceCode",symbol="labels",conns=ctx$conns),"category_source_forbidden")
})

test_that("C51 C62 equal labels cannot hide different public hierarchy maps", {
  ctx<-.clf_setup();.numeric_lexer()
  for(i in seq_along(ctx$paths)) {
    con<-DBI::dbConnect(RSQLite::SQLite(),ctx$paths[[i]])
    DBI::dbExecute(con,"INSERT INTO concept SELECT 45000123,concept_name,domain_id,vocabulary_id,concept_class_id,standard_concept,concept_code,valid_start_date,valid_end_date,invalid_reason FROM concept WHERE concept_id=45000124 AND NOT EXISTS (SELECT 1 FROM concept WHERE concept_id=45000123)")
    for(id in 45000501:45000502)DBI::dbExecute(con,paste0("INSERT INTO concept SELECT ",id,",'Same target',domain_id,'SNOMED',concept_class_id,standard_concept,'",id,"',valid_start_date,valid_end_date,invalid_reason FROM concept WHERE concept_id=45000124"))
    ca<-data.frame(ancestor_concept_id=if(i==1L)45000501L else 45000502L,descendant_concept_id=45000123:45000125,min_levels_of_separation=1L,max_levels_of_separation=1L)
    DBI::dbWriteTable(con,"concept_ancestor",ca,append=TRUE);DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  v<-omop_category(v,omop_category_spec(rollup=omop_category_rollup("snomed",separation=1L,domain=paste0("concept:",45000123:45000125))))
  for(input in list(v,omop_category_indicators(v,c(target="concept:45000501"))[["target"]])) {
    r<-omop_recipe(variables=list(x=input))
    expect_error(recipe_execute(r,out="DifferentMaps",symbol="labels",conns=ctx$conns),"category_domain_incompatible")
  }
})

test_that("C11 C17 required adapter defects refuse and out-of-domain cells stay private NA", {
  ctx<-.clf_setup();.numeric_lexer()
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  spec<-omop_category_spec(rollup=omop_category_rollup("snomed",ancestors="concept:999999",domain="concept:45000124",unmatched="keep"))
  r<-omop_recipe(variables=list(x=omop_category(v,spec)))
  result<-recipe_execute(r,out="PublicDomain",symbol="labels",conns=ctx$conns)
  for(i in 1:3) {
    f<-.clf_frames(ctx,"PublicDomain")[[i]]
    expect_equal(sum(!is.na(f$x)),if(i==1L)6L else 9L)
  }
  expect_false(any(c("observed","count","unavailable") %in% names(recipe_codebook(result)[[1L]]$columns)))
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE concept SET concept_class_id=NULL WHERE concept_id=45000124");DBI::dbDisconnect(con)
  }
  expect_error(recipe_execute(r,out="MalformedAdapter",symbol="labels",conns=ctx$conns),"category_domain_incompatible")
})
