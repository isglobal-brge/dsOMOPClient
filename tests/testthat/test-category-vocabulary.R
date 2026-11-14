test_that("C09 C10 ATC levels use classes and preflight every declared input", {
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"INSERT INTO concept SELECT 45000123, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id=45000124 AND NOT EXISTS (SELECT 1 FROM concept WHERE concept_id=45000123)")
    targets<-data.frame(concept_id=45000201:45000205,concept_name=paste0("ATC",1:5),domain_id="Drug",vocabulary_id="ATC",concept_class_id=paste("ATC",c("1st","2nd","3rd","4th","5th")),standard_concept="C",concept_code=paste0("A",1:5),valid_start_date="2000-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_)
    DBI::dbWriteTable(con,"concept",targets,append=TRUE)
    ca<-expand.grid(ancestor_concept_id=45000201:45000205,descendant_concept_id=45000123:45000125)
    ca$min_levels_of_separation<-8L;ca$max_levels_of_separation<-9L
    DBI::dbWriteTable(con,"concept_ancestor",ca,append=TRUE);DBI::dbDisconnect(con)
  }
  v<-omop_variable(event=omop_event("observation",45000120L),reducer=omop_reduce("last_value",value="value_as_concept_id"))
  for(level in 1:5) {
    r<-omop_recipe(variables=list(atc=omop_category(v,omop_category_spec(rollup=omop_category_rollup("atc",level=level,domain=paste0("concept:",45000123:45000125))))))
    symbol<-paste0("ATC",level)
    recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns)
    for(f in .clf_frames(ctx,symbol))expect_true(all(as.character(f$atc)==paste0("ATC",level)))
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
