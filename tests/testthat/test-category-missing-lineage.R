test_that("C28 C29 C30 C31 unavailable and undefined inputs never become ordinary Missing", {
  ctx<-.clf_setup(drop_tables="device_exposure");.numeric_lexer()
  absent<-omop_event("device_exposure",on_missing_table="unavailable")
  empty<-omop_event("observation",999999L,name="empty")
  available<-omop_event("observation",45000120L)
  missing<-omop_category_spec(missing=omop_category_missing("level"))
  cat<-function(e,window=NULL)omop_category(omop_variable(event=e,reducer=omop_reduce("last_value",value="concept_id",window=window)),missing)
  r<-omop_recipe(events=list(empty),variables=list(unavailable=cat(absent),unavailable_both=omop_category(cat(absent),omop_category_spec(missing=omop_category_missing("level","level"))),empty=cat(empty),
    undefined=cat(available,omop_window(-7,7,anchor=omop_anchor(empty))),
    logical=omop_category(omop_variable(event=absent,reducer=omop_reduce("binary")),omop_category_spec(binary=omop_category_binary("logical")))))
  for(mode in c("memory","staged")) {
    symbol<-paste0("Lineage_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(is.na(f$unavailable)));expect_true(all(is.na(f$undefined)))
      expect_true(all(f$empty=="Missing"));expect_true(all(is.na(f$logical)));expect_type(f$logical,"logical")
      expect_identical(levels(f$unavailable),"Missing")
      expect_true(all(is.na(f$unavailable_both)));expect_identical(levels(f$unavailable_both),c("Missing","Unknown"))
    }
  }
  quantity<-omop_variable(event=available,reducer=omop_reduce("count"))
  r<-omop_recipe(variables=list(x=quantity),category_spec=omop_category_spec(binary=omop_category_binary("logical")))
  recipe_execute(r,out="NumericOne",symbol="labels",conns=ctx$conns)
  for(f in .clf_frames(ctx,"NumericOne"))expect_type(f$x,"integer")
  r$variables$x<-omop_category(quantity,omop_category_spec(binary=omop_category_binary("logical")))
  expect_error(recipe_execute(r,out="BadBinary",symbol="labels",conns=ctx$conns),"category_type_unsupported")
})

test_that("C30 C31 only semantic identity chains retain flags and factor order cannot invert truth", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L)
  flag<-omop_variable(event=e,reducer=omop_reduce("binary"))
  doubled<-omop_number(flag,omop_number_spec(transform=omop_number_transform("affine",factor=2,offset=0)))
  restored<-omop_number_flag(doubled,"gt",1)
  reversed<-omop_category(flag,omop_category_spec(binary=omop_category_binary("factor"),missing=omop_category_missing("level"),order=omop_category_order(c("flag:1","flag:0","special:missing"))))
  indicator<-omop_category_indicators(flag,c(yes="flag:1"))
  r<-omop_recipe(variables=list(quantity=doubled,restored=omop_category(restored,omop_category_spec(binary=omop_category_binary("logical"))),reversed=reversed,indicator=indicator),category_spec=omop_category_spec(binary=omop_category_binary("logical")))
  for(mode in c("memory","staged")) {
    symbol<-paste0("BooleanIdentity_",mode);recipe_execute(r,out=symbol,symbol="labels",conns=ctx$conns,output_mode=mode)
    for(f in .clf_frames(ctx,symbol)) {
      expect_true(all(f$quantity==2));expect_type(f$restored,"logical");expect_true(all(f$restored))
      expect_identical(levels(f$reversed),c("yes","no","Missing"));expect_true(all(f$reversed=="yes"));expect_true(all(f$indicator_yes))
    }
  }
  bad<-omop_recipe(variables=list(x=omop_category(doubled,omop_category_spec(binary=omop_category_binary()))))
  expect_error(recipe_execute(bad,out="BadArithmeticFlag",symbol="labels",conns=ctx$conns),"category_type_unsupported")
  bad$variables$x<-omop_category(flag,omop_category_spec(binary=omop_category_binary(),missing=omop_category_missing("level")))
  expect_error(recipe_execute(bad,out="BadIntegerMissing",symbol="labels",conns=ctx$conns),"category_spec_invalid")
})

test_that("C56 C61 source aliases cannot declassify and date-dependent categories retain class two", {
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L,name="e")
  for(column in c("observation_source_value","observation_source_concept_id","value_as_string")) {
    r<-omop_recipe(variables=list(x=omop_category_reduce(omop_category_rows(e,column),omop_reduce("mode"))))
    expect_error(recipe_execute(r,out="ForbiddenCategory",symbol="labels",conns=ctx$conns),"validation_error|cannot be used|not a predicate|category_source_forbidden")
    for(server in ctx$servers)expect_error(server$getSessionData(server$getSessionIds()[[1L]],"ForbiddenCategory"))
  }
  repeated<-omop_repeated(e,list(value=omop_number_rows(e,"value_as_number")),timepoints=omop_timepoints(list(baseline=omop_timepoint())))
  period<-omop_category(omop_date(omop_anchor(e),omop_date_spec(precision="month")),omop_category_spec(ordered=TRUE))
  r<-omop_recipe(events=list(e),populations=omop_population("p",index=omop_index(e)),variables=list(period=period,repeated=repeated),outputs=omop_output("wide",rows="p"))
  result<-recipe_execute(r,out="ClassLineage",symbol="labels",conns=ctx$conns)
  columns<-recipe_codebook(result)[[1L]]$columns
  expect_identical(columns$privacy_class[match(c("period","repeated_value_baseline"),columns$name)],c("2","2"))
  expect_false(any(c("source_value","observed","count","unavailable","selected_date","selected_key") %in% names(columns)))
})
