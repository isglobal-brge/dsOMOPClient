test_that("Q21 actual published weight-for-age agrees with pinned WHO medians", {
  ctx<-.clf_setup();.numeric_lexer()
  ref<-read.csv(test_path("..","fixtures","who-growth","wfa-medians.csv"))
  expect_equal(nrow(ref),6L)
  rows<-ref[rep(1:6,3),];rows$person_id<-1:18
  rows$gender<-ifelse(rows$sex==1L,8507L,8532L);rows$unit<-990050L
  rows$gender[13]<-0L;rows$unit[14]<-990051L;rows$weight_kg[15]<-NA
  rows$age_days[16]<-1827L;rows$age_days[17]<--1L
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path);DBI::dbExecute(con,"DELETE FROM measurement")
    DBI::dbWriteTable(con,"measurement",data.frame(measurement_id=1:18,person_id=1:18,
      measurement_concept_id=990060L,measurement_date="2020-01-01",measurement_type_concept_id=0L,
      value_as_number=rows$weight_kg,unit_concept_id=rows$unit),append=TRUE)
    for(i in 1:18) {
      birth<-as.Date("2020-01-01")-rows$age_days[i]
      DBI::dbExecute(con,"UPDATE person SET year_of_birth=?,month_of_birth=?,day_of_birth=?,gender_concept_id=? WHERE person_id=?",
        params=list(as.integer(format(birth,"%Y")),as.integer(format(birth,"%m")),as.integer(format(birth,"%d")),rows$gender[i],i))
    }
    DBI::dbDisconnect(con)
  }
  # 990050 is a declared SYNTHETIC kg coordinate in this fixture; 990051 is
  # deliberately not mapped. The event normalizer makes that value missing.
  event<-omop_event("measurement",990060L,name="weight",unit=omop_unit(990050L,unknown="missing"))
  weight<-omop_number(omop_variable(event=event,reducer=omop_reduce("first_value",missing="keep")),
    omop_number_spec(unit=omop_number_unit("unit:990050","coordinate:kg",1)))
  age<-omop_number(omop_age_at("2020-01-01",unit="days"),omop_number_spec(unit=omop_number_unit("days","days",1)))
  sex<-omop_variable(table="person",column="gender_concept_id")
  flags<-omop_category_indicators(sex,c(male="concept:8507",female="concept:8532"))
  expr<-list(ifelse=list(test=list(var="m",op="==",value=1),yes=list(lit=1,as="numeric"),
    no=list(ifelse=list(test=list(var="f",op="==",value=1),yes=list(lit=2,as="numeric"),no=list(lit=NULL,as="numeric")))))
  code<-omop_number_derive(list(m=flags$male,f=flags$female),list(version=1L,assign=list(list(name="answer",expr=expr))),"1")
  recipe<-omop_recipe(variables=list(weight=weight,age_days=age,sex=code),
    populations=omop_population("p",index=omop_index(event)),outputs=omop_output("o",rows="p"))
  recipe_execute(recipe,out="Growth",symbol="labels",conns=ctx$conns[1:2])
  answer<-dsBaseClient::ds.getWGSR(sex="Growth$sex",firstPart="Growth$weight",secondPart="Growth$age_days",
    index="wfa",standing=2,newobj="GrowthZ",datasources=ctx$conns[1:2])
  expect_match(answer$is.object.created,"created in all")
  expected<-rep(0,18);expected[13:17]<-NA_real_
  for(server in ctx$servers[1:2]) {
    z<-server$getSessionData(server$getSessionIds()[[1]],"GrowthZ")
    # Private synthetic reference inspection only. No row is returned by a
    # client endpoint or displayed in teaching output.
    expect_equal(sort(z,na.last=TRUE),sort(expected,na.last=TRUE),tolerance=1e-10)
  }
  aggregate<-dsBaseClient::ds.mean("GrowthZ",type="combine",datasources=ctx$conns[1:2])
  expect_equal(as.numeric(aggregate$Global.Mean[1,1]),0,tolerance=1e-10)
})
