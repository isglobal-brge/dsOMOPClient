test_that("C08 C60 protected tables and GLMs honor nominal reference and ordinal contrasts", {
  skip_if_not_installed("dsBaseClient");skip_if_not_installed("dsBase")
  ctx<-.clf_setup();.numeric_lexer()
  for(path in ctx$paths) {
    con<-DBI::dbConnect(RSQLite::SQLite(),path)
    DBI::dbExecute(con,"UPDATE observation SET value_as_number=1+((person_id-1)%3) WHERE observation_concept_id=45000120")
    DBI::dbDisconnect(con)
  }
  e<-omop_event("observation",45000120L)
  v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  keys<-paste0("concept:",c(45000124L,45000123L,45000125L))
  common<-omop_category_spec(levels=keys,order=omop_category_order(keys),reference="concept:45000123")
  flag<-omop_category_indicators(v,c(yes="concept:45000124"))[["yes"]]
  r<-omop_recipe(variables=list(education=omop_category(v,common),
    ordinal=omop_category(v,omop_category_spec(levels=keys,order=omop_category_order(keys),ordered=TRUE)),
    band=omop_number_bins(omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_number")),breaks=c(0,1.5,2.5,4)),
    flag_integer=flag,
    flag_logical=omop_category(flag,omop_category_spec(binary=omop_category_binary("logical"))),
    flag_factor=omop_category(flag,omop_category_spec(binary=omop_category_binary("factor"))),
    age=omop_age(reference_date="2025-01-01")))
  recipe_execute(r,out="CategoriesModel",symbol="labels",conns=ctx$conns)
  frames<-.clf_frames(ctx,"CategoriesModel")
  for(n in 2:3) {
    conns<-ctx$conns[seq_len(n)]
    expected<-Reduce(`+`,lapply(frames[seq_len(n)],function(f)table(f$education)))
    names(dimnames(expected))<-"CategoriesModel$education"
    expect_equal(as.table(ds.omop.table("CategoriesModel","education",conns)),expected)
    expect_equal(as.table(ds.omop.table("CategoriesModel","education",rev(conns))),expected)
    combined<-do.call(rbind,lapply(frames[seq_len(n)],as.data.frame))
    for(column in c("education","ordinal","band","flag_integer","flag_logical","flag_factor")) {
      # Base rbind drops custom contrast attributes; preserve the released
      # contract when constructing the independent centralized reference.
      if(is.factor(combined[[column]]))contrasts(combined[[column]])<-contrasts(frames[[1L]][[column]])
      formula<-paste("age ~",column)
      expected_fit<-stats::glm(stats::as.formula(formula),data=combined,family="gaussian")
      fit<-dsBaseClient::ds.glm(formula=formula,data="CategoriesModel",family="gaussian",datasources=conns)
      expect_equal(unname(fit$coefficients[,1L]),unname(coef(expected_fit)),tolerance=1e-6)
    }
  }
})
