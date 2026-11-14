test_that("C08 C60 protected tables and GLMs honor nominal reference and ordinal contrasts", {
  skip_if_not_installed("dsBaseClient");skip_if_not_installed("dsBase")
  ctx<-.clf_setup();.numeric_lexer()
  e<-omop_event("observation",45000120L)
  v<-omop_variable(event=e,reducer=omop_reduce("last_value",value="value_as_concept_id"))
  keys<-paste0("concept:",c(45000124L,45000123L,45000125L))
  common<-omop_category_spec(levels=keys,order=omop_category_order(keys),reference="concept:45000123")
  r<-omop_recipe(variables=list(education=omop_category(v,common),
    ordinal=omop_category(v,omop_category_spec(levels=keys,order=omop_category_order(keys),ordered=TRUE)),
    age=omop_age(reference_date="2025-01-01")))
  recipe_execute(r,out="CategoriesModel",symbol="labels",conns=ctx$conns)
  frames<-.clf_frames(ctx,"CategoriesModel")
  for(n in 2:3) {
    conns<-ctx$conns[seq_len(n)]
    expected<-Reduce(`+`,lapply(frames[seq_len(n)],function(f)table(f$education)))
    names(dimnames(expected))<-"CategoriesModel$education"
    expect_equal(ds.omop.table("CategoriesModel","education",conns),expected)
    expect_equal(ds.omop.table("CategoriesModel","education",rev(conns)),expected)
    combined<-do.call(rbind,lapply(frames[seq_len(n)],as.data.frame))
    for(column in c("education","ordinal")) {
      # Base rbind drops custom contrast attributes; preserve the released
      # contract when constructing the independent centralized reference.
      contrasts(combined[[column]])<-contrasts(frames[[1L]][[column]])
      formula<-paste("age ~",column)
      expected_fit<-stats::glm(stats::as.formula(formula),data=combined,family="gaussian")
      fit<-dsBaseClient::ds.glm(formula=formula,data="CategoriesModel",family="gaussian",datasources=conns)
      expect_equal(unname(fit$coefficients[,1L]),unname(coef(expected_fit)),tolerance=1e-6)
    }
  }
})
