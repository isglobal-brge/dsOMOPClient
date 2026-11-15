test_that("copy prototypes branch at the same canonical and presented stages", {
  ctx <- .clf_setup(); .numeric_lexer()
  e <- omop_event("observation",45000120L)
  n <- omop_variable(event=e,reducer=omop_reduce("count"))
  r <- omop_recipe(variables=list(n=omop_number_bins(n,breaks=c(0,2,10))),
    outputs=omop_output("wide",columns=c("n","raw","nominal"),
      number_columns=list(raw=omop_number_column("n",number_spec=omop_number_spec(representation="number"))),
      category_columns=list(nominal=omop_category_column("n",category_spec=omop_category_spec(ordered=FALSE)))))
  for(mode in c("memory","staged")) .equivalence_books(r,ctx,mode,paste("copies",mode))
})
