.audit_formula_inputs <- function() {
  e<-omop_event("measurement",3000963L,name="scr",unit=omop_unit(8554L))
  raw<-omop_variable(event=e,reducer=omop_reduce("first_value",missing="keep"))
  coordinate<-function(unit)omop_number(raw,omop_number_spec(unit=omop_number_unit("unit:8554",unit,1)))
  age<-omop_age_at(omop_anchor(e,which="first",scope="episode"))
  sex<-omop_variable(table="person",column="gender_concept_id")
  list(event=e,raw=raw,coordinate=coordinate,age=age,sex=sex,
    map=list(female="concept:8532",male="concept:8507"))
}
