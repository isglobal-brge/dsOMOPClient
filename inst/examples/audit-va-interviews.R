# Synthetic VA teaching composition. A recorded visit identifies an interview
# only when the study's ETL contract explicitly guarantees that association.
# The input concept IDs and public labels are instrument-version declarations.
audit_va_interviews <- function(interview_event,question_one,question_two,form_revision,
                                answer_labels,form_labels) {
  answer<-function(id)omop_variable(event=omop_event("observation",id,visit="index"),
    reducer=omop_reduce("last_value",value="value_as_concept_id",missing="keep"))
  omop_recipe(variables=list(answer_one=answer(question_one),answer_two=answer(question_two),
    form_revision=answer(form_revision),
    recorded_cause=omop_variable(table="death",column="cause_concept_id")),
    populations=omop_population("interviews",index=omop_index(interview_event,select="all")),
    outputs=omop_output("interviews",rows="interviews"),labels=c(answer_labels,form_labels))
}
