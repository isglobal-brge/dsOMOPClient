# Assertions for the review's misleading-output findings, on public practice data.
verify_book4_fix1 <- function(input, env) {
  if (grepl('book-04-vocabulary', input)) {
    stopifnot('Childhood asthma' %in% env$desc$pooled$concept_name,
      'Asthma' %in% subset(env$parents$pooled, direction=='ancestor')$concept_name,
      is.null(env$serum_glucose_units$pooled),
      any(grepl('disclosure-safe',env$serum_glucose_units$meta$warnings)),
      env$glucose_id==3000483L,env$bmi_id==3038553L,
      nrow(env$glucose_units$pooled)==1L,
      env$glucose_units$pooled$concept_name=='milligram per deciliter',
      env$glucose_units$pooled$n==225L,
      env$bmi_locations$pooled$n_records==435L,
      env$bmi_locations$pooled$n_persons==25L,
      env$bmi_units$pooled$concept_name=='kilogram per square meter',
      env$bmi_units$pooled$n==435L,
      identical(readRDS(env$concept_file)$measured_bmi,env$bmi_set))
    message('REVIEW1 DISCOVERY PASS: hierarchy directions, visible refusal, admitted glucose units and measured BMI')
  }
  if (grepl('book-07-populations', input)) {
    stopifnot(identical(env$compiled_membership$populations[[1]]$index$select,'all'))
    d <- DSLite::getDSLiteData(env$conns,'D_all_visitors')$site
    stopifnot(nrow(d)==30L,length(unique(d$person_id))==18L)
    message('REVIEW1 MEMBERSHIP PASS: compiled all selector yields repeated episodes')
  }
  if (grepl('book-09-dates', input)) {
    stopifnot(env$calendar$date_spec$precision=='month',
      env$calendar$date_spec$representation=='factor',
      env$calendar$derived$reference$event$table=='visit_occurrence',
      env$duration$from$event$table=='visit_occurrence',
      env$duration$to$event$table=='death',env$duration$unit=='days')
    message('REVIEW1 DATES PASS: calendar source, precision and representation versus duration endpoints/unit')
  }
  if (grepl('book-10-harmonise', input)) {
    stopifnot(identical(env$selected$event$concepts,env$education$event$concepts),
      identical(env$selected$event$concepts,env$without_labels$event$concepts),
      identical(names(env$selected$labels),'900102'),
      identical(unname(env$selected$labels),'University education'),
      !is.null(env$selected$event$concepts))
    message('REVIEW1 LABELS PASS: concrete answer identity/label and unchanged question selection')
  }
  if (grepl('book-15-formulas', input)) {
    stopifnot(is.null(env$serum_locations$pooled),
      any(grepl('Strict pooling failed',env$serum_locations$meta$warnings)),
      all(c('Creatinine [Mass/volume] in Blood',
        'Creatinine [Mass/volume] in Serum or Plasma') %in% env$creatinine_hits$concept_name))
    message('REVIEW1 PREREQUISITES PASS: vocabulary availability distinguished from failed pooled locator')
  }
}
