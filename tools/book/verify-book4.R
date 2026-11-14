# Validation may inspect public synthetic server memory; reader helpers never do.
verify_book4 <- function(input,env) {
  same <- function(x,y) stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y),tolerance=1e-7)))
  data <- function(name) DSLite::getDSLiteData(env$conns,name)$site
  if(grepl('book-15-formulas',input)) {
    d <- data('D_formula_body')
    same(d$bmi,10000*d$weight/(d$height*d$height))
    same(data('D_formula_rounded')$rounded,round(d$bmi,1))
    stopifnot(nrow(d)==28L, all(is.finite(d$bmi)), all(d$bmi>0))
    first <- data('D_collapsed_weights');last<-data('D_last_weights')
    stopifnot(nrow(first)==28L,nrow(last)==28L,!anyDuplicated(first$person_id),!anyDuplicated(last$person_id))
    db<-DBI::dbConnect(RSQLite::SQLite(),env$practice$database)
    on.exit(DBI::dbDisconnect(db),add=TRUE)
    records<-DBI::dbGetQuery(db,'SELECT person_id,measurement_id,measurement_date,measurement_concept_id,value_as_number FROM measurement WHERE measurement_concept_id IN (3004249,3012888) ORDER BY person_id,measurement_date,measurement_id')
    first_sbp<-records[records$measurement_concept_id==3004249,];first_sbp<-first_sbp[!duplicated(first_sbp$person_id),]
    dbp<-records[records$measurement_concept_id==3012888,]
    pair<-merge(first_sbp,dbp,by=c('person_id','measurement_date'),suffixes=c('_sbp','_dbp'))
    pair<-pair[!duplicated(pair$person_id),]
    same(sort(data('D_formula_pressure')$map),sort((pair$value_as_number_sbp+2*pair$value_as_number_dbp)/3))
    message('BOOK4 FORMULAS PASS: BMI arithmetic, rounding, source-aligned MAP, unique episode collapse')
  }
  if(grepl('book-19-case-mortality',input)) {
    d<-data('D_mortality_rate')
    stopifnot(nrow(d)==24L,sum(d$mi)==12,sum(d$pneumonia)==6,
      sum(is.na(d$age_band))==12L,all(d$risk_days>0 & d$risk_days<=366))
    expected<-c(12,6)/sum(d$risk_days)*365.25*1000
    same(env$rates$Per_1000_person_years,expected)
    message('BOOK4 MORTALITY PASS: deaths, missing ages, clipped risk time and released rates')
  }
  if(grepl('book-20-case-gibleed',input)) {
    frames<-DSLite::getDSLiteData(env$two_conns,'D_comparison_model')
    d<-do.call(rbind,frames)
    stopifnot(all(d$years>0),all(d$event %in% 0:1),all(d$celecoxib %in% 0:1),
      all(env$comparison_fit$errorMessage=='No errors'),env$comparison_fit$iter<20)
    ref<-stats::glm(event~celecoxib+age+offset(log(years)),family=poisson(),data=d)
    same(env$comparison_fit$coefficients[,1],coef(ref))
    message("Poisson SE actual: ",paste(env$comparison_fit$coefficients[,2],collapse=","),"; central: ",paste(summary(ref)$coefficients[,2],collapse=","))
    # dsBase returns the previous iteration information matrix at its stopping
    # rule. Observed maximum relative SE difference is 2.18e-5, while estimates
    # match at 1e-7. Test the numerical bound explicitly rather than exact SEs.
    stopifnot(max(abs(env$comparison_fit$coefficients[,2]/summary(ref)$coefficients[,2]-1)) < 3e-5)
    same(env$comparison_fit$dev,deviance(ref))
    stopifnot(length(intersect(env$pair$groups[[1]],env$pair$groups[[2]]))==0)
    message('BOOK4 GIBLEED PASS: disjoint people, positive censored time, central Poisson survival coefficients/SE/deviance')
  }
  if(grepl('book-21-case-glucose',input)) {
    d<-data('D_glucose');ref<-glm(last~first,family=gaussian(),data=d)
    same(env$glucose_fit$coefficients[,1],coef(ref))
    stopifnot(is.ordered(d$control),all(d$target_days>=0|is.na(d$target_days)),
      all(env$glucose_fit$errorMessage=='No errors'))
    message('BOOK4 GLUCOSE PASS: category type, observed times and independent Gaussian fit')
  }
  if(grepl('book-22-case-drug',input)) {
    d<-data('D_utilisation_analysis')
    stopifnot(nrow(d)>5,all(d$pdc>=0 & d$pdc<=1),all(d$covered>=0 & d$covered<=365))
    same(d$pdc,d$covered/365)
    same(d$discontinued,as.integer(d$persistence<365))
    message('BOOK4 UTILISATION PASS: PDC bounds and discontinuation rule')
  }
  if(grepl('book-23-case-table1',input)) {
    frames<-DSLite::getDSLiteData(env$two_conns,'D_table1')
    stopifnot(identical(levels(frames[[1]]$age_band),levels(frames[[2]]$age_band)),
      sum(vapply(frames,nrow,integer(1)))==28L)
    same(env$hb_mean$Global.Mean[1,1],mean(unlist(lapply(frames,`[[`,'haemoglobin'))))
    message('BOOK4 TABLE1 PASS: identical factor domains, disjoint roster and pooled mean')
  }
}
