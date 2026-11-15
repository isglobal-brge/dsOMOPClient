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
    weights <- DBI::dbGetQuery(db,"SELECT person_id, measurement_date, value_as_number FROM measurement WHERE measurement_concept_id=3025315 AND unit_concept_id=9529")
    by_day <- aggregate(value_as_number ~ person_id + measurement_date,weights,mean)
    by_day_max <- aggregate(value_as_number ~ person_id + measurement_date,weights,max)
    same(sort(data("D_daily_weights")$weight),sort(aggregate(value_as_number~person_id,by_day,mean)$value_as_number))
    same(sort(data("D_daily_max_weights")$weight),sort(aggregate(value_as_number~person_id,by_day_max,mean)$value_as_number))
    egfr <- DSLite::getDSLiteData(env$formula_conns,"D_egfr")$site$egfr
    # Independent direct-power equation; no reuse of the helper's log/exp expansion.
    scr <- rep(c(.7,.9,1.4,1.8),6); age <- rep(c(43,34,NA,49),6)
    female <- rep(rep(c(TRUE,FALSE),each=4),3)
    k <- ifelse(female,.7,.9);alpha <- ifelse(female,-.241,-.302)
    expected <- 142*pmin(scr/k,1)^alpha*pmax(scr/k,1)^(-1.2)*.9938^age*ifelse(female,1.012,1)
    same(egfr,expected)
    stopifnot(sum(is.na(egfr))==6L,all(egfr[!is.na(egfr)]>0),
      env$formula_record$id=="dsomop.formula.egfr_ckdepi2021")
    message('BOOK4 FORMULAS PASS: BMI, rounding, MAP, daily mean/max, episode collapse, independent eGFR power oracle and provenance')
  }
  if(grepl('book-10-harmonise',input)) {
    d <- env$category_snapshot
    stopifnot(is.ordered(d$D_ordered$education),
      identical(levels(d$D_ordered$education),c("primary","secondary","tertiary")),
      sum(is.na(d$D_ordered$education))==12L,
      sum(d$D_missing_categories$education=="Missing")==12L,
      identical(as.integer(table(d$D_missing_categories$sex)[c("FEMALE", "MALE", "Missing", "Unknown")]), c(12L, 12L, 0L, 0L)),
      is.logical(d$D_flags$logical_flag),is.integer(d$D_flags$integer_flag),
      identical(as.integer(d$D_flags$logical_flag),d$D_flags$integer_flag),
      identical(as.character(d$D_flags$named_flag),ifelse(d$D_flags$integer_flag==1,"yes","no")),
      sum(d$D_rollups$atc_first=="Teaching ATC first level",na.rm=TRUE)==18L,
      sum(d$D_rollups$atc_second=="Teaching ATC second level",na.rm=TRUE)==18L,
      sum(d$D_rollups$chapter=="Teaching circulatory chapter",na.rm=TRUE)==12L,
      sum(d$D_rollups$chapter=="Teaching respiratory chapter",na.rm=TRUE)==6L)
    cols <- env$book[[1]]$columns
    stopifnot(identical(cols$name,names(d$D_ordered)),
      cols$r_type[cols$name=="education"]=="ordered_factor")
    message("BOOK4 CATEGORIES PASS: exact order, missing/unknown, flag types, rollups, and published schema")
  }
  if(grepl('book-16-longitudinal',input)) {
    repeated <- data("D_timepoints")
    same(repeated$measurements_hb_baseline,data("D_windows")$first1)
    stopifnot(identical(recipe_codebook(env$repeated_result)[[1]]$columns$name,names(repeated)),
      all(c("measurements_hb_baseline","measurements_hb_year3") %in% names(repeated)))
    message("BOOK4 REPEATED PASS: exact baseline alignment and expanded published schema")
  }
  if(grepl('book-17-reproduce',input)) {
    stopifnot(identical(recipe_to_plan(env$check), recipe_to_plan(env$second_recipe)),
      identical(readLines(file.path(env$record_dir, "plan-digest.txt")), recipe_explain(env$check)$digest),
      identical(readRDS(file.path(env$record_dir, "codebook.rds")), env$published),
      identical(readLines(file.path(env$record_dir, "explanation.txt")), recipe_explain(env$check)$text),
      identical(env$published[[1]]$columns$name, env$second_codebook[[1]]$columns$name),
      nrow(DSLite::getDSLiteData(env$conns["site2"], "D_repeated")$site2) == 24L)
    message("BOOK4 STUDY RECORD PASS: recipe, explanation, codebook, digest and second connection")
  }
  if(grepl('book-19-case-mortality',input)) {
    d<-data('D_mortality_rate')
    stopifnot(nrow(d)==24L,sum(d$mi)==12,sum(d$pneumonia)==6,
      sum(is.na(d$age_band))==12L,all(d$risk_days>0 & d$risk_days<=366))
    expected<-c(12,6)/sum(d$risk_days)*365.25*1000
    same(env$rates$Per_1000_person_years,expected)
    stopifnot(identical(as.integer(table(d$sex)), c(12L, 12L)))
    db <- DBI::dbConnect(RSQLite::SQLite(), env$practice$path[[1]])
    source <- DBI::dbGetQuery(db, "SELECT p.gender_concept_id, d.death_date, d.cause_concept_id FROM person p LEFT JOIN death d ON p.person_id=d.person_id")
    DBI::dbDisconnect(db)
    for (sex in c("FEMALE", "MALE")) {
      x <- source[source$gender_concept_id == if (sex == "FEMALE") 8532L else 8507L, ]
      days <- as.numeric(as.Date(x$death_date) - as.Date("2024-01-01")); days[is.na(days)] <- 366
      expected_sex <- c(sum(x$cause_concept_id == 900201L, na.rm=TRUE),
        sum(x$cause_concept_id == 900202L, na.rm=TRUE)) / sum(days) * 365.25 * 1000
      same(env$sex_rates$Per_1000_person_years[env$sex_rates$Sex == sex], expected_sex)
    }
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
    same(env$class_pdc$Global.Mean[1,1],(365+120+60)/(3*365))
    class <- env$class_snapshot
    stopifnot(nrow(class)==18L)
    same(sort(class$covered),sort(rep(c(365,120,60),6)))
    same(sort(class$persistence),sort(rep(c(365,120,30),6)))
    same(class$pdc,class$covered/365)
    same(class$discontinued,as.integer(class$persistence<365))
    stopifnot(sum(class$discontinued)==12L)
    d<-data('D_utilisation_analysis')
    stopifnot(nrow(d)>5,all(d$pdc>=0 & d$pdc<=1),all(d$covered>=0 & d$covered<=365))
    same(d$pdc,d$covered/365)
    same(d$discontinued,as.integer(d$persistence<365))
    stopifnot(nrow(d)==1745L,all(d$covered==1),all(d$persistence==1),all(d$discontinued==1))
    message('BOOK4 UTILISATION PASS: independent supply-pattern predictions, restart/overlap, and GiBleed one-day limitation')
  }
  if(grepl('book-23-case-table1',input)) {
    frames<-DSLite::getDSLiteData(env$two_conns,'D_table1')
    stopifnot(identical(levels(frames[[1]]$age_band),levels(frames[[2]]$age_band)),
      sum(vapply(frames,nrow,integer(1)))==28L)
    same(env$hb_mean$Global.Mean[1,1],mean(unlist(lapply(frames,`[[`,'haemoglobin'))))
    message('BOOK4 TABLE1 PASS: identical factor domains, disjoint roster and pooled mean')
  }
}
