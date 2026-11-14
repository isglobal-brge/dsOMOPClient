# Administrator-only independent checks over public synthetic source records.
# Never sourced by a reader lesson and never added to the helper download.
verify_longitudinal <- function(input, env, get_data) {
  if (!grepl('book-16-longitudinal', input)) return(invisible(NULL))
  same <- function(x, y, tolerance=1e-7) stopifnot(isTRUE(all.equal(
    unname(as.numeric(x)), unname(as.numeric(y)), tolerance=tolerance)))
  same_values <- function(x,y) same(sort(x,na.last=TRUE),sort(y,na.last=TRUE))
  db <- DBI::dbConnect(RSQLite::SQLite(),env$practice$database)
  on.exit(DBI::dbDisconnect(db),add=TRUE)
  labs <- DBI::dbGetQuery(db,paste('SELECT person_id, measurement_id, measurement_date, value_as_number',
    'FROM measurement WHERE measurement_concept_id=3000963 AND unit_concept_id=8713',
    'ORDER BY person_id,measurement_date,measurement_id'))
  labs$day <- ave(as.numeric(as.Date(labs$measurement_date)),labs$person_id,FUN=function(x)x-min(x))
  by_person <- split(labs,labs$person_id)
  actual <- get_data('D_windows')
  for (period in 1:3) {
    expected <- lapply(by_person,function(x) {
      x <- x[x$day >= (period-1)*1000 & x$day < period*1000,]
      c(first=if(nrow(x)) x$value_as_number[1] else NA_real_,
        last=if(nrow(x)) tail(x$value_as_number,1) else NA_real_)
    })
    expected <- do.call(rbind,expected)
    for (col in c('first','last')) same_values(actual[[paste0(col,period)]],expected[,col])
    result <- env$window_results[[period]]
    same(result$First,mean(expected[,'first'],na.rm=TRUE))
    same(result$Last,mean(expected[,'last'],na.rm=TRUE))
    same(result$Observed,sum(!is.na(expected[,'first'])))
    same(result$Missing,sum(is.na(expected[,'first'])))
    counts <- env$missing_counts[[period]]
    expected_counts <- table(ifelse(is.na(expected[,'first']),'missing','observed'),useNA='always')
    same(counts$Count,as.numeric(expected_counts))
  }
  reshaped <- get_data('D_reshaped')
  stopifnot(nrow(reshaped)==84L,length(unique(reshaped$person_id))==28L)
  for(i in 1:3) same_values(reshaped$first[reshaped$period==i],actual[[paste0('first',i)]])
  same(book_counts(env$period_counts)$Count,c(28,28,28,0))
  gdb <- DBI::dbConnect(RSQLite::SQLite(),file.path(dirname(env$practice$database),'gibleed.sqlite'))
  visits <- DBI::dbReadTable(gdb,'visit_occurrence');DBI::dbDisconnect(gdb)
  visits <- visits[visits$visit_concept_id==9201L,]
  dates <- as.numeric(as.Date(visits$visit_start_date))
  same(env$visit_time$Global.Mean[1,'EstimatedMean'],mean(dates-ave(dates,visits$person_id,FUN=min)))
  same(book_counts(env$visit_counts)$Count,c(nrow(visits),0))
  stopifnot(book_counts(env$visit_counts)$Category[1]=='Inpatient Visit')
  # Panel last values independently matched to the wide first/last windows.
  panel <- get_data('D_panel')
  for (i in 1:3) {
    p <- panel[panel$period_start_days==(i-1)*1000,]
    at <- match(p$person_id,actual$person_id)
    same_values(p$hb_last,actual[[paste0('last',i)]][at])
  }
  # Source-level daily risk set, stopping at outcome, death and coverage.
  source_days <- function(path) {
    db <- DBI::dbConnect(RSQLite::SQLite(),path);on.exit(DBI::dbDisconnect(db))
    ids <- DBI::dbReadTable(db,'person')$person_id
    obs <- DBI::dbReadTable(db,'observation_period')
    death <- DBI::dbReadTable(db,'death')
    do.call(rbind,lapply(ids,function(id) {
      x <- labs[labs$person_id==id,]; index <- min(as.Date(x$measurement_date))
      next_day <- if(any(x$day>0)) min(x$day[x$day>0]) else Inf
      dd <- as.numeric(as.Date(death$death_date[death$person_id==id])-index)
      stop_day <- min(c(7300,next_day,dd),na.rm=TRUE)
      day <- if(stop_day>=1) seq_len(stop_day) else integer()
      coverage <- obs[obs$person_id==id,]
      inside <- vapply(day,function(d) any(index+d>=as.Date(coverage$observation_period_start_date) &
        index+d<=as.Date(coverage$observation_period_end_date)),logical(1))
      day <- day[inside]
      data.frame(event=as.integer(day==next_day),years=day/365.25)
    }))
  }
  frames <- DSLite::getDSLiteData(env$two_conns,'D_hazard')
  source <- lapply(env$pair$path,source_days)
  canonical <- function(d) {
    at <- order(d$years,d$event)
    data.frame(event=as.integer(d$event[at]),years=as.numeric(d$years[at]))
  }
  for(site in names(frames)) {
    stopifnot(identical(canonical(frames[[site]]),canonical(source[[site]])))
    events <- tapply(frames[[site]]$event,frames[[site]]$person_id,sum)
    stopifnot(all(events<=1),all(frames[[site]]$days==1))
  }
  pooled <- do.call(rbind,source)
  ref <- glm(event~years,data=pooled,family=binomial())
  same(env$hazard_fit$coefficients[,1],coef(ref),1e-6)
  # stats::glm's stored QR can refer to its preceding iteration. Rebuild the
  # binomial Fisher information at the converged fit, matching ds.glm's release.
  X <- model.matrix(ref); p <- fitted(ref)
  final_se <- sqrt(diag(solve(crossprod(X,X*p*(1-p)))))
  same(env$hazard_fit$coefficients[,2],final_se,1e-6)
  same(env$hazard_fit$dev,deviance(ref),1e-6)
  same(env$hazard_fit$Nvalid,nrow(pooled))
  stopifnot(env$hazard_fit$iter<30,all(env$hazard_fit$disclosure.risk==0),
    all(env$hazard_fit$errorMessage=='No errors'))
  same(book_counts(env$hazard_counts)$Count,c(sum(pooled$event==0),sum(pooled$event==1),0))
  # BMI site estimates and explicit inverse-variance pooling from source records.
  bmi <- DBI::dbGetQuery(db,paste('SELECT person_id, measurement_id, measurement_date, value_as_number',
    'FROM measurement WHERE measurement_concept_id=3038553 AND unit_concept_id=9531',
    'ORDER BY person_id,measurement_date,measurement_id'))
  bmi$years <- ave(as.numeric(as.Date(bmi$measurement_date)),bmi$person_id,FUN=function(x)(x-min(x))/365.25)
  bmi$value <- bmi$value_as_number
  bmi$above_cutoff <- as.integer(bmi$value>=25)
  repeat_frames <- DSLite::getDSLiteData(env$two_conns,'D_repeat')
  estimates <- list(linear=list(),binary=list())
  for (site in names(env$pair$groups)) {
    d <- bmi[bmi$person_id %in% env$pair$groups[[site]],]
    a <- repeat_frames[[site]]
    same_values(a$value,d$value);same_values(a$years,d$years)
    same_values(a$above_cutoff,d$above_cutoff)
    d$person_id <- factor(d$person_id)
    estimates$linear[[site]] <- summary(lme4::lmer(value~years+(1|person_id),data=d))$coefficients[,1:2]
    estimates$binary[[site]] <- summary(lme4::glmer(above_cutoff~years+(1|person_id),family=binomial(),data=d))$coefficients[,1:2]
  }
  for(kind in names(estimates)) {
    fit <- env[[paste0(kind,'_mixed')]]
    stopifnot(fit$num.valid.studies==2,
      all(grepl('no convergence error reported',fit$Convergence.error.message)))
    beta <- do.call(cbind,lapply(estimates[[kind]],function(x)x[,1]))
    se <- do.call(cbind,lapply(estimates[[kind]],function(x)x[,2]))
    same(fit$betamatrix.all,beta,1e-4);same(fit$sematrix.all,se,1e-4)
    message(kind, " source-fit maximum coefficient difference: ", max(abs(fit$betamatrix.all-beta)),
      "; maximum SE difference: ",max(abs(fit$sematrix.all-se)))
    # Verify pooling from the independently checked RELEASED site statistics.
    # Reordering random-effect levels can slightly change glmer optimization.
    beta <- fit$betamatrix.all; se <- fit$sematrix.all
    weights <- 1/se^2
    same(fit$SLMA.pooled.ests.matrix[,'pooled.FE'],rowSums(beta*weights)/rowSums(weights),1e-10)
    same(fit$SLMA.pooled.ests.matrix[,'se.FE'],sqrt(1/rowSums(weights)),1e-10)
    for(s in c('study1','study2')) stopifnot(fit$output.summary[[s]]$disclosure.risk==0,
      fit$output.summary[[s]]$errorMessage=='No errors',all(as.data.frame(fit$output.summary[[s]]$varcor)$vcov > 1e-8))
  }
  same(env$repeat_mean$Global.Mean[1,'EstimatedMean'],mean(bmi$value))
  same(book_counts(env$repeat_counts)$Count,c(sum(bmi$above_cutoff==0),sum(bmi$above_cutoff==1),0))
  message('LONGITUDINAL PASS: source windows/missingness, reshape, observation-censored first-event daily risk set, central GLM, both site mixed models and inverse-variance pooling')
}
