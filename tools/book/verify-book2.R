# Synthetic-only checks of the actual objects created by the analyst chunks.
# This file is never sourced into a real analyst session or rendered into lessons.
verify_book2 <- function(input, env, get_data) {
  same_values <- function(x, expected) stopifnot(isTRUE(all.equal(
    sort(as.numeric(x), na.last=TRUE), sort(as.numeric(expected), na.last=TRUE))))
  if (grepl("book-06-events", input)) {
    db <- DBI::dbConnect(RSQLite::SQLite(),env$practice$database)
    on.exit(DBI::dbDisconnect(db),add=TRUE)
    source <- DBI::dbGetQuery(db,"SELECT person_id,measurement_id,measurement_date,value_as_number FROM measurement WHERE measurement_concept_id=3000963 AND unit_concept_id=8713 ORDER BY person_id,measurement_date,measurement_id")
    people <- split(source,source$person_id)
    d <- get_data("D_reducers")
    stopifnot(nrow(d)==28L)
    oracle <- lapply(people,function(x) {
      v<-x$value_as_number;v<-v[!is.na(v)]
      c(first=v[1],last=tail(v,1),mean=mean(v),days=length(unique(x$measurement_date)),
        ever=1L,any_high=as.integer(any(v>=16)),all_high=as.integer(all(v>=14)))
    })
    oracle <- do.call(rbind,oracle)
    for (name in colnames(oracle)) same_values(d[[name]],oracle[,name])
    same_values(get_data("D_at_index")$hemoglobin,oracle[,"first"])
    same_values(get_data("D_row_count")$rows,vapply(people,nrow,integer(1)))
    same_values(get_data("D_converted")$mean_gl,10*oracle[,"mean"])
    stopifnot(all(get_data("D_lab_ranges")$high_count==0L),
      all(get_data("D_lab_ranges")$low_count==0L))
    bins<-get_data("D_lab_bins")
    stopifnot(identical(as.integer(bins$high),as.integer(d$mean>=15)),
      identical(as.integer(bins$low),as.integer(d$mean<14)))
    # Test actual released objects, including the exercise, against independent
    # queries of the PUBLIC synthetic data (never an analyst workflow).
    summaries <- list(first = env$first_result, last = env$last_result,
      mean = env$mean_result, days = env$days_result,
      rows = env$solution_result, converted = env$conversion_result,
      at_index = env$at_index_result)
    expected <- list(first = oracle[,"first"], last = oracle[,"last"],
      mean = oracle[,"mean"], days = oracle[,"days"],
      rows = vapply(people,nrow,integer(1)), converted = 10*oracle[,"mean"],
      at_index = oracle[,"first"])
    for (name in names(summaries)) {
      released <- book_summary(summaries[[name]])
      stopifnot(nrow(released) == 8L,
        isTRUE(all.equal(released$Value, unname(c(quantile(expected[[name]],
          probs=c(.05,.1,.25,.5,.75,.9,.95)), Mean=mean(expected[[name]]))))))
    }
    tables <- list(any = env$any_result, all = env$all_result,
      ever = env$ever_result, bins = env$bins_result,
      ranges = env$ranges_result)
    expected_counts <- list(any = c(9,19,0), all = c(24,4,0), ever = 28,
      bins = as.numeric(table(factor(as.character(cut(oracle[,"mean"], c(0,14,15,100),
        labels=c("below 14","14 to below 15","15 or above"), right=FALSE)),
        levels=c("below 14","14 to below 15","15 or above")))),
      ranges = 28)
    expected_counts$bins <- c(expected_counts$bins, 0)
    for (name in names(tables)) {
      counts <- book_counts(tables[[name]])$Count
      stopifnot(identical(counts, as.numeric(expected_counts[[name]])), sum(counts)==28)
    }
    # A normal-return refusal must never be formatted as successful/empty data.
    message("EXPECTED REFUSAL PROBE: exact measurement-day frequency table")
    refused <- dsBaseClient::ds.table("D_reducers$days", useNA="no", datasources=env$conns)
    stopifnot(grepl("All studies failed", refused$validity.message),
      inherits(try(book_counts(refused), silent=TRUE), "try-error"))
    message("AGGREGATES PASS: seven summaries, five tables, refusal formatter guard")

  }
  if (grepl("book-07-populations", input)) {
    stopifnot(nrow(get_data("D_members"))==18L,
      nrow(get_data("D_eligible"))==12L,
      nrow(get_data("D_covered"))==12L,
      nrow(get_data("D_cohort"))==12L,
      nrow(get_data("D_new_users"))==18L,
      all(get_data("D_new_users")$prior_fills==0),
      nrow(get_data("D_all_visitors"))==30L,
      length(unique(get_data("D_all_visitors")$person_id))==18L)
    same_values(get_data("D_eligible")$visits, rep(2,12))
    d <- get_data("D_matched")
    stopifnot(nrow(d)==36L, sum(d$is_case)==18L, all(d$weight>0))
  }
  if (grepl("book-08-outputs", input)) {
    for (format in c("wide","long","features","baseline","panel","temporal",
                    "survival","intervals","sparse","membership","dictionary","joined")) {
      d <- get_data(paste0("D_fmt_",format))
      stopifnot(is.data.frame(d), nrow(d)>0)
    }
    w <- get_data("D_fmt_wide"); j <- get_data("D_fmt_joined")
    stopifnot(nrow(w)==28L, nrow(j)==28L,
      identical(w$visits,j$visits), identical(w$age,j$age),
      nrow(get_data("D_fmt_long"))==1735L,
      nrow(get_data("D_fmt_panel"))==84L,
      nrow(get_data("D_fmt_intervals"))==1735L,
      nrow(get_data("D_fmt_membership"))==28L,
      is.numeric(w$hemoglobin))
    s <- get_data("D_fmt_survival")
    stopifnot(nrow(s)==28L, all(s$entry_days_from_index==1L),
      all(s$follow_up_days==s$exit_days_from_index-s$entry_days_from_index))
    db<-DBI::dbConnect(RSQLite::SQLite(),env$practice$database)
    on.exit(DBI::dbDisconnect(db),add=TRUE)
    visits<-DBI::dbGetQuery(db,"SELECT person_id,visit_concept_id,visit_start_date FROM visit_occurrence WHERE visit_concept_id IN (9201,9202)")
    obs<-DBI::dbReadTable(db,"observation_period");deaths<-DBI::dbReadTable(db,"death")
    expected<-lapply(split(visits,visits$person_id),function(x) {
      pid<-x$person_id[1];index<-min(as.Date(x$visit_start_date))
      outcomes<-as.Date(x$visit_start_date[x$visit_concept_id==9201]);outcomes<-outcomes[outcomes>index]
      censor<-min(c(as.Date(obs$observation_period_end_date[obs$person_id==pid]),
        as.Date(deaths$death_date[deaths$person_id==pid])),na.rm=TRUE)
      event<-length(outcomes)>0 && min(outcomes)<=censor
      exit<-if(event) min(outcomes) else censor
      c(event=as.integer(event),exit=as.integer(exit-index))
    })
    expected<-do.call(rbind,expected)
    same_values(s$event,expected[,"event"])
    same_values(s$exit_days_from_index,expected[,"exit"])
    for (format in c("temporal","sparse")) {
      d <- get_data(paste0("D_fmt_",format))
      ref <- get_data(paste0("D_fmt_",format,".covariateRef"))
      stopifnot(all(d$covariate_id %in% ref$covariate_id), all(d$covariate_value!=0))
    }
    symbols <- DSI::datashield.symbols(env$conns)$site
    stopifnot(!any(c("D_counts","D_ages") %in% symbols))
  }
  if (grepl("book-09-dates", input)) {
    stopifnot(inherits(get_data("D_date_exact")$visit_date,"Date"),
      is.ordered(get_data("D_date_months")$month),
      identical(levels(get_data("D_date_months")$month),
        c("2020-03","2020-04","2020-05","2020-06","2020-07")),
      inherits(get_data("D_date_bounds")$start,"Date"),
      inherits(get_data("D_date_bounds")$end,"Date"))
    same_values(get_data("D_date_numbers")$month_index,rep(c(5,2,6,NA),6))
    same_values(get_data("D_month_lengths")$days,rep(c(30,31,31,NA),6))
    same_values(get_data("D_date_ages")$at_death,rep(c(43,33,NA,NA),6))
    same_values(get_data("D_date_ages")$at_question,rep(c(39,29,NA,NA),6))
    same_values(get_data("D_date_estimated")$age,rep(c(43,33,23,NA),6))
    stopifnot(sum(is.na(get_data("D_date_partial")$birth_year))==0L,
      setequal(na.omit(as.character(get_data("D_date_custom")$month)),
        c("Mar 2020","Jun 2020","Jul 2020")))
    same_values(get_data("D_date_elapsed")$days,
      rep(c(as.integer(as.Date("2024-06-01")-as.Date("2020-06-15")),
            as.integer(as.Date("2024-03-14")-as.Date("2020-03-14")),
            as.integer(as.Date("2024-05-10")-as.Date("2020-07-20")),NA),6))
  }
  if (grepl("book-08-harmonise", input)) {
    # Single-site objects were released before the temporary connections closed.
    stopifnot(identical(dimnames(env$site_tables[[1L]]),dimnames(env$site_tables[[2L]])),
      identical(dimnames(env$site_tables[[1L]])[[1L]],
        c("Secondary education","University education")))
  }
  if (grepl("book-11-parameters", input)) {
    stopifnot(nrow(get_data("D_param_outpatient"))==28L,
      nrow(get_data("D_param_inpatient"))==7L,
      all(get_data("D_shortcuts")$ever==1),
      all(get_data("D_derived")$twice==2L*get_data("D_shortcuts")$count),
      nrow(get_data("D_high"))==13L, all(get_data("D_high")$value>=14))
  }
  if (grepl("book-12-privacy", input)) {
    stopifnot(grepl("insufficient_support|disclosure|subset|threshold",env$small_message,ignore.case=TRUE),
      grepl("value_as_string",env$text_message),
      grepl("protected|person_id",env$key_message),
      nrow(get_data("D_private"))==24L,
      inherits(get_data("D_private_date")$visit_date,"Date"))
    stopifnot(!any(c("D_too_small","D_key_copy") %in% DSI::datashield.symbols(env$conns)$site))
  }
}
