# Synthetic-only checks of the actual objects created by the analyst chunks.
# This file is never sourced into a real analyst session or rendered into lessons.
verify_book2 <- function(input, env, get_data) {
  same_values <- function(x, expected) stopifnot(isTRUE(all.equal(
    sort(as.numeric(x), na.last=TRUE), sort(as.numeric(expected), na.last=TRUE))))
  if (grepl("book-06-events", input)) {
    d <- get_data("D_reducers")
    stopifnot(nrow(d)==24L)
    for (name in c("first","last","days","ever","any_high","all_high")) {
      expected <- switch(name, first=c(60,80,100,NA), last=c(66,80,100,NA),
        days=c(2,2,1,0), ever=c(1,1,1,0), any_high=c(0,1,1,0), all_high=c(0,NA,1,NA))
      same_values(d[[name]], rep(expected,6))
    }
    same_values(get_data("D_at_index")$weight, rep(c(60,80,100),6))
    same_values(get_data("D_row_count")$rows, d$days)
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
    stopifnot(nrow(w)==18L, nrow(j)==18L,
      identical(w$visits,j$visits), identical(w$age,j$age),
      nrow(get_data("D_fmt_long"))==30L,
      nrow(get_data("D_fmt_panel"))==54L,
      nrow(get_data("D_fmt_intervals"))==18L,
      nrow(get_data("D_fmt_membership"))==18L)
    s <- get_data("D_fmt_survival")
    stopifnot(nrow(s)==18L, sum(s$event)==12L,
      all(s$entry_days_from_index==1L),
      all(s$follow_up_days==s$exit_days_from_index-s$entry_days_from_index))
    # Death censors the no-admission pattern on 2024-05-10, not observation end.
    same_values(s$exit_days_from_index[s$event==0L],
      rep(as.integer(as.Date("2024-05-10")-as.Date("2020-07-20")),6))
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
    stopifnot(nrow(get_data("D_param_outpatient"))==18L,
      nrow(get_data("D_param_inpatient"))==12L,
      all(get_data("D_shortcuts")$ever==1),
      all(get_data("D_derived")$twice==2L*get_data("D_shortcuts")$count),
      nrow(get_data("D_high"))==12L, all(get_data("D_high")$value>=70))
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
