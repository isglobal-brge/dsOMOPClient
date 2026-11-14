# Source-data and whole-object checks; never part of the analyst workflow.
# precompute.R snapshots the synthetic objects just before refusal/connection switch.
verify_standalone_showcase <- function(input, env, get_data) {
  if (!grepl("book-11-parameters|book-showcase", input)) return(invisible(NULL))
  same <- function(x,y) stopifnot(isTRUE(all.equal(as.numeric(x),as.numeric(y), tolerance=1e-8)))
  summary_matches <- function(actual, x) same(book_summary(actual)$Value,
    c(quantile(x,c(.05,.1,.25,.5,.75,.9,.95),na.rm=TRUE),mean(x,na.rm=TRUE)))
  counts_match <- function(actual,x) {
    released <- book_counts(actual)
    expected <- table(x,useNA="always")
    # Compare named nonmissing categories and the explicit missing cell.
    for (i in seq_len(nrow(released))) {
      category <- released$Category[i]
      n <- if (is.na(category) || category %in% c("NA","<NA>")) sum(is.na(x)) else sum(x==category,na.rm=TRUE)
      same(released$Count[i],n)
    }
    same(sum(released$Count),length(x))
  }
  db <- DBI::dbConnect(RSQLite::SQLite(),env$practice$database)
  on.exit(DBI::dbDisconnect(db),add=TRUE)
  if (grepl("book-11-parameters", input)) {
    weights <- DBI::dbGetQuery(db,paste("SELECT person_id,measurement_id,measurement_date,value_as_number",
      "FROM measurement WHERE measurement_concept_id=3025315 AND unit_concept_id=9529",
      "ORDER BY person_id,measurement_date,measurement_id"))
    heights <- DBI::dbGetQuery(db,paste("SELECT person_id,measurement_id,measurement_date,value_as_number",
      "FROM measurement WHERE measurement_concept_id=3036277 AND unit_concept_id=8582",
      "ORDER BY person_id,measurement_date,measurement_id"))
    people <- DBI::dbReadTable(db,"person")
    oracle <- do.call(rbind,lapply(split(weights,weights$person_id),function(w) {
      first <- as.Date(w$measurement_date[1L]); last <- as.Date(tail(w$measurement_date,1L))
      h <- heights[heights$person_id==w$person_id[1L] & heights$measurement_date==w$measurement_date[1L],]
      birth <- as.Date(people$birth_datetime[match(w$person_id[1L],people$person_id)])
      age <- as.integer(format(first,"%Y"))-as.integer(format(birth,"%Y"))-
        as.integer(format(first,"%m-%d")<format(birth,"%m-%d"))
      data.frame(weight_kg=w$value_as_number[1L],height_cm=h$value_as_number[1L],age=age,
        first_date=first,last_date=last)
    }))
    original <- get_data("D_body"); final <- get_data("D_bmi")
    # Compare complete tuples to source, not just unrelated marginal summaries.
    order_rows <- function(x) x[do.call(order,x),,drop=FALSE]
    actual <- order_rows(as.data.frame(original[names(oracle)])); expected <- order_rows(oracle)
    rownames(actual)<-rownames(expected)<-NULL
    stopifnot(isTRUE(all.equal(actual,expected,check.attributes=FALSE)),nrow(original)==28L,
      identical(env$rollback_snapshot,final),
      identical(original[names(original)],final[names(original)]),
      !"bmi" %in% names(original),"bmi_copy" %in% names(final),
      grepl("sqrt",env$unknown_message),grepl("protected",env$key_message),
      grepl("protected",env$rollback_message),
      grepl("blocked|class-1",env$blocked_message))
    bmi <- oracle$weight_kg/(oracle$height_cm/100)^2
    summary_matches(env$bmi_result,bmi)
    summary_matches(env$before_bmi,bmi);summary_matches(env$after_bmi,bmi)
    same(final$bmi,original$weight_kg/(original$height_cm/100)^2)
    same(final$bmi_copy,final$bmi)
    same(get_data("D_span")$span_days,as.numeric(original$last_date-original$first_date))
    summary_matches(env$span_result,as.numeric(oracle$last_date-oracle$first_date))
    summary_matches(env$years_result,as.numeric(oracle$last_date-oracle$first_date)/365)
    counts_match(env$age_counts,cut(oracle$age,c(0,18,121),labels=c("under 18","18 to 120"),right=FALSE))
    counts_match(env$adult_counts,ifelse(oracle$age>=18,"yes","no"))
    counts_match(env$recode_counts,ifelse(oracle$age>=18,"yes","no"))
    counts_match(env$stage_counts,ifelse(oracle$age<5,"young child",ifelse(oracle$age<18,"older child","adult")))
    summary_matches(env$filtered_result,bmi[oracle$age>=18])
    summary_matches(env$working_result,bmi[oracle$age>=18])
    stopifnot(nrow(get_data("D_adult_bmi"))==14L,nrow(get_data("D_working_age"))==14L,
      identical(get_data("D_recoded")$adult,get_data("D_adults")$adult))
    message("STANDALONE PASS: 28 source tuples; all BMI/date/age/filter solutions; exact rollback including attributes")
  } else {
    snap <- env$showcase_snapshot
    stopifnot(length(env$named_tables)==2L, identical(env$named_tables[[1]],env$named_tables[[2]]),
      identical(as.numeric(env$named_tables[[1]]),c(6,6)))
    summary_matches(env$age_result,rep(c(43,33,NA,NA),6))
    stopifnot(setequal(na.omit(as.character(snap$D_tour_dates$month)),c("Mar 2020","Jun 2020","Jul 2020")),
      nrow(snap$D_tour_va)==24L,
      identical(snap$D_tour_va$age_at_death,rep(c(43L,33L,NA_integer_,NA_integer_),6)),
      identical(as.character(snap$D_tour_va$cause_of_death),rep(c("Myocardial infarction","Pneumonia","Myocardial infarction",NA),6)))
    for (shape in c("wide","long","panel","survival","features","baseline","intervals",
                    "sparse","temporal","membership","dictionary","joined")) {
      d<-snap[[paste0("D_tour_",shape)]]
      stopifnot(is.data.frame(d),nrow(d)>0)
    }
    stopifnot(nrow(snap$D_tour_wide)==18L,nrow(snap$D_tour_long)==30L,
      nrow(snap$D_tour_panel)==54L,nrow(snap$D_tour_survival)==18L,
      identical(snap$D_tour_wide$visits,snap$D_tour_joined$visits),
      all(snap$D_tour_survival$entry_days_from_index==1L),
      all(snap$D_tour_survival$exit_days_from_index<=365L))
    m<-DBI::dbGetQuery(db,paste("SELECT person_id,value_as_number FROM measurement",
      "WHERE measurement_concept_id=3000963 AND unit_concept_id=8713"))
    means<-vapply(split(m$value_as_number,m$person_id),mean,0)
    same(sort(get_data("D_tour_labs")$hb),sort(means))
    same(env$mean_result$Global.Mean[1,1],mean(means))
    same(env$band_display$Count[order(env$band_display$Category)],c(9,9,10))
    summary_matches(env$selected_result,10*means[means>=14])
    summary_matches(env$all_result,10*means)
    stopifnot(nrow(get_data("D_tour_selected"))==19L)
    message("SHOWCASE PASS: two harmonised sites, ages/dates, 12 shapes, VA, numeric counts, converted/filter summaries and solution")
  }
}
