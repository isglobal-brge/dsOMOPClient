.library_variant_membership <- new.env(parent = emptyenv())
.library_variant_match_keys <- function(ctx, frame, handle) {
  tokens <- dsOMOP:::.pseudonymizeIdentifiers(data.frame(person_id=1:24),
    dsOMOP:::.personKey(handle),dsOMOP:::.personKeyPublicContract(handle))$person_id
  person <- match(frame$person_id,tokens)
  cases <- setNames(person[frame$is_case==1L],as.character(frame$set_id[frame$is_case==1L]))
  sort(paste(cases[as.character(frame$set_id)],person,frame$is_case))
}
# Acceptance scenarios are study definitions and hand arithmetic, independent
# of the executor. Each case is expanded into the complete artifact bundle.
.library_rows <- function(n = 24L, ...) rep(list(list(...)), n)
.library_date <- function(day) as.character(as.Date("2020-01-01") + day)
.library_replace <- function(conn, table, rows) {
  DBI::dbExecute(conn, paste("DELETE FROM", table))
  if (nrow(rows)) DBI::dbWriteTable(conn, table, rows, append = TRUE)
}
.library_dx <- function(conn, days, concepts = 990005L, people = 1:24, append = FALSE) {
  rows <- do.call(rbind, lapply(seq_along(days), function(i) data.frame(
    condition_occurrence_id = people + (i-1L)*24L, person_id = people,
    condition_concept_id = rep(concepts, length.out = length(days))[i],
    condition_start_date = .library_date(days[i]), condition_end_date = .library_date(days[i]),
    condition_type_concept_id = 0L)))
  if(append) {
    rows$condition_occurrence_id <- rows$condition_occurrence_id + 10000L
    DBI::dbWriteTable(conn,"condition_occurrence",rows,append=TRUE)
  } else .library_replace(conn, "condition_occurrence", rows)
}
.library_rx <- function(conn, starts, ends, supply = ends-starts+1L, concepts = 990006L, people = 1:24) {
  rows <- do.call(rbind, lapply(seq_along(starts), function(i) data.frame(
    drug_exposure_id = people + (i-1L)*24L, person_id = people,
    drug_concept_id = rep(concepts, length.out = length(starts))[i],
    drug_exposure_start_date = .library_date(starts[i]), drug_exposure_end_date = .library_date(ends[i]),
    days_supply = rep(supply, length.out = length(starts))[i], drug_type_concept_id = 0L)))
  .library_replace(conn, "drug_exposure", rows)
}
.library_obs_periods <- function(conn, starts, ends) {
  rows <- do.call(rbind, lapply(seq_along(starts), function(i) data.frame(
    observation_period_id = (1:24)+(i-1L)*24L, person_id = 1:24,
    observation_period_start_date = .library_date(starts[i]),
    observation_period_end_date = .library_date(ends[i]), period_type_concept_id = 0L)))
  .library_replace(conn, "observation_period", rows)
}
.library_variant_specs <- function() {
  cases <- list()
  add <- function(id, case, bindings = list(), mutate = function(conn) NULL,
                  expected, edit = identity, error = NULL, note = case) {
    cases[[paste(id, case, sep = "/")]] <<- list(id = id, case = case, bindings = bindings,
      mutate = mutate, expected = expected, edit = edit, error = error, note = note)
  }
  rows <- .library_rows
  union <- function(r) {r$populations$study$coverage <- omop_coverage(policy = "union", gap = 10L); r}
  uncovered <- function(r) {r$populations$study$coverage <- omop_coverage(on_missing = "keep"); r}
  # T01: annual age despite Dec-31 birthday; repeated episodes; missing fields;
  # short histories and ambiguous coverage have independent case inventories.
  add("demographics.baseline", "repeated_partial", list(select = "all", lookback = 10L),
    function(c) {
      .library_obs_periods(c, -5, 500)
      visits <- DBI::dbReadTable(c, "visit_occurrence"); visits$visit_occurrence_id <- visits$visit_occurrence_id+24L
      visits$visit_start_date <- visits$visit_end_date <- .library_date(1)
      DBI::dbWriteTable(c, "visit_occurrence", visits, append = TRUE)
    }, list(baseline = c(rows(age=40L,sex="M",race="0",ethnicity="0",prior_observed_days=5L),
                         rows(age=40L,sex="M",race="0",ethnicity="0",prior_observed_days=6L))))
  add("demographics.baseline", "missing_demographics", list(lookback=1L),
    function(c) DBI::dbExecute(c,"UPDATE person SET year_of_birth=NULL,gender_concept_id=NULL,race_concept_id=NULL,ethnicity_concept_id=NULL"),
    list(baseline=rows(age=NULL,sex=NULL,race=NULL,ethnicity=NULL,prior_observed_days=1L)))
  add("demographics.baseline", "ambiguous_drop", list(lookback=1L),
    function(c) .library_obs_periods(c,c(-10,-5),c(100,200)), list(), error="insufficient_support")
  # T02/T03: same-date key ordering, missing last and explicit zero; unselected
  # question records cannot change the selected answer or count.
  for (id in c("socioeconomic.education", "socioeconomic.employment")) local({
    id <- id; prefix <- sub("socioeconomic.","",id,fixed=TRUE)
    result <- function(value,count) setNames(list(rep(list(setNames(list(value,count),
      c(paste0(prefix,"_concept_id"),paste0(prefix,"_records")))),24)),prefix)
    concept <- if(prefix=="education") 990001L else 990002L
    add(id,"tied_zero",list(lookback=5L),function(c) {
      ob <- DBI::dbReadTable(c,"observation"); ob$observation_date <- .library_date(-1)
      ob$observation_concept_id <- concept; ob$value_as_concept_id <- rep(c(990101L,NA,0L),each=24)
      .library_replace(c,"observation",ob)
    },result("0",3L))
    add(id,"last_missing",list(lookback=5L),function(c) {
      ob <- DBI::dbReadTable(c,"observation"); ob$observation_date <- .library_date(-1)
      ob$observation_concept_id <- concept; ob$value_as_concept_id <- rep(c(990101L,0L,NA),each=24)
      .library_replace(c,"observation",ob)
    },result(NULL,3L))
    add(id,"empty_questions",list(lookback=1L),expected=result(NULL,0L))
  })
  # T04: affine conversion and both unknown-unit policies, including last NA.
  for (unknown in c("missing","exclude")) local({
    unknown <- unknown
    add("socioeconomic.numeric_observation",paste0("affine_",unknown),
      list(lookback=10L,measure=omop_event("measurement",990003,name="measure",
        unit=omop_unit(8840,convert=list("8554"=c(factor=2,offset=1)),unknown=unknown))),
      function(c) {
        DBI::dbExecute(c,"UPDATE measurement SET unit_concept_id=8554")
        DBI::dbExecute(c,"UPDATE measurement SET measurement_date='2019-12-31',unit_concept_id=999 WHERE measurement_date='2020-01-01'")
      },list(numeric_observation=rows(numeric_value=if(unknown=="missing") NULL else 9,source_records=if(unknown=="missing") 2L else 1L)))
  })
  add("socioeconomic.numeric_observation","affine_value",list(lookback=10L,
    measure=omop_event("measurement",990003,name="measure",unit=omop_unit(8840,convert=list("8554"=c(factor=2,offset=1))))),
    function(c) DBI::dbExecute(c,"UPDATE measurement SET unit_concept_id=8554"),
    list(numeric_observation=rows(numeric_value=9,source_records=1L)))
  # T05: inclusive horizon, absent death, cause zero and duplicate missing-first.
  for (day in c(0L,10L,11L)) local({day<-day
    add("demographics.death_cause",paste0("day_",day),list(horizon=10L),
      function(c) DBI::dbExecute(c,paste0("UPDATE death SET death_date='",.library_date(day),"'")),
      list(death_cause=rows(death_recorded=as.integer(day<=10),death_days=if(day<=10) day else NULL,
                           cause_concept_id=if(day<=10) "0" else NULL)))
  })
  add("demographics.death_cause","first_missing",list(horizon=10L),function(c) {
    DBI::dbExecute(c,"UPDATE death SET death_date='2020-01-01',cause_concept_id=NULL")
    d<-DBI::dbReadTable(c,"death");d$death_date<-.library_date(1);d$cause_concept_id<-990101L
    DBI::dbWriteTable(c,"death",d,append=TRUE)
  },list(death_cause=rows(death_recorded=1L,death_days=0L,cause_concept_id=NULL)))
  # T06-T10: actual library bindings, pinned independent contributions.
  scores<-list(charlson=list(c=201820L,v=1,future=0),chads2=list(c=201820L,v=2,future=1),
    chadsvasc=list(c=201820L,v=4,future=3),dcsi=list(c=c(4174977L,443611L),v=3,future=0),
    hfrs=list(c=373995L,v=3.2,future=0))
  for(kind in names(scores)) for(day in c(-1L,0L,1L)) local({kind<-kind;day<-day;s<-scores[[kind]]
    add(paste0("comorbidity.",kind),if(day<0) "before_index" else if(day==0) "indexed_duplicates" else "future_excluded",list(select="all"),
      function(c) {DBI::dbExecute(c,"UPDATE person SET year_of_birth=1940,gender_concept_id=8532")
        .library_dx(c,rep(day,length(s$c)*2),rep(s$c,each=2))},
      list(score=rep(list(setNames(list(if(day<=0)s$v else s$future),kind)),24)))
  })
  # T11: [-10,-1] inclusive; duplicate days count once; next/previous day excluded.
  add("comorbidity.condition_history","window_duplicates",list(lookback=10L),
    function(c) .library_dx(c,c(-11,-10,-10,-1,0),990004L),
    list(history=rows(history_present=1L,history_days=2L)))
  add("comorbidity.condition_history","empty",list(conditions=numeric(),lookback=10L),
    expected=list(history=rows(history_present=0L,history_days=0L)))
  # T12: nested fills; gap exactly 2 merges, gap 3 separates; persistence only
  # changes course envelopes, never recorded coverage.
  for(persistence in c(0L,1L)) local({persistence<-persistence
    add("drug.era_summary",paste0("gap_nested_p",persistence),list(gap=2L,persistence=persistence),
      function(c) .library_rx(c,c(0,2,12,25),c(9,3,21,29)),
      list(eras=rows(era_count=if(persistence==0)2L else 1L,
        era_days=if(persistence==0)27L else 31L,first_era_days=if(persistence==0)22L else 31L,covered_days=25L)))
  })
  add("drug.era_summary","supply_fallback",list(gap=0L),
    function(c) .library_rx(c,c(0,20,40,60,80,90),c(9,NA,NA,69,NA,89),c(NA,5,-1,-1,NA,1)),
    list(eras=rows(era_count=4L,era_days=26L,first_era_days=10L,covered_days=26L)))
  # T13: observed union 4/4, carry-in and a cap-binding stockpile example.
  add("drug.adherence_components","union_4_of_4",list(horizon=10L),
    function(c) {.library_obs_periods(c,c(0,8),c(1,9));.library_rx(c,0,9)},
    list(adherence=rows(observed_days=4L,recorded_covered_days=4L,effective_covered_days=4L,supply_days=10L)),edit=union)
  add("drug.adherence_components","carry_in",list(horizon=10L),
    function(c) .library_rx(c,-5,4,10),
    list(adherence=rows(observed_days=10L,recorded_covered_days=5L,effective_covered_days=5L,supply_days=5L)))
  add("drug.adherence_components","cap_binding",list(horizon=250L),
    function(c) .library_rx(c,c(0,1,152),c(199,10,161),c(200,10,10)),
    list(adherence=rows(observed_days=250L,recorded_covered_days=200L,effective_covered_days=200L,supply_days=220L)))
  add("drug.adherence_components","kept_uncovered",list(horizon=10L),
    function(c) .library_obs_periods(c,10,100),
    list(adherence=rows(observed_days=NULL,recorded_covered_days=NULL,effective_covered_days=NULL,supply_days=10L)),edit=uncovered)
  add("drug.adherence_components","unavailable",list(horizon=10L),
    function(c) DBI::dbExecute(c,"DROP TABLE drug_exposure"),
    list(adherence=rows(observed_days=10L,recorded_covered_days=NULL,effective_covered_days=NULL,supply_days=NULL)),
    edit=function(r) {
      walk<-function(x) {
        if(inherits(x,"omop_event")&&identical(x$table,"drug_exposure")) x$on_missing_table<-"unavailable"
        if(is.list(x)) for(n in seq_along(x)) x[n]<-list(walk(x[[n]]))
        x
      };walk(r)
    })

  # T14: first-eligible candidate, zero washout and overlapping prior supply.
  add("cohort.new_users","zero_washout",list(washout_days=0L,by="event",select="all"),
    mutate=function(c) .library_rx(c,c(0,100),c(9,109)),expected=list(new_users=rows(48,age=40L,sex="M",index_source="rx_990006_era")))
  add("cohort.new_users","later_eligible",list(washout_days=10L,by="event"),
    function(c) {.library_obs_periods(c,-5,500);.library_rx(c,c(0,100),c(1,101))},
    list(new_users=rows(age=40L,sex="M",index_source="rx_990006_era")))
  add("cohort.new_users","prior_overlap",list(washout_days=10L,by="ingredient",drug=c(990006L,990007L)),
    function(c) {.library_obs_periods(c,-15,500);.library_rx(c,c(-20,0),c(10,40),concepts=c(990007L,990006L))},
    list(new_users=rows(age=40L,sex="M",index_source="rx_990006_era")))
  # T15: own restart versus switch with long grace, same-day arm priority,
  # short initial courses and outcome/censor tie. ITT never uses course end.
  survival <- function(n=24L,event=0L,exit=100L,entry=1L,cause="outcome")
    rows(n,outcome_name=cause,event=event,entry_days_from_index=entry,exit_days_from_index=exit,follow_up_days=exit-entry)
  ac <- function(exit, n=24L, event=0L) lapply(survival(n,event,exit),function(x)c(x,list(arm="era_a",age=40L,sex="M")))
  for(case in c("dual_same_day","own_restart","opposite_switch","short_entry","prior_on_index","event_censor_tie")) local({case<-case
    add("cohort.active_comparator",case,list(horizon=100L,grace_days=100L),
      function(c) {
        DBI::dbExecute(c,"DELETE FROM condition_occurrence")
        if(case=="dual_same_day") .library_rx(c,c(0,0),c(9,9),concepts=c(990006L,990007L))
        if(case=="own_restart") .library_rx(c,c(0,50),c(9,59))
        if(case%in%c("opposite_switch","event_censor_tie")) .library_rx(c,c(0,50),c(9,59),concepts=c(990006L,990007L))
        if(case=="short_entry") {.library_rx(c,0,9);DBI::dbExecute(c,"UPDATE drug_exposure SET drug_exposure_end_date='2020-01-01',days_supply=1 WHERE person_id<=12")}
        if(case=="prior_on_index") .library_dx(c,0,people=1:12)
        if(case=="event_censor_tie") .library_dx(c,50)
      },list(itt_analysis=ac(if(case=="event_censor_tie")50L else 100L,if(case=="prior_on_index")12L else 24L,as.integer(case=="event_censor_tie")),
        on_treatment_analysis=ac(if(case%in%c("opposite_switch","event_censor_tie"))50L else 100L,
          if(case%in%c("short_entry","prior_on_index"))12L else 24L,as.integer(case=="event_censor_tie"))))
  })
  # T16: inclusive prior window, duplicate outcomes and later eligible index.
  for(day in c(-10L,-11L)) local({day<-day
    add("outcome.incident_event",paste0("prior_",abs(day)),list(lookback=10L),
      function(c) {.library_obs_periods(c,-10,100);.library_dx(c,c(day,0,0))},
      if(day== -10L) list() else list(incident_events=rows(age=40L,sex="M",prior_observed_days=10L)),
      error=if(day== -10L) "insufficient_support" else NULL)
  })
  add("outcome.incident_event","later_candidate",list(lookback=10L),function(c) {
    .library_obs_periods(c,0,100);.library_dx(c,c(1,20,20))
  },list(incident_events=rows(age=40L,sex="M",prior_observed_days=10L)))
  add("outcome.incident_event","empty",list(lookback=1L,outcome=omop_event("condition_occurrence",numeric(),name="outcome")),
      expected=list(),error="insufficient_support")
  # T17: exact entry/horizon, elapsed zero, death ties and actual coverage gaps.
  for(day in c(0L,1L,10L,11L)) local({day<-day
    add("survival.first_event",paste0("boundary_",day),list(horizon=10L,prior_days=0L),
      function(c) .library_dx(c,day),list(survival=survival(event=as.integer(day%in%c(1L,10L)),exit=if(day==1L)1L else 10L)))
  })
  add("survival.first_event","death_tie",list(horizon=10L,prior_days=0L),
    function(c) {.library_dx(c,5);DBI::dbExecute(c,"UPDATE death SET death_date='2020-01-06'")},
    list(survival=survival(event=1L,exit=5L)))
  add("survival.first_event","gap_censor",list(horizon=10L,prior_days=0L),
    function(c) {.library_obs_periods(c,c(-10,8),c(3,20));.library_dx(c,5)},
    list(survival=survival(exit=3L)),edit=union)
  # T18: priority and explicit death cause. Censored cause is NULL.
  for(case in c("tie","overlap","death_cause","censored")) local({case<-case
    add("survival.competing_risk",case,list(horizon=10L,
      outcome_b=if(case=="death_cause")omop_event("death",name="cause_b") else
        omop_event("condition_occurrence",if(case=="overlap")990005L else 990008L,name="cause_b")),
      function(c) {
        if(case=="censored") DBI::dbExecute(c,"DELETE FROM condition_occurrence") else .library_dx(c,c(5,5),c(990005L,990008L))
        if(case=="death_cause") DBI::dbExecute(c,"UPDATE death SET death_date='2020-01-04'")
      },list(competing_risk=survival(event=if(case=="censored")0L else 1L,
        exit=if(case=="censored")10L else if(case=="death_cause")3L else 5L,
        cause=if(case=="censored")NULL else if(case=="death_cause")"cause_b" else "cause_a")))
  })
  # T19: separation boundary and complete sentinel; risk intervals remain an
  # independently published component subject to final support.
  add("survival.recurrent_events","washout_boundary",list(horizon=10L,washout_days=2L),
    function(c) .library_dx(c,c(1,1,3,4,7)),
    list(recurrent.events=c(rows(event=1L,outcome_name="outcome",event_number=1L,outcome_event_number=1L,event_days_from_index=1L,entry_days_from_index=1L,exit_days_from_index=10L),
      rows(event=1L,outcome_name="outcome",event_number=2L,outcome_event_number=2L,event_days_from_index=7L,entry_days_from_index=1L,exit_days_from_index=10L)),
      recurrent.risk_sets=rows(entry_days_from_index=1L,exit_days_from_index=10L,follow_up_days=9L)))
  add("survival.recurrent_events","empty_sentinel",list(horizon=10L),
    function(c) DBI::dbExecute(c,"DELETE FROM condition_occurrence"),
    list(recurrent.events=rows(outcome_name=NULL,event=0L,event_number=1L,outcome_event_number=NULL,event_days_from_index=NULL,entry_days_from_index=1L,exit_days_from_index=10L),
      recurrent.risk_sets=rows(entry_days_from_index=1L,exit_days_from_index=10L,follow_up_days=9L)))
  add("survival.recurrent_events","zero_risk",list(horizon=1L),
    function(c) .library_dx(c,1),
    list(recurrent.events=rows(outcome_name="outcome",event=1L,event_number=1L,outcome_event_number=1L,event_days_from_index=1L,entry_days_from_index=1L,exit_days_from_index=1L),
      recurrent.risk_sets=rows(entry_days_from_index=1L,exit_days_from_index=1L,follow_up_days=0L)))
  # T20: vocabulary-defined two-concept schema, including whole-site empty and
  # unavailable sources; equality and one-day-outside carry boundaries.
  for(case in c("two_concepts","empty","unavailable","carry_equal","carry_expired","lookback_outside")) local({case<-case
    b<-list(horizon=30L,bin_days=10L,lookback=10L,expire=10L,
      measure=omop_event("measurement",c(990003L,990009L),name="measure",unit=omop_unit(8840),
        on_missing_table=if(case=="unavailable")"unavailable" else "error"))
    panelrows<-list()
    for(start in c(0L,10L,20L,30L)) {
      count<-if(case%in%c("empty","unavailable"))0L else if(start%in%c(0L,10L))1L else 0L
      val<-if(count==0)NULL else if(start==0)6 else 8
      carry<-if(case%in%c("empty","unavailable"))NULL else if(start==0)6 else if(start==10)8 else if(start==20 && case!="carry_expired")8 else NULL
      if(case%in%c("carry_equal","carry_expired","lookback_outside")) {
        count<-0L;val<-NULL;carry<-if(start==0L && case=="carry_equal")4 else NULL
      }
      second_count<-if(case=="unavailable")NULL else if(case=="two_concepts"&&start==0L)1L else 0L
      second_value<-if(case=="two_concepts"&&start==0L)6 else NULL
      second_carry<-if(case=="two_concepts"&&start%in%c(0L,10L))6 else NULL
      panelrows<-c(panelrows,rows(period_start_days=start,period_end_days=min(start+9L,30L),days=if(start==30L)1L else 10L,
        measure_numeric_test_count=if(case=="unavailable")NULL else count,measure_numeric_test_mean=val,
        measure_numeric_test_last=val,measure_numeric_test_carry=carry,
        measure_second_numeric_test_count=second_count,measure_second_numeric_test_mean=second_value,
        measure_second_numeric_test_last=second_value,measure_second_numeric_test_carry=second_carry))
    }
    if(case=="carry_expired") b$expire<-9L
    if(case=="lookback_outside") b$expire<-30L
    add("panel.measurements",case,b,function(c) {
      v<-DBI::dbGetQuery(c,"SELECT * FROM concept WHERE concept_id=990003");v$concept_id<-990009L;v$concept_name<-"Second numeric test";v$concept_code<-"990009"
      DBI::dbWriteTable(c,"concept",v,append=TRUE)
      if(case=="two_concepts") {
        m<-DBI::dbGetQuery(c,"SELECT * FROM measurement WHERE measurement_date='2020-01-01'")
        m$measurement_id<-m$measurement_id+100L;m$measurement_concept_id<-990009L
        DBI::dbWriteTable(c,"measurement",m,append=TRUE)
      }
      if(case=="empty") DBI::dbExecute(c,"DELETE FROM measurement")
      if(case=="unavailable") DBI::dbExecute(c,"DROP TABLE measurement")
      if(case%in%c("carry_equal","carry_expired","lookback_outside")) {
        DBI::dbExecute(c,"DELETE FROM measurement WHERE measurement_date>='2020-01-01'")
        if(case=="lookback_outside") DBI::dbExecute(c,"UPDATE measurement SET measurement_date='2019-12-21'")
      }
    },list(measurements=panelrows))
  })
  # T21: leap-day, Jan-1 split, partial years, gaps and repeated episodes.
  add("panel.incidence","year_gap",list(horizon=367L),
    function(c) {.library_obs_periods(c,c(-10,2),c(0,500));.library_dx(c,c(0,1,2,2,366))},
    list(incidence=c(rows(period_start_days=0L,period_end_days=0L,days=1L,calendar_year=2020L,n_outcome=1L),
      rows(period_start_days=2L,period_end_days=365L,days=364L,calendar_year=2020L,n_outcome=2L),
      rows(period_start_days=366L,period_end_days=367L,days=2L,calendar_year=2021L,n_outcome=1L))),edit=union)
  add("panel.incidence","repeated_empty",list(horizon=1L,select="all"),function(c) {
    DBI::dbExecute(c,"DELETE FROM condition_occurrence")
    v<-DBI::dbReadTable(c,"visit_occurrence");v$visit_occurrence_id<-v$visit_occurrence_id+24L
    DBI::dbWriteTable(c,"visit_occurrence",v,append=TRUE)
  },list(incidence=rows(48,period_start_days=0L,period_end_days=1L,days=2L,calendar_year=2020L,n_outcome=0L)))
  # T22: requested interior window with no interior boundary, exposure union,
  # gap events excluded, annual age increments at Jan 1.
  add("panel.sccs","interior_11_days",list(horizon=29L),
    function(c) {.library_obs_periods(c,0,99);.library_rx(c,c(0,10),c(99,90));.library_dx(c,20)},
    list(sccs=rows(period_start_days=19L,period_end_days=29L,days=11L,age=40L,age_band=8L,calendar_year=2020L,exposed_drug=11L,active_drug=1L,n_outcome=1L)),
    edit=function(r){r$outputs$sccs$options$window<-omop_window(19L,29L);r})
  add("panel.sccs","empty_year_gap",list(horizon=367L),
    function(c) {.library_obs_periods(c,c(-10,2),c(0,500));DBI::dbExecute(c,"DELETE FROM drug_exposure");.library_dx(c,c(1,366))},
    list(sccs=c(rows(period_start_days=0L,period_end_days=0L,days=1L,age=40L,age_band=8L,calendar_year=2020L,exposed_drug=0L,active_drug=0L,n_outcome=0L),
      rows(period_start_days=2L,period_end_days=365L,days=364L,age=40L,age_band=8L,calendar_year=2020L,exposed_drug=0L,active_drug=0L,n_outcome=0L),
      rows(period_start_days=366L,period_end_days=367L,days=2L,age=41L,age_band=8L,calendar_year=2021L,exposed_drug=0L,active_drug=0L,n_outcome=1L))),edit=union)
  # T23: shared physical keys in separate namespaces and repeated index.
  add("longitudinal.event_log","repeated_namespaces",list(select="all"),
    function(c) {DBI::dbExecute(c,"UPDATE measurement SET measurement_date='2019-12-22'")
      v<-DBI::dbReadTable(c,"visit_occurrence");v$visit_occurrence_id<-v$visit_occurrence_id+24L
      DBI::dbWriteTable(c,"visit_occurrence",v,append=TRUE)},
    list(event_log=c(rows(48,source="measure",concept_id="990003",start_days=-10L,end_days=-10L,value=4,unit_concept_id="8840",event_number=2L),
      rows(48,source="measure",concept_id="990003",start_days=-10L,end_days=-10L,value=6,unit_concept_id="8840",event_number=3L),
      rows(48,source="measure",concept_id="990003",start_days=-10L,end_days=-10L,value=8,unit_concept_id="8840",event_number=4L),
      rows(48,source="history",concept_id="990004",start_days=-10L,end_days=-10L,value=NULL,unit_concept_id=NULL,event_number=1L))))
  add("longitudinal.event_log","empty_source",list(source_b=omop_event("condition_occurrence",numeric(),name="history")),
    expected=list(event_log=c(rows(source="measure",concept_id="990003",start_days=-10L,end_days=-10L,value=4,unit_concept_id="8840",event_number=1L),
      rows(source="measure",concept_id="990003",start_days=0L,end_days=0L,value=6,unit_concept_id="8840",event_number=2L),
      rows(source="measure",concept_id="990003",start_days=10L,end_days=10L,value=8,unit_concept_id="8840",event_number=3L))))
  # T24: no controls remains a supported case component; death, gaps, same-time
  # outcome, missing strata, full outcome history and future cases tested by
  # deterministic, non-sampling roster restrictions.
  for(case in c("same_time","death","gap","missing_strata","earlier_ineligible","future_cases")) local({case<-case
    add("matching.nested_case_control",case,list(lookback=10L,controls_per_case=1L,seed=2L),
      function(c) {
        if(case=="same_time") .library_dx(c,10)
        if(case=="death") DBI::dbExecute(c,"UPDATE death SET death_date='2020-01-06' WHERE person_id>12")
        if(case=="gap") DBI::dbExecute(c,"UPDATE observation_period SET observation_period_end_date='2020-01-06' WHERE person_id>12")
        if(case=="missing_strata") DBI::dbExecute(c,"UPDATE person SET year_of_birth=NULL WHERE person_id>12")
        if(case=="earlier_ineligible") {.library_obs_periods(c,0,500);.library_dx(c,c(1,20),people=1:12);.library_dx(c,20,people=13:24,append=TRUE)}
        if(case=="future_cases") {.library_dx(c,10,people=1:12);.library_dx(c,30,people=13:24,append=TRUE)}
      },list(matched=if(case=="future_cases") c(rows(12,is_case=1L,sampling_weight=1,age=40L,sex="M",exposure_history=1L),rows(12,is_case=1L,sampling_weight=1,age=40L,sex="M",exposure_history=0L),rows(12,is_case=0L,sampling_weight=12,age=40L,sex="M",exposure_history=1L)) else
        rows(if(case=="same_time")24L else 12L,is_case=1L,sampling_weight=1,age=40L,sex="M",exposure_history=1L)))
  })
  # Additional matrix edges, each with its own literal control and bundle.
  add("comorbidity.charlson","hierarchy",list(select="all"),function(c) {
    .library_dx(c,c(-1,0),990004L)
    DBI::dbWriteTable(c,"concept_ancestor",data.frame(ancestor_concept_id=201820L,
      descendant_concept_id=990004L,min_levels_of_separation=1L,max_levels_of_separation=1L),append=TRUE)
  },list(score=rows(charlson=1L)))
  for(kind in c("charlson","chads2","chadsvasc","dcsi","hfrs")) local({kind<-kind
    add(paste0("comorbidity.",kind),"missing_required_input",list(select="all"),
      function(c) DBI::dbExecute(c,"DROP TABLE condition_occurrence"),list(),error="condition_occurrence|unavailable|validation_error")
  })
  for(kind in c("chads2","chadsvasc")) local({kind<-kind
    add(paste0("comorbidity.",kind),"age_threshold",list(select="all"),function(c) {
      DBI::dbExecute(c,"DELETE FROM condition_occurrence")
      DBI::dbExecute(c,"UPDATE person SET year_of_birth=1946 WHERE person_id<=12")
      DBI::dbExecute(c,"UPDATE person SET year_of_birth=1945 WHERE person_id>12")
    },list(score=c(rep(list(setNames(list(if(kind=="chads2")0L else 1L),kind)),12),
      rep(list(setNames(list(if(kind=="chads2")1L else 2L),kind)),12))))
    add(paste0("comorbidity.",kind),"missing_age_sex",list(select="all"),function(c) {
      DBI::dbExecute(c,"DELETE FROM condition_occurrence")
      DBI::dbExecute(c,"UPDATE person SET year_of_birth=NULL,gender_concept_id=NULL")
    },list(score=rep(list(setNames(list(0L),kind)),24)))
  })
  add("comorbidity.dcsi","vocabulary_mismatch",list(select="all"),
    function(c) DBI::dbExecute(c,"DELETE FROM concept_relationship"),list(),error="mapping|validation_error")
  add("comorbidity.condition_history","rare_private_feature",list(lookback=10L),
    function(c) .library_dx(c,-1,990004L,people=1L),
    list(history=c(rows(1,history_present=1L,history_days=1L),rows(23,history_present=0L,history_days=0L))))
  missing <- function(r) {
    walk<-function(x) {
      if(inherits(x,"omop_event")&&identical(x$table,"condition_occurrence")) x$on_missing_table<-"unavailable"
      if(is.list(x)) for(n in seq_along(x)) x[n]<-list(walk(x[[n]]))
      x
    };walk(r)
  }
  add("comorbidity.condition_history","unavailable",list(lookback=10L),
    function(c) DBI::dbExecute(c,"DROP TABLE condition_occurrence"),
    list(history=rows(history_present=NULL,history_days=NULL)),edit=missing)
  for(arm in c(990006L,990007L)) local({arm<-arm
    add("cohort.active_comparator",paste0("prior_start_",arm),list(horizon=100L),function(c) {
      .library_obs_periods(c,-365,500)
      .library_rx(c,c(-1,0),c(-1,9),concepts=c(arm,990006L))
      DBI::dbExecute(c,"DELETE FROM drug_exposure WHERE person_id>12 AND drug_exposure_start_date='2019-12-31'")
    },list(itt_analysis=ac(100L,12L),on_treatment_analysis=ac(39L,12L)))
  })
  add("longitudinal.event_log","empty_final",list(source_a=omop_event("measurement",numeric(),name="measure"),
    source_b=omop_event("condition_occurrence",numeric(),name="history")),expected=list(),error="insufficient_support")
  add("matching.nested_case_control","ratio_seed",list(controls_per_case=1L,seed=2L),
    expected=list(matched=c(rows(12,is_case=1L,sampling_weight=1,age=40L,sex="M",exposure_history=1L),
      rows(12,is_case=0L,sampling_weight=12,age=40L,sex="M",exposure_history=1L))))
  add("matching.nested_case_control","candidate_cap",list(seed=2L),expected=list(),error="candidate|validation_error",
    edit=function(r) {r$populations$matched$match$max_candidates<-1L;r})
  add("comorbidity.dcsi","highest_severity",list(select="all"),function(c) {
    # ICD9 250.50 maps to retinopathy tier 1; 362.02 to tier 2. Both recorded
    # SNOMED targets therefore contribute max(1,2)=2, never 1+2.
    src<-DBI::dbGetQuery(c,"SELECT * FROM concept WHERE concept_id=44826430")
    src$concept_id<-990020L;src$concept_code<-"362.02";src$concept_name<-"Synthetic severe retinopathy"
    DBI::dbWriteTable(c,"concept",src,append=TRUE)
    target<-DBI::dbGetQuery(c,"SELECT * FROM concept WHERE concept_id=4174977")
    target$concept_id<-990021L;target$concept_code<-"990021";target$concept_name<-"Synthetic mapped retinopathy"
    DBI::dbWriteTable(c,"concept",target,append=TRUE)
    cr<-DBI::dbGetQuery(c,"SELECT * FROM concept_relationship WHERE concept_id_1=44826430 AND relationship_id='Maps to'")
    cr$concept_id_1<-990020L;cr$concept_id_2<-990021L
    DBI::dbWriteTable(c,"concept_relationship",cr,append=TRUE)
    .library_dx(c,c(-1,0,0),c(4174977L,990021L,990021L))
  },list(score=rows(dcsi=2L)))
  add("socioeconomic.numeric_observation","last_row_missing",list(lookback=10L),function(c) {
    DBI::dbExecute(c,"UPDATE measurement SET measurement_date='2019-12-31',value_as_number=NULL WHERE measurement_date='2020-01-01'")
  },list(numeric_observation=rows(numeric_value=NULL,source_records=2L)))
  add("cohort.new_users","same_day_ingredients",list(washout_days=0L,select="all"),function(c) {
    .library_rx(c,0,9)
    v<-DBI::dbGetQuery(c,"SELECT * FROM concept WHERE concept_id=990006")
    v<-v[rep(1L,2),];v$concept_id<-c(990030L,990031L);v$concept_code<-c("990030","990031")
    v$vocabulary_id<-"RxNorm";v$concept_class_id<-"Ingredient";v$concept_name<-c("Ingredient A","Ingredient B")
    DBI::dbWriteTable(c,"concept",v,append=TRUE)
    DBI::dbWriteTable(c,"concept_ancestor",data.frame(ancestor_concept_id=c(990030L,990031L),
      descendant_concept_id=990006L,min_levels_of_separation=1L,max_levels_of_separation=1L),append=TRUE)
  },list(new_users=rows(48,age=40L,sex="M",index_source="rx_990006_era")))
  add("survival.recurrent_events","component_support_refusal",list(horizon=10L),
    function(c) .library_dx(c,5,people=1:4),list(),error="insufficient_support",
    edit=function(r) {r$outputs$recurrent$options$complete<-FALSE;r})
  add("survival.recurrent_events","rare_complete",list(horizon=10L),
    function(c) .library_dx(c,5,people=1:4),
    list(recurrent.events=c(rows(4,outcome_name="outcome",event=1L,event_number=1L,outcome_event_number=1L,event_days_from_index=5L,entry_days_from_index=1L,exit_days_from_index=10L),
      rows(20,outcome_name=NULL,event=0L,event_number=1L,outcome_event_number=NULL,event_days_from_index=NULL,entry_days_from_index=1L,exit_days_from_index=10L)),
      recurrent.risk_sets=rows(entry_days_from_index=1L,exit_days_from_index=10L,follow_up_days=9L)))
  add("matching.nested_case_control","distinct_person_support",list(seed=2L),function(c) {
    .library_dx(c,10,people=1L)
    .library_dx(c,20,people=2L,append=TRUE)
    .library_dx(c,30,people=3L,append=TRUE)
    DBI::dbExecute(c,"UPDATE person SET year_of_birth=NULL WHERE person_id>4")
  },list(),error="insufficient_support")
  cases
}

.library_variant_recipe <- function(spec) {
  b <- .library_test_bindings(spec$id, TRUE)
  b[names(spec$bindings)] <- spec$bindings
  spec$edit(do.call(recipe_library_instantiate, c(list(spec$id), b)))
}
.library_variant_control <- function(spec) {
  # Start with the separately authored §4 literal control, change only explicit
  # study constants, then apply the documented ordinary-recipe edit.
  expr <- parse(file=file.path(.library_test_dir(spec$id),"control.R"))
  aliases <- c(washout_days="washout",grace_days="grace",controls_per_case="m")
  bindings <- spec$bindings
  for(nm in names(bindings)) {
    target <- if(nm %in% names(aliases)) aliases[[nm]] else nm
    found <- FALSE
    for(i in seq_along(expr)) if(is.call(expr[[i]]) && identical(expr[[i]][[1]],as.name("<-")) &&
      identical(expr[[i]][[2]],as.name(target))) {
      expr[[i]][[3]] <- parse(text=if(is.object(bindings[[nm]])) dsOMOPClient:::.v2_codegen(bindings[[nm]]) else dsOMOPClient:::.codegen_value(bindings[[nm]]))[[1]]
      found <- TRUE
    }
    if(!found) stop("No independent literal constant for ",nm," in ",spec$id)
  }
  env <- new.env(parent=environment())
  spec$edit(eval(expr,env))
}
.library_variant_dir <- function(spec) file.path(dirname(.library_test_dir(spec$id)),spec$case)
.library_write_json <- function(x,path) writeLines(as.character(jsonlite::toJSON(x,auto_unbox=TRUE,
  pretty=TRUE,null="null",na="null",digits=NA)),path)
.library_variant_artifacts <- function(spec, record=FALSE) {
  r <- .library_variant_recipe(spec)
  control <- .library_variant_control(spec)
  p <- recipe_to_plan(r)
  expect_identical(.library_test_digest(recipe_to_plan(control)),.library_test_digest(p),info=paste(spec$id,spec$case,"literal control"))
  dir <- .library_variant_dir(spec)
  d <- .library_test_digest(p)
  expect_false(identical(d$sha256,.library_test_digest(recipe_to_plan(.library_test_recipe(spec$id)))$sha256))
  normal<-dsOMOPClient:::.planNormalizeV2(p)
  expect_identical(.library_test_canonical(dsOMOPClient:::.planNormalizeV2(normal)),.library_test_canonical(normal))
  staged<-recipe_library_instantiate(spec$id)
  b<-.library_test_bindings(spec$id,TRUE);b[names(spec$bindings)]<-spec$bindings
  for(nm in names(b)) staged<-do.call(recipe_bind,c(list(staged),b[nm]))
  expect_identical(.library_test_digest(recipe_to_plan(spec$edit(staged))),d)
  implicit<-.library_test_bindings(spec$id);implicit[names(spec$bindings)]<-spec$bindings
  expect_identical(.library_test_digest(recipe_to_plan(spec$edit(do.call(recipe_library_instantiate,c(list(spec$id),implicit))))),d)
  if(record) {
    dir.create(dir,recursive=TRUE,showWarnings=FALSE)
    b <- .library_test_bindings(spec$id,TRUE);b[names(spec$bindings)]<-spec$bindings
    .library_write_json(lapply(b,dsOMOPClient:::.recipe_strip_classes),file.path(dir,"bindings.json"))
    writeLines(recipe_to_code(r),file.path(dir,"recipe.R"))
    writeLines(c("# Independent literal control with explicit variant constants and documented edits.",recipe_to_code(control)),file.path(dir,"control.R"))
    .library_write_json(dsOMOPClient:::.v2_plain(p),file.path(dir,"plan.json"))
    .library_write_json(dsOMOPClient:::.planNormalizeV2(p),file.path(dir,"normalized.json"))
    writeLines(d$payload,file.path(dir,"semantic.json"));writeLines(d$sha256,file.path(dir,"semantic.sha256"))
    .library_write_json(list(oracle="Hand arithmetic in helper-recipe-library-variants.R; never derived from executor rows",
      excluded_protected_keys=c("row_id","cohort_row_id","person_id","set_id"),
      refusal=spec$error,components=spec$expected),file.path(dir,"expected.json"))
  }
  frozen_expected<-.library_test_json(file.path(dir,"expected.json"))
  expect_identical(.library_test_canonical(frozen_expected$components),.library_test_canonical(spec$expected))
  expect_identical(frozen_expected$refusal,spec$error)
  expect_identical(.library_test_canonical(dsOMOPClient:::.v2_plain(p)),.library_test_canonical(.library_test_json(file.path(dir,"plan.json"))))
  expect_identical(.library_test_canonical(dsOMOPClient:::.planNormalizeV2(p)),.library_test_canonical(.library_test_json(file.path(dir,"normalized.json"))))
  expect_identical(d$payload,.library_test_canonical(.library_test_json(file.path(dir,"semantic.json"))))
  expect_identical(d$sha256,readLines(file.path(dir,"semantic.sha256")))
  expect_identical(.library_test_canonical(dsOMOPClient:::.planNormalizeV2(p)),.library_test_canonical(dsOMOP:::.planNormalizeV2(p)))
  expect_true(dsOMOPClient:::.jsonSchemaValidate(dsOMOPClient:::.v2_plan_schema(),dsOMOPClient:::.v2_plain(p))$valid)
  for(rt in list(recipe_import_json(recipe_export_json(r)),recipe_import_yaml(recipe_export_yaml(r)),
                 eval(parse(file=file.path(dir,"recipe.R"))),eval(parse(file=file.path(dir,"control.R")))))
    expect_identical(.library_test_digest(recipe_to_plan(rt)),d)
  r
}
.library_variant_execute <- function(spec, backend="sqlite", record=FALSE, env=parent.frame()) {
  r <- .library_variant_artifacts(spec,record)
  ctx <- .library_cdm_setup(backend,env=env,mutate=spec$mutate)
  session <- dsOMOPClient:::.get_session(ctx$symbol);server<-ctx$servers[[1L]]
  handle <- server$getSessionData(server$getSessionIds()[[1L]],paste0(".dsomop_handle_",session$res_symbol))
  cat("\nVARIANT_START",spec$id,spec$case,"\n")
  dir <- .library_variant_dir(spec)
  fixture<-ctx$variant_fixture_hash
  vocabulary<-ctx$vocabulary_hash
  if(record) .library_write_json(list(id=spec$id,case=spec$case,template_version="1.0.0",
    definition_checksum=r$meta$recipe_library[[1]]$definition_checksum,
    recording_base_client_commit=Sys.getenv("LIBRARY_CLIENT_COMMIT"),recording_working_tree=TRUE,
    server_commit=Sys.getenv("LIBRARY_SERVER_COMMIT"),
    client_version=as.character(packageVersion("dsOMOPClient")),server_version=as.character(packageVersion("dsOMOP")),
    normalizer="A-11",schema="native-v2",fixture_sha256=fixture,vocabulary_sha256=vocabulary,
    backend=backend,nfilter=5L,stockpile_cap_days=90L,operations=dsOMOPClient:::.plan_v2_operations(recipe_to_plan(r)),
    note=spec$note,evidence="inst/recipe-library/evidence/library-fix1-tests.json"),file.path(dir,"manifest.json"))
  manifest<-.library_test_json(file.path(dir,"manifest.json"))
  expect_identical(manifest$definition_checksum,r$meta$recipe_library[[1]]$definition_checksum)
  if(backend==manifest$backend) {
    expect_identical(manifest$fixture_sha256,fixture)
    expect_identical(manifest$vocabulary_sha256,vocabulary)
  }
  before<-DSI::datashield.symbols(ctx$conns)
  out<-setNames(paste0("D_variant_",seq_along(r$publish)),r$publish)
  if(!is.null(spec$error)) {
    for(mode in c("memory","staged")) {
      expect_error(recipe_execute(r,out=out,symbol=ctx$symbol,conns=ctx$conns,output_mode=mode),spec$error)
      expect_identical(DSI::datashield.symbols(ctx$conns),before)
    }
    if(record) .library_write_json(list(publication="refused",components=list()),file.path(dir,"schema.json"))
    return(invisible(NULL))
  }
  memory<-NULL
  for(mode in c("memory","staged")) {
    out<-setNames(paste0("D_variant_",mode,"_",seq_along(r$publish)),r$publish)
    symbols_before<-unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE)
    result<-recipe_execute(r,out=out,symbol=ctx$symbol,conns=ctx$conns,output_mode=mode)
    # The public return is a symbol map or a verified per-site inventory for
    # native multi-component roots such as recurrent survival.
    if(is.character(result)) expect_identical(result,out) else {
      expect_named(result,"site1")
      expect_setequal(names(result$site1),r$publish)
    }
    added<-setdiff(unlist(DSI::datashield.symbols(ctx$conns),use.names=FALSE),symbols_before)
    expect_length(added,length(spec$expected))
    got<-list();schema<-list()
    for(component in names(spec$expected)) {
      root<-strsplit(component,".",fixed=TRUE)[[1]][1]
      suffix<-substring(component,nchar(root)+1L)
      sym<-paste0(out[[root]],if(suffix==".risk_sets")".riskSets" else suffix)
      # Native version uses .risk_sets on some adapters.
      available<-DSI::datashield.symbols(ctx$conns)[[1]]
      if(!sym%in%available) sym<-paste0(out[[root]],suffix)
      expect_true(sym%in%added)
      f<-.library_frames(ctx,sym)[[1]];got[[component]]<-f
      if(spec$id=="matching.nested_case_control") {
        membership<-.library_variant_match_keys(ctx,f,handle)
        if(exists(spec$case,envir=.library_variant_membership,inherits=FALSE))
          expect_identical(membership,get(spec$case,envir=.library_variant_membership)) else
          assign(spec$case,membership,envir=.library_variant_membership)
      }
      expect_true(all(grepl("^p2[0-9a-f]+\\.[0-9a-f]{64}$",f$person_id)))
      expect_gte(length(unique(f$person_id)),5L)
      expect_false(any(c("subject_id","index_key","index_visit_id")%in%names(f)))
      if(mode=="memory") {
        columns<-attr(f,"omop_column_contract")
        if(spec$id=="panel.measurements") {
          # The presentation layer labels dynamic columns; the contract capsule
          # retains wire identifiers. Pin that exact vocabulary mapping as well.
          display<-gsub("measure_990003_","measure_numeric_test_",names(columns),fixed=TRUE)
          display<-gsub("measure_990009_","measure_second_numeric_test_",display,fixed=TRUE)
          expect_identical(display,names(f))
          columns<-Map(function(x,native){x$native_name<-native;x},columns,names(columns))
          names(columns)<-display
        }
        expect_named(columns,names(f))
        schema[[component]]<-list(columns=lapply(columns,function(x)list(type=x$type,nullable=x$nullable,role=x$role,native_name=x$native_name)),
          protection=list(person_id="existing pseudonymisation",row_id="native episode key",cohort_row_id="native episode key"))
      }
      expected<-.library_test_json(file.path(dir,"expected.json"))$components[[component]]
      cols<-names(expected[[1]])
      want<-as.data.frame(setNames(lapply(cols,function(col)vapply(expected,function(row)if(is.null(row[[col]]))NA_character_ else as.character(row[[col]]),"")),cols))
      actual<-.library_oracle_frame(f)[cols]
      sortrows<-function(x) {
        x<-x[do.call(order,c(unname(x),list(na.last=TRUE,method="radix"))),,drop=FALSE]
        rownames(x)<-NULL;x
      }
      if(!isTRUE(all.equal(sortrows(actual),sortrows(want)))) {cat("\nORACLE_DIFFERENCE",spec$id,spec$case,component,"\n");print(unique(actual));print(unique(want))}
      expect_equal(sortrows(actual),sortrows(want),info=paste(spec$id,spec$case,mode,component))
      if(mode=="staged") {
        expect_identical(names(f),names(memory[[component]]))
        for(col in names(f)) expect_identical(as.character(f[[col]]),as.character(memory[[component]][[col]]))
      }
    }
    if(mode=="memory") {
      memory<-got
      if(record) .library_write_json(schema,file.path(dir,"schema.json"))
      frozen_schema<-.library_test_json(file.path(dir,"schema.json"))
      expect_identical(names(schema),names(frozen_schema))
      for(component in names(schema)) expect_identical(names(schema[[component]]$columns),names(frozen_schema[[component]]$columns))
      expect_identical(.library_test_canonical(schema),.library_test_canonical(frozen_schema))
    }
  }
  cat("\nLIBRARY_VARIANT",backend,spec$id,spec$case,"fixture",fixture,"vocabulary",vocabulary,"\n")
}
