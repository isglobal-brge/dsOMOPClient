#' Composable calendar presentation and anchored ages
#'
#' Date declarations execute privately on each server. Calendar periods are
#' ordered factors by default; number supplies trend coordinates, and start/end
#' supplies inclusive period bounds. Exact ages use completed anniversaries
#' (February 29 uses March 1 in non-leap years). Missing partial dates give NA
#' unless a policy is declared. All patient-linked results retain class 2.
#' Variable fields override recipe fields, then design defaults.
#' @param precision Calendar period, day, relative_days or custom.
#' @param representation auto, factor, number, start or end.
#' @param numeric index or decimal_year period coordinate.
#' @param origin Fixed ISO origin for numeric period indices (default 1970-01-01).
#' @param anchor Relative-days anchor.
#' @param pattern Closed calendar pattern; ISO weeks require \%G-W\%V.
#' @param labels Named public calendar label map; labels must remain unique.
#' @param level_range Two inclusive public ISO dates declaring factor periods.
#' @param fiscal_start Fiscal starting month (1..12).
#' @param week_start,week_min_days Explicit epidemiological week profile (1..7).
#' @param hemisphere north or south, for meteorological seasons.
#' @param partial none, start, mid or end, for incomplete dates.
#' @param invalid na (invalid row values remain missing).
#' @param future allow or na; na requires a fixed as_of.
#' @param as_of Fixed ISO future-policy cutoff.
#' @param reference,from,to Date sources: anchor, ISO date, index, index_end,
#'   observation_start/end, birth, death, first_visit or last_visit.
#' @param date_spec Optional omop_date_spec declaration.
#' @param broadcast Repeat a roster value in long event rows.
#' @param year,month,day Integer literals or registered integer source columns.
#' @param event Optional event source for components (default person).
#' @param which,n,scope,search Existing anchor row-selection semantics.
#' @param parts Unique date parts; named entries supply exact final column names.
#' @param unit days, weeks, months, years or decimal_years; age also annual_years.
#' @param absolute Absolute elapsed time (default signed to minus from).
#' @param rounding none, floor, ceiling, trunc or nearest (ties to even).
#' @param bands Return ordered age bands (years/annual_years only).
#' @param age_breaks Integer lower bounds beginning at zero; default 0,5,...,100.
#' @details
#' Calendar precisions are day, year, semester, quarter, month, iso_week,
#' cdc_week, epi_week, fiscal_year, fiscal_semester, fiscal_quarter,
#' fiscal_month, relative_days and custom. ISO weeks start Monday with four
#' new-year days in week 1; CDC weeks start Sunday with four. epi_week requires
#' both week_start and week_min_days. Fiscal year labels name the starting year.
#' Numeric periods count from the period containing origin (zero); year and
#' fiscal_year instead return the year. decimal_year is a period coordinate,
#' not an elapsed-year fraction. start/end return inclusive period bounds.
#'
#' Parts are year, semester, quarter, month, month_name, month_abbr, day,
#' weekday, weekday_name, weekday_abbr, iso_week, iso_year, day_of_year,
#' epi_week, epi_year, fiscal_year, fiscal_quarter, season, month_sin,
#' month_cos, weekday_sin, weekday_cos, season_sin and season_cos.
#' Month/weekday/season categories have full ordered public domains. Names
#' use fixed English. Monday is weekday 1. Northern seasons are Winter,
#' Spring, Summer, Autumn (December-February first); southern names are
#' Summer, Autumn, Winter, Spring. Cyclic values use 2*pi*(k-1)/P with
#' P=12, 7 or 4. Ordered factors use ordered-factor contrasts; request number
#' for a trend coordinate. Shared unused periods are zero-count levels.
#'
#' Custom patterns accept only \%Y, \%m, \%d, \%j, \%G, \%V, \%u, \%b, \%B, \%a,
#' \%A, \%\% and ASCII letters, digits, space or -_/.,:(). ISO \%G and \%V must
#' appear together without \%Y. No locale, code or time-of-day directives are
#' evaluated. Public labels must be unique and cannot merge date categories.
#' level_range declares additional levels and never filters observed dates.
#'
#' Gregorian dates cover 0001-01-01 through 9999-12-31. Partial dates with
#' partial=none retain only invariant coarsened periods/parts; exact age is NA.
#' start/mid/end impute the first/middle/last possible day and produce estimated
#' ages. Invalid components cannot be repaired. annual_years explicitly returns
#' calendar-year age, including partial births. Negative age is NA. Elapsed
#' months clamp the original day; elapsed years use completed anniversaries;
#' decimal years divide the remainder by the next anniversary interval.
#' A fixed as_of is required for future=na. Bind a fixed date instead of today
#' to make the plan reproducible. Missing anchors retain all roster rows.
#'
#' An explicit empty spec selects native Date presentation for first_date and
#' last_date reducers; an omitted spec preserves legacy offsets. Native helpers
#' need language 2 at every selected site. Explicit overlapping date_handling
#' refuses; use date_spec for that policy. No owner date permission is required
#' on assignment. Existing publication, metadata support and aggregate gates
#' still apply. Only symbols and disclosure-checked aggregates reach analysts.
#' @return A date specification, source, variable or variable bundle.
#' @examples
#' omop_age_at("death")
#' omop_date("first_visit", omop_date_spec(precision = "quarter"))
#' omop_date_parts("first_visit", c(visit_year = "year", visit_month = "month"))
#' @export
omop_date_spec <- function(precision=NULL, representation=NULL, numeric=NULL,
  origin=NULL, anchor=NULL, pattern=NULL, labels=NULL, level_range=NULL,
  fiscal_start=NULL, week_start=NULL, week_min_days=NULL, hemisphere=NULL,
  partial=NULL, invalid=NULL, future=NULL, as_of=NULL) {
  s<-as.list(environment());s<-s[!vapply(s,is.null,logical(1))]
  if(!is.null(anchor)) s$anchor<-.dateSourceArg(anchor)
  .dateSpecCheck(s)
  structure(s,class=c("omop_date_spec","list"))
}
.dateSourceArg <- function(x) {
  if(inherits(x,"omop_date_source") || inherits(x,"omop_anchor")) return(x)
  if(is.character(x)&&length(x)==1L&&!is.na(x)) {
    if(x=="birth") return(omop_date_components("year_of_birth","month_of_birth","day_of_birth"))
    if(x %in% c("death","first_visit","last_visit")) return(omop_anchor(omop_event(if(x=="death")"death" else "visit_occurrence"),which=if(x=="last_visit")"last" else "first"))
    if(x %in% .V2_BUILTIN_ANCHORS || .dateISO(x)) return(x)
  }
  .dateReject("Use a typed date source, anchor or valid fixed ISO date.","date_source_invalid")
}
.dateSpecArg <- function(s) {
  if(!is.null(s)&&!inherits(s,"omop_date_spec")) .dateReject("Construct date_spec with omop_date_spec().")
  .dateSpecCheck(s);s
}
.dateVariable <- function(d,s,broadcast) {
  .v2_variable("derived",derived=d,date_spec=.dateSpecArg(s),broadcast=.v2_flag(broadcast,"date helper broadcast"))
}
#' @rdname omop_date_spec
#' @export
omop_date <- function(reference,date_spec=NULL,broadcast=FALSE) .dateVariable(list(kind="calendar_date",reference=.dateSourceArg(reference)),date_spec,broadcast)
#' @rdname omop_date_spec
#' @export
omop_date_components <- function(year,month=NULL,day=NULL,event=NULL,which="first",n=1L,scope="person",search=NULL) {
  operand<-function(x) {
    if(is.null(x)) return(NULL)
    if(is.numeric(x)&&length(x)==1L&&!is.na(x)&&is.finite(x)&&x==trunc(x)) return(list(kind="integer",value=if(abs(x)<=.Machine$integer.max)as.integer(x) else x))
    if(is.character(x)&&length(x)==1L&&!is.na(x)&&grepl("^[A-Za-z][A-Za-z0-9_]*$",x)) return(list(kind="column",name=x))
    .dateReject("Components need an integer literal or source-column name.","date_source_invalid")
  }
  if(is.null(year)) .dateReject("year is required.","date_source_invalid")
  which<-.v2_choice(which,c("first","last","nth"),"date components which")
  n<-.v2_int(n,"date components n",min=1L)
  if(which!="nth"&&n!=1L) .dateReject("Nondefault n requires nth.","date_source_invalid")
  scope<-.v2_choice(scope,c("person","episode"),"date components scope")
  if(!is.null(event)) event<-.v2_source(event,"date components event")
  if(!is.null(search)&&!inherits(search,"omop_window")) .dateReject("search needs omop_window().","date_source_invalid")
  structure(list(kind="date_components",year=operand(year),month=operand(month),day=operand(day),event=event,which=which,n=n,scope=scope,search=search),class=c("omop_date_source","list"))
}
#' @rdname omop_date_spec
#' @export
omop_date_parts <- function(reference,parts=c("year","month","day"),date_spec=NULL,broadcast=FALSE) {
  if(!is.character(parts)||!length(parts)||anyNA(parts)||anyDuplicated(unname(parts))||any(!parts %in% .DATE_PARTS)) .dateReject("parts must be unique supported date parts.")
  structure(list(reference=.dateSourceArg(reference),parts=parts,date_spec=.dateSpecArg(date_spec),broadcast=.v2_flag(broadcast,"date parts broadcast")),class=c("omop_variable_bundle","list"))
}
#' @rdname omop_date_spec
#' @export
omop_time_between <- function(from,to,unit="days",absolute=FALSE,rounding="none",date_spec=NULL,broadcast=FALSE) {
  unit<-.v2_choice(unit,c("days","weeks","months","years","decimal_years"),"time_between unit")
  rounding<-.v2_choice(rounding,c("none","floor","ceiling","trunc","nearest"),"time_between rounding")
  .dateVariable(list(kind="time_between",from=.dateSourceArg(from),to=.dateSourceArg(to),unit=unit,absolute=.v2_flag(absolute,"time_between absolute"),rounding=rounding),date_spec,broadcast)
}
#' @rdname omop_date_spec
#' @export
omop_age_at <- function(reference="index",unit="years",bands=FALSE,age_breaks=NULL,date_spec=NULL,broadcast=FALSE) {
  unit<-.v2_choice(unit,c("years","decimal_years","months","weeks","days","annual_years"),"age_at unit")
  bands<-.v2_flag(bands,"age_at bands")
  if(bands&&!unit %in% c("years","annual_years")||!bands&&!is.null(age_breaks)) .dateReject("Age bands need years/annual_years and bands=TRUE.")
  if(bands) age_breaks<-.v2_age_breaks(age_breaks %||% seq(0L,100L,5L),"age_at age_breaks")
  .dateVariable(list(kind="age_at",reference=.dateSourceArg(reference),unit=unit,bands=bands,age_breaks=age_breaks),date_spec,broadcast)
}
.dateExpandBundles <- function(variables) {
  if(is.null(variables)) return(NULL)
  items<-.recipe_arg_list(variables);out<-list()
  for(i in seq_along(items)) {
    x<-items[[i]];name<-names(items)[i]
    if(!inherits(x,"omop_variable_bundle")) {out<-c(out,items[i]);next}
    for(j in seq_along(x$parts)) {
      nm<-names(x$parts)[j]
      if(is.null(nm)||is.na(nm)||!nzchar(nm)) {
        if(is.null(name)||is.na(name)||!nzchar(name)) .dateReject("Unnamed bundles require named parts.","date_parts_names")
        nm<-paste0(name,"_",unname(x$parts[j]))
      }
      if(!.conceptValidNames(nm)||nm %in% c("person_id","cohort_row_id","row_id","cohort_definition_id","cohort_start_date","cohort_end_date")) .dateReject("Part names must be valid nonreserved R names.","date_parts_names")
      out<-c(out,setNames(list(.dateVariable(list(kind="date_part",reference=x$reference,part=unname(x$parts[j])),x$date_spec,x$broadcast)),nm))
    }
  }
  nms<-names(out);if(anyDuplicated(nms[nzchar(nms)])) .dateReject("Duplicate expanded variable name.","date_parts_names")
  out
}
.dateWireSource <- function(ctx,x,context) {
  if(!inherits(x,"omop_date_source")) return(.v2w_anchor(ctx,x,context))
  out<-unclass(x);out<-out[!vapply(out,is.null,logical(1))]
  if(!is.null(x$event)) out$event<-.v2c_source(ctx,x$event,context)
  if(!is.null(x$search)) out$search<-.v2w_window(ctx,x$search,context)
  out
}
.dateWireSpec <- function(ctx,s,context) {
  if(is.null(s)) return(NULL)
  s<-unclass(s)
  if(!is.null(s$anchor)) s$anchor<-.dateWireSource(ctx,s$anchor,context)
  if(!is.null(s$labels)) s$labels<-as.list(s$labels)
  if(!is.null(s$level_range)) s$level_range<-.v2_array(s$level_range)
  if(!length(s)) s<-.pnv2EmptyObject()
  s
}

.dateExplain <- function(plan) {
  out<-character()
  if(!is.null(plan$options$date_spec)) {
    s<-.dateEffective(NULL,plan$options$date_spec)
    out<-paste0("Registered output date roles: ",s$precision," / ",s$representation,
      "; presentation follows selection, joins, censoring and interval arithmetic; class 2; partial=",s$partial,
      "; invalid=",s$invalid,"; future=",s$future,"; date.presentation required.")
  }
  for(n in names(plan$variables)) {
    v<-plan$variables[[n]];d<-v$derived;s<-v$date_spec
    if(is.null(s)&&!(d$kind %||% "") %in% .DATE_KINDS) next
    kind<-d$kind %||% "calendar_date"
    s<-.dateEffective(s,plan$options$date_spec,kind,d$part)
    fields<-vapply(names(s),function(k) paste0(k,"=",paste(unlist(s[[k]]),collapse=",")),"")
    inactive<-setdiff(names(plan$options$date_spec),names(s))
    out<-c(out,paste0("Date variable '",n,"': ",kind,if(!is.null(d$part))paste0(" part=",d$part),"; ",paste(fields,collapse="; "),
      "; precedence variable > recipe > design; class 2 quasi_identifier; Gregorian 0001..9999 (outside domain -> typed NA).",
      if(length(inactive))paste0(" Inactive recipe fields: ",paste(inactive,collapse=", "))),
      paste0("Sources: ",paste(unlist(d[c("reference","from","to")]),collapse=" / "),"; selected in existing total event order; missing anchors retain the roster."),
      paste0("Partial policy ",s$partial,"; ",if(s$partial=="none")"no imputation" else "values may be imputed; ages are estimated","; invalid values -> NA; future policy ",s$future,"."))
    if(kind %in% c("time_between","age_at")) out<-c(out,paste0("Unit ",d$unit,"; ",if(d$unit=="annual_years")"annual approximation; within-year ordering unresolved for incomplete birth" else "completed calendar anniversaries; Feb 29 -> March 1; months clamp from original day; decimal years divide by anniversary interval", "; absolute=",isTRUE(d$absolute),"; rounding=",d$rounding %||% "none","; date.elapsed required."))
    else out<-c(out,"date.presentation required; day -> R Date; coarsened periods -> ordered factor (polynomial contrasts), with factor/number/start/end alternatives. Numeric period indices use the period containing origin as zero; year numbers remain calendar/fiscal years. Cyclic terms use 2*pi*(k-1)/P, P=12/7/4.")
    if(!is.null(s$origin)) out<-c(out,paste0("Numeric origin ",s$origin," lies in period starting ",as.character(.datePeriod(as.Date(s$origin),s)$start)," (index zero)."))
    if(isTRUE(d$bands)) out<-c(out,paste0("Public age-band lower bounds: ",paste(unlist(d$age_breaks %||% seq(0L,100L,5L)),collapse=", "),"; left closed, right open, last band to infinity; all ordered levels retained."))
  }
  out
}
