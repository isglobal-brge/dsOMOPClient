#' Prespecified repeated-measure columns
#' @param offset Signed integer offset from the original anchor.
#' @param unit days, weeks, months or years.
#' @param tolerance Inclusive lower/upper day offsets around each target.
#' @param anchor Typed anchor or fixed date; NULL inherits the grid anchor.
#' @param select closest, first or last.
#' @param points Named ordered list of time points.
#' @param reuse Allow one event to serve overlapping time points.
#' @param numbers Unique positive visit ordinals, in output order.
#' @param source Registered visit_occurrence event.
#' @param window Optional visit-start window.
#' @param event One typed event or registered event name.
#' @param values Named list of same-event numeric/category row projections.
#' @param timepoints A time-point grid.
#' @param visits A visit grid; exactly one grid is required.
#' @param missing keep, skip_all or skip_any for canonical operands.
#' @param category_spec Bundle category defaults.
#' @param broadcast Explicit scalar broadcast to long event rows.
#' @return A time/visit declaration or named bundle of scalar variables.
#' @export
omop_timepoint <- function(offset=0L,unit="days",tolerance=c(0L,0L),anchor=NULL,select="closest") {
  .categoryChoice(unit,c("days","weeks","months","years"));.categoryChoice(select,c("closest","first","last"))
  if(!is.numeric(offset)||length(offset)!=1L||!is.finite(offset)||offset!=trunc(offset)||abs(offset)>.Machine$integer.max) .categoryReject("Time-point offset must be a signed integer.","repeated_spec_invalid")
  if(!is.numeric(tolerance)||length(tolerance)!=2L||any(!is.finite(tolerance))||any(tolerance!=trunc(tolerance))||tolerance[1]>0||tolerance[2]<0) .categoryReject("Tolerance is an inclusive integer-day interval containing zero.","repeated_spec_invalid")
  .categoryTyped("timepoint",list(offset=offset,unit=unit,tolerance=tolerance,anchor=if(!is.null(anchor)).dateSourceArg(anchor),select=select))
}
#' @rdname omop_timepoint
#' @export
omop_timepoints <- function(points,anchor="index",reuse=TRUE) {
  .numberNames(names(points))
  if(!is.list(points)||!length(points)||any(!vapply(points,inherits,logical(1),"omop_timepoint"))) .categoryReject("points must be named timepoint declarations.","repeated_spec_invalid")
  .categoryTyped("timepoints",list(points=points,anchor=.dateSourceArg(anchor),reuse=.categoryBool(reuse)))
}
#' @rdname omop_timepoint
#' @export
omop_visits <- function(numbers,source,window=NULL,select="last") {
  if(!is.numeric(numbers)||!length(numbers)||anyNA(numbers)||any(!is.finite(numbers))||any(numbers<1|numbers!=trunc(numbers))||anyDuplicated(numbers)) .categoryReject("Visits require unique positive integer ordinals.","repeated_spec_invalid")
  .categoryChoice(select,c("first","last","closest"))
  if(!is.null(window)&&!inherits(window,"omop_window")) .categoryReject("Use a typed visit window.","repeated_spec_invalid")
  .categoryTyped("visits",list(numbers=numbers,source=.v2_source(source,"visit source"),window=window,select=select))
}
#' @rdname omop_timepoint
#' @export
omop_repeated <- function(event,values,timepoints=NULL,visits=NULL,missing="keep",category_spec=NULL,broadcast=FALSE) {
  event<-.v2_source(event,"repeated event");.numberNames(names(values))
  if(!is.list(values)||!length(values)) .categoryReject("Repeated values need named typed row operands.","repeated_spec_invalid")
  if(is.null(timepoints)==is.null(visits)) .categoryReject("Choose exactly timepoints or visits.","repeated_spec_invalid")
  if(!is.null(timepoints)&&!inherits(timepoints,"omop_timepoints")||!is.null(visits)&&!inherits(visits,"omop_visits")) .categoryReject("Use a typed timepoints/visits declaration.","repeated_spec_invalid")
  missing<-.categoryChoice(missing,c("keep","skip_all","skip_any"));broadcast<-.categoryBool(broadcast)
  flat<-list()
  for(n in names(values)) {
    if(inherits(values[[n]],"omop_category_bundle"))for(k in names(values[[n]]))flat<-c(flat,setNames(list(values[[n]][[k]]),paste(n,k,sep="_")))
    else flat<-c(flat,setNames(list(values[[n]]),n))
  }
  if(anyDuplicated(names(flat))) .categoryReject("Expanded repeated operand names collide.","repeated_spec_invalid")
  if(any(!vapply(flat,inherits,logical(1),"omop_variable"))) .categoryReject("Only typed same-event row projections are repeated.","repeated_grain_incompatible")
  points<-if(!is.null(timepoints))names(timepoints$points) else paste0("visit_",visits$numbers)
  out<-list()
  for(point in points)for(value in names(flat)) {
    input<-flat[[value]]
    spec<-.categoryEffective(input$category_spec,.categoryArg(category_spec))
    output_name<-paste(value,point,sep="_")
    if(output_name %in% names(out)) .categoryReject("Repeated names collide.","repeated_spec_invalid")
    out[[output_name]]<-.v2_variable("derived",derived=list(kind="repeated",source=event,
      values=flat,timepoints=timepoints,visits=visits,missing=missing,point=point,value=value),
      category_spec=if(length(spec))structure(spec,class=c("omop_category_spec","list")),number_spec=input$number_spec,broadcast=broadcast)
  }
  if(anyDuplicated(names(out))) .categoryReject("Repeated names collide.","repeated_spec_invalid")
  structure(out,class=c("omop_category_bundle","list"))
}

.repeatedScalar <- function(declaration,broadcast=FALSE) {
  d<-declaration
  bundle<-omop_repeated(d$source,d$values,d$timepoints,d$visits,d$missing,broadcast=broadcast)
  bundle[[paste(d$value,d$point,sep="_")]]
}
