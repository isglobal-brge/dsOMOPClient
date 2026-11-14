#' Private numeric presentation and expressions
#'
#' Declare numeric computations without evaluating analyst code. Unit conversion
#' precedes source predicates/reduction; scalar transforms, exact binary64
#' rounding and public ordered categories follow it. Missing/invalid inputs stay
#' missing; unavailable sources stay unavailable even for presence/counts.
#' Native language 2 and numeric capabilities are required at every site.
#' @param unit A source map, fixed unit conversion, source, or expression coordinate.
#' @param transform One transform or an ordered nonempty transform list.
#' @param rounding A numeric rounding declaration.
#' @param representation number, bins, category, flag, present or missing.
#' @param breaks Strictly increasing public boundaries; optional infinite tails.
#' @param grid count_1_2_5, proportion_quarters or signed_decades.
#' @param closed left or right interval closure.
#' @param labels Complete unique public labels, in numeric order.
#' @param range reference, thresholds or bins.
#' @param thresholds Public thresholds in the incoming numeric coordinate.
#' @param op lt, le, gt, ge, eq, ne, inside or outside.
#' @param category_map Complete nondecreasing bin-to-category mapping.
#' @param levels Complete ordered public category domain.
#' @param pipeline Ordered typed steps, atomic for inheritance.
#' @param ... Typed pipeline steps.
#' @param exprs Closed mutate-v1 request, never executable R/SQL.
#' @param result Assignment selected as result; defaults to the last assignment.
#' @param variable A numeric variable declaration.
#' @param stage canonical or presented numeric input.
#' @param event Typed event or registered source name.
#' @param value Registered numeric value role/column.
#' @param number_spec Numeric presentation declaration.
#' @param column Stable pre-rename logical numeric output column key.
#' @param component Declared patient row-frame component name.
#' @param inputs Named numeric declarations or explicit input bindings.
#' @param reducer Optional numeric reducer, applied after same-event expressions.
#' @param broadcast Explicit scalar broadcast to event rows (default false).
#' @param from,to Canonical fixed-unit tokens.
#' @param factor,offset Public affine parameters.
#' @param method Transform, rounding or longitudinal method.
#' @param center,scale Public standardisation constants; scale must be positive.
#' @param lower,upper Finite clipping bounds.
#' @param digits Decimal places (-308..308) or significant digits (1..17).
#' @param window Explicit eligible-event window; NULL is unbounded.
#' @param kind nonmissing or missing numeric row count.
#' @param missing skip or keep endpoint missingness; slopes/thresholds require skip.
#' @param reference First observation, anchor, index or fixed ISO origin.
#' @param time_unit days or weeks for slopes/attainment times.
#' @param threshold Public observed-attainment threshold.
#' @return A typed numeric specification, step, input or variable declaration.
#' @details
#' Decimal rounding uses ties to even on the actual binary64 input. Public bins
#' are ordered factors with all declared levels, including unused levels. Clinical
#' reference categories require a selected observation with paired bounds;
#' use thresholds after aggregate reducers or transforms. Numeric expressions use
#' the existing closed mutate-v1 grammar. Inputs bind privately; canonical inputs
#' bypass presentation and never inherit outer recipe display defaults. Explicit
#' presented inputs must remain numeric. Rows and output-column copies preserve
#' identity, availability and privacy lineage. Counts and first-to-last summaries
#' create no population. Threshold times mean first observed attainment, without
#' interpolation. No observed range, unit inventory or failure count is released.
#' @examples
#' lab <- omop_event("measurement", 3000963L, unit = omop_unit(8713L))
#' mean_lab <- omop_variable(event = lab, reducer = omop_reduce("mean"))
#' rounded <- omop_number(mean_lab,
#'   omop_number_spec(rounding = omop_number_round("decimals", 1)))
#' band <- omop_number_bins(mean_lab, breaks = c(-Inf, 12, 15, Inf))
#' # A declared arithmetic coordinate with a closed expression.
#' doubled <- list(version = 1L, assign = list(list(name = "twice",
#'   expr = list(op = "*", args = list(list(col = "x"), list(lit = 2))))))
#' transformed <- omop_number_derive(list(x = mean_lab), doubled,
#'   unit = "coordinate:doubled_lab")
#' study <- omop_recipe(variables = list(raw = omop_number(mean_lab),
#'   rounded = rounded, band = band, doubled = transformed))
#' recipe_to_plan(study, version = 2)
#' @export
omop_number_spec <- function(unit=NULL,transform=NULL,rounding=NULL,representation=NULL,
  breaks=NULL,grid=NULL,closed=NULL,labels=NULL,range=NULL,thresholds=NULL,op=NULL,
  category_map=NULL,levels=NULL,pipeline=NULL) {
  s<-as.list(environment());s<-s[!vapply(s,is.null,logical(1))]
  if(!is.null(unit)) s$unit<-.numberUnitArg(unit)
  if(!is.null(pipeline)) s$pipeline<-lapply(pipeline,.numberStepArg)
   .numberCheckPartial(s)
  structure(s,class=c("omop_number_spec","list"))
}
.numberSpecArg <- function(s) {
  if(!is.null(s)&&!inherits(s,"omop_number_spec")) .numberReject("Use omop_number_spec() or omop_number_pipeline().")
  if(!is.null(s)) .numberCheckPartial(s)
  s
}
.numberUnitArg <- function(u) {
  if(inherits(u,"omop_unit")) return(list(kind="event_unit",spec=u))
  if(inherits(u,"omop_number_unit")) return(c(list(kind="fixed_unit"),unclass(u)))
  if(.numberHasParam(u))u else .numberUnitCheck(u)
}
.numberStepArg <- function(s) {
  if(inherits(s,c("omop_unit","omop_number_unit"))) return(.numberUnitArg(s))
  if(inherits(s,"omop_number_transform")) return(c(list(kind="transform"),unclass(s)))
  if(inherits(s,"omop_number_round")) return(c(list(kind="round"),unclass(s)))
  if(inherits(s,"omop_number_expr")) return(c(list(kind="expression"),unclass(s)))
  if(inherits(s,"omop_number_spec")) {
    s<-unclass(s);k<-s$representation;s$representation<-NULL
    if(is.null(k)||k=="number"||length(intersect(names(s),c("unit","transform","rounding","pipeline")))) .numberReject("Pipeline classification steps must have one representation and its fields only.")
    return(c(list(kind=k),s))
  }
  # Already tagged wire steps occur in validated recipe JSON round trips.
  if(is.list(s)&&!is.null(s$kind)) return(s)
  .numberReject("Pipeline steps must be typed numeric declarations.")
}
#' @rdname omop_number_spec
#' @export
omop_number_pipeline <- function(...) {
  steps<-list(...)
  if(length(steps)==1L&&is.list(steps[[1L]])&&!inherits(steps[[1L]],c("omop_number_spec","omop_number_transform","omop_number_round","omop_number_expr","omop_unit","omop_number_unit"))) steps<-steps[[1L]]
  omop_number_spec(pipeline=steps)
}
#' @rdname omop_number_spec
#' @export
omop_number_unit <- function(from,to,factor,offset=0) {
  u<-list(from=from,to=to,factor=factor,offset=offset)
  if(!.numberHasParam(u)) .numberUnitCheck(c(list(kind="fixed_unit"),u))
  structure(u,class=c("omop_number_unit","list"))
}
#' @rdname omop_number_spec
#' @export
omop_number_transform <- function(method="identity",factor=NULL,offset=NULL,center=NULL,scale=NULL,lower=NULL,upper=NULL) {
  s<-as.list(environment());s<-s[!vapply(s,is.null,logical(1))]
  structure(if(.numberHasParam(s))s else .numberTransformCheck(s),class=c("omop_number_transform","list"))
}
#' @rdname omop_number_spec
#' @export
omop_number_round <- function(method="none",digits=NULL) {
  s<-list(method=method);if(!is.null(digits))s$digits<-digits
  structure(if(.numberHasParam(s))s else .numberRoundCheck(s),class=c("omop_number_round","list"))
}
#' @rdname omop_number_spec
#' @export
omop_number_expr <- function(exprs,result=NULL,unit=NULL) {
  result<-.numberPublicExpression(exprs,result,"x")
  if(!is.null(unit)&&!.numberHasParam(unit)) .numberUnitToken(unit)
  s<-list(exprs=exprs,result=result);if(!is.null(unit))s$unit<-unit
  structure(s,class=c("omop_number_expr","list"))
}
.numberVariableArg <- function(v) {
  if(!inherits(v,"omop_variable")) .numberReject("Use one numeric variable declaration, not data, a column string or bundle.","number_source_invalid")
  v
}
#' @rdname omop_number_spec
#' @export
omop_number_input <- function(variable,stage="canonical") {
  structure(list(variable=.numberVariableArg(variable),stage=.numberChoice(stage,c("canonical","presented"))),class=c("omop_number_input","list"))
}
#' @rdname omop_number_spec
#' @export
omop_number <- function(variable,number_spec=NULL,broadcast=NULL) {
  v<-.numberVariableArg(variable);s<-.numberSpecArg(number_spec)
  out<-if(.numberHasParam(list(s,v$number_spec))) {low<-v$number_spec %||% list();if(!is.null(s$pipeline)||!is.null(low$pipeline)&&length(s))low<-list();utils::modifyList(low,s %||% list())} else .numberOverlay(s,v$number_spec)
  v$number_spec<-structure(out,class=c("omop_number_spec","list"))
  if(!is.null(broadcast))v$broadcast<-.v2_flag(broadcast,"numeric broadcast")
  if(identical(v$derived$kind,"number_rows")&&isTRUE(v$broadcast)) .numberReject("Event rows cannot be broadcast to scalar grain.","number_dependency_invalid")
  v
}
.numberShortcut <- function(v,s,b,...) {
  s<-.numberSpecArg(s) %||% omop_number_spec();direct<-list(...);direct<-direct[!vapply(direct,is.null,logical(1))]
  if(!is.null(s$pipeline)) .numberReject("Shortcuts use shorthand; compose bins/category with omop_number_pipeline().")
  for(k in names(direct)) {
    if(!is.null(s[[k]])&&!identical(unname(unlist(s[[k]])),unname(unlist(direct[[k]])))) .numberReject("Conflicting shortcut and number_spec fields.")
    s[[k]]<-direct[[k]]
  }
  omop_number(v,s,b)
}
#' @rdname omop_number_spec
#' @export
omop_number_bins <- function(variable,breaks=NULL,grid=NULL,closed=NULL,labels=NULL,number_spec=NULL,broadcast=NULL) .numberShortcut(variable,number_spec,broadcast,representation="bins",breaks=breaks,grid=grid,closed=closed,labels=labels)
#' @rdname omop_number_spec
#' @export
omop_number_category <- function(variable,range=NULL,thresholds=NULL,labels=NULL,number_spec=NULL,broadcast=NULL) .numberShortcut(variable,number_spec,broadcast,representation="category",range=range,thresholds=thresholds,labels=labels)
#' @rdname omop_number_spec
#' @export
omop_number_flag <- function(variable,op,thresholds,closed=NULL,number_spec=NULL,broadcast=NULL) .numberShortcut(variable,number_spec,broadcast,representation="flag",op=op,thresholds=thresholds,closed=closed)
#' @rdname omop_number_spec
#' @export
omop_number_present <- function(variable,number_spec=NULL,broadcast=NULL) .numberShortcut(variable,number_spec,broadcast,representation="present")
#' @rdname omop_number_spec
#' @export
omop_number_missing <- function(variable,number_spec=NULL,broadcast=NULL) .numberShortcut(variable,number_spec,broadcast,representation="missing")
.numberVariable <- function(d,s,b=FALSE) .v2_variable("derived",derived=d,number_spec=.numberSpecArg(s),broadcast=.v2_flag(b,"numeric broadcast"))
#' @rdname omop_number_spec
#' @export
omop_number_rows <- function(event,value=NULL,number_spec=NULL) .numberVariable(list(kind="number_rows",source=.v2_source(event,"number_rows"),value=value %||% "value"),number_spec)
#' @rdname omop_number_spec
#' @export
omop_number_column <- function(column,component=NULL,number_spec=NULL) {
  .numberNames(column);if(!is.null(component)).numberNames(component)
  .numberVariable(list(kind="number_column",column=column,component=component),number_spec)
}
#' @rdname omop_number_spec
#' @export
omop_number_count <- function(event,value=NULL,window=NULL,kind="nonmissing",number_spec=NULL,broadcast=FALSE) {
  if(!is.null(window)&&!inherits(window,"omop_window")) .numberReject("Use omop_window().")
  .numberVariable(list(kind="number_count",source=.v2_source(event,"number_count"),value=value %||% "value",window=window,count_kind=.numberChoice(kind,c("nonmissing","missing"))),number_spec,broadcast)
}
#' @rdname omop_number_spec
#' @export
omop_number_change <- function(event,method="difference",value=NULL,window=NULL,missing="skip",reference=NULL,time_unit=NULL,threshold=NULL,op=NULL,number_spec=NULL,broadcast=FALSE) {
  d<-list(kind="number_change",source=.v2_source(event,"number_change"),value=value %||% "value",window=window,method=method,missing=missing,reference=reference,time_unit=time_unit,threshold=threshold,op=op)
  d<-d[!vapply(d,is.null,logical(1))]
  if(.numberHasParam(d$threshold)) {
    parameter<-d$threshold;d$threshold<-0;d<-.numberChangeCheck(d);d$threshold<-parameter
  } else d<-.numberChangeCheck(d)
  if(!is.null(d$reference)&&!identical(d$reference,"first")) d$reference<-.dateSourceArg(d$reference)
  .numberVariable(d,number_spec,broadcast)
}
#' @rdname omop_number_spec
#' @export
omop_number_derive <- function(inputs,exprs,unit,result=NULL,reducer=NULL,number_spec=NULL,broadcast=FALSE) {
  if(!is.list(inputs)||!length(inputs)) .numberReject("Declare a nonempty named input map.","number_dependency_invalid")
  .numberNames(names(inputs))
  inputs<-lapply(inputs,function(v)if(inherits(v,"omop_number_input"))v else omop_number_input(v))
  result<-.numberPublicExpression(exprs,result,names(inputs));if(!.numberHasParam(unit)).numberUnitToken(unit)
  if(!is.null(reducer)) {
    if(!inherits(reducer,"omop_reducer")||!reducer$fn %in% c("first_value","last_value","nth_value","closest_value","mean","min","max","sum","sd","cv","median","quantile","mode","slope")||isTRUE(attr(reducer,"number_value_explicit"))||!is.null(reducer$unit)) .numberReject("Use an admitted numeric reducer with value/unit omitted.","number_expression_invalid")
  }
  if(!is.null(reducer))reducer$value<-NULL
  .numberVariable(list(kind="number_derive",inputs=inputs,exprs=exprs,unit=unit,result=result,reducer=reducer),number_spec,broadcast)
}
.numberWireSpec <- function(ctx,s,context) {
  if(is.null(s))return(NULL)
  s<-.numberSpecCheck(unclass(.numberBindParams(s,context)))
  unit<-function(u) {if(u$kind=="event_unit")u$spec<-.v2w_unit(u$spec);u}
  if(!is.null(s$unit))s$unit<-unit(s$unit)
  if(!is.null(s$pipeline))s$pipeline<-lapply(s$pipeline,function(p)if(p$kind=="event_unit")unit(p) else p)
  encode<-function(x) {
    if(is.list(x))return(lapply(x,encode))
    if(is.numeric(x)&&length(x)==1L&&!is.finite(x))return(if(x<0)"-Inf" else "Inf")
    x
  }
  for(k in intersect(names(s),c("breaks","thresholds","labels","category_map","levels")))s[[k]]<-as.list(s[[k]])
  s<-encode(s)
  if(!length(s)) .pnv2EmptyObject() else .pnv2Plain(s)
}
.numberWireDerived <- function(ctx,d,context) {
  d<-.numberBindParams(d,context)
  out<-d[!vapply(d,is.null,logical(1))]
  if(identical(d$kind,"number_daily"))out$input<-.v2w_variable(ctx,d$input,paste0(context,"_daily"))
  if(!is.null(d$source))out$source<-.v2c_source(ctx,d$source,context)
  if(!is.null(d$window))out$window<-.v2w_window(ctx,d$window,context)
  if(!is.null(d$reference))out$reference<-if(identical(d$reference,"first"))list(kind="first_observation") else .dateWireSource(ctx,d$reference,context)
  if(!is.null(d$inputs))out$inputs<-unname(lapply(names(d$inputs),function(n)list(alias=n,variable=.v2w_variable(ctx,d$inputs[[n]]$variable,paste0(context,"_",n)),stage=d$inputs[[n]]$stage)))
  if(!is.null(d$reducer))out$reducer<-.v2w_reducer(ctx,d$reducer,context)
  out
}

.numberHasParam <- function(x) {
  if(.v2_is_param(x))return(TRUE)
  is.list(x)&&any(vapply(x,.numberHasParam,logical(1)))
}
.numberBindParams <- function(x,context) {
  if(.v2_is_param(x))return(.v2_param_value(x,context))
  if(is.list(x))for(i in seq_along(x))x[i]<-list(.numberBindParams(x[[i]],context))
  x
}

.numberCheckPartial <- function(s) {
  if(!.numberHasParam(s))return(.numberSpecCheck(s))
  if(!is.null(s$rounding)&&!.numberHasParam(s$rounding)).numberRoundCheck(s$rounding)
  if(!is.null(s$unit)&&!.numberHasParam(s$unit)).numberUnitCheck(s$unit)
  if(!is.null(s$breaks)&&!.numberHasParam(s$breaks)).numberBreaks(s$breaks)
  transforms<-s$transform
  if(!is.null(transforms$method))transforms<-list(transforms)
  for(t in transforms)if(!.numberHasParam(t)).numberTransformCheck(t)
  invisible(s)
}

# Enumerate source-qualified generated keys from declared source/concept lineage.
# Outcome/exposure aliases belong to the output and intentionally do not change.
.numberGeneratedKeyMap <- function(recipe,maps) {
  result<-character();nodes<-.merge_named_nodes(recipe)
  for(kind in c("event","interval"))for(name in names(maps[[kind]])) {
    node<-nodes[[paste0(kind,":",name)]] %||% recipe[[paste0(kind,"s")]][[name]]
    if(is.null(node))next
    cs<-node$concepts
    if(is.character(cs)&&length(cs)==1L)cs<-recipe$concept_sets[[cs]]
    if(inherits(cs,"omop_concept_set"))cs<-cs$concepts
    if(.numberHasParam(cs))next
    ids<-if(is.numeric(cs))unique(cs) else numeric()
    stems<-if(kind=="interval"||length(ids)==1L)name else if(length(ids))paste0(name,"_",ids) else character()
    new_stems<-if(kind=="interval"||length(ids)==1L)maps[[kind]][[name]] else if(length(ids))paste0(maps[[kind]][[name]],"_",ids) else character()
    for(a in c("binary","count","mean","last","exposed_days","carry")) {
      result[paste0(stems,"_",a)]<-paste0(new_stems,"_",a)
    }
  }
  result
}

# Parameter literals are bound by the recipe before the same closed validator
# sees the wire request; no parameter object is sent to a site.
.numberPublicExpression <- function(exprs,result,aliases) {
  if(!.numberHasParam(exprs))return(.numberExpressionShape(exprs,result,aliases))
  if(!is.list(exprs)||!is.list(exprs$assign)||!length(exprs$assign))
    .numberReject("Use a closed mutate-v1 request.","number_expression_invalid")
  result %||% tail(vapply(exprs$assign,function(a)a$name,""),1L)
}

# Portable numeric recipe data retains explicit infinite bin tails as wire
# markers while finite boundaries keep the exact lossless numeric encoding.
.numberRecipePlain <- function(x) {
  if(inherits(x,"POSIXt"))return(format(x,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"))
  if(inherits(x,"Date"))return(format(x,"%Y-%m-%d"))
  if(is.factor(x))return(as.character(x))
  if(is.list(x))return(lapply(x,.numberRecipePlain))
  if(is.numeric(x)&&any(is.infinite(x))) {
    if(length(x)==1L)return(if(x<0)"-Inf" else "Inf")
    return(lapply(x,.numberRecipePlain))
  }
  x
}

#' Collapse numeric observations by recorded calendar day
#'
#' @param input Numeric event rows or an unreduced same-event numeric expression.
#' @param reducer Explicit numeric omop_reduce declaration, with value/unit omitted.
#' @param within_day mean, max or first in canonical event order.
#' @param missing skip or propagate within each day, independently of the outer reducer.
#' @param number_spec Final numeric presentation after both reductions.
#' @return An ordinary numeric variable requiring the number.daily capability.
#' @details Selection precedes grouping by person/consumer episode, source,
#' concept, normalized coordinate and recorded date. Input presentation precedes
#' collapse. No absent days are filled. Selected-row ranges and visit identities
#' are discarded, including for first. Multiple concepts require an explicitly
#' declared common coordinate; they remain separate concept-day observations.
#' @export
omop_number_daily <- function(input,reducer,within_day="mean",missing="skip",number_spec=NULL) {
  input<-.numberVariableArg(input)
  if(!(input$derived$kind %||% "") %in% c("number_rows","number_derive") || !is.null(input$derived$reducer))
    .numberReject("/input: use numeric event rows or an unreduced same-event derive.","number_daily_input")
  if(!inherits(reducer,"omop_reducer")||isTRUE(attr(reducer,"number_value_explicit"))||!is.null(reducer$unit))
    .numberReject("/reducer: use an explicit numeric omop_reduce with value/unit omitted.","number_daily_reducer")
  reducer$value<-NULL;.numberReducerCheck(reducer)
  .numberVariable(list(kind="number_daily",input=input,reducer=reducer,
    within_day=.numberChoice(within_day,c("mean","max","first")),
    missing=.numberChoice(missing,c("skip","propagate"))),number_spec)
}

# yaml's precision is decimal places rather than significant digits. Emit
# finite doubles in scientific notation so small coefficients round-trip too.
.numberYamlNumeric <- function(x) {
  if(any(!is.finite(x)))return(x)
  structure(sprintf("%.17e",x),class="verbatim")
}
