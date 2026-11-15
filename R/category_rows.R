#' Event category projections and identity indicators
#' @param event Typed event or registered event name.
#' @param value Registered concept-valued role; NULL selects the primary concept.
#' @param category_spec Explicit category presentation.
#' @param variable A category variable declaration.
#' @param keys Named stable category keys; names are suffixes.
#' @param missing propagate or zero for ordinary available NA.
#' @return A variable declaration or named immutable bundle of declarations.
#' @export
omop_category_rows <- function(event,value=NULL,category_spec=NULL) {
  if(!is.null(value))value<-.v2_name(value,"category value")
  .v2_variable("derived",derived=list(kind="category_rows",source=.v2_source(event,"category rows"),value=value %||% "concept_id"),category_spec=.categoryArg(category_spec),broadcast=FALSE)
}
#' @rdname omop_category_rows
#' @export
omop_category_indicators <- function(variable,keys,missing="propagate") {
  if(!inherits(variable,"omop_variable")) .categoryReject("Indicators require a typed category variable.")
  .numberNames(names(keys));.categoryKeys(unname(keys),FALSE)
  .categoryChoice(missing,c("propagate","zero"))
  out<-setNames(lapply(seq_along(keys),function(i).v2_variable("derived",derived=list(
    kind="category_indicators",variable=variable,key=unname(keys[i]),missing=missing),broadcast=isTRUE(variable$broadcast))),names(keys))
  structure(out,class=c("omop_category_bundle","list"))
}
.categoryExpandBundles <- function(variables) {
  if(is.null(variables))return(NULL)
  items<-.recipe_arg_list(variables);out<-list()
  for(i in seq_along(items)) {
    x<-items[[i]]
    if(!inherits(x,"omop_category_bundle")){out<-c(out,items[i]);next}
    name<-names(items)[i]
    if(is.null(name)||is.na(name)||!nzchar(name)) .categoryReject("Category bundles require an explicit outer name.")
    for(k in names(x))out<-c(out,setNames(list(x[[k]]),paste(name,k,sep="_")))
  }
  nms <- names(out)
  if(anyDuplicated(nms[nzchar(nms)])) .categoryReject("Expanded category names collide.")
  out
}
.categoryWireDerived <- function(ctx,d,context) {
  out<-d[!vapply(d,is.null,logical(1))]
  if(!is.null(d$reducer)) {out$reducer<-.v2w_reducer(ctx,d$reducer,context);out$reducer$value<-NULL;out$reducer$predicate<-NULL}
  if(!is.null(d$source))out$source<-.v2c_source(ctx,d$source,context)
  if(identical(d$kind,"repeated")) {
    out$values<-unname(lapply(names(d$values),function(n)list(name=n,variable=.v2w_variable(ctx,d$values[[n]],paste0(context,"_",n)))))
    if(!is.null(d$timepoints)) {
      out$timepoints<-list(kind="timepoints",anchor=.dateWireSource(ctx,d$timepoints$anchor,context),reuse=d$timepoints$reuse,
        points=unname(lapply(names(d$timepoints$points),function(n) {
          p<-unclass(d$timepoints$points[[n]]);p$tolerance<-as.list(p$tolerance)
          if(!is.null(p$anchor))p$anchor<-.dateWireSource(ctx,p$anchor,context)
          list(name=n,point=p)
        })))
    }
    if(!is.null(d$visits)) {
      out$visits<-unclass(d$visits);out$visits$numbers<-as.list(d$visits$numbers)
      out$visits$source<-.v2c_source(ctx,d$visits$source,context)
      if(!is.null(d$visits$window))out$visits$window<-.v2w_window(ctx,d$visits$window,context)
    }
  }
  if(!is.null(d$variable))out$variable<-.v2w_variable(ctx,d$variable,paste0(context,"_input"))
  out
}

.categoryIndicatorScalar <- function(variable,key,missing) omop_category_indicators(variable,c(value=key),missing)[["value"]]

#' Copy a categorical output column before presentation
#' @param column Stable original column key.
#' @param component Exact component for outputs with multiple row frames.
#' @param category_spec Independent category presentation.
#' @return A row-preserving output-local variable declaration.
#' @export
omop_category_column <- function(column,component=NULL,category_spec=NULL) {
  .v2_variable("derived",derived=list(kind="category_column",column=.v2_name(column,"category column"),component=if(!is.null(component)).v2_name(component,"category component")),category_spec=.categoryArg(category_spec),broadcast=FALSE)
}

#' Reduce explicitly transformed event category identities
#' @param variable Typed event category or indicator projection.
#' @param reducer Existing typed selector, mode, count, distinct or flag reducer.
#' @param category_spec Optional final presentation (after reduction).
#' @param broadcast Explicit scalar broadcast.
#' @return A scalar variable declaration (or the reducer's nth-value columns).
#' @export
omop_category_reduce <- function(variable,reducer,category_spec=NULL,broadcast=FALSE) {
  # Evaluate the reducer promise with this typed consumer as its input context.
  # Outside this dynamic extent, predicate-free ordinary any/all still refuse.
  reducer <- withCallingHandlers(reducer,
    dsomop_reducer_predicate_required = function(e) {
      invokeRestart("dsomop_category_reducer_input")
    })
  if(!inherits(variable,"omop_variable")||!inherits(reducer,"omop_reducer")) .categoryReject("Use a typed event category and omop_reduce().")
  if(!reducer$fn %in% c("first_value","last_value","nth_value","closest_value","mode","count","n_distinct","any","all","sum")) .categoryReject("Use a category selector/count or semantic-indicator reducer.")
  if(isTRUE(attr(reducer,"number_value_explicit"))||!is.null(reducer$predicate)) .categoryReject("Category reducers consume their typed input; omit value and predicate.")
  .v2_variable("derived",derived=list(kind="category_reduce",variable=variable,reducer=reducer),category_spec=.categoryArg(category_spec),broadcast=.categoryBool(broadcast))
}
