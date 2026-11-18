# Pure public-plan schemas. Open vocabulary/source schemas stay unresolved.
.codebookAllocateComponents <- function(readable, identities, fixed = rep(FALSE, length(readable))) {
  out <- rep(NA_character_, length(readable))
  out[fixed] <- readable[fixed]
  repeated <- duplicated(readable) | duplicated(readable, fromLast = TRUE)
  plain <- !fixed & !repeated & nchar(readable) <= 64L & grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", readable)
  out[plain] <- readable[plain]
  used <- out[!is.na(out)]
  for (i in order(identities, method = "radix")) {
    if (!is.na(out[[i]])) next
    hash <- substr(paste0(as.character(openssl::sha256(charToRaw(enc2utf8(identities[[i]]))))),1L,10L)
    probe <- 1L
    repeat {
      tail <- paste0("_", hash, if (probe > 1L) paste0("_", probe))
      candidate <- paste0(substr(readable[[i]], 1L, 64L - nchar(tail)), tail)
      if (!candidate %in% used) break
      probe <- probe + 1L
    }
    out[[i]] <- candidate
    used <- c(used, candidate)
  }
  if (anyDuplicated(out) || any(!grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", out))) {
    stop("Output component allocation failed its distinctness check.", call. = FALSE)
  }
  out
}

# Keep the primary's name (and therefore its requested symbol and NULL staged
# semantic component). Reference companions keep their documented names unless
# the output itself uses one of them; the dynamic inventory carries that name.
.codebookReferenceNames <- function(output, references) {
  readable <- c(output, references)
  identities <- c(.jcsCanonical(list(output, "primary")),
                  vapply(references, function(ref) .jcsCanonical(list(output, "reference", ref)), ""))
  allocated <- .codebookAllocateComponents(readable, identities,
                                         c(TRUE, rep(FALSE, length(references))))
  stats::setNames(allocated[-1L], references)
}

# Component names of one TAR's family, allocated against all sibling TARs.
.codebookSurvivalFamily <- function(fmt, opts, tar, output, multi) {
  tars <- if (multi) sort(names(opts[["tars"]]),method="radix") else tar
  part_names <- switch(fmt, recurrent_events = c("events", "risk_sets"),
                       multi_state = "msdata", "primary")
  readable <- identities <- character(0)
  fixed <- logical(0)
  for (t in tars) for (p in part_names) {
    readable <- c(readable, if (identical(fmt, "recurrent_events")) {
      if (multi) paste0(t, "_", p) else p
    } else if (multi) t else output)
    identities <- c(identities, .jcsCanonical(list(output, t, p)))
    fixed <- c(fixed, !identical(fmt, "recurrent_events"))
  }
  if (identical(fmt, "multi_state")) {
    readable <- c(readable, "transitionRef")
    identities <- c(identities, .jcsCanonical(list(output, "", "transition_ref")))
    fixed <- c(fixed, FALSE)
  }
  allocated <- .codebookAllocateComponents(readable, identities, fixed)
  pick <- vapply(part_names, function(p) .jcsCanonical(list(output, tar, p)), "")
  out <- as.list(stats::setNames(allocated[match(pick, identities)], part_names))
  if (identical(fmt, "multi_state")) out$transition_ref <- tail(allocated, 1L)
  out
}

.codebookPlanColumn <- function(key,type="unresolved",semantic="unresolved",variable=NULL,defaults=NULL,plan=list(),output_node=NULL,original=NULL) {
  if(type=="flag") {type<-"integer";semantic<-"flag"}
  spec<-.categoryEffective(variable$category_spec,defaults)
  d<-variable$derived;inherited<-original
  if(!is.null(original)) {type<-original$r_type;semantic<-original$semantic_type}
  operand<-if(identical(d$kind,"repeated")) {
    at<-match(d$value,vapply(d$values,function(x)x$name,""));d$values[[at]]$variable
  } else if(identical(d$kind,"category_reduce")&&!d$reducer$fn %in% c("count","n_distinct","sum","any","all"))d$variable else NULL
  if(!is.null(operand)) {
    if (identical(d$kind,"category_reduce") && is.null(operand$category_spec))
      operand$category_spec <- list(order=list(kind="category_order",metadata="producer"))
    input_plan <- plan; input_plan$options$number_spec <- NULL; input_plan$options$date_spec <- NULL
    inherited<-.codebookPlanColumn(key,variable=operand,plan=input_plan)
    type<-inherited$r_type;semantic<-inherited$semantic_type
  }
  date<-variable$date_spec
  date_kind<-if((d$kind %||% "") %in% .DATE_KINDS)d$kind else "calendar_date"
  if(!is.null(date)||(d$kind %||% "") %in% .DATE_KINDS || is.null(variable)&&grepl("_date$",key)) {
    date<-.dateEffective(date,plan$options$date_spec,date_kind,d$part)
    semantic <- if(date_kind %in% c("time_between","age_at"))"numeric" else "date"
    type <- switch(.dateResultType(date,list(kind=date_kind,part=d$part,unit=d$unit,bands=d$bands)),
      text="ordered_factor",date="Date",integer="integer",numeric="double")
  }

  was_flag <- semantic=="flag"
  if(is.null(variable) && semantic=="date" && is.null(plan$options$date_spec)) {
    mode <- output_node$options$date_handling$mode %||% "relative"
    date <- NULL
    type <- switch(mode,absolute="Date",binned="character","integer")
  }
  number<-variable$number_spec
  number_input <- if(identical(d$kind,"number_column"))attr(original,"number_input") else NULL
  if(!is.null(number_input)) {
    type <- .presentationDescribe(number_input$value,domains=FALSE)$r_type
    semantic <- if(isTRUE(number_input$flag))"flag" else "numeric"
    number <- number %||% number_input$spec
  }
  concept_kind <- .categoryDeclaredConcept(variable)
  concept <- isTRUE(concept_kind) || semantic=="concept"
  if(concept)type <- inherited$r_type %||% "factor"
  fn <- variable$reducer$fn %||% d$reducer$fn %||% ""
  generated_numeric <- is.null(variable) && type %in% c("integer","double") && !semantic %in% c("key","concept","date") &&
    !key %in% .numberStructuralColumns(output_node$format %||% "wide") &&
    (!identical(output_node$format,"survival") || key %in% .numberOutputDurations("survival"))
  numeric <- (generated_numeric || !is.null(variable)) && !concept && !type %in% c("factor","ordered_factor","Date") &&
    (generated_numeric || nzchar(fn) || (d$kind %||% "") %in% c(.NUMBER_KINDS,"category_indicators","died") || semantic %in% c("numeric","date") && type %in% c("integer","double") || identical(inherited$semantic_type,"numeric") || identical(inherited$semantic_type,"flag"))
  pipeline<-if(numeric && (!is.null(number)||!is.null(plan$options$number_spec))) .numberEffective(number,plan$options$number_spec)$pipeline else list()
  terminal<-if(length(pipeline))tail(pipeline,1L)[[1L]] else list()
  representation<-terminal$kind %||% ""
  if(!is.null(variable)) {
    fn<-variable$reducer$fn %||% variable$derived$reducer$fn %||% ""
    flag<-fn %in% c("binary","any","all","abnormal","abnormal_low","abnormal_high") ||
      identical(variable$derived$kind,"category_indicators") || identical(variable$derived$kind,"died") || identical(inherited$semantic_type,"flag") || semantic=="flag"
    was_flag<-flag
    flag_column <- list()
    attr(flag_column,"semantic_flag") <- flag
    attr(flag_column,"number_contract") <- list(pipeline=pipeline)
    flag <- .categoryFlag(variable,flag_column)
    if(was_flag&&!flag) {semantic<-"numeric";type<-"double"}
    if(flag) {semantic<-"flag";type<-spec$binary$as %||% "integer"}
    else if(concept) {semantic<-"concept";type<-inherited$r_type %||% "factor"}
    else if(representation %in% c("bins","category")) {semantic<-representation;type<-"ordered_factor"}
    else if(fn %in% c("count","n_distinct","sum","mean","min","max","median","first_value","last_value","nth_value","closest_value","mode") && is.null(inherited) || (d$kind %||% "") %in% c("number_rows","number_derive","number_change","number_count","number_daily")) {semantic<-"numeric";type<-if(fn %in% c("count","n_distinct")||identical(d$kind,"number_count"))"integer" else "double"}
    if (numeric && !flag && !representation %in% c("bins","category")) {
      for (step in pipeline) {
        if (step$kind=="fixed_unit" || step$kind=="transform" && step$method!="identity" || step$kind=="round" && step$method!="none") type<-"double"
        if (step$kind=="expression") type<-"unresolved"
      }
    }
    spec <- .categoryApplicableVocabulary(spec,variable$category_spec,concept_kind)
    if (!flag) spec$binary <- NULL
  }
  # Structural producers have no variable node. Only concept/date factors
  # consume category defaults; keys and ordinary measures have no category
  # lineage. Vocabulary fields require concepts and binary requires a flag.
  if (is.null(variable)) {
    if (semantic %in% c("date","concept") && type %in% c("factor","ordered_factor")) {
      spec <- .categoryApplicableVocabulary(spec,NULL,semantic=="concept")
      spec$binary <- NULL
    } else spec <- list()
  }
  unit<-number_input$unit %||% inherited$unit %||% if(!is.null(variable)&&semantic %in% c("numeric","flag","bins","category")) .categoryDeclaredUnit(plan,variable) else "not_applicable"
  for(step in pipeline) {
    if(step$kind=="event_unit")unit<-paste0("unit:",step$spec$target)
    if(step$kind=="fixed_unit")unit<-step$to
    if(step$kind=="expression")unit<-step$unit %||% "unresolved"
    if(step$kind %in% c("flag","present","missing"))unit<-"1"
  }
  # Evaluate public empty prototypes with the execution functions. There are no
  # patient values or vocabulary queries in this path. Open domains stay open.
  value <- switch(type,integer=integer(),double=double(),logical=logical(),
    character=character(),Date=as.Date(character()),factor=factor(character()),
    ordered_factor=ordered(character()),NULL)
  if (!is.null(date) && !date_kind %in% c("time_between","age_at"))
    value <- .datePresent(list(lo=as.Date(character()),hi=as.Date(character())),date,
      part=if(date_kind=="date_part")d$part,anchor=as.Date(character()))
  base <- if(fn %in% c("count","n_distinct") || identical(d$kind,"number_count") ||
    isTRUE(was_flag))integer() else if(!is.null(value)&&is.numeric(value)&&!is.factor(value))value else double()
  source_unit <- number_input$unit %||% if(!is.null(variable)).categoryDeclaredUnit(plan,variable) else unit
  if(!is.null(number_input))base <- number_input$value
  number_input <- if(numeric)list(value=base,unit=source_unit,spec=number,flag=isTRUE(was_flag))
  if (numeric && length(pipeline)) {
    # Expressions whose operand schema is not yet bound remain unresolved.
    if(any(vapply(pipeline,function(p)p$kind=="expression",logical(1)))) value <- NULL
    else {
      value <- .numberPresent(base,list(pipeline=pipeline),
        unit=source_unit,
        ranges=list(low=double(),high=double()))
    }
  }
  frame <- data.frame(value=if(is.null(value))logical() else value)
  column <- list(type=type,role="measure")
  if(!is.null(attr(value,"omop_date_contract"))) frame <- .dateCapture(frame,list(value=column))
  else if(representation %in% c("bins","category") && is.factor(value)) {
    attr(column,"number_contract") <- list(pipeline=.numberWireFinite(pipeline))
    frame <- .numberCapture(frame,list(value=column))
  } else if(!is.null(inherited$levels) && is.factor(value)) {
    keys <- unlist(inherited$levels$keys,use.names=FALSE)
    labels <- unlist(inherited$levels$labels,use.names=FALSE)
    contract <- list(spec=list(),producer=list(kind=semantic),producer_order=as.list(keys),producer_ordered=is.ordered(value))
    records <- setNames(lapply(seq_along(keys),function(i)list(id=keys[i],label=labels[i],origin="producer",category=contract)),keys)
    attr(frame,"omop_concepts") <- list(value=list(kind="category",category=contract,records=records,ids=character(),declared=keys))
  }
  if(semantic=="flag") {
    attr(column,"semantic_flag") <- TRUE
    attr(column,"number_contract") <- list(pipeline=.numberWireFinite(pipeline))
    if(!.categoryFlag(variable,column))semantic <- if(is.factor(value))representation else "numeric"
  }
  attr(column,"category_spec") <- variable$category_spec
  input <- list(frame=frame,column=column)
  if(!is.null(original) && identical(d$kind,"category_column")) {
    input <- attr(original,"presentation_input") %||% input
    frame <- input$frame; column <- input$column
    attr(column,"category_spec") <- .categoryEffective(variable$category_spec,attr(column,"category_spec"))
    value <- frame$value
    concept <- FALSE
  }
  if(!is.null(value) && !concept && semantic!="concept") {
    context <- list(ir=list(options=list(category_spec=defaults),variables=list()))
    attr(column,"category_spec") <- .categoryEffective(variable$category_spec,attr(column,"category_spec"))
    if(!length(attr(column,"category_spec")))attr(column,"category_spec") <- NULL
    frame <- .categoryPresent(context,frame,list(value=column))
    if(!is.null(value))value <- frame$value
    spec <- attr(frame,"omop_concepts")$value$category$spec %||% spec
  } else if(is.factor(value)) {
    # Vocabulary labels and mapped domains are unresolved, but factor type and
    # ordering use the very same resolver/factor constructor as publication.
    contract <- list(spec=spec,producer=list(kind="concept"),producer_ordered=is.ordered(value))
    value <- .categoryFactor(character(),.categoryResolveRecords(list(),complete=FALSE,identity=contract))
  }
  description <- if(is.null(value))list(r_type="unresolved") else
    .presentationDescribe(value,attr(frame,"omop_concepts")$value)
  type <- description$r_type
  out <- list(stable_key=key,name=key,meaning=if(is.null(variable))paste("Output",semantic,"role",key) else paste("Declared variable",key),
    semantic_type=semantic,r_type=type,storage_type="unresolved",unit=unit,
    levels=description$levels,ordered=description$ordered,reference=description$reference,
    contrasts=description$contrasts,
    derivation=list(variable=variable,category_spec=spec),privacy_class=if(!is.null(variable$date_spec)||identical(variable$derived$kind,"repeated"))2L else "unresolved",
    aggregate_policy="Existing endpoint admission; unresolved for unrequested endpoints",
    missing_policy="Ordinary available NA follows the declaration; unavailable and undefined anchors remain NA",
    provenance=list(source="public plan; no observed schema or vocabulary lookup"),completeness="unresolved")
  attr(out,"number_input") <- number_input
  attr(out,"presentation_input") <- input
  out
}

# Closed panel identities can be enumerated without a vocabulary or patient query.
# Expanded/mapped concept sets remain open until admitted release metadata exists.
.codebookPanelColumns <- function(plan, node) {
  opts <- node$options %||% list()
  columns <- c(period_start_days="integer", period_end_days="integer", days="integer")
  if (!is.null(opts$partition$age_bands)) columns <- c(columns, age="integer", age_band="integer")
  if (identical(opts$bins$calendar,"year") || identical(opts$partition$calendar,"year"))
    columns <- c(columns, calendar_year="integer")
  if (identical(opts$partition$calendar,"month")) columns <- c(columns, calendar_month="integer")
  for (key in names(opts$exposures)) columns <- c(columns, setNames(c("integer","flag"),c(paste0("exposed_",key),paste0("active_",key))))
  for (key in names(opts$outcomes)) columns <- c(columns, setNames("integer",paste0("n_",key)))
  concepts <- function(cs) {
    if (is.character(cs) || is.list(cs) && length(cs)==1L && is.character(cs[[1L]]))
      cs <- plan$concept_sets[[as.character(unlist(cs))]]
    if (is.null(cs) || isTRUE(cs$include_descendants) || isTRUE(cs$include_mapped)) return(NULL)
    ids <- as.character(unlist(cs$concepts,use.names=FALSE))
    excluded <- cs$exclude
    if (is.list(excluded) && !is.null(excluded$concepts)) {
      excluded <- concepts(excluded)
      if (is.null(excluded)) return(NULL)
    }
    setdiff(ids,as.character(unlist(excluded,use.names=FALSE)))
  }
  identities <- function(source) {
    if (!is.null(source$interval)) return(NA_character_)
    event <- plan$events[[source$event]]
    if (!is.null(event$cost)) return(identities(event$cost$event))
    concepts(event$concept_set)
  }
  analyses <- c("binary","count","mean","last","exposed_days")
  for (source in c(node$sources,opts$sources)) {
    key <- source$event %||% source$interval
    event <- plan$events[[key]]
    ids <- identities(source)
    selected <- unlist(opts$analyses %||% list("binary"),use.names=FALSE)
    if (!is.null(opts$source_analyses[[key]])) selected <- intersect(selected,unlist(opts$source_analyses[[key]],use.names=FALSE))
    if (is.null(source$interval)) selected <- setdiff(selected,"exposed_days")
    has_value <- is.null(source$interval) && (!is.null(event$cost) || !is.null(event$value$column) || event$table %in% c("measurement","observation"))
    if (!has_value) selected <- setdiff(selected,c("mean","last"))
    selected <- analyses[analyses %in% selected]
    stems <- if (length(ids)==1L) key else paste0(key,"_",ids)
    if (!length(ids)) stems <- character()
    for (stem in stems) {
      if (length(selected)) columns <- c(columns,setNames(ifelse(selected=="binary","flag",ifelse(selected %in% c("mean","last"),"double","integer")),paste0(stem,"_",selected)))
      if (!is.null(opts$carry_forward[[key]])) columns <- c(columns,setNames("double",paste0(stem,"_carry")))
    }
  }
  columns
}

# Every declared native roster (including unindexed persons and inherited/set
# rosters) carries nullable index provenance. Membership publishes it after the
# dates, even when all its values are NULL; no patient/schema lookup is needed.
.codebookMembershipColumns <- function(plan, node) {
  .presentationMembershipColumns(plan$populations[[node$rows]],node$options %||% list())
}

.codebookPlanSchemas <- function(plan) {
  schemas<-list();visiting<-character()
  build<-function(output) {
    if(any(startsWith(names(schemas) %||% character(),paste0(output,"/"))))return(invisible(NULL))
    if(output %in% visiting) .categoryReject("Cyclic output dependency.","codebook_metadata_missing")
    visiting<<-c(visiting,output);on.exit(visiting<<-setdiff(visiting,output))
    node<-plan$outputs[[output]];opts<-node$options %||% list();fmt<-node$format
    # Native reducer transport preserves BIGINT row keys as exact decimal
    # text; publication also exposes person pseudonyms as character strings.
    id<-if(identical(node$grain,"person"))c(person_id="character") else c(row_id="character",cohort_row_id="character",person_id="character")
    definitions<-list();grains<-list();origins<-list()
    add<-function(component,types,grain=node$grain %||% "episode") {definitions[[component]]<<-types;grains[[component]]<<-grain}
    if(fmt=="survival") {
      style<-opts$format %||% "survival";multi<-length(opts$tars)>0L;tars<-if(multi)sort(names(opts$tars),method="radix") else output
      common<-c(outcome_name="character",event="integer")
      for(tar in tars) {
        family<-.codebookSurvivalFamily(style,opts,tar,output,multi)
        for(part in names(family)) {
          extra<-switch(part,primary=if(style=="counting_process")c(common,interval_number="integer",interval_start_days="integer",interval_end_days="integer") else c(common,entry_days_from_index="integer",exit_days_from_index="integer",follow_up_days="integer"),
            events=c(common,event_number="integer",outcome_event_number="integer",event_days_from_index="integer",entry_days_from_index="integer",exit_days_from_index="integer"),
            risk_sets=c(entry_days_from_index="integer",exit_days_from_index="integer",follow_up_days="integer"),
            msdata=c(from="integer",to="integer",trans="integer",Tstart="double",Tstop="double",time="double",status="integer",from_name="character",to_name="character",state_visit_number="integer"),
            transition_ref=c(from="integer",to="integer",trans="integer",from_name="character",to_name="character",from_hierarchy_rank="integer",to_hierarchy_rank="integer",from_is_initial="logical",to_is_absorbing="logical",tie_policy="character",state_step="double"))
          ref<-part=="transition_ref";add(family[[part]],c(if(!ref)id,extra),if(ref)"none" else "episode")
        }
      }
    } else if(fmt %in% c("sparse","temporal_covariates")) {
      add(output,c(id,if(fmt=="temporal_covariates")c(time_id="integer"),covariate_id="integer",covariate_value="double"))
      refs<-.codebookReferenceNames(output,c("covariateRef",if(fmt=="temporal_covariates")"timeRef"))
      add(refs[["covariateRef"]],c(covariate_id="integer",source="character",concept_id="factor",concept_name="character",analysis="character"),"none")
      if(fmt=="temporal_covariates")add(refs[["timeRef"]],c(time_id="integer",start_day="integer",end_day="integer"),"none")
    } else if(fmt=="joined") {
      rows<-list()
      for(source in opts$from) {
        build(source$output)
        entry<-schemas[[paste(source$output,source$component %||% source$output,sep="/")]]
        for(column in entry$schema$columns)if(is.null(rows[[column$stable_key]]))rows[[column$stable_key]]<-column
      }
      add(output,setNames(vapply(rows,function(c)c$r_type,""),names(rows)));origins[[output]]<-rows
    } else {
      extra<-switch(fmt,wide=character(),membership=.codebookMembershipColumns(plan,node),
        dictionary=c(concept_id="factor",concept_name="character",domain_id="character",vocabulary_id="character",concept_class_id="character",source="character"),
        long=c(concept_id="factor",start_date="unresolved",end_date="unresolved"),
        intervals=c(interval_type="character",concept_id="factor",start_days_from_index="integer",end_days_from_index="integer"),
        person_period=.codebookPanelColumns(plan,node),character())
      add(output,c(if(fmt!="dictionary")id,extra),if(fmt=="dictionary")"none" else node$grain %||% "episode")
    }
    for(component in names(definitions)) {
      types<-definitions[[component]]
      rows<-lapply(names(types),function(key)origins[[component]][[key]] %||% .codebookPlanColumn(key,types[[key]],if(key %in% names(id))"key" else if(key=="concept_id")"concept" else "structural",defaults=plan$options$category_spec,plan=plan,output_node=node))
      names(rows)<-names(types)
      structural <- names(rows)
      selected <- unlist(node$columns,use.names=FALSE)
      owners <- .pnv2ColumnVariables(selected,plan$variables)
      declared <- unlist(lapply(owners,function(key).pnv2VariableColumns(key,plan$variables[[key]])),use.names=FALSE)
      expanded <- any(selected %in% setdiff(declared,names(plan$variables)))
      if(component==output || fmt=="survival" && grains[[component]]!="none") {
        variables<-unique(c(.pnv2ColumnVariables(unlist(node$columns,use.names=FALSE),plan$variables),
          if(fmt=="wide" && is.null(node$columns))names(plan$variables)))
        for (selected in variables) {
          owners <- .pnv2ColumnVariables(selected, plan$variables)
          for (owner in owners) {
            keys <- if (selected == owner) .pnv2VariableColumns(owner, plan$variables[[owner]]) else selected
            for (key in keys) rows[[key]] <- .codebookPlanColumn(key, variable=plan$variables[[owner]], defaults=plan$options$category_spec, plan=plan)
          }
        }
      }
      rows <- rows[.presentationVariableOrder(names(rows),plan$variables,node$columns)]
      # Execution appends native endpoints after variable covariates, presents
      # them, then projects copies. Reference components have no roster index.
      if (grains[[component]] != "none") {
        endpoints <- .dateOutputEndpoints(fmt, names(rows), plan$options$date_spec)
        for (key in unname(endpoints)) {
          if (key %in% names(rows)) .dateReject("Native endpoint name collides with an explicit variable.", "date_parts_names")
          rows[[key]] <- .codebookPlanColumn(key, "Date", "date", defaults=plan$options$category_spec, plan=plan, output_node=node)
        }
        structural <- c(structural, unname(endpoints))
      }
      if (!expanded && (length(node$number_columns) || length(node$category_columns))) {
        # Match execution's typed numeric eligibility before final projection.
        # Keys and reserved event/status roles are required; measures are selectable.
        analytic <- vapply(rows,function(column) column$r_type %in% c("integer","integer64","double") &&
          !column$semantic_type %in% c("key","concept","date"),logical(1))
        analytic[names(rows) %in% .numberStructuralColumns(fmt)] <- FALSE
        structural <- setdiff(structural,names(rows)[analytic])
      }
      if (fmt == "joined" && (expanded || length(node$number_columns) || length(node$category_columns))) {
        structural <- structural[vapply(rows[structural], function(column) is.null(column$derivation$variable), logical(1))]
      }
      for(map in list(node$number_columns,node$category_columns))for(key in names(map)) {
        v<-map[[key]];dep<-v$derived
        while(!is.null(dep$variable))dep<-dep$variable$derived
        if(is.null(dep$component)&&length(definitions)==1L||identical(dep$component,component))rows[[key]]<-.codebookPlanColumn(key,variable=v,defaults=plan$options$category_spec,plan=plan,output_node=node,original=rows[[dep$column %||% ""]])
      }
      # Ordinary selected covariates already have their execution order. Only
      # expanded keys or output copies trigger execution's final projection.
      if (!is.null(node$columns) && grains[[component]] != "none" &&
          (expanded || length(node$number_columns) || length(node$category_columns))) {
        rows <- rows[.categorySelectedColumns(node$columns, plan$variables, structural, names(rows))]
      }
      schemas[[paste(output,component,sep="/")]]<<-list(schema=list(columns=unname(rows),row_keys=as.list(intersect(names(id),names(rows)))),categories=list(),row_grain=grains[[component]],status="prospective")
    }
  }
  for(output in names(plan$outputs))build(output)
  schemas[unlist(lapply(names(plan$outputs),function(output)names(schemas)[startsWith(names(schemas) %||% character(),paste0(output,"/"))]),use.names=FALSE)]
}

.categoryExplain <- function(plan) {
  if(!length(.categoryCapabilities(plan)))return(character())
  lines<-c("Categorical identity: rollup, explicit recode, user-label equivalence, then Missing/Unknown; labels and order never determine identity.",
    "Precedence: wrapper/variable > bundle defaults > recipe > design; nested declarations are atomic. Inapplicable recipe defaults are inactive; explicit incompatible fields refuse.",
    "Semantic flags retain false/true identity through integer, logical and factor presentation. Numeric arithmetic clears flag identity unless a terminal explicit flag establishes it again.",
    "Missing applies to available ordinary NA only; unavailable sources, undefined anchors and overflow targets remain NA. Sentinels are analyst-chosen categories, not clinical severity.",
    "Open concept and period domains remain unresolved until admitted federation metadata certifies completeness. Codebooks never attest occurrence or site support.",
    "Repeated selection uses one canonical source row, before display; points are processed in declaration order. Month offsets clamp from the original anchor; years use February-29 to March-1.")
  defaults<-plan$options$category_spec
  if(!is.null(defaults))lines<-c(lines,paste0("Recipe category defaults: ",.jcsCanonical(.pnv2Plain(defaults))))
  for(name in names(plan$variables)) {
    v<-plan$variables[[name]];s<-.categoryEffective(v$category_spec,defaults)
    if(length(s))lines<-c(lines,paste0("Category variable '",name,"': ",.jcsCanonical(.pnv2Plain(s)),"; applicability follows its declared producer; complete public keys are in recipe_codebook()."))
    declaration<-.codebookPlanColumn(name,variable=v,defaults=defaults,plan=plan)
    inactive<-character()
    if(declaration$semantic_type %in% c("bins","category","numeric"))inactive<-intersect(names(defaults),c("label_style","language","rollup","binary"))
    if(declaration$semantic_type=="flag"&&declaration$r_type %in% c("integer","logical"))inactive<-union(inactive,intersect(names(defaults),c("levels","order","ordered","reference","recode","rollup","label_style","language","missing")))
    inactive<-setdiff(inactive,names(v$category_spec))
    if(length(inactive))lines<-c(lines,paste0("  Inactive recipe fields for '",name,"': ",paste(inactive,collapse=", ")))
    d<-v$derived
    if(identical(d$kind,"repeated"))lines<-c(lines,paste0("Repeated variable '",name,"': ",.jcsCanonical(.pnv2Plain(d)),"; broadcast=",isTRUE(v$broadcast),"; date-dependent class 2."))
    if(identical(d$kind,"category_reduce"))lines<-c(lines,paste0("Category reduction '",name,"' applies identity grouping before ",d$reducer$fn,"; consumed input operations are not repeated on the result."))
    if(!is.null(s$rollup)&&s$rollup$kind!="none")lines<-c(lines,paste0("  Public vocabulary mapping only: ",s$rollup$kind,"; out-of-domain inputs become NA; multiple=",s$rollup$multiple,"; unmatched=",s$rollup$unmatched,
      if(s$rollup$kind=="icd10")"; vocabulary-derived classification, not recorded source code." else "; effective adapter/domain/map digests must agree across sites."))
  }
  for(name in names(plan$outputs))if(length(plan$outputs[[name]]$category_columns))lines<-c(lines,paste0("Output '",name,"' category copies bind original stable keys once: ",.jcsCanonical(.pnv2Plain(plan$outputs[[name]]$category_columns))))
  lines
}
