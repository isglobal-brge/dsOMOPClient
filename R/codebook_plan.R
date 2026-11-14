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

.codebookPlanColumn <- function(key,type="unresolved",semantic="unresolved",variable=NULL,defaults=NULL,plan=list()) {
  spec<-.categoryEffective(variable$category_spec,defaults)
  category_present <- length(spec)>0L
  d<-variable$derived;inherited<-NULL
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

  number<-variable$number_spec
  concept_kind <- .categoryDeclaredConcept(variable)
  concept <- isTRUE(concept_kind)
  fn <- variable$reducer$fn %||% d$reducer$fn %||% ""
  numeric <- !is.null(variable) && !concept && !type %in% c("factor","ordered_factor","Date") &&
    (nzchar(fn) || (d$kind %||% "") %in% c(.NUMBER_KINDS,"category_indicators","died") || semantic %in% c("numeric","date") && type %in% c("integer","double") || identical(inherited$semantic_type,"numeric") || identical(inherited$semantic_type,"flag"))
  pipeline<-if(numeric && (!is.null(number)||!is.null(plan$options$number_spec))) .numberEffective(number,plan$options$number_spec)$pipeline else list()
  terminal<-if(length(pipeline))tail(pipeline,1L)[[1L]] else list()
  representation<-terminal$kind %||% ""
  if(!is.null(variable)) {
    fn<-variable$reducer$fn %||% variable$derived$reducer$fn %||% ""
    flag<-fn %in% c("binary","any","all","abnormal","abnormal_low","abnormal_high") ||
      identical(variable$derived$kind,"category_indicators") || identical(variable$derived$kind,"died") || identical(inherited$semantic_type,"flag")
    was_flag<-flag
    for(step in pipeline) {
      if(step$kind %in% c("flag","present","missing"))flag<-TRUE
      else if(!(step$kind=="transform" && step$method=="identity" || step$kind=="round" && step$method=="none" || step$kind=="fixed_unit" && step$factor==1 && (step$offset %||% 0)==0))flag<-FALSE
    }
    if(was_flag&&!flag) {semantic<-"numeric";type<-"double"}
    if(flag) {semantic<-"flag";type<-spec$binary$as %||% "integer"}
    else if(concept) {semantic<-"concept";type<-if(isFALSE(plan$options$translate_concepts))"integer" else inherited$r_type %||% "factor"}
    else if(representation %in% c("bins","category")) {semantic<-representation;type<-"ordered_factor"}
    else if(fn %in% c("count","n_distinct","sum","mean","min","max","median","first_value","last_value","nth_value","closest_value","mode") && is.null(inherited) || (d$kind %||% "") %in% c("number_rows","number_derive","number_change","number_count")) {semantic<-"numeric";type<-if(fn %in% c("count","n_distinct")||identical(d$kind,"number_count"))"integer" else "double"}
    if (numeric && !flag && !representation %in% c("bins","category")) {
      for (step in pipeline) {
        if (step$kind=="fixed_unit" || step$kind=="transform" && step$method!="identity" || step$kind=="round" && step$method!="none") type<-"double"
        if (step$kind=="expression") type<-"unresolved"
      }
    }
    spec <- .categoryApplicableVocabulary(spec,variable$category_spec,concept_kind)
    if (!flag) spec$binary <- NULL
    if(type %in% c("factor","ordered_factor")) {
      if(!is.null(spec$ordered))type<-if(spec$ordered)"ordered_factor" else "factor"
      if(length(spec$recode$map))type<-if(isTRUE(spec$ordered %||% spec$recode$ordered))"ordered_factor" else "factor"
    }
  }
  unit<-inherited$unit %||% if(!is.null(variable)&&semantic %in% c("numeric","flag","bins","category")) .categoryDeclaredUnit(plan,variable) else "not_applicable"
  for(step in pipeline) {
    if(step$kind=="event_unit")unit<-paste0("unit:",step$spec$target)
    if(step$kind=="fixed_unit")unit<-step$to
    if(step$kind=="expression")unit<-step$unit %||% "unresolved"
    if(step$kind %in% c("flag","present","missing"))unit<-"1"
  }
  # Resolve closed producer domains with the same public identity rules as
  # publication. Open vocabulary names still require admitted metadata.
  levels<-inherited$levels$keys; labels<-inherited$levels$labels
  if(representation %in% c("bins","category")) {
    labels<-.numberDeclaredLevels(terminal)
    if(length(labels))levels<-paste0(if(!category_present)"N" else if(representation=="bins")"bin:" else "category:",seq_along(labels))
  }
  if (!is.null(date) && type %in% c("factor","ordered_factor") && !representation %in% c("bins","category")) {
    public <- if (date_kind=="date_part") .datePart(as.Date(character()),d$part,date) else if (date_kind=="calendar_date") .datePresent(list(lo=as.Date(character()),hi=as.Date(character())),date) else NULL
    calendar <- attr(public,"omop_date_contract")
    if (length(calendar$levels)) {levels<-if(category_present)paste0("period:",calendar$levels) else calendar$levels;labels<-calendar$labels}
  }
  if(identical(semantic,"flag")&&type %in% c("factor","ordered_factor")) {levels<-c("flag:0","flag:1");labels<-spec$binary$labels %||% inherited$levels$labels %||% c("no","yes")}
  resolved <- NULL
  if (type %in% c("factor","ordered_factor") && category_present && length(levels) && length(labels)==length(levels) && !concept) {
    if (semantic %in% c("bins","category")) levels<-sub("^N",if(semantic=="bins")"bin:" else "category:",levels)
    if (semantic=="date") levels<-ifelse(startsWith(levels,"period:"),levels,paste0("period:",levels))
    contract <- list(spec=spec,producer=list(kind=semantic),producer_order=as.list(levels),producer_ordered=type=="ordered_factor")
    records <- setNames(lapply(seq_along(levels),function(i)list(id=levels[[i]],label=labels[[i]],origin="producer",category=contract)),levels)
    if (length(spec$recode$map)) {
      targets<-unlist(spec$recode$levels,use.names=FALSE)
      target_labels<-unlist(spec$recode$labels %||% sub("^[^:]+:","",targets),use.names=FALSE)
      for (key in setdiff(targets,names(records))) records[[key]]<-list(id=key,label=target_labels[match(key,targets)],origin="recode",domain_only=TRUE,category=contract)
    }
    resolved <- .categoryResolveRecords(unname(records),complete=FALSE,identity=contract)
    levels<-resolved$keys;labels<-resolved$levels
  } else {
    levels<-spec$levels %||% if(length(spec$recode$map))spec$recode$levels else levels
    labels<-spec$recode$labels %||% labels
    if(!is.null(levels)&&identical(spec$missing$na,"level")) {levels<-c(levels,"special:missing");labels<-NULL}
    if(!is.null(levels)&&identical(spec$missing$unknown,"level")) {levels<-c(levels,"special:unknown");labels<-NULL}
  }
  if (semantic=="concept" && isFALSE(plan$options$translate_concepts)) type<-"integer"
  if (!type %in% c("factor","ordered_factor")) {levels<-NULL;labels<-NULL;spec$reference<-NULL}
  list(stable_key=key,name=key,meaning=if(is.null(variable))paste("Output",semantic,"role",key) else paste("Declared variable",key),
    semantic_type=semantic,r_type=type,storage_type="unresolved",unit=unit,
    levels=if(!is.null(levels))list(keys=levels,labels=labels,completeness="declared; open producers remain unresolved"),
    ordered=if(type %in% c("factor","ordered_factor"))type=="ordered_factor",reference=if(type %in% c("factor","ordered_factor"))resolved$reference %||% spec$reference %||% inherited$reference,
    contrasts=resolved$contrasts %||% if(type=="ordered_factor")list(kind="polynomial") else if(type=="factor")list(kind="treatment",reference=spec$reference),
    derivation=list(variable=variable,category_spec=spec),privacy_class=if(!is.null(variable$date_spec)||identical(variable$derived$kind,"repeated"))2L else "unresolved",
    aggregate_policy="Existing endpoint admission; unresolved for unrequested endpoints",
    missing_policy="Ordinary available NA follows the declaration; unavailable and undefined anchors remain NA",
    provenance=list(source="public plan; no observed schema or vocabulary lookup"),completeness="unresolved")
}

.codebookPlanSchemas <- function(plan) {
  schemas<-list();visiting<-character()
  build<-function(output) {
    if(any(startsWith(names(schemas) %||% character(),paste0(output,"/"))))return(invisible(NULL))
    if(output %in% visiting) .categoryReject("Cyclic output dependency.","codebook_metadata_missing")
    visiting<<-c(visiting,output);on.exit(visiting<<-setdiff(visiting,output))
    node<-plan$outputs[[output]];opts<-node$options %||% list();fmt<-node$format
    id<-if(identical(node$grain,"person"))c(person_id="character") else c(row_id="integer64",cohort_row_id="integer64",person_id="character")
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
      extra<-switch(fmt,wide=character(),membership=c(cohort_start_date="unresolved",cohort_end_date="unresolved"),
        dictionary=c(concept_id="factor",concept_name="character",domain_id="character",vocabulary_id="character",concept_class_id="character",source="character"),
        long=c(concept_id="factor",start_date="unresolved",end_date="unresolved"),
        intervals=c(interval_type="character",concept_id="factor",start_days_from_index="integer",end_days_from_index="integer"),
        person_period=c(period_start_days="integer",period_end_days="integer",days="integer"),character())
      add(output,c(if(fmt!="dictionary")id,extra),if(fmt=="dictionary")"none" else node$grain %||% "episode")
    }
    for(component in names(definitions)) {
      types<-definitions[[component]]
      rows<-lapply(names(types),function(key)origins[[component]][[key]] %||% .codebookPlanColumn(key,types[[key]],if(key %in% names(id))"key" else if(key=="concept_id")"concept" else "structural",plan=plan))
      names(rows)<-names(types)
      structural <- names(rows)
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
      for(map in list(node$number_columns,node$category_columns))for(key in names(map)) {
        v<-map[[key]];dep<-v$derived
        while(!is.null(dep$variable))dep<-dep$variable$derived
        if(is.null(dep$component)&&length(definitions)==1L||identical(dep$component,component))rows[[key]]<-.codebookPlanColumn(key,variable=v,defaults=plan$options$category_spec,plan=plan)
      }
      if (!is.null(node$columns) && grains[[component]] != "none") {
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
