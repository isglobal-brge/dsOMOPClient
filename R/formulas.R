# Four immutable client expansions. There is no formula opcode or R evaluator.
.formulaReject <- function(reason,path,remedy) {
  stop(structure(list(message=paste0(reason,": ",path,"; ",remedy),call=NULL,
    code="validation_error",reason=reason,path=path),class=c("omop_formula_error","error","condition")))
}
.formulaVersion <- function(version) {
  if(!identical(version,"1.0.0")) .formulaReject("formula_version_unsupported","/version","use installed exact version 1.0.0.")
}
.formulaCol <- function(x)list(col=x)
.formulaLit <- function(x)list(lit=x,as="numeric")
.formulaOp <- function(op,a,b)list(op=op,args=list(a,b))
.formulaIf <- function(test,yes,no=.formulaLit(NULL))list(ifelse=list(test=test,yes=yes,no=no))
.formulaTest <- function(x,op,value)list(var=x,op=op,value=value)
.formulaExpr <- function(expr)list(version=1L,assign=list(list(name="answer",expr=expr)))
.formulaDerive <- function(inputs,expr,unit,spec=NULL) {
  omop_number_derive(lapply(inputs,function(v)omop_number_input(v,"presented")),
    .formulaExpr(expr),unit,number_spec=spec)
}
.formulaUnit <- function(variable,expected=NULL,path="/input") {
  .numberVariableArg(variable)
  p<-tryCatch(recipe_to_plan(omop_recipe(variables=list(operand=variable)),version=2),error=function(e)NULL)
  if(is.null(p)) .formulaReject("formula_input_unresolved",path,"supply a bound self-contained typed variable with explicit source and selection.")
  unit<-.numberPresentedUnit(p,p$variables$operand)
  if(unit %in% c("unresolved_source","mixed_coordinates","categorical")||
    !is.null(expected)&&!unit %in% expected)
    .formulaReject("formula_unit_mismatch",path,"declare the documented canonical unit or an explicit numeric conversion before this formula.")
  unit
}
.formulaDefinitions <- function()list(
  bmi=list(id="dsomop.formula.bmi",version="1.0.0",equation="weight/(height*height)",
    input_units=list(weight="coordinate:kg",height="coordinate:metres"),unit="coordinate:kg_per_m2",
    eligibility="positive finite weight and height",meaning="body mass index"),
  map=list(id="dsomop.formula.map",version="1.0.0",equation="(systolic+2*diastolic)/3",
    input_units=list(systolic="coordinate:mmHg",diastolic="coordinate:mmHg"),unit="coordinate:mmHg",
    eligibility="positive finite pressures",meaning="usual arithmetic MAP approximation; not measured pressure"),
  ratio=list(id="dsomop.formula.ratio",version="1.0.0",equation="numerator/denominator",
    eligibility="finite operands; zero denominator and nonfinite arithmetic become NA; negative denominators allowed"),
  egfr_ckdepi2021=list(id="dsomop.formula.egfr_ckdepi2021",version="1.0.0",
    equation="142*min(Scr/k,1)^alpha*max(Scr/k,1)^(-1.200)*0.9938^age*sex_factor",
    female=list(k=.7,alpha=-.241,sex_factor=1.012),male=list(k=.9,alpha=-.302,sex_factor=1),
    log_age_factor=-0.0062192998139168326,unit="coordinate:ml_min_1_73m2",
    input_units=list(creatinine="coordinate:idms_creatinine_mg_dl",age="years"),
    eligibility="age >=18 completed calendar years at creatinine measurement; positive finite standardized creatinine; exactly one explicitly mapped sex branch",
    assay="IDMS standardized creatinine; public protocol assumption, not inferred from values",
    source="https://www.kidney.org/professionals/ckd-epi-creatinine-equation-2021",
    equation_version="CKD-EPI 2021 creatinine; race-free"))
.formulaStamp <- function(variable,id,details=list()) {
  definition<-.formulaDefinitions()[[id]]
  definition$sha256<-paste(openssl::sha256(charToRaw(.numberJson(definition))))
  attr(variable,"omop_formula")<-c(definition,list(binding=details,original_expansion_sha256=.formulaExpansionHash(variable),
    alignment="same person/consumer episode; same-event operands require exact one-to-one identity",
    stages="all dependencies explicitly presented; raw input declarations remain available"))
  variable
}
#' Versioned clinical formulas expanded into private numeric declarations
#'
#' @param weight,height Typed numeric variables in coordinate:kg and coordinate:metres.
#' @param systolic,diastolic Typed numeric variables in coordinate:mmHg.
#' @param numerator,denominator Aligned numeric declarations with resolved units.
#' @param unit Declared result coordinate for a ratio.
#' @param creatinine Typed selected creatinine in coordinate:idms_creatinine_mg_dl.
#' @param age Completed calendar years from omop_age_at at the creatinine anchor.
#' @param sex Admitted categorical variable whose source is justified by the study.
#' @param sex_map Named list female and male, each a nonempty vector of disjoint
#'   stable category identities; no missing, unknown or wildcard identities.
#' @param version Installed exact definition version, currently 1.0.0.
#' @return An ordinary numeric variable expanded into closed primitives, with
#'   immutable client formula provenance. No patient computation occurs here.
#' @details
#' Declare named physical coordinates with an explicit omop_number_unit conversion
#' from the source unit; for example micromol/L to IDMS creatinine mg/dL divides
#' by 88.4. No unit, source concept, sex interpretation or anchor is guessed.
#' All bindings use stage=presented so conversions and log/exp transforms survive.
#' Independently selected measurements do not imply contemporaneity. Invalid
#' inputs become private typed NA; unavailable sources retain unavailability.
#' Age requires completed calendar years, not annual_years or baseline age at
#' an unrelated visit. Use a first/last/nth measurement selector with missing=keep
#' and a matching start-date anchor, or a day-zero selector and age at index.
#' Put source-unit normalization on the shared event used by both creatinine
#' and its age anchor. An input-local source map must match that shared map;
#' keep explicit numeric coordinate conversions on the creatinine input.
#' Sex mapping is an explicit public protocol assertion, not an interpretation of
#' a field named gender. CKD-EPI 2021 uses no race operand and is an adult estimate.
#' @export
omop_formula_bmi <- function(weight,height,version="1.0.0") {
  .formulaVersion(version)
  .formulaUnit(weight,"coordinate:kg","/weight");.formulaUnit(height,"coordinate:metres","/height")
  good<-list(and=list(.formulaTest("w",">",0),.formulaTest("h",">",0)))
  expr<-.formulaIf(good,.formulaOp("/",.formulaCol("w"),.formulaOp("*",.formulaCol("h"),.formulaCol("h"))))
  .formulaStamp(.formulaDerive(list(w=weight,h=height),expr,"coordinate:kg_per_m2"),"bmi")
}
#' @rdname omop_formula_bmi
#' @export
omop_formula_map <- function(systolic,diastolic,version="1.0.0") {
  .formulaVersion(version)
  .formulaUnit(systolic,"coordinate:mmHg","/systolic");.formulaUnit(diastolic,"coordinate:mmHg","/diastolic")
  good<-list(and=list(.formulaTest("s",">",0),.formulaTest("d",">",0)))
  expr<-.formulaIf(good,.formulaOp("/",.formulaOp("+",.formulaCol("s"),.formulaOp("*",.formulaLit(2),.formulaCol("d"))),.formulaLit(3)))
  .formulaStamp(.formulaDerive(list(s=systolic,d=diastolic),expr,"coordinate:mmHg"),"map")
}
#' @rdname omop_formula_bmi
#' @export
omop_formula_ratio <- function(numerator,denominator,unit,version="1.0.0") {
  .formulaVersion(version);.numberUnitToken(unit)
  units<-list(numerator=.formulaUnit(numerator,path="/numerator"),denominator=.formulaUnit(denominator,path="/denominator"),result=unit)
  .formulaStamp(.formulaDerive(list(n=numerator,d=denominator),.formulaOp("/",.formulaCol("n"),.formulaCol("d")),unit),"ratio",units)
}
.formulaAgeCheck <- function(age,creatinine) {
  a<-age$derived;r<-creatinine$reducer;ref<-a$reference
  valid<-identical(a$kind,"age_at")&&identical(a$unit,"years")&&!isTRUE(a$bands)
  # A numerically transformed age is no longer the pinned calendar-age input,
  # even if its declared unit still says years. Keep only identity presentation.
  identity<-vapply(.numberEffective(age$number_spec)$pipeline,function(s) {
    s$kind=="transform"&&s$method=="identity" ||
      s$kind=="round"&&s$method=="none" ||
      s$kind=="fixed_unit"&&s$from=="years"&&s$to=="years"&&s$factor==1&&s$offset==0
  },logical(1))
  valid<-valid&&all(identity)
  at_index<-identical(ref,"index")&&identical(r$window$anchor,"index")&&
    identical(as.numeric(r$window$start),0)&&identical(as.numeric(r$window$end),0)
  # Source maps run before selection and can remove measurements. Comparing
  # only the original event declarations would then accept the wrong age.
  maps<-Filter(function(s)s$kind=="event_unit",.numberEffective(creatinine$number_spec)$pipeline)
  if(inherits(ref,"omop_anchor")&&length(maps)&&!all(vapply(maps,function(s)
    identical(.jcsCanonical(.v2_plain_value(s$spec)),.jcsCanonical(.v2_plain_value(ref$event$unit))),logical(1))))
    .formulaReject("formula_age_anchor","/age",
      "put source-unit normalization on the shared event used by creatinine and its age anchor; keep explicit numeric coordinate conversions on the creatinine input.")
  selected<-inherits(ref,"omop_anchor")&&r$fn %in% c("first_value","last_value","nth_value")&&
    identical(r$missing,"keep")&&identical(ref$field,"start")&&identical(ref$scope,"episode")&&
    identical(ref$which,switch(r$fn,first_value="first",last_value="last",nth_value="nth"))&&
    identical(.v2_plain_value(ref$event),.v2_plain_value(creatinine$event))&&
    identical(.v2_plain_value(ref$search),.v2_plain_value(r$window))&&
    (r$fn!="nth_value"||identical(ref$n,r$n))
  if(!isTRUE(valid&&(at_index||selected))) .formulaReject("formula_age_anchor","/age",
    "declare completed calendar years at the selected creatinine measurement anchor; align the selector/window explicitly.")
}
#' @rdname omop_formula_bmi
#' @export
omop_formula_egfr_ckdepi2021 <- function(creatinine,age,sex,sex_map,version="1.0.0") {
  .formulaVersion(version)
  .formulaUnit(creatinine,"coordinate:idms_creatinine_mg_dl","/creatinine")
  .formulaUnit(age,"years","/age");.formulaAgeCheck(age,creatinine)
  if(!inherits(sex,"omop_variable")||!is.list(sex_map)||anyDuplicated(names(sex_map))||
    !setequal(names(sex_map),c("female","male"))) .formulaReject("formula_sex_map","/sex_map","declare disjoint nonempty female and male stable category keys.")
  keys<-lapply(sex_map,function(x)unlist(x,use.names=FALSE))
  if(any(!vapply(keys,function(x)is.character(x)&&length(x)>0L&&!anyNA(x)&&!anyDuplicated(x),logical(1)))||
    length(intersect(keys$female,keys$male))||any(grepl("^special:|^concept:0$|[*]",unlist(keys))))
    .formulaReject("formula_sex_map","/sex_map","use disjoint known identities without unknown/missing sentinels or wildcards.")
  for(k in keys)tryCatch(.categoryKeys(k,FALSE),error=function(e).formulaReject("formula_sex_map","/sex_map","use canonical category identities."))
  indicator<-function(keys) {
    vars<-omop_category_indicators(sex,setNames(keys,paste0("i",seq_along(keys))),missing="propagate")
    terms<-lapply(names(vars),.formulaCol)
    .formulaDerive(unclass(vars),Reduce(function(a,b).formulaOp("+",a,b),terms),"1")
  }
  female<-indicator(keys$female);male<-indicator(keys$male)
  valid<-list(and=list(.formulaTest("scr",">",0),.formulaTest("age",">=",18),
    list(or=list(list(and=list(.formulaTest("f","==",1),.formulaTest("m","==",0))),
                 list(and=list(.formulaTest("f","==",0),.formulaTest("m","==",1)))))))
  q<-.formulaDerive(list(scr=creatinine,age=age,f=female,m=male),
    .formulaIf(valid,.formulaOp("/",.formulaCol("scr"),.formulaIf(.formulaTest("f","==",1),.formulaLit(.7),.formulaLit(.9)))),"1")
  branch<-function(op).formulaDerive(list(q=q),.formulaIf(list(var="q",op="not_null"),.formulaIf(.formulaTest("q",op,1),.formulaCol("q"),.formulaLit(1))),"1",
    omop_number_spec(transform=omop_number_transform("log")))
  alpha<-.formulaIf(.formulaTest("f","==",1),.formulaLit(-.241),.formulaLit(-.302))
  sum<-.formulaOp("+",.formulaOp("+",.formulaOp("*",alpha,.formulaCol("lo")),
    .formulaOp("*",.formulaLit(-1.2),.formulaCol("hi"))),
    .formulaOp("*",.formulaCol("age"),.formulaLit(.formulaDefinitions()$egfr_ckdepi2021$log_age_factor)))
  exponent<-.formulaDerive(list(lo=branch("<="),hi=branch(">="),age=age,f=female),sum,"1",
    omop_number_spec(transform=omop_number_transform("exp")))
  value<-.formulaDerive(list(e=exponent,f=female),.formulaOp("*",.formulaOp("*",.formulaLit(142),.formulaCol("e")),
    .formulaIf(.formulaTest("f","==",1),.formulaLit(1.012),.formulaLit(1))),"coordinate:ml_min_1_73m2")
  .formulaStamp(value,"egfr_ckdepi2021",list(sex_map=keys,sex_source_assertion="study explicitly maps this categorical source to equation sex",
    age_convention="completed calendar years at creatinine measurement"))
}

.formulaExpansionHash <- function(variable) {
  attr(variable,"omop_formula")<-NULL;variable$name<-NULL
  paste(openssl::sha256(charToRaw(.numberJson(.v2_plain_value(variable)))))
}
.formulaProvenance <- function(variable) {
  p<-attr(variable,"omop_formula")
  if(is.null(p))return(NULL)
  p$expansion_sha256<-.formulaExpansionHash(variable)
  p$derivative<-!identical(p$expansion_sha256,p$original_expansion_sha256)
  p
}
.formulaRestore <- function(variable,provenance) {
  .numberVariableArg(variable)
  definitions<-.formulaDefinitions()
  ids<-vapply(definitions,function(x)x$id,"")
  at<-match(provenance$id,ids)
  if(length(at)!=1L||is.na(at)||!identical(provenance$version,"1.0.0"))
    .formulaReject("formula_provenance_invalid","/provenance","use an installed formula definition.")
  expected<-.formulaStamp(variable,names(definitions)[at],provenance$binding)
  immutable<-c(names(definitions[[at]]),"sha256","alignment","stages")
  if(!identical(.numberJson(provenance[immutable]),.numberJson(attr(expected,"omop_formula")[immutable]))||
    length(setdiff(names(provenance),c(immutable,"binding","original_expansion_sha256","expansion_sha256","derivative"))))
    .formulaReject("formula_provenance_invalid","/provenance","retain the immutable definition and record edits as derivatives.")
  p<-attr(expected,"omop_formula");p$original_expansion_sha256<-provenance$original_expansion_sha256
  attr(variable,"omop_formula")<-p
  variable
}
.formulaInventory <- function(x) {
  out<-list()
  walk<-function(v,path) {
    p<-.formulaProvenance(v)
    if(!is.null(p))out[[path]]<<-p
    if(is.list(v))for(i in seq_along(v))walk(v[[i]],paste0(path,"/",if(!is.null(names(v)))names(v)[i] else i))
  }
  walk(x,"");out
}
