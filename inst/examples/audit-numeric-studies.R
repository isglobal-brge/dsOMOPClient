# dsOMOPClient 2.10.5 native language-2 teaching declarations.
# Source this file on the CLIENT after library(dsOMOPClient). No patient data,
# server symbols, network calls or arbitrary server functions are used here.
audit_windows <- function(pre_index=FALSE) {
  end<-if(pre_index)-1L else 0L
  list(long_term=omop_window(-365L,end),medium_term=omop_window(-180L,end),
       short_term=omop_window(-30L,end))
}
audit_expression <- function(inputs,expr,unit,reducer=NULL,number_spec=NULL) {
  omop_number_derive(lapply(inputs,function(v)omop_number_input(v,"presented")),
    list(version=1L,assign=list(list(name="answer",expr=expr))),unit,
    reducer=reducer,number_spec=number_spec)
}
audit_numeric_studies <- function(measure,observation=NULL) {
  col<-function(x)list(col=x);lit<-function(x)list(lit=x,as="numeric")
  op<-function(o,a,b)list(op=o,args=list(a,b))
  choose<-function(test,yes,no=lit(NULL))list(ifelse=list(test=test,yes=yes,no=no))
  selected<-function(fn,start,end)omop_variable(event=measure,reducer=omop_reduce(fn,window=omop_window(start,end)))
  rows<-omop_number_rows(measure)
  # Q16 Independent closest visits: day zero within [-30,30], and the last
  # visit in [150,210]. A six-month window is not a contemporaneous pair.
  baseline<-selected("closest_value",-30L,30L)
  followup<-selected("last_value",150L,210L)
  difference<-audit_expression(list(b=baseline,f=followup),op("-",col("f"),col("b")),"unit:8554")
  ratio<-audit_expression(list(b=baseline,f=followup),op("/",col("f"),col("b")),"1")
  percent<-audit_expression(list(b=baseline,f=followup),op("*",lit(100),op("/",op("-",col("f"),col("b")),col("b"))),"1")
  # Q17 Each eligible nonmissing observation weighs once. Censored handling
  # and units belong to the supplied event, not a guessed clinical policy.
  inrange<-choose(list(and=list(list(var="x",op=">=",value=4),list(var="x",op="<=",value=6))),lit(1),lit(0))
  flag<-choose(list(var="x",op="not_null"),inrange)
  fraction<-audit_expression(list(x=rows),flag,"1",omop_reduce("mean"))
  above<-audit_expression(list(x=rows),choose(list(var="x",op="not_null"),choose(list(var="x",op=">",value=6),lit(1),lit(0))),"count",omop_reduce("sum"))
  # Q18 Day-summary research rule; no 48-hour or index-row comparison claim.
  prior<-selected("min",-7L,-1L);current<-selected("max",0L,0L)
  day_ratio<-audit_expression(list(prior=prior,current=current),op("/",col("current"),col("prior")),"1")
  # Q19 Analyst-declared bounds, before reduction. No automatic correction.
  clean<-choose(list(var="x",op="<=",value=250),col("x"))
  clean_mean<-audit_expression(list(x=rows),clean,"unit:8554",omop_reduce("mean"))
  clip_mean<-audit_expression(list(x=omop_number(rows,omop_number_spec(transform=omop_number_transform("clip",upper=250)))),col("x"),"unit:8554",omop_reduce("mean"))
  raw<-selected("mean",NULL,NULL)
  after_mean<-audit_expression(list(x=raw),clean,"unit:8554")
  out<-list(baseline=baseline,followup=followup,difference=difference,ratio=ratio,percent_change=percent,
    observation_fraction=fraction,above_count=above,previous_days_min=prior,current_day_max=current,day_ratio=day_ratio,
    raw_mean=raw,clean_mean=clean_mean,clip_mean=clip_mean,after_mean=after_mean)
  if(!is.null(observation)) {
    other<-omop_variable(event=observation,reducer=omop_reduce("last_value"))
    out$fallback<-audit_expression(list(primary=selected("last_value",NULL,NULL),secondary=other),
      list(coalesce=list(col("primary"),col("secondary"))),"unit:8554")
  }
  out
}
# Q20 Equality with an explicitly rounded sibling establishes integer validity
# before ordinal bins. Missing, out-of-range and noninteger values become NA.
audit_pain_scale <- function(value) {
  rounded<-omop_number(value,omop_number_spec(rounding=omop_number_round("decimals",digits=0L)))
  difference<-audit_expression(list(x=value,r=rounded),list(op="-",args=list(list(col="x"),list(col="r"))),"1")
  valid<-list(and=list(list(var="d",op="==",value=0),list(var="x",op=">=",value=0),list(var="x",op="<=",value=10)))
  numeric<-audit_expression(list(x=value,d=difference),list(ifelse=list(test=valid,yes=list(col="x"),no=list(lit=NULL,as="numeric"))),"1")
  list(numeric=numeric,ordinal=omop_number_bins(numeric,breaks=seq(-.5,10.5,1),labels=as.character(0:10)))
}
