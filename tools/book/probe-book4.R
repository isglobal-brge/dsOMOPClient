library(dsOMOPClient)
source('tools/book/connection.R');source('tools/book/practice.R')
p<-book_practice('gibleed'); conns<-p$conns
print(ds.omop.concept.lookup(317009L,conns=conns)$pooled)
print(names(recipe_to_plan(omop_recipe(variables=list(x=omop_age())))$outputs))
for (term in c('celecoxib','diclofenac','gastrointestinal hemorrhage','antiinflammatory')) {
 print(term);print(ds.omop.concept.search(term,standard_only=FALSE,conns=conns)$pooled)
}
ds.omop.disconnect(conns=conns);DSI::datashield.logout(conns)
p<-book_practice('numeric');conns<-p$conns
for (term in c('Glucose','Creatinine','Systolic','Diastolic','Body weight','Body height')) {
 print(term);print(ds.omop.concept.search(term,conns=conns)$pooled)
}
print(ds.omop.concept.prevalence('measurement',top_n=50,scope='pooled',conns=conns)$pooled)
print(grep('Surv|surv|cox',rownames(installed.packages()),value=TRUE))
for(pkg in c('dsBaseClient','dsSurvivalClient')) if(requireNamespace(pkg,quietly=TRUE)) print(grep('cox|Surv|rbind|glm',getNamespaceExports(pkg),value=TRUE))
print(ds.omop.concept.search(NULL,vocabulary='ATC',standard_only=FALSE,conns=conns)$pooled)
