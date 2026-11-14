library(dsOMOPClient);source('tools/book/connection.R');source('tools/book/practice.R')
pair<-book_numeric_pair(resource='gibleed');two_conns<-pair$conns
ce<-ds.omop.concept.search('celecoxib',domain='Drug',conns=two_conns,symbol='book_pool')$pooled
ceid<-ce$concept_id[trimws(ce$concept_name)=='celecoxib']
di<-ds.omop.concept.search('diclofenac',domain='Drug',conns=two_conns,symbol='book_pool')$pooled
diid<-di$concept_id[trimws(di$concept_name)=='Diclofenac']
gi<-ds.omop.concept.search('Gastrointestinal hemorrhage',domain='Condition',conns=two_conns,symbol='book_pool')$pooled
giid<-gi$concept_id[trimws(gi$concept_name)=='Gastrointestinal hemorrhage']
bleed<-omop_event('condition_occurrence',omop_concept_set(giid,descendants=TRUE))
ce_event<-omop_event('drug_exposure',omop_concept_set(ceid,descendants=TRUE),name='celecoxib')
di_event<-omop_event('drug_exposure',omop_concept_set(diid,descendants=TRUE),name='diclofenac')
pop<-omop_population('users',index=omop_index(omop_any_of(ce_event,di_event),select='first_eligible'),filters=list(omop_observed(before=365),omop_has_not(ce_event,window=omop_window(-365,-1)),omop_has_not(di_event,window=omop_window(-365,-1)),omop_not(omop_all(omop_has(ce_event,window=omop_window(0,0)),omop_has(di_event,window=omop_window(0,0))))))
r<-omop_recipe(populations=list(pop),variables=list(age=omop_age(),celecoxib=omop_variable(event=ce_event,reducer=omop_reduce('binary',window=omop_window(0,0)))),outputs=list(omop_output('baseline',rows='users',format='wide'),omop_output('survival',rows='users',format='survival',options=list(outcomes=list(bleed=bleed),tar=omop_window(1,365),censor=list('observation_end','death'))),omop_output('analysis',rows='users',format='joined',options=list(from=list(list(output='baseline'),list(output='survival')),how='inner',cardinality='one_to_one'))),publish='analysis')
print(recipe_execute(r,out=c(analysis='D_comparison'),conns=two_conns,symbol='book_pool'))
print(dsBaseClient::ds.colnames('D_comparison',datasources=two_conns))
ds.omop.mutate('D_comparison',years=follow_up_days/365.25,newobj='D_model',conns=two_conns,symbol='book_pool')
dsBaseClient::ds.log('D_model$years',newobj='log_years',datasources=two_conns)
fit<-dsBaseClient::ds.glm('event ~ celecoxib + age',data='D_model',family='poisson',offset='log_years',datasources=two_conns)
print(fit$coefficients)
