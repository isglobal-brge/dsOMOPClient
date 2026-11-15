# 20\. Case study: celecoxib versus diclofenac

**What you will learn**

Find the study concepts, define mutually exclusive new-user entry,
account for censoring, and fit a pooled adjusted constant-hazard
survival model.

**You need:** [the installed packages and practice
resources](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
[concept
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
and the preceding tutorial chapters. Run these sections in order in a
fresh `book-work` session. All observations are public synthetic
teaching data; the numerical results are not clinical evidence.

## Research question

Among people beginning celecoxib or diclofenac after a year without
either drug, how does first GI bleeding during the following year
differ? We adjust for age. This is a methods exercise; age adjustment
alone does not identify a causal drug effect.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
pair <- book_numeric_pair(resource = "gibleed")
two_conns <- pair$conns
names(two_conns)
```

    #> [1] "north" "south"

The helper partitions public GiBleed people into two disjoint sites. It
never splits or downloads a real site’s records through an analyst
connection.

## Find the drugs and outcome

``` r
ce <- ds.omop.concept.search("celecoxib", domain = "Drug", conns = two_conns, symbol = "book_pool")$pooled
di <- ds.omop.concept.search("diclofenac", domain = "Drug", conns = two_conns, symbol = "book_pool")$pooled
gi <- ds.omop.concept.search("Gastrointestinal hemorrhage", domain = "Condition", conns = two_conns, symbol = "book_pool")$pooled
knitr::kable(rbind(ce, di, gi)[c("concept_name", "standard_concept", "concept_class_id")])
```

| concept\_name                                       | standard\_concept | concept\_class\_id |
| :-------------------------------------------------- | :---------------- | :----------------- |
| celecoxib 200 MG Oral Capsule \[Celebrex\]          | S                 | Branded Drug       |
| celecoxib                                           | S                 | Ingredient         |
| Diclofenac Sodium 75 MG Delayed Release Oral Tablet | S                 | Clinical Drug      |
| Diclofenac                                          | S                 | Ingredient         |
| Gastrointestinal hemorrhage                         | S                 | Clinical Finding   |

``` r
ceid <- ce$concept_id[trimws(ce$concept_name) == "celecoxib"]
diid <- di$concept_id[trimws(di$concept_name) == "Diclofenac"]
giid <- gi$concept_id[trimws(gi$concept_name) == "Gastrointestinal hemorrhage"]
```

Select the standard ingredients and standard outcome, then inspect
descendants. The source products map into these ingredient hierarchies;
ingredient names alone would not select every product record.

``` r
ce_children <- ds.omop.concept.descendants(ceid, conns = two_conns, symbol = "book_pool")$pooled
knitr::kable(ce_children["concept_name"])
```

| concept\_name |
| :------------ |
| celecoxib     |

``` r
map_rows <- ds.omop.concept.relationships(ceid, relationship_id = "Maps to", conns = two_conns, symbol = "book_pool")$pooled
print(data.frame(mapping_rows = NROW(map_rows)))
```

    #>   mapping_rows
    #> 1            0

``` r
knitr::kable(ds.omop.concept.prevalence("drug_exposure", top_n = 10, scope = "pooled",
  conns = two_conns, symbol = "book_pool")$pooled)
```

| concept\_id | n\_persons | concept\_name                                                                                                     |
| ----------: | ---------: | :---------------------------------------------------------------------------------------------------------------- |
|    40213227 |       2660 | tetanus and diphtheria toxoids, adsorbed, preservative free, for adult use                                        |
|     1127433 |       2580 | Acetaminophen 325 MG Oral Tablet                                                                                  |
|    40213160 |       2140 | poliovirus vaccine, inactivated                                                                                   |
|     1713671 |       2015 | Amoxicillin 250 MG / Clavulanate 125 MG Oral Tablet                                                               |
|    19059056 |       1920 | Aspirin 81 MG Oral Tablet                                                                                         |
|     1118084 |       1840 | celecoxib                                                                                                         |
|    40213296 |       1735 | hepatitis A vaccine, adult dosage                                                                                 |
|    40213306 |       1555 | hepatitis B vaccine, adult dosage                                                                                 |
|     1127078 |       1420 | Acetaminophen 160 MG Oral Tablet                                                                                  |
|    40229134 |       1385 | Acetaminophen 21.7 MG/ML / Dextromethorphan Hydrobromide 1 MG/ML / doxylamine succinate 0.417 MG/ML Oral Solution |

``` r
study_sets <- list(celecoxib = omop_concept_set(ceid, descendants = TRUE),
  diclofenac = omop_concept_set(diid, descendants = TRUE),
  bleeding = omop_concept_set(giid, descendants = TRUE))
```

## Define entry and washout

Choose first eligible exposure to either study drug. Require 365
observed days and no exposure to either during the preceding year.
Exclude simultaneous starts so each person’s baseline treatment is
unambiguous. Later switching does not change baseline treatment: this is
an initial-treatment follow-up definition.

``` r
ce_event <- omop_event("drug_exposure", study_sets$celecoxib, name = "celecoxib")
di_event <- omop_event("drug_exposure", study_sets$diclofenac, name = "diclofenac")
bleed <- omop_event("condition_occurrence", study_sets$bleeding)
users <- omop_population("users", index = omop_index(omop_any_of(ce_event, di_event),
  select = "first_eligible"), filters = list(omop_observed(before = 365),
  omop_has_not(ce_event, window = omop_window(-365, -1)),
  omop_has_not(di_event, window = omop_window(-365, -1)),
  omop_not(omop_all(omop_has(ce_event, window = omop_window(0, 0)),
    omop_has(di_event, window = omop_window(0, 0))))))
```

## Join baseline variables to censored follow-up

One row represents one person’s first eligible episode. Only the joined
analysis frame is published; its dependencies remain private
intermediates.

``` r
comparison <- omop_recipe(populations = list(users),
  variables = list(age = omop_age(), celecoxib = omop_variable(event = ce_event,
    reducer = omop_reduce("binary", window = omop_window(0, 0)))),
  outputs = list(omop_output("baseline", rows = "users", format = "wide"),
    omop_output("survival", rows = "users", format = "survival", options = list(
      outcomes = list(bleed = bleed), tar = omop_window(1, 365),
      censor = list("observation_end", "death"))),
    omop_output("analysis", rows = "users", format = "joined", options = list(
      from = list(list(output = "baseline"), list(output = "survival")),
      how = "inner", cardinality = "one_to_one"))), publish = "analysis")
cat(grep("censor|follow|survival", recipe_explain(comparison)$text, value = TRUE), sep = "\n")
```

    #> Output 'survival' [survival] rows 'users', grain episode - private input, never published
    #> variables.celecoxib: concept origin follows reducer binary over the declared source value; explicit variable name is retained.

``` r
book_symbols(recipe_execute(comparison, out = c(analysis = "D_comparison"), conns = two_conns, symbol = "book_pool"))
```

    #>       analysis 
    #> "D_comparison"

``` r
dsBaseClient::ds.colnames("D_comparison", datasources = two_conns)
```

    #> $north
    #>  [1] "row_id"                "cohort_row_id"         "person_id"            
    #>  [4] "age"                   "celecoxib"             "outcome_name"         
    #>  [7] "event"                 "entry_days_from_index" "exit_days_from_index" 
    #> [10] "follow_up_days"       
    #> 
    #> $south
    #>  [1] "row_id"                "cohort_row_id"         "person_id"            
    #>  [4] "age"                   "celecoxib"             "outcome_name"         
    #>  [7] "event"                 "entry_days_from_index" "exit_days_from_index" 
    #> [10] "follow_up_days"

The censor list and 365-day window define the estimand. Event = 0 means
censored within that follow-up, not “never bleeds”. Person rows remain
private at each site.

## Fit an adjusted survival model with dsBase

An exponential survival model assumes a constant event hazard during
follow-up. Its censored likelihood can be fitted by Poisson regression
of the event indicator with log(person-years) as an offset. The
coefficient of celecoxib is a log hazard ratio against diclofenac,
conditional on age and this model. This is not a Cox fit.

``` r
print(ds.omop.mutate("D_comparison", years = follow_up_days / 365.25,
  newobj = "D_comparison_model", conns = two_conns, symbol = "book_pool"))
```

    #> [1] "D_comparison_model"

The installed dsBase uses one-iteration local starting fits and may
print the known setup convergence warning. Check the final distributed
iteration count, diagnostics and coefficients; an unexplained final
failure is not a result.

``` r
dsBaseClient::ds.log("D_comparison_model$years", newobj = "log_years", datasources = two_conns)
comparison_fit <- dsBaseClient::ds.glm("event ~ celecoxib + age",
  data = "D_comparison_model", family = "poisson", offset = "log_years", datasources = two_conns)
```

    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge
    #> Warning: glm.fit: algorithm did not converge

``` r
knitr::kable(comparison_fit$coefficients)
```

|             |    Estimate | Std. Error |    z-value |   p-value | low0.95CI.LP | high0.95CI.LP | EXPONENTIATED RR | low0.95CI.EXP | high0.95CI.EXP |
| :---------- | ----------: | ---------: | ---------: | --------: | -----------: | ------------: | ---------------: | ------------: | -------------: |
| (Intercept) | \-2.4632199 |  0.5396061 | \-4.564848 | 0.0000050 |  \-3.5208285 |   \-1.4056112 |        0.0851603 |     0.0295749 |      0.2452171 |
| celecoxib   |   0.3217293 |  0.1043243 |   3.083934 | 0.0020428 |    0.1172574 |     0.5262012 |        1.3795113 |     1.1244088 |      1.6924906 |
| age         |   0.0185955 |  0.0137416 |   1.353233 | 0.1759812 |  \-0.0083374 |     0.0455285 |        1.0187695 |     0.9916972 |      1.0465808 |

Synthetic data can show an association even without a real biological
mechanism. Confounding by indication, selection, outcome validity and
informative censoring all need a study design; federating a model does
not remove those problems.

## Exercises

**1.** Count observed outcomes and censored episodes across the two
sites.

Solution 1

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_comparison$event", datasources = two_conns)))
```

| Category | Count |
| :------- | ----: |
| 0        |  2151 |
| 1        |   479 |
| NA       |     0 |

Each person enters at most once. Do not add counts from overlapping drug
cohorts.

**2.** Express the treatment coefficient as a hazard ratio.

Solution 2

``` r
exp(comparison_fit$coefficients["celecoxib", "Estimate"])
```

    #> [1] 1.379511

This exponentiates a released coefficient, not a patient value.

**3.** Confirm the final model reports no site errors and stopped before
its limit.

Solution 3

``` r
comparison_fit[c("Nvalid", "Nmissing", "iter", "errorMessage")]
```

    #> $Nvalid
    #> [1] 2630
    #> 
    #> $Nmissing
    #> [1] 0
    #> 
    #> $iter
    #> [1] 7
    #> 
    #> $errorMessage
    #>       ERROR MESSAGES
    #> north "No errors"   
    #> south "No errors"

Compare the valid sample with the intended episode population. Missing
covariates can change it even when extraction succeeded.

## Recap

Find standard ingredients and their descendants, make new-user entry
mutually exclusive, declare time at risk and censoring, then pool a
model with an explicit survival assumption. Protected tables and model
diagnostics remain part of interpretation.

**Next:** [21. Case study: glucose
trajectories](https://isglobal-brge.github.io/dsOMOPClient/articles/book-21-case-glucose.md).
