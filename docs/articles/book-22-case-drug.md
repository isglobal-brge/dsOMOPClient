# 22\. Case study: drug utilisation

**What you will learn**

Select a drug class, distinguish stockpiled supply from persistence, and
calculate proportion of days covered with an explicit follow-up
denominator.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.questionnaire_drugs`, then `omop.gibleed`.

## Research question and supply patterns

For new users of a drug class, what fraction of the first year is
covered by recorded supply, and when does the first treatment era end?
The questionnaire example has three synthetic supply patterns, with 12
people per pattern:

| Pattern                          | Fill days from entry | Days supplied per fill |
| -------------------------------- | -------------------- | ---------------------- |
| Continuous, with early refills   | 0, 100, 220          | 120, 120, 125          |
| Two overlapping fills, then stop | 0, 30                | 60, 60                 |
| Short course, then restart       | 0, 120               | 30, 30                 |

Stockpiling shifts overlapping supply later. For the first pattern, the
raw union of fill intervals covers 345 days, but carrying unused supply
forward covers 365. A gap over 30 days starts a new era. Predict covered
days and first-era duration before examining the summaries.

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.questionnaire_drugs", server = "site")$conns
```

## Find a class and its descendants

``` r
classes <- ds.omop.concept.search(NULL, vocabulary = "ATC", standard_only = FALSE, datasources = conns)$pooled
classes[c("concept_id", "concept_name", "concept_class_id", "standard_concept")]
```

| concept\_id | concept\_name             | concept\_class\_id | standard\_concept |
| ----------: | :------------------------ | :----------------- | :---------------- |
|      960001 | Teaching ATC first level  | ATC 1st            | C                 |
|      960002 | Teaching ATC second level | ATC 2nd            | C                 |

``` r
class_id <- classes$concept_id[classes$concept_class_id == "ATC 2nd"]
children <- ds.omop.concept.descendants(class_id, datasources = conns)$pooled
children[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name                |
| ----------: | :--------------------------- |
|      950001 | Questionnaire study medicine |

``` r
ds.omop.concept.prevalence("drug_exposure", type = "both", datasources = conns)$pooled
```

| concept\_id | concept\_name                | n\_persons | n\_records |
| ----------: | :--------------------------- | ---------: | ---------: |
|      950001 | Questionnaire study medicine |         35 |         80 |

``` r
class_set <- omop_concept_set(class_id, descendants = TRUE)
saveRDS(class_set, "utilisation-class.rds")
```

ATC classification concepts have standard status C, so the search
includes nonstandard entries. Select second-level class **960002**. Its
synthetic hierarchy selects **950001**, Questionnaire study medicine.
The occurrence band 35 means 35–39 exposed people; the analysis below
has 36. Class membership comes from the hierarchy, not a drug’s spelling
or a Maps to substitution.

## Define eligible time and supply

Require a 365-day washout and observed follow-up through day 364: days
0–364 contain 365 eligible days. This complete-follow-up restriction
simplifies the denominator but can select people with more stable
observation. A different study may instead use variable observed
follow-up.

``` r
users <- omop_new_users(class_set, washout_days = 365)
drug <- omop_event("drug_exposure", class_set)
era <- omop_era(drug, gap = 30, by = "ingredient", stockpile = TRUE)
covered <- omop_variable(event = era, reducer = omop_reduce("covered_days",
  window = omop_window(0, 364), coverage = "effective_supply"))
persistence <- omop_variable(event = era, reducer = omop_reduce("first_era_days",
  window = omop_window(0, 364)))
study <- omop_study(users, outputs = "baseline", require = list(followup = 364),
  baseline = list(covered = covered, persistence = persistence,
    discontinued = omop_number_flag(persistence, "lt", 365)))
recipe_execute(study, out = c(baseline = "D_supply"), datasources = conns)
ds.omop.mutate("D_supply", pdc = covered / 365, newobj = "D_use", datasources = conns)
dsBaseClient::ds.summary("D_use$pdc", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #>
    #> $site$length
    #> [1] 36
    #>
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean
    #> 0.1643836 0.1643836 0.1643836 0.3287671 1.0000000 1.0000000 1.0000000 0.4977169

Mean PDC is **0.4977** across 36 people: (365 + 120 + 60)/(3 × 365). PDC
is a dimensionless fraction of eligible days. It measures recorded
supply, not ingestion. This class contains one ingredient; a class with
several would also need a rule about whether switching preserves
persistence.

``` r
dsBaseClient::ds.table("D_use$covered",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_use$covered
    #>  60 120 365  NA
    #>  12  12  12   0

Twelve people each have 60, 120 and 365 covered days, matching the three
patterns. The restarted course contributes 60 covered days across two
eras, even though its first era lasts only 30 days.

## Exercises

**1. Compare persistence with coverage.** Summarise first-era duration.
Explain why it differs from total covered days for the restarted course.

Solution 1

``` r
dsBaseClient::ds.table("D_use$persistence",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_use$persistence
    #>  30 120 365  NA
    #>  12  12  12   0

First eras last 30, 120 and 365 days, with 12 people in each and mean
**171.67** days. The gap before the restart exceeds 30 days, so the
second course does not extend the first era. A tolerated gap can add to
an era’s span without adding covered supply days.

**2. Interpret discontinuation.** How many first eras end before the
first year is complete? Does this prove permanent cessation?

Solution 2

``` r
dsBaseClient::ds.table("D_use$discontinued",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_use$discontinued
    #>  0  1 NA
    #> 12 24  0

There are **24** indicators of one and **12** of zero. The definition
says the first era ended before day 365 under this gap rule. It permits
restarting, as the third pattern demonstrates; it does not prove
permanent cessation.

## Check feasibility before applying this to another resource

GiBleed has no ATC hierarchy, but
[chapter 20](https://isglobal-brge.github.io/dsOMOPClient/articles/book-20-case-gibleed.html#find-the-drugs-and-outcome)
identifies celecoxib ingredient **1118084**. Check what its available
exposure spans can support.

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.gibleed", datasources = conns)
```

``` r
hits <- ds.omop.concept.search("celecoxib", domain = "Drug", vocabulary = "RxNorm", datasources = conns)$pooled
hits[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name                              | standard\_concept |
| ----------: | :----------------------------------------- | :---------------- |
|     1118088 | celecoxib 200 MG Oral Capsule \[Celebrex\] | S                 |
|     1118084 | celecoxib                                  | S                 |

``` r
ingredient <- hits$concept_id[trimws(hits$concept_name) == "celecoxib"]
```

Select the standard celecoxib ingredient by its exact name. An
ingredient analysis does not become a class analysis merely by giving it
an ATC label.

``` r
gibleed_drug <- omop_event("drug_exposure", ingredient)
gibleed_era <- omop_era(gibleed_drug, gap = 30, by = "ingredient", stockpile = TRUE)
feasibility <- omop_study(omop_new_users(ingredient, washout_days = 365),
  outputs = "baseline", require = list(followup = 364), baseline = list(
    covered = omop_variable(event = gibleed_era, reducer = omop_reduce("covered_days",
      window = omop_window(0, 364), coverage = "effective_supply"))))
recipe_execute(feasibility, out = c(baseline = "D_feasibility"), datasources = conns)
dsBaseClient::ds.summary("D_feasibility$covered", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 1745
    #>
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean
    #>    1    1    1    1    1    1    1    1

Every covered-days summary is one day because the source has zero days
supplied and equal exposure start/end dates. These records cannot
establish longitudinal adherence or discontinuation, so a PDC conclusion
would be unsupported.

## Recap

Establish vocabulary and supply-history feasibility, then define
follow-up, stockpiling and gaps. Covered days, first-era persistence and
discontinuation answer different questions even when derived from the
same prescriptions.

**Next:** [23. Case study: describing the same population across two
sites](https://isglobal-brge.github.io/dsOMOPClient/articles/book-23-case-table1.md).
