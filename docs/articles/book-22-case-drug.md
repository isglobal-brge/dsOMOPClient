# 22\. Case study: drug utilisation

**What you will learn**

Separate an ATC-class definition from an ingredient study, define new
users, and measure covered days, persistence and discontinuation.

**You need:** [the installed packages and practice
resources](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
[concept
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
and the preceding tutorial chapters. Run these sections in order in a
fresh `book-work` session. All observations are public synthetic
teaching data; the numerical results are not clinical evidence.

## Research question and vocabulary feasibility

For a prespecified ATC class, what fraction of the first treatment year
is covered, and when does the first treatment era end? First establish
that the vocabulary supports that class. The small GiBleed vocabulary
has no ATC entries; an ATC-class estimate is therefore unavailable on
this practice resource. The executable worked analysis below is
explicitly a **celecoxib ingredient** study instead. It teaches the
mechanics without relabelling an ingredient as an ATC class.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
source("tools/book/case-helpers.R")
practice <- book_connect("gibleed")
conns <- practice$conns
atc <- ds.omop.concept.search(NULL, vocabulary = "ATC", standard_only = FALSE, conns = conns)$pooled
print(data.frame(ATC_classes_available = NROW(atc)))
```

    #>   ATC_classes_available
    #> 1                     0

On an ATC-enabled resource, search the intended class with
`standard_only = FALSE`, inspect its class level and descendants, check
prevalence, then save that set. Do not infer an ATC hierarchy from a
drug’s spelling.

## Find the executable ingredient set

``` r
hits <- ds.omop.concept.search("celecoxib", domain = "Drug", vocabulary = "RxNorm", conns = conns)$pooled
knitr::kable(hits[c("concept_name", "standard_concept", "concept_class_id")])
```

| concept\_name                              | standard\_concept | concept\_class\_id |
| :----------------------------------------- | :---------------- | :----------------- |
| celecoxib 200 MG Oral Capsule \[Celebrex\] | S                 | Branded Drug       |
| celecoxib                                  | S                 | Ingredient         |

``` r
ingredient_id <- hits$concept_id[trimws(hits$concept_name) == "celecoxib"]
knitr::kable(ds.omop.concept.descendants(ingredient_id, conns = conns)$pooled["concept_name"])
```

| concept\_name |
| :------------ |
| celecoxib     |

``` r
knitr::kable(ds.omop.concept.prevalence("drug_exposure", top_n = 10, scope = "pooled", conns = conns)$pooled)
```

| concept\_id | n\_persons | concept\_name                                                                                                     |
| ----------: | ---------: | :---------------------------------------------------------------------------------------------------------------- |
|    40213227 |       2660 | tetanus and diphtheria toxoids, adsorbed, preservative free, for adult use                                        |
|     1127433 |       2580 | Acetaminophen 325 MG Oral Tablet                                                                                  |
|    40213160 |       2140 | poliovirus vaccine, inactivated                                                                                   |
|     1713671 |       2020 | Amoxicillin 250 MG / Clavulanate 125 MG Oral Tablet                                                               |
|    19059056 |       1925 | Aspirin 81 MG Oral Tablet                                                                                         |
|     1118084 |       1840 | celecoxib                                                                                                         |
|    40213296 |       1735 | hepatitis A vaccine, adult dosage                                                                                 |
|    40213306 |       1560 | hepatitis B vaccine, adult dosage                                                                                 |
|     1127078 |       1425 | Acetaminophen 160 MG Oral Tablet                                                                                  |
|    40229134 |       1390 | Acetaminophen 21.7 MG/ML / Dextromethorphan Hydrobromide 1 MG/ML / doxylamine succinate 0.417 MG/ML Oral Solution |

``` r
mapping <- ds.omop.concept.relationships(ingredient_id, relationship_id = "Maps to",
  conns = conns, symbol = "omop")$pooled
children <- ds.omop.concept.descendants(ingredient_id, include_self = FALSE,
  conns = conns, symbol = "omop")$pooled
print(data.frame(Maps_to_rows = NROW(mapping), Descendant_rows = NROW(children)))
```

    #>   Maps_to_rows Descendant_rows
    #> 1            0               0

``` r
drug_set <- omop_concept_set(ingredient_id, descendants = TRUE)
```

## Define new users and the denominator

Require a 365-day washout and a complete next year of observation. This
complete- follow-up restriction simplifies the demonstration but can
select healthier or more stable patients. A real protocol may instead
use each person’s observed time.

``` r
users <- omop_new_users(drug_set, washout_days = 365)
users$filters <- c(users$filters, list(omop_observed(after = 364)))
drug <- omop_event("drug_exposure", drug_set)
era <- omop_era(drug, gap = 30, by = "ingredient", stockpile = TRUE)
covered <- omop_variable(event = era, reducer = omop_reduce("covered_days",
  window = omop_window(0, 364), coverage = "effective_supply"))
persistence <- omop_variable(event = era, reducer = omop_reduce("first_era_days",
  window = omop_window(0, 364)))
utilisation <- omop_study(users, outputs = "baseline", baseline = list(
  covered = covered, persistence = persistence,
  pdc = omop_number_derive(list(covered = covered),
    book_expr(book_op("/", book_col("covered"), book_lit(365))), unit = "1"),
  discontinued = omop_number_flag(persistence, "lt", 365),
  fills = omop_variable(event = drug, reducer = omop_reduce("count", window = omop_window(0, 364)))))
print(recipe_execute(utilisation, out = c(baseline = "D_utilisation"), conns = conns))
```

    #>        baseline
    #> "D_utilisation"

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_utilisation$covered", datasources = conns)))
```

| Site | Statistic | Value |
| :--- | :-------- | ----: |
| site | 5%        |     1 |
| site | 10%       |     1 |
| site | 25%       |     1 |
| site | 50%       |     1 |
| site | 75%       |     1 |
| site | 90%       |     1 |
| site | 95%       |     1 |
| site | Mean      |     1 |

The stockpiled era carries overlapping supply forward under the declared
rule. Covered days count the union of effective supply days, avoiding
double counting overlap. Era persistence tolerates a declared 30-day
gap; it is not the same as covered days. Recorded prescribing is not
proof of actual ingestion.

## PDC and discontinuation

Proportion of days covered (PDC) divides covered days by the 365
eligible days. The native expression declares a dimensionless result
(`unit = "1"`), rather than retaining the covered-days coordinate after
a display rescaling. A first era shorter than 365 days is a
protocol-defined discontinuation indicator under this gap rule, not
proof of permanent treatment cessation.

``` r
print(recipe_execute(utilisation, out = c(baseline = "D_utilisation_analysis"), conns = conns))
```

    #>                 baseline
    #> "D_utilisation_analysis"

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_utilisation_analysis$pdc", datasources = conns)))
```

| Site | Statistic |     Value |
| :--- | :-------- | --------: |
| site | 5%        | 0.0027397 |
| site | 10%       | 0.0027397 |
| site | 25%       | 0.0027397 |
| site | 50%       | 0.0027397 |
| site | 75%       | 0.0027397 |
| site | 90%       | 0.0027397 |
| site | 95%       | 0.0027397 |
| site | Mean      | 0.0027397 |

Inspect the definition before interpreting these values. Changing
stockpiling, gap tolerance or the follow-up denominator changes the
question even when the column name remains PDC. Privacy checks still
apply to the final table and releases.

## Exercises

**1.** Summarise persistence and compare its units with PDC.

Solution 1

``` r
knitr::kable(book_summary(dsBaseClient::ds.summary("D_utilisation_analysis$persistence", datasources = conns)))
```

| Site | Statistic | Value |
| :--- | :-------- | ----: |
| site | 5%        |     1 |
| site | 10%       |     1 |
| site | 25%       |     1 |
| site | 50%       |     1 |
| site | 75%       |     1 |
| site | 90%       |     1 |
| site | 95%       |     1 |
| site | Mean      |     1 |

Persistence is measured in days; PDC is a fraction. A tolerated gap
contributes to an era’s span but need not contribute covered supply
days.

**2.** Count the discontinuation indicator.

Solution 2

``` r
knitr::kable(book_counts(dsBaseClient::ds.table("D_utilisation_analysis$discontinued", datasources = conns)))
```

| Category | Count |
| :------- | ----: |
| 1        |  1745 |
| NA       |     0 |

The indicator is specific to the first era and this complete follow-up
cohort. Subsequent restarting remains possible.

**3.** Save the ingredient definition. Would changing its name to an ATC
code turn this into an ATC-class analysis?

Solution 3

``` r
set_file <- tempfile(fileext = ".rds")
saveRDS(drug_set, set_file)
identical(readRDS(set_file), drug_set)
```

    #> [1] TRUE

No. Class membership comes from the vocabulary hierarchy, not the local
label.

## Recap

Check vocabulary feasibility first. Ingredient utilisation is executable
here; ATC-class utilisation requires the missing classification
vocabulary. Distinguish covered supply, era persistence, PDC and
protocol-defined discontinuation, and state the denominator and
follow-up selection explicitly.

**Next:** [23. Case study: a harmonised
Table 1](https://isglobal-brge.github.io/dsOMOPClient/articles/book-23-case-table1.md).
