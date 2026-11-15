# 19\. Case study: cause-specific mortality by age, sex and period

**What you will learn**

Define the mortality denominator, retain missing causes and ages, and
report cause-specific rates for a calendar period.

**You need:** [the installed packages and practice
resources](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start),
[concept
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
and the preceding tutorial chapters. Run these sections in order in a
fresh `book-work` session. All observations are public synthetic
teaching data; the numerical results are not clinical evidence.

## Research question and data boundary

How many deaths from each recorded cause occur in 2024, and how much
observed person-time supports those counts? This worked example uses the
synthetic **questionnaire fixture**. Every person is observed throughout
2024 until death or the year end. Its sex codes are unknown, so it
cannot supply a defensible sex-stratified mortality estimate. We show
that boundary explicitly rather than manufacture female/male
assignments.

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/fixture.R")
practice <- book_fixture()
conns <- practice$conns
```

## Find the cause and answer concepts

Repeat chapter 4’s local catalogue search. These fixture concepts have
no external Maps to or ancestry contract. Their domain separates
question, answer and cause.

``` r
catalogue <- ds.omop.concept.search(NULL, standard_only = FALSE, limit = 100, conns = conns)$pooled
knitr::kable(catalogue[c("concept_name", "domain_id", "standard_concept")])
```

| concept\_name         | domain\_id  | standard\_concept |
| :-------------------- | :---------- | :---------------- |
| Education question    | Observation | S                 |
| Fever question        | Observation | S                 |
| Secondary education   | Meas Value  | S                 |
| University education  | Meas Value  | S                 |
| Myocardial infarction | Condition   | S                 |
| Pneumonia             | Condition   | S                 |
| Yes                   | Meas Value  | S                 |
| No                    | Meas Value  | S                 |
| Inpatient Visit       | Meas Value  | S                 |
| Outpatient Visit      | Meas Value  | S                 |
| Education answer      | Meas Value  | S                 |
| Practice medicine     | Drug        | S                 |
| Practice measurement  | Measurement | S                 |
| kilogram              | Unit        | S                 |

``` r
ids <- setNames(catalogue$concept_id, catalogue$concept_name)
cause_sets <- lapply(ids[c("Myocardial infarction", "Pneumonia")], omop_concept_set)
death_prevalence <- ds.omop.concept.prevalence("death", scope = "pooled", conns = conns)$pooled
knitr::kable(death_prevalence[setdiff(names(death_prevalence), "concept_id")])
```

| n\_persons | concept\_name         |
| ---------: | :-------------------- |
|         10 | Myocardial infarction |
|          5 | Pneumonia             |

## Define the private cause-of-death table

The all-person population retains survivors. Completed age at death
preserves missing birth components. The cause is the coded answer, not
free source text. The two flags count cause-specific deaths; all deaths
in this fixture occur in 2024.

``` r
mi <- omop_event("death", cause_sets[["Myocardial infarction"]])
pneumonia <- omop_event("death", cause_sets[["Pneumonia"]])
mortality <- omop_recipe(variables = list(
  sex = omop_category(omop_variable(name = "sex", table = "person", column = "gender_concept_id"),
    omop_category_spec(missing = omop_category_missing(unknown = "level"))),
  cause = omop_variable(event = omop_event("death"), reducer = omop_reduce("first_value", value = "cause_concept_id")),
  age_band = omop_number_bins(omop_age_at("death"), breaks = c(0, 40, 121),
    labels = c("under 40", "40 and over")),
  period = omop_date("death", omop_date_spec(precision = "year")),
  death_days = omop_time_between("2024-01-01", "death"),
  mi = omop_variable(event = mi, reducer = omop_reduce("binary")),
  pneumonia = omop_variable(event = pneumonia, reducer = omop_reduce("binary"))))
book_symbols(recipe_execute(mortality, out = c(output = "D_mortality"), conns = conns))
```

    #>        output 
    #> "D_mortality"

``` r
ds.omop.table("D_mortality", "cause", conns = conns)
```

    #> D_mortality$cause
    #> Myocardial infarction             Pneumonia 
    #>                    12                     6

The output keeps missing cause separate from an observed cause. In a
mortality study, restrict cause events to the analysis period and
intersect residency intervals with follow-up. Do not borrow this
fixture’s complete residency assumption.

## Count causes by age band

``` r
dsBaseClient::ds.table("D_mortality$cause", "D_mortality$age_band",
  useNA = "always", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE.STUDY.site_row.props
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over  NA
    #>   Myocardial infarction        0         0.5 0.5
    #>   Pneumonia                    1         0.0 0.0
    #>   NA                           0         0.0 1.0
    #> 
    #> $output.list$TABLE.STUDY.site_col.props
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over  NA
    #>   Myocardial infarction        0           1 0.5
    #>   Pneumonia                    1           0 0.0
    #>   NA                           0           0 0.5
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_row.props
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over  NA
    #>   Myocardial infarction        0         0.5 0.5
    #>   Pneumonia                    1         0.0 0.0
    #>   NA                           0         0.0 1.0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_col.props
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over  NA
    #>   Myocardial infarction        0           1 0.5
    #>   Pneumonia                    1           0 0.0
    #>   NA                           0           0 0.5
    #> 
    #> $output.list$TABLE_STUDY.site_counts
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over NA
    #>   Myocardial infarction        0           6  6
    #>   Pneumonia                    6           0  0
    #>   NA                           0           0  6
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #>                        D_mortality$age_band
    #> D_mortality$cause       under 40 40 and over NA
    #>   Myocardial infarction        0           6  6
    #>   Pneumonia                    6           0  0
    #>   NA                           0           0  6
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Missing age remains visible. Dropping it would silently lose deaths with
partial birth dates. Fine age-by-sex-by-period cells may be withheld:
combine categories only when the study question supports the broader
estimand.

## Check whether sex stratification is supported

Concept zero means unknown, not a female or male equation category.
Retain it explicitly rather than distributing these people between
invented sex groups.

``` r
dsBaseClient::ds.table("D_mortality$cause", "D_mortality$sex",
  useNA = "always", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE.STUDY.site_row.props
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown NA
    #>   Myocardial infarction       1  0
    #>   Pneumonia                   1  0
    #>   NA                          1  0
    #> 
    #> $output.list$TABLE.STUDY.site_col.props
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown  NA
    #>   Myocardial infarction    0.50 NaN
    #>   Pneumonia                0.25 NaN
    #>   NA                       0.25 NaN
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_row.props
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown NA
    #>   Myocardial infarction       1  0
    #>   Pneumonia                   1  0
    #>   NA                          1  0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_col.props
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown  NA
    #>   Myocardial infarction    0.50 NaN
    #>   Pneumonia                0.25 NaN
    #>   NA                       0.25 NaN
    #> 
    #> $output.list$TABLE_STUDY.site_counts
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown NA
    #>   Myocardial infarction      12  0
    #>   Pneumonia                   6  0
    #>   NA                          6  0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #>                        D_mortality$sex
    #> D_mortality$cause       Unknown NA
    #>   Myocardial infarction      12  0
    #>   Pneumonia                   6  0
    #>   NA                          6  0
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

All the recorded causes fall under Unknown sex. The table therefore
supports a cause-specific overall rate, but no female/male comparison. A
study with known sex can use the same cross-tabulation and compute rates
within each prespecified stratum using its own observed person-time.

## Calculate the period rate

2024 has 366 days. Stop each deceased person’s contribution at death and
each survivor’s contribution at the end of the year. Divide released
mean indicators by released mean follow-up: the common person count
cancels. This works because both means use the same complete roster, not
different nonmissing subsets.

``` r
print(ds.omop.mutate("D_mortality", risk_days = coalesce(death_days, 366),
  newobj = "D_mortality_rate", conns = conns))
```

    #> [1] "D_mortality_rate"

``` r
mean_days <- dsBaseClient::ds.mean("D_mortality_rate$risk_days", type = "combine", datasources = conns)
mean_mi <- dsBaseClient::ds.mean("D_mortality_rate$mi", type = "combine", datasources = conns)
mean_pneumonia <- dsBaseClient::ds.mean("D_mortality_rate$pneumonia", type = "combine", datasources = conns)
rates <- data.frame(Period = "2024", Cause = c("Myocardial infarction", "Pneumonia"),
  Per_1000_person_years = 1000 * 365.25 * c(mean_mi$Global.Mean[1,1],
    mean_pneumonia$Global.Mean[1,1]) / mean_days$Global.Mean[1,1])
knitr::kable(rates)
```

| Period | Cause                 | Per\_1000\_person\_years |
| :----- | :-------------------- | -----------------------: |
| 2024   | Myocardial infarction |                1013.1761 |
| 2024   | Pneumonia             |                 506.5881 |

These rates describe an intentionally tiny synthetic population with
many deaths. These teaching results do not estimate mortality in a real
population. A count divided by all enrolled people is a risk, not a
person-time rate.

## Compare calendar periods without reusing the wrong denominator

In the preceding year, 2023, all 24 fixture residents remain alive and
observed through the year end. Apply a period predicate to each cause
event and cap risk time at the 365-day year. This example deliberately
has zero deaths in that period.

``` r
mi_2023 <- omop_event("death", cause_sets[["Myocardial infarction"]],
  where = omop_where(death_date >= "2023-01-01", death_date < "2024-01-01"))
pneumonia_2023 <- omop_event("death", cause_sets[["Pneumonia"]],
  where = omop_where(death_date >= "2023-01-01", death_date < "2024-01-01"))
earlier <- omop_recipe(variables = list(
  mi = omop_variable(event = mi_2023, reducer = omop_reduce("binary")),
  pneumonia = omop_variable(event = pneumonia_2023, reducer = omop_reduce("binary")),
  until_death = omop_time_between("2023-01-01", "death")))
book_symbols(recipe_execute(earlier, out = c(output = "D_mortality_2023"), conns = conns))
```

    #>             output 
    #> "D_mortality_2023"

``` r
print(ds.omop.mutate("D_mortality_2023", potential_days = coalesce(until_death, 365),
  risk_days = ifelse(potential_days > 365, 365, potential_days),
  newobj = "D_risk_2023", conns = conns))
```

    #> [1] "D_risk_2023"

``` r
earlier_days <- dsBaseClient::ds.mean("D_risk_2023$risk_days", type = "combine", datasources = conns)
earlier_mi <- dsBaseClient::ds.mean("D_risk_2023$mi", type = "combine", datasources = conns)
earlier_pneumonia <- dsBaseClient::ds.mean("D_risk_2023$pneumonia", type = "combine", datasources = conns)
earlier_rates <- data.frame(Period = "2023", Cause = rates$Cause,
  Per_1000_person_years = 1000 * 365.25 * c(earlier_mi$Global.Mean[1,1],
    earlier_pneumonia$Global.Mean[1,1]) / earlier_days$Global.Mean[1,1])
knitr::kable(rbind(earlier_rates, rates))
```

| Period | Cause                 | Per\_1000\_person\_years |
| :----- | :-------------------- | -----------------------: |
| 2023   | Myocardial infarction |                   0.0000 |
| 2023   | Pneumonia             |                   0.0000 |
| 2024   | Myocardial infarction |                1013.1761 |
| 2024   | Pneumonia             |                 506.5881 |

The zero rate is a released estimate for this defined synthetic period,
not a replacement for suppression. A general mortality analysis must
additionally remove people who died before the period and intersect each
residency interval with it.

## Exercises

**1 (basic).** Count the calendar-period labels.

Solution 1

``` r
ds.omop.table("D_mortality", "period", conns = conns)
```

    #> D_mortality$period
    #> 2024 
    #>   18

Observed deaths belong to 2024; survivors have no death-period label.

**2 (intermediate).** Report the same rates per 100,000 person-years.

Solution 2

``` r
knitr::kable(transform(rates, Per_100000_person_years = 100 * Per_1000_person_years))
```

| Period | Cause                 | Per\_1000\_person\_years | Per\_100000\_person\_years |
| :----- | :-------------------- | -----------------------: | -------------------------: |
| 2024   | Myocardial infarction |                1013.1761 |                  101317.61 |
| 2024   | Pneumonia             |                 506.5881 |                   50658.81 |

Only public aggregate arithmetic changed. Person-time did not change.

**3 (advanced).** Check that the numerator and denominator used the same
valid population size before cancelling their denominators.

Solution 3

``` r
c(same_mi_roster = mean_mi$Global.Mean[1,"Nvalid"] == mean_days$Global.Mean[1,"Nvalid"],
  same_pneumonia_roster = mean_pneumonia$Global.Mean[1,"Nvalid"] == mean_days$Global.Mean[1,"Nvalid"])
```

    #>        same_mi_roster same_pneumonia_roster 
    #>                  TRUE                  TRUE

Both must be TRUE. Also establish the same person membership from the
definition; equal counts alone would not prove identical rosters in
arbitrary tables.

## Recap

Define causes and residency before computing rates. Keep unknown age and
cause visible, stop risk time at death, and state when missing sex or
small cells prevent the requested stratification. The same
period-specific definition must be rerun for each calendar period; a
death-year label is not a person-time denominator.

**Next:** [20. Case study: celecoxib versus
diclofenac](https://isglobal-brge.github.io/dsOMOPClient/articles/book-20-case-gibleed.md).
