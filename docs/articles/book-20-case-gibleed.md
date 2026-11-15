# 20\. Case study: celecoxib versus diclofenac

**What you will learn**

Find the study concepts, define mutually exclusive new-user entry,
account for censoring, and fit a pooled adjusted constant-hazard
survival model.

**You need:** the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md),
[concept
discovery](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
and the preceding tutorial chapters. This chapter uses the two GiBleed
sites. All observations are public synthetic teaching data; the
numerical results are not clinical evidence.

## Research question

Among people beginning celecoxib or diclofenac after a year without
either drug, how does first GI bleeding during the following year
differ? We adjust for age. This is a methods exercise; age adjustment
alone does not identify a causal drug effect.

``` r
library(dsOMOPClient)
two_conns <- ds.omop.login(rep("https://localhost:8443", 2), user = "administrator",
  password = "password",
  server = c("north", "south"),
  resource = c(north = "omop.gibleed_north", south = "omop.gibleed_south"))$conns
```

The two GiBleed resources hold disjoint halves of the public GiBleed
people, so no person is counted twice when the sites are pooled.

## Find the drugs and outcome

``` r
ce <- ds.omop.concept.search("celecoxib", domain = "Drug", conns = two_conns)$pooled
di <- ds.omop.concept.search("diclofenac", domain = "Drug", conns = two_conns)$pooled
gi <- ds.omop.concept.search("Gastrointestinal hemorrhage", domain = "Condition", conns = two_conns)$pooled
rbind(ce, di, gi)[c("concept_id", "concept_name", "standard_concept", "concept_class_id")]
```

| concept\_id | concept\_name                                       | standard\_concept | concept\_class\_id |
| ----------: | :-------------------------------------------------- | :---------------- | :----------------- |
|     1118088 | celecoxib 200 MG Oral Capsule \[Celebrex\]          | S                 | Branded Drug       |
|     1118084 | celecoxib                                           | S                 | Ingredient         |
|    40162359 | Diclofenac Sodium 75 MG Delayed Release Oral Tablet | S                 | Clinical Drug      |
|     1124300 | Diclofenac                                          | S                 | Ingredient         |
|      192671 | Gastrointestinal hemorrhage                         | S                 | Clinical Finding   |

``` r
ceid <- ce$concept_id[trimws(ce$concept_name) == "celecoxib"]
diid <- di$concept_id[trimws(di$concept_name) == "Diclofenac"]
giid <- gi$concept_id[trimws(gi$concept_name) == "Gastrointestinal hemorrhage"]
```

Select **1118084** (celecoxib), **1124300** (Diclofenac) and **192671**
(Gastrointestinal hemorrhage), then inspect descendants. These GiBleed
study-drug records are already ingredient-coded. The descendant request
includes the seed by default: the celecoxib row below is self-inclusion,
not a more specific product. Ingredient-to-product membership uses the
ancestry hierarchy; Maps to serves a different purpose. This reduced
vocabulary lacks the ingredient-to-product ancestor edge from
**1118084** to **1118088**. A catalogue match for the capsule therefore
does not establish a descendant relationship, which is why only the seed
appears below.

``` r
ce_children <- ds.omop.concept.descendants(ceid, conns = two_conns)$pooled
ce_children[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name |
| ----------: | :------------ |
|     1118084 | celecoxib     |

``` r
ds.omop.concept.locate(c(ceid, diid, giid), scope = "pooled",
  conns = two_conns)$pooled
```

| concept\_id | table\_name           | concept\_column        | n\_records | n\_persons |
| ----------: | :-------------------- | :--------------------- | ---------: | ---------: |
|      192671 | condition\_era        | condition\_concept\_id |        475 |        475 |
|      192671 | condition\_occurrence | condition\_concept\_id |        475 |        475 |
|     1118084 | drug\_exposure        | drug\_concept\_id      |       1840 |       1840 |
|     1118084 | drug\_era             | drug\_concept\_id      |       1840 |       1840 |
|     1124300 | drug\_exposure        | drug\_concept\_id      |        850 |        850 |
|     1124300 | drug\_era             | drug\_concept\_id      |        850 |        850 |

``` r
study_sets <- list(celecoxib = omop_concept_set(ceid, descendants = TRUE),
  diclofenac = omop_concept_set(diid, descendants = TRUE),
  bleeding = omop_concept_set(giid, descendants = TRUE))
```

The locator shows recorded use of both ingredient IDs and the bleeding
concept. Counts are bands of five; the analysis below supplies its own
exact denominators.

## Define entry and washout

Choose first eligible exposure to either study drug. Require 365
observed days and no exposure to either during the preceding year.
Exclude simultaneous starts so each person’s baseline treatment is
unambiguous. Later switching does not change baseline treatment: this is
an initial-treatment follow-up definition. Exclude any bleeding before
or at entry so the outcome is first recorded bleeding after entry,
rather than recurrence of a known prior outcome.

``` r
ce_event <- omop_event("drug_exposure", study_sets$celecoxib, name = "celecoxib")
di_event <- omop_event("drug_exposure", study_sets$diclofenac, name = "diclofenac")
bleed <- omop_event("condition_occurrence", study_sets$bleeding)
users <- omop_population("users", index = omop_index(omop_any_of(ce_event, di_event),
  select = "first_eligible"), filters = list(omop_observed(before = 365),
  omop_has_not(ce_event, window = omop_window(-365, -1)),
  omop_has_not(di_event, window = omop_window(-365, -1)),
  omop_has_not(bleed, window = omop_window(NULL, 0)),
  omop_not(omop_all(omop_has(ce_event, window = omop_window(0, 0)),
    omop_has(di_event, window = omop_window(0, 0))))))
```

## Join baseline variables to censored follow-up

One row represents one person’s first eligible episode. Only the joined
analysis frame is published; its dependencies remain private
intermediates.

``` r
comparison <- omop_recipe(populations = list(users),
  variables = list(birth_year = omop_variable(name = "birth_year", table = "person", column = "year_of_birth"),
    birth_month = omop_variable(name = "birth_month", table = "person", column = "month_of_birth"),
    birth_day = omop_variable(name = "birth_day", table = "person", column = "day_of_birth"),
    entry = omop_date_parts("index", c("year", "month", "day"),
      date_spec = omop_date_spec(representation = "number")),
    celecoxib = omop_variable(event = ce_event,
    reducer = omop_reduce("binary", window = omop_window(0, 0)))),
  outputs = list(omop_output("baseline", rows = "users", format = "wide"),
    omop_output("survival", rows = "users", format = "survival", options = list(
      outcomes = list(bleed = bleed), tar = omop_window(1, 365),
      censor = list("observation_end", "death"))),
    omop_output("analysis", rows = "users", format = "joined", options = list(
      from = list(list(output = "baseline"), list(output = "survival")),
      how = "inner", cardinality = "one_to_one"))), publish = "analysis")
recipe_execute(comparison, out = c(analysis = "D_comparison"), conns = two_conns)
```

The censor list and 365-day window define the estimand. Event = 0 means
censored within that follow-up, not “never bleeds”. Person rows remain
private at each site.

Completed age subtracts one from the difference in calendar years when
entry occurs before the birthday. We calculate it explicitly from the
birth components and index-date parts below; missing birth components
preserve missing age. GiBleed stores its birth components as numeric
rather than integer columns, which the native age constructor does not
accept. Use `omop_age_at("index")` where the source schema supports it;
here the integral component values permit the completed-age calculation
below.

## Describe the arms before modelling

Follow-up starts on day 1 and ends at bleeding, death, observation end
or day 365, whichever comes first. The survival output measures elapsed
days as exit minus entry. A day-1 outcome would therefore have zero
time; check this before taking a logarithm rather than silently adding
time or dropping those events.

``` r
ds.omop.mutate("D_comparison", years = follow_up_days / 365.25,
  zero_time = ifelse(follow_up_days <= 0, 1L, 0L),
  month_difference = entry_month - birth_month, day_difference = entry_day - birth_day,
  age = entry_year - birth_year - ifelse(month_difference < 0 |
    (month_difference == 0 & day_difference < 0), 1L, 0L),
  newobj = "D_comparison_model", conns = two_conns)
dsBaseClient::ds.table("D_comparison_model$zero_time",
  datasources = two_conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_comparison_model$zero_time
    #>    0   NA
    #> 2630    0

``` r
dsBaseClient::ds.table("D_comparison$celecoxib",
  "D_comparison$event", datasources = two_conns)$output.list$TABLES.COMBINED_all.sources_counts
```

|    |    0 |   1 | NA |
| :- | ---: | --: | -: |
| 0  |  706 | 124 |  0 |
| 1  | 1445 | 355 |  0 |
| NA |    0 |   0 |  0 |

All 2,630 episodes have positive time. Treatment 0 is diclofenac and 1
is celecoxib; event 1 is bleeding and 0 is censoring. The event counts
are 124 and 355 respectively. No prior bleeding is present in these
synthetic data, so the exclusion leaves these counts unchanged while
making the study definition explicit.

``` r
arm_summary <- do.call(rbind, lapply(0:1, function(arm) {
  ds.omop.filter("D_comparison_model", var = "celecoxib", op = "==", value = arm,
    newobj = paste0("D_arm", arm), conns = two_conns)
  aggregate <- function(column) dsBaseClient::ds.mean(paste0("D_arm", arm, "$", column),
    type = "combine", datasources = two_conns)$Global.Mean
  time <- aggregate("years"); event <- aggregate("event"); age <- aggregate("age")
  n <- time[1, "Nvalid"]
  events <- round(event[1, "EstimatedMean"] * n)
  py <- time[1, "EstimatedMean"] * n
  data.frame(Arm = c("Diclofenac", "Celecoxib")[arm + 1], People = n,
    Events = events, Person_years = py, Rate_per_1000 = 1000 * events / py,
    Mean_followup_days = 365.25 * py / n, Mean_age = age[1, "EstimatedMean"])
}))
arm_summary
```

| Arm        | People | Events | Person\_years | Rate\_per\_1000 | Mean\_followup\_days | Mean\_age |
| :--------- | -----: | -----: | ------------: | --------------: | -------------------: | --------: |
| Diclofenac |    830 |    124 |        710.05 |          174.64 |               312.46 |     38.10 |
| Celecoxib  |   1800 |    355 |       1470.74 |          241.37 |               298.44 |     38.17 |

Celecoxib contributes 1,800 people, 355 events and 1,470.74
person-years; diclofenac contributes 830 people, 124 events and 710.05
person-years. The crude rates are about 241 and 175 per 1,000
person-years. Read the age and follow-up columns too: unequal follow-up
is why a comparison of raw event counts would mislead.

## Fit an adjusted survival model with dsBase

An exponential survival model assumes a constant event hazard during
follow-up. Fit a Poisson model of the event indicator with
log(person-years) as an offset. The treatment coefficient is a log
hazard ratio against diclofenac, conditional on completed age at entry.
This is not a Cox model.

``` r
dsBaseClient::ds.log("D_comparison_model$years", newobj = "log_years", datasources = two_conns)
comparison_fit <- dsBaseClient::ds.glm("event ~ celecoxib + age",
  data = "D_comparison_model", family = "poisson", offset = "log_years", datasources = two_conns)
beta <- comparison_fit$coefficients["celecoxib", "Estimate"]
se <- comparison_fit$coefficients["celecoxib", "Std. Error"]
data.frame(Hazard_ratio = exp(beta),
  Lower_95 = exp(beta - 1.96 * se), Upper_95 = exp(beta + 1.96 * se))
```

| Hazard\_ratio | Lower\_95 | Upper\_95 |
| ------------: | --------: | --------: |
|          1.38 |     1.124 |     1.692 |

After age adjustment, celecoxib starters have about 1.38 times the
bleeding hazard of diclofenac starters (95% CI about 1.12–1.69), under
the constant-hazard assumption. Age adjustment alone cannot remove
confounding by indication, selection or outcome misclassification; this
synthetic association is not a causal drug effect.

## Exercises

**1. Compare crude and adjusted results.** Calculate the crude
incidence-rate ratio from the arm table. Does agreement establish
adequate adjustment?

Solution 1

``` r
data.frame(Crude_rate_ratio = arm_summary$Rate_per_1000[2] /
  arm_summary$Rate_per_1000[1], Adjusted_hazard_ratio = exp(beta))
```

| Crude\_rate\_ratio | Adjusted\_hazard\_ratio |
| -----------------: | ----------------------: |
|              1.382 |                    1.38 |

Both ratios are about 1.38. Similar estimates only show that this age
adjustment changes little here; they do not rule out other confounding.

**2. Check censoring.** How many episodes end before day 365 without
bleeding?

Solution 2

``` r
ds.omop.mutate("D_comparison", early_censor = ifelse(event == 0 & follow_up_days < 364, 1L, 0L),
  newobj = "D_censor", conns = two_conns)
dsBaseClient::ds.table("D_censor$early_censor",
  datasources = two_conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_censor$early_censor
    #>    0    1   NA
    #> 2578   52    0

The 52 early censored episodes end at observation end; GiBleed has no
recorded deaths. Event-free follow-up through day 365 has elapsed
duration 364 from day 1. Censoring means the outcome is unknown after
follow-up ends, not that it never occurs.

## Recap

Find standard ingredients and their descendants, make new-user entry
mutually exclusive, declare time at risk and censoring, then pool a
model with an explicit survival assumption. Protected tables and model
diagnostics remain part of interpretation.

**Next:** [21. Case study: glucose
trajectories](https://isglobal-brge.github.io/dsOMOPClient/articles/book-21-case-glucose.md).
