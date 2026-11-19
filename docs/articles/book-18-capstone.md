# 18\. Capstone: a cause-of-death wide table

**What you will learn**

Combine ages, questionnaire answers, visit periods and causes in one
private table, then use it to compare causes across age groups.

**You need:** the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md),
plus the date and category lessons. This chapter uses
`omop.questionnaire`.

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.questionnaire", server = "site")$conns
```

## Find the concepts

``` r
catalogue <- ds.omop.concept.lookup(c(900001L, 900002L, 900101L, 900102L,
  900201L, 900202L), datasources = conns)$pooled
catalogue[c("concept_id", "concept_name", "domain_id")]
```

| concept\_id | concept\_name         | domain\_id  |
| ----------: | :-------------------- | :---------- |
|      900001 | Education question    | Observation |
|      900002 | Fever question        | Observation |
|      900101 | Secondary education   | Meas Value  |
|      900102 | University education  | Meas Value  |
|      900201 | Myocardial infarction | Condition   |
|      900202 | Pneumonia             | Condition   |

The questions are Education **900001** and Fever **900002**; answers
Secondary **900101** and University **900102** are distinct from the
questions. The coded causes are Myocardial infarction **900201** and
Pneumonia **900202**. Their occurrence was established in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md).

## Define one row per person

Keep people without visits, answers or deaths. Read the latest
nonmissing answer from `value_as_concept_id`, and the cause from
`cause_concept_id`. These definitions use all recorded history; the
exercise below restricts education to before death.

``` r
education_event <- omop_event("observation", 900001L)
education <- omop_variable(event = education_event,
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
fever <- omop_variable(event = omop_event("observation", 900002L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
cause <- omop_variable(event = omop_event("death"),
  reducer = omop_reduce("first_value", value = "cause_concept_id"))
```

Completed age uses the full birth date and the chosen anchor. An
incomplete birth date or absent anchor gives missing age; it does not
silently create a birthday. All calendar presentations below refer to
the same first visit.

``` r
age_at_death <- omop_age_at("death")
first_visit <- omop_anchor(omop_event("visit_occurrence"), which = "first")
answer_keys <- c("concept:900101", "concept:900102")
variables <- list(
  age_at_death = age_at_death,
  age_band = omop_number_bins(age_at_death, breaks = c(0, 40, 121),
    labels = c("under 40", "40 and over")),
  age_at_first_visit = omop_age_at(first_visit),
  education = omop_category(education, omop_category_spec(levels = answer_keys,
    order = omop_category_order(c(answer_keys, "special:missing")), ordered = TRUE,
    missing = omop_category_missing(na = "level"))),
  fever = fever, cause = cause,
  visit_date = omop_date(first_visit),
  visit_month = omop_date(first_visit, omop_date_spec(precision = "month")),
  visit_quarter = omop_date(first_visit, omop_date_spec(precision = "quarter")),
  visit_year = omop_date(first_visit, omop_date_spec(precision = "year", representation = "number")))
capstone <- omop_recipe(variables = variables,
  outputs = omop_output("people", format = "wide", grain = "person"))
result <- recipe_execute(capstone, out = c(people = "D_capstone"), datasources = conns)
```

The table stays on the server with a pseudonymous person key. Exact
visit dates remain private; we use month and quarter categories for
calendar counts.

## Check age and missingness

``` r
dsBaseClient::ds.table("D_capstone$age_at_death",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_capstone$age_at_death
    #> 32 33 42 43 NA
    #>  6  6  6  6 24

Ages 32, 33, 42 and 43 each occur six times. The 24 missing ages combine
12 deaths with incomplete birth dates and 12 survivors. Missing age is
not a fifth age band.

``` r
dsBaseClient::ds.summary("D_capstone$age_at_first_visit", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 48
    #>
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean
    #>   29   29   29   34   39   39   39   34

``` r
dsBaseClient::ds.numNA("D_capstone$age_at_first_visit", datasources = conns)
```

    #> $site
    #> [1] 24

There are 24 valid ages and 24 missing. The valid ages are 29 and 39,
with a mean of 34 years. These describe the age at first care, not the
later age at death.

## Read the answers and calendar periods

``` r
dsBaseClient::ds.table("D_capstone$education",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_capstone$education
    #>  Secondary_education University_education              Missing                   NA
    #>                   12                   12                   24                    0

``` r
dsBaseClient::ds.table("D_capstone$fever",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_capstone$fever
    #> Yes  No  NA
    #>  12  12  24

Education has 12 Secondary, 12 University and 24 Missing answers. Fever
has 12 Yes, 12 No and 24 missing answers. No is an observed response; it
is not a substitute for missingness.

``` r
dsBaseClient::ds.table("D_capstone$visit_month",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_capstone$visit_month
    #> 2020-03 2020-06 2020-07      NA
    #>      12      12      12      12

``` r
dsBaseClient::ds.table("D_capstone$visit_quarter",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_capstone$visit_quarter
    #> 2020-Q1 2020-Q2 2020-Q3      NA
    #>      12      12      12      12

March, June and July each contain 12 first visits, with 12 people
lacking a visit. They fall in three different quarters, so changing
precision leaves these counts unchanged. In other data, several months
may merge into one quarter.

## Use the table: cause by age at death

``` r
dsBaseClient::ds.table("D_capstone$cause",
  "D_capstone$age_band", useNA = "always",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

|                        | under\_40 | 40\_and\_over | NA |
| :--------------------- | --------: | ------------: | -: |
| Myocardial\_infarction |         0 |            12 | 12 |
| Pneumonia              |        12 |             0 |  0 |
| NA                     |         0 |             0 | 12 |

The 12 pneumonia deaths are under 40. Of 24 myocardial-infarction
deaths, 12 are 40 or over and 12 have missing age. The 12 survivors have
neither cause nor age at death. These are counts, not age-specific
mortality rates: rates require age and person-time for survivors too,
which the next chapter supplies. Fine cross-tabulations may be withheld
when cells are too small; a withheld cell is not zero.

## Exercises

**1. Use only education recorded by death.** Keep the same people and
compare the education counts. Explain what happens to survivors.

Solution 1

``` r
before_death <- omop_variable(event = education_event,
  reducer = omop_reduce("last_value", value = "value_as_concept_id",
    window = omop_window(NULL, 0, anchor = omop_anchor(omop_event("death"), which = "first"))))
restricted <- omop_recipe(variables = list(education = before_death))
recipe_execute(restricted, out = "D_before_death", datasources = conns)
dsBaseClient::ds.table("D_before_death$education",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_before_death$education
    #>  Secondary_education University_education                   NA
    #>                   12                   12                   24

The counts remain 12 Secondary, 12 University and 24 missing: all
observed answers preceded death. This exercise uses the raw education
variable without the category specification, so missing answers have the
raw `NA` label instead of the main table’s “Missing”. A survivor has no
death anchor, so this definition cannot supply an answer for that
person. The unchanged totals do not make the window irrelevant.

**2. Use year as a number.** Find the mean first-visit year and its
denominator. Then check how its type differs from the exact date and
month category.

Solution 2

``` r
dsBaseClient::ds.summary("D_capstone$visit_year", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 48
    #>
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean
    #> 2020 2020 2020 2020 2020 2020 2020 2020

``` r
columns <- recipe_codebook(result)[[1]]$columns
columns[columns$name %in% c("visit_date", "visit_month", "visit_year"), c("name", "r_type")]
```

| name         | r\_type         |
| :----------- | :-------------- |
| visit\_date  | Date            |
| visit\_month | ordered\_factor |
| visit\_year  | integer         |

All 36 visitors have year 2020, so its mean is 2020; 12 people have no
value. The codebook distinguishes Date, factor and numeric
representations. Averaging calendar years is possible, but it cannot
reveal the distribution across months.

## Recap

One person-level table can combine answers, completed ages and calendar
summaries. Its usefulness comes from a question answered with those
columns, while retaining missingness and keeping patient rows private.

**Next:** [19. Case study: cause-specific mortality by age, sex and
period](https://isglobal-brge.github.io/dsOMOPClient/articles/book-19-case-mortality.md).
