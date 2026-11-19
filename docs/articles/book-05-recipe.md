# 5\. A first recipe

**What you will learn**

Build education and cause-of-death columns, predict their output, and
request category counts.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.questionnaire`, the synthetic questionnaire
study described there.

## Start this chapter

Log in first:

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.questionnaire", server = "site")$conns
```

The connection is ready; `conns` identifies the server used by the
analysis calls.

The questionnaire has 48 people. Of these, 24 answered the education
question (twice each), 36 have a recorded death, and 12 have neither
education nor death. These are the teaching data described in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).

## Select the education question

An *event* describes records of interest. It is a definition on your
computer, not a downloaded table. We have switched from chapter 4’s
GiBleed asthma search to the questionnaire dataset. First look up its
education question by concept:

``` r
question <- ds.omop.concept.lookup(900001L, datasources = conns)
question$pooled[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name      |
| ----------: | :----------------- |
|      900001 | Education question |

Education question is concept **900001**, found in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md).
Use that ID to select observations:

``` r
education_event <- omop_event("observation", concepts = 900001L)
```

The server will select observations whose question is Education
question.

## Choose one answer per person

A *reducer* turns possibly many records into one value. Use the latest
nonmissing coded answer, ordered by the observation date:

``` r
education <- omop_variable(
  event = education_event,
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
education
```

    #> omop_variable: <unnamed>
    #>    last_value of observation

This makes one categorical column. It does not create a separate binary
column for every education level. A same-date tie uses the documented
deterministic record ordering; it does not mean “largest answer ID”.

## Add cause of death

``` r
cause <- omop_variable(
  event = omop_event("death"),
  reducer = omop_reduce("first_value", value = "cause_concept_id"))
```

Here the value comes from `cause_concept_id`. Do not substitute
`cause_source_value`: that is the verbatim source code, such as an
ICD-10 code, rather than the standard concept used for translated
categories. Source-value fields are blocked on this route.

## Describe a wide table

*Wide* means one column per variable. *Person grain* means one row per
person. The native all-person population retains people who have neither
answer nor death.

``` r
people <- omop_recipe(
  variables = list(education = education, cause_of_death = cause),
  outputs = omop_output("people", format = "wide", grain = "person"))
```

`person_id` is included automatically as a pseudonym. Missing records
produce missing values. The roster does not require an extra identifier
variable.

## Predict: read the recipe explanation

Before running, predict **48 rows and three columns**: the automatic
person key, education and cause of death. Education should have **24
missing values**. Both variables read all recorded history because no
time window restricts them.

``` r
explained <- recipe_explain(people, datasources = conns)
writeLines(head(explained$text, 9))
```

    #> Recipe plan
    #> Population 'base' (criteria): none
    #> Variables:
    #>   education: last_value of event 'obs_900001' (value 'value_as_concept_id') over any time; missing = "skip"
    #>   cause_of_death: first_value of event 'death' (value 'cause_concept_id') over any time; missing = "skip"
    #> Output 'people' [wide] rows 'base', grain person
    #> Published: people
    #> Defaults relied on:
    #>   - variable 'education': rows with a NULL value are skipped before the row is selected (missing = "skip")

Read this short opening in order: the population sets one row per
person; `education` takes the last coded observation answer;
`cause_of_death` takes the first coded death cause; the output arranges
them as columns. The remaining explanation describes technical defaults;
these are not observed patient counts.

## Run: assign the result

``` r
recipe_execute(people, out = c(people = "D_people"), datasources = conns)
```

Nothing comes back to you: the output named `people` is now `D_people`,
a table kept on the server. Concept-valued columns are labelled factors
because of their declared type. The names `education` and
`cause_of_death` need no `_concept_id` suffix.

## Verify: check what you got

Use `DSI::datashield.symbols()` to list assigned server objects. Then
check the size, column names, type and missingness. These calls return
metadata or checked aggregates; they never retrieve patient rows.

``` r
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "D_people" "omop"

`D_people` is present alongside the `omop` resource handle: the
extraction has created a private analysis table.

``` r
dsBaseClient::ds.dim("D_people", datasources = conns)
```

    #> $`dimensions of D_people in site`
    #> [1] 48  3
    #>
    #> $`dimensions of D_people in combined studies`
    #> [1] 48  3

The 48 rows correspond to all 48 people, including people with missing
answers. The three columns match the prediction.

``` r
dsBaseClient::ds.colnames("D_people", datasources = conns)
```

    #> $site
    #> [1] "person_id"      "education"      "cause_of_death"

`person_id` is the automatic protected key; the other two columns are
the variables we requested.

``` r
dsBaseClient::ds.class("D_people$education", datasources = conns)
```

    #> $site
    #> [1] "dsomop_concept_factor" "factor"

Education is a factor, which stores categories. `dsomop_concept_factor`
records that these categories come from OMOP concepts.

``` r
dsBaseClient::ds.numNA("D_people$education", datasources = conns)
```

    #> $site
    #> [1] 24

The 24 missing values are people with no education answer. The next
category table completes the **predict, run, verify** loop. In later
lessons, choose the one check that helps answer the current question.

## Count the education categories

``` r
education_table_result <- dsBaseClient::ds.table("D_people$education", useNA = "no", datasources = conns)
education_table_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_people$education
    #>  Secondary_education University_education
    #>                   12                   12

There are 12 people whose **latest** answer is Secondary education and
12 whose latest answer is University education. Every respondent gave
both answers at different times; these are counts of the selected latest
answer, not everyone who ever gave each answer. `useNA = "no"` leaves
out the 24 missing values.

## Count causes

``` r
ds.omop.table("D_people", "cause_of_death", datasources = conns)
```

    #> Combined (all valid sites): cause_of_death
    #> D_people$cause_of_death
    #> Myocardial_infarction             Pneumonia
    #>                    24                    12
    #>
    #> By site:
    #>                        study
    #> D_people$cause_of_death site
    #>   Myocardial_infarction   24
    #>   Pneumonia               12

The table shows 24 myocardial infarctions and 12 pneumonias.
`ds.omop.table()` returns the same object as dsBase’s `ds.table()`
(counts and proportions for each site and combined), with the exact
published labels and the categories that nobody has; printing shows the
combined table and each site’s. Missing causes are excluded here; the
exercise below will include them. Absence of a death record is not proof
of survival.

## Give a question your own yes/no labels

[Chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md)
identifies Fever question as **900002**, Yes as **900301**, and No as
**900302**. Here is a label map keyed by those answer IDs:

``` r
fever <- omop_variable(
  event = omop_event("observation", concepts = 900002L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"),
  labels = c("900301" = "yes", "900302" = "no"))
```

The map is keyed by answer concepts. It changes presentation, not which
question is selected. Missing fever answers remain missing; they do not
become “no”.

``` r
answers <- omop_recipe(
  variables = list(education = education, fever = fever, cause_of_death = cause),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(answers, out = c(people = "D_answers"), datasources = conns)
```

``` r
yes_no_result <- dsBaseClient::ds.table("D_answers$fever", useNA = "no", datasources = conns)
yes_no_result$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_answers$fever
    #> yes  no
    #>  12  12

The table contains 12 “yes” and 12 “no” answers, using your lower-case
labels. The other 24 people did not answer; they are excluded by `useNA
= "no"`. See
[chapter 10](https://isglobal-brge.github.io/dsOMOPClient/articles/book-10-harmonise.md)
for the same operation across two different vocabularies.

## Exercise 1: first answer instead of last

Take each person’s first education answer. Do the totals for each
category reveal that people changed answers?

Solution

``` r
first_education <- omop_variable(event = education_event,
  reducer = omop_reduce("first_value", value = "value_as_concept_id"))
comparison <- omop_recipe(variables = list(first = first_education, last = education),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(comparison, out = c(people = "D_education_change"), datasources = conns)
dsBaseClient::ds.table(
  "D_education_change$first", "D_education_change$last", useNA = "no",
  datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

|                       | Secondary\_education | University\_education |
| :-------------------- | -------------------: | --------------------: |
| Secondary\_education  |                    0 |                    12 |
| University\_education |                   12 |                     0 |

Rows show first answers and columns show last answers. The off-diagonal
cells show 12 people changing from Secondary to University and 12
changing in the opposite direction. The first and last category totals
are both 12/12, so those totals alone hide every respondent’s change.

## Exercise 2: include missing causes

Count causes with dsBase and include people without a coded cause.

Solution

``` r
dsBaseClient::ds.table("D_people$cause_of_death",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_people$cause_of_death
    #> Myocardial_infarction             Pneumonia                    NA
    #>                    24                    12                    12

The 24 myocardial infarctions, 12 pneumonias and 12 missing causes
account for all 48 people. Missing is not another coded cause and does
not establish survival.

## Exercise 3: why not use the source code?

Try compiling a cause variable based on `cause_source_value`. Does
renaming it make that source field available?

Solution

``` r
source_cause <- omop_recipe(variables = list(cause = omop_variable(
  event = omop_event("death"),
  reducer = omop_reduce("first_value", value = "cause_source_value"))))
recipe_to_plan(source_cause)
```

    #> Error: Disclosive: the plan requests blocked (class-1) column(s) that the server can never release or filter on: cause_source_value (variable 'cause' reducer value).

The source-value field is blocked before execution. A new variable name
does not change its protection; use `cause_concept_id` for the coded
cause.

## Recap

An event selects records, a reducer selects a value, and a wide output
arranges private rows. Only the aggregate returns to you.

**Next:** [6. Events, windows and
reducers](https://isglobal-brge.github.io/dsOMOPClient/articles/book-06-events.md).
