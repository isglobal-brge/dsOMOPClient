# 7\. Populations and index events

**What you will learn**

Choose who enters a study, define episodes and observation coverage,
reuse a cohort, identify new users and sample controls at case time.

This chapter uses the 24-person synthetic questionnaire test fixture
described in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).

**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run its sections in order. Each chapter starts from a fresh practice
connection; all the outputs below come from real dsOMOP and dsOMOPClient
2.10.5 calls. The numbers describe teaching data, not clinical findings.

Concept selections refer to the searches and saved definitions in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md),
including the separate questionnaire catalogue. Additional
body-measurement concepts are found on screen in the [formula
walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-15-formulas.html#find-the-inputs-before-calculating).

## Start this chapter

Start a fresh R session in the `book-work` directory created in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#get-the-book-helpers).
Run this setup first:

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/fixture.R")
practice <- book_fixture()
conns <- practice$conns
names(conns)
```

    #> [1] "site"

The `site` connection is ready. This chapter creates its temporary
questionnaire fixture locally.

## Start with an index event

A *population* specifies who contributes rows. An *index* selects an
event date for each study episode. Here a person’s first visit starts
one episode.

``` r
visits <- omop_event("visit_occurrence")
visitors <- omop_population("visitors", index = omop_index(visits, select = "first"))
class(visitors)
```

    #> [1] "omop_population" "list"

People without visits are absent. The default episode end is the end of
the covering observation period. `select = "all"` would keep recurrent
visit episodes.

## Release membership as a private table

``` r
visit_members <- omop_recipe(populations = list(visitors),
  outputs = omop_output("members", rows = "visitors", format = "membership"))
print(recipe_execute(visit_members, out = c(members = "D_members"), conns = conns))
```

    #>     members
    #> "D_members"

Membership includes pseudonymous keys and episode dates. Inspect its
schema, not its patient rows:

``` r
dsBaseClient::ds.colnames("D_members", datasources = conns)
```

    #> $site
    #> [1] "row_id"            "cohort_row_id"     "person_id"         "cohort_start_date"
    #> [5] "cohort_end_date"   "index_source"

The default date fields are offsets from the index. Several episodes can
belong to the same person; a row count is not necessarily a person
count.

## Require an eligible visit

A *criterion* is an inclusion test. These criteria require an inpatient
record on the index day and at least 365 days of observation before it.

``` r
inpatient <- omop_event("visit_occurrence", concepts = 9201L)
eligible <- omop_population("eligible",
  index = omop_index(visits, select = "first_eligible"),
  filters = list(omop_observed(before = 365),
    omop_has(inpatient, window = omop_window(0, 0))))
class(eligible)
```

    #> [1] "omop_population" "list"

`first_eligible` tests the complete criterion tree at each candidate,
then selects the earliest passing visit. `first` selects a person’s
first visit before testing; it would not advance to a later visit when
that first one fails. Combine alternatives with `omop_any()` and joint
requirements with `omop_all()`. Only TRUE survives; an unknown criterion
is not evidence of eligibility.

``` r
eligible_recipe <- omop_recipe(populations = list(eligible),
  variables = list(visits = omop_count("visit_occurrence", before = Inf)),
  outputs = omop_output("people", rows = "eligible", format = "wide"))
print(recipe_execute(eligible_recipe, out = c(people = "D_eligible"), conns = conns))
```

    #>       people
    #> "D_eligible"

The count includes visits through the chosen index day.

``` r
dsBaseClient::ds.summary("D_eligible$visits", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #>
    #> $site$length
    #> [1] 12
    #>
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean
    #>    2    2    2    2    2    2    2    2

This aggregate describes the eligible episodes, not everybody in the
database.

## Make coverage explicit

An *observation period* states when the data source observes a person.
The default requires exactly one period covering the index and drops
missing or ambiguous coverage. A union policy can join adjacent periods
when that matches the study.

``` r
covered <- eligible
covered$coverage <- omop_coverage(policy = "union", gap = 0)
coverage_recipe <- omop_recipe(populations = list(covered),
  outputs = omop_output("members", rows = "eligible", format = "membership"))
print(recipe_execute(coverage_recipe, out = c(members = "D_covered"), conns = conns))
```

    #>     members
    #> "D_covered"

Here each person has one observation period, so the policy does not
change membership. `gap = 0` does not tolerate unobserved days. The
`earliest` policy instead chooses one complete covering record; it does
not union records. Coverage is not proof that a person took a drug or
attended a visit.

## Reuse an existing cohort

A *results cohort* is a named definition with private membership and
start/end dates already available to the resource. Practice cohort 1
contains two of the four synthetic patterns. Its ID is a definition ID,
not a patient ID.

``` r
cohort_population <- omop_population("saved", index = omop_index(
  list(cohort_definition_id = 1L), select = "all", end = omop_end("event_end")))
cohort_recipe <- omop_recipe(populations = list(cohort_population),
  outputs = omop_output("members", rows = "saved", format = "membership"))
print(recipe_execute(cohort_recipe, out = c(members = "D_cohort"), conns = conns))
```

    #>    members
    #> "D_cohort"

This reads existing cohort episodes without writing a cohort to the
database. Use `omop_in_cohort(1L)` as a criterion when you want
membership alone while retaining a different index. Do not invent a
cohort ID on a real resource.

## Identify new users

A *washout* requires an observed period without the study exposure
before entry. The shortcut combines drug records into eras, selects the
first eligible era, and checks both observation and absence of prior
exposure.

``` r
new_users <- omop_new_users(950001L, washout_days = 365, by = "concept")
new_user_recipe <- omop_study(new_users, outputs = "baseline",
  baseline = list(prior_fills = omop_count("drug_exposure", 950001L,
    window = omop_window(-365, -1))))
print(recipe_execute(new_user_recipe, out = c(baseline = "D_new_users"), conns = conns))
```

    #>      baseline
    #> "D_new_users"

The synthetic medicine uses concept grouping explicitly; ingredient
grouping is the normal default when the vocabulary supplies ingredient
relationships. The era uses a 30-day gap and ends at the treatment
interval end. “New user” means no recorded use during this washout, not
no lifetime use.

``` r
dsBaseClient::ds.table("D_new_users$prior_fills", useNA = "no", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                        study
    #> D_new_users$prior_fills site
    #>                       0    1
    #>
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                        study
    #> D_new_users$prior_fills site
    #>                       0    1
    #>
    #> $output.list$TABLE_rvar.by.study_counts
    #>                        study
    #> D_new_users$prior_fills site
    #>                       0   18
    #>
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_new_users$prior_fills
    #> 0
    #> 1
    #>
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_new_users$prior_fills
    #>  0
    #> 18
    #>
    #>
    #> $validity.message
    #> [1] "Data in all studies were valid"

Zero prior fills is part of the inclusion definition, so this result is
expected.

## Define cases and the control pool

*Risk-set sampling* selects controls who are still eligible at each
case’s event time. A person who becomes a case later may serve as a
control earlier. For a compact teaching example the outcome is death.

``` r
outcome <- omop_event("death")
cases <- omop_population("cases", index = omop_index(outcome),
  filters = list(omop_observed(before = 365)))
controls <- omop_population("controls",
  filters = list(omop_observed(before = 365)))
class(cases)
```

    #> [1] "omop_population" "list"

The control criteria are evaluated at the case’s date. The sampler also
checks that a control is alive and has not had the outcome on or before
that date.

## Match at case time

``` r
matched <- omop_population("matched", match = omop_match(
  cases = "cases", controls = "controls", outcome = outcome,
  match = list("calendar_year"), controls_per_case = 1L, seed = 42L,
  prior_outcome = "exclude", future_cases = "eligible",
  replacement = "between_sets", insufficient = "keep_available"))
class(matched)
```

    #> [1] "omop_population" "list"

The seed fixes the sampling order. Controls may be reused between sets
here; `replacement = "none"` forbids that reuse. If too few controls are
available, this policy keeps those available; `drop_case` would discard
that case’s set. Real studies may also match on sex, age bands or prior
event strata.

``` r
matched_recipe <- omop_recipe(populations = list(cases, controls, matched),
  variables = list(is_case = omop_population_column("is_case"),
    weight = omop_population_column("sampling_weight")),
  outputs = omop_output("sample", rows = "matched", format = "wide"))
print(recipe_execute(matched_recipe, out = c(sample = "D_matched"), conns = conns))
```

    #>      sample
    #> "D_matched"

The match-set key stays protected on the server. Sampling weights and
case status are available to approved downstream analyses.

``` r
dsBaseClient::ds.table("D_matched$is_case", useNA = "no", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                  study
    #> D_matched$is_case site
    #>                 0    1
    #>                 1    1
    #>
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                  study
    #> D_matched$is_case site
    #>                 0  0.5
    #>                 1  0.5
    #>
    #> $output.list$TABLE_rvar.by.study_counts
    #>                  study
    #> D_matched$is_case site
    #>                 0   18
    #>                 1   18
    #>
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_matched$is_case
    #>   0   1
    #> 0.5 0.5
    #>
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_matched$is_case
    #>  0  1
    #> 18 18
    #>
    #>
    #> $validity.message
    #> [1] "Data in all studies were valid"

These are sampled rows, including repeated controls when permitted. They
are not disease prevalence or counts of unique people. Matching defines
a design; it does not by itself remove all confounding.

## Try it

Keep every visit episode instead of just the first, and inspect the
resulting private membership schema.

Solution

``` r
all_visitors <- omop_population("visitors", index = omop_index(visits, select = "all"))
all_recipe <- omop_recipe(populations = list(all_visitors),
  outputs = omop_output("members", rows = "visitors", format = "membership"))
print(recipe_execute(all_recipe, out = c(members = "D_all_visitors"), conns = conns))
```

    #>          members
    #> "D_all_visitors"

``` r
dsBaseClient::ds.colnames("D_all_visitors", datasources = conns)
```

    #> $site
    #> [1] "row_id"            "cohort_row_id"     "person_id"         "cohort_start_date"
    #> [5] "cohort_end_date"   "index_source"

The keys distinguish episodes even when a person appears more than once.
The synthetic verification harness checks 30 episodes for 18 distinct
people.

## Exercise 2: a common variant

Inspect the new-user output. Is the washout a column created after
entry?

Solution 2

``` r
dsBaseClient::ds.colnames("D_new_users", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "prior_fills"

Washout belongs to eligibility. A baseline prior-fill count can verify
the design but cannot replace its inclusion criterion.

## Exercise 3: a pitfall to avoid

Compile the all-visit membership definition and check its index
selection.

Solution 3

``` r
all_visitors$index$select
```

    #> [1] "all"

The all selector retains repeated episodes. The episode, not necessarily
the person, is the analysis row.

## Recap

Index selection sets episodes; criteria and coverage decide eligibility.
Existing cohorts, new-user eras and risk-set samples are population
definitions in the same language.

**Next:** [8. Choosing an output
format](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md).
