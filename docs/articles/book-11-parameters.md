# 11\. Reusable studies and standalone verbs

**What you will learn**

Bind typed parameters, use omop\_study and shortcuts, then derive and
filter columns without bringing rows to your computer.

This chapter uses the public synthetic Synthea27Nj numeric connection
from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).

**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run its sections in order. Each chapter starts from a fresh practice
connection; all the outputs below come from real dsOMOP and dsOMOPClient
2.10.4 calls. The numbers describe teaching data, not clinical findings.

## Describe the setting as a parameter

A *parameter* is a named, typed placeholder in a study definition. It
lets you reuse a recipe without editing its logic. It is not R code or
SQL text.

``` r
setting <- omop_param("setting", type = "concept_set")
class(setting)
```

    #> [1] "omop_param" "list"

This parameter accepts concept sets. A separate date parameter would
declare a date type; give each changing input its own appropriate type.

## Build a template

``` r
template <- omop_study(omop_event("visit_occurrence", concepts = setting),
  outputs = "baseline", baseline = list(
    prior_visits = omop_count("visit_occurrence", window = omop_window(-365, -1)),
    age = omop_age()))
class(template)
```

    #> [1] "omop_recipe" "list"

`omop_study()` returns an ordinary recipe. It creates an indexed
population and baseline output. We explicitly exclude day zero from the
prior-visit count. The unbound template cannot execute until its
required setting is supplied.

## Bind and inspect

``` r
outpatient <- recipe_bind(template, setting = 9202L)
stopifnot(recipe_explain(outpatient, conns = conns)$selected == "v2")
recipe_explain(outpatient)$selected
```

    #> [1] "v2"

Binding fills the placeholder while preserving the template. Concept
9202 is the outpatient visit setting, not the record’s provenance type.

``` r
print(recipe_execute(outpatient, out = c(baseline = "D_param_outpatient"), conns = conns))
```

    #>             baseline 
    #> "D_param_outpatient"

``` r
dsBaseClient::ds.summary("D_param_outpatient$age", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 28
    #> 
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean 
    #>  0.000000  0.000000  0.750000  3.500000 17.000000 18.000000 18.000000  8.071429

The server computes annual age (index year minus birth year) at the
first outpatient visit. The analyst receives only the approved summary.

## Read the simple-layer defaults

``` r
print(outpatient)
```

    #> Concept presentation: factors; identity is the declared concept class, preserved through aliases and value-preserving reducers.
    #> Label precedence: variable recode, recipe recode, vocabulary name, decimal ID fallback. Explicit equal recodes merge categories.
    #> Federation: permitted ID union, including declared unused levels; newest comparable vocabulary version, then bytewise name tie. Unresolved versions never establish recency.
    #> Generated names: ASCII snake_case vocabulary tokens; colliding automatic names gain concept-ID suffixes. Vocabulary-dependent names are unresolved until permitted federation reconciliation.
    #> variables.prior_visits: concept origin follows reducer count over the declared source value; explicit variable name is retained. 
    #> === omop_recipe ===
    #> Populations ( 2 ):
    #>    base  :  All Persons 
    #>    study  :  Base Population 
    #> Variables ( 2 ):
    #>    prior_visits :  count of visit_occurrence 
    #>    age :  derived age (exact annual age) 
    #> Filters ( 0 ):
    #> Outputs ( 1 ):
    #>    baseline  [ wide ] pop: study 
    #> Study 'study' (omop_study()) defaults chosen:
    #>   - index selection "first" (default): the first candidate per person, selected before the requirements
    #>   - observation coverage unique/drop/drop (default)
    #>   - every output has episode grain; survival ties follow the priority rule; dates are relative to the index date (simple-layer choices)

The study’s default index selection is `first`. The population uses the
default coverage policy; all requirements are combined with AND.
Baseline has episode rows. For survival, supply outcomes and an explicit
follow-up definition. For example, `follow_up = list(start = 1, end
= 365, censor = c("observation_end", "death"))` describes a year after
entry, ending sooner at a listed censor. The [output
chapter](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)
executes the corresponding native survival format.

## Shortcuts expand into ordinary variables

Use a shortcut when its defaults match your question; use an explicit
reducer when you need more control. `before = 365` includes day zero,
while `after = 365` starts at day one. Without either, most shortcuts
use all history through entry.

``` r
shortcuts <- omop_study(omop_event("measurement", 3000963L), outputs = "baseline",
  baseline = list(ever = omop_ever("measurement", 3000963L, before = 365),
    count = omop_count("measurement", 3000963L, before = 365),
    value = omop_value_at_index("measurement", 3000963L,
      tolerance = c(-30, 0), unit = 8713L)))
stopifnot(recipe_explain(shortcuts, conns = conns)$selected == "v2")
print(recipe_execute(shortcuts, out = c(baseline = "D_shortcuts"), conns = conns))
```

    #>      baseline 
    #> "D_shortcuts"

``` r
dsBaseClient::ds.table("D_shortcuts$ever", useNA = "no", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>                 study
    #> D_shortcuts$ever site
    #>                1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>                 study
    #> D_shortcuts$ever site
    #>                1    1
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>                 study
    #> D_shortcuts$ever site
    #>                1   28
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_shortcuts$ever
    #> 1 
    #> 1 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_shortcuts$ever
    #>  1 
    #> 28 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

Everyone has ever = 1 because the index hemoglobin measurement itself is
included. To ask about measurements strictly before entry use `window =
omop_window(-365, -1)` instead. First/last/mean/min/max-value shortcuts
use the same event and window model. `omop_history()` bundles named ever
variables; it does not define a new language.

## Mutate a protected table

*Mutate* adds or replaces a column inside the server object. It accepts
a closed set of arithmetic and conditional expressions, not arbitrary R
functions.

``` r
print(ds.omop.mutate("D_shortcuts", twice = count * 2L,
  high = ifelse(value >= 14, 1L, 0L), newobj = "D_derived", conns = conns))
```

    #> [1] "D_derived"

``` r
dsBaseClient::ds.colnames("D_derived", datasources = conns)
```

    #> $site
    #> [1] "row_id"        "cohort_row_id" "person_id"     "ever"          "count"        
    #> [6] "value"         "twice"         "high"

Bare names refer to columns. Unknown values stay unknown in the
comparison. `newobj` makes the output explicit; omitting the input would
use the most recent output and make a tutorial harder to follow.

``` r
dsBaseClient::ds.table("D_derived$high", useNA = "always", datasources = conns)
```

    #> $output.list
    #> $output.list$TABLE_rvar.by.study_row.props
    #>               study
    #> D_derived$high site
    #>             0     1
    #>             1     1
    #>             NA  NaN
    #> 
    #> $output.list$TABLE_rvar.by.study_col.props
    #>               study
    #> D_derived$high      site
    #>             0  0.5357143
    #>             1  0.4642857
    #>             NA 0.0000000
    #> 
    #> $output.list$TABLE_rvar.by.study_counts
    #>               study
    #> D_derived$high site
    #>             0    15
    #>             1    13
    #>             NA    0
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_proportions
    #> D_derived$high
    #>     0     1    NA 
    #> 0.536 0.464 0.000 
    #> 
    #> $output.list$TABLES.COMBINED_all.sources_counts
    #> D_derived$high
    #>  0  1 NA 
    #> 15 13  0 
    #> 
    #> 
    #> $validity.message
    #> [1] "Data in all studies were valid"

The high flag remains private; this table counts its values. Division by
zero and nonfinite arithmetic results become missing. `case_when`,
`coalesce` and `cut` are supported for declared recoding and bands;
arbitrary function evaluation is refused.

## Filter using a predicate

A *predicate* is a condition. Only TRUE rows survive a filter; missing
conditions are dropped. Filtering creates another private object with
its own release check.

``` r
print(ds.omop.filter("D_derived", expr = omop_where(high == 1L),
  newobj = "D_high", conns = conns))
```

    #> [1] "D_high"

``` r
dsBaseClient::ds.summary("D_high$value", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "numeric"
    #> 
    #> $site$length
    #> [1] 13
    #> 
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #> 14.48000 14.68000 15.10000 15.50000 16.10000 16.96000 17.18000 15.65385

The cutoff of 14 g/dL is an arbitrary teaching value, not a clinical
threshold. This subset is large enough in Synthea27Nj. A predicate
cannot read protected person/episode keys to target a known person.
These verbs work on assigned `omop.table` objects, not on arbitrary
database tables.

## Recap

Parameters change declared inputs, shortcuts express common native
recipes, and mutate/filter operate privately on assigned tables. Each
final object still passes publication checks.

## Try it

Bind the same template to inpatient visits and summarise the prior-visit
count. Do not alter the outpatient template in place.

Solution

``` r
inpatient <- recipe_bind(template, setting = 9201L)
stopifnot(recipe_explain(inpatient, conns = conns)$selected == "v2")
print(recipe_execute(inpatient, out = c(baseline = "D_param_inpatient"), conns = conns))
```

    #>            baseline 
    #> "D_param_inpatient"

``` r
dsBaseClient::ds.summary("D_param_inpatient$prior_visits", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 7
    #> 
    #> $site$`quantiles & mean`
    #>        5%       10%       25%       50%       75%       90%       95%      Mean 
    #>  2.000000  2.000000  2.500000  6.000000  8.500000 11.000000 12.500000  6.285714

Each binding is a separate recipe. Its own index changes which earlier
records fall into the lookback window. A numeric summary suits these
varied counts; a categorical table can be refused when it creates too
many distinct levels in a small population.
