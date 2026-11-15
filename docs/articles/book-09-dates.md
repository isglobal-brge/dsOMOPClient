# 9\. Dates, periods and ages

**What you will learn**

Separate date sources from presentation, choose factor/number/date
periods, extract parts, calculate anchored ages, and handle partial
dates explicitly.

This chapter uses the synthetic questionnaire dataset described in
[Getting
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

## A date source and a presentation are separate

A *source* says which date to use. A *date specification* says how to
represent it. `first_visit`, `death`, `birth`, `index`, `index_end`,
observation bounds, a fixed ISO date and `omop_anchor(event)` are date
sources. An ISO date is written `YYYY-MM-DD`, independently of the
analyst’s locale.

``` r
first_visit <- omop_date("first_visit")
class(first_visit)
```

    #> [1] "omop_variable" "list"

With no specification this is an exact R Date assigned privately on the
server. It does not download a date vector. We can inspect the column
inventory:

``` r
exact <- omop_recipe(variables = list(visit_date = first_visit),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(exact, out = c(people = "D_date_exact"), conns = conns))
```

    #>         people 
    #> "D_date_exact"

``` r
dsBaseClient::ds.colnames("D_date_exact", datasources = conns)
```

    #> $site
    #> [1] "person_id"  "visit_date"

People with no visit keep a missing date. Native `omop_date()` and
ordinary `first_date`/`last_date` reducers differ: those reducers
without a date specification return day offsets. The [visit
walkthrough](https://isglobal-brge.github.io/dsOMOPClient/articles/book-13-visits.md)
compares both.

## Month periods as categories

A *period* is a calendar block, such as a month or quarter. Its default
presentation is an ordered factor: categories with a defined
chronological order.

``` r
month_spec <- omop_date_spec(precision = "month", representation = "factor",
  level_range = c("2020-03-01", "2020-07-31"))
class(month_spec)
```

    #> [1] "omop_date_spec" "list"

The public range declares common levels, including empty months. It
never filters observed dates. Do not derive a “public” range from
private minimum/maximum dates.

``` r
months <- omop_recipe(variables = list(month = omop_date("first_visit", month_spec)),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(months, out = c(people = "D_date_months"), conns = conns))
```

    #>          people 
    #> "D_date_months"

``` r
ds.omop.table("D_date_months", "month", conns = conns)
```

    #> D_date_months$month
    #> 2020-03 2020-04 2020-05 2020-06 2020-07 
    #>       6       0       0       6       6

April and May remain declared zero categories. Ordered factors have
ordered-factor model contrasts; for a linear time trend request a number
explicitly.

## Periods as numbers

``` r
numbers <- omop_recipe(variables = list(month_index = omop_date("first_visit",
  omop_date_spec(precision = "month", representation = "number",
    numeric = "index", origin = "2020-01-01"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(numbers, out = c(people = "D_date_numbers"), conns = conns))
```

    #>           people 
    #> "D_date_numbers"

``` r
dsBaseClient::ds.summary("D_date_numbers$month_index", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #> 2.000000 2.000000 2.000000 5.000000 6.000000 6.000000 6.000000 4.333333

January 2020 is zero, February one and March two. These are period
coordinates, not elapsed months since each person’s birthday.
Year/fiscal-year numbers instead use the year; `numeric =
"decimal_year"` supplies a fractional period coordinate.

## Periods as boundary dates

``` r
month_start <- omop_date("first_visit",
  omop_date_spec(precision = "month", representation = "start"))
month_end <- omop_date("first_visit",
  omop_date_spec(precision = "month", representation = "end"))
class(month_end)
```

    #> [1] "omop_variable" "list"

Start and end are Date objects, with inclusive month bounds. They
deliberately coarsen the original day; they do not claim that the visit
occurred at a boundary.

``` r
bounds <- omop_recipe(variables = list(start = month_start, end = month_end),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(bounds, out = c(people = "D_date_bounds"), conns = conns))
```

    #>          people 
    #> "D_date_bounds"

``` r
print(ds.omop.mutate("D_date_bounds", days = end - start + 1L,
  newobj = "D_month_lengths", conns = conns))
```

    #> [1] "D_month_lengths"

``` r
dsBaseClient::ds.summary("D_month_lengths$days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #> 30.00000 30.00000 30.00000 31.00000 31.00000 31.00000 31.00000 30.66667

The inclusive length adds one day. Different months need different
lengths; a fixed 30-day bin is a different definition.

## Extract date parts

A *part* is a component such as year, month or weekday. Named entries
choose the final column names in this bundle.

``` r
parts <- omop_recipe(variables = list(visit = omop_date_parts("first_visit",
  c(visit_year = "year", visit_month = "month", weekday = "weekday_name"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(parts, out = c(people = "D_date_parts"), conns = conns))
```

    #>         people 
    #> "D_date_parts"

``` r
ds.omop.table("D_date_parts", "weekday", conns = conns)
```

    #> D_date_parts$weekday
    #>    Monday   Tuesday Wednesday  Thursday    Friday  Saturday    Sunday 
    #>        12         0         0         0         0         6         0

Weekday names use a fixed English domain, with Monday first. Month
categories likewise include all twelve months. Year, ISO week/year,
quarter, day-of-year and cyclic sine/cosine parts are also available.
For season parts, declare the intended hemisphere (the default is
north); using only a week number without its week-year can mix different
years.

**Tip**

Choose a calendar coordinate for a time trend and an elapsed duration
for time between events. They answer different questions.

## Ages at any anchor

`omop_age_at()` measures completed birthdays at the chosen date. The
anchor below selects the first education observation for each person,
not the first visit.

``` r
education_date <- omop_anchor(omop_event("observation", concepts = 900001L))
ages <- omop_recipe(variables = list(at_death = omop_age_at("death"),
  at_question = omop_age_at(education_date)),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(ages, out = c(people = "D_date_ages"), conns = conns))
```

    #>        people 
    #> "D_date_ages"

``` r
dsBaseClient::ds.summary("D_date_ages$at_death", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>   33   33   33   38   43   43   43   38

Missing death or incomplete birth dates can leave age missing. A
February 29 birthday completes on March 1 in a non-leap year. Exact age
does not equal year subtraction.

``` r
dsBaseClient::ds.summary("D_date_ages$at_question", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>   29   29   29   34   39   39   39   34

The selected event sets a different age reference. An index, an
observation bound or a fixed ISO date works through the same API.

## Partial dates: retain what is known

One synthetic birth pattern has a year but no month/day. The default
`partial = "none"` keeps an invariant year but cannot invent a day or
completed age.

``` r
partial <- omop_recipe(variables = list(birth_year = omop_date("birth",
  omop_date_spec(precision = "year"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(partial, out = c(people = "D_date_partial"), conns = conns))
```

    #>           people 
    #> "D_date_partial"

``` r
ds.omop.table("D_date_partial", "birth_year", conns = conns)
```

    #> D_date_partial$birth_year
    #> 1975 1980 1990 2000 
    #>    6    6    6    6

The year is known for all four patterns. If the study permits an
estimate, declare imputation instead of letting an implicit convention
choose it.

``` r
estimated <- omop_recipe(variables = list(age = omop_age_at("death",
  date_spec = omop_date_spec(partial = "mid"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(estimated, out = c(people = "D_date_estimated"), conns = conns))
```

    #>             people 
    #> "D_date_estimated"

``` r
dsBaseClient::ds.summary("D_date_estimated$age", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>   5%  10%  25%  50%  75%  90%  95% Mean 
    #>   23   23   23   33   43   43   43   33

The middle possible birth day supplies an **estimated** completed age.
`start` and `end` use the first/last possible day. `unit =
"annual_years"` explicitly asks for calendar-year subtraction instead.
Invalid components stay missing even with imputation. `future = "na"`
requires a fixed `as_of` date; do not use a moving “today” cutoff in a
reproducible plan.

## Custom labels without arbitrary date code

``` r
custom <- omop_recipe(variables = list(month = omop_date("first_visit",
  omop_date_spec(precision = "custom", pattern = "%b %Y"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(custom, out = c(people = "D_date_custom"), conns = conns))
```

    #>          people 
    #> "D_date_custom"

``` r
ds.omop.table("D_date_custom", "month", conns = conns)
```

    #> D_date_custom$month
    #> Mar 2020 Jun 2020 Jul 2020 
    #>        6        6        6

This uses fixed English month abbreviations and a year. The supported
directives are `%Y`, `%m`, `%d`, `%j`, `%G`, `%V`, `%u`, `%b`, `%B`,
`%a`, `%A` and `%%`, with simple literal text/punctuation. ISO weeks
require `%G-W%V`, not `%Y-W%V`. Arbitrary `strftime` directives,
locale-dependent code and time-of-day are refused. A named `labels` map
may rename public date categories but must keep them unique; it cannot
merge dates. Fine-grained factor labels still face disclosure checks.

## Other calendar systems

| Precision                                                      | Meaning or required declaration                                        |
| -------------------------------------------------------------- | ---------------------------------------------------------------------- |
| year, semester, quarter, month                                 | Calendar year; halves, quarters, months                                |
| iso\_week                                                      | Monday weeks; week 1 has at least four new-year days                   |
| cdc\_week                                                      | Sunday weeks; week 1 has at least four new-year days                   |
| epi\_week                                                      | Supply `week_start` and `week_min_days`                                |
| fiscal\_year, fiscal\_semester, fiscal\_quarter, fiscal\_month | Choose `fiscal_start` (default January); labels name the starting year |
| relative\_days                                                 | Supply an anchor for day offsets                                       |
| day                                                            | Exact private Date                                                     |
| custom                                                         | Restricted public pattern                                              |

For example, choose an April-start fiscal quarter explicitly:

``` r
fiscal <- omop_recipe(variables = list(quarter = omop_date("first_visit",
  omop_date_spec(precision = "fiscal_quarter", fiscal_start = 4L))),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(fiscal, out = c(people = "D_date_fiscal"), conns = conns))
```

    #>          people 
    #> "D_date_fiscal"

``` r
ds.omop.table("D_date_fiscal", "quarter", conns = conns)
```

    #> D_date_fiscal$quarter
    #> 2019-FQ4 2020-FQ1 2020-FQ2 
    #>        6        6        6

March belongs to the preceding fiscal year. The variable’s specification
overrides the recipe’s specification, which overrides the defaults.

## Try it

Calculate elapsed days from first visit to death and request a summary.
Why are some values missing?

Solution

``` r
elapsed <- omop_recipe(variables = list(days = omop_time_between("first_visit", "death")),
  outputs = omop_output("people", format = "wide", grain = "person"))
book_symbols(recipe_execute(elapsed, out = c(people = "D_date_elapsed"), conns = conns))
```

    #>           people 
    #> "D_date_elapsed"

``` r
dsBaseClient::ds.summary("D_date_elapsed$days", datasources = conns)
```

    #> $site
    #> $site$class
    #> [1] "integer"
    #> 
    #> $site$length
    #> [1] 24
    #> 
    #> $site$`quantiles & mean`
    #>       5%      10%      25%      50%      75%      90%      95%     Mean 
    #> 1390.000 1390.000 1390.000 1447.000 1461.000 1461.000 1461.000 1432.667

Elapsed days need both dates. This calculation does not need a complete
birth date, unlike exact age. It is a duration, not a calendar-period
coordinate.

## Exercise 2: a common variant

Compare the month calendar variable with the elapsed-days variable. Read
their sources and the month precision/representation instead of
inferring meaning from a number.

Solution 2

``` r
calendar <- months$variables$month
list(source_table = calendar$derived$reference$event$table,
  selection = calendar$derived$reference$which,
  precision = calendar$date_spec$precision,
  representation = calendar$date_spec$representation)
```

    #> $source_table
    #> [1] "visit_occurrence"
    #> 
    #> $selection
    #> [1] "first"
    #> 
    #> $precision
    #> [1] "month"
    #> 
    #> $representation
    #> [1] "factor"

``` r
duration <- elapsed$variables$days$derived
list(from_table = duration$from$event$table, from_selection = duration$from$which,
  to_table = duration$to$event$table, unit = duration$unit)
```

    #> $from_table
    #> [1] "visit_occurrence"
    #> 
    #> $from_selection
    #> [1] "first"
    #> 
    #> $to_table
    #> [1] "death"
    #> 
    #> $unit
    #> [1] "days"

The month variable uses `first_visit`, month precision and factor
representation. The elapsed variable measures the duration from first
visit to death. A calendar period and a day duration answer different
questions.

## Exercise 3: a pitfall to avoid

Does exporting a date recipe export any dates?

Solution 3

``` r
is.character(recipe_export_json(elapsed))
```

    #> [1] TRUE

No: JSON serialises the study definition, not the assigned patient
table.

## Recap

Choose a source, precision and representation. Partial information stays
partial unless you declare estimation. Exact dates and ages remain
private server columns.

**Next:** [10. Concepts as
categories](https://isglobal-brge.github.io/dsOMOPClient/articles/book-10-harmonise.md).
