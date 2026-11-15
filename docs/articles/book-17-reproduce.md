# 17\. Checking and recording a study

**What you will learn**

Save a study definition and its data-source information, repeat it at
another site, and inspect unsupported cohort rules before running them.

**You need:** [the setup and questionnaire
dataset](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
Start a fresh R session in `book-work`. The two questionnaire sites have
different education answers and vocabulary dates, so identical recipes
need not give identical results.

For real servers, replace the two local connection lines below with the
`conns` / `site` / `site2` variant in the [two-site Opal
instructions](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#connect-to-two-opal-sites).

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
practice <- book_connect("questionnaire", two_sites = TRUE)
conns <- practice$conns
```

## Record the data actually used

Keep the vocabulary and source information returned by these
connections. A record for another dataset would not document this study.

``` r
vocabularies <- ds.omop.vocab.vocabularies(conns = conns)$per_site
sources <- ds.omop.cdm.source(conns = conns)$per_site
vocabulary_rows <- do.call(rbind, lapply(names(vocabularies), function(site) {
  x <- vocabularies[[site]]
  x <- x[x$vocabulary_id == "Questionnaire", c("vocabulary_id", "vocabulary_version"), drop = FALSE]
  data.frame(Site = site, x)
}))
knitr::kable(vocabulary_rows)
```

| Site  | vocabulary\_id | vocabulary\_version |
| :---- | :------------- | :------------------ |
| site  | Questionnaire  | 2025-01-01          |
| site2 | Questionnaire  | 2026-01-01          |

The questionnaire vocabulary dates are 2025-01-01 and 2026-01-01. Retain
both: newer wording can change labels even when concept IDs stay the
same.

## Define and inspect the study

Education question **900001** was found in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md).
Include education so that a repeated run can reveal differences between
sites. Death year is an ordered calendar category; age is completed age
at death.

``` r
education <- omop_variable(event = omop_event("observation", 900001L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
check <- omop_recipe(variables = list(education = education,
  age = omop_age_at("death"),
  death_period = omop_date("death", omop_date_spec(precision = "year"))),
  outputs = omop_output("people", format = "wide", grain = "person"))
result <- recipe_execute(check, out = c(people = "D_documented"), conns = conns["site"])
published <- recipe_codebook(result)
knitr::kable(published[[1]]$columns[c("name", "semantic_type", "r_type", "unit")])
```

| name          | semantic\_type | r\_type         | unit            |
| :------------ | :------------- | :-------------- | :-------------- |
| person\_id    | key            | character       | not\_applicable |
| education     | concept        | factor          | not\_applicable |
| age           | numeric        | integer         | years           |
| death\_period | date           | ordered\_factor | not\_applicable |

Read `r_type`: age is numeric, while education and death period are
factors. Keep this codebook to interpret the assigned columns later; it
does not report how many people have missing values.

## Save the study record

The recipe stores the selection rules. Its explanation makes those rules
readable, and its digest identifies the definition. Save the codebook,
software information, and both sites’ source metadata alongside it.
These files contain definitions and metadata, not patient rows.

``` r
record_dir <- "questionnaire-study"
dir.create(record_dir, showWarnings = FALSE)
recipe_save(check, file.path(record_dir, "recipe.json"))
explanation <- recipe_explain(check)
writeLines(explanation$text, file.path(record_dir, "explanation.txt"))
writeLines(explanation$digest, file.path(record_dir, "plan-digest.txt"))
saveRDS(published, file.path(record_dir, "codebook-site.rds"))
saveRDS(vocabularies, file.path(record_dir, "vocabularies.rds"))
saveRDS(sources, file.path(record_dir, "sources.rds"))
package_versions <- vapply(c("dsOMOPClient", "DSI", "dsBaseClient"),
  function(p) as.character(packageVersion(p)), character(1))
write.csv(data.frame(Package = names(package_versions), Release = unname(package_versions)),
  file.path(record_dir, "client-packages.csv"), row.names = FALSE)
saveRDS(ds.omop.server.version(conns = conns), file.path(record_dir, "server-packages.rds"))
knitr::kable(data.frame(File = list.files(record_dir)))
```

| File                |
| :------------------ |
| client-packages.csv |
| codebook-site.rds   |
| explanation.txt     |
| plan-digest.txt     |
| recipe.json         |
| server-packages.rds |
| sources.rds         |
| vocabularies.rds    |

The filenames identify the definition, software and data sources needed
to repeat the study. Matching digests establish matching rules; they
cannot establish that a database has stayed unchanged.

## Run the saved definition at the second site

``` r
restored <- recipe_load(file.path(record_dir, "recipe.json"))
second_result <- recipe_execute(restored, out = c(people = "D_repeated"), conns = conns["site2"])
saveRDS(recipe_codebook(second_result), file.path(record_dir, "codebook-site2.rds"))
knitr::kable(book_counts(dsBaseClient::ds.table("D_repeated$death_period",
  useNA = "always", datasources = conns["site2"])))
```

| Category | Count |
| :------- | ----: |
| 2023     |    18 |
| 2024     |    18 |
| NA       |    12 |

The second site has 18 deaths in 2023, 18 in 2024, and 12 people without
a death year. The counts describe its data; the saved recipe describes
how those categories were obtained.

## Exercises

**1. Compare data, not just definitions.** Tabulate education at each
site after the separate runs. What changed?

Solution 1

``` r
first_counts <- book_counts(dsBaseClient::ds.table("D_documented$education",
  useNA = "always", datasources = conns["site"]))
second_counts <- book_counts(dsBaseClient::ds.table("D_repeated$education",
  useNA = "always", datasources = conns["site2"]))
knitr::kable(rbind(transform(first_counts, Site = "site"),
                  transform(second_counts, Site = "site2")))
```

| Category                 | Count | Site  |
| :----------------------- | ----: | :---- |
| Secondary education      |    12 | site  |
| University education     |    12 | site  |
| NA                       |    24 | site  |
| University qualification |    24 | site2 |
| NA                       |    24 | site2 |

Site has 12 Secondary and 12 University answers; site2 has 24 University
answers. Both have 24 missing answers. The newer catalogue calls
University education “University qualification”. Because these were
separate executions, their labels were resolved separately; a joint
execution reconciles them as shown in chapter 10.

**2. Inspect an unsupported cohort rule.** Circe JSON describes an OHDSI
cohort. Does a successful strict import establish the intended clinical
meaning?

Solution 2

``` r
circe_json <- jsonlite::toJSON(list(AdditionalCriteria = list(Type = "ALL",
  CriteriaList = list(list(unsupported = TRUE)))), auto_unbox = TRUE)
circe_check <- recipe_circe_check(circe_json)
issues <- circe_check$issues[c("path", "remedy")]
issues$path <- paste0("`", issues$path, "`")
knitr::kable(issues, row.names = FALSE)
```

| path                   | remedy                                                                         |
| :--------------------- | :----------------------------------------------------------------------------- |
| `$.AdditionalCriteria` | AdditionalCriteria has no implemented adapter; retain the complete definition. |
| `$.PrimaryCriteria`    | Provide an object with the documented structural fields.                       |

The check identifies structural problems in this example, so it is not
ready for extraction. Attempt the strict import and read its refusal:

``` r
tryCatch(recipe_import_circe(circe_json),
  error = function(e) cat(conditionMessage(e), "\n"))
```

    #> unsupported_circe_construct: $.AdditionalCriteria; AdditionalCriteria has no implemented adapter; retain the complete definition. Inspect recipe_circe_check() for all structural issues.

`unsupported_circe_construct` identifies the unsupported
`AdditionalCriteria` rule. Keep the complete cohort definition and
implement that rule in a reviewed native recipe before extracting. A
successful strict import means every construct had an adapter; you must
still compare the translated population with the intended inclusion and
exclusion rules. Never delete a scientific criterion merely to make an
import succeed.

**3. Read the actual calendar levels.** Which years can the assigned
death-period column represent, and are they ordered?

Solution 3

``` r
columns <- recipe_codebook(result, output = "people", component = "people")[[1]]$columns
period <- match("death_period", columns$name)
knitr::kable(data.frame(Year = columns$levels[[period]]$labels,
  Type = columns$r_type[[period]]))
```

| Year | Type            |
| :--- | :-------------- |
| 2023 | ordered\_factor |
| 2024 | ordered\_factor |

The levels are 2023 and 2024 in chronological order. A column called
“year” could instead be numeric or a Date under a different
declaration; its name alone is insufficient.

## Exercise 4: preserve a required clean period

Suppose a study requires 365 observed days without the outcome before
entry. Can the incidence-rate analysis below implement that requirement?
If it refuses, which scientific rule must the alternative preserve?

Solution: keep the outcome-free observation period

``` r
tryCatch(ds.omop.analysis.run("dsomop:incidence.rate",
  params = list(clean_window = 365), conns = conns),
  error = function(e) cat(conditionMessage(e), "\n"))
```

    #> unsupported_clean_window: params.clean_window; zero selects the descriptive first-episode/full-window estimand; use a validated native risk recipe when a clean period is required

`unsupported_clean_window` means this analysis does not implement a
nonzero clean period. Setting it to zero would answer a different
question. Use a validated native risk recipe: require continuous
observation for the preceding 365 days, exclude entrants with the
outcome in days −365 through −1, and declare the subsequent time at
risk, outcome and censoring rules. The [population
lessons](https://isglobal-brge.github.io/dsOMOPClient/articles/book-07-populations.md)
introduce eligibility, and the [drug
comparison](https://isglobal-brge.github.io/dsOMOPClient/articles/book-20-case-gibleed.md)
shows explicit prior-event exclusion and follow-up. Adapt those rules to
the protocol and check eligibility and person-time before estimating the
rate; no recorded outcome alone does not establish a fully observed
clean period. Retain the refusal and the reviewed replacement definition
with the study record.

## Recap

Keep the rules, codebook, software and actual source metadata together.
Repeating a definition at another site is an opportunity to check data
differences, not a reason to assume identical answers.

**Next:** [18. Capstone: a cause-of-death wide
table](https://isglobal-brge.github.io/dsOMOPClient/articles/book-18-capstone.md).
