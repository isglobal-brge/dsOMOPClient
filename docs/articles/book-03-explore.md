# 3\. Exploring the CDM

**What you will learn**

Find the right fields, distinguish rows from people, and check numeric
coverage.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.gibleed`.

## Start this chapter

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

## Read metadata before selecting records

A table’s schema describes its fields, even when the table is empty. The
metadata returned by `ds.omop.columns()` help you decide which field to
use. We display only the rows and columns needed for each decision.

| Metadata                    | How to read it                                                                                           |
| :-------------------------- | :------------------------------------------------------------------------------------------------------- |
| `cdm_datatype`, `is_date`   | The field’s expected type and whether it represents a date.                                              |
| `is_blocked`                | The field is unavailable for analytical extraction, for example free text or a source value.             |
| `is_quasi_identifier`       | Values such as dates and ages could help identify someone; they remain private and releases are checked. |
| `concept_role`, `fk_domain` | Whether a code is a standard target or source code, and which kind of concept it should reference.       |
| `is_extension`              | The field is an addition to the standard CDM schema.                                                     |
| `is_untyped_identifier`     | The field looks like an identifier but has no recognised standard role.                                  |
| `is_sensitive`              | The field is flagged as sensitive, for example free text or an original source value.                    |
| `semantic_class`            | The kind of field, such as `concept` or `ordinary`; this alone does not determine what can be released.  |

A FALSE flag is not permission to download rows. These flags describe
fields; the server also checks the operation and the support for each
result.

## Separate what was observed from its value

``` r
columns <- ds.omop.columns("observation", datasources = conns)$site
subset(columns, column_name %in% c("observation_concept_id", "value_as_concept_id",
  "value_as_number", "value_as_string", "observation_date"),
  select = c(column_name, cdm_datatype, is_date, is_blocked))
```

| column\_name             | cdm\_datatype | is\_date | is\_blocked |
| :----------------------- | :------------ | :------- | :---------- |
| observation\_concept\_id | integer       | FALSE    | FALSE       |
| observation\_date        | date          | TRUE     | FALSE       |
| value\_as\_number        | float         | FALSE    | FALSE       |
| value\_as\_string        | varchar(60)   | FALSE    | TRUE        |
| value\_as\_concept\_id   | Integer       | FALSE    | FALSE       |

`observation_concept_id` identifies what was observed; in a
questionnaire it identifies the question. `value_as_concept_id` holds a
coded value or answer, and `value_as_number` a numeric value.
`value_as_string` is blocked free text. Selecting the observation and
choosing its value field are separate decisions.

``` r
ds.omop.table.stats("observation", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $rows
    #> [1] 1475
    #>
    #> $persons
    #> [1] 670
    #>
    #>
    #> $pooled: the same as $site (one server)

The bands begin at 1,475 rows and 670 people: one person can contribute
several observations. These represent 1,475–1,479 rows and 670–674
people. GiBleed’s observations describe allergies and histories, not
education answers. The questionnaire chapters use a separate dataset for
those answers.

## Find the visit date and setting

``` r
columns <- ds.omop.columns("visit_occurrence", datasources = conns)$site
subset(columns, column_name %in% c("visit_start_date", "visit_end_date",
  "visit_concept_id", "visit_type_concept_id"),
  select = c(column_name, cdm_datatype, is_date, is_blocked))
```

| column\_name             | cdm\_datatype | is\_date | is\_blocked |
| :----------------------- | :------------ | :------- | :---------- |
| visit\_concept\_id       | integer       | FALSE    | FALSE       |
| visit\_start\_date       | date          | TRUE     | FALSE       |
| visit\_end\_date         | date          | TRUE     | FALSE       |
| visit\_type\_concept\_id | Integer       | FALSE    | FALSE       |

`visit_start_date` is when the visit started. `visit_concept_id`
describes its setting, such as inpatient or outpatient.
`visit_type_concept_id` describes where the record came from; use the
setting field to select inpatient visits.

``` r
ds.omop.table.stats("visit_occurrence", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $rows
    #> [1] 1035
    #>
    #> $persons
    #> [1] 890
    #>
    #>
    #> $pooled: the same as $site (one server)

The bands are 1,035–1,039 visits and 890–894 people. Repeated visits
explain why rows exceed people; a visit count is not a study sample
size.

## Exercise 1: a coded cause does not guarantee a death record

Find the coded cause and death date. Then check whether the server can
release a count of deaths.

Solution

``` r
columns <- ds.omop.columns("death", datasources = conns)$site
subset(columns, column_name %in% c("death_date", "cause_concept_id", "cause_source_value"),
  select = c(column_name, cdm_datatype, is_date, is_blocked))
```

| column\_name         | cdm\_datatype | is\_date | is\_blocked |
| :------------------- | :------------ | :------- | :---------- |
| death\_date          | date          | TRUE     | FALSE       |
| cause\_concept\_id   | integer       | FALSE    | FALSE       |
| cause\_source\_value | varchar(50)   | FALSE    | TRUE        |

`cause_concept_id` is the coded cause. `cause_source_value` is the
original source code, for example an ICD code, and is blocked here. The
date field’s existence does not establish that any deaths were recorded.

``` r
ds.omop.table.stats("death", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #> Not pooled: No complete disclosure-safe pooled table statistics were available.
    #>
    #> $site
    #> list()

The empty response contains no count. The public GiBleed source has an
empty death table; through DataSHIELD alone this response could also
mean a small withheld count. Treat it as unavailable, rather than
asserting zero deaths.

## Exercise 2: distinguish dispensing from visits

Find the drug exposure dates and supply duration. Does supply duration
establish that somebody took the medicine every day?

Solution

``` r
columns <- ds.omop.columns("drug_exposure", datasources = conns)$site
subset(columns, column_name %in% c("drug_exposure_start_date",
  "drug_exposure_end_date", "days_supply"),
  select = c(column_name, cdm_datatype, is_date, is_blocked))
```

| column\_name                | cdm\_datatype | is\_date | is\_blocked |
| :-------------------------- | :------------ | :------- | :---------- |
| drug\_exposure\_start\_date | date          | TRUE     | FALSE       |
| drug\_exposure\_end\_date   | date          | TRUE     | FALSE       |
| days\_supply                | integer       | FALSE    | FALSE       |

The two dates delimit the recorded exposure and `days_supply` describes
the supplied duration. These prescribing or dispensing fields do not
prove ingestion. They answer a different question from a visit start
date.

## Exercise 3: test numeric coverage

Does the presence of a measurement table and a `value_as_number` column
guarantee that lab values can be analysed? Check the column itself.

Solution

``` r
ds.omop.table.stats("measurement", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $rows
    #> [1] 44050
    #>
    #> $persons
    #> [1] 2685
    #>
    #>
    #> $pooled: the same as $site (one server)

The row band begins at 44,050. The public GiBleed data have exactly
44,053 measurement rows; this analyst call rounds down to its band.

``` r
ds.omop.column.stats("measurement", "value_as_number", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: split (per-server results)
    #>
    #> $site
    #> $n_total
    #> [1] 44050
    #>
    #> $n_missing
    #> [1] 2685
    #>
    #> $n_distinct
    #> [1] 0
    #>
    #> $n_persons
    #> [1] 2685
    #>
    #> $mean
    #> [1] NA
    #>
    #> $sd
    #> [1] NA

`n_missing` equals `n_persons`, both 2,685 (the band 2,685–2,689). The
same response reports `n_distinct = 0`: no distinct numeric values are
available. The public GiBleed data contain no numeric lab values. Use
Synthea27Nj for the lab lessons, and check the chosen lab and unit there
before fitting a model.

## Recap

A schema tells you which fields exist. Protected counts describe their
support. Neither a large row count nor a numeric field guarantees usable
lab values.

**Next:** [4. Finding the concept IDs you
need](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.md).
