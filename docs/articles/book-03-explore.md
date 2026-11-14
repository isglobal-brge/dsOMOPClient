# 3\. Exploring the CDM

**What you will learn**

Read table and column metadata, then interpret safe counts.

This chapter uses the public synthetic GiBleed practice connection from
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## Find a table

``` r
available <- ds.omop.tables(conns = conns)
knitr::kable(available$site)
```

| table\_name              | schema\_category | has\_person\_id | concept\_prefix |
| :----------------------- | :--------------- | :-------------- | :-------------- |
| person                   | CDM              | TRUE            |                 |
| observation\_period      | CDM              | TRUE            |                 |
| visit\_occurrence        | CDM              | TRUE            | visit           |
| visit\_detail            | CDM              | TRUE            | visit\_detail   |
| condition\_occurrence    | CDM              | TRUE            | condition       |
| drug\_exposure           | CDM              | TRUE            | drug            |
| procedure\_occurrence    | CDM              | TRUE            | procedure       |
| device\_exposure         | CDM              | TRUE            | device          |
| measurement              | CDM              | TRUE            | measurement     |
| observation              | CDM              | TRUE            | observation     |
| death                    | CDM              | TRUE            |                 |
| note                     | CDM              | TRUE            |                 |
| note\_nlp                | CDM              | FALSE           |                 |
| specimen                 | CDM              | TRUE            | specimen        |
| fact\_relationship       | CDM              | FALSE           |                 |
| location                 | CDM              | FALSE           |                 |
| care\_site               | CDM              | FALSE           |                 |
| provider                 | CDM              | FALSE           |                 |
| payer\_plan\_period      | CDM              | TRUE            |                 |
| cost                     | CDM              | FALSE           |                 |
| drug\_era                | CDM              | TRUE            |                 |
| dose\_era                | CDM              | TRUE            |                 |
| condition\_era           | CDM              | TRUE            |                 |
| metadata                 | CDM              | FALSE           |                 |
| cdm\_source              | CDM              | FALSE           |                 |
| concept                  | Vocabulary       | FALSE           |                 |
| vocabulary               | Vocabulary       | FALSE           |                 |
| domain                   | Vocabulary       | FALSE           |                 |
| concept\_class           | Vocabulary       | FALSE           |                 |
| concept\_relationship    | Vocabulary       | FALSE           |                 |
| relationship             | Vocabulary       | FALSE           |                 |
| concept\_synonym         | Vocabulary       | FALSE           |                 |
| concept\_ancestor        | Vocabulary       | FALSE           |                 |
| source\_to\_concept\_map | Vocabulary       | FALSE           |                 |
| drug\_strength           | Vocabulary       | FALSE           |                 |

Check that `observation`, `death` and `visit_occurrence` exist before
designing a recipe. A CDM may contain an empty table or omit a table
altogether.

## Read the question and answer fields

``` r
knitr::kable(ds.omop.columns("observation", conns = conns)$site)
```

| column\_name                     | cdm\_datatype | db\_datatype | concept\_role      | semantic\_class | fk\_domain   | is\_date | is\_extension | is\_untyped\_identifier | is\_sensitive | is\_blocked | is\_quasi\_identifier |
| :------------------------------- | :------------ | :----------- | :----------------- | :-------------- | :----------- | :------- | :------------ | :---------------------- | :------------ | :---------- | :-------------------- |
| observation\_id                  | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| person\_id                       | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| observation\_concept\_id         | integer       | real         | domain\_concept    | concept         |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| observation\_date                | date          | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| observation\_datetime            | datetime      | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| observation\_type\_concept\_id   | integer       | real         | type\_concept      | concept         | Type Concept | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| value\_as\_number                | float         | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| value\_as\_string                | varchar(60)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| value\_as\_concept\_id           | Integer       | real         | attribute\_concept | concept         |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| qualifier\_concept\_id           | integer       | real         | attribute\_concept | concept         |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| unit\_concept\_id                | integer       | real         | attribute\_concept | concept         | Unit         | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| provider\_id                     | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| visit\_occurrence\_id            | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| visit\_detail\_id                | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| observation\_source\_value       | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| observation\_source\_concept\_id | integer       | real         | source\_concept    | concept         |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| unit\_source\_value              | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| qualifier\_source\_value         | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |

`observation_concept_id` identifies the question. `value_as_concept_id`
identifies a coded answer. Filtering on the question and reading the
answer are two separate choices. `value_as_number` is for numeric
values; it is not our education answer.

## Count safely

``` r
ds.omop.table.stats("observation", conns = conns)$per_site$site
```

    #> $rows
    #> [1] 1475
    #> 
    #> $persons
    #> [1] 670

An observation row is an answer, not necessarily a distinct person.
Repeated answers mean that counting rows and counting people can give
different results. Respect the returned disclosure representation; do
not reinterpret a band as an exact number or a refused result as an
empty table.

## Identify the date field

``` r
knitr::kable(ds.omop.columns("visit_occurrence", conns = conns)$site)
```

| column\_name                     | cdm\_datatype | db\_datatype | concept\_role      | semantic\_class | fk\_domain   | is\_date | is\_extension | is\_untyped\_identifier | is\_sensitive | is\_blocked | is\_quasi\_identifier |
| :------------------------------- | :------------ | :----------- | :----------------- | :-------------- | :----------- | :------- | :------------ | :---------------------- | :------------ | :---------- | :-------------------- |
| visit\_occurrence\_id            | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| person\_id                       | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| visit\_concept\_id               | integer       | real         | domain\_concept    | concept         | Visit        | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| visit\_start\_date               | date          | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| visit\_start\_datetime           | datetime      | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| visit\_end\_date                 | date          | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| visit\_end\_datetime             | datetime      | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| visit\_type\_concept\_id         | Integer       | real         | type\_concept      | concept         | Type Concept | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| provider\_id                     | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| care\_site\_id                   | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| visit\_source\_value             | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| visit\_source\_concept\_id       | integer       | real         | source\_concept    | concept         |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| admitting\_source\_concept\_id   | integer       | real         | source\_concept    | concept         | Visit        | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| admitting\_source\_value         | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| discharge\_to\_concept\_id       | integer       | real         | attribute\_concept | concept         | Visit        | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| discharge\_to\_source\_value     | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| preceding\_visit\_occurrence\_id | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |

`visit_start_date` records when a visit started. `visit_concept_id`
describes its setting, such as inpatient or outpatient.
`visit_type_concept_id` describes the provenance of the record; it is
not the field for that setting filter.

## Recap

Schema exploration tells you which fields exist. Safe counts describe
support without returning records.

## Try it

Inspect the death table and locate its coded cause field.

Solution

``` r
knitr::kable(ds.omop.columns("death", conns = conns)$site)
```

| column\_name               | cdm\_datatype | db\_datatype | concept\_role      | semantic\_class | fk\_domain   | is\_date | is\_extension | is\_untyped\_identifier | is\_sensitive | is\_blocked | is\_quasi\_identifier |
| :------------------------- | :------------ | :----------- | :----------------- | :-------------- | :----------- | :------- | :------------ | :---------------------- | :------------ | :---------- | :-------------------- |
| person\_id                 | integer       | real         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| death\_date                | date          | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| death\_datetime            | datetime      | text         | non\_concept       | ordinary        |              | TRUE     | FALSE         | FALSE                   | FALSE         | FALSE       | TRUE                  |
| death\_type\_concept\_id   | integer       | real         | type\_concept      | concept         | Type Concept | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| cause\_concept\_id         | integer       | real         | attribute\_concept | concept         |              | FALSE    | FALSE         | FALSE                   | FALSE         | FALSE       | FALSE                 |
| cause\_source\_value       | varchar(50)   | text         | non\_concept       | ordinary        |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
| cause\_source\_concept\_id | integer       | real         | source\_concept    | concept         |              | FALSE    | FALSE         | FALSE                   | TRUE          | TRUE        | FALSE                 |
