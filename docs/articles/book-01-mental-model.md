# 1\. What DataSHIELD and OMOP mean

**What you will learn**

Distinguish the database, a private assignment and a released aggregate.

This chapter uses the public synthetic GiBleed practice connection from
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## Start this chapter

Start a fresh R session in the `book-work` directory created in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#get-the-book-helpers).
Run this setup first:

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- if (book_driver == "DSLite") book_practice("gibleed") else book_connect("gibleed")
conns <- practice$conns
names(conns)
```

    #> [1] "site"

The `site` connection is ready. The shared `connection.R` selects DSLite
or Opal and resolves the resource names.

## A shared table structure

OMOP is a common data model (CDM): a shared set of tables and column
meanings. `person` describes people; `visit_occurrence` describes
visits; `observation` holds observations and questionnaire answers;
`death` records deaths. A *concept* is a coded meaning, such as an
education answer. A vocabulary supplies its name.

Ask which tables are available:

``` r
tables <- ds.omop.tables(conns = conns)
knitr::kable(tables$site)
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

These are table names, not records. You can learn the structure without
seeing a person’s dates or answers.

## Send a question to the data

DataSHIELD sends approved operations to the server. An *assignment*
creates a private object there. An *aggregate* returns a checked
summary. The server can refuse a result that lacks enough supporting
people; a refusal is not a zero.

Ask for safe information about the person table:

``` r
ds.omop.table.stats("person", conns = conns)$per_site$site
```

    #> $rows
    #> [1] 2690
    #> 
    #> $persons
    #> [1] 2690

The displayed `2690` is the lower end of a five-person band: from 2690
through 2694 people. This endpoint rounds down to a multiple of five in
the practice configuration; it has not told you the exact count. A later
categorical table may release exact supported cells through its own
checks. Different endpoints answer different questions and have
different disclosure rules.

**You**Choose events, variables and outputs.

→

**Server**Compute with private records and apply release checks.

→

**You see**Symbol names, schemas and checked summaries.

## What stays on the server

You never download this book’s assigned person table through DataSHIELD.
`person_id` becomes a pseudonym, but pseudonyms are still private row
values. Dates and ages are also private. Coarsening dates helps define a
useful analysis; it does not grant permission to fetch rows.

The practical sequence is: choose events, describe variables, assign a
table, then request an aggregate. Chapters 5–7 walk through it one small
step at a time.

## Recap

OMOP makes data structures familiar. DataSHIELD keeps patient rows at
the server and checks released summaries.

## Try it

Find the columns of the death table without asking for its rows.

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
