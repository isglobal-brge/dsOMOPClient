# 1\. What DataSHIELD and OMOP mean

**What you will learn**

Distinguish the database, a private assignment and a released aggregate.

This chapter uses the synthetic practice connection from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## A shared table structure

OMOP is a common data model (CDM): a shared set of tables and column
meanings. `person` describes people; `visit_occurrence` describes
visits; `observation` holds observations and questionnaire answers;
`death` records deaths. A *concept* is a coded meaning, such as an
education answer. A vocabulary supplies its name.

Ask which tables are available:

``` r
tables <- ds.omop.tables(conns = conns)
tables$site
#>            table_name schema_category has_person_id concept_prefix
#> 1              person             CDM          TRUE           <NA>
#> 2  observation_period             CDM          TRUE           <NA>
#> 3    visit_occurrence             CDM          TRUE          visit
#> 10        observation             CDM          TRUE    observation
#> 11              death             CDM          TRUE           <NA>
#> 27         cdm_source             CDM         FALSE           <NA>
#> 28            concept      Vocabulary         FALSE           <NA>
#> 29         vocabulary      Vocabulary         FALSE           <NA>
#> 35   concept_ancestor      Vocabulary         FALSE           <NA>
```

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
#> $rows
#> [1] 20
#> 
#> $persons
#> [1] 20
```

Read the reported count bands as bands, not exact counts. A later
categorical table may release exact supported cells through its own
checks. Different endpoints answer different questions and have
different disclosure rules.

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
ds.omop.columns("death", conns = conns)$site
#>             column_name cdm_datatype db_datatype      concept_role semantic_class
#> 1             person_id      integer     integer       non_concept       ordinary
#> 2            death_date         date        text       non_concept       ordinary
#> 3 death_type_concept_id      integer     integer      type_concept        concept
#> 4      cause_concept_id      integer     integer attribute_concept        concept
#>      fk_domain is_date is_extension is_untyped_identifier is_sensitive is_blocked
#> 1                FALSE        FALSE                 FALSE        FALSE      FALSE
#> 2                 TRUE        FALSE                 FALSE        FALSE      FALSE
#> 3 Type Concept   FALSE        FALSE                 FALSE        FALSE      FALSE
#> 4                FALSE        FALSE                 FALSE        FALSE      FALSE
#>   is_quasi_identifier
#> 1               FALSE
#> 2                TRUE
#> 3               FALSE
#> 4               FALSE
```
