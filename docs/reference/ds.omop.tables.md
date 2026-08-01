# List tables in the OMOP CDM database

Queries each connected server for the list of OMOP CDM tables present in
the database. Returns metadata including the schema category (`"CDM"`,
`"Vocabulary"`, `"Results"`) and whether the table contains a
`person_id` column. An optional filter allows restricting results to a
single schema category.

## Usage

``` r
ds.omop.tables(schema_category = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- schema_category:

  Character; optional filter to restrict results to a specific category:
  `"CDM"`, `"Vocabulary"`, or `"Results"`. NULL returns all tables
  (default: NULL).

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

A named list (one element per server) of data frames with table metadata
columns such as `table_name`, `schema_category`, and `has_person_id`.

## Examples

``` r
if (FALSE) { # \dontrun{
tables <- ds.omop.tables()
tables$server1

cdm_only <- ds.omop.tables(schema_category = "CDM")
} # }
```
