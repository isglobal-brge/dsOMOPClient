# List tables in the OMOP CDM database

Queries each connected server for the list of OMOP CDM tables present in
the database. Returns metadata including the schema category (`"CDM"`,
`"Vocabulary"`, `"Results"`) and whether the table contains a
`person_id` column. An optional filter allows restricting results to a
single schema category.

## Usage

``` r
ds.omop.tables(
  schema_category = NULL,
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - schema\_category:
    
    Character; optional filter to restrict results to a specific
    category: `"CDM"`, `"Vocabulary"`, or `"Results"`. NULL returns all
    tables (default: NULL).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. The
    pooled view lists the tables present on every server. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. `$per_site` is the named list (one data frame per
server, with `table_name`, `schema_category`, `has_person_id`, ...)
returned by earlier versions, and `res$<server>` still reaches one
server's frame. `$pooled` holds the rows of the tables present on every
server (`NULL`, with `meta$pooling_reason`, when a server did not
answer).

## Examples

``` r
if (FALSE) { # \dontrun{
tables <- ds.omop.tables()
tables$server1
tables$pooled$table_name   # tables present everywhere

cdm_only <- ds.omop.tables(schema_category = "CDM")
} # }
```
