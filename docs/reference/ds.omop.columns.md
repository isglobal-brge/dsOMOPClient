# List columns in a table

Queries each connected server for the list of columns present in the
specified OMOP CDM table. Returns metadata for each column including the
column name, data type, whether the column is nullable, and whether it
is a concept ID column or a date column.

## Usage

``` r
ds.omop.columns(
  table,
  symbol = "omop",
  conns = NULL,
  type = "both",
  datasources = NULL
)
```

## Arguments

  - table:
    
    Character; the CDM table name to introspect (e.g.,
    `"condition_occurrence"`, `"person"`).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"both"` (default), `"split"`, or `"combine"`. The
    pooled view lists the columns present on every server. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result`. `$per_site` is the named list (one data frame per
server, with `column_name`, `data_type`, `is_nullable`, `is_concept`,
`is_date`, ...) returned by earlier versions, and `res$<server>` still
reaches one server's frame. `$pooled` holds the rows of the columns
present on every server.

## Examples

``` r
if (FALSE) { # \dontrun{
cols <- ds.omop.columns("condition_occurrence")
cols$server1
cols$pooled$column_name   # columns present everywhere
} # }
```
