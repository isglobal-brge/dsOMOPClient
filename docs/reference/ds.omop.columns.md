# List columns in a table

Queries each connected server for the list of columns present in the
specified OMOP CDM table. Returns metadata for each column including the
column name, data type, whether the column is nullable, and whether it
is a concept ID column or a date column.

## Usage

``` r
ds.omop.columns(table, symbol = "omop", conns = NULL)
```

## Arguments

- table:

  Character; the CDM table name to introspect (e.g.,
  `"condition_occurrence"`, `"person"`).

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

A named list (one element per server) of data frames with column
metadata such as `column_name`, `data_type`, `is_nullable`,
`is_concept`, and `is_date`.

## Examples

``` r
if (FALSE) { # \dontrun{
cols <- ds.omop.columns("condition_occurrence")
cols$server1
} # }
```
