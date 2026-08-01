# Compare schemas across servers

Compares the OMOP CDM schemas across all connected servers to identify
structural differences. Returns the set of tables common to all servers,
tables unique to specific servers, and per-table column differences.
This is useful for diagnosing schema mismatches before running pooled
analyses. Requires at least two connected servers for meaningful
comparison; with a single server, returns that server's tables as the
common set.

## Usage

``` r
ds.omop.compare(symbol = "omop", conns = NULL, tables = NULL)
```

## Arguments

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- tables:

  Optional character vector limiting column introspection to the tables
  relevant to a plan. Table presence is still compared globally.

## Value

A list with schema components including `servers` (the exact nodes
compared), `common_tables` (character vector of table names present on
all servers), `server_only` (named list of tables unique to each
server), and `column_diffs` (named list of per-table column
differences), plus `common_columns` (the columns present on every server
for each common table with compatible type families),
`common_column_types` (their canonical type families),
`column_type_diffs` (per-table type-family mismatches), and
`column_errors` (named character vector of tables whose columns could
not be inspected). `semantic_versions` records the reported CDM, dsOMOP
specification and vocabulary versions for each node. An empty
`column_errors` means that all requested common-table column contracts
were established successfully.

## Examples

``` r
if (FALSE) { # \dontrun{
diff <- ds.omop.compare()
diff$common_tables
diff$server_only
diff$column_diffs
} # }
```
