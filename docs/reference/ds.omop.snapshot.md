# Get a full schema snapshot

Retrieves a comprehensive schema snapshot from each connected server,
combining capabilities metadata (available tables, CDM version info)
with the join relationship graph into a single structure. This provides
a complete picture of the database schema that can be cached client-side
and used to drive headless schema exploration and query building.

## Usage

``` r
ds.omop.snapshot(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

A named list (one element per server), where each element is a list with
`tables` (character vector of table names), `cdm_info` (list with CDM
version and DBMS details), and `edges` (data frame of join
relationships).

## Examples

``` r
if (FALSE) { # \dontrun{
snap <- ds.omop.snapshot()
snap$server1$tables
snap$server1$cdm_info
snap$server1$edges
} # }
```
