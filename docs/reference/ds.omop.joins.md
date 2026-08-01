# Get the join relationship graph

Retrieves the join relationship graph for the OMOP CDM schema from each
connected server. The graph describes standard OMOP relationships (e.g.,
via `person_id`, `visit_occurrence_id`, or concept foreign keys). It is
an introspection aid; current recipe/plan execution does not consume it
to invent arbitrary joins automatically.

## Usage

``` r
ds.omop.joins(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

## Value

A named list (one element per server) of data frames with edge metadata
such as `from_table`, `to_table`, `from_column`, and `to_column`.

## Examples

``` r
if (FALSE) { # \dontrun{
joins <- ds.omop.joins()
joins$server1
} # }
```
