# Get safe numeric cutpoints for a column

Returns a public numeric grid configured by the data controller for an
OMOP column. The server releases the complete grid only when every bin
is supported by the minimum number of distinct persons after
one-contribution- per-person reduction. Edges are not estimated from
protected values.

## Usage

``` r
ds.omop.safe.cutpoints(
  table,
  column,
  concept_id = NULL,
  n_bins = 10L,
  scope = c("per_site", "pooled"),
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- table:

  Character; the OMOP CDM table name (e.g., `"measurement"`,
  `"observation"`).

- column:

  Character; the numeric column to bin (e.g., `"value_as_number"`).

- concept_id:

  Integer or NULL; optional concept ID to restrict rows before computing
  bins (default: NULL for all rows).

- n_bins:

  Integer; exact number of bins in the controller-configured public grid
  (default: 10). Unsupported or under-populated grids fail closed; bins
  are never merged based on protected counts.

- scope:

  Character; `"per_site"` (default) or `"pooled"`. Cutpoints are
  inherently per-site; pooled scope is accepted but the pooled slot will
  be NULL.

- symbol:

  Character; the session symbol identifying the OMOP connection
  (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list where each element
contains public `breaks`, banded `counts`, a session `contract`, and
clipping/grid metadata), `$pooled` (always NULL for cutpoints), and
`$meta` (list with `call_code` and `scope`).

## Examples

``` r
if (FALSE) { # \dontrun{
cuts <- ds.omop.safe.cutpoints("measurement", "value_as_number",
                                concept_id = 3004249, n_bins = 5)
cuts$per_site$server1$breaks
} # }
```
