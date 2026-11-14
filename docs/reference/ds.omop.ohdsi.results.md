# Query an OHDSI result table

Reads rows from a pre-computed OHDSI result table with server-controlled
disclosure. Split-only requests preserve that compatibility endpoint.
Combined requests obtain the physical result table's closed,
server-owned pooling contract and combine complete disclosure-controlled
frames through that contract; they never route through a same-named live
analysis or a column-name pooling heuristic. Reviewed equality filters
are applied server-side; selection, ordering, and the row limit are
applied only after contractual pooling.

## Usage

``` r
ds.omop.ohdsi.results(
  table_name,
  columns = NULL,
  filters = NULL,
  order_by = NULL,
  limit = 5000L,
  tool_id = NULL,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  type = NULL
)
```

## Arguments

  - table\_name:

    Character; which result table to query.

  - columns:

    Character vector; columns to select (NULL = all safe columns).

  - filters:

    Named list; WHERE conditions.

  - order\_by:

    Character; ORDER BY column.

  - limit:

    Integer; max rows (capped at 5000 server-side).

  - tool\_id:

    Character; optional tool identifier.

  - scope:

    Character; `"per_site"` or `"pooled"`.

  - pooling\_policy:

    Character; `"strict"` (default) or `"pooled_only_ok"`.

  - symbol:

    Character; the session symbol (default: `"omop"`).

  - conns:

    DSI connection object(s). If `NULL`, uses active session.

  - type:

    Optional result view: `"split"`, `"combine"`, or `"both"`. When
    omitted, legacy `scope` behaviour is preserved.

## Value

A `dsomop_result` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Cohort diagnostics results per site
cd <- ds.omop.ohdsi.results("index_event_breakdown")

# Cohort counts pooled across servers
cc <- ds.omop.ohdsi.results("cohort_count", scope = "pooled")
} # }
```
