# Query an OHDSI result table

Reads rows from a pre-computed OHDSI result table with server-controlled
disclosure. Split-only requests preserve that compatibility endpoint.
Combined requests obtain the physical result table's closed,
server-owned pooling contract and combine complete disclosure-controlled
frames through that contract; they never route through a same-named live
analysis or a column-name pooling heuristic. Reviewed equality filters
are applied server-side; selection, ordering, and the row limit are
applied only after contractual pooling. With `type = "both"`, a result
that cannot be combined (no consistent contract, an incomplete
federation, a capped table) still returns the per-server frames, with
`pooled = NULL` and `meta$pooling_reason`; `type = "combine"` stops with
the reason.

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
  type = "split",
  datasources = NULL
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
    
    Deprecated alias of `type` (`"per_site"` or `"pooled"`).

  - pooling\_policy:
    
    Character; `"strict"` (default) or `"pooled_only_ok"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - symbol:
    
    Character; the session symbol (default: `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - type:
    
    Result view: `"split"` (default), `"both"`, or `"combine"`. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

A `dsomop_result` object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Cohort diagnostics results per site
cd <- ds.omop.ohdsi.results("index_event_breakdown")

# Cohort counts per server and pooled across servers
cc <- ds.omop.ohdsi.results("cohort_count", type = "both")
} # }
```
