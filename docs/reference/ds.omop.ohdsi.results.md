# Query an OHDSI result table

Reads rows from a pre-computed OHDSI result table with server-controlled
disclosure. When `scope = "pooled"`, counts are summed across servers
with suppression propagation; rates/proportions are set to NA.

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
  conns = NULL
)
```

## Arguments

- table_name:

  Character; which result table to query.

- columns:

  Character vector; columns to select (NULL = all safe columns).

- filters:

  Named list; WHERE conditions.

- order_by:

  Character; ORDER BY column.

- limit:

  Integer; max rows (capped at 5000 server-side).

- tool_id:

  Character; optional tool identifier.

- scope:

  Character; `"per_site"` or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL`, uses active session.

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
