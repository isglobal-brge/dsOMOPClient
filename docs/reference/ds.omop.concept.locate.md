# Locate concept across all CDM tables

Searches all OMOP CDM tables that contain concept columns and returns a
presence matrix showing which tables contain the specified concept IDs.
This is useful for understanding where a concept appears in the database
before performing deeper exploration or extraction. Counts are
disclosure-controlled; tables where a concept appears fewer than the
threshold number of times are reported as suppressed.

## Usage

``` r
ds.omop.concept.locate(
  concept_ids,
  scope = c("per_site", "pooled"),
  pooling_policy = "strict",
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- concept_ids:

  Integer vector; one or more OMOP concept IDs to search for across all
  CDM tables.

- scope:

  Character; `"per_site"` (default) or `"pooled"`.

- pooling_policy:

  Character; `"strict"` (default) or `"pooled_only_ok"`.

- symbol:

  Character; the session symbol (default: `"omop"`).

- conns:

  DSI connection object(s) or NULL to use the session default.

- execute:

  Logical; if `FALSE`, return a dry-run result containing only the
  generated call code (default: `TRUE`).

## Value

A `dsomop_result` object with `$per_site` (named list of data frames
with columns `concept_id`, `table_name`, `count_value`), `$pooled`
(combined presence matrix when pooled), and `$meta`.

## Examples

``` r
if (FALSE) { # \dontrun{
loc <- ds.omop.concept.locate(c(201820, 316139))
loc$per_site$server1
} # }
```
