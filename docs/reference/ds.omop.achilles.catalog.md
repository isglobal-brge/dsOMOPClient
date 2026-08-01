# Get Achilles analysis catalog

Returns the full catalog of available Achilles analyses from the server.
If the `achilles_analysis` table exists, its contents are returned
directly; otherwise, the server discovers available analyses by
inspecting distinct `analysis_id` values in the `achilles_results`
table. Since the catalog is identical across servers, the pooled result
is taken from the first responding server.

## Usage

``` r
ds.omop.achilles.catalog(symbol = "omop", conns = NULL)
```

## Arguments

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A `dsomop_result` object with `scope = "pooled"`. The pooled result is a
data frame listing all available analysis IDs and their descriptions.

## Examples

``` r
if (FALSE) { # \dontrun{
catalog <- ds.omop.achilles.catalog()
head(catalog$pooled)
} # }
```
