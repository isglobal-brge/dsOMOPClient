# List available Achilles analyses

Returns the catalog of Achilles analyses available on the connected
servers, optionally filtered by clinical domain. Since the catalog is
identical across servers (it is defined by the Achilles specification),
the pooled result is taken from the first responding server.

## Usage

``` r
ds.omop.achilles.analyses(domain = NULL, symbol = "omop", conns = NULL)
```

## Arguments

- domain:

  Character; optional domain filter. Valid values include `"person"`,
  `"condition"`, `"drug"`, `"measurement"`, `"procedure"`,
  `"observation"`, `"visit"`, and `"device"`. `NULL` (the default)
  returns all domains.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

A `dsomop_result` object with `scope = "pooled"`. The pooled result is a
data frame listing analysis IDs, names, and associated domains.

## Examples

``` r
if (FALSE) { # \dontrun{
# All available analyses
catalog <- ds.omop.achilles.analyses()
catalog$pooled

# Only condition-related analyses
cond <- ds.omop.achilles.analyses(domain = "condition")
} # }
```
