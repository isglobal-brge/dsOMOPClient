# Get a cohort definition by ID

Retrieves the full definition of a specific cohort from each connected
server, including the inclusion criteria and any metadata stored with
the cohort definition.

## Usage

``` r
ds.omop.cohort.definition(id, symbol = "omop", conns = NULL)
```

## Arguments

- id:

  Integer; the cohort definition ID to retrieve. An id that is unknown
  or below the disclosure threshold returns the same not-available
  result (see Disclosure control), so a usable definition requires a
  cohort that appears in
  [`ds.omop.cohort.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.list.md).

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

## Value

Named list (one entry per server) with definition details for an
available (at- or above-threshold) cohort. For an unavailable id –
absent OR sub-threshold – the server returns its uniform not-available
response (identical for both cases), surfaced unchanged by the client.

## Disclosure control

An id that is not available – whether because no such cohort exists OR
because the cohort exists but is below the server's per-subset threshold
(`nfilter_subset`) – yields the SAME "not available" result. The two
cases are deliberately indistinguishable: a sub-threshold cohort is
treated as nonexistent, and the server's response for it is identical to
that for a genuinely unknown id (it never says "too small" or otherwise
hints that a small cohort exists, which would itself confirm its
existence). The client surfaces the server's response as-is and adds no
distinction of its own, so a caller cannot tell small-but-present from
absent from either side. Only definitions of cohorts at or above the
threshold (the ones that appear in
[`ds.omop.cohort.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.list.md))
are readable.

## See also

[`ds.omop.cohort.list`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.cohort.list.md)

## Examples

``` r
if (FALSE) { # \dontrun{
defn <- ds.omop.cohort.definition(id = 1)
defn[["server_a"]]
} # }
```
