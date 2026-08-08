# Inspect sticky privacy-release services

Queries every selected DataSHIELD server. Unlike permissive exploration
helpers, this function never returns a partial federation: each
requested node must provide a well-formed status.

## Usage

``` r
ds.omop.dp.status(datasources = NULL)
```

## Arguments

- datasources:

  Named DataSHIELD connection list. `NULL` uses
  [`DSI::datashield.connections_find()`](https://datashield.github.io/DSI/reference/datashield.connections_find.html).

## Value

A complete named list of per-server DP status records.

## Details

`"bounded_accounted"` uses a summable, non-blocking nominal allocation
and may eventually return data-independent degraded releases. The
compatibility mode `"sticky_unbounded"` remains sticky for an exact
authenticated canonical lineage and typed statistic, but does not
identify every mathematically equivalent alternate query construction
and does not bound global composition over unlimited distinct queries.
The `privacy_guarantee` field names the implemented sticky,
person-bounded mechanism and nominal accounting contract. Eligible input
frames must also carry the server's authenticated person-local
provenance capsule; a copied class or plain attribute is not sufficient.
Each status contains the custodian's public `snapshot_id`. Federated
sites may legitimately report different snapshot identifiers. Release
preflight rejects duplicate noise domains, ledgers, or domain-scoped
ledger authentication keys so the same logical privacy node cannot be
pooled twice, including while replicas converge on a rotated noise root
or when durable state was accidentally forked.

## Examples

``` r
if (FALSE) ds.omop.dp.status() # \dontrun{}
```
