# Inspect sticky privacy-release services

Queries every selected DataSHIELD server. Unlike permissive exploration
helpers, this function never returns a partial federation: each requested
node must provide a well-formed status.

## Usage

``` r
ds.omop.dp.status(datasources = NULL)
```

## Arguments

- datasources:

  Named DataSHIELD connection list. `NULL` uses
  [`DSI::datashield.connections_find()`](https://datashield.github.io/DSI/reference/datashield.connections_find.html).

## Value

An `omop_dp_status` named list of per-server DP status records.

## Details

Since dsOMOP 2.7.1, the v3 service reports history-dependent first-answer
binding: `persistent_state = "noise_root_and_release_bindings"`,
`release_binding = "snapshot_first_answer_v1"`, and
`service_capacity = "public_identity_reservations_v1"`.
It retains one complete answer per public request, snapshot and privacy
epoch until the custodian rotates the snapshot or epoch. New public
identities reserve storage; existing identities remain readable at capacity.
There is no lifetime privacy budget or privacy call quota. Epsilon, the
privacy epoch and the secret root remain server-owned. Legacy v2 status is
inspectable and printed as legacy; release calls require v3 and reject
mixed contracts. The old contract has the unrotated-refresh equality
residual and does not provide first-answer binding.
Eligible input frames must also carry the server's authenticated
person-local provenance capsule; a copied class or plain attribute is not
sufficient.
Since server 2.7.0, `exclusive = TRUE` is the default policy. When
both `enabled` and `exclusive` are true, standard population
statistics are refused; use `ds.omop.dp.release`. Only the
custodian can opt out with `dsomop.dp.exclusive = FALSE`. A missing
`exclusive` field on older servers is treated as `FALSE`.
Each status contains the custodian's public `snapshot_id`. Federated
sites may legitimately report different snapshot identifiers.
Release preflight rejects either a repeated `noise_domain_id` or a
repeated server-owned logical `domain`. The second check also prevents
one logical privacy node from being pooled twice when its connections expose
different noise material.

## Examples

``` r
if (FALSE) ds.omop.dp.status() # \dontrun{}
```
