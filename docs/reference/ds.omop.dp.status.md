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
    `DSI::datashield.connections_find()`.

## Value

An `omop_dp_status` named list of per-server DP status records.

## Details

dsOMOP 2.10.3 uses the protected-release contract: canonical fixed-basis
or realised-contribution components, without history-based admission or
stored answers. This client declares that contract in its status
request; the new server refuses clients 2.10.2 and earlier with an
upgrade message. Every older server is explicitly warned as unpatched.
Installing the new client cannot repair server-side disclosure defects.
Mixed old/new protected pooling is refused; per-site results remain
labelled by contract.

On servers 2.10.0 to 2.10.2 the v3 service is seed-only:
`release_binding = "seed_data_versioned_v1"`, `persistent_state =
"noise_root_only"`, `service_capacity = "none"` and `history_dependent =
FALSE`. Every answer is a deterministic function of the server's secret
root, the question, the protected statistic and a private automatic
database version, so it is identical after restarts and on every R
server sharing the root, and a reload that adds rows re-draws every
answer at once. Servers 2.7.1 to 2.9.x report history-dependent
first-answer binding instead: `persistent_state =
"noise_root_and_release_bindings"`, `release_binding =
"snapshot_first_answer_v1"`, and `service_capacity =
"public_identity_reservations_v1"`. They retain one complete answer per
public request, snapshot and privacy epoch until the custodian rotates
the snapshot or epoch. Both contracts are accepted, also mixed in one
federation. There is no lifetime privacy budget or privacy call quota.
Epsilon, the privacy epoch and the secret root remain server-owned.
Legacy v2 status is inspectable and printed as legacy. Release calls
require v3 by default. During a staged upgrade,
`options(dsomop.dp.allow_legacy_servers = TRUE)` permits v2 releases,
including mixed federations, with a warning naming each legacy server.
The old contract has no first-answer binding: an unrotated data refresh
can reveal whether a released statistic changed; see
isglobal-brge/dsOMOP\#20. The release result records every site's
contract. Eligible input frames must also carry the server's
authenticated person-local provenance capsule; a copied class or plain
attribute is not sufficient. DP concerns only the OHDSI community tools:
connecting, loading tables, filters, factors and dsOMOP's own
exploration never use it. Since server 2.8.0, `exclusive` is `FALSE` by
default and, when a custodian sets it, applies only to the community
tools (servers 2.7.x applied it to standard statistics). A missing
`exclusive` field on older servers is treated as `FALSE`. Servers 2.9.0
and later also report `tools_mode`, the community tools' protection mode
(`"auto"` by default, `"standard"` or `"dp"`). Status may contain a
public `snapshot_id` label. Federated sites may report different labels.
On 2.10.3 servers it is optional display metadata with no effect on
admission or noise. Fixed synopses are keyed on a private
person-membership generation recomputed within each request's database
snapshot, without a per-session cache. Clinical-value updates that leave
person membership unchanged do not change that generation; cross-version
inference remains a residual. The public `data_generation`,
`data_generation_basis` and `data_generation_residual` fields describe
the policy without revealing the private generation. Protected pooling
across multiple sites requires distinct payload identifiers
`source$logical_source_id`; missing or repeated identifiers are refused,
while per-site results remain available. On legacy servers only, release
preflight rejects repeated `noise_domain_id` or server-owned logical
`domain` values.

## Examples

``` r
if (FALSE) ds.omop.dp.status() # \dontrun{}
```
