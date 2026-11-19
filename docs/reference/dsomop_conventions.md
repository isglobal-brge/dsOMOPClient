# Result views, connections and pooling in dsOMOPClient

dsOMOPClient follows the dsBaseClient conventions for every function
that contacts the servers.

## Connections

`datasources` names the DataSHIELD connections to use. `NULL` (the
default) uses the connections stored in the OMOP session of `symbol`
when one exists, and otherwise `DSI::datashield.connections_find()`. The
older name `conns` keeps working (with a one-time message); giving both
with different connections is an error.

## Result views (`type`)

  - `"split"`: the per-server results only (`$per_site`).

  - `"combine"`: the pooled result only (`$pooled`). When the result
    cannot be pooled, the call stops with the reason, before contacting
    the servers when the reason is known in advance.

  - `"both"`: both. When pooling is impossible the per-server results
    are kept, `$pooled` is `NULL` and `meta$pooling_reason` says why.

Count and table readers (`ds.omop.table.stats`,
`ds.omop.domain.coverage`, `ds.omop.date.counts`,
`ds.omop.value.counts`, `ds.omop.crosstab`, `ds.omop.concept.locate`,
`ds.omop.achilles.results`) default to `"both"`, as `ds.dim`,
`ds.length` and `ds.table` do. Readers of means and distributions
default to `"split"`, as `ds.mean` and `ds.var` do. Aliases: `"s"`,
`"splits"`, `"per_site"` (split); `"c"`, `"combined"` (combine); `"b"`,
`"pooled"` (both). The older `scope` (`"per_site"`, `"pooled"`) and
`pool` (`FALSE`, `TRUE`) arguments keep working as aliases of `"split"`
and `"both"`.

Every pooled result records how it was pooled in `meta$pooling`
(`method` and `estimand`). Pooled counts are sums of the servers' banded
counts: a person present in several databases counts once per database.

## Pooling policy

`pooling_policy = "strict"` (the default) releases a pooled value only
when every server returned it: a group (value, concept, period, bin,
cell) absent or suppressed on any server is left out or `NA`, and an
unavailable server leaves the whole pooled result `NULL`.
`"pooled_only_ok"` pools the servers that did return a disclosure-safe
value, so a pooled value may cover only some servers; the warnings name
what was left out. Suppressed contributions are never treated as zero
under either policy.

## Output names

Functions that create one server-side object accept `newobj` for its
name, as dsBaseClient assigning functions do.
