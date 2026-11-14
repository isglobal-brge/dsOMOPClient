# dsOMOP version on each server

Works on plain DataSHIELD connections, before (or without)
`ds.omop.connect`. For every server it reports the dsOMOP version whose
DataSHIELD methods the profile publishes, the version that answers a
call (the one installed), and the methods this client calls that the
profile does not publish. A server is ready when the two versions agree
and nothing is missing; otherwise `advice` says what to change.

## Usage

``` r
ds.omop.server.version(conns = NULL, symbol = "omop")
```

## Arguments

  - conns:

    DSI connections (default: the DSI default connections).

  - symbol:

    OMOP session symbol whose capability blocks (read by
    `ds.omop.connect`) are reported.

## Value

A data frame with one row per server: `server`, `published` (version of
the published dsOMOP methods, NA when none), `running` (version
answering `omopPingDS`, NA when it cannot be called), `missing` (methods
this client calls that the profile lacks), `ready`, `advice` and the
list column `recipe_language`: per server the recipe-language capability
block of its OMOP session (`version`, `semantic_version`, `operations`,
`reducers`, `dialect`), independently of method readiness. The string
`"unavailable"` means the connected site supplied no block, or an
unconnected site's profile lacks the publication protocol; `"not
connected"` means no OMOP session `symbol` is connected to a site
publishing that protocol. No language capability is inferred from a
package version or from `ready`. The client version is in its `"client"`
attribute.

## Details

Opal publishes a package's methods when it is first installed, but not
when an installed package is upgraded: after an upgrade the profile
keeps the old list until an administrator publishes the package's
settings again.

A server whose profile lacks the publication protocol
(`omopPlanStatusDS`, `omopStagedRevokeDS`) runs v1 plans only through
the legacy call and refuses language-2 plans; the advice says whether
the installed dsOMOP predates the protocol (install the release that has
it, then publish) or its upgraded methods are not published yet (publish
again). `ready` concerns the published methods only: whether a server
executes a language-2 plan is its `recipe_language` block.

## Examples

``` r
if (FALSE) { # \dontrun{
ds.omop.client.version()
ds.omop.server.version(conns)
} # }
```
