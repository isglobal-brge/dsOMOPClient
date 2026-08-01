# Log in and open an OMOP CDM session in one call

One-line entry point for first-time users: builds the DataSHIELD login
([`newDSLoginBuilder`](https://datashield.github.io/DSI/reference/newDSLoginBuilder.html) +
[`datashield.login`](https://datashield.github.io/DSI/reference/datashield.login.html)),
then assigns + initialises the OMOP resource
([`ds.omop.connect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)),
returning BOTH the live connections and the OMOP session. It is a thin
convenience over the existing lower-level path —
[`datashield.login()`](https://datashield.github.io/DSI/reference/datashield.login.html)
followed by
[`ds.omop.connect()`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)
— with one ownership guarantee: if OMOP initialisation fails after
authentication, it logs out those new DataSHIELD connections before
propagating the error. Reach for the two-step path when you need a
custom login builder, multiple resources per server, or to reuse
connections across several OMOP sessions.

## Usage

``` r
ds.omop.login(
  url,
  user = "",
  password = "",
  resource,
  server = NULL,
  driver = "OpalDriver",
  token = NULL,
  profile = NULL,
  symbol = "omop",
  ...
)
```

## Arguments

- url:

  Character; server URL(s). A single URL or one per server.

- user:

  Character; username(s) (recycled if scalar). Ignored for a server
  whose `token` is supplied.

- password:

  Character; password(s) (recycled if scalar). Ignored for a server
  whose `token` is supplied.

- resource:

  Character; the OMOP CDM resource path(s) (e.g. `"project.omop_cdm"`).
  A single value applies to every server; a named vector maps server
  name to resource; an unnamed vector matches positionally.

- server:

  Character; server name(s) (default `"server1"`, or `server1..N` when
  several URLs are given).

- driver:

  Character; the DSI driver to connect with (default `"OpalDriver"`,
  from the DSOpal package). Recycled if scalar.

- token:

  Character or `NULL`; personal access token(s) used instead of
  `user`/`password` where supplied.

- profile:

  Character or `NULL`; Opal/Armadillo R server profile(s).

- symbol:

  Character; server-side OMOP session symbol (default `"omop"`).

- ...:

  Further arguments forwarded to
  [`ds.omop.connect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md)
  (e.g. `cdm_schema`, `strict`).

## Value

Invisibly, a list with `conns` (the DSI connections) and `session` (the
`omop_session`). The session is also stored under `symbol` so every
other `ds.omop.*` call can default to it.

## Details

The single-server common case is one call:
`ds.omop.login(url, user, password, resource)`. For several servers pass
`server`/`url`/`resource` (and, if they differ, `user`/`password`) as
equal-length vectors; scalars are recycled.

## See also

[`ds.omop.connect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.connect.md),
[`ds.omop.disconnect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.disconnect.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# The whole connect, in one line:
login <- ds.omop.login(
  url = "https://opal.example.org",
  user = "analyst", password = "secret",
  resource = "project.omop_cdm")
login$conns     # the DataSHIELD connections
login$session   # the OMOP session
} # }
```
