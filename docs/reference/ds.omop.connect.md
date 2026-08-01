# Connect to an OMOP CDM resource on DataSHIELD servers

Establishes a connection to one or more OMOP CDM databases via
DataSHIELD. Assigns the resource server-side, initializes the OMOP
handle, retrieves capabilities, and returns an `omop_session` object.
This is the entry point for all dsOMOPClient operations.

## Usage

``` r
ds.omop.connect(
  resource,
  symbol = "omop",
  cdm_schema = NULL,
  vocab_schema = NULL,
  results_schema = NULL,
  temp_schema = NULL,
  strict = TRUE,
  conns = NULL
)
```

## Arguments

- resource:

  Character or named list; resource name(s). A single string applies to
  all servers; a named list maps server names to resource names.

- symbol:

  Character; server-side symbol name (default: "omop").

- cdm_schema:

  Character; CDM schema override (NULL uses server default).

- vocab_schema:

  Character; vocabulary schema override (NULL uses server default).

- results_schema:

  Character; results schema override (NULL uses server default).

- temp_schema:

  Character; temp schema override (NULL uses server default).

- strict:

  Logical; whether unknown/missing names in a named resource map are
  immediate mapping errors (`TRUE`) or warnings where possible.
  Transactional connection still requires a valid resource and
  successful initialization on every requested server regardless of this
  value.

- conns:

  DSI connections object (NULL uses default connections).

## Value

An `omop_session` object (invisibly).

## Details

Connection setup is all-or-none across the requested servers. The public
handle symbol must be absent everywhere, one resource must resolve for
every node, and capabilities plus transient-resource cleanup are
verified before the local session is recorded. Partial initialization is
closed and removed. Schema arguments are passed as literal call values,
never parsed into code.

Before assigning any resource, the client requires a complete
AggregateMethods inventory from every node. A method named `c`/`list`,
or an alias targeting
[`base::c`](https://rdrr.io/r/base/c.html)/[`base::list`](https://rdrr.io/r/base/list.html),
aborts connection because it can wrap and release an unreviewed
protected object. This safety preflight is mandatory even when
`strict = FALSE`.

The connection is self-healing: the server-side OMOP database connection
auto-reconnects on demand, so a dropped or timed-out database connection
is transparently re-established on the next call. There is therefore no
need to keep the session warm with periodic pings during long idle
periods. Use
[`ds.omop.status`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.status.md)
if you want to manually probe connectivity.

## See also

[`ds.omop.disconnect`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.disconnect.md),
[`ds.omop.status`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.status.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(DSI)
builder <- newDSLoginBuilder()
builder$append(server = "server1", url = "https://opal.example.org",
               resource = "project.omop_cdm", driver = "OpalDriver")
conns <- datashield.login(builder$build())
session <- ds.omop.connect(resource = "project.omop_cdm", conns = conns)
} # }
```
