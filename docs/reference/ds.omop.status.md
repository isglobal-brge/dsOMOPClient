# Get OMOP session status

Pings each connected server and returns the current session status
including capabilities, server versions, and any connection errors.

## Usage

``` r
ds.omop.status(symbol = "omop", datasources = NULL)
```

## Arguments

  - symbol:
    
    Character; session symbol (default: "omop").

  - datasources:
    
    DataSHIELD connections to ping. `NULL` (the default) uses the
    session's connections. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

Named list with symbol, servers, capabilities, ping results, and errors.
`ping` has one entry per server: that server's ping answer, or
`list(error = <message>)` for a server that did not answer.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- ds.omop.status("omop")
status$ping
} # }
```
