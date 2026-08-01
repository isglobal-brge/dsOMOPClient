# Get OMOP session status

Pings each connected server and returns the current session status
including capabilities, server versions, and any connection errors.

## Usage

``` r
ds.omop.status(symbol = "omop")
```

## Arguments

- symbol:

  Character; session symbol (default: "omop").

## Value

Named list with symbol, servers, capabilities, ping results, and errors.

## Examples

``` r
if (FALSE) { # \dontrun{
status <- ds.omop.status("omop")
status$ping
} # }
```
