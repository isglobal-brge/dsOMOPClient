# Retrieve a stored OMOP session

Looks up a previously created `omop_session` object by its symbol name
in the internal client environment. Stops with an informative error if
no session with that symbol exists.

## Usage

``` r
.get_session(symbol = "omop")
```

## Arguments

- symbol:

  Character; the session symbol to look up.

## Value

The `omop_session` object.
