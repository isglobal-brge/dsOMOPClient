# Client-side filter safety classification (informational only)

Mirrors the server-side classification for UI display purposes. The
server performs the authoritative check.

## Usage

``` r
.classifyFilterClient(filter_type, params = list())
```

## Arguments

- filter_type:

  Character; filter type

- params:

  List; filter parameters (unused for now)

## Value

Character; "allowed", "constrained", or "blocked"
