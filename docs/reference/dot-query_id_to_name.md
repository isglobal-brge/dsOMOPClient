# Map a legacy query id to its analysis-catalog entry name

QueryLibrary templates are registered in the unified catalog under the
pack-prefixed name `"dsomop:<query_id>"`. This prefixes a bare legacy
`query_id` (and leaves an already-prefixed name untouched).

## Usage

``` r
.query_id_to_name(query_id)
```

## Arguments

- query_id:

  Character; a legacy query id or an already-prefixed name.

## Value

Character; the catalog entry name.
