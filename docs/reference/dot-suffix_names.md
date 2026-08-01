# Generate suffixed names for multi-column variables

Produces a vector of column names from a base name by appending index,
range, or label suffixes. Used when a single variable expands into
multiple output columns.

## Usage

``` r
.suffix_names(
  base_name,
  n,
  mode = c("index", "range", "label"),
  labels = NULL,
  ranges = NULL
)
```

## Arguments

- base_name:

  Character; base variable name.

- n:

  Integer; number of columns to generate.

- mode:

  Character; suffix mode ("index", "range", or "label").

- labels:

  Character vector; labels for "label" mode (optional).

- ranges:

  Numeric matrix; start/end for "range" mode (optional).

## Value

Character vector of suffixed names.
