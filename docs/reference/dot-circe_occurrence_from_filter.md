# Build a Circe occurrence criteria object for one recipe filter

Returns `list(criteria, sets)`; `criteria` is the wrapped
`list(<Domain> = ..., StartWindow, Occurrence)` ready for a
CriteriaList.

## Usage

``` r
.circe_occurrence_from_filter(f, sets)
```
