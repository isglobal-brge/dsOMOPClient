# Translate a recipe index-relative window to a Circe StartWindow

Recipe windows are `list(start, end)` day offsets relative to the index
date. Circe expresses each endpoint as `list(Days, Coeff)` where `Coeff`
is -1 (before index) or 1 (after). Day 0 uses Coeff 1.

## Usage

``` r
.circe_window_from_recipe(window)
```
