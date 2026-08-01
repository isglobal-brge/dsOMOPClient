# Build (or reuse) a Circe concept set for a vector of concept IDs

Concept sets are deduplicated by their ID vector so repeated references
share one codeset. Returns `list(id, sets)` where `sets` is the updated
registry. `include_descendants` mirrors the recipe block/expand notion.

## Usage

``` r
.circe_codeset(
  sets,
  concept_ids,
  name = NULL,
  include_descendants = FALSE,
  include_mapped = FALSE,
  is_excluded = FALSE
)
```
