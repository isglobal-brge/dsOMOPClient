# The plan-v2 (design section 4) JSON form of one language-2 object

Compiles a single object the way `recipe_to_plan()` compiles it inside a
recipe and returns its wire node together with the registry nodes it
references (concept sets, events, intervals), so the object's JSON form
can be inspected or reused (section 12.1: "JSON = section 4").

## Usage

``` r
.v2_wire_of(x)
```

## Arguments

  - x:

    A language-2 object (concept set, event, era, criterion, window,
    anchor, index, censor, reducer, match or variable) or a recipe.

## Value

`list(node, concept_sets, events, intervals)` (a recipe gives its whole
plan).
