# The plan a recipe executes as, before any capability check (A-14)

A recipe built only from 2.10.1 constructors (and without `parent_id`)
executes as its unchanged v1 plan; the server's legacy adapters keep its
results. When the v1 compiler refuses one of the shapes 2.10.3 newly
permits (`.v1_newly_permitted()`), or the recipe uses a language-2
construct or `parent_id`, the plan is its v2 rendering. A recipe that
has neither a v1 plan nor a native rendering is refused, naming both the
construct the native compiler cannot render and what prevents the v1
plan.

## Usage

``` r
.recipe_select_plan(recipe)
```

## Arguments

  - recipe:

    An `omop_recipe`.

## Value

A v1 `omop_plan` or an `omop_plan_v2`; attribute `"v1_refusal"` on a v2
plan selected for a newly permitted shape.
