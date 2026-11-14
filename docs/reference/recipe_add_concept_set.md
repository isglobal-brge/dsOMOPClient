# Language-2 recipe registries

`recipe_add_concept_set()`, `recipe_add_event()` and
`recipe_add_interval()` declare named nodes that other objects reference
by name; the `recipe_remove_*()` functions remove one by name;
`recipe_set_publish()` sets the publication roots (design section 2.10).
They belong to the complete set of exported editing functions with
`recipe_add_population`, `recipe_add_block`, `recipe_add_variable`,
`recipe_add_filter`, `recipe_add_output`, their `recipe_remove_*()`
counterparts, `recipe_set_scope` and `recipe_set_options`;
`omop_recipe(...)` is the same recipe built in one call.

## Usage

``` r
recipe_add_concept_set(recipe, concept_set)

recipe_add_event(recipe, event)

recipe_add_interval(recipe, interval)

recipe_remove_concept_set(recipe, name)

recipe_remove_event(recipe, name)

recipe_remove_interval(recipe, name)

recipe_set_publish(recipe, publish)
```

## Arguments

  - recipe:

    An `omop_recipe`.

  - concept\_set, event, interval:

    The object to declare (a registry concept set must be named; events
    and intervals without a name get a deterministic one when compiled).
    Declaring an existing name again with a different definition is an
    error.

  - name:

    Name of the node to remove.

  - publish:

    Character vector of output names, `character(0)` for none (every
    output is then a private computed input), or `NULL` for all.

## Value

The modified recipe.

## Examples

``` r
rec <- omop_recipe()
rec <- recipe_add_concept_set(rec, omop_concept_set(201826, descendants = TRUE,
                                                    name = "t2dm"))
rec <- recipe_add_event(rec, omop_event("condition_occurrence", "t2dm", name = "t2dm_dx"))
rec <- recipe_remove_event(rec, "t2dm_dx")
```
