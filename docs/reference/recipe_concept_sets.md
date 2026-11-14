# Concept sets of a recipe; OHDSI concept-set exchange

`recipe_concept_sets()` lists the recipe's registry concept sets.
`recipe_export_concept_sets()` writes them as OHDSI (ATLAS) concept-set
expressions; `recipe_import_concept_sets()` reads such expressions into
the registry. Items must share their descendant/mapped flags; excluded
items become the set's `exclude` (exact IDs, or a set with descendants
when the excluded items include descendants). A nested exclusion (an
excluded set with its own exclusion) has no OHDSI form and is refused on
export rather than flattened.

## Usage

``` r
recipe_concept_sets(recipe)

recipe_export_concept_sets(recipe, file = NULL)

recipe_import_concept_sets(recipe, x)
```

## Arguments

  - recipe:

    An `omop_recipe`.

  - file:

    Optional path; `NULL` returns the JSON string.

  - x:

    A JSON string or file with one concept-set expression (`list(items =
    ...)`), a named list of them, or an ATLAS `ConceptSets` array
    (elements with `name` and `expression`).

## Value

A named list of `omop_concept_set` objects, a JSON string, or the
modified recipe.
