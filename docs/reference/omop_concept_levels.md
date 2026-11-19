# Declared category levels from the public vocabulary

Expands a concept set on every connected server's vocabulary tables and
returns the union as declared category keys for
`omop_category_spec(levels = )`. The expansion reads only vocabulary
reference tables (`concept`, `concept_ancestor`,
`concept_relationship`), never patient rows.

## Usage

``` r
omop_concept_levels(concept_set, symbol = "omop", conns = NULL)
```

## Arguments

  - concept\_set:
    
    An `omop_concept_set()`; `descendants`, `mapped` and `exclude`
    expand as in an event.

  - symbol:
    
    Character; the OMOP session symbol.

  - conns:
    
    DSI connections; defaults to the session's connections.

## Value

Character vector of `"concept:<id>"` keys in concept ID order.

## Details

Declared levels are public. A concept column presented with them is
built from the declaration at every site, so nothing about which
concepts occur at a site is released and no factor-level limit applies
to them, however many levels the list holds. Observed values outside the
list are still discovered under DataSHIELD's factor-level rule
(`nfilter.levels.max` and `nfilter.levels.density`). Counts and models
keep their usual disclosure controls.

The union covers sites whose vocabulary versions differ. The keys are
stored in the recipe, so it records exactly which levels it declares.

## See also

`omop_category_spec`, `omop_concept_set`

## Examples

``` r
if (FALSE) { # \dontrun{
causes <- omop_concept_levels(omop_concept_set(4306655, descendants = TRUE))
cause <- omop_category(
  omop_variable(event = omop_event("death", name = "cause_of_death"),
                reducer = omop_reduce("first_value", value = "cause_concept_id")),
  omop_category_spec(levels = causes))
} # }
```
