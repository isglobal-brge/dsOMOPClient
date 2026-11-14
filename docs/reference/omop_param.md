# Recipe parameters: placeholders, binding and merging

`omop_param()` declares a named placeholder of a fixed type that may
stand for a value in any language-2 constructor (an event's concepts, a
window bound, an era gap, a matching seed, a predicate literal inside
`omop_where`, ...). The same name may appear several times; every
occurrence must carry the same definition.

## Usage

``` r
recipe_merge(a, b, prefix = NULL)

omop_param(
  name,
  type = c("concept_set", "event", "integer", "number", "string", "date", "window"),
  default = NULL,
  description = NULL
)

recipe_params(recipe)

recipe_bind(recipe, ...)
```

## Arguments

  - a, b:

    Recipes to merge (`recipe_merge()`).

  - prefix:

    `NULL` (one shared namespace) or a name: every node of `b` is
    renamed `<prefix>_<name>` and every reference to it is rewritten.

  - name:

    Parameter name (a letter, then letters, digits or underscores).

  - type:

    Parameter type (see Details).

  - default:

    Optional default value of the declared type; `NULL` leaves the
    parameter unbound until `recipe_bind()`.

  - description:

    Optional description.

  - recipe:

    An `omop_recipe`.

  - ...:

    `recipe_bind()`: values by parameter name (`name = value`).

## Value

`omop_param()`: a parameter placeholder. `recipe_params()`: a named list
of the declared parameters, in order of first appearance.
`recipe_bind()`: the bound recipe.

## Details

`recipe_merge(a, b, prefix = NULL)` joins two recipes. Without a prefix
both share one namespace: a name used by both must carry the same
definition, and their base populations, recipe-level filters, scopes,
plan options and coverage defaults must agree. With a prefix every node
of `b` (concept sets, events, intervals, populations, variables, blocks,
outputs) is renamed and every reference rewritten – anchors, censoring
(and its `when_index_source` tags), index arms, parents, set-operation
members, match inputs, output rows and columns, output dependencies and
publication roots; `b`'s base population keeps its meaning (its
recipe-level filters move onto it) and is shared only when it equals
`a`'s. When equality depends on parameters, it is reconsidered after
binding or default resolution against the current population and source
definitions, preserving edits made after merging. Outputs whose columns
were implicit (every variable) are made explicit first, so each output
keeps exactly its own variables. Block metadata (`block_id`,
`block_version`) is preserved. Parameters are shared by name.

`recipe_params()` lists the parameters a recipe declares.
`recipe_bind()` binds values by name: each value is checked against the
declared type and substituted, and every object that held a placeholder
is rebuilt through its constructor, so a bound recipe is validated
exactly like a hand-written one. Parameters left unbound keep their
placeholder (a recipe may be bound in stages). `recipe_to_plan` binds
the remaining parameters to their defaults and refuses a parameter
without a default.

Integer and number parameters admit the arithmetic operators `+`, `-`,
`*` and `/` (also unary minus and unary plus) with numeric literals and
other numeric parameters, e.g. `omop_window(-omop_param("washout", type
= "integer"), -1)`; the expression is evaluated when its parameters are
bound and its value is then validated where it is used.

Types and the values they accept: `"integer"` (an exact integer),
`"number"` (a finite number), `"string"` (one string), `"date"` (an ISO
date or a `Date`), `"concept_set"` (an `omop_concept_set`, concept IDs,
or the name of a registry concept set), `"event"` (an `omop_event`, an
`omop_era`, or the name of a declared one) and `"window"` (an
`omop_window`).

## See also

`recipe_merge`, `recipe_to_plan`

## Examples

``` r
rec <- omop_recipe(
  populations = list(omop_population("c1", index = omop_index(
    list(cohort_definition_id = omop_param("cohort", type = "integer", default = 1)),
    select = "all"))),
  variables = list(dm = omop_variable(
    event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set")),
    reducer = omop_reduce("binary", window = omop_window(-omop_param("lookback",
      type = "integer", default = 365L), 0)))),
  outputs = list(omop_output("w", rows = "c1", format = "wide")))
names(recipe_params(rec))
bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE))
```
