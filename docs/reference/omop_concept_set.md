# Define a concept set

A concept set is a list of OMOP concept IDs with optional vocabulary
expansion. Resolution order: `concepts`, plus their descendants
(`concept_ancestor`) when `descendants = TRUE`, plus the concepts that
map to the set (`'Maps to'`) when `mapped = TRUE`, minus the expansion
of `exclude`. An object-valued `exclude` is expanded with its own flags;
a bare vector means exact IDs. A specified set that resolves to nothing
matches no row (it never means "all concepts").

## Usage

``` r
omop_concept_set(
  concepts,
  descendants = FALSE,
  mapped = FALSE,
  exclude = NULL,
  name = NULL
)
```

## Arguments

  - concepts:

    Integer vector of concept IDs (may be empty: a set that matches
    nothing).

  - descendants:

    Logical; include descendants.

  - mapped:

    Logical; include source concepts that map to the set.

  - exclude:

    `NULL`, an integer vector of exact concept IDs, or an unnamed
    `omop_concept_set()` expanded with its own flags.

  - name:

    Optional registry name.

## Value

An `omop_concept_set` object.

## Details

A named concept set is registered in the plan's `concept_sets` and
referenced by name; an unnamed one stays inline in the event that uses
it.

## See also

`omop_event`, `omop_recipe`

## Examples

``` r
t2dm <- omop_concept_set(201826, descendants = TRUE, name = "t2dm")
```
