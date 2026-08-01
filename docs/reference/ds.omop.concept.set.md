# Build a concept set specification (client-only)

Creates a local `omop_concept_set` object that defines a set of OMOP
concepts along with expansion rules. This is a client-side-only helper
that does not contact any server; the resulting object is passed to
[`ds.omop.concept.expand`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.concept.expand.md)
for server-side resolution.

## Usage

``` r
ds.omop.concept.set(
  concepts,
  include_descendants = FALSE,
  include_mapped = FALSE,
  exclude = NULL
)
```

## Arguments

- concepts:

  Integer or numeric vector of seed concept IDs that form the base of
  the concept set.

- include_descendants:

  Logical; if `TRUE`, all descendants of the seed concepts (via
  `concept_ancestor`) will be included when the set is expanded.
  Default: `FALSE`.

- include_mapped:

  Logical; if `TRUE`, non-standard concepts mapped to the seed concepts
  (via `concept_relationship`) will be included when the set is
  expanded. Default: `FALSE`.

- exclude:

  Integer or numeric vector of concept IDs to explicitly remove from the
  expanded set. `NULL` (the default) excludes nothing.

## Value

An `omop_concept_set` object (a list with class
`c("omop_concept_set", "list")`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Simple concept set with descendants
cs <- ds.omop.concept.set(c(201826, 316139), include_descendants = TRUE)

# Expand the concept set on the server
expanded <- ds.omop.concept.expand(cs)
} # }
```
