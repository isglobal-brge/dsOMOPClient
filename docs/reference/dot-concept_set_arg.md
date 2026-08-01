# Build a concept_set argument, expanding descendants when requested

Returns a bare integer vector by default, or a concept-set spec of the
form `list(concepts = ids, include_descendants = TRUE)` when any
contributing variable carries `$expand = TRUE`. The server expands the
spec via `.vocabExpandConceptSet` at execution time.

## Usage

``` r
.concept_set_arg(vs, ids)
```

## Arguments

- vs:

  List of `omop_variable` objects contributing the concepts.

- ids:

  Integer vector of concept IDs.

## Value

Either an integer vector, a concept-set spec list, or `NULL`.
