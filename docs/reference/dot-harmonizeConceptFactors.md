# Harmonize concept-id columns into federation-wide factors

Cross-server coordination layer invoked after a memory-mode plan
execution. For each freshly assigned output symbol it collects every
server's disclosure-safe `_concept_id` levels, computes their union in
one deterministic order client-side, and broadcasts that ordering back
so each server recodes the columns as factors that share identical level
coding. This is what makes pooled `ds.glm`, `ds.glmSLMA`, and `ds.table`
behave correctly on the federated factor.

## Usage

``` r
.harmonizeConceptFactors(owned, conns)
```

## Arguments

- owned:

  Named list mapping plan outputs to their exact server symbols.

- conns:

  DSI connections object.

## Value

`NULL` invisibly; the server symbols are modified in place.

## Details

A value present on only some sites becomes an empty level on the sites
that lack it (valid base R; the modelling functions tolerate it). Every
expected output component must exist on every server. Any discovery,
level-collection, or recoding error fails closed and removes the freshly
assigned output symbols rather than leaving inconsistent factors
available.
