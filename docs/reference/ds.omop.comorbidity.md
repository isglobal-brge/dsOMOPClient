# Disclosure-safe two-by-two comorbidity (person co-occurrence)

A thin wrapper over
[`ds.omop.crosstab`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.crosstab.md)
that cross-tabulates the person-level presence/absence of two concepts
as a 2x2 table, using the SAME server-side `omopCrossTabDS` machinery
(primary + iterative complementary suppression, no exact margins).
Because counting and suppression happen server-side, suppressed cells
cannot be backed out from margins client-side.

## Usage

``` r
ds.omop.comorbidity(
  conceptA,
  conceptB,
  tableA = "condition_occurrence",
  tableB = tableA,
  ...
)
```

## Arguments

- conceptA:

  Integer; the first concept ID (row presence axis).

- conceptB:

  Integer; the second concept ID (column presence axis).

- tableA:

  Character; the CDM table holding `conceptA` (default:
  `"condition_occurrence"`).

- tableB:

  Character; the CDM table holding `conceptB` (default: same as
  `tableA`).

- ...:

  Additional arguments forwarded to
  [`ds.omop.crosstab`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.crosstab.md)
  (e.g., `cohort_table`, `scope`, `pooling_policy`, `symbol`, `conns`,
  `execute`).

## Value

A `dsomop_result` object (see
[`ds.omop.crosstab`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.crosstab.md)).

## Comorbidity is descriptive, not inferential

This reports raw person-overlap between two conditions. It is NOT an
adjusted measure of association and does not control for confounders
such as age. For a genuine multivariable comorbidity model, route to
[`ds.glm`](https://rdrr.io/pkg/dsBaseClient/man/ds.glm.html).

## Examples

``` r
if (FALSE) { # \dontrun{
cm <- ds.omop.comorbidity(316866, 201826)  # hypertension x type 2 diabetes
cm$per_site
} # }
```
