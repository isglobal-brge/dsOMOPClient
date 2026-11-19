# Disclosure-safe two-by-two comorbidity (person co-occurrence)

A thin wrapper over `ds.omop.crosstab` that cross-tabulates the
person-level presence/absence of two concepts of one table as a 2x2
table, using the SAME server-side `omopCrossTabDS` machinery (primary +
iterative complementary suppression, no exact margins). Because counting
and suppression happen server-side, suppressed cells cannot be backed
out from margins client-side. Both axes use the table's domain concept
column (for example `drug_concept_id` for `drug_exposure`).

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
    
    Character; the CDM table holding both concepts (default:
    `"condition_occurrence"`).

  - tableB:
    
    Character; must equal `tableA`. The server cross-tab reads one
    table, so two concepts of different tables are refused with an
    explanation rather than silently counted in `tableA`.

  - ...:
    
    Additional arguments forwarded to `ds.omop.crosstab` (e.g.,
    `cohort`, `type`, `pooling_policy`, `symbol`, `datasources`,
    `execute`).

## Value

A `dsomop_result` object (see `ds.omop.crosstab`).

## Comorbidity is descriptive, not inferential

This reports raw person-overlap between two conditions. It is NOT an
adjusted measure of association and does not control for confounders
such as age. For a genuine multivariable comorbidity model, route to
`ds.glm`.

## Examples

``` r
if (FALSE) { # \dontrun{
cm <- ds.omop.comorbidity(316866, 201826)  # hypertension x type 2 diabetes
cm$per_site
} # }
```
