# Set plan-wide options

Configures global options that apply to all outputs in the plan. Only
non-NULL arguments are updated; existing option values are preserved for
omitted arguments.

## Usage

``` r
ds.omop.plan.options(
  plan,
  translate_concepts = NULL,
  block_sensitive = NULL,
  factor_concepts = NULL
)
```

## Arguments

- plan:

  An `omop_plan` object.

- translate_concepts:

  Logical; if `TRUE`, concept ID columns are automatically translated to
  human-readable concept names in output tables.

- block_sensitive:

  Logical; if `TRUE`, sensitive columns (e.g. exact dates, free-text
  notes) are excluded from outputs.

- factor_concepts:

  Logical; if `TRUE` (default), after a memory-mode execution every
  `_concept_id` column is converted into a factor whose levels are
  harmonized across all connected servers, so pooled
  `ds.glm`/`ds.glmSLMA`/`ds.table` see an identical level coding.
  Columns whose distinct values exceed the server disclosure cap are
  left raw. Set `FALSE` to keep the raw integer ids (or translated
  character names) unchanged.

## Value

The modified `omop_plan` with updated options.

## See also

[`ds.omop.plan`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.md),
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.options(plan,
  translate_concepts = TRUE,
  block_sensitive = TRUE,
  factor_concepts = TRUE
)
} # }
```
