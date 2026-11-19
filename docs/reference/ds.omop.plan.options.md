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

  - translate\_concepts:
    
    Logical; chooses concept factor labels: vocabulary names when
    `TRUE`, decimal ID strings when `FALSE`. Concept columns always have
    shared factor levels across retained servers.

  - block\_sensitive:
    
    Logical; if `TRUE`, sensitive columns (e.g. exact dates, free-text
    notes) are excluded from outputs.

  - factor\_concepts:
    
    Logical; `TRUE` remains accepted. Since 2.10.6, concept IDs are
    always categories and `FALSE` is rejected. Discovery uses
    DataSHIELD's factor-level rule; a refusal prevents publication.

## Value

The modified `omop_plan` with updated options.

## See also

`ds.omop.plan`, `ds.omop.plan.execute`

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
