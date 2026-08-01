# Save an extraction plan to JSON or YAML

Serializes an `omop_plan` to a file so it can be version-controlled,
shared, and re-run later. The on-disk format is a faithful, class-free
copy of the plan (all outputs, nested `filters$custom` and/or trees,
`concept_set`s, `time_window`s, cohort, and representation formats),
tagged with a schema version. The format is chosen from the file
extension unless given explicitly: `.json` uses jsonlite; `.yaml`/`.yml`
uses yaml. A plan reloaded with
[`ds.omop.plan.load`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.load.md)
executes identically to the original (the round-trip is lossless with
respect to what is sent to the server).

## Usage

``` r
ds.omop.plan.save(plan, file, format = NULL)
```

## Arguments

- plan:

  An `omop_plan` object.

- file:

  Character; destination path ending in `.json`, `.yml`, or `.yaml`.

- format:

  Character or `NULL`; optional explicit format (`"json"` or `"yaml"`)
  overriding the extension.

## Value

The file path, invisibly.

## See also

[`ds.omop.plan.load`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.load.md),
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan)
plan <- ds.omop.plan.events(plan, "conditions",
  "condition_occurrence", concept_set = c(201826))

ds.omop.plan.save(plan, "extraction.json")
ds.omop.plan.save(plan, "extraction.yaml")
} # }
```
