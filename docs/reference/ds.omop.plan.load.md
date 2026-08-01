# Load an extraction plan from JSON or YAML

Reconstructs an `omop_plan` previously written by
[`ds.omop.plan.save`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.save.md).
The parser is selected from the file extension (`.json` via jsonlite;
`.yaml`/`.yml` via yaml). Atomic vectors and integer concept/offset
fields are restored so the returned plan is accepted unchanged by
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)
and produces an identical execution.

## Usage

``` r
ds.omop.plan.load(file)
```

## Arguments

- file:

  Character; source path ending in `.json`, `.yml`, or `.yaml`.

## Value

An `omop_plan` object.

## See also

[`ds.omop.plan.save`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.save.md),
[`ds.omop.plan.execute`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.plan.execute.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan.load("extraction.json")
ds.omop.plan.execute(plan, out = "D")
} # }
```
