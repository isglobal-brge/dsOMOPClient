# Import an OHDSI Circe cohort expression as a recipe population

Reverse of
[`recipe_export_circe`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_circe.md):
parses an OHDSI Circe cohort-expression JSON (as produced by ATLAS or by
`recipe_export_circe`) into an `omop_population`, reconstructing the
supported constructs (explicit entry event, inclusion-rule occurrence /
presence criteria, demographics, observation windows, OR groups).
Unsupported semantics are rejected rather than warned about and dropped.
See
[`recipe_export_circe`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_circe.md)
for the full supported-constructs list.

## Usage

``` r
recipe_import_circe(file_or_json, id = NULL, label = NULL)
```

## Arguments

- file_or_json:

  Character; a Circe JSON string, or a path to a JSON file.

- id:

  Character or `NULL`; population ID for the result (defaults to the
  `.dsomop` round-trip hint, else `"imported"`).

- label:

  Character or `NULL`; population label (defaults likewise).

## Value

An `omop_population` object.

## See also

[`recipe_export_circe`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_export_circe.md),
[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md)

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- recipe_import_circe("cohort_from_atlas.json")
recipe <- omop_recipe(populations = pop,
                      outputs = omop_output(population_id = pop$id))
} # }
```
