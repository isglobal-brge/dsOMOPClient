# Import an OHDSI Circe cohort expression as a recipe population

Reverse of `recipe_export_circe`: parses an OHDSI Circe
cohort-expression JSON (as produced by ATLAS or by
`recipe_export_circe`) into an `omop_population`, reconstructing the
supported constructs (explicit entry event, inclusion-rule occurrence /
presence criteria, demographics, observation windows, OR groups).
Unsupported semantics are rejected rather than warned about and dropped.
See `recipe_export_circe` for the full supported-constructs list.

## Usage

``` r
recipe_import_circe(file_or_json, id = NULL, label = NULL, origin = NULL)
```

## Arguments

  - file\_or\_json:
    
    A Circe JSON string, a path to a JSON file, or a parsed list.
    Concept IDs must be scalar integral JSON numbers in 0..2147483647;
    text, arrays and classed numbers are refused. Present
    expansion/exclusion flags must be scalar Booleans; omitted flags
    mean false, while null is invalid. Duplicate object keys and
    unsupported item fields are refused.

  - id:
    
    Character or `NULL`; population ID for the result (defaults to the
    `.dsomop` round-trip hint, else `"imported"`).

  - label:
    
    Character or `NULL`; population label (defaults likewise).

  - origin:
    
    Optional public provenance list with uri, phenotype\_id, release,
    review\_status and deprecation\_status. URI
    credentials/query/fragment are stripped; no URI is fetched. Original
    JSON bytes/hash and adapter/digests stay client-side and survive
    recipe serialization. Edited populations are marked derivative when
    serialized; the original definition is retained.

## Value

An `omop_population` object.

## See also

`recipe_export_circe`, `omop_population`, `recipe_circe_check`

## Examples

``` r
if (FALSE) { # \dontrun{
pop <- recipe_import_circe("cohort_from_atlas.json")
recipe <- omop_recipe(populations = pop,
                      outputs = omop_output(population_id = pop$id))
} # }
```
