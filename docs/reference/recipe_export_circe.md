# Export a recipe population to an OHDSI Circe cohort expression

Maps the recipe POPULATION / cohort layer to an OHDSI Circe
cohort-expression JSON (the format ATLAS imports/exports). The entry
event comes only from an explicit
[`omop_index_event`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_index_event.md).
This is an executable, fail-closed subset: unsupported semantics raise
an error rather than being omitted and accidentally broadening the
cohort.

## Usage

``` r
recipe_export_circe(recipe, population_id = "base", file = NULL)
```

## Arguments

- recipe:

  An `omop_recipe` object.

- population_id:

  Character; which population to export (default `"base"`).

- file:

  Character or `NULL`; path to write the Circe JSON to. If `NULL`, the
  JSON string is returned.

## Value

The Circe JSON string (if `file` is `NULL`) or the file path invisibly.

## Details

**Supported constructs (recipe \<-\> Circe):**

- `omop_index_event` on a condition/drug/measurement/observation/
  procedure/device/visit table -\> PrimaryCriteria First/Last. Direct
  dsOMOP plans also support All; Circe All is rejected because Circe
  subsequently applies ERA collapse while dsOMOP preserves individual
  episodes.

- `omop_filter_has_concept` -\> InclusionRule occurrence criteria; it is
  never implicitly promoted to an entry event.

- `omop_filter_not_has_concept` -\> InclusionRule occurrence "exactly 0"
  criteria.

- `omop_filter_concept_count` -\> InclusionRule occurrence "at least N"
  criteria.

- Presence-only `omop_filter_has_measurement` -\> Measurement criteria.
  Numeric bounds are rejected because Circe cannot carry the
  DataSHIELD-issued safe-bin contract.

- `omop_filter_sex` / `omop_filter_age` -\> DemographicCriteria Gender /
  Age.

- Filter `window = list(start, end)` day offsets -\> criteria
  `StartWindow` (index-relative days).

- `omop_filter_prior_observation` / `omop_filter_followup` -\> the entry
  event `ObservationWindow` (PriorDays / PostDays).

- An `omop_filter_group(operator = "OR")` -\> a nested Circe
  CriteriaGroup of Type ANY; the population's top-level AND criteria map
  to the cohort's implicit ALL.

**Intentionally unsupported** (rejected, never silently lost):
set-operation populations, `cohort_definition_id` references,
fixed-reference ages/windows, `age_group`, `visit_count`,
`missing_measurement`, `value_bin` / `value_concept` / `date_range`
(row-level) filters, and the recipe variable/output layer. Circe-only
end/censor strategies, non-start windows, unsupported occurrence
operators, multiple primary criteria, and nested groups are rejected on
import.

## See also

[`recipe_import_circe`](https://isglobal-brge.github.io/dsOMOPClient/reference/recipe_import_circe.md),
[`omop_population`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_population.md)

## Examples

``` r
if (FALSE) { # \dontrun{
recipe <- omop_recipe(
  populations = omop_population(
    id = "t2d", label = "Type 2 diabetes, female, 18-65",
    index_event = omop_index_event(201820, "condition_occurrence"),
    filters = list(
      omop_filter_sex("F"),
      omop_filter_age(18, 65))),
  outputs = omop_output(type = "wide", population_id = "t2d"))
circe_json <- recipe_export_circe(recipe, population_id = "t2d")
# Imports the executable supported subset:
pop <- recipe_import_circe(circe_json)
} # }
```
