# Define the OMOP event that anchors a longitudinal population

An index event is deliberately distinct from an inclusion filter. Every
matching source row is a candidate cohort episode; `primary_limit`
selects the first, last, or all candidates per person before the
population's index-relative filters are evaluated (the ordering used by
OHDSI Circe).

## Usage

``` r
omop_index_event(
  concept_id = NULL,
  table,
  concept_name = NULL,
  primary_limit = c("first", "last", "all"),
  include_descendants = FALSE,
  include_mapped = FALSE
)
```

## Arguments

- concept_id:

  Integer concept ID(s), or `NULL` for any event in the selected table.

- table:

  Character OMOP event table. The currently executable portable subset
  is condition_occurrence, drug_exposure, measurement, observation,
  procedure_occurrence, device_exposure, and visit_occurrence.

- concept_name:

  Optional human-readable concept-set name.

- primary_limit:

  Character; `"first"`, `"last"`, or `"all"` candidate events per
  person.

- include_descendants, include_mapped:

  Logical concept-set expansion flags.

## Value

An `omop_index_event` object.
