# Build an executable sticky redesign of an OHDSI QueryLibrary question

Creates a validated workflow specification containing an
[`omop_privacy`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_privacy.md)
object. The input table is not created by this function: it must be a
server-created, person-local `omop.table` produced by the typed
Recipe/Plan path, with the requested value and ordering columns. All
levels, breaks, bounds, positive values and record caps are public
analysis choices fixed before data access.

## Usage

``` r
omop_querylibrary_sticky(
  upstream_id,
  variable = NULL,
  levels = NULL,
  breaks = NULL,
  lower = NULL,
  upper = NULL,
  positive = NULL,
  max_contributions = NULL,
  order_by = NULL,
  population_id = NULL,
  preparation = NULL
)
```

## Arguments

- upstream_id:

  Published QueryLibrary ID in the pinned catalog.

- variable:

  Value column in the protected prepared table. Not used for a
  distinct-person count.

- levels:

  Complete fixed public domain for a categorical histogram.

- breaks:

  Fixed public numeric, ISO-date, or UTC-datetime histogram breaks.

- lower, upper:

  Fixed finite bounds for a bounded person mean.

- positive:

  Fixed public values defining a positive binary outcome.

- max_contributions:

  Public person-level contribution cap. It is required for record
  counts, record histograms, and bounded distinct cardinality; it
  defaults to one for a person/category presence histogram.

- order_by:

  Public longitudinal ordering column. It is required for a record
  histogram. First/last date mappings use `variable` itself when it is
  omitted.

- population_id:

  Optional public compatibility label passed to
  [`omop_privacy`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_privacy.md).

- preparation:

  Optional `omop_recipe` or `omop_plan` documenting how the protected
  input will be prepared. It must compile to exactly one output.
  Execution remains explicit so the assigned symbol is visible to the
  analyst and custodian.

## Value

An `omop_querylibrary_sticky` specification. Pass it to
[`ds.omop.querylibrary.sticky.release`](https://isglobal-brge.github.io/dsOMOPClient/reference/ds.omop.querylibrary.sticky.release.md)
after preparing its input.

## Details

The literal upstream SQL is never returned or executed. Exact ZIP,
source/free-text labels, and patient/event rows fail closed. Record
counts and distinct-concept cardinality are bounded per person and
therefore target capped redesign estimands rather than the unbounded
upstream estimands. The resulting object has no formal-DP switch:
release delegates to the one server-owned sticky privacy service, whose
status and result metadata state the implemented guarantee and
accounting limitations.
