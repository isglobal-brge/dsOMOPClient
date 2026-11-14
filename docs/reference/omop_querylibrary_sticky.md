# Build an executable sticky redesign of an OHDSI QueryLibrary question

Creates a validated workflow specification containing an `omop_privacy`
object. The input table is not created by this function: it is a
server-created, person-local `omop.table` prepared for DP from a typed
recipe or plan, with the requested value and ordering columns. Give that
recipe or plan as `preparation` and
`ds.omop.querylibrary.sticky.release` prepares the input automatically
(dsOMOP 2.9.1 or later), or prepare it yourself with
`ds.omop.dp.prepare`. Tables loaded with `ds.omop.plan.execute` or
`recipe_execute` carry no DP provenance (the core never uses DP), and a
person-local table from a community-tool loader (an assign-mode
analysis-catalog entry) also works. Questions with an
`automatic_equivalent` in `omop_querylibrary_sticky_catalog` are
answered by the analysis catalog with no preparation at all. All levels,
breaks, bounds, positive values, reference dates and record caps are
public analysis choices fixed before data access; the data custodian
owns every privacy setting.

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

  - upstream\_id:

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

  - max\_contributions:

    Public person-level contribution cap. It is required for record
    counts, record histograms, and bounded distinct cardinality; it
    defaults to one for a person/category presence histogram.

  - order\_by:

    Public longitudinal ordering column. It is required for a record
    histogram. First/last date mappings use `variable` itself when it is
    omitted.

  - population\_id:

    Optional public compatibility label passed to `omop_privacy`.

  - preparation:

    Optional `omop_recipe` or `omop_plan` that defines the release
    population and its variables. It must compile to exactly one output.
    `ds.omop.querylibrary.sticky.release` prepares the input from it
    automatically when no `x` is given.

## Value

An `omop_querylibrary_sticky` specification. Pass it to
`ds.omop.querylibrary.sticky.release` after preparing its input.

## Details

The literal upstream SQL is never returned or executed. Exact ZIP,
source/free-text labels, and patient/event rows fail closed. Record
counts and distinct-concept cardinality are bounded per person and
therefore target capped redesign estimands rather than the unbounded
upstream estimands. The resulting object delegates release to the one
server-owned fixed per-release sticky service, whose status and result
metadata state the implemented guarantee.
