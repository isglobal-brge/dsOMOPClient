# Private numeric presentation and expressions

Declare numeric computations without evaluating analyst code. Unit
conversion precedes source predicates/reduction; scalar transforms,
exact binary64 rounding and public ordered categories follow it.
Missing/invalid inputs stay missing; unavailable sources stay
unavailable even for presence/counts. Native language 2 and numeric
capabilities are required at every site.

## Usage

``` r
omop_number_spec(
  unit = NULL,
  transform = NULL,
  rounding = NULL,
  representation = NULL,
  breaks = NULL,
  grid = NULL,
  closed = NULL,
  labels = NULL,
  range = NULL,
  thresholds = NULL,
  op = NULL,
  category_map = NULL,
  levels = NULL,
  pipeline = NULL
)

omop_number_pipeline(...)

omop_number_unit(from, to, factor, offset = 0)

omop_number_transform(
  method = "identity",
  factor = NULL,
  offset = NULL,
  center = NULL,
  scale = NULL,
  lower = NULL,
  upper = NULL
)

omop_number_round(method = "none", digits = NULL)

omop_number_expr(exprs, result = NULL, unit = NULL)

omop_number_input(variable, stage = "canonical")

omop_number(variable, number_spec = NULL, broadcast = NULL)

omop_number_bins(
  variable,
  breaks = NULL,
  grid = NULL,
  closed = NULL,
  labels = NULL,
  number_spec = NULL,
  broadcast = NULL
)

omop_number_category(
  variable,
  range = NULL,
  thresholds = NULL,
  labels = NULL,
  number_spec = NULL,
  broadcast = NULL
)

omop_number_flag(
  variable,
  op,
  thresholds,
  closed = NULL,
  number_spec = NULL,
  broadcast = NULL
)

omop_number_present(variable, number_spec = NULL, broadcast = NULL)

omop_number_missing(variable, number_spec = NULL, broadcast = NULL)

omop_number_rows(event, value = NULL, number_spec = NULL)

omop_number_column(column, component = NULL, number_spec = NULL)

omop_number_count(
  event,
  value = NULL,
  window = NULL,
  kind = "nonmissing",
  number_spec = NULL,
  broadcast = FALSE
)

omop_number_change(
  event,
  method = "difference",
  value = NULL,
  window = NULL,
  missing = "skip",
  reference = NULL,
  time_unit = NULL,
  threshold = NULL,
  op = NULL,
  number_spec = NULL,
  broadcast = FALSE
)

omop_number_derive(
  inputs,
  exprs,
  unit,
  result = NULL,
  reducer = NULL,
  number_spec = NULL,
  broadcast = FALSE
)
```

## Arguments

  - unit:
    
    A source map, fixed unit conversion, source, or expression
    coordinate.

  - transform:
    
    One transform or an ordered nonempty transform list.

  - rounding:
    
    A numeric rounding declaration.

  - representation:
    
    number, bins, category, flag, present or missing.

  - breaks:
    
    Strictly increasing public boundaries; optional infinite tails.

  - grid:
    
    count\_1\_2\_5, proportion\_quarters or signed\_decades.

  - closed:
    
    left or right interval closure.

  - labels:
    
    Complete unique public labels, in numeric order.

  - range:
    
    reference, thresholds or bins.

  - thresholds:
    
    Public thresholds in the incoming numeric coordinate.

  - op:
    
    lt, le, gt, ge, eq, ne, inside or outside.

  - category\_map:
    
    Complete nondecreasing bin-to-category mapping.

  - levels:
    
    Complete ordered public category domain.

  - pipeline:
    
    Ordered typed steps, atomic for inheritance.

  - ...:
    
    Typed pipeline steps.

  - from, to:
    
    Canonical fixed-unit tokens.

  - factor, offset:
    
    Public affine parameters.

  - method:
    
    Transform, rounding or longitudinal method.

  - center, scale:
    
    Public standardisation constants; scale must be positive.

  - lower, upper:
    
    Finite clipping bounds.

  - digits:
    
    Decimal places (-308..308) or significant digits (1..17).

  - exprs:
    
    Closed mutate-v1 request, never executable R/SQL.

  - result:
    
    Assignment selected as result; defaults to the last assignment.

  - variable:
    
    A numeric variable declaration.

  - stage:
    
    canonical or presented numeric input.

  - number\_spec:
    
    Numeric presentation declaration.

  - broadcast:
    
    Explicit scalar broadcast to event rows (default false).

  - event:
    
    Typed event or registered source name.

  - value:
    
    Registered numeric value role/column.

  - column:
    
    Stable pre-rename logical numeric output column key.

  - component:
    
    Declared patient row-frame component name.

  - window:
    
    Explicit eligible-event window; NULL is unbounded.

  - kind:
    
    nonmissing or missing numeric row count.

  - missing:
    
    skip or keep endpoint missingness; slopes/thresholds require skip.

  - reference:
    
    First observation, anchor, index or fixed ISO origin.

  - time\_unit:
    
    days or weeks for slopes/attainment times.

  - threshold:
    
    Public observed-attainment threshold.

  - inputs:
    
    Named numeric declarations or explicit input bindings.

  - reducer:
    
    Optional numeric reducer, applied after same-event expressions.

## Value

A typed numeric specification, step, input or variable declaration.

## Details

Decimal rounding uses ties to even on the actual binary64 input. Public
bins are ordered factors with all declared levels, including unused
levels. Clinical reference categories require a selected observation
with paired bounds; use thresholds after aggregate reducers or
transforms. Numeric expressions use the existing closed mutate-v1
grammar. Inputs bind privately; canonical inputs bypass presentation and
never inherit outer recipe display defaults. Explicit presented inputs
must remain numeric. Rows and output-column copies preserve identity,
availability and privacy lineage. Counts and first-to-last summaries
create no population. Threshold times mean first observed attainment,
without interpolation. No observed range, unit inventory or failure
count is released.

## Examples

``` r
lab <- omop_event("measurement", 3000963L, unit = omop_unit(8713L))
mean_lab <- omop_variable(event = lab, reducer = omop_reduce("mean"))
rounded <- omop_number(mean_lab,
  omop_number_spec(rounding = omop_number_round("decimals", 1)))
band <- omop_number_bins(mean_lab, breaks = c(-Inf, 12, 15, Inf))
# A declared arithmetic coordinate with a closed expression.
doubled <- list(version = 1L, assign = list(list(name = "twice",
  expr = list(op = "*", args = list(list(col = "x"), list(lit = 2))))))
transformed <- omop_number_derive(list(x = mean_lab), doubled,
  unit = "coordinate:doubled_lab")
study <- omop_recipe(variables = list(raw = omop_number(mean_lab),
  rounded = rounded, band = band, doubled = transformed))
recipe_to_plan(study, version = 2)
```
