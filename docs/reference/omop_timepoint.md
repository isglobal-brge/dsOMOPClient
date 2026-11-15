# Prespecified repeated-measure columns

Prespecified repeated-measure columns

## Usage

``` r
omop_timepoint(
  offset = 0L,
  unit = "days",
  tolerance = c(0L, 0L),
  anchor = NULL,
  select = "closest"
)

omop_timepoints(points, anchor = "index", reuse = TRUE)

omop_visits(numbers, source, window = NULL, select = "last")

omop_repeated(
  event,
  values,
  timepoints = NULL,
  visits = NULL,
  missing = "keep",
  category_spec = NULL,
  broadcast = FALSE
)
```

## Arguments

  - offset:
    
    Signed integer offset from the original anchor.

  - unit:
    
    days, weeks, months or years.

  - tolerance:
    
    Inclusive lower/upper day offsets around each target.

  - anchor:
    
    Typed anchor or fixed date; NULL inherits the grid anchor.

  - select:
    
    closest, first or last.

  - points:
    
    Named ordered list of time points.

  - reuse:
    
    Allow one event to serve overlapping time points.

  - numbers:
    
    Unique positive visit ordinals, in output order.

  - source:
    
    Registered visit\_occurrence event.

  - window:
    
    Optional visit-start window.

  - event:
    
    One typed event or registered event name.

  - values:
    
    Named list of same-event numeric/category row projections.

  - timepoints:
    
    A time-point grid.

  - visits:
    
    A visit grid; exactly one grid is required.

  - missing:
    
    keep, skip\_all or skip\_any for canonical operands.

  - category\_spec:
    
    Bundle category defaults.

  - broadcast:
    
    Explicit scalar broadcast to long event rows.

## Value

A time/visit declaration or named bundle of scalar variables.
