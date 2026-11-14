### 4.13 `drug.adherence_components`

Parameters: common; `drug: concept_set` required; `horizon: integer = 365`,
at least 1. Fixed window `[0,horizon-1]` is exactly `horizon` calendar days.
Fixed class grouping `by="event"`, gap/persistence/min_days 0, stockpiling on.

```r
drug <- omop_param("drug", "concept_set")
horizon <- omop_param("horizon", "integer", default = 365L)
window <- omop_window(0L, horizon - 1L)
rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                 name = "rx")
stock <- omop_era(rx, gap = 0L, persistence = 0L, min_days = 0L, by = "event",
                  end = "recorded_end", fallback = "days_supply",
                  stockpile = TRUE, name = "stock")
events <- list(rx)
intervals <- list(stock)
variables <- list(
  observed_days = omop_observed_days(window),
  recorded_covered_days = omop_variable(event = stock,
    reducer = omop_reduce("covered_days", window = window,
                          coverage = "recorded", observed_only = TRUE)),
  effective_covered_days = omop_variable(event = stock,
    reducer = omop_reduce("covered_days", window = window,
                          coverage = "effective_supply", observed_only = TRUE)),
  supply_days = omop_variable(event = rx,
    reducer = omop_reduce("supply_days", window = window, fills = "clipped")))
outputs <- list(omop_output("adherence", rows = "study", format = "wide", grain = "episode", columns = names(variables), options = list(date_handling = "relative")))
publish <- "adherence"
```

Output: `adherence`, `K + observed_days, recorded_covered_days,
effective_covered_days, supply_days` (integers/NA). No `pdc` or `mpr` column
is falsely promised by this template. Carry-in is computed before clipping.
Effective coverage uses A-20's previous-effective-end recurrence and existing
operational stockpile cap; recorded coverage never becomes effective coverage.

Documentation: “Assign transparent components for class-level adherence.
PDC is effective covered observed days divided by observed days. The supplied
MPR numerator sums each fill's supplied days clipped to the calendar window,
including carry-in and overlapping fills; it is not observation-intersected
and can exceed the denominator. These are explicit study conventions, not a
universal adherence definition. Zero or missing denominator yields NA.”

The documentation may show the existing separate, analyst-invoked operation
after successful assignment:

```r
ds.omop.mutate(x = "analysis_adherence",
  pdc = ifelse(observed_days > 0, effective_covered_days / observed_days, NA_real_),
  mpr = ifelse(observed_days > 0, supply_days / observed_days, NA_real_),
  newobj = "analysis_adherence_ratios", conns = conns, symbol = "omop")
```

This is not automatically run by instantiation/execution of a library entry.
It is a separate protected final assignment under A-23, invalidates DP
provenance as specified there, and neither retrieves rows nor changes keys.
One-plan calculated ratios remain gap G1.



The complete executable constructor expansion is in `definition.R` in this directory and is returned by `recipe_library_describe()`.
