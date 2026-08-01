# Render an entry's client-side plot over already-gated pooled data

The plotting half of the analysis catalog runs ENTIRELY on the client,
over data that has ALREADY passed the server's single per-patient
disclosure gate. For entries that ship one, the server returns
declarative plot metadata. The client accepts only an allowlisted
`plot$type` plus optional column-name mappings and dispatches to local
renderers. Remote source text is never parsed or evaluated.

## Usage

``` r
.analysis_render_plot(meta, pooled, params)
```

## Arguments

- meta:

  Named list; one entry's metadata from `omopAnalysisGetDS`. The plot
  recipe is read from `meta$plot` (a `list(type, mapping)`), tolerating
  a nested `meta$compute$plot` for forward compatibility.

- pooled:

  Data frame; the pooled, gate-passed aggregate to plot.

- params:

  Named list; retained for API compatibility with future local
  renderers. It is never evaluated as code.

## Value

A `ggplot` object, or `NULL` when the entry ships no plot recipe or the
recipe could not be built (with a warning in the latter case).

## Details

Safety model:

- Server metadata cannot execute code in the analyst's R process.

- Only a local, allowlisted renderer sees `df` — the pooled data frame
  that already cleared the gate (small-cell suppressed, banded and
  distribution-protected). Remote source text is ignored.

- Rendering is wrapped in
  [`tryCatch`](https://rdrr.io/r/base/conditions.html) so incompatible
  declarative metadata never costs the analyst the already-returned
  data; it degrades to a warning and `NULL` plot.

ggplot2 is required only on this path (`plot = TRUE`); a clear message
is raised if it is not installed, rather than failing obscurely inside
the recipe.
