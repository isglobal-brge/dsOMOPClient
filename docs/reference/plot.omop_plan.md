# Plot an extraction plan as a dependency graph

Renders the structure of an `omop_plan` as a small directed graph
linking the cohort node to each output node. By default the graph is
emitted as Graphviz DOT text (printed via `cat`) which can be piped to
`dot`, pasted into any Graphviz viewer, or rendered with the DiagrammeR
package. A base-graphics fallback (`engine = "base"`) draws a simple
cohort-to-outputs diagram using only base graphics, requiring no
additional packages. This replaces the interactive plan DAG.

## Usage

``` r
# S3 method for class 'omop_plan'
plot(x, engine = c("dot", "base"), ...)
```

## Arguments

- x:

  An `omop_plan` object.

- engine:

  Character; `"dot"` (default) to emit Graphviz DOT text, or `"base"` to
  draw a base-graphics diagram.

- ...:

  Additional arguments (ignored).

## Value

Invisibly: the DOT string when `engine = "dot"`, otherwise `x`. Output
is produced as a side effect.

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.baseline(plan)
plot(plan)                 # Graphviz DOT text
plot(plan, engine = "base")
} # }
```
