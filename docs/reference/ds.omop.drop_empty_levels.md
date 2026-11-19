# Drop levels that are empty at every site, for models

Published factor columns share one level set across sites, including
levels that no site uses (declared vocabulary lists, calendars, level
ranges). A level empty at every site gives an all-zero design column, so
a pooled `ds.glm()` fails as singular. This makes a copy for models
whose factor columns keep only the levels that hold a value at some
site, identical at every site and in the same order; each copy keeps its
own rows and values.

## Usage

``` r
ds.omop.drop_empty_levels(
  x,
  columns = NULL,
  newobj = paste0(x, "_model"),
  symbol = "omop",
  conns = NULL
)
```

## Arguments

  - x:
    
    Character; a published in-memory table on every site (a
    `recipe_execute()` output, for example).

  - columns:
    
    Optional character vector of factor columns; by default every
    published factor column.

  - newobj:
    
    Character; the server-side name of the copy.

  - symbol:
    
    Character; the OMOP session symbol.

  - conns:
    
    DSI connections; defaults to the session's connections.

## Value

Invisibly, a data frame with one row per column: `levels` before, `kept`
and `status`.

## Details

Which levels hold a value at a site is an observed-level release: each
site reports it under DataSHIELD's factor-level rule
(`nfilter.levels.max` and `nfilter.levels.density`, as `ds.asFactor`
does), without counts. A column whose levels in use fail that rule at
some site keeps all its levels; group it first
(`omop_category_recode()`, `omop_category_rollup()`), which also suits
models better.

The copy is for analysis. Its dropped columns no longer carry the dsOMOP
presentation contract, so publish again from the recipe for anything
else.

## Examples

``` r
if (FALSE) { # \dontrun{
recipe_execute(recipe, out = c(study = "D"))
ds.omop.drop_empty_levels("D", newobj = "D_model")
dsBaseClient::ds.glm(died ~ cause_of_death, data = "D_model", family = "binomial")
} # }
```
