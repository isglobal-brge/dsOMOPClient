# Make categories held by very few people missing, for tables

DataSHIELD refuses a table or a summary of a factor at any site where a
non-empty category has fewer than `nfilter.tab` people (dsBase's
`ds.summary()` answers "INVALID object\!"); those counts are never
released. This makes a copy where, at each site, the values of such
categories are missing, so that tables and summaries of the other
categories work. While the people made missing number between 1 and
`nfilter.tab - 1`, the next smallest category is made missing too, so
that a total minus a table never gives a small count. Levels that no
site uses are then dropped, as `ds.omop.drop_empty_levels()` does.

## Usage

``` r
ds.omop.drop_small_categories(
  x,
  columns = NULL,
  newobj = paste0(x, "_small"),
  symbol = "omop",
  conns = NULL,
  datasources = NULL
)
```

## Arguments

  - x:
    
    Character; a published in-memory table on every site (a
    `recipe_execute()` output, for example).

  - columns:
    
    Optional character vector of factor columns; by default every
    published concept, category or calendar column (the columns the
    servers reconciled; other factors, such as legacy age bands, are
    left as they are).

  - newobj:
    
    Character; the server-side name of the copy.

  - symbol:
    
    Character; the OMOP session symbol.

  - conns:
    
    Deprecated name of `datasources`.

  - datasources:
    
    DSI connections, as in dsBaseClient; defaults to the session's
    connections.

## Value

Invisibly, the summary of `ds.omop.drop_empty_levels()` for the copy.

## Details

The copy loses those people's values in these columns: use it for
descriptive tables and summaries. For models, group rare categories
instead (`omop_category_recode()`, `omop_category_rollup()`). Each
column is treated on its own, so a two-way table of the copy can still
have small cells; `ds.omop.table(by = , small = "missing")` handles
those. As with dsBase's own tables, the suppression protects each table
on its own: tables of the same people suppressed differently, or counts
from other publications, can still be compared with it.

## Examples

``` r
if (FALSE) { # \dontrun{
ds.omop.drop_small_categories("D", newobj = "D_small")
dsBaseClient::ds.summary("D_small$cause_of_death_first_value")
ds.omop.table("D_small", "cause_of_death_first_value")
} # }
```
