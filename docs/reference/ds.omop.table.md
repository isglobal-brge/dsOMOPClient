# Tables of published categories, as `ds.table()` returns them

Calls `dsBaseClient::ds.table()` on published dsOMOP factor columns
(concepts, categories, calendars, number bins) through a temporary copy
whose labels are plain tokens, and returns `ds.table()`'s own object
with the exact labels back: per-site counts and proportions, combined
counts and proportions, the validity message and, when asked, the
chi-squared tests. Labels with spaces, commas or any other characters
work; declared categories that no site has are shown with zero counts,
in the shared level order. The temporary copy is removed afterwards,
including on failure; the published table is unchanged.

## Usage

``` r
ds.omop.table(
  data,
  column,
  conns = NULL,
  by = NULL,
  useNA = c("no", "ifany", "always"),
  report.chisq.tests = FALSE,
  small = c("refuse", "missing"),
  datasources = NULL
)
```

## Arguments

  - data:
    
    Character; a published table on every site.

  - column:
    
    Character; a published factor column of `data` (the `rvar` of
    `ds.table()`).

  - conns:
    
    Deprecated name of `datasources`.

  - by:
    
    Optional character; a second published factor column for a two-way
    table (the `cvar` of `ds.table()`).

  - useNA, report.chisq.tests:
    
    As in `ds.table()`.

  - small:
    
    `"refuse"` (default) leaves DataSHIELD's small-cell refusal as it
    is; `"missing"` suppresses small categories or cells in the
    temporary copy.

  - datasources:
    
    DSI connections for the retained sites, as in dsBaseClient. `NULL`
    uses the OMOP session's connections, or every DataSHIELD connection
    when there is no session.

## Value

`ds.table()`'s object (`output.list` with `TABLE_rvar.by.study_counts`,
`TABLES.COMBINED_all.sources_counts` and their proportions for one
column; per-site and combined counts and proportions for two;
`validity.message`), of class `dsomop_table` for printing.

## Details

DataSHIELD's rules apply as in `ds.table()`: a site whose table has a
non-empty cell below `nfilter.tab`, or more categories in use than the
level rule allows, is reported in the validity message and left out
(never filled with zeros). With `small = "missing"`, categories (or,
with `by`, cells) held by fewer than `nfilter.tab` people at a site are
made missing in the temporary copy first, with secondary suppression so
that a total minus the table never gives a small count; a zero can then
mean "fewer than nfilter.tab" at that site.

Factor columns without a published contract, such as the copies made by
`ds.omop.drop_empty_levels` or `ds.omop.drop_small_categories`, are
passed to `ds.table()` as they are.

## Examples

``` r
if (FALSE) { # \dontrun{
tab <- ds.omop.table("D", "cause_of_death_first_value")
tab$output.list$TABLES.COMBINED_all.sources_counts
tab$output.list$TABLE_rvar.by.study_counts
ds.omop.table("D", "cause_of_death_first_value", by = "sex", small = "missing")
} # }
```
