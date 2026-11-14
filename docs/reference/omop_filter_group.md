# Create an AND/OR group of filters

Combines multiple filters (or nested groups) using a Boolean operator.
Groups can contain `omop_filter` objects or other `omop_filter_group`
objects, allowing arbitrarily nested condition trees. During plan
compilation, these are translated to the server's filter DSL.

## Usage

``` r
omop_filter_group(..., operator = c("AND", "OR", "NOT"), label = NULL)
```

## Arguments

  - ...:

    `omop_filter` or `omop_filter_group` objects to combine.

  - operator:

    Character; `"AND"`, `"OR"` or (2.10.3) `"NOT"`, which negates the
    conjunction of its children: the legacy spelling of `omop_not()`. A
    NOT group has no v1 rendering; it runs as a language-2 plan on
    dsOMOP 2.10.3 servers.

  - label:

    Character or `NULL`; human-readable description (auto-generated from
    children if `NULL`).

## Value

An `omop_filter_group` object.

## See also

`omop_filter`, `omop_recipe`

## Examples

``` r
if (FALSE) { # \dontrun{
grp <- omop_filter_group(
  omop_filter_sex("F"),
  omop_filter_age(min = 18, max = 65),
  operator = "AND"
)
} # }
```
