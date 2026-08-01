# Create an AND/OR group of filters

Combines multiple filters (or nested groups) using a Boolean operator.
Groups can contain
[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md)
objects or other `omop_filter_group` objects, allowing arbitrarily
nested condition trees. During plan compilation, these are translated to
the server's filter DSL.

## Usage

``` r
omop_filter_group(..., operator = c("AND", "OR"), label = NULL)
```

## Arguments

- ...:

  `omop_filter` or `omop_filter_group` objects to combine.

- operator:

  Character; `"AND"` or `"OR"`.

- label:

  Character or `NULL`; human-readable description (auto-generated from
  children if `NULL`).

## Value

An `omop_filter_group` object.

## See also

[`omop_filter`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_filter.md),
[`omop_recipe`](https://isglobal-brge.github.io/dsOMOPClient/reference/omop_recipe.md)

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
