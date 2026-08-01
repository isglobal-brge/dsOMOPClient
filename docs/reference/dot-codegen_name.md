# Quote a list element name for code generation when needed

Returns the name unchanged if it is a syntactic R name, otherwise wraps
it in backticks so `list(<name> = ...)` stays valid (filter IDs such as
`"f1_sex"` are syntactic; defensive for arbitrary IDs).

## Usage

``` r
.codegen_name(nm)
```

## Arguments

- nm:

  Character; the element name.

## Value

Character; a safe name token.
