# Harmonize a plan for multi-server execution

Inspects table and column availability across connected servers. In
`"intersection"` mode the default strict policy rejects a plan whose
requested tables, compatible column types, implicit feature
dependencies, or output source contracts are not common to every server.
With `strict = FALSE`, only semantics-preserving column intersections
are attempted; an output is removed when its required dependencies are
absent. Population/cohort dependencies always fail closed. The returned
plan is bound to the compared servers and relevant schema snapshot and
is rechecked by validate, preview, and execute. No unsupported strategy
or output type is accepted silently.

## Usage

``` r
ds.omop.plan.harmonize(
  plan,
  mode = "intersection",
  strict = TRUE,
  symbol = "omop",
  conns = NULL,
  datasources = NULL
)
```

## Arguments

  - plan:
    
    An `omop_plan` object.

  - mode:
    
    Character; harmonization strategy. Only `"intersection"` is
    currently executable. The former `"union_with_missing"` placeholder
    is rejected until typed missing columns can be synthesized
    consistently on every backend.

  - strict:
    
    Logical; fail when any requested dependency is not common (default).
    If false, trim raw optional columns/tables or remove whole
    incompatible outputs with explicit warnings; population semantics
    are never weakened.

  - symbol:
    
    Character; name of the OMOP session symbol on the server (default
    `"omop"`).

  - conns:
    
    Deprecated alias of `datasources`, kept for backward compatibility
    (a one-time message names the new argument).

  - datasources:
    
    DataSHIELD connections (a named list of `DSConnection` objects).
    `NULL` (the default) uses the OMOP session's connections, or
    `DSI::datashield.connections_find()` when no session exists. See
    [dsomop\_conventions](https://isglobal-brge.github.io/dsOMOPClient/reference/dsomop_conventions.md).

## Value

The harmonized `omop_plan` with outputs adjusted for cross-server
compatibility.

## See also

`ds.omop.plan.execute`, `ds.omop.compare`

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- ds.omop.plan()
plan <- ds.omop.plan.person_level(plan,
  tables = list(person = c("gender_concept_id"),
                measurement = c("value_as_number")))
plan <- ds.omop.plan.harmonize(plan, mode = "intersection")
} # }
```
