# Validate one protected typed payload

Checks the protected-release protocol, route and guarantee scope,
forbidden top-level private-state fields, signed-component and
public-basis structure, view masks, and basic feasibility of displayed
counts and rates. It does not recompute displayed values from the signed
basis; pooling derives its own display from compatible signed
components.

## Usage

``` r
.dp_validate_protected_payload(value, server, privacy)
```
