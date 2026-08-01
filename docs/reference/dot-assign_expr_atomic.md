# Assign one server-side symbol atomically across a federation

Assign one server-side symbol atomically across a federation

## Usage

``` r
.assign_expr_atomic(
  conns,
  newobj,
  expr,
  context,
  session_symbol = NULL,
  required_symbols = character(0)
)
```

## Arguments

- conns:

  Named DataSHIELD connections.

- newobj:

  Destination symbol, which must not already exist.

- expr:

  Unevaluated assign expression.

- context:

  Human-readable operation label.

- session_symbol:

  Optional local OMOP session name to update after commit.

- required_symbols:

  Source symbols that must exist on every server.

## Value

\`newobj\`, invisibly.
