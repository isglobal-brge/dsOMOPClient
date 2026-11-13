# Execute a v1 plan with the legacy call of dsOMOPClient 2.10.2

The execution of dsOMOPClient 2.10.2 (tag 2.10.2), unchanged: the same
v1 payload and arguments (no `commit_policy`), fresh destinations for
every reserved composite sibling, one assignment per site, removal of
the status symbol, the static output-component check with removal of
every freshly assigned output on failure, and the federation's factor
harmonisation. No publication status is read and nothing is revoked.

## Usage

``` r
.plan_execute_legacy(
  plan,
  out,
  symbol,
  session,
  conns,
  output_mode,
  server_method,
  harmonize_factors
)
```

## Arguments

  - plan:

    A v1 plan (after the staged-mode preparation).

  - out:

    The caller's out mapping.

  - symbol:

    OMOP session symbol.

  - session:

    The OMOP session.

  - conns:

    DSI connections.

  - output\_mode:

    `"memory"` or `"staged"`.

  - server\_method:

    Assign method.

  - harmonize\_factors:

    Logical.

## Value

Invisibly, the resolved out mapping.
