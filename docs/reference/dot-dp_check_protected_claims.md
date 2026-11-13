# Check the guarantee claims of a protected release or protection label

One coherence check for typed payloads and catalogue protection labels
(B14). A fixed synopsis declares its guarantee scope
(`fixed_vector_before_exact_support_gate`), the finite epsilon of its
bounded vector and delta 0 (before the exact support gates). A heuristic
realised-contribution release declares the heuristic scope and claims no
finite mechanism epsilon and no delta. A mixed frame names the
protection of each block: exactly one protected route among its blocks,
whose scope the frame declares, and no frame-level epsilon or delta.
Standard, standard-fallback and unavailable labels claim no protected
scope, epsilon or delta. No route claims a finite epsilon for the whole
API transcript. A missing or contradictory descriptor is refused rather
than displayed.

## Usage

``` r
.dp_check_protected_claims(value, server, what = "release")
```

## Arguments

  - value:

    Payload or `dsomop_protection` label.

  - server:

    Server name (for messages).

  - what:

    Description of `value` (for messages).

## Value

`value`, invisibly.
