# Record execute-produced symbols on the stored session

After a plan or recipe execution, stamps the produced server-side
symbols onto the `omop_session` held in `.dsomop_client_env` so the
manipulation wrappers can default their target symbol to the most recent
output. Updates `session$outputs` (accumulated, de-duplicated) and
`session$last_output` (the final symbol of this execution), then
persists the session back into the registry. Never throws: a missing
session simply skips recording.

## Usage

``` r
.record_session_outputs(symbol, out)
```

## Arguments

- symbol:

  Character; the session symbol used for the execution.

- out:

  Named list of exact produced component symbols, or a character vector
  for backwards-compatible internal use.

## Value

`NULL` invisibly.
