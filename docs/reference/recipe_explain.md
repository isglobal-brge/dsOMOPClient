# Explain a recipe: its semantics and every default it relies on

Renders what a recipe means as prose: each population (index, selection,
end, observation coverage, criteria), each variable (reducer, window,
missing-value rule, the column class of what it projects), each output
(grain, collapse, follow-up, censoring, representation) and the
publication roots. Every default the recipe relies on is stated (for
example the 730-day follow-up of a survival output without `tar`, the
banded `age_group` of a baseline output, the unique/drop/drop
observation coverage of an indexed population). No counts and no SQL are
shown.

## Usage

``` r
recipe_explain(
  recipe,
  detail = c("summary", "semantics"),
  conns = NULL,
  symbol = "omop"
)
```

## Arguments

  - recipe:

    An `omop_recipe`.

  - detail:

    `"summary"` (one line per node) or `"semantics"` (every default and
    rule spelled out).

  - conns:

    Optional DSI connections (uses the active OMOP session).

  - symbol:

    OMOP session symbol used with `conns`.

## Value

An `omop_recipe_explain` object: `$text` (character lines), `$selected`
(`"v1"` or `"v2"`), `$payload` (the selected wire plan), `$plan` (the
normalised plan, or `NULL` for an offline v1 explanation), `$defaults`
(a data frame of the defaults relied on), `$digest` (the semantic
digest, or `NULL` when unavailable), `$server` (per-server explain
payloads, when `conns` is given) and `$refusal` (why execution on these
servers would be refused, if it would). Printing it shows the text.

## Details

The plan explained is the plan execution would send (amendment A-14):
`recipe_explain()`, `recipe_validate`, `recipe_preview` and
`recipe_execute` share one plan-selection rule and, with `conns`, the
same target capabilities. A language-2 plan is explained from its normal
form with its semantic digest. A recipe that executes as its v1 plan is
explained from that v1 payload and the legacy rules it relies on;
offline its lowered IR and digest are not available (the server lowers
it), and its native rendering (`recipe_to_plan(version = 2)`) is never
presented as the executed plan. With `conns`, each server whose profile
publishes `omopPlanExplainDS` (dsOMOP 2.10.3) receives the selected wire
plan and reports its normalised IR, compatibility variants and semantic
digest (attached as `$server`). A server whose profile does not publish
it, and a call that fails, is reported by site and changes nothing else.
For a v1 plan the IR and digest are taken from the servers that
explained it when they agree; otherwise they are reported unavailable
(`$digest` is then `NULL`). The execution route on these servers (the
publication protocol, the legacy call of a federation with an older
server, or a refusal) is stated, and a recipe they could not execute is
still explained, with the refusal reported.

With connections, a matching last-successful-publication receipt reports
permitted conflicts, chosen labels and versions. It uses only the factor
metadata already admitted for each retained component; explain does not
query category occurrence. Before publication those names are
unresolved.

## See also

`recipe_lint`, `recipe_to_plan`

## Examples

``` r
if (FALSE) { # \dontrun{
recipe_explain(recipe, detail = "semantics")
} # }
```
