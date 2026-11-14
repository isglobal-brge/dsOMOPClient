# Normalise a version-2 plan: every default expanded exactly once

Rules (amendment A-11), in this order: an absent root \`options\` is
\`\` and absent registries are \`\`; the schema \`default\` keywords are
applied in the branch each instance matches; an absent \`publish\` is
the output names in declaration order; an \`index\`/\`criteria\`
population without \`coverage\` takes \`options.coverage\`, else
unique/drop/drop/0 (\`on\_missing = "keep"\` when its
\`index.restrict\_to\_observation\` is false); an output without
\`grain\` takes \`episode\` when its rows population is indexed and
\`person\` otherwise; a \`wide\` output with \`columns: null\` takes
every variable in declaration order; a \`survival\` output without
\`event\_order\` takes \`first\` for survival/competing\_risk and
\`all\` otherwise, and without \`outcome\_mode\` takes \`composite\` for
survival and \`named\` otherwise. No other field is added and none is
removed; the result is a fixed point.

## Usage

``` r
.planNormalizeV2(plan, schema = NULL)
```

## Arguments

  - plan:

    A version-2 plan (a parsed JSON list or a plan object).

  - schema:

    Optional parsed plan-v2 schema (the packaged one by default).

## Value

The normalised plan as a plain list (no S3 classes).
