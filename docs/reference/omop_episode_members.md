# Events linked to oncology/treatment episodes

An event source whose rows are the events linked to the episodes of
`episode` (an `omop_event("episode", ...)`) through `episode_event`,
using the package's reviewed field-concept mapping. The same event
linked to two episodes yields two rows.

## Usage

``` r
omop_episode_members(
  episode,
  tables = NULL,
  where = NULL,
  name = NULL,
  on_missing_table = "error"
)
```

## Arguments

  - episode:

    An `omop_event` over the `episode` table (or its name).

  - tables:

    Optional member tables to keep (default: every mapped table).

  - where:

    Optional row predicate over the member rows.

  - name:

    Event name.

  - on\_missing\_table:

    `"error"` or `"unavailable"`.

## Value

An `omop_event` object (adapter kind `episode_members`).
