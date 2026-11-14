# Cost rows linked to an event

An event source whose rows are the `cost` rows linked to the rows of a
table event (through the reviewed event-domain mapping). Its value is
the one declared amount `field`; amounts are never summed across
currencies (a source mixing currencies is refused unless `currency`
restricts it).

## Usage

``` r
omop_cost(
  event,
  field = "total_cost",
  currency = NULL,
  cost_type = NULL,
  where = NULL,
  name = NULL,
  on_missing_table = "error"
)
```

## Arguments

  - event:

    A table `omop_event` (or its name).

  - field:

    One cost amount column.

  - currency:

    Optional currency concept ID.

  - cost\_type:

    Optional cost type concept IDs.

  - where:

    Optional row predicate over the cost rows.

  - name:

    Event name.

  - on\_missing\_table:

    `"error"` or `"unavailable"`.

## Value

An `omop_event` object (adapter kind `cost`).
