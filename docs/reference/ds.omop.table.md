# Pool a published concept category with its complete level contract

Calls the real `dsBaseClient::ds.table` on a published in-memory concept
factor through a temporary protected copy with lossless category tokens.
Uses the existing disclosure-checked factor harmonisation transport and
maps counts back to the exact public labels, restoring unused declared
categories as zero cells in federation level order. All sites must
return valid counts; failed or suppressed tables are never zero-filled.

## Usage

``` r
ds.omop.table(data, column, conns = NULL)
```

## Arguments

  - data:

    Character; published server-side data frame symbol.

  - column:

    Character; concept factor column in that data frame.

  - conns:

    DSI connections for the retained sites. Defaults to the active
    DataSHIELD connections.

## Value

A one-dimensional `table` of pooled counts, including unused declared
categories with count zero.

## Details

dsBase rebuilds factors from observed values, so a direct `ds.table`
call can omit globally unused levels. Use this integration when the
pooled table must retain the complete declared category contract. Counts
exclude actual missing values; a literal label `"NA"` remains a
category. Commas and other free text are preserved. The temporary copy
is removed after analysis, including on failure; the published object is
unchanged. Requires the matching server implementation of the table
analysis transport.
