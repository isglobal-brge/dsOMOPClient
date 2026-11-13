# Pool protected analysis-catalog frames from their signed components

dsOMOP 2.10.3 protected catalog releases carry a `dsomop_release`
payload: per owner and stratum the signed noisy person total, histogram
cells reconciled to that total, bounded lattice moments, or person-time
status and time. Compatible sites (same entry, owners, strata, cells,
grids, bounds, caps and allocations) are pooled by summing the signed
components; every pooled view is derived once (person totals projected
top-down, cells onto their stratum, moments onto their feasible set).
Standard and protected frames are never mixed, nothing is pooled across
incompatible grids, and every site must be a distinct logical source:
two connections to one logical node are refused instead of being counted
twice.

## Usage

``` r
.pool_protected_release_frames(per_site, policy = "strict", columns = NULL)
```

## Arguments

  - per\_site:

    Named list of released frames.

  - policy:

    `"strict"` or `"pooled_only_ok"` (record-weighted strata present on
    only some servers).

  - columns:

    The entry's contracted columns (record-weighted schema).

## Value

List: `result` (one row per owner, stratum and interaction view with the
pooled persons, mean, SD, quantiles, events and time where defined; the
pooled cells as attribute `cells`) and `warnings`.

## Details

Complete person-time hierarchies are summed and projected from the root
down; public view masks select sums of those projected nodes. Nested
Achilles histograms use the parent's protected cells as their second
margin. First-event and capped recurrent event-time hierarchies project
their signed A/B/C nodes from the person or event total down before
selecting day, month or year views. Recurrent persons use a separate
complete interval-presence vector for distinct contributors among the
same K-capped occurrences. Its versioned radial projection enforces
public feasibility; retreat toward the event-mass anchor can add bias.
Coarse person counts are selected directly, never summed from fine bins.

Record-weighted releases (family `RM`) are pooled per stratum by
`.pool_protected_record_moments`. Exit-time x status cells of a
grouped-time Cox release (family `C8`) are summed and projected onto
their pooled C6 event/non-event parents. Interaction C6/C8 exports
retain the complete signed atomic family, projected before view
coarsening; supplemental rows and cells retain family and view
identifiers. C8 uses finite IPF with a feasible margin repair, not an
exact KL/L2 claim. Cox cells are never refitted; the event-weighted q x
exposure cells of an SCCS release (`w`) are not pooled (only its case
total).
