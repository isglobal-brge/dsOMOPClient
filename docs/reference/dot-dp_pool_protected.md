# Pool compatible protected typed releases

Every site must report the same complete public basis (owner, basis
identity including the registered domain labels, coordinates, families),
the same versioned projection and the same view (kind and public masks
of basis cells), and be a distinct logical source
(`.dp_protected_sources`). The signed basis components are summed once,
the complete basis is projected once with the server's rule, and every
view is derived from those projected cells: histogram bins are sums of
cells; a binary rate's numerator and denominator are the sums of its
numerator and denominator masks (R04); a one-site pool therefore equals
the site's displayed values. A distinct-label cardinality is not
additive across sites and is not pooled.

## Usage

``` r
.dp_pool_protected(per_site, privacy)
```
