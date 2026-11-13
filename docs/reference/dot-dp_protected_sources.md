# Require one distinct logical source per site before pooling

Pooling sums the signed components of every site, so a site counted
twice (two connections to one logical node) would double its
contribution. Every site must report its public logical source
identifier and all identifiers must differ; otherwise pooling is refused
(the per-site results remain available without pooling). Equal numeric
answers never establish or refute source identity. One site has nothing
to pool against.

## Usage

``` r
.dp_protected_sources(payloads, what = "protected releases")
```

## Arguments

  - payloads:

    Named list of payloads (typed releases or catalogue `dsomop_release`
    attributes).

  - what:

    Description for messages.

## Value

The identifiers, invisibly.
