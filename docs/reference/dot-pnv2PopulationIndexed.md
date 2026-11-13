# Is a population indexed (does its roster carry episodes)?

\`index\` and \`match\` populations are indexed; a \`criteria\`
population inherits from its parent; a \`union\` is indexed when any
member is; \`intersect\` and \`setdiff\` inherit from their first (left)
member, whose episodes they keep (design 6.9). A set-op's parent only
narrows membership.

## Usage

``` r
.pnv2PopulationIndexed(populations, id, seen = character(0))
```
