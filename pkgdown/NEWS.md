# dsOMOPClient 2.10.8

Use matching dsOMOP and dsOMOPClient packages for the study examples in this book.
Publish the installed server methods and options in the selected Opal profile,
then check its published and running versions from the client.

Concept IDs are categories everywhere, with the same levels at every site.
Observed categories follow DataSHIELD's factor-level rule; declared lists
(`omop_concept_levels()`, recodes, label maps, concept sets) need no discovery.
When a site observes more categories than the rule admits, the servers declare
a public list that holds them, climbing the vocabulary's hierarchy only as far
as needed; dates by month or week declare every period of their observed years. Before a pooled
model, `ds.omop.drop_empty_levels()` drops levels that no site uses.

The package also provides numeric units, rounding, bins, thresholds,
transformations, change over time and clinical formulas. Categories support
declared ordering, labels, hierarchy groups and missing-value policies. Each
output has a codebook that records its column types and presentation rules.

Start with [Getting started](../articles/book-00-start.html), explore the
[worked examples](../articles/book-showcase.html), or read the
[complete package release history](https://github.com/isglobal-brge/dsOMOPClient/blob/main/NEWS.md).
