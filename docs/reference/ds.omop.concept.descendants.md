# Get descendant concepts via the concept_ancestor table

Traverses the OMOP `concept_ancestor` hierarchy on each connected server
and returns all descendant concepts for the given ancestor IDs. This is
the standard way to expand a high-level concept (e.g., "Diabetes
mellitus") into all of its more specific child concepts.

## Usage

``` r
ds.omop.concept.descendants(
  ancestor_ids,
  include_self = TRUE,
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- ancestor_ids:

  Integer or numeric vector of ancestor concept IDs to expand (e.g.,
  `c(201820)`).

- include_self:

  Logical; if `TRUE` (the default), the ancestor concepts themselves are
  included in the result alongside their descendants.

- symbol:

  Character; the session symbol used when the OMOP connection was
  initialised (default: `"omop"`).

- conns:

  DSI connection object(s). If `NULL` (the default), the connections
  stored in the active session are used.

- execute:

  Logical; if `FALSE`, returns a dry-run `dsomop_result` containing only
  the reproducible R code without contacting the servers.

## Value

A `dsomop_result` object with `scope = "pooled"` (a de-duplicated
cross-site view of the shared vocabulary; per-site frames remain
available). Each server's result is a data frame of descendant concept
rows.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all descendants of "Type 2 diabetes mellitus" (concept 201826)
desc <- ds.omop.concept.descendants(201826)
nrow(desc$per_site[[1]])

# Exclude the ancestor itself
desc <- ds.omop.concept.descendants(201826, include_self = FALSE)
} # }
```
