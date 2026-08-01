# Search for OMOP concepts by name

Searches the vocabulary tables across all connected DataSHIELD servers
for concepts matching the given pattern and/or filters. Results include
concept ID, name, domain, vocabulary, and standard concept flag. The
search is executed server-side via `omopSearchConceptsDS` so that
vocabulary tables never leave the server. Vocabulary metadata carries no
patient data, so this reader is not disclosure-gated.

## Usage

``` r
ds.omop.concept.search(
  pattern = NULL,
  domain = NULL,
  vocabulary = NULL,
  standard_only = TRUE,
  limit = 50,
  concept_id = NULL,
  standard = NULL,
  valid = NULL,
  symbol = "omop",
  conns = NULL,
  execute = TRUE
)
```

## Arguments

- pattern:

  Character; search term or SQL LIKE pattern to match against concept
  names. Supports partial matching (e.g., `"diabetes"` matches
  `"Type 2 diabetes mellitus"`). Now optional: pass `NULL` to search by
  `concept_id` or by the metadata filters alone.

- domain:

  Character; restrict results to a specific OMOP domain (e.g.,
  `"Condition"`, `"Drug"`, `"Measurement"`). `NULL` (the default)
  returns concepts from all domains.

- vocabulary:

  Character; restrict results to a specific vocabulary (e.g.,
  `"SNOMED"`, `"ICD10CM"`, `"RxNorm"`). `NULL` (the default) searches
  across all vocabularies.

- standard_only:

  Logical; if `TRUE` (the default), only standard concepts are returned.
  Set to `FALSE` to include non-standard and classification concepts.
  Ignored when `standard` is supplied.

- limit:

  Integer; maximum number of results to return per server (default: 50).
  Increase for broader searches, but larger values will increase
  server-side processing time.

- concept_id:

  Integer or numeric vector; restrict the search to these exact concept
  IDs. `NULL` (the default) applies no ID filter.

- standard:

  Character; explicit `standard_concept` value to filter on (e.g. `"S"`
  for standard, `"C"` for classification). When supplied this overrides
  `standard_only`. `NULL` (the default) applies no explicit value
  filter.

- valid:

  Logical; `TRUE` keeps only currently-valid concepts
  (`invalid_reason IS NULL`), `FALSE` only invalidated ones. `NULL` (the
  default) returns both.

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
available). Each server's result is a data frame with columns such as
`concept_id`, `concept_name`, `domain_id`, `vocabulary_id`, and
`standard_concept`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Search for diabetes-related conditions
results <- ds.omop.concept.search("diabetes", domain = "Condition")
results$pooled

# Search across all domains, including non-standard concepts
all_hits <- ds.omop.concept.search("aspirin", standard_only = FALSE, limit = 100)

# Look up exact IDs, keeping only currently-valid standard concepts
hits <- ds.omop.concept.search(NULL, concept_id = c(201826, 4329847),
                               standard = "S", valid = TRUE)
} # }
```
