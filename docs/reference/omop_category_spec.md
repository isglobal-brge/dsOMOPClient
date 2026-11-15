# Category identity, ordering and presentation

These declarations use public typed keys, never factor storage codes or
observed category discovery. Explicit variable fields override recipe
defaults. Nested declarations are replaced atomically. Values remain on
the server and existing metadata and publication policies still apply.

## Usage

``` r
omop_category_spec(
  levels = NULL,
  order = NULL,
  ordered = NULL,
  reference = NULL,
  recode = NULL,
  rollup = NULL,
  label_style = NULL,
  language = NULL,
  missing = NULL,
  binary = NULL
)

omop_category(variable, category_spec = NULL, broadcast = NULL)

omop_category_order(keys = NULL, metadata = NULL)

omop_category_recode(
  map,
  levels,
  labels = NULL,
  ordered = FALSE,
  unmatched = "keep"
)

omop_category_rollup(
  kind,
  level = NULL,
  ancestors = NULL,
  separation = NULL,
  distance = "min",
  vocabulary = NULL,
  domain = NULL,
  multiple = "error",
  priority = NULL,
  unmatched = "keep"
)

omop_category_language(language_concept_id)

omop_category_missing(
  na = "keep",
  unknown = "keep",
  labels = c(missing = "Missing", unknown = "Unknown")
)

omop_category_binary(as = "integer", labels = c("no", "yes"))
```

## Arguments

  - levels:
    
    Complete declared category keys, including unused categories.

  - order:
    
    An order declaration.

  - ordered:
    
    Whether the resulting factor is ordered.

  - reference:
    
    Treatment reference key; independent of display order.

  - recode:
    
    A finite nominal recode declaration.

  - rollup:
    
    A vocabulary-only hierarchy declaration.

  - label\_style:
    
    name, code, code\_name, vocabulary\_code or source\_code.

  - language:
    
    A language declaration, or en for English fallback.

  - missing:
    
    A missing/unknown declaration.

  - binary:
    
    A semantic flag representation declaration.

  - variable:
    
    An existing typed variable declaration.

  - category\_spec:
    
    A category specification.

  - broadcast:
    
    Explicit scalar broadcast, or NULL to preserve the input.

  - keys:
    
    Unique canonical category keys.

  - metadata:
    
    identity, producer, or public id/version/sha256/keys metadata.

  - map:
    
    Named vector mapping input keys to nominal category keys.

  - labels:
    
    Public labels, in declared order.

  - unmatched:
    
    keep or missing for unmapped inputs.

  - kind:
    
    none, atc, icd10, snomed or ancestors.

  - level:
    
    ATC class 1..5, or ICD10 chapter/block.

  - ancestors:
    
    Public ancestor concept keys.

  - separation:
    
    Exact nonnegative hierarchy separation.

  - distance:
    
    min or max separation, only with separation.

  - vocabulary:
    
    Explicit vocabulary for ICD10 or named ancestors.

  - domain:
    
    Public input concept keys.

  - multiple:
    
    error, priority or all for ambiguous targets.

  - priority:
    
    Complete public target priority order.

  - language\_concept\_id:
    
    Positive vocabulary language concept ID.

  - na:
    
    keep or level for ordinary available missing values.

  - unknown:
    
    keep, level or missing for concept zero.

  - as:
    
    integer, logical or factor for semantic flags.

## Value

A closed category declaration or a copy of a variable.
