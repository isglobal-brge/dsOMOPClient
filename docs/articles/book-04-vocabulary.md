# 4\. Vocabulary and concept sets

**What you will learn**

Find meanings, look up names and define a reusable concept set.

This chapter uses the public synthetic GiBleed practice connection from
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## Start this chapter

Start a fresh R session in the `book-work` directory created in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#get-the-book-helpers).
Run this setup first:

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
source("tools/book/practice.R")
practice <- if (book_driver == "DSLite") book_practice("gibleed") else book_connect("gibleed")
conns <- practice$conns
names(conns)
```

    #> [1] "site"

The `site` connection is ready. The shared `connection.R` selects DSLite
or Opal and resolves the resource names.

## Search a name

A concept ID identifies a meaning. It is not a patient’s ID. Search the
GiBleed vocabulary for asthma:

``` r
hits <- ds.omop.concept.search("Asthma", conns = conns)
knitr::kable(head(hits$pooled[c("concept_name", "domain_id", "vocabulary_id")], 8))
```

| concept\_name    | domain\_id | vocabulary\_id |
| :--------------- | :--------- | :------------- |
| Childhood asthma | Condition  | SNOMED         |
| Asthma           | Condition  | SNOMED         |
| Asthma screening | Procedure  | SNOMED         |

This is public vocabulary metadata. We show names in tables throughout
the book; numeric IDs occur only in code where they specify what to
select.

## Look up known diagnosis concepts

``` r
answers <- ds.omop.concept.lookup(c(317009L, 80502L), conns = conns)
answers$pooled[c("concept_name", "domain_id")]
```

    #>   concept_name domain_id
    #> 1       Asthma Condition
    #> 2 Osteoporosis Condition

These are diagnosis meanings. Looking them up does not tell you how many
people had either diagnosis.

## Include descendants deliberately

A *descendant* is a more specific meaning below a parent in a vocabulary
hierarchy. The asthma hierarchy can include more specific diagnosis
concepts.

``` r
desc <- ds.omop.concept.descendants(317009L, include_self = FALSE, conns = conns)
knitr::kable(head(desc$pooled["concept_name"], 8))
```

| concept\_name    |
| :--------------- |
| Childhood asthma |

Descendants come from `concept_ancestor`. Check their clinical meaning
before including them; a search result is not automatically a study
definition.

## Store a set

``` r
diagnoses <- omop_concept_set(c(317009L, 80502L))
class(diagnoses)
```

    #> [1] "omop_concept_set" "list"

A *concept set* is a reusable selection of concepts. For a focused
asthma definition, keep a separate named set:

``` r
asthma <- omop_concept_set(317009L)
class(asthma)
```

    #> [1] "omop_concept_set" "list"

Both objects are local definitions. Creating one has not selected any
patient rows.

## Recap

Use IDs to define selections and names to understand them. Chapter 5
switches to a small synthetic questionnaire fixture, where question and
answer concepts have different roles.

## Try it

Look up inpatient and outpatient visit concepts and show only their
names.

Solution

``` r
settings <- ds.omop.concept.lookup(c(9201L, 9202L), conns = conns)
settings$pooled["concept_name"]
```

    #>       concept_name
    #> 1  Inpatient Visit
    #> 2 Outpatient Visit
