# Local public-definition helpers. These never access server memory or rows.
book_find <- function(term, name, domain, conns, symbol="omop") {
  hits <- dsOMOPClient::ds.omop.concept.search(term, domain=domain,
    standard_only=TRUE, limit=200, conns=conns, symbol=symbol)$pooled
  selected <- hits[trimws(hits$concept_name)==name,,drop=FALSE]
  stopifnot(nrow(selected)==1L)
  knitr::kable(selected[intersect(c("concept_name","vocabulary_id","standard_concept",
    "concept_class_id","valid_start_date","valid_end_date"),names(selected))])
}
# Closed arithmetic AST constructors used visibly in the formula lesson.
book_col <- function(name) list(col=name)
book_lit <- function(value) list(lit=value)
book_op <- function(op,left,right) list(op=op,args=list(left,right))
book_expr <- function(expr) list(version=1L,assign=list(list(name="result",expr=expr)))
