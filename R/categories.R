#' Category identity, ordering and presentation
#'
#' These declarations use public typed keys, never factor storage codes or
#' observed category discovery. Explicit variable fields override recipe
#' defaults. Nested declarations are replaced atomically. Values remain on
#' the server and existing metadata and publication policies still apply.
#' @param levels Complete declared category keys, including unused categories.
#'   For a concept column, \code{\link{omop_concept_levels}()} builds them from
#'   the public vocabulary; declared levels need no discovery, so no
#'   factor-level limit applies to them.
#' @param order An order declaration.
#' @param ordered Whether the resulting factor is ordered.
#' @param reference Treatment reference key; independent of display order.
#' @param recode A finite nominal recode declaration.
#' @param rollup A vocabulary-only hierarchy declaration.
#' @param label_style name, code, code_name, vocabulary_code or source_code.
#' @param language A language declaration, or en for English fallback.
#' @param missing A missing/unknown declaration.
#' @param binary A semantic flag representation declaration.
#' @param variable An existing typed variable declaration.
#' @param category_spec A category specification.
#' @param broadcast Explicit scalar broadcast, or NULL to preserve the input.
#' @param keys Unique canonical category keys.
#' @param metadata identity, producer, or public id/version/sha256/keys metadata.
#' @param map Named vector mapping input keys to nominal category keys.
#' @param labels Public labels, in declared order.
#' @param unmatched keep or missing for unmapped inputs.
#' @param kind none, atc, icd10, snomed or ancestors.
#' @param level ATC class 1..5, or ICD10 chapter/block.
#' @param ancestors Public ancestor concept keys.
#' @param separation Exact nonnegative hierarchy separation.
#' @param distance min or max separation, only with separation.
#' @param vocabulary Explicit vocabulary for ICD10 or named ancestors.
#' @param domain Public input concept keys.
#' @param multiple error, priority or all for ambiguous targets.
#' @param priority Complete public target priority order.
#' @param language_concept_id Positive vocabulary language concept ID.
#' @param na keep or level for ordinary available missing values.
#' @param unknown keep, level or missing for concept zero.
#' @param as integer, logical or factor for semantic flags.
#' @return A closed category declaration or a copy of a variable.
#' @export
omop_category_spec <- function(levels=NULL, order=NULL, ordered=NULL,
  reference=NULL, recode=NULL, rollup=NULL, label_style=NULL, language=NULL,
  missing=NULL, binary=NULL) {
  s <- as.list(environment()); s <- s[!vapply(s,is.null,logical(1))]
  .categorySpecCheck(s)
  structure(s,class=c("omop_category_spec","list"))
}
.categoryArg <- function(s) {
  if (!is.null(s) && !inherits(s,"omop_category_spec")) .categoryReject("Construct category_spec with omop_category_spec().")
  .categorySpecCheck(s); s
}
.categoryTyped <- function(kind, fields) {
  structure(c(list(kind=kind),fields[!vapply(fields,is.null,logical(1))]),
    class=c(paste0("omop_",kind),"list"))
}
#' @rdname omop_category_spec
#' @export
omop_category <- function(variable, category_spec=NULL, broadcast=NULL) {
  if (!inherits(variable,"omop_variable")) .categoryReject("Wrap an existing typed variable declaration.")
  s <- .categoryArg(category_spec)
  effective <- .categoryEffective(s,variable$category_spec)
  variable$category_spec <- structure(effective,class=c("omop_category_spec","list"))
  if (!is.null(broadcast)) variable$broadcast <- .categoryBool(broadcast)
  variable
}
#' @rdname omop_category_spec
#' @export
omop_category_order <- function(keys=NULL, metadata=NULL) {
  x <- .categoryTyped("category_order",list(keys=keys,metadata=metadata))
  .categoryOrderCheck(x); x
}
#' @rdname omop_category_spec
#' @export
omop_category_recode <- function(map, levels, labels=NULL, ordered=FALSE, unmatched="keep") {
  x <- .categoryTyped("category_recode",list(map=map,levels=levels,labels=labels,ordered=ordered,unmatched=unmatched))
  .categoryRecodeCheck(x); x
}
#' @rdname omop_category_spec
#' @export
omop_category_rollup <- function(kind, level=NULL, ancestors=NULL,
  separation=NULL, distance="min", vocabulary=NULL, domain=NULL,
  multiple="error", priority=NULL, unmatched="keep") {
  fields <- as.list(environment()); fields$kind <- NULL
  if (kind == "none") {
    if (any(!vapply(fields[c("level","ancestors","separation","vocabulary","domain","priority")],is.null,logical(1))) ||
        !missing(distance) || !missing(multiple) || !missing(unmatched)) .categoryReject("rollup none has no other fields.")
    fields <- list()
  } else if (is.null(separation)) {
    if (!missing(distance)) .categoryReject("distance requires separation.")
    fields$distance <- NULL
  }
  x <- structure(c(list(kind=kind),fields[!vapply(fields,is.null,logical(1))]),class=c("omop_category_rollup","list"))
  .categoryRollupCheck(x); x
}
#' @rdname omop_category_spec
#' @export
omop_category_language <- function(language_concept_id) {
  x <- .categoryTyped("category_language",list(language_concept_id=language_concept_id))
  .categorySpecCheck(list(language=x)); x
}
#' @rdname omop_category_spec
#' @export
omop_category_missing <- function(na="keep", unknown="keep", labels=c(missing="Missing",unknown="Unknown")) {
  x <- .categoryTyped("category_missing",list(na=na,unknown=unknown,labels=labels))
  .categorySpecCheck(list(missing=x)); x
}
#' @rdname omop_category_spec
#' @export
omop_category_binary <- function(as="integer", labels=c("no","yes")) {
  x <- .categoryTyped("category_binary",list(as=as,labels=labels))
  .categorySpecCheck(list(binary=x)); x
}
.categoryWire <- function(x, field=NULL) {
  if (is.null(x)) return(NULL)
  if(inherits(x,"omop_concept_set"))return(.v2w_concept_set_inline(x))
  if (is.list(x)) {
    x <- unclass(x)
    if (!length(x)) return(.pnv2EmptyObject())
    return(setNames(lapply(seq_along(x),function(i).categoryWire(x[[i]],names(x)[i])),names(x)))
  }
  if (field %in% c("keys","levels","ancestors","priority","domain") || length(x)>1L || !is.null(names(x))) return(as.list(x))
  x
}
