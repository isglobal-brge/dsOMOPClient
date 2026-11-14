# Shared strict public-definition parser (A-31 Q01/Q02).
# No patient, vocabulary or network access occurs here.
.ohdsi_definition_error <- function(code, path, remedy) {
  stop(code, ": ", path, "; ", remedy, call. = FALSE)
}

.ohdsi_read_definition <- function(x) {
  if (is.character(x)) {
    if (length(x) != 1L || is.na(x)) {
      .ohdsi_definition_error("invalid_definition", "$", "provide one JSON definition or list")
    }
    if (nchar(x, type = "bytes") < 4096L && file.exists(x)) {
      size <- file.info(x)$size
      if (is.na(size) || size > 10485760L) {
        .ohdsi_definition_error("invalid_definition", "$", "provide a definition under 10 MiB")
      }
      x <- paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    }
    if (nchar(x, type = "bytes") > 10485760L) {
      .ohdsi_definition_error("invalid_definition", "$", "provide a definition under 10 MiB")
    }
    x <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) {
      .ohdsi_definition_error("invalid_definition", "$", "provide valid JSON")
    })
  }
  nodes <- 0L
  walk <- function(value, depth = 0L) {
    nodes <<- nodes + 1L
    if (depth > 64L || nodes > 100000L) {
      .ohdsi_definition_error("invalid_definition", "$", "reduce definition size or nesting")
    }
    if (is.list(value)) {
      nm <- names(value)
      if (!is.null(nm) && (anyNA(nm) || any(!nzchar(nm)) || anyDuplicated(nm))) {
        .ohdsi_definition_error("duplicate_definition_key", "$", "use unique nonempty object keys")
      }
      for (child in value) walk(child, depth + 1L)
    }
    invisible(NULL)
  }
  if (!is.list(x) || is.object(x)) {
    .ohdsi_definition_error("invalid_definition", "$", "provide a JSON object or array")
  }
  walk(x)
  x
}

.ohdsi_concept_items <- function(expr, path = "$.expression") {
  fail <- function(code, suffix, remedy) .ohdsi_definition_error(code, paste0(path, suffix), remedy)
  if (!is.list(expr) || is.object(expr) || is.null(names(expr)) ||
      anyDuplicated(names(expr)) || !identical(names(expr), "items") ||
      !is.list(expr$items) || !is.null(names(expr$items)) || !length(expr$items)) {
    fail("invalid_concept_expression", "", "provide one nonempty items array")
  }
  descriptive <- c("CONCEPT_NAME", "STANDARD_CONCEPT", "STANDARD_CONCEPT_CAPTION",
                   "INVALID_REASON", "INVALID_REASON_CAPTION", "CONCEPT_CODE",
                   "DOMAIN_ID", "VOCABULARY_ID", "CONCEPT_CLASS_ID",
                   "VALID_START_DATE", "VALID_END_DATE")
  flags <- c("isExcluded", "includeDescendants", "includeMapped")
  lapply(seq_along(expr$items), function(i) {
    it <- expr$items[[i]]
    at <- paste0(".items[", i, "]")
    if (!is.list(it) || is.object(it) || is.null(names(it)) ||
        anyDuplicated(names(it)) || !"concept" %in% names(it) ||
        any(!names(it) %in% c("concept", flags))) {
      fail("invalid_concept_item", at, "provide concept and Boolean expansion/exclusion flags only")
    }
    con <- it$concept
    if (!is.list(con) || is.object(con) || is.null(names(con)) ||
        anyDuplicated(names(con)) || !"CONCEPT_ID" %in% names(con) ||
        any(!names(con) %in% c("CONCEPT_ID", descriptive))) {
      fail("invalid_concept_id", paste0(at, ".concept"), "provide CONCEPT_ID and documented descriptive fields only")
    }
    id <- con$CONCEPT_ID
    if (!typeof(id) %in% c("integer", "double") || is.object(id) || !is.null(dim(id)) ||
        length(id) != 1L || is.na(id) || !is.finite(id) ||
        id != floor(id) || id < 0 || id > 2147483647) {
      fail("invalid_concept_id", paste0(at, ".concept.CONCEPT_ID"),
           "provide a non-negative integer JSON number in 0..2147483647")
    }
    values <- lapply(flags, function(flag) {
      if (!flag %in% names(it)) return(FALSE)
      v <- it[[flag]]
      if (!is.logical(v) || is.object(v) || !is.null(dim(v)) || length(v) != 1L || is.na(v)) {
        fail("invalid_concept_flag", paste0(at, ".", flag), "provide a JSON Boolean; omit the flag for false")
      }
      unname(v)
    })
    list(concept_id = as.integer(id), is_excluded = values[[1L]],
         include_descendants = values[[2L]], include_mapped = values[[3L]], item_index = i)
  })
}
