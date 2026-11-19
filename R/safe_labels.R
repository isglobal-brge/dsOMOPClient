# Labels that every dsBase function carries on Opal (dsOMOP 2.10.9 servers
# apply them when the client asks). Identical to the server's .safeLabels():
# ASCII letters, digits, ".", "_", "-" and "+"; common Latin letters
# transliterated with a fixed table; other runs of characters become "_"; an
# empty label takes its fallback; collisions get the level's position.
.SAFE_LABEL_FROM <- paste0(
  "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑ",
  "ÒÓÔÕÖÙÚÛÜÝàáâãäå",
  "çèéêëìíîïñòóôõöù",
  "úûüýÿ")
.SAFE_LABEL_TO <- "AAAAAACEEEEIIIINOOOOOUUUUYaaaaaaceeeeiiiinooooouuuuyy"
.SAFE_LABEL_PAIRS <- c("ß" = "ss", "Æ" = "AE", "æ" = "ae", "Œ" = "OE", "œ" = "oe",
                       "Ø" = "O", "ø" = "o", "Đ" = "D", "đ" = "d", "Ł" = "L",
                       "ł" = "l", "Þ" = "TH", "þ" = "th", "Ð" = "D", "ð" = "d")

.safeLabels <- function(x, fallback = NULL) {
  x <- enc2utf8(as.character(x))
  clean <- function(v) {
    v <- chartr(.SAFE_LABEL_FROM, .SAFE_LABEL_TO, v)
    for (from in names(.SAFE_LABEL_PAIRS)) v <- gsub(from, .SAFE_LABEL_PAIRS[[from]], v, fixed = TRUE)
    v <- gsub("[^A-Za-z0-9._+-]+", "_", v, perl = TRUE)
    gsub("^_+|_+$", "", v, perl = TRUE)
  }
  out <- clean(x)
  out[is.na(x)] <- NA_character_
  empty <- !is.na(out) & !nzchar(out)
  if (any(empty) && length(fallback) == length(x)) out[empty] <- clean(as.character(fallback[empty]))
  out[!is.na(out) & !nzchar(out)] <- "level"
  # A collision keeps the first label and numbers the others by position.
  repeat {
    clash <- !is.na(out) & duplicated(out)
    if (!any(clash)) break
    out[clash] <- paste0(out[clash], "_", which(clash))
  }
  out
}

# A shared column as the servers publish it with safe labels; the exact
# labels stay in `names` for the codebook.
.conceptSafeColumn <- function(column) {
  levels <- as.character(unlist(column$levels, use.names = FALSE))
  ids <- as.character(unlist(column$ids %||% column$keys, use.names = FALSE))
  labels <- vapply(column$labels %||% list(), function(x) if (is.null(x) || !length(x)) NA_character_ else as.character(x[[1L]]), "")
  first <- if (length(ids) == length(labels)) ids[match(levels, labels)] else NULL
  safe <- .safeLabels(levels, first)
  column$names <- levels
  if (identical(safe, levels)) return(column)
  map <- stats::setNames(safe, levels)
  relabelled <- unname(map[labels])
  relabelled[is.na(labels)] <- NA_character_
  column$labels <- as.list(relabelled)
  column$levels <- safe
  column
}
