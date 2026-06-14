# Module: Disclosure-Gated Data Manipulation
# Client wrappers for the server-side omop.table manipulation verbs
# (omopMergeDS / omopFilterDS / omopSelectDS / omopBindRowsDS). Each takes the
# NAME of one (or two) server-side data.frame symbol(s) previously produced by a
# dsOMOP assign method, validates its arguments client-side, and asks every
# connected server to compute the new frame and store it under `newobj`. The
# server re-gates the result on the number of DISTINCT person tokens and fails
# closed (errors) when the result would describe too few individuals, so a
# too-small frame is never created.

#' Force-encode a scalar string for the DataSHIELD expression transport
#'
#' \code{\link{.ds_encode}} only base64-wraps lists and multi-element vectors;
#' a scalar string passes through as a bare literal. That is a problem for the
#' filter operator: the comparison operators (\code{">="}, \code{">"},
#' \code{"=="}, ...) contain \code{<}, \code{>}, \code{=} characters that the
#' DataSHIELD expression lexer (DSLite and Opal) refuses inside a bare string
#' literal ("Syntax error"). Wrapping the value as a single-element list routes
#' it through the same URL-safe base64 path, and the server-side \code{.ds_arg}
#' transparently decodes it back to the scalar.
#'
#' @param x A length-1 character value.
#' @return A \code{B64:}-prefixed token that the server's \code{.ds_arg} decodes
#'   back to the scalar string.
#' @keywords internal
.ds_encode_scalar <- function(x) {
  json <- as.character(jsonlite::toJSON(as.character(x)[[1]], auto_unbox = TRUE))
  b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
  b64 <- gsub("\\+", "-", b64)
  b64 <- gsub("/", "_", b64)
  b64 <- gsub("=+$", "", b64)
  paste0("B64:", b64)
}

#' Merge two server-side omop.table data frames on the person key
#'
#' Performs an inner or left join of two server-side, token-keyed data frames
#' on the per-session person token. The join is restricted to the person key so
#' a merge cannot correlate individuals on any other quasi-identifier. The
#' server re-gates the joined result on its distinct-person count and fails
#' closed if it falls below the disclosure threshold.
#'
#' @param x,y Character; names of the server-side \code{omop.table} symbols to
#'   join (e.g. the \code{newobj} returned by a previous verb, or a symbol
#'   created by \code{\link{ds.omop.plan.execute}}).
#' @param by Character vector; the join key. Must be the person key
#'   (\code{"person_id"} or \code{"subject_id"}); other columns are rejected
#'   server-side.
#' @param type Character; \code{"inner"} (default) or \code{"left"}.
#' @param newobj Character; name of the server-side symbol to create. If
#'   \code{NULL} (default), a unique name is generated.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly, the name of the created server-side symbol (\code{newobj}).
#' @examples
#' \dontrun{
#' joined <- ds.omop.merge("cohort_a", "cohort_b", type = "left")
#' }
#' @seealso \code{\link{ds.omop.filter}}, \code{\link{ds.omop.bind_rows}}
#' @export
ds.omop.merge <- function(x, y, by = "person_id", type = "inner",
                          newobj = NULL, symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  type <- match.arg(type, c("inner", "left"))
  if (!is.character(x) || length(x) != 1L ||
      !is.character(y) || length(y) != 1L) {
    stop("x and y must be the names of server-side omop.table symbols.",
         call. = FALSE)
  }
  newobj <- newobj %||% .generate_symbol("omop.merge")

  DSI::datashield.assign.expr(
    conns,
    symbol = newobj,
    expr = call("omopMergeDS", as.name(x), as.name(y),
                .ds_encode(by), type)
  )
  invisible(newobj)
}

#' Filter the rows of a server-side omop.table data frame
#'
#' Applies a single comparison filter to a server-side, token-keyed data frame.
#' Filtering on a protected/identifier column (the person token or any
#' \code{dsomop_protected} column) is rejected server-side. The server re-gates
#' the filtered result on its distinct-person count and fails closed if the
#' filter narrows the population below the disclosure threshold.
#'
#' @param x Character; name of the server-side \code{omop.table} symbol.
#' @param var Character; the (non-protected) column to filter on.
#' @param op Character; one of \code{"=="}, \code{"!="}, \code{">"},
#'   \code{">="}, \code{"<"}, \code{"<="}, \code{"in"}, \code{"not_in"}.
#' @param value Comparison value(s); scalar for the relational operators, a
#'   vector for \code{"in"} / \code{"not_in"}.
#' @param newobj Character; name of the server-side symbol to create. If
#'   \code{NULL} (default), a unique name is generated.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly, the name of the created server-side symbol (\code{newobj}).
#' @examples
#' \dontrun{
#' adults <- ds.omop.filter("features", var = "age", op = ">=", value = 18)
#' }
#' @seealso \code{\link{ds.omop.select}}, \code{\link{ds.omop.merge}}
#' @export
ds.omop.filter <- function(x, var, op, value,
                           newobj = NULL, symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  op <- match.arg(op, c("==", "!=", ">", ">=", "<", "<=", "in", "not_in"))
  if (!is.character(x) || length(x) != 1L) {
    stop("x must be the name of a server-side omop.table symbol.",
         call. = FALSE)
  }
  if (!is.character(var) || length(var) != 1L) {
    stop("var must be a single column name.", call. = FALSE)
  }
  newobj <- newobj %||% .generate_symbol("omop.filter")

  # The operator literal (and any scalar string value) contains characters the
  # DataSHIELD expression lexer rejects in a bare string ("<", ">", "="), so
  # force them through the base64 transport; .ds_encode already base64-wraps
  # vector values. The column name is a validated identifier and travels bare.
  value_enc <- if (is.character(value) && length(value) == 1L)
    .ds_encode_scalar(value) else .ds_encode(value)

  DSI::datashield.assign.expr(
    conns,
    symbol = newobj,
    expr = call("omopFilterDS", as.name(x), var, .ds_encode_scalar(op), value_enc)
  )
  invisible(newobj)
}

#' Keep a subset of columns of a server-side omop.table data frame
#'
#' Projects a server-side, token-keyed data frame to \code{cols}. The person
#' token column is always retained server-side so the result stays a valid,
#' joinable \code{omop.table}.
#'
#' @param x Character; name of the server-side \code{omop.table} symbol.
#' @param cols Character vector; the columns to keep.
#' @param newobj Character; name of the server-side symbol to create. If
#'   \code{NULL} (default), a unique name is generated.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly, the name of the created server-side symbol (\code{newobj}).
#' @examples
#' \dontrun{
#' slim <- ds.omop.select("features", cols = c("age", "sex"))
#' }
#' @seealso \code{\link{ds.omop.filter}}
#' @export
ds.omop.select <- function(x, cols,
                           newobj = NULL, symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  if (!is.character(x) || length(x) != 1L) {
    stop("x must be the name of a server-side omop.table symbol.",
         call. = FALSE)
  }
  if (!is.character(cols) || length(cols) == 0L) {
    stop("cols must be a non-empty character vector of column names.",
         call. = FALSE)
  }
  newobj <- newobj %||% .generate_symbol("omop.select")

  DSI::datashield.assign.expr(
    conns,
    symbol = newobj,
    expr = call("omopSelectDS", as.name(x), .ds_encode(cols))
  )
  invisible(newobj)
}

#' Row-bind two schema-identical server-side omop.table data frames
#'
#' Stacks two server-side, token-keyed data frames that share an identical set
#' of column names. The server re-gates the bound result on its DISTINCT person
#' count: because the gate counts distinct persons rather than rows, binding a
#' frame to itself cannot inflate the count and the result is still blocked when
#' it covers too few individuals.
#'
#' @param x,y Character; names of the server-side \code{omop.table} symbols to
#'   bind (must have identical column names server-side).
#' @param newobj Character; name of the server-side symbol to create. If
#'   \code{NULL} (default), a unique name is generated.
#' @param symbol Character; the session symbol used when the OMOP connection was
#'   initialised (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL} (the default), the
#'   connections stored in the active session are used.
#' @return Invisibly, the name of the created server-side symbol (\code{newobj}).
#' @examples
#' \dontrun{
#' stacked <- ds.omop.bind_rows("wave1", "wave2")
#' }
#' @seealso \code{\link{ds.omop.merge}}
#' @export
ds.omop.bind_rows <- function(x, y,
                              newobj = NULL, symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns
  if (!is.character(x) || length(x) != 1L ||
      !is.character(y) || length(y) != 1L) {
    stop("x and y must be the names of server-side omop.table symbols.",
         call. = FALSE)
  }
  newobj <- newobj %||% .generate_symbol("omop.bind")

  DSI::datashield.assign.expr(
    conns,
    symbol = newobj,
    expr = call("omopBindRowsDS", as.name(x), as.name(y))
  )
  invisible(newobj)
}
