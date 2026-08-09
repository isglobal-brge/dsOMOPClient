# Module: OHDSI Results Consumer (Client)
# Client-side wrappers for querying pre-computed OHDSI tool result tables.

.ohdsi_shape_frame <- function(value, columns, filters, order_by, limit) {
  if (is.null(value)) return(NULL)
  shaping <- !is.null(columns) || length(filters %||% list()) > 0L ||
    !is.null(order_by) || !identical(as.integer(limit), 5000L)
  if (!is.data.frame(value)) {
    if (shaping) {
      stop("columns, filters, order_by, and limit require a tabular OHDSI ",
           "analysis result.", call. = FALSE)
    }
    return(value)
  }

  actual <- names(value)
  match_column <- function(column, label) {
    index <- match(tolower(column), tolower(actual))
    if (is.na(index)) {
      stop("Unknown OHDSI result ", label, ": '", column, "'.",
           call. = FALSE)
    }
    actual[[index]]
  }

  if (!is.null(filters) && length(filters) > 0L) {
    if (!is.list(filters) || is.null(names(filters)) ||
        any(!nzchar(names(filters))) || anyDuplicated(names(filters))) {
      stop("filters must be a uniquely named list.", call. = FALSE)
    }
    keep <- rep(TRUE, nrow(value))
    for (column in names(filters)) {
      filter_value <- filters[[column]]
      if (length(filter_value) != 1L || is.na(filter_value)) {
        stop("Each result filter must contain one non-missing scalar.",
             call. = FALSE)
      }
      actual_column <- match_column(column, "filter column")
      matches <- value[[actual_column]] == filter_value
      matches[is.na(matches)] <- FALSE
      keep <- keep & matches
    }
    value <- value[keep, , drop = FALSE]
  }

  if (!is.null(order_by)) {
    if (!is.character(order_by) || length(order_by) != 1L ||
        is.na(order_by) ||
        !grepl("^[A-Za-z_][A-Za-z0-9_]*( (ASC|DESC))?$", trimws(order_by),
               ignore.case = TRUE)) {
      stop("order_by must be one column with optional ASC or DESC.",
           call. = FALSE)
    }
    order_by <- trimws(order_by)
    descending <- grepl(" DESC$", order_by, ignore.case = TRUE)
    order_column <- sub(" (ASC|DESC)$", "", order_by, ignore.case = TRUE)
    actual_column <- match_column(order_column, "order column")
    ordering <- order(value[[actual_column]], na.last = TRUE,
                      decreasing = descending)
    value <- value[ordering, , drop = FALSE]
  }

  limit_number <- suppressWarnings(as.numeric(limit))
  if (length(limit_number) != 1L || is.na(limit_number) ||
      !is.finite(limit_number) || limit_number != floor(limit_number) ||
      limit_number < 1L) {
    stop("limit must be one positive integer.", call. = FALSE)
  }
  limit_number <- min(as.integer(limit_number), 5000L)
  value <- utils::head(value, limit_number)

  if (!is.null(columns)) {
    if (!is.character(columns) || length(columns) == 0L || anyNA(columns) ||
        any(!nzchar(columns))) {
      stop("columns must be a non-empty character vector.", call. = FALSE)
    }
    selected <- vapply(columns, match_column, character(1L), label = "column")
    value <- value[, unique(selected), drop = FALSE]
  }
  rownames(value) <- NULL
  value
}

.ohdsi_shape_result <- function(result, columns, filters, order_by, limit) {
  if (!inherits(result, "dsomop_result")) {
    stop("OHDSI analysis execution did not return a dsomop_result.",
         call. = FALSE)
  }
  result[["per_site"]] <- lapply(
    result[["per_site"]], .ohdsi_shape_frame,
    columns = columns, filters = filters, order_by = order_by, limit = limit
  )
  result["pooled"] <- list(.ohdsi_shape_frame(
    result[["pooled"]], columns, filters, order_by, limit
  ))
  result
}

#' Check OHDSI result tool availability
#'
#' Queries each connected server to determine which OHDSI tool result tables
#' (CohortDiagnostics, CohortIncidence, Characterization, and others) are present.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. Status is site-specific, so a combined view has no value
#'   and records that reason.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#' @examples
#' \dontrun{
#' status <- ds.omop.ohdsi.status()
#' status$per_site[["server_a"]]$cohort_diagnostics$available
#' }
#' @export
ds.omop.ohdsi.status <- function(symbol = "omop", conns = NULL,
                                 type = NULL) {
  result_type <- .resolve_result_type(type, default_type = "split")
  code <- .build_code("ds.omop.ohdsi.status", symbol = symbol,
                      type = if (is.null(type)) NULL else result_type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopOhdsiStatusDS", session$res_symbol)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site")), result_type,
    "OHDSI result-table availability is site-specific and cannot be combined.")
}

#' List discovered OHDSI result tables
#'
#' Returns a catalog of all OHDSI result tables found across connected servers,
#' including tool identification and row counts.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. The historical default is \code{"both"}.
#' @return A \code{dsomop_result} object. Pooled is the union of catalogs.
#' @examples
#' \dontrun{
#' tables <- ds.omop.ohdsi.tables()
#' tables$pooled
#' }
#' @export
ds.omop.ohdsi.tables <- function(symbol = "omop", conns = NULL,
                                 type = NULL) {
  result_type <- .resolve_result_type(type, default_type = "both")
  code <- .build_code("ds.omop.ohdsi.tables", symbol = symbol,
                      type = if (is.null(type)) NULL else result_type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopOhdsiTablesDS", session$res_symbol)
  )

  aggregate_errors <- attr(raw, "ds_errors") %||% list()
  missing_servers <- setdiff(names(conns), names(raw))
  unavailable <- unique(c(names(aggregate_errors), missing_servers))
  warnings <- character(0)

  pooled <- NULL
  if (.result_type_wants_combine(result_type)) {
    # Pool only a complete federation. A partial union looks authoritative
    # while silently omitting an unavailable server's result tables.
    all_dfs <- list()
    for (srv in names(raw)) {
      df <- raw[[srv]]
      if (is.data.frame(df) && nrow(df) > 0) {
        df$.server <- srv
        all_dfs[[srv]] <- df
      }
    }
    pooled <- if (length(unavailable) > 0L) {
      warnings <- paste0(
        "Federated OHDSI table catalog unavailable: incomplete federation; ",
        "unavailable server(s): ", paste(unavailable, collapse = ", "), "."
      )
      NULL
    } else if (length(all_dfs) > 0L) {
      result <- do.call(rbind, all_dfs)
      rownames(result) <- NULL
      result
    } else NULL
  }

  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = list(call_code = code, scope = "pooled", warnings = warnings)),
    result_type,
    "No complete federated OHDSI table catalog was available.")
}

#' Query an OHDSI result table
#'
#' Reads rows from a pre-computed OHDSI result table with server-controlled
#' disclosure. Split-only requests preserve that compatibility endpoint.
#' Combined requests obtain the physical result table's closed, server-owned
#' pooling contract and combine complete disclosure-controlled frames through
#' that contract; they never route through a same-named live analysis or a
#' column-name pooling heuristic. Reviewed equality filters are applied
#' server-side; selection, ordering, and the row limit are applied only after
#' contractual pooling.
#'
#' @param table_name Character; which result table to query.
#' @param columns Character vector; columns to select (NULL = all safe columns).
#' @param filters Named list; WHERE conditions.
#' @param order_by Character; ORDER BY column.
#' @param limit Integer; max rows (capped at 5000 server-side).
#' @param tool_id Character; optional tool identifier.
#' @param scope Character; \code{"per_site"} or \code{"pooled"}.
#' @param pooling_policy Character; \code{"strict"} (default) or
#'   \code{"pooled_only_ok"}.
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. When omitted, legacy \code{scope} behaviour is preserved.
#' @return A \code{dsomop_result} object.
#' @examples
#' \dontrun{
#' # Cohort diagnostics results per site
#' cd <- ds.omop.ohdsi.results("index_event_breakdown")
#'
#' # Cohort counts pooled across servers
#' cc <- ds.omop.ohdsi.results("cohort_count", scope = "pooled")
#' }
#' @export
ds.omop.ohdsi.results <- function(table_name, columns = NULL,
                                    filters = NULL, order_by = NULL,
                                    limit = 5000L, tool_id = NULL,
                                    scope = c("per_site", "pooled"),
                                    pooling_policy = "strict",
                                    symbol = "omop", conns = NULL,
                                    type = NULL) {
  scope_missing <- missing(scope)
  scope <- match.arg(scope)
  pooling_policy <- match.arg(pooling_policy,
                              c("strict", "pooled_only_ok"))
  result_type <- .resolve_result_type(
    type, scope = scope, scope_missing = scope_missing
  )

  code <- .build_code("ds.omop.ohdsi.results",
    table_name = table_name,
    scope = if (is.null(type)) scope else NULL,
    type = if (is.null(type)) NULL else result_type, symbol = symbol)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  if (.result_type_wants_combine(result_type)) {
    contract_raw <- .ds_safe_aggregate(
      conns,
      expr = call("omopOhdsiResultContractDS", session$res_symbol,
                  table_name, tool_id)
    )
    contract_meta <- .analysis_consistent_metadata(
      contract_raw, names(conns),
      paste0("physical OHDSI result contract for '", table_name, "'")
    )
    required_meta <- c("contract_version", "tool_id", "table_name",
                       "pooling_contract")
    if (!is.list(contract_meta) ||
        length(names(contract_meta)) != length(required_meta) ||
        anyNA(names(contract_meta)) || any(!nzchar(names(contract_meta))) ||
        anyDuplicated(names(contract_meta)) ||
        !setequal(names(contract_meta), required_meta) ||
        !is.list(contract_meta$pooling_contract)) {
      stop("The server returned an invalid physical OHDSI result contract.",
           call. = FALSE)
    }
    version <- suppressWarnings(as.numeric(contract_meta$contract_version))
    if (length(version) != 1L || is.na(version) || !is.finite(version) ||
        version != 1 || !is.character(contract_meta$tool_id) ||
        length(contract_meta$tool_id) != 1L || is.na(contract_meta$tool_id) ||
        !nzchar(contract_meta$tool_id) ||
        !is.character(contract_meta$table_name) ||
        length(contract_meta$table_name) != 1L ||
        is.na(contract_meta$table_name) || !nzchar(contract_meta$table_name)) {
      stop("The server returned an invalid physical OHDSI result contract.",
           call. = FALSE)
    }
    .validate_analysis_pooling_contract(contract_meta$pooling_contract)

    # Fetch the complete reviewed release schema. Equality filters are safe to
    # push down because the server allowlists their dimensions. Projection,
    # ordering, and limits stay post-pool: applying them per site could discard
    # sufficient statistics or select different groups.
    raw <- .ds_safe_aggregate(
      conns,
      expr = call("omopOhdsiResultsDS", session$res_symbol,
                  table_name, NULL, .ds_encode(filters), NULL, 5000L, tool_id)
    )
    raw <- .analysis_complete_results(
      raw, names(conns), paste0("physical OHDSI result '", table_name, "'")
    )
    capped <- names(raw)[vapply(raw, function(frame) {
      is.data.frame(frame) && nrow(frame) >= 5000L
    }, logical(1L))]
    if (length(capped) > 0L) {
      stop("Physical OHDSI result reached the 5000-row server cap on: ",
           paste(capped, collapse = ", "),
           ". Narrow filters before combining so pooling is complete.",
           call. = FALSE)
    }
    harmonization <- .session_harmonization_for_connections(session, conns)
    pool_out <- .pool_analysis_contract(
      raw, contract_meta$pooling_contract, policy = pooling_policy,
      harmonization = harmonization
    )
    if (length(raw) > 1L) {
      pool_out$warnings <- unique(c(
        pool_out$warnings,
        "Physical OHDSI results are combined by their contracted study/cohort identifiers; those definitions must be deployed identically across servers."
      ))
    }
    result <- dsomop_result(
      per_site = raw, pooled = pool_out$result,
      meta = list(call_code = code, scope = "pooled", type = result_type,
                  servers = names(raw), pooling_policy = pooling_policy,
                  warnings = pool_out$warnings)
    )
    result <- .result_type_view(
      result, result_type,
      if (identical(contract_meta$pooling_contract$strategy,
                    "not_poolable")) contract_meta$pooling_contract$reason
    )
    result <- .ohdsi_shape_result(
      result, columns = columns, filters = NULL,
      order_by = order_by, limit = limit
    )
    result[["meta"]]$tool_id <- contract_meta$tool_id
    result[["meta"]]$table_name <- contract_meta$table_name
    result[["meta"]]$contract_version <- as.integer(version)
    return(result)
  }

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopOhdsiResultsDS", session$res_symbol,
                table_name, columns, .ds_encode(filters), order_by,
                as.integer(limit), tool_id)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = scope,
                pooling_policy = pooling_policy)), result_type)
}

#' Get OHDSI tool summary
#'
#' Returns a summary of available result tables for a specific OHDSI tool.
#'
#' @param tool_id Character; which tool to summarize (e.g.,
#'   \code{"cohort_diagnostics"}, \code{"cohort_incidence"},
#'   \code{"characterization"}).
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s). If \code{NULL}, uses active session.
#' @param type Optional result view: \code{"split"}, \code{"combine"}, or
#'   \code{"both"}. Tool summaries are site-specific, so a combined view has
#'   no value and records that reason.
#' @return A \code{dsomop_result} object with \code{scope = "per_site"}.
#' @examples
#' \dontrun{
#' cd_summary <- ds.omop.ohdsi.summary("cohort_diagnostics")
#' cd_summary$per_site
#' }
#' @export
ds.omop.ohdsi.summary <- function(tool_id, symbol = "omop", conns = NULL,
                                  type = NULL) {
  result_type <- .resolve_result_type(type, default_type = "split")
  code <- .build_code("ds.omop.ohdsi.summary",
    tool_id = tool_id, symbol = symbol,
    type = if (is.null(type)) NULL else result_type)

  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  raw <- .ds_safe_aggregate(
    conns,
    expr = call("omopOhdsiSummaryDS", session$res_symbol, tool_id)
  )

  .result_type_view(dsomop_result(
    per_site = raw, pooled = NULL,
    meta = list(call_code = code, scope = "per_site")), result_type,
    "OHDSI tool summaries are site-specific and cannot be combined.")
}
