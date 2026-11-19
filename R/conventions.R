# Module: dsBaseClient conventions
# Shared handling of the arguments every server-calling wrapper accepts:
# connections (`datasources`), result views (`type`), the pooling policy, and
# the legacy argument names kept as aliases.

#' Result views, connections and pooling in dsOMOPClient
#'
#' @description
#' dsOMOPClient follows the dsBaseClient conventions for every function that
#' contacts the servers.
#'
#' @section Connections:
#' \code{datasources} names the DataSHIELD connections to use. \code{NULL}
#' (the default) uses the connections stored in the OMOP session of
#' \code{symbol} when one exists, and otherwise
#' \code{DSI::datashield.connections_find()}. The older name \code{conns}
#' keeps working (with a one-time message); giving both with different
#' connections is an error.
#'
#' @section Result views (\code{type}):
#' \itemize{
#'   \item \code{"split"}: the per-server results only (\code{$per_site}).
#'   \item \code{"combine"}: the pooled result only (\code{$pooled}). When the
#'     result cannot be pooled, the call stops with the reason, before
#'     contacting the servers when the reason is known in advance.
#'   \item \code{"both"}: both. When pooling is impossible the per-server
#'     results are kept, \code{$pooled} is \code{NULL} and
#'     \code{meta$pooling_reason} says why.
#' }
#' Count and table readers (\code{\link{ds.omop.table.stats}},
#' \code{\link{ds.omop.domain.coverage}}, \code{\link{ds.omop.date.counts}},
#' \code{\link{ds.omop.value.counts}}, \code{\link{ds.omop.crosstab}},
#' \code{\link{ds.omop.concept.locate}}, \code{\link{ds.omop.achilles.results}})
#' default to \code{"both"}, as \code{ds.dim}, \code{ds.length} and
#' \code{ds.table} do. Readers of means and distributions default to
#' \code{"split"}, as \code{ds.mean} and \code{ds.var} do. Aliases:
#' \code{"s"}, \code{"splits"}, \code{"per_site"} (split); \code{"c"},
#' \code{"combined"} (combine); \code{"b"}, \code{"pooled"} (both). The older
#' \code{scope} (\code{"per_site"}, \code{"pooled"}) and \code{pool}
#' (\code{FALSE}, \code{TRUE}) arguments keep working as aliases of
#' \code{"split"} and \code{"both"}.
#'
#' Every pooled result records how it was pooled in \code{meta$pooling}
#' (\code{method} and \code{estimand}). Pooled counts are sums of the servers'
#' banded counts: a person present in several databases counts once per
#' database.
#'
#' @section Pooling policy:
#' \code{pooling_policy = "strict"} (the default) releases a pooled value only
#' when every server returned it: a group (value, concept, period, bin, cell)
#' absent or suppressed on any server is left out or \code{NA}, and an
#' unavailable server leaves the whole pooled result \code{NULL}.
#' \code{"pooled_only_ok"} pools the servers that did return a
#' disclosure-safe value, so a pooled value may cover only some servers; the
#' warnings name what was left out. Suppressed contributions are never treated
#' as zero under either policy.
#'
#' @section Output names:
#' Functions that create one server-side object accept \code{newobj} for its
#' name, as dsBaseClient assigning functions do.
#'
#' @name dsomop_conventions
#' @aliases dsomop_result_views
NULL

# Remember which legacy argument names have already been announced, so each
# alias message appears once per R session.
.dsomop_alias_notes <- new.env(parent = emptyenv())

.alias_note <- function(fn, old, new) {
  if (isTRUE(.dsomop_alias_notes$.quiet)) return(invisible(FALSE))
  key <- paste(old, new, sep = "\r")
  if (isTRUE(.dsomop_alias_notes[[key]])) return(invisible(FALSE))
  assign(key, TRUE, envir = .dsomop_alias_notes)
  values <- switch(old,
    scope = " (\"per_site\" is type = \"split\", \"pooled\" is type = \"both\")",
    pool = " (TRUE is type = \"both\", FALSE is type = \"split\")",
    "")
  message(fn, "(): the argument '", old, "' is now called '", new, "'",
          values, "; '", old, "' keeps working.")
  invisible(TRUE)
}

# Internal calls between wrappers may still pass the old argument names (for
# instance `conns`); they must not announce a rename the user never used.
.quiet_aliases <- function(expr) {
  previous <- .dsomop_alias_notes$.quiet
  assign(".quiet", TRUE, envir = .dsomop_alias_notes)
  on.exit(assign(".quiet", isTRUE(previous), envir = .dsomop_alias_notes),
          add = TRUE)
  force(expr)
}

# Resolve a renamed argument. `old_given` is TRUE when the caller supplied the
# old name; `new_given` when the caller supplied the new one. Both with
# different values is an error.
.alias_value <- function(new_value, old_value, new_given, old_given,
                         new, old, fn) {
  if (!isTRUE(old_given)) return(new_value)
  if (isTRUE(new_given) && !identical(new_value, old_value)) {
    stop(fn, "(): '", new, "' and its old name '", old, "' were both ",
         "given, with different values. Use '", new, "' only.",
         call. = FALSE)
  }
  .alias_note(fn, old, new)
  old_value
}

# The connections a caller asked for: `datasources`, or its old name `conns`.
# Returns NULL when neither was given (resolution happens later, where the
# session is known).
.datasources_arg <- function(datasources = NULL, conns = NULL,
                             fn = "dsOMOPClient") {
  if (is.null(conns)) return(datasources)
  if (!is.null(datasources) && !identical(datasources, conns)) {
    stop(fn, "(): 'datasources' and its old name 'conns' were both given, ",
         "with different connections. Use 'datasources' only.",
         call. = FALSE)
  }
  .alias_note(fn, "conns", "datasources")
  conns
}

# NULL connections mean: the OMOP session's stored connections when a session
# exists for `symbol`, otherwise every DataSHIELD connection found by DSI.
.resolve_datasources <- function(datasources = NULL, symbol = "omop",
                                 session = NULL) {
  if (!is.null(datasources)) return(datasources)
  if (is.null(session) && is.character(symbol) && length(symbol) == 1L &&
      !is.na(symbol)) {
    session <- tryCatch(.get_session(symbol), error = function(e) NULL)
  }
  if (!is.null(session)) return(session$conns)
  DSI::datashield.connections_find()
}

# Both steps at once, for wrappers that need the final connections.
.connections <- function(datasources = NULL, conns = NULL, symbol = "omop",
                         session = NULL, fn = "dsOMOPClient") {
  .resolve_datasources(.datasources_arg(datasources, conns, fn),
                       symbol = symbol, session = session)
}

# One validated pooling policy; `policy` is the old name used by
# ds.omop.query.pool().
.pooling_policy_arg <- function(pooling_policy = "strict") {
  match.arg(pooling_policy, c("strict", "pooled_only_ok"))
}

# "combine" can never be satisfied for outputs that are site-specific by
# design: stop before contacting the servers.
.combine_unavailable <- function(type, reason) {
  if (identical(.normalize_result_type(type), "combine")) {
    stop(reason, " Use type = \"split\" or \"both\".", call. = FALSE)
  }
  invisible(NULL)
}

# How each pooled result is formed (meta$pooling).
.pooling_methods <- list(
  table_stats = c(
    method = "sum of banded counts",
    estimand = paste0("rows and persons summed over the servers; date_range ",
                      "spans the servers' supported months")),
  column_stats = c(
    method = "weighted mean and pooled SD",
    estimand = paste0("mean weighted by each server's contributing persons; ",
                      "SD from within- and between-server sums of squares; ",
                      "counts summed")),
  domain_coverage = c(
    method = "sum of banded counts",
    estimand = "persons (and records) per table summed over the servers"),
  missingness = c(
    method = "ratio of summed banded counts",
    estimand = "missing rate = sum of n_missing / sum of n_total per column"),
  value_counts = c(
    method = "sum of banded counts",
    estimand = "every count column summed per value"),
  concept_prevalence = c(
    method = "sum of banded counts",
    estimand = paste0("n_persons and n_records summed per concept, ranked by ",
                      "the requested metric and cut at top_n")),
  histogram = c(
    method = "sum of banded counts",
    estimand = paste0("counts summed per bin on the shared edges, bins ",
                      "matched by bin_start and bin_end")),
  date_counts = c(
    method = "sum of banded counts",
    estimand = "records and persons summed per calendar period"),
  concept_drilldown = c(
    method = "sum of banded counts",
    estimand = paste0("summary counts, histogram bins, categorical values and ",
                      "yearly counts summed; date range spanned; quantiles ",
                      "and non-additive parts not pooled")),
  concept_locate = c(
    method = "sum of banded counts",
    estimand = "records and persons summed per concept, table and column"),
  achilles_results = c(
    method = "sum of banded counts",
    estimand = "count_value summed per analysis and strata"),
  achilles_distribution = c(
    method = "weighted mean and pooled SD",
    estimand = paste0("count-weighted average and pooled SD; percentiles ",
                      "not poolable")),
  crosstab = c(
    method = "sum of banded counts",
    estimand = "cell-wise sums; a cell suppressed or absent anywhere stays NA"),
  metadata_union = c(
    method = "union of rows",
    estimand = "de-duplicated union of the servers' reference metadata"),
  metadata_consensus = c(
    method = "consensus",
    estimand = "the metadata every server returned identically"),
  common_tables = c(
    method = "intersection",
    estimand = "the tables present on every server"),
  common_columns = c(
    method = "intersection",
    estimand = "the columns present on every server"),
  federation_contract = c(
    method = "negotiated federation contract",
    estimand = paste0("the settings every server can honour together (the ",
                      "contract used for federated analyses)")),
  table_union = c(
    method = "union of rows",
    estimand = "the servers' catalog rows stacked, with a .server column"),
  contract_tabular = c(
    method = "server-owned pooling contract",
    estimand = paste0("columns pooled by their contracted roles (sums, ",
                      "ratios of sums, weighted means, pooled SDs, min/max)")),
  contract_effect_estimate = c(
    method = "inverse-variance meta-analysis",
    estimand = paste0("fixed-effect and DerSimonian-Laird random-effects ",
                      "pooled estimates of the per-site log estimates")),
  contract_kaplan_meier = c(
    method = "pooled risk sets",
    estimand = paste0("Kaplan-Meier survival recomputed from summed numbers ",
                      "at risk and events per time bin")),
  protected_components = c(
    method = "sum of DP components",
    estimand = paste0("signed protected components summed before display ",
                      "projection; effects meta-analysed from the per-site ",
                      "protected estimates"))
)

.pooling_meta <- function(kind) {
  entry <- .pooling_methods[[kind]]
  if (is.null(entry)) return(NULL)
  list(method = unname(entry[["method"]]),
       estimand = unname(entry[["estimand"]]))
}

# The pooling descriptor of a server-owned analysis contract.
.pooling_meta_for_contract <- function(contract) {
  strategy <- if (is.list(contract)) contract$strategy else NULL
  if (identical(strategy, "protected_components")) {
    return(.pooling_meta("protected_components"))
  }
  switch(strategy %||% "",
    tabular = .pooling_meta("contract_tabular"),
    effect_estimate = .pooling_meta("contract_effect_estimate"),
    kaplan_meier = .pooling_meta("contract_kaplan_meier"),
    NULL)
}

# Stack per-site data frames into one frame with a leading `server` column.
# Columns missing from a site are filled with NA; non-frame entries are left
# out.
.stack_per_site <- function(per_site) {
  frames <- Filter(is.data.frame, per_site)
  if (length(frames) == 0L) return(data.frame())
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  columns <- setdiff(columns, "server")
  rows <- lapply(names(frames), function(server) {
    frame <- frames[[server]]
    if ("server" %in% names(frame)) frame$server <- NULL
    for (column in setdiff(columns, names(frame))) {
      frame[[column]] <- rep(NA, nrow(frame))
    }
    frame <- frame[, columns, drop = FALSE]
    cbind(data.frame(server = rep(server, nrow(frame)),
                     stringsAsFactors = FALSE), frame)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# The standard tail of an aggregate reader: pool when the view wants it,
# record the pooling descriptor, and apply the view (see .result_type_view for
# the rule when pooling is impossible).
.reader_result <- function(raw, kind, result_type, pooling_policy, code,
                           reason, options = list(), warnings = character(0),
                           meta = list()) {
  pooled <- NULL
  if (.result_type_wants_combine(result_type) && length(raw) > 0L) {
    pool_out <- if (length(options) > 0L) {
      .pool_result(raw, kind, pooling_policy, options = options)
    } else .pool_result(raw, kind, pooling_policy)
    pooled <- pool_out$result
    warnings <- c(warnings, pool_out$warnings)
  }
  .result_type_view(dsomop_result(
    per_site = raw, pooled = pooled,
    meta = c(list(call_code = code, pooling_policy = pooling_policy,
                  warnings = warnings), meta)),
    result_type, reason, pooling = .pooling_meta(kind))
}

# A dry-run result: only the reproducible call.
.reader_dry_run <- function(code, result_type) {
  .result_type_view(dsomop_result(
    per_site = list(), pooled = NULL, meta = list(call_code = code)),
    result_type)
}

# Call code records non-default arguments: the default pooling policy is
# omitted, any other is written out.
.code_policy <- function(pooling_policy) {
  if (identical(pooling_policy, "strict")) NULL else pooling_policy
}
