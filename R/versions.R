# Module: Versions and server readiness
# The client and server versions, what each server's DataSHIELD profile
# publishes, and readable reasons when a server is not ready for dsOMOP.

# The dsOMOP server methods this client calls, by DataSHIELD type. A test keeps
# this list equal to the methods named in the client's code. Plan execution
# requires the publication status and revocation methods; ds.omop.mutate()
# also requires omopMutateDS (dsOMOP 2.10.4).
# Methods under `optional` are called only on servers that advertise them
# (omopPlanExplainDS, for a language-2 plan's server explain); a server without
# them is still ready for the methods above.
.DSOMOP_SERVER_METHODS <- list(
  aggregate = c(
    "omopPingDS", "omopGetCapabilitiesDS", "omopDisclosureSettingsDS",
    "omopListTablesDS", "omopListColumnsDS", "omopRelationshipGraphDS",
    "omopTableStatsDS", "omopColumnStatsDS", "omopDomainCoverageDS",
    "omopMissingnessDS", "omopValueCountsDS", "omopSearchConceptsDS",
    "omopLookupConceptsDS", "omopGetDescendantsDS",
    "omopExpandConceptSetDS", "omopConceptAncestorsDS",
    "omopConceptSynonymsDS", "omopConceptRelationshipsDS",
    "omopListConceptsDS", "omopVocabulariesDS", "omopDomainsDS",
    "omopConceptClassesDS", "omopCdmSourceDS", "omopCdmVersionDS",
    "omopPlanPreviewDS", "omopPlanStatusDS", "omopStagedRevokeDS",
    "omopCohortListDS",
    "omopCohortGetDefinitionDS", "omopConceptPrevalenceDS",
    "omopCrossTabDS", "omopNumericRangeDS", "omopNumericHistogramDS",
    "omopNumericQuantilesDS", "omopDateCountsDS",
    "omopConceptDrilldownDS", "omopLocateConceptDS",
    "omopSafeCutpointsDS", "omopAchillesStatusDS",
    "omopAchillesAnalysesDS", "omopAchillesResultsDS",
    "omopAchillesDistributionDS", "omopAchillesCatalogDS",
    "omopOhdsiStatusDS", "omopOhdsiTablesDS", "omopOhdsiResultsDS",
    "omopOhdsiResultContractDS", "omopOhdsiSummaryDS",
    "omopAnalysisListDS", "omopAnalysisGetDS", "omopAnalysisRunDS",
    "omopQueryLibraryStickyCatalogDS", "omopDpStatusDS",
    "omopDpReleaseDS", "omopFactorLevelsDS"
  ),
  assign = c(
    "omopInitDS", "omopPlanExecuteDS", "omopCohortCreateDS",
    "omopCohortCombineDS", "omopCohortFromTableDS", "omopCleanupDS",
    "omopAsFactorColumnsDS", "omopMergeDS", "omopFilterDS",
    "omopSelectDS", "omopBindRowsDS", "omopMutateDS", "omopAnalysisRunAssignDS",
    "omopDpPlanExecuteDS"
  ),
  optional = c("omopPlanExplainDS")
)

# The dsOMOP release whose methods this client's execution protocol needs
# (publication status, revocation and commit_policy, design section 7.3).
.DSOMOP_PROTOCOL_RELEASE <- "2.10.4"

# What an administrator does when the profile lacks dsOMOP's methods.
.DSOMOP_PUBLISH_ADVICE <- paste0(
  "An Opal administrator installs dsOMOP ", .DSOMOP_PROTOCOL_RELEASE,
  " and publishes its settings in the profile the analysts use ",
  "(Administration > DataSHIELD > Packages > dsOMOP > Publish); Opal does not ",
  "publish the methods of an upgraded package by itself."
)

# The first dsOMOP release with the security fixes of the OHDSI community
# tools (protected releases, support and window fixes).
.DSOMOP_PATCHED_SERVER_VERSION <- "2.10.3"

#' Plain warnings for servers older than dsOMOP 2.10.3
#'
#' Such a server lacks the dsOMOP 2.10.3 security fixes of its OHDSI community
#' tools (analysis catalogue, Achilles and OHDSI readers, DP releases); a
#' newer client cannot repair a server, and never claims to.
#'
#' @param versions Named character vector of server dsOMOP versions (NA when
#'   unknown).
#' @return Character vector of warnings, one per server older than 2.10.3.
#' @keywords internal
.dsomop_unpatched_server_warnings <- function(versions) {
  old <- names(versions)[vapply(versions, function(v) {
    !is.na(v) && nzchar(v) && !inherits(try(numeric_version(v), silent = TRUE),
                                        "try-error") &&
      numeric_version(v) < numeric_version(.DSOMOP_PATCHED_SERVER_VERSION)
  }, logical(1L))]
  vapply(old, function(server) {
    paste0(
      "Server '", server, "' runs dsOMOP ", versions[[server]], ", older than ",
      .DSOMOP_PATCHED_SERVER_VERSION, ": its OHDSI community-tool results ",
      "(analysis catalogue, Achilles and OHDSI readers, DP releases) lack the ",
      "dsOMOP ", .DSOMOP_PATCHED_SERVER_VERSION, " security fixes, and this ",
      "dsOMOPClient cannot repair them. They are reported under that server's ",
      "own legacy contract; upgrade the server to dsOMOP ",
      .DSOMOP_PATCHED_SERVER_VERSION, ".")
  }, character(1L), USE.NAMES = FALSE)
}

#' dsOMOPClient version
#'
#' @return Character; the installed dsOMOPClient version.
#' @examples
#' ds.omop.client.version()
#' @export
ds.omop.client.version <- function() {
  as.character(utils::packageVersion("dsOMOPClient"))
}

#' dsOMOP version on each server
#'
#' Works on plain DataSHIELD connections, before (or without)
#' \code{\link{ds.omop.connect}}. For every server it reports the dsOMOP
#' version whose DataSHIELD methods the profile publishes, the version that
#' answers a call (the one installed), and the methods this client calls that
#' the profile does not publish. A server is ready when the two versions agree
#' and nothing is missing; otherwise \code{advice} says what to change.
#'
#' Opal publishes a package's methods when it is first installed, but not when
#' an installed package is upgraded: after an upgrade the profile keeps the old
#' list until an administrator publishes the package's settings again.
#'
#' A server whose profile lacks the publication protocol
#' (\code{omopPlanStatusDS}, \code{omopStagedRevokeDS}) runs v1 plans only
#' through the legacy call and refuses language-2 plans; the advice says
#' whether the installed dsOMOP predates the protocol (install the release
#' that has it, then publish) or its upgraded methods are not published yet
#' (publish again). \code{ready} concerns the published methods only: whether
#' a server executes a language-2 plan is its \code{recipe_language} block.
#'
#' @param conns DSI connections (default: the DSI default connections).
#' @param symbol OMOP session symbol whose capability blocks (read by
#'   \code{\link{ds.omop.connect}}) are reported.
#' @return A data frame with one row per server: \code{server},
#'   \code{published} (version of the published dsOMOP methods, NA when none),
#'   \code{running} (version answering \code{omopPingDS}, NA when it cannot be
#'   called), \code{missing} (methods this client calls that the profile lacks),
#'   \code{ready}, \code{advice} and the list column \code{recipe_language}:
#'   per server the recipe-language capability block of its OMOP session
#'   (\code{version}, \code{semantic_version}, \code{operations},
#'   \code{reducers}, \code{dialect}), independently of method readiness. The
#'   string \code{"unavailable"} means the connected site supplied no block,
#'   or an unconnected site's profile lacks the publication protocol;
#'   \code{"not connected"} means no OMOP session \code{symbol} is connected
#'   to a site publishing that protocol. No language capability is inferred
#'   from a package version or from \code{ready}. The client version is in
#'   its \code{"client"} attribute.
#' @examples
#' \dontrun{
#' ds.omop.client.version()
#' ds.omop.server.version(conns)
#' }
#' @export
ds.omop.server.version <- function(conns = NULL, symbol = "omop") {
  conns <- conns %||% DSI::datashield.connections_default()
  if (is.null(conns) || length(conns) == 0L || is.null(names(conns))) {
    stop("No DataSHIELD connections available.", call. = FALSE)
  }
  servers <- names(conns)
  published <- .dsomop_published_methods(conns)
  ping <- .ds_safe_aggregate(conns, call("omopPingDS"))
  ping_errors <- attr(ping, "ds_errors") %||% list()
  client <- ds.omop.client.version()
  session <- tryCatch(.get_session(symbol), error = function(e) NULL)

  rows <- lapply(servers, function(server) {
    pub <- published[[server]]
    running <- tryCatch(as.character(ping[[server]]$version),
                        error = function(e) character(0))
    running <- if (length(running) == 1L && !is.na(running)) running else NA_character_
    missing <- if (is.null(pub$error)) {
      c(setdiff(.DSOMOP_SERVER_METHODS$aggregate, pub$aggregate),
        setdiff(.DSOMOP_SERVER_METHODS$assign, pub$assign))
    } else character(0)
    version <- if (length(pub$version) > 0L) {
      paste(pub$version, collapse = ", ")
    } else NA_character_
    advice <- if (!is.null(pub$error)) {
      paste0("The profile's methods could not be listed: ", pub$error)
    } else if (length(pub$version) == 0L) {
      paste0("The profile publishes no dsOMOP methods. ",
             .DSOMOP_PUBLISH_ADVICE)
    } else if (any(grepl("^1\\.", pub$version))) {
      paste0("The profile publishes the methods of dsOMOP ", version,
             ", which this client cannot use. ", .DSOMOP_PUBLISH_ADVICE)
    } else if (any(.DSOMOP_PROTOCOL_METHODS %in% missing) &&
               !is.na(running) &&
               utils::compareVersion(running, .DSOMOP_PROTOCOL_RELEASE) < 0L) {
      paste0("dsOMOP ", running, " is installed",
             if (identical(version, running)) " and published" else "",
             " but predates the publication protocol (",
             paste(intersect(.DSOMOP_PROTOCOL_METHODS, missing), collapse = ", "),
             "): v1 plans run only through the legacy call and language-2 plans are ",
             "refused. Publishing again cannot add these methods: an administrator ",
             "installs dsOMOP ", .DSOMOP_PROTOCOL_RELEASE, " or later on the server and then ",
             "publishes its settings (Administration > DataSHIELD > Packages > dsOMOP > ",
             "Publish).")
    } else if (!is.na(running) && !identical(version, running)) {
      paste0("dsOMOP ", running, " is installed but the profile publishes ",
             "the methods of ", version, ": an administrator publishes ",
             "dsOMOP's settings again (Administration > DataSHIELD > ",
             "Packages > dsOMOP > Publish).")
    } else if (any(.DSOMOP_PROTOCOL_METHODS %in% missing)) {
      paste0(if (is.na(running)) {
               "The installed dsOMOP version could not be verified. The profile lacks "
             } else paste0("dsOMOP ", running, " is installed but the profile lacks "),
             "publication protocol methods (",
             paste(intersect(.DSOMOP_PROTOCOL_METHODS, missing), collapse = ", "),
             "): v1 plans run only through the legacy call and language-2 plans are ",
             "refused. An administrator ",
             if (is.na(running)) paste0("checks that dsOMOP ", .DSOMOP_PROTOCOL_RELEASE,
                                        " or later is installed, upgrades if needed, then ") else "",
             "publishes dsOMOP's settings again (Administration > DataSHIELD > ",
             "Packages > dsOMOP > Publish).")
    } else if (length(missing) > 0L) {
      paste0("The profile lacks ", length(missing), " dsOMOP method(s) this ",
             "client calls. ", .DSOMOP_PUBLISH_ADVICE)
    } else if (is.na(running)) {
      paste0("omopPingDS did not answer: ",
             .ds_error_core(ping_errors[[server]] %||% "no result"))
    } else if (!identical(.minor_version(running), .minor_version(client))) {
      paste0("dsOMOP ", running, " with dsOMOPClient ", client, ": install ",
             "the same minor version on both sides.")
    } else {
      ""
    }
    row <- data.frame(
      server = server, published = version, running = running,
      missing = paste(missing, collapse = ", "),
      ready = !nzchar(advice), advice = advice,
      stringsAsFactors = FALSE
    )
    old <- is.null(pub$error) && any(.DSOMOP_PROTOCOL_METHODS %in% missing)
    block <- session$capabilities[[server]]$recipe_language
    row$recipe_language <- I(list(
      if (!is.null(block)) block[intersect(c("version", "semantic_version", "operations",
                                                   "reducers", "dialect"), names(block))]
      else if (old || (!is.null(session) && server %in% names(session$conns))) "unavailable"
      else "not connected"))
    row
  })
  out <- do.call(rbind, rows)
  attr(out, "client") <- client
  out
}

#' The dsOMOP methods each server's profile publishes
#' @param conns DSI connections.
#' @return Named list per server: \code{version} (versions recorded with the
#'   published dsOMOP methods), \code{aggregate}, \code{assign}, or
#'   \code{error} when the profile could not be listed.
#' @keywords internal
.dsomop_published_methods <- function(conns) {
  out <- stats::setNames(
    lapply(names(conns), function(server) {
      list(version = character(0), aggregate = character(0),
           assign = character(0))
    }),
    names(conns)
  )
  for (type in c("aggregate", "assign")) {
    methods <- tryCatch(DSI::datashield.methods(conns, type = type),
                        error = identity)
    if (inherits(methods, "error")) {
      for (server in names(out)) {
        out[[server]]$error <- .ds_error_core(conditionMessage(methods))
      }
      next
    }
    if (!is.data.frame(methods) || nrow(methods) == 0L) next
    value <- as.character(methods$value %||% "")
    package <- as.character(methods$package %||% "")
    ours <- (!is.na(package) & package == "dsOMOP") |
      (!is.na(value) & startsWith(value, "dsOMOP::"))
    for (server in intersect(unique(as.character(methods$server)), names(out))) {
      rows <- ours & as.character(methods$server) == server
      out[[server]][[type]] <- unique(c(
        out[[server]][[type]], as.character(methods$name[rows])
      ))
      versions <- as.character(methods$version[rows] %||% character(0))
      versions <- versions[!is.na(versions) & nzchar(versions)]
      out[[server]]$version <- unique(c(out[[server]]$version, versions))
    }
  }
  out
}

#' Major.minor part of a version string
#' @keywords internal
.minor_version <- function(version) {
  sub("^([0-9]+\\.[0-9]+).*$", "\\1", as.character(version))
}

#' The server's own error behind a DataSHIELD failure
#'
#' Opal reports a failed call as "Command '...' failed on 'server': Error
#' while evaluating '...' -> Error : <message>"; this keeps <message>.
#' @param message Character error message.
#' @return Character.
#' @keywords internal
.ds_error_core <- function(message) {
  message <- paste(as.character(message), collapse = " ")
  core <- sub("^.* -> ", "", message)
  core <- sub("^Error( in [^:]*)? ?: ?", "", core)
  trimws(gsub("[[:space:]]+", " ", core))
}

#' What to do about a server failure an administrator can fix
#' @param message A server's error message (see \code{.ds_error_core}).
#' @return Character advice, or NULL when there is none to add.
#' @keywords internal
.ds_failure_hint <- function(message) {
  method <- regmatches(message, regexec(
    "No such DataSHIELD '(ASSIGN|AGGREGATE)' method with name: ([A-Za-z0-9._]+)",
    message
  ))[[1L]]
  if (length(method) == 3L && startsWith(method[[3L]], "omop")) {
    return(paste0(
      "The server's DataSHIELD profile does not publish dsOMOP's ",
      method[[3L]], ". ", .DSOMOP_PUBLISH_ADVICE,
      " ds.omop.server.version() shows what each server publishes."
    ))
  }
  if (grepl("there is no package called .dsOMOP.", message)) {
    return(paste0("dsOMOP is not installed on that server. ",
                  .DSOMOP_PUBLISH_ADVICE))
  }
  if (grepl("No resolver could be found for resource", message, fixed = TRUE)) {
    return(paste0("No installed package can open that resource: check that ",
                  "dsOMOP is installed on the server and the resource's ",
                  "URL and format."))
  }
  NULL
}

#' Per-server reasons for a failed DataSHIELD step
#'
#' The servers' own messages, as reported to DSI's error callback or kept by
#' \code{DSI::datashield.errors()}, instead of DSI's generic "There are some
#' DataSHIELD errors" notice; each followed by what to do when that is known.
#' @param errors Named list or character vector of messages per server.
#' @param condition Optional condition raised by DSI.
#' @param servers Servers that did not succeed.
#' @return Character; one line per server.
#' @keywords internal
.ds_failure_detail <- function(errors = list(), condition = NULL,
                               servers = character(0)) {
  errors <- as.list(errors)
  unexplained <- setdiff(servers, names(errors))
  if (length(unexplained) > 0L) {
    recorded <- tryCatch(suppressMessages(DSI::datashield.errors()),
                         error = function(e) NULL)
    for (server in intersect(unexplained, names(recorded))) {
      errors[[server]] <- recorded[[server]]
    }
  }
  if (length(errors) == 0L) {
    return(if (!is.null(condition)) {
      conditionMessage(condition)
    } else {
      paste(servers, collapse = ", ")
    })
  }
  lines <- vapply(names(errors), function(server) {
    core <- .ds_error_core(errors[[server]])
    hint <- .ds_failure_hint(core)
    paste0(server, ": ", core, if (!is.null(hint)) paste0(" ", hint) else "")
  }, character(1))
  paste(lines, collapse = "\n")
}
