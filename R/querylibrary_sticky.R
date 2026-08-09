# Module: executable sticky redesigns of OHDSI QueryLibrary questions
#
# The upstream SQL is evidence only. These mappings prepare a fixed
# omop_privacy specification for use on a person-local omop.table created by a
# typed Recipe/Plan. No arbitrary SQL or data-dependent domain is accepted.

.QUERYLIBRARY_STICKY_COMMIT <-
  "df8a21074b08519e581ca1afb7510468538117a4"
.QUERYLIBRARY_STICKY_REPOSITORY <- "https://github.com/OHDSI/QueryLibrary"

.querylibrary_sticky_metadata <- function() {
  vocabulary_ids <- c(
    "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09",
    "C10", "C11", "D01", "D02", "D03", "D04", "D05", "D06", "D07",
    "D08", "D09", "D10", "D11", "D14", "D15", "D16", "D17", "D18",
    "D19", "D20", "D22", "D24", "D25", "D26", "D27", "DEX12", "G01",
    "G02", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11",
    "G12", "G13", "G14", "G15", "G16", "G17", "O01", "P02"
  )
  c(
    CS01 = paste0(
      "institutional care_site aggregate; use a separately reviewed gated ",
      "metadata/aggregate path, not the sticky DP redesign path"
    ),
    stats::setNames(
      rep.int(
        paste0(
          "vocabulary/reference question; use reviewed dsOMOP vocabulary ",
          "tools, not the sticky DP redesign path"
        ),
        length(vocabulary_ids)
      ),
      vocabulary_ids
    )
  )
}

.querylibrary_sticky_groups <- function() {
  list(
    list(
      family = "distinct_person_count", statistic = "count",
      reducer = "any", contribution_mode = "one_boolean_per_person",
      ids = c(
        "CO11", "DER09", "DEX21", "DEX22", "OP01", "OP03", "OP04",
        "PE02"
      )
    ),
    list(
      family = "fixed_domain_person_histogram",
      statistic = "categorical_histogram", reducer = "presence",
      contribution_mode = "deduplicated_person_category_pairs",
      ids = c(
        "CS02", "CE06", "CE07", "CE10", "CO24", "COC11",
        "DER26", "DEX01", "DEX04", "DEX10", "DEX19", "DEX40", "DEX42",
        "OP16", "PE03", "PE07", "PE11"
      )
    ),
    list(
      family = "fixed_domain_person_histogram",
      statistic = "categorical_histogram", reducer = "first",
      contribution_mode = "one_chronologically_first_category_per_person",
      order_by_required = TRUE, ids = "COC01"
    ),
    list(
      family = "bounded_binary_rate", statistic = "binary_rate",
      reducer = "any",
      contribution_mode =
        "one_denominator_and_one_numerator_boolean_per_person",
      ids = c(
        "COC05", "COC09", "DER05", "DER06", "DEX11", "DEX14", "DEX20"
      )
    ),
    list(
      family = "bounded_person_mean", statistic = "bounded_mean",
      reducer = "mean",
      contribution_mode = "one_clipped_reduced_value_per_person",
      ids = c("CO08", "DRC03", "DER03", "DER07", "DEX16", "OP05", "OP06")
    ),
    list(
      family = "bounded_numeric_histogram",
      statistic = "numeric_histogram", reducer = "mean",
      contribution_mode = "one_binned_reduced_value_per_person",
      ids = c(
        "CE01", "CE02", "CE03", "CE08", "CE13", "CE16", "CE17", "CO01",
        "CO05", "CO07", "CO15", "CO17", "CO21", "CO25", "COC02", "COC06",
        "COC10", "DER13", "DER15", "DER18", "DEX02", "DEX03", "DEX07",
        "DEX08", "DEX09", "DEX15", "DEX23", "DEX24", "DEX29", "DEX31",
        "DEX34", "DEX35", "DEX36", "DEX37", "DRC01", "DRC07", "OP02",
        "OP07", "OP08", "OP12", "OP14", "OP15", "OP19", "OP20", "PE06",
        "PE09", "PE10", "PE12", "PP01"
      )
    ),
    list(
      family = "bounded_numeric_histogram",
      statistic = "numeric_histogram", reducer = "first",
      contribution_mode = "one_chronologically_first_binned_value_per_person",
      ids = c(
        "CE12", "CO02", "CO13", "DER11", "DER23", "DEX27", "DEX41",
        "OP13"
      )
    ),
    list(
      family = "bounded_numeric_histogram",
      statistic = "numeric_histogram", reducer = "last",
      contribution_mode = "one_chronologically_last_binned_value_per_person",
      ids = c("CE11", "CO12", "DER10", "DEX26", "OP11")
    ),
    list(
      family = "bounded_numeric_histogram",
      statistic = "numeric_histogram", reducer = "records",
      contribution_mode = "at_most_k_ordered_records_per_person",
      record_cap_required = TRUE, order_by_required = TRUE,
      ids = c(
        "CE05", "CO09", "CO19", "CO23", "DER17", "DEX33", "OP10",
        "OP17", "OP18"
      )
    ),
    list(
      family = "bounded_record_count", statistic = "bounded_record_count",
      reducer = "records",
      contribution_mode = "at_most_k_records_per_person",
      record_cap_required = TRUE,
      ids = c("CO10", "CO18", "DER08", "DEX25")
    ),
    list(
      family = "fixed_domain_record_histogram",
      statistic = "categorical_histogram", reducer = "records",
      contribution_mode = "at_most_k_ordered_records_per_person",
      record_cap_required = TRUE, order_by_required = TRUE,
      ids = c(
        "CE04", "CE09", "CO03", "CO04", "CO14", "CO22", "DER12",
        "DER21", "DEX05", "DEX13", "DEX18", "DEX28", "DEX39"
      )
    ),
    list(
      family = "bounded_distinct", statistic = "bounded_distinct",
      reducer = "distinct",
      contribution_mode = "at_most_k_distinct_categories_per_person",
      record_cap_required = TRUE, ids = "DEX06"
    )
  )
}

.querylibrary_sticky_unavailable <- function() {
  list(
    metadata_reference = .querylibrary_sticky_metadata(),
    held_back = stats::setNames(character(0), character(0)),
    blocked = c(
      CO20 = "uncontrolled source or free-text labels",
      DEX17 = "uncontrolled source or free-text labels",
      DEX38 = "uncontrolled source or free-text labels",
      PP02 = "uncontrolled source or free-text labels",
      PE08 = "exact ZIP geography",
      CO06 = "single-patient result",
      CO16 = "patient or event rows",
      COC07 = "patient or event rows",
      COC08 = "patient or event rows",
      DER01 = "patient or event rows",
      DER02 = "single-event result",
      DER04 = "single-patient result",
      DER14 = "patient or event rows",
      DER16 = "patient or event rows",
      DEX30 = "patient or event rows",
      DEX32 = "patient or event rows",
      DEX43 = "patient or event rows",
      OP09 = "patient or event rows"
    )
  )
}

.querylibrary_sticky_catalog_data <- function(include_unavailable = FALSE) {
  groups <- .querylibrary_sticky_groups()
  rows <- lapply(groups, function(group) {
    data.frame(
      upstream_id = group$ids,
      status = "executable_redesign",
      family = group$family,
      statistic = group$statistic,
      reducer = group$reducer,
      contribution_mode = group$contribution_mode,
      record_cap_required = identical(group$record_cap_required, TRUE),
      order_by_required = identical(group$order_by_required, TRUE),
      reason = NA_character_,
      literal_upstream_sql_authorized = FALSE,
      source_commit = .QUERYLIBRARY_STICKY_COMMIT,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, rows)
  if (isTRUE(include_unavailable)) {
    unavailable <- .querylibrary_sticky_unavailable()
    available_statuses <- names(unavailable)[vapply(
      unavailable, length, integer(1L)
    ) > 0L]
    extra <- do.call(rbind, lapply(available_statuses, function(status) {
      values <- unavailable[[status]]
      data.frame(
        upstream_id = names(values), status = status,
        family = NA_character_, statistic = NA_character_,
        reducer = NA_character_, contribution_mode = NA_character_,
        record_cap_required = FALSE, order_by_required = FALSE,
        reason = unname(values), literal_upstream_sql_authorized = FALSE,
        source_commit = .QUERYLIBRARY_STICKY_COMMIT,
        stringsAsFactors = FALSE
      )
    }))
    result <- rbind(result, extra)
  }
  result$source_url <- paste0(
    .QUERYLIBRARY_STICKY_REPOSITORY, "/tree/",
    .QUERYLIBRARY_STICKY_COMMIT
  )
  result <- result[order(result$upstream_id, method = "radix"), , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' List audited sticky redesign mappings for OHDSI QueryLibrary
#'
#' Lists semantic redesigns of questions in the pinned OHDSI QueryLibrary
#' snapshot. These are not ports of the upstream SQL. An executable mapping
#' fixes one of the seven person-bounded sticky primitives and a longitudinal
#' reducer for a protected table prepared through \code{\link{omop_recipe}} or
#' \code{\link{ds.omop.plan}}.
#' With \code{include_unavailable = TRUE}, the returned 201-ID partition also
#' records vocabulary/reference questions and blocked result shapes. Those rows
#' are not sticky-DP mappings and never authorize literal upstream SQL.
#'
#' The catalog is not an arbitrary SQL or join gateway. It exposes no analyst
#' control over epsilon, seed, nonce, epoch, or rerolls. The server-owned fixed
#' per-release sticky contract governs every actual release.
#'
#' @param include_unavailable Include vocabulary/reference metadata,
#'   explicitly held-back, and blocked upstream IDs as well as executable
#'   redesigns.
#' @return A data frame with mapping status, primitive family, statistic,
#'   reducer, contribution contract and pinned source commit.
#' @export
omop_querylibrary_sticky_catalog <- function(include_unavailable = FALSE) {
  if (!is.logical(include_unavailable) || length(include_unavailable) != 1L ||
      is.na(include_unavailable)) {
    stop("include_unavailable must be TRUE or FALSE.", call. = FALSE)
  }
  .querylibrary_sticky_catalog_data(include_unavailable)
}

.querylibrary_sticky_unused <- function(values, statistic) {
  used <- names(values)[!vapply(values, is.null, logical(1L))]
  if (length(used) > 0L) {
    stop("`", paste(used, collapse = "`, `"), "` ",
         if (length(used) == 1L) "is" else "are",
         " not valid for the mapped ", statistic, " statistic.",
         call. = FALSE)
  }
}

#' Build an executable sticky redesign of an OHDSI QueryLibrary question
#'
#' Creates a validated workflow specification containing an
#' \code{\link{omop_privacy}} object. The input table is not created by this
#' function: it must be a server-created, person-local \code{omop.table}
#' produced by the typed Recipe/Plan path, with the requested value and ordering
#' columns. All levels, breaks, bounds, positive values and record caps are
#' public analysis choices fixed before data access.
#'
#' The literal upstream SQL is never returned or executed. Exact ZIP,
#' source/free-text labels, and patient/event rows fail closed. Record counts
#' and distinct-concept cardinality are bounded per person and therefore target
#' capped redesign estimands rather than the unbounded upstream estimands. The
#' resulting object delegates release to the one server-owned fixed per-release
#' sticky service, whose status and result metadata state the implemented
#' guarantee.
#'
#' @param upstream_id Published QueryLibrary ID in the pinned catalog.
#' @param variable Value column in the protected prepared table. Not used for a
#'   distinct-person count.
#' @param levels Complete fixed public domain for a categorical histogram.
#' @param breaks Fixed public numeric, ISO-date, or UTC-datetime histogram
#'   breaks.
#' @param lower,upper Fixed finite bounds for a bounded person mean.
#' @param positive Fixed public values defining a positive binary outcome.
#' @param max_contributions Public person-level contribution cap. It is required
#'   for record counts, record histograms, and bounded distinct cardinality;
#'   it defaults to one for a person/category presence histogram.
#' @param order_by Public longitudinal ordering column. It is required for a
#'   record histogram. First/last date mappings use \code{variable} itself when
#'   it is omitted.
#' @param population_id Optional public compatibility label passed to
#'   \code{\link{omop_privacy}}.
#' @param preparation Optional \code{omop_recipe} or \code{omop_plan} documenting
#'   how the protected input will be prepared. It must compile to exactly one
#'   output. Execution remains explicit so the assigned symbol is visible to
#'   the analyst and custodian.
#' @return An \code{omop_querylibrary_sticky} specification. Pass it to
#'   \code{\link{ds.omop.querylibrary.sticky.release}} after preparing its input.
#' @export
omop_querylibrary_sticky <- function(
    upstream_id, variable = NULL, levels = NULL, breaks = NULL,
    lower = NULL, upper = NULL, positive = NULL,
    max_contributions = NULL, order_by = NULL, population_id = NULL,
    preparation = NULL) {
  if (!is.character(upstream_id) || length(upstream_id) != 1L ||
      is.na(upstream_id) || !grepl("^[A-Za-z0-9]+$", upstream_id)) {
    stop("upstream_id must be one published QueryLibrary ID.", call. = FALSE)
  }
  upstream_id <- toupper(upstream_id)
  unavailable <- .querylibrary_sticky_unavailable()
  if (upstream_id %in% names(unavailable$metadata_reference)) {
    stop("QueryLibrary ", upstream_id, " is metadata/reference, not a sticky ",
         "DP redesign: ", unavailable$metadata_reference[[upstream_id]], ".",
         call. = FALSE)
  }
  if (upstream_id %in% names(unavailable$held_back)) {
    stop("QueryLibrary ", upstream_id, " is held back: ",
         unavailable$held_back[[upstream_id]], ".", call. = FALSE)
  }
  if (upstream_id %in% names(unavailable$blocked)) {
    stop("QueryLibrary ", upstream_id, " is blocked: ",
         unavailable$blocked[[upstream_id]], ".", call. = FALSE)
  }
  catalog <- .querylibrary_sticky_catalog_data(FALSE)
  entry <- catalog[catalog$upstream_id == upstream_id, , drop = FALSE]
  if (nrow(entry) != 1L) {
    stop("QueryLibrary ", upstream_id,
         " has no executable sticky redesign in the pinned catalog.",
         call. = FALSE)
  }

  if (!is.null(preparation)) {
    if (inherits(preparation, "omop_recipe")) {
      plan <- recipe_to_plan(preparation)
    } else if (inherits(preparation, "omop_plan")) {
      plan <- preparation
    } else {
      stop("preparation must be an omop_recipe or omop_plan.",
           call. = FALSE)
    }
    if (!is.list(plan$outputs) || length(plan$outputs) != 1L) {
      stop("preparation must compile to exactly one protected output.",
           call. = FALSE)
    }
  }

  statistic <- entry$statistic[[1L]]
  reducer <- entry$reducer[[1L]]
  privacy <- switch(
    statistic,
    count = {
      .querylibrary_sticky_unused(
        list(variable = variable, levels = levels, breaks = breaks,
             lower = lower, upper = upper, positive = positive,
             max_contributions = max_contributions, order_by = order_by),
        statistic
      )
      omop_privacy("count", population_id = population_id)
    },
    bounded_record_count = {
      .querylibrary_sticky_unused(
        list(variable = variable, levels = levels, breaks = breaks,
             lower = lower, upper = upper, positive = positive,
             order_by = order_by), statistic
      )
      if (is.null(max_contributions)) {
        stop("max_contributions is required for a bounded record count.",
             call. = FALSE)
      }
      omop_privacy(
        statistic, reducer = "records",
        max_contributions = max_contributions,
        population_id = population_id
      )
    },
    categorical_histogram = {
      .querylibrary_sticky_unused(
        list(breaks = breaks, lower = lower, upper = upper,
             positive = positive), statistic
      )
      is_records <- identical(reducer, "records")
      if (reducer %in% c("first", "last", "records") &&
          is.null(order_by)) {
        stop("order_by is required for this categorical longitudinal ",
             "mapping.", call. = FALSE)
      }
      if (identical(reducer, "presence") && !is.null(order_by)) {
        stop("order_by is not valid for a categorical presence mapping.",
             call. = FALSE)
      }
      if (is_records && is.null(max_contributions)) {
        stop("max_contributions is required for a categorical record ",
             "histogram.", call. = FALSE)
      }
      cap <- if (is.null(max_contributions)) 1L else max_contributions
      if (reducer %in% c("first", "last") &&
          !identical(as.numeric(cap), 1)) {
        stop("First/last categorical mappings have one contribution per ",
             "person; max_contributions must be 1.", call. = FALSE)
      }
      omop_privacy(
        statistic, variable = variable, levels = levels, reducer = reducer,
        max_contributions = cap, order_by = order_by,
        population_id = population_id
      )
    },
    numeric_histogram = {
      .querylibrary_sticky_unused(
        list(levels = levels, lower = lower, upper = upper,
             positive = positive), statistic
      )
      is_records <- identical(reducer, "records")
      if (is_records && is.null(max_contributions)) {
        stop("max_contributions is required for a record histogram.",
             call. = FALSE)
      }
      if (is_records && is.null(order_by)) {
        stop("order_by is required for a record histogram.",
             call. = FALSE)
      }
      if (!is_records && !is.null(max_contributions) &&
          !identical(as.numeric(max_contributions), 1)) {
        stop("Person-reduced numeric histograms have one contribution per ",
             "person; max_contributions must be 1.", call. = FALSE)
      }
      if (reducer %in% c("first", "last") && is.null(order_by)) {
        order_by <- variable
      }
      omop_privacy(
        statistic, variable = variable, breaks = breaks, reducer = reducer,
        max_contributions = if (is_records) max_contributions else 1L,
        order_by = order_by, population_id = population_id
      )
    },
    bounded_distinct = {
      .querylibrary_sticky_unused(
        list(breaks = breaks, lower = lower, upper = upper,
             positive = positive, order_by = order_by), statistic
      )
      if (is.null(max_contributions)) {
        stop("max_contributions is required for bounded distinct ",
             "cardinality.", call. = FALSE)
      }
      omop_privacy(
        statistic, variable = variable, levels = levels,
        reducer = "distinct", max_contributions = max_contributions,
        population_id = population_id
      )
    },
    bounded_mean = {
      .querylibrary_sticky_unused(
        list(levels = levels, breaks = breaks, positive = positive,
             order_by = order_by), statistic
      )
      if (!is.null(max_contributions) &&
          !identical(as.numeric(max_contributions), 1)) {
        stop("A bounded person mean has one contribution per person; ",
             "max_contributions must be 1.", call. = FALSE)
      }
      omop_privacy(
        statistic, variable = variable, lower = lower, upper = upper,
        reducer = reducer, max_contributions = 1L,
        population_id = population_id
      )
    },
    binary_rate = {
      .querylibrary_sticky_unused(
        list(levels = levels, breaks = breaks, lower = lower, upper = upper,
             max_contributions = max_contributions, order_by = order_by),
        statistic
      )
      omop_privacy(
        statistic, variable = variable, positive = positive,
        reducer = reducer, denominator = "all_persons",
        max_contributions = 1L, population_id = population_id
      )
    },
    stop("Unsupported QueryLibrary sticky statistic.", call. = FALSE)
  )

  structure(
    list(
      upstream_id = upstream_id,
      source = list(
        repository = .QUERYLIBRARY_STICKY_REPOSITORY,
        commit = .QUERYLIBRARY_STICKY_COMMIT
      ),
      mapping = list(
        family = entry$family[[1L]], statistic = statistic,
        reducer = reducer,
        contribution_mode = entry$contribution_mode[[1L]],
        literal_upstream_sql_authorized = FALSE
      ),
      preparation_contract = list(
        route = "typed_recipe_or_plan_memory_output",
        required_class = "server_created_person_local_omop.table",
        fixed_before_data_access = c(
          "population", "concept_sets", "time_windows", "levels",
          "breaks", "bounds", "record_cap", "order"
        )
      ),
      preparation = preparation,
      privacy = privacy
    ),
    class = c("omop_querylibrary_sticky", "list")
  )
}

#' @export
print.omop_querylibrary_sticky <- function(x, ...) {
  cat("OHDSI QueryLibrary sticky redesign:", x$upstream_id, "\n")
  cat("  Primitive:", x$mapping$statistic, "\n")
  cat("  Reducer:", x$mapping$reducer, "\n")
  cat("  Upstream SQL authorized: no\n")
  invisible(x)
}

.querylibrary_sticky_validate_redesign <- function(redesign) {
  invalid <- function() {
    stop("redesign does not match its pinned QueryLibrary mapping.",
         call. = FALSE)
  }
  if (!inherits(redesign, "omop_querylibrary_sticky") ||
      !is.list(redesign) ||
      !is.character(redesign$upstream_id) ||
      length(redesign$upstream_id) != 1L || is.na(redesign$upstream_id) ||
      !inherits(redesign$privacy, "omop_privacy")) {
    invalid()
  }

  catalog <- .querylibrary_sticky_catalog_data(FALSE)
  entry <- catalog[
    catalog$upstream_id == redesign$upstream_id, , drop = FALSE
  ]
  if (nrow(entry) != 1L ||
      !is.list(redesign$source) ||
      !identical(redesign$source$repository,
                 .QUERYLIBRARY_STICKY_REPOSITORY) ||
      !identical(redesign$source$commit, .QUERYLIBRARY_STICKY_COMMIT) ||
      !is.list(redesign$mapping) ||
      !identical(redesign$mapping$family, entry$family[[1L]]) ||
      !identical(redesign$mapping$statistic, entry$statistic[[1L]]) ||
      !identical(redesign$mapping$reducer, entry$reducer[[1L]]) ||
      !identical(redesign$mapping$contribution_mode,
                 entry$contribution_mode[[1L]]) ||
      !identical(redesign$mapping$literal_upstream_sql_authorized, FALSE)) {
    invalid()
  }

  canonical_privacy <- tryCatch(
    do.call(omop_privacy, unclass(redesign$privacy)),
    error = function(e) NULL
  )
  if (is.null(canonical_privacy) ||
      !identical(canonical_privacy, redesign$privacy) ||
      !identical(redesign$privacy$statistic, entry$statistic[[1L]]) ||
      !identical(redesign$privacy$reducer, entry$reducer[[1L]]) ||
      (isTRUE(entry$record_cap_required[[1L]]) &&
       (is.null(redesign$privacy$max_contributions) ||
        redesign$privacy$max_contributions < 1L)) ||
      (isTRUE(entry$order_by_required[[1L]]) &&
       is.null(redesign$privacy$order_by))) {
    invalid()
  }
  entry
}

.querylibrary_sticky_server_preflight <- function(redesign, datasources) {
  expected_entry <- .querylibrary_sticky_validate_redesign(redesign)
  raw <- .dp_complete_aggregate(
    datasources, call("omopQueryLibraryStickyCatalogDS"),
    "QueryLibrary sticky catalog preflight"
  )
  for (server in names(raw)) {
    value <- raw[[server]]
    required <- c(
      "upstream_id", "family", "statistic", "reducer", "source_commit",
      "contribution_mode", "record_cap_required", "order_by_required",
      "literal_sql_authorized"
    )
    if (!is.data.frame(value) || any(!required %in% names(value))) {
      stop("Server '", server,
           "' returned a malformed QueryLibrary sticky catalog.",
           call. = FALSE)
    }
    row <- value[value$upstream_id == redesign$upstream_id, , drop = FALSE]
    expected <- redesign$mapping
    if (nrow(row) != 1L ||
        !identical(as.character(row$family[[1L]]), expected$family) ||
        !identical(as.character(row$statistic[[1L]]), expected$statistic) ||
        !identical(as.character(row$reducer[[1L]]), expected$reducer) ||
        !identical(as.character(row$contribution_mode[[1L]]),
                   expected_entry$contribution_mode[[1L]]) ||
        !identical(as.logical(row$record_cap_required[[1L]]),
                   expected_entry$record_cap_required[[1L]]) ||
        !identical(as.logical(row$order_by_required[[1L]]),
                   expected_entry$order_by_required[[1L]]) ||
        !identical(as.character(row$source_commit[[1L]]),
                   redesign$source$commit) ||
        !identical(row$literal_sql_authorized[[1L]], FALSE)) {
      stop("Server '", server, "' does not advertise the requested pinned ",
           "QueryLibrary sticky redesign.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Release a pinned QueryLibrary sticky redesign
#'
#' Verifies that every selected server advertises the same pinned semantic
#' redesign, then delegates to \code{\link{ds.omop.dp.release}}. \code{x} must be
#' the single memory-mode output assigned by the Recipe/Plan preparation. The
#' upstream QueryLibrary SQL is never submitted to a server. No client argument
#' can control epsilon, seed, nonce, epoch, or rerolls.
#'
#' @param x Bare DataSHIELD symbol naming the prepared person-local table.
#' @param redesign An object created by
#'   \code{\link{omop_querylibrary_sticky}}.
#' @param datasources Named DataSHIELD connection list. \code{NULL} uses active
#'   connections.
#' @param pool Pool compatible releases across sites. For
#'   \code{bounded_distinct}, pooling is the sum of site-local cardinalities,
#'   not a cross-site set union; use \code{FALSE} for site-specific estimates.
#' @param format Output format passed to \code{\link{ds.omop.dp.release}}.
#' @return A \code{dsomop_result} from \code{\link{ds.omop.dp.release}}.
#' @export
ds.omop.querylibrary.sticky.release <- function(
    x, redesign, datasources = NULL, pool = TRUE,
    format = c("long", "wide", "vector", "raw")) {
  .querylibrary_sticky_validate_redesign(redesign)
  datasources <- .dp_datasources(datasources)
  .querylibrary_sticky_server_preflight(redesign, datasources)
  ds.omop.dp.release(
    x = x, privacy = redesign$privacy, datasources = datasources,
    pool = pool, format = format
  )
}
