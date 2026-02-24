# ==============================================================================
# dsOMOPClient v2 - Extraction Plan DSL
# ==============================================================================

#' Create a new extraction plan
#'
#' @return A plan list object
#' @export
ds.omop.plan <- function() {
  plan <- list(
    cohort = NULL,
    anchor = list(table = "person", id_col = "person_id"),
    outputs = list(),
    options = list(
      translate_concepts = FALSE,
      block_sensitive = TRUE,
      min_persons = NULL
    )
  )
  class(plan) <- c("omop_plan", "list")
  plan
}

#' Set a cohort filter on the plan
#'
#' @param plan An omop_plan object
#' @param cohort_definition_id Integer; ID of existing cohort
#' @param spec Named list; cohort specification DSL
#' @return The modified plan
#' @export
ds.omop.plan.cohort <- function(plan,
                                cohort_definition_id = NULL,
                                spec = NULL) {
  if (!is.null(cohort_definition_id)) {
    plan$cohort <- list(
      type = "cohort_table",
      cohort_definition_id = as.integer(cohort_definition_id)
    )
  } else if (!is.null(spec)) {
    plan$cohort <- list(
      type = "spec",
      spec = spec
    )
  }
  plan
}

#' Add a baseline demographics output to the plan
#'
#' Produces one row per cohort member with demographics from the person
#' table and optional derived fields. Requires a cohort to be set.
#'
#' @param plan An omop_plan object
#' @param columns Character vector; person-table columns to include
#' @param derived Character vector; derived fields to compute
#'   (e.g. \code{"age_at_index"}, \code{"prior_observation"},
#'   \code{"future_observation"})
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.baseline <- function(plan,
                                  columns = c("gender_concept_id",
                                              "year_of_birth",
                                              "race_concept_id"),
                                  derived = c("age_at_index"),
                                  name = "baseline") {
  plan$outputs[[name]] <- list(
    type = "baseline",
    columns = columns,
    derived = derived
  )
  plan
}

#' Add person-level tables to the plan (raw join)
#'
#' Joins one or more tables by person_id and merges into a single
#' wide data.frame. For cohort-aware demographics with derived fields,
#' use \code{\link{ds.omop.plan.baseline}} instead.
#'
#' @param plan An omop_plan object
#' @param tables Named list; table_name = c(column_names)
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.person_level <- function(plan, tables,
                                      name = "person_data") {
  plan$outputs[[name]] <- list(
    type = "person_level",
    tables = tables
  )
  plan
}

#' Add a survival (time-to-event) output to the plan
#'
#' Produces one row per cohort member with event indicator (0/1) and
#' time-to-event in days. No calendar dates in output.
#'
#' @param plan An omop_plan object
#' @param outcome_table Character; OMOP table containing outcome events
#' @param outcome_concepts Numeric vector; concept IDs for the outcome
#' @param tar Named list; time-at-risk with \code{start_offset} and
#'   \code{end_offset} (days from cohort_start_date)
#' @param event_order Character; \code{"first"} or \code{"last"} event
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.survival <- function(plan,
                                  outcome_table = "condition_occurrence",
                                  outcome_concepts,
                                  tar = list(start_offset = 0,
                                             end_offset = 730),
                                  event_order = "first",
                                  name = "survival") {
  plan$outputs[[name]] <- list(
    type = "survival",
    outcome = list(
      table = outcome_table,
      concept_set = as.integer(outcome_concepts)
    ),
    tar = tar,
    event_order = event_order
  )
  plan
}

#' Add a concept dictionary output to the plan
#'
#' Scans other outputs for concept IDs and produces a lookup table
#' with concept names, domains, and which outputs reference each concept.
#'
#' @param plan An omop_plan object
#' @param source_outputs Character vector; names of outputs to scan
#'   (NULL = all non-dictionary outputs)
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.concept_dictionary <- function(plan,
                                             source_outputs = NULL,
                                             name = "concept_dictionary") {
  plan$outputs[[name]] <- list(
    type = "concept_dictionary",
    source_outputs = source_outputs
  )
  plan
}

#' Add an event-level extraction to the plan
#'
#' @param plan An omop_plan object
#' @param name Character; output name
#' @param table Character; source table name
#' @param columns Character vector; columns to include
#' @param concept_set Numeric vector or concept set spec
#' @param time_window Named list with start_date, end_date
#' @param filters Named list; additional filter DSL
#' @param temporal An omop_temporal_spec or list; temporal filtering
#' @param date_handling A list; date handling specification
#' @param representation Named list with format and settings
#' @return The modified plan
#' @export
ds.omop.plan.events <- function(plan, name, table,
                                columns = NULL,
                                concept_set = NULL,
                                time_window = NULL,
                                temporal = NULL,
                                date_handling = NULL,
                                filters = NULL,
                                representation = list(
                                  format = "long")) {
  output <- list(
    type = "event_level",
    table = table,
    columns = columns,
    representation = representation
  )

  output$filters <- list()
  if (!is.null(concept_set)) {
    output$filters$concept_set <- list(ids = concept_set)
    output$concept_set <- concept_set
  }
  if (!is.null(time_window)) {
    output$filters$time_window <- time_window
  }
  if (!is.null(filters)) {
    output$filters$custom <- filters
  }
  if (!is.null(temporal)) {
    output$temporal <- temporal
  }
  if (!is.null(date_handling)) {
    output$date_handling <- date_handling
  }

  plan$outputs[[name]] <- output
  plan
}

#' Add feature extraction with recipe specs
#'
#' @param plan An omop_plan object
#' @param name Character; output name
#' @param table Character; source table name
#' @param specs Named list of omop_feature_spec objects
#' @return The modified plan
#' @export
ds.omop.plan.features <- function(plan, name, table,
                                  specs) {
  output <- list(
    type = "event_level",
    table = table,
    representation = list(
      format = "features",
      features = specs
    )
  )

  # Collect concept IDs from all specs
  all_concepts <- unique(unlist(lapply(specs, function(s) {
    cs <- s$concept_set
    if (is.list(cs) && !is.null(cs$concepts)) cs$concepts
    else as.integer(cs)
  })))

  if (length(all_concepts) > 0) {
    output$filters <- list(
      concept_set = list(ids = all_concepts)
    )
    output$concept_set <- all_concepts
  }

  plan$outputs[[name]] <- output
  plan
}

#' Add an outcome extraction (convenience)
#'
#' @param plan An omop_plan object
#' @param name Character; output name
#' @param concept_set Numeric vector; outcome concept IDs
#' @param table Character; source table
#' @return The modified plan
#' @export
ds.omop.plan.outcome <- function(plan, name, concept_set,
                                 table = "condition_occurrence") {
  ds.omop.plan.events(
    plan, name = name, table = table,
    concept_set = concept_set,
    representation = list(format = "features")
  )
}

#' Add a cohort membership output to the plan
#'
#' Produces the standard OHDSI cohort table as a named output with
#' row_id, subject_id, cohort_definition_id, cohort_start_date,
#' cohort_end_date. Requires a cohort to be set.
#'
#' @param plan An omop_plan object
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.cohort_membership <- function(plan,
                                            name = "cohort_membership") {
  plan$outputs[[name]] <- list(
    type = "cohort_membership"
  )
  plan
}

#' Add an intervals (long) output to the plan
#'
#' Extracts interval data (observation periods, visits, drug/condition
#' durations) with start/end days relative to the cohort index date.
#' Requires a cohort to be set.
#'
#' @param plan An omop_plan object
#' @param tables Character vector; OMOP tables to extract intervals from
#' @param concept_filter Named list; per-table concept ID filters
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.intervals <- function(plan,
                                    tables = c("observation_period",
                                               "visit_occurrence",
                                               "drug_exposure",
                                               "condition_occurrence"),
                                    concept_filter = NULL,
                                    name = "intervals") {
  plan$outputs[[name]] <- list(
    type = "intervals_long",
    tables = tables,
    concept_filter = concept_filter
  )
  plan
}

#' Add a temporal (time-binned) covariates output to the plan
#'
#' Produces FeatureExtraction-compatible sparse covariates binned into
#' time windows relative to the cohort index date. Returns 3 symbols:
#' \code{<name>.temporalCovariates}, \code{<name>.covariateRef},
#' \code{<name>.timeRef}. Requires a cohort to be set.
#'
#' @param plan An omop_plan object
#' @param table Character; source OMOP table
#' @param concept_set Numeric vector; concept IDs to include
#' @param bin_width Integer; bin width in days
#' @param window_start Integer; start of window (days from index)
#' @param window_end Integer; end of window (days from index)
#' @param analyses Character vector; analyses to compute
#'   (\code{"binary"}, \code{"count"})
#' @param name Character; output name
#' @return The modified plan
#' @export
ds.omop.plan.temporal_covariates <- function(plan,
                                              table,
                                              concept_set,
                                              bin_width = 30L,
                                              window_start = -365L,
                                              window_end = 0L,
                                              analyses = c("binary"),
                                              name = "temporal") {
  plan$outputs[[name]] <- list(
    type = "temporal_covariates",
    table = table,
    concept_set = as.integer(concept_set),
    bin_width = as.integer(bin_width),
    window_start = as.integer(window_start),
    window_end = as.integer(window_end),
    analyses = analyses
  )
  plan
}

#' Set plan-wide options
#'
#' @param plan An omop_plan object
#' @param translate_concepts Logical
#' @param block_sensitive Logical
#' @param min_persons Integer
#' @return The modified plan
#' @export
ds.omop.plan.options <- function(plan,
                                 translate_concepts = NULL,
                                 block_sensitive = NULL,
                                 min_persons = NULL) {
  if (!is.null(translate_concepts)) {
    plan$options$translate_concepts <- translate_concepts
  }
  if (!is.null(block_sensitive)) {
    plan$options$block_sensitive <- block_sensitive
  }
  if (!is.null(min_persons)) {
    plan$options$min_persons <- min_persons
  }
  plan
}

#' Build a temporal filtering specification
#'
#' @param index_window Named list with start/end (days relative to index)
#' @param calendar Named list with start/end (calendar dates)
#' @param event_select Named list with order ("first"/"last") and n
#' @param min_gap Integer; minimum days between events
#' @return An omop_temporal_spec object
#' @export
omop.temporal <- function(index_window = NULL, calendar = NULL,
                          event_select = NULL, min_gap = NULL) {
  spec <- list()
  if (!is.null(index_window)) spec$index_window <- index_window
  if (!is.null(calendar)) spec$calendar <- calendar
  if (!is.null(event_select)) spec$event_select <- event_select
  if (!is.null(min_gap)) spec$min_gap <- min_gap
  class(spec) <- c("omop_temporal_spec", "list")
  spec
}

#' Build a date handling specification
#'
#' @param mode Character; "absolute", "relative", "binned", or "remove"
#' @param reference Character; reference for relative mode ("index")
#' @param bin_width Character; for binned mode ("week", "month", "year")
#' @param date_columns Character vector; specific columns (NULL = all)
#' @return A date handling spec list
#' @export
omop.date_handling <- function(mode = "absolute", reference = "index",
                               bin_width = NULL, date_columns = NULL) {
  list(mode = mode, reference = reference,
       bin_width = bin_width, date_columns = date_columns)
}

#' Validate a plan against server schemas
#'
#' @param plan An omop_plan object
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) with validation results
#' @export
ds.omop.plan.validate <- function(plan, symbol = "omop",
                                  conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, plan)
  )
}

#' Preview a plan (safe aggregate)
#'
#' @param plan An omop_plan object
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Named list (per server) with preview info
#' @export
ds.omop.plan.preview <- function(plan, symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  DSI::datashield.aggregate(
    conns,
    expr = call("omopPlanPreviewDS",
                session$res_symbol, plan)
  )
}

#' Execute a plan and create server-side tables
#'
#' Sends the plan to the server for execution. The server-side
#' \code{omopPlanExecuteDS} assigns each output directly into the
#' DataSHIELD session as the symbol names specified in \code{out}.
#' Sparse outputs are split into two symbols:
#' \code{<name>.covariates} and \code{<name>.covariateRef}.
#'
#' @param plan An omop_plan object
#' @param out Named character vector; output_name -> symbol_name mapping.
#'   E.g. \code{c(baseline = "D_base", survival = "D_tte")}
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return Invisible; the output symbol mapping
#' @export
ds.omop.plan.execute <- function(plan, out,
                                 symbol = "omop",
                                 conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  # Single assign call: server assigns each output directly into session
  exec_symbol <- .generate_symbol("dsOexec")

  DSI::datashield.assign.expr(
    conns,
    symbol = exec_symbol,
    expr = call("omopPlanExecuteDS",
                session$res_symbol, plan, out)
  )

  # exec_symbol holds TRUE (return value); clean up
  tryCatch(
    DSI::datashield.rm(conns, exec_symbol),
    error = function(e) NULL
  )

  invisible(out)
}

#' Harmonize a plan for multi-server execution
#'
#' @param plan An omop_plan object
#' @param mode Character; "intersection" or "union_with_missing"
#' @param symbol Character; OMOP session symbol
#' @param conns DSI connections
#' @return The harmonized plan
#' @export
ds.omop.plan.harmonize <- function(plan,
                                   mode = "intersection",
                                   symbol = "omop",
                                   conns = NULL) {
  comparison <- ds.omop.compare(symbol, conns)

  if (mode == "intersection") {
    common_tables <- comparison$common_tables
    for (out_name in names(plan$outputs)) {
      out <- plan$outputs[[out_name]]
      if (out$type == "person_level" && !is.null(out$tables)) {
        plan$outputs[[out_name]]$tables <-
          out$tables[tolower(names(out$tables)) %in%
                       common_tables]
      }
      if (out$type == "event_level") {
        tbl <- tolower(out$table %||% "")
        if (!tbl %in% common_tables) {
          warning("Output '", out_name,
                  "' references table '", tbl,
                  "' not present on all servers.")
        }
      }
    }
  }

  plan
}

#' Print method for extraction plans
#'
#' @param x An omop_plan object
#' @param ... Additional arguments
#' @export
print.omop_plan <- function(x, ...) {
  cat("=== dsOMOP Extraction Plan ===\n")

  if (!is.null(x$cohort)) {
    cat("Cohort: ",
        if (!is.null(x$cohort$cohort_definition_id))
          paste("ID", x$cohort$cohort_definition_id)
        else "custom spec", "\n")
  } else {
    cat("Cohort: none (all persons)\n")
  }

  cat("Outputs (", length(x$outputs), "):\n")
  for (name in names(x$outputs)) {
    out <- x$outputs[[name]]
    otype <- out$type %||% "event_level"

    if (otype == "person_level") {
      cat("  [person_level] ", name, ": ",
          length(out$tables), " tables\n")
    } else if (otype == "baseline") {
      n_cols <- length(out$columns %||% character(0))
      n_derived <- length(out$derived %||% character(0))
      cat("  [baseline] ", name, ": ",
          n_cols, " columns, ", n_derived, " derived\n")
    } else if (otype == "survival") {
      n_concepts <- length(out$outcome$concept_set %||% integer(0))
      tar_end <- out$tar$end_offset %||% "cohort_end"
      cat("  [survival] ", name, ": ",
          out$outcome$table %||% "?", " (",
          n_concepts, " concepts), TAR 0-", tar_end, " days\n")
    } else if (otype == "concept_dictionary") {
      srcs <- out$source_outputs %||% "all"
      cat("  [dictionary] ", name, ": from ",
          paste(srcs, collapse = ", "), "\n")
    } else if (otype == "cohort_membership") {
      cat("  [cohort] ", name, ": standard OHDSI format\n")
    } else if (otype == "intervals_long") {
      n_tbls <- length(out$tables %||% character(0))
      cat("  [intervals] ", name, ": ",
          n_tbls, " tables\n")
    } else if (otype == "temporal_covariates") {
      bw <- out$bin_width %||% 30L
      ws <- out$window_start %||% -365L
      we <- out$window_end %||% 0L
      cat("  [temporal] ", name, ": ", out$table,
          " bins=", bw, "d [", ws, ",", we, "]\n")
    } else {
      repr <- out$representation$format %||% "long"
      n_concepts <- length(
        out$filters$concept_set$ids %||%
          out$concept_set)
      temporal_info <- ""
      if (!is.null(out$temporal)) {
        parts <- character(0)
        if (!is.null(out$temporal$index_window))
          parts <- c(parts, "index-window")
        if (!is.null(out$temporal$calendar))
          parts <- c(parts, "calendar")
        if (!is.null(out$temporal$event_select))
          parts <- c(parts, out$temporal$event_select$order)
        if (length(parts) > 0)
          temporal_info <- paste0(" [", paste(parts, collapse = "+"), "]")
      }
      dh_info <- ""
      if (!is.null(out$date_handling)) {
        dh_info <- paste0(" dates:", out$date_handling$mode)
      }
      cat("  [", repr, "] ", name, ": ",
          out$table,
          if (n_concepts > 0)
            paste0(" (", n_concepts, " concepts)")
          else "",
          temporal_info, dh_info, "\n")
    }
  }

  cat("Options: translate=",
      x$options$translate_concepts %||% FALSE,
      " block_sensitive=",
      x$options$block_sensitive %||% TRUE, "\n")
  invisible(x)
}
