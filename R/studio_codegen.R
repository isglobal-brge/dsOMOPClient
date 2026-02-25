# ==============================================================================
# dsOMOPClient v2 - OMOP Studio Code Generator
# ==============================================================================
# Generates reproducible R code from interactive Shiny state.
# ==============================================================================

#' Generate R code for a plan
#'
#' @param plan An omop_plan object
#' @param out Named character vector; output symbol mapping
#' @param symbol Character; OMOP session symbol
#' @return Character; reproducible R script
#' @keywords internal
.studio_codegen_plan <- function(plan, out, symbol = "omop") {
  lines <- character(0)
  lines <- c(lines, "plan <- ds.omop.plan()")

  # Cohort

  if (!is.null(plan$cohort)) {
    if (!is.null(plan$cohort$cohort_definition_id)) {
      lines <- c(lines, paste0(
        "plan <- ds.omop.plan.cohort(plan, cohort_definition_id = ",
        plan$cohort$cohort_definition_id, ")"
      ))
    }
  }

  # Outputs
  for (nm in names(plan$outputs)) {
    out_spec <- plan$outputs[[nm]]
    otype <- out_spec$type %||% "event_level"

    if (otype == "baseline") {
      cols_str <- paste0('c("',
        paste(out_spec$columns, collapse = '", "'), '")')
      derived_str <- if (length(out_spec$derived) > 0)
        paste0('c("', paste(out_spec$derived, collapse = '", "'), '")')
      else "NULL"
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.baseline(plan, columns = ', cols_str,
        ', derived = ', derived_str,
        ', name = "', nm, '")'
      ))
    } else if (otype == "survival") {
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.survival(plan, outcome_table = "',
        out_spec$outcome$table, '", outcome_concepts = c(',
        paste(out_spec$outcome$concept_set, collapse = ", "),
        '), tar = list(start_offset = ',
        out_spec$tar$start_offset %||% 0,
        ', end_offset = ', out_spec$tar$end_offset %||% 730,
        '), name = "', nm, '")'
      ))
    } else if (otype == "cohort_membership") {
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.cohort_membership(plan, name = "', nm, '")'
      ))
    } else if (otype == "intervals_long") {
      tbls_str <- paste0('c("',
        paste(out_spec$tables, collapse = '", "'), '")')
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.intervals(plan, tables = ', tbls_str,
        ', name = "', nm, '")'
      ))
    } else if (otype == "temporal_covariates") {
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.temporal_covariates(plan, table = "',
        out_spec$table, '", concept_set = c(',
        paste(out_spec$concept_set, collapse = ", "),
        '), bin_width = ', out_spec$bin_width %||% 30L,
        ', window_start = ', out_spec$window_start %||% -365L,
        ', window_end = ', out_spec$window_end %||% 0L,
        ', name = "', nm, '")'
      ))
    } else if (otype == "concept_dictionary") {
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.concept_dictionary(plan, name = "', nm, '")'
      ))
    } else if (otype == "event_level") {
      cs_str <- ""
      cs <- out_spec$concept_set %||% out_spec$filters$concept_set$ids
      if (!is.null(cs) && length(cs) > 0) {
        cs_str <- paste0(", concept_set = c(",
          paste(cs, collapse = ", "), ")")
      }
      repr_str <- paste0(', representation = list(format = "',
        out_spec$representation$format %||% "long", '")')
      lines <- c(lines, paste0(
        'plan <- ds.omop.plan.events(plan, name = "', nm,
        '", table = "', out_spec$table, '"',
        cs_str, repr_str, ')'
      ))
    }
  }

  # Execute
  lines <- c(lines, "")
  if (length(out) > 0) {
    out_str <- paste0('c(',
      paste(vapply(names(out), function(nm) {
        paste0(nm, ' = "', out[[nm]], '"')
      }, character(1)), collapse = ", "), ')')
    lines <- c(lines, paste0(
      'ds.omop.plan.execute(plan, out = ', out_str,
      ', symbol = "', symbol, '")'
    ))
  }

  paste(lines, collapse = "\n")
}

#' Generate R code for a concept set
#'
#' @param concept_ids Integer vector
#' @param name Character; variable name
#' @return Character; R code
#' @keywords internal
.studio_codegen_concept_set <- function(concept_ids, name = "my_concepts") {
  paste0(name, " <- ds.omop.concept.set(c(",
         paste(concept_ids, collapse = ", "), "))")
}

# --- Per-function codegen helpers for Studio modules --------------------------

#' Generate R code for an exploration function call
#' @param fn_name Character; function name (e.g. "ds.omop.concept.prevalence")
#' @param args_list Named list of arguments
#' @return Character; R code string
#' @keywords internal
.studio_codegen_exploration <- function(fn_name, args_list) {
  do.call(.build_code, c(list(fn_name = fn_name), args_list))
}

#' Generate R code for a profiling function call
#' @param fn_name Character; function name
#' @param args_list Named list of arguments
#' @return Character; R code string
#' @keywords internal
.studio_codegen_profiling <- function(fn_name, args_list) {
  do.call(.build_code, c(list(fn_name = fn_name), args_list))
}

#' Generate R code for a vocabulary function call
#' @param fn_name Character; function name
#' @param args_list Named list of arguments
#' @return Character; R code string
#' @keywords internal
.studio_codegen_vocabulary <- function(fn_name, args_list) {
  do.call(.build_code, c(list(fn_name = fn_name), args_list))
}
