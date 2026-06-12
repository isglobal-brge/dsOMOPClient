# Module: Plan DAG visualisation
# Builds a directed-acyclic-graph view of an omop_recipe for the Plan tab:
# COHORT -> EXTRACTION -> DERIVATION -> OUTPUT. Rendered with visNetwork
# (an optional Suggests dependency). All helpers are internal.

# Formats that are computed from the person/observation_period record itself
# and therefore need no concept extraction step (mirrors recipe_to_plan()).
.plan_person_derived_fmts <- c(
  "age", "sex_mf", "obs_duration", "drug_duration",
  "prior_obs", "followup", "demo_missingness",
  "charlson", "chads2", "chadsvasc", "dcsi", "hfrs"
)

# Distribution statistics whose disclosure floor is stricter (n < 10).
.plan_distribution_fmts <- c("mean", "sd", "cv", "slope", "min", "max")

# Per-domain colour + FontAwesome icon (unicode codepoints visNetwork uses).
.plan_domain_style <- function(table) {
  styles <- list(
    condition_occurrence  = list(color = "#e74c3c", icon = "f0f1"), # stethoscope
    drug_exposure         = list(color = "#9b59b6", icon = "f490"), # capsules
    measurement           = list(color = "#16a085", icon = "f492"), # vial
    procedure_occurrence  = list(color = "#e67e22", icon = "f0fe"), # plus-square
    observation           = list(color = "#2980b9", icon = "f06e"), # eye
    visit_occurrence      = list(color = "#795548", icon = "f0f8"), # hospital
    person                = list(color = "#7f8c8d", icon = "f007"), # user
    observation_period    = list(color = "#7f8c8d", icon = "f073")  # calendar
  )
  styles[[table]] %||% list(color = "#7f8c8d", icon = "f111")        # circle
}

#' Human-readable label for a recipe variable's concept
#'
#' Falls back to \code{"concept <id>"} when \code{concept_name} is missing, and
#' to the table name when there is no concept at all.
#'
#' @param v An \code{omop_variable} (or list with the same fields).
#' @return Character scalar.
#' @keywords internal
.plan_concept_label <- function(v) {
  if (!is.null(v$concept_name) && nzchar(v$concept_name)) return(v$concept_name)
  if (!is.null(v$concept_id)) return(paste0("concept ", v$concept_id))
  v$table %||% "variable"
}

#' Compact window text for tooltips / provenance
#' @keywords internal
.plan_window_text <- function(window) {
  if (is.null(window)) return(NULL)
  paste0(window$start %||% "?", " to ", window$end %||% "?", " days")
}

#' HTML-escape a scalar for use inside a visNetwork tooltip
#' @keywords internal
.plan_html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Population (cohort) filter labels for a recipe
#'
#' Collects population-level filters from the dedicated \code{$filters} slot and
#' from the base population's own filters, returning their human-readable
#' labels. Uses the package's existing flatten helper so nested AND/OR groups
#' are expanded.
#'
#' @param recipe An \code{omop_recipe} object.
#' @return Character vector of filter labels (possibly empty).
#' @keywords internal
.plan_cohort_filter_labels <- function(recipe) {
  items <- .collect_pop_filter_items(recipe)
  if (length(items) == 0) return(character(0))
  vapply(items, function(f) f$label %||% f$type %||% "filter", character(1))
}

#' Build a visNetwork node/edge structure from a recipe
#'
#' Compiles an \code{omop_recipe} into the four-layer DAG used by the Plan tab:
#' a single COHORT node (base population + filter-tree label), one EXTRACTION
#' node per concept-bearing variable (coloured/iconed by domain), one
#' DERIVATION node per variable format, and one OUTPUT node per
#' \code{recipe$outputs} entry. Person-derived formats skip extraction and
#' attach directly to the cohort; the \code{"raw"} format skips derivation and
#' wires extraction straight to the output.
#'
#' Disclosure flags (dashed red border) are placed on: extraction nodes
#' (aggregate cells, n < 3); cohort and output nodes when
#' \code{recipe$options$min_persons} is set; and distribution-stat derivations
#' (mean/sd/cv/slope/min/max, n < 10).
#'
#' @param recipe An \code{omop_recipe} object, or \code{NULL}.
#' @return A list with \code{nodes} and \code{edges} data frames
#'   (\code{stringsAsFactors = FALSE}) ready for \code{visNetwork::visNetwork}.
#' @keywords internal
.recipe_to_dag <- function(recipe) {
  empty_nodes <- function() data.frame(
    id = character(0), label = character(0), group = character(0),
    title = character(0), color.background = character(0),
    color.border = character(0), shapeProperties.borderDashes = logical(0),
    shape = character(0), icon.code = character(0), icon.color = character(0),
    stringsAsFactors = FALSE
  )
  empty_edges <- function() data.frame(
    from = character(0), to = character(0), arrows = character(0),
    stringsAsFactors = FALSE
  )

  # Edge case: no recipe / nothing in it -> single placeholder node.
  has_content <- !is.null(recipe) &&
    inherits(recipe, "omop_recipe") &&
    (length(recipe$variables) > 0 || length(recipe$outputs) > 0)
  if (!has_content) {
    # Single placeholder node. No icon.* columns (their nested NA -> JSON null
    # stops vis.js rendering) and a friendly group label for the legend.
    nodes <- data.frame(
      id = "empty", label = "No plan yet", group = "(empty)",
      title = "Add variables and an output to build a plan.",
      color.background = "#ecf0f1", color.border = "#bdc3c7",
      shapeProperties.borderDashes = FALSE, shape = "box",
      stringsAsFactors = FALSE
    )
    return(list(nodes = nodes, edges = empty_edges()))
  }

  min_persons_set <- !is.null(recipe$options$min_persons)
  cohort_labels <- .plan_cohort_filter_labels(recipe)
  base_label <- recipe$populations$base$label %||% "All Persons"

  node_rows <- list()
  edge_rows <- list()
  add_node <- function(...) node_rows[[length(node_rows) + 1L]] <<- list(...)
  add_edge <- function(from, to) edge_rows[[length(edge_rows) + 1L]] <<-
    list(from = from, to = to, arrows = "to")

  # --- COHORT node -----------------------------------------------------------
  cohort_id <- "cohort"
  cohort_lbl <- base_label
  if (length(cohort_labels) > 0) {
    cohort_lbl <- paste0(base_label, "\n", paste(cohort_labels, collapse = "\n"))
  }
  cohort_title <- paste0(
    "<b>Cohort</b><br/>", .plan_html_escape(base_label),
    if (length(cohort_labels) > 0)
      paste0("<br/>Filters:<br/>- ",
             paste(.plan_html_escape(cohort_labels), collapse = "<br/>- "))
    else "",
    if (min_persons_set)
      paste0("<br/>min_persons = ", recipe$options$min_persons) else ""
  )
  add_node(
    id = cohort_id, label = cohort_lbl, group = "COHORT", title = cohort_title,
    color.background = "#34495e", color.border = if (min_persons_set) "#c0392b" else "#2c3e50",
    shapeProperties.borderDashes = min_persons_set, shape = "box",
    icon.code = NA_character_, icon.color = NA_character_
  )

  # --- per-variable EXTRACTION + DERIVATION nodes ----------------------------
  # Track the "terminal" node id for each variable name so outputs can wire to
  # it (derivation node when present, else extraction node, else cohort).
  var_terminal <- list()

  for (vname in names(recipe$variables)) {
    v <- recipe$variables[[vname]]
    fmt <- v$format %||% "raw"
    is_person_derived <- fmt %in% .plan_person_derived_fmts
    label <- .plan_concept_label(v)

    extraction_id <- NULL
    if (!is_person_derived) {
      # Extraction node (concept-bearing). Person-derived formats skip this.
      style <- .plan_domain_style(v$table)
      extraction_id <- paste0("extract_", vname)
      cells_disclosive <- TRUE  # aggregate extraction cells: n < 3 floor
      ex_title <- paste0(
        "<b>Extraction</b><br/>", .plan_html_escape(label),
        if (!is.null(v$concept_id)) paste0(" (", v$concept_id, ")") else "",
        "<br/>Table: ", .plan_html_escape(v$table %||% "?"),
        if (!is.null(v$value_source))
          paste0("<br/>Value: ", .plan_html_escape(v$value_source)) else "",
        if (!is.null(.plan_window_text(v$time_window)))
          paste0("<br/>Window: ", .plan_window_text(v$time_window)) else "",
        if (length(v$filters) > 0)
          paste0("<br/>Row filters: ", length(v$filters)) else ""
      )
      add_node(
        id = extraction_id, label = label, group = "EXTRACTION", title = ex_title,
        color.background = style$color,
        color.border = if (cells_disclosive) "#c0392b" else style$color,
        shapeProperties.borderDashes = cells_disclosive, shape = "box",
        icon.code = intToUtf8(strtoi(style$icon, 16L)), icon.color = style$color
      )
      add_edge(cohort_id, extraction_id)
    }

    if (fmt == "raw") {
      # Raw skips derivation; extraction wires straight to output.
      var_terminal[[vname]] <- extraction_id %||% cohort_id
      next
    }

    # Derivation node (one per variable's format).
    deriv_id <- paste0("derive_", vname)
    is_dist <- fmt %in% .plan_distribution_fmts
    dv_title <- paste0(
      "<b>Derivation</b><br/>", .plan_html_escape(vname),
      "<br/>Format: ", .plan_html_escape(fmt),
      if (is_person_derived) "<br/>Person-derived" else ""
    )
    add_node(
      id = deriv_id, label = paste0(vname, "\n[", fmt, "]"),
      group = "DERIVATION", title = dv_title,
      color.background = "#f1c40f",
      color.border = if (is_dist) "#c0392b" else "#f39c12",
      shapeProperties.borderDashes = is_dist, shape = "box",
      icon.code = NA_character_, icon.color = NA_character_
    )
    # Person-derived attaches directly to cohort; otherwise to its extraction.
    add_edge(if (is_person_derived) cohort_id else extraction_id, deriv_id)
    var_terminal[[vname]] <- deriv_id
  }

  # --- OUTPUT nodes ----------------------------------------------------------
  for (out_name in names(recipe$outputs)) {
    out <- recipe$outputs[[out_name]]
    out_id <- paste0("output_", out_name)
    out_type <- out$type %||% "wide"
    out_vars <- out$variables %||% names(recipe$variables)
    out_title <- paste0(
      "<b>Output</b><br/>", .plan_html_escape(out_name),
      "<br/>Type: ", .plan_html_escape(out_type),
      "<br/>Population: ", .plan_html_escape(out$population_id %||% "base"),
      "<br/>Variables: ", length(out_vars)
    )
    add_node(
      id = out_id, label = paste0(out_name, " (", out_type, ")"),
      group = "OUTPUT", title = out_title,
      color.background = "#27ae60",
      color.border = if (min_persons_set) "#c0392b" else "#1e8449",
      shapeProperties.borderDashes = min_persons_set, shape = "box",
      icon.code = NA_character_, icon.color = NA_character_
    )
    # Wire each of the output's variables from its terminal node. Multiple
    # outputs may share the same variable -> multiple edges from one terminal.
    for (vname in out_vars) {
      term <- var_terminal[[vname]]
      if (is.null(term)) next   # output references an unknown variable
      add_edge(term, out_id)
    }
    # Output with no (resolvable) variables: anchor it to the cohort so it is
    # still reachable in the layout.
    wired <- any(vapply(out_vars, function(vn) !is.null(var_terminal[[vn]]),
                        logical(1)))
    if (!wired) add_edge(cohort_id, out_id)
  }

  nodes <- do.call(rbind, lapply(node_rows, function(r)
    as.data.frame(r, stringsAsFactors = FALSE)))
  edges <- if (length(edge_rows) > 0)
    do.call(rbind, lapply(edge_rows, function(r)
      as.data.frame(r, stringsAsFactors = FALSE)))
  else empty_edges()

  # All nodes use shape="box"; the icon.* columns are unused, and their NA
  # values serialize to nested JSON nulls that prevent vis.js from rendering the
  # network (only the legend shows). Drop them — color.background already encodes
  # the domain and the dashed border encodes the disclosure flag.
  nodes$icon.code <- NULL
  nodes$icon.color <- NULL

  list(nodes = nodes, edges = edges)
}

#' Render the recipe plan DAG with visNetwork
#'
#' Produces a \code{visNetwork::renderVisNetwork} expression for the Plan tab.
#' Layout is left-to-right hierarchical (sorted by edge direction); nodes are
#' boxes; a group legend is shown; clicking a node sets the Shiny input
#' \code{ns("plan_node")} to the node id for downstream provenance display.
#'
#' Guarded by \code{requireNamespace("visNetwork")}: if the package is absent,
#' returns \code{NULL} so the caller can fall back to a text summary.
#'
#' @param recipe An \code{omop_recipe} object, or \code{NULL}.
#' @param ns A Shiny namespace function (\code{session$ns}).
#' @return A \code{shiny::renderUI}-compatible render object, or \code{NULL}.
#' @keywords internal
.render_plan_dag <- function(recipe, ns) {
  if (!requireNamespace("visNetwork", quietly = TRUE)) return(NULL)

  dag <- .recipe_to_dag(recipe)

  visNetwork::renderVisNetwork({
    vis <- visNetwork::visNetwork(dag$nodes, dag$edges)
    vis <- visNetwork::visHierarchicalLayout(vis, direction = "LR",
                                             sortMethod = "directed")
    vis <- visNetwork::visNodes(vis, shape = "box")
    vis <- visNetwork::visEdges(vis, arrows = "to")
    vis <- visNetwork::visLegend(vis, useGroups = TRUE)
    vis <- visNetwork::visEvents(vis, selectNode = paste0(
      "function(nodes) {",
      "  Shiny.setInputValue('", ns("plan_node"),
      "', nodes.nodes[0], {priority: 'event'});",
      "}"
    ))
    vis
  })
}

#' Provenance UI for a clicked plan node
#'
#' Given a node id from \code{ns("plan_node")}, returns a Shiny UI block listing
#' that node's full provenance: concept(s), table, format, value source, time
#' window, and every applicable filter (population/cohort filters plus the
#' variable's own row filters). Returns a friendly placeholder for the cohort,
#' output, or empty nodes.
#'
#' @param recipe An \code{omop_recipe} object, or \code{NULL}.
#' @param node_id Character; the clicked node id (e.g. \code{"extract_age"}),
#'   or \code{NULL}.
#' @return A Shiny UI object.
#' @keywords internal
.render_node_provenance <- function(recipe, node_id) {
  if (is.null(node_id) || is.null(recipe) || identical(node_id, "empty")) {
    return(.empty_state_ui("hand-pointer", "No node selected",
      "Click a node in the plan to inspect its provenance."))
  }

  pop_labels <- .plan_cohort_filter_labels(recipe)
  row_item <- function(lbl, val) {
    if (is.null(val) || (is.character(val) && !nzchar(val))) return(NULL)
    shiny::tags$li(shiny::strong(paste0(lbl, ": ")), shiny::span(as.character(val)))
  }

  # Cohort node.
  if (identical(node_id, "cohort")) {
    return(shiny::div(
      shiny::h5(recipe$populations$base$label %||% "All Persons"),
      shiny::tags$ul(
        row_item("Population", "base"),
        if (!is.null(recipe$options$min_persons))
          row_item("min_persons", recipe$options$min_persons)
      ),
      shiny::h6("Cohort filters"),
      if (length(pop_labels) > 0)
        shiny::tags$ul(lapply(pop_labels, shiny::tags$li))
      else shiny::p(shiny::em("None"))
    ))
  }

  # Output node.
  if (startsWith(node_id, "output_")) {
    out_name <- sub("^output_", "", node_id)
    out <- recipe$outputs[[out_name]]
    if (is.null(out)) {
      return(.empty_state_ui("circle-question", "Unknown output", node_id))
    }
    out_vars <- out$variables %||% names(recipe$variables)
    return(shiny::div(
      shiny::h5(paste0(out_name, " (", out$type %||% "wide", ")")),
      shiny::tags$ul(
        row_item("Type", out$type %||% "wide"),
        row_item("Population", out$population_id %||% "base"),
        row_item("Variables", paste(out_vars, collapse = ", "))
      )
    ))
  }

  # Variable nodes: extraction or derivation both map to one variable.
  vname <- sub("^(extract|derive)_", "", node_id)
  v <- recipe$variables[[vname]]
  if (is.null(v)) {
    return(.empty_state_ui("circle-question", "Unknown node", node_id))
  }

  own_filters <- if (length(v$filters) > 0)
    vapply(v$filters, function(f) f$label %||% f$type %||% "filter", character(1))
  else character(0)
  all_filters <- c(pop_labels, own_filters)

  shiny::div(
    shiny::h5(.plan_concept_label(v)),
    shiny::tags$ul(
      row_item("Variable", vname),
      row_item("Concept", if (!is.null(v$concept_id))
        paste0(.plan_concept_label(v), " (", v$concept_id, ")") else NULL),
      row_item("Table", v$table),
      row_item("Column", v$column),
      row_item("Format", v$format),
      row_item("Value source", v$value_source),
      row_item("Time window", .plan_window_text(v$time_window))
    ),
    shiny::h6("Applicable filters"),
    if (length(all_filters) > 0)
      shiny::tags$ul(lapply(all_filters, shiny::tags$li))
    else shiny::p(shiny::em("None"))
  )
}
