# Module: Studio - Data Explorer
# Shiny module for interactive data profiling and exploration.

#' Studio Data Explorer UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
.mod_explore_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_pill(
    id = ns("explore_nav"),
    bslib::nav_panel("Prevalence", icon = shiny::icon("chart-simple"),
      .mod_explore_prevalence_ui(ns("prevalence"))),
    bslib::nav_panel("Drilldown", icon = shiny::icon("magnifying-glass-chart"),
      .mod_explore_drilldown_ui(ns("drilldown"))),
    bslib::nav_panel("Locator", icon = shiny::icon("location-dot"),
      .mod_explore_locator_ui(ns("locator"))),
    bslib::nav_panel("Vocabulary", icon = shiny::icon("book"),
      .mod_explore_vocab_ui(ns("vocab"))),
    bslib::nav_panel("Concept Summary", icon = shiny::icon("database"),
      .mod_explore_concept_summary_ui(ns("concept_summary"))),
    bslib::nav_panel("Value Ranking", icon = shiny::icon("ranking-star"),
      .mod_explore_value_ranking_ui(ns("value_ranking"))),
    bslib::nav_panel("Numeric Distribution", icon = shiny::icon("chart-area"),
      .mod_explore_numeric_ui(ns("numeric"))),
    bslib::nav_panel("Cross-tab", icon = shiny::icon("table-cells"),
      .mod_explore_crosstab_ui(ns("crosstab"))),
    bslib::nav_panel("Missingness", icon = shiny::icon("table-cells-large"),
      .mod_explore_missingness_ui(ns("missingness")))
  )
}

#' Studio Data Explorer Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @param parent_session Shiny session; the parent session for tab navigation.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_explore_server <- function(id, state, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_explore_prevalence_server("prevalence", state, session)
    .mod_explore_drilldown_server("drilldown", state, session)
    .mod_explore_locator_server("locator", state)
    .mod_explore_vocab_server("vocab", state)
    .mod_explore_concept_summary_server("concept_summary", state, session)
    .mod_explore_value_ranking_server("value_ranking", state)
    .mod_explore_numeric_server("numeric", state)
    .mod_explore_crosstab_server("crosstab", state)
    .mod_explore_missingness_server("missingness", state)
  })
}


# SUB-MODULE 1: Prevalence (was .mod_table_concepts)

.mod_explore_prevalence_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Explore", width = 260, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("concept_col"), "Concept Column", choices = NULL),
      shiny::selectInput(ns("metric"), "Metric",
        choices = c("Distinct Persons" = "persons",
                    "Total Records" = "records")),
      shiny::actionButton(ns("run_btn"), "Run",
                          icon = shiny::icon("play"),
                          class = "btn-primary w-100")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Table Statistics"),
      bslib::card_body(
        shiny::uiOutput(ns("table_stats"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::textOutput(ns("results_title")),
          shiny::div(
            shiny::actionButton(ns("add_selected_btn"), "Add to Set",
                                class = "btn-sm btn-outline-primary me-1"),
            bslib::tooltip(
              shiny::actionButton(ns("extract_selected_btn"),
                                  shiny::tagList(shiny::icon("plus"), "Extract"),
                                  class = "btn-sm btn-success text-white me-1"),
              "Add selected concepts as variables to Builder"
            ),
            bslib::tooltip(
              shiny::actionButton(ns("filter_selected_btn"),
                                  shiny::tagList(shiny::icon("filter"), "Filter"),
                                  class = "btn-sm btn-warning text-white"),
              "Add selected concepts as filters to Builder"
            )
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("results_content")),
        shiny::uiOutput(ns("scope_info"))
      )
    )
  )
}

.mod_explore_prevalence_server <- function(id, state, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    prevalence_data <- shiny::reactiveVal(NULL)

    # Populate table dropdown from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) == 0) {
        tbl_choices <- c("condition_occurrence", "drug_exposure",
                         "measurement", "procedure_occurrence",
                         "observation", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table",
                               choices = .table_choices(tbl_choices))
    })

    # When table changes, fetch columns and populate concept column dropdown
    shiny::observeEvent(input$table, {
      tbl <- input$table
      if (is.null(tbl) || nchar(tbl) == 0) return()
      tryCatch({
        cols_res <- ds.omop.columns(tbl, symbol = state$symbol)
        srv <- names(cols_res)[1]
        col_df <- cols_res[[srv]]
        concept_cols <- .get_concept_columns(col_df)
        if (length(concept_cols) > 0) {
          # Prefer domain_concept column
          domain_cols <- col_df$column_name[col_df$concept_role == "domain_concept"]
          selected <- if (length(domain_cols) > 0) domain_cols[1]
                      else concept_cols[1]
          shiny::updateSelectInput(session, "concept_col",
                                   choices = concept_cols,
                                   selected = selected)
        } else {
          shiny::updateSelectInput(session, "concept_col",
                                   choices = character(0))
        }
      }, error = function(e) {
        shiny::updateSelectInput(session, "concept_col",
                                 choices = character(0))
      })
    }, ignoreInit = TRUE)

    # Table stats
    output$table_stats <- shiny::renderUI({
      tbl <- input$table
      if (is.null(tbl) || nchar(tbl) == 0) {
        return(shiny::p("Select a table to begin exploring."))
      }
      tryCatch({
        stats_res <- ds.omop.table.stats(tbl, stats = c("rows", "persons"),
                                          symbol = state$symbol)
        if (length(stats_res$per_site) == 0) {
          return(shiny::p(class = "text-muted",
            "Table not available on any server."))
        }
        scope <- state$scope %||% "pooled"

        if (scope == "pooled" && length(stats_res$per_site) > 1) {
          # Build a data.frame for DT display
          rows_list <- lapply(names(stats_res$per_site), function(srv) {
            s <- stats_res$per_site[[srv]]
            data.frame(
              Server = srv,
              Rows = if (isTRUE(s$rows_suppressed)) NA_character_
                     else format(s$rows, big.mark = ","),
              Persons = if (is.null(s$persons) || isTRUE(s$persons_suppressed))
                NA_character_ else format(s$persons, big.mark = ","),
              stringsAsFactors = FALSE
            )
          })
          cmp <- do.call(rbind, rows_list)
          cmp[is.na(cmp)] <- "\u2014"
          shiny::tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.84rem;",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th(class = "text-muted", "Server"),
                shiny::tags$th(class = "text-muted text-end", "Rows"),
                shiny::tags$th(class = "text-muted text-end", "Persons")
              )
            ),
            shiny::tags$tbody(
              shiny::tagList(lapply(seq_len(nrow(cmp)), function(i) {
                shiny::tags$tr(
                  shiny::tags$td(cmp$Server[i]),
                  shiny::tags$td(class = "text-end fw-semibold", cmp$Rows[i]),
                  shiny::tags$td(class = "text-end fw-semibold", cmp$Persons[i])
                )
              }))
            )
          )
        } else {
          srv <- if (scope == "per_site" && length(state$selected_servers) > 0)
            state$selected_servers[1] else names(stats_res$per_site)[1]
          if (is.null(srv) || !srv %in% names(stats_res$per_site))
            srv <- names(stats_res$per_site)[1]
          s <- stats_res$per_site[[srv]]
          rows_val <- if (isTRUE(s$rows_suppressed)) "\u2014"
                      else format(s$rows, big.mark = ",")
          persons_val <- if (is.null(s$persons) || isTRUE(s$persons_suppressed))
            "\u2014" else format(s$persons, big.mark = ",")
          shiny::tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.84rem;",
            shiny::tags$tbody(
              shiny::tags$tr(
                shiny::tags$td(class = "text-muted", "Rows"),
                shiny::tags$td(class = "text-end fw-semibold", rows_val)
              ),
              if (!is.null(s$persons)) shiny::tags$tr(
                shiny::tags$td(class = "text-muted", "Persons"),
                shiny::tags$td(class = "text-end fw-semibold", persons_val)
              )
            )
          )
        }
      }, error = function(e) {
        shiny::p(class = "text-danger", conditionMessage(e))
      })
    })

    # Track last result for scope info
    last_result <- shiny::reactiveVal(NULL)

    # Run concept prevalence
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$concept_col)

      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"

      shiny::withProgress(message = "Fetching concept prevalence...", value = 0.3, {
        tryCatch({
          res <- ds.omop.concept.prevalence(
            table = input$table, concept_col = input$concept_col,
            metric = input$metric, top_n = 99999L,
            scope = .backend_scope(scope), pooling_policy = policy,
            symbol = state$symbol
          )
          shiny::incProgress(0.5)
          # Accumulate code for Script tab
          if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, res$meta$call_code)
          }
          last_result(res)

          # Extract display data based on scope
          df <- .extract_display_data(res, scope, state$selected_servers,
                                       intersect_only = FALSE)

          # Remove disclosure-suppressed rows (NA in count columns)
          if (is.data.frame(df) && nrow(df) > 0) {
            metric_col <- if (input$metric == "persons") "n_persons" else "n_records"
            if (metric_col %in% names(df)) {
              df <- df[!is.na(df[[metric_col]]), , drop = FALSE]
            }
          }
          prevalence_data(df)
        }, error = function(e) {
          shiny::showNotification(
            .clean_ds_error(e), type = "error"
          )
        })
      })
    })

    # Re-extract on scope/server/intersect change (no re-query)
    shiny::observeEvent(list(state$scope, state$selected_servers), {
      res <- last_result()
      if (is.null(res)) return()
      scope <- state$scope %||% "per_site"

      df <- .extract_display_data(res, scope, state$selected_servers,
                                   intersect_only = FALSE)

      # Remove disclosure-suppressed rows
      if (is.data.frame(df) && nrow(df) > 0) {
        metric_col <- if (input$metric == "persons") "n_persons" else "n_records"
        if (metric_col %in% names(df)) {
          df <- df[!is.na(df[[metric_col]]), , drop = FALSE]
        }
      }
      prevalence_data(df)
    }, ignoreInit = TRUE)

    output$results_title <- shiny::renderText({
      tbl <- input$table
      if (is.null(tbl) || nchar(tbl) == 0) return("Concepts")
      paste("Concepts in", tbl)
    })

    output$results_content <- shiny::renderUI({
      df <- prevalence_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(.empty_state_ui("chart-simple", "No concepts found",
          "Select a table and run the query."))
      }
      shiny::tagList(
        shiny::p(class = "text-muted small mb-2",
          shiny::icon("hand-pointer"),
          " Tick the checkbox in the first column to select concepts, then click +Extract or Filter."),
        DT::DTOutput(ns("results_dt"))
      )
    })

    output$results_dt <- DT::renderDT({
      df <- prevalence_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      # Add a drilldown link column
      links <- vapply(seq_len(nrow(df)), function(i) {
        as.character(shiny::actionLink(
          ns(paste0("drill_", i)), df$concept_name[i],
          class = "dt-drill-link"))
      }, character(1))
      # Add checkbox column
      chk <- vapply(seq_len(nrow(df)), function(i) {
        paste0('<input type="checkbox" class="dt-row-chk" data-row="', i, '">')
      }, character(1))
      display <- data.frame(sel = chk, df, stringsAsFactors = FALSE)
      display$concept_name <- links
      DT::datatable(display,
        options = list(pageLength = 20, dom = "ftip", scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0,
                 orderable = FALSE, width = "30px")
          )),
        rownames = FALSE, selection = "none", escape = FALSE,
        callback = DT::JS(paste0(
          # Drill link click -> navigate
          "table.on('click', '.dt-drill-link', function(e) {",
          "  e.stopPropagation();",
          "  Shiny.setInputValue('", ns("drill_row"), "',",
          "    table.row($(this).closest('tr')).index() + 1,",
          "    {priority: 'event'});",
          "});",
          # Checkbox change -> track selected rows
          "var sel = [];",
          "table.on('change', '.dt-row-chk', function() {",
          "  var row = parseInt($(this).data('row'));",
          "  if (this.checked) { if (sel.indexOf(row)===-1) sel.push(row); }",
          "  else { sel = sel.filter(function(r){return r!==row;}); }",
          "  Shiny.setInputValue('", ns("chk_selected"), "', sel, {priority:'event'});",
          "});",
          # Select-all header checkbox
          "var hdr = $('<input type=\"checkbox\" class=\"dt-chk-all\">');",
          "$(table.column(0).header()).empty().append(hdr)",
          "  .append('<span class=\"ms-1 small text-muted\">Select</span>');",
          "hdr.on('change', function() {",
          "  var checked = this.checked;",
          "  sel = [];",
          "  table.rows({search:'applied'}).every(function() {",
          "    var cb = $(this.node()).find('.dt-row-chk');",
          "    cb.prop('checked', checked);",
          "    if (checked) sel.push(parseInt(cb.data('row')));",
          "  });",
          "  Shiny.setInputValue('", ns("chk_selected"), "', sel, {priority:'event'});",
          "});"
        ))
      )
    })

    # Drill link click -> navigate to Drilldown (does NOT affect checkbox selection)
    shiny::observeEvent(input$drill_row, {
      df <- prevalence_data()
      idx <- input$drill_row
      if (!is.null(idx) && !is.null(df) && idx <= nrow(df)) {
        state$selected_table <- input$table
        state$selected_concept_col <- input$concept_col
        state$selected_concept_id <- as.integer(df$concept_id[idx])
        state$selected_concept_name <- as.character(
          df$concept_name[idx] %||% ""
        )
        bslib::nav_select("explore_nav", "Drilldown",
                           session = parent_session)
      }
    })

    # Bulk add selected concepts to set
    shiny::observeEvent(input$add_selected_btn, {
      df <- prevalence_data()
      idx <- input$chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      new_ids <- as.integer(df$concept_id[idx])
      current <- state$concept_set
      added <- setdiff(new_ids, current)
      if (length(added) > 0) {
        state$concept_set <- c(current, added)
        shiny::showNotification(
          paste("Added", length(added), "concept(s) to set"),
          type = "message", duration = 2)
      }
    })

    # +Extract: add selected concepts as variables to recipe
    shiny::observeEvent(input$extract_selected_btn, {
      df <- prevalence_data()
      idx <- input$chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      tbl <- input$table
      added <- 0L
      errors <- character(0)
      for (i in idx) {
        cid <- as.integer(df$concept_id[i])
        cname <- if ("concept_name" %in% names(df))
          as.character(df$concept_name[i]) else NULL
        tryCatch({
          v <- omop_variable(
            table = tbl, concept_id = cid,
            concept_name = cname, format = "raw"
          )
          state$recipe <- recipe_add_variable(state$recipe, v)
          added <- added + 1L
        }, error = function(e) {
          errors <<- c(errors, .clean_ds_error(e))
        })
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "variable(s) to recipe"),
          type = "message", duration = 2)
      }
      for (msg in unique(errors)) {
        shiny::showNotification(msg, type = "error")
      }
    })

    # +Filter: add selected concepts as has_concept filters to recipe
    shiny::observeEvent(input$filter_selected_btn, {
      df <- prevalence_data()
      idx <- input$chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      tbl <- input$table
      added <- 0L
      errors <- character(0)
      for (i in idx) {
        cid <- as.integer(df$concept_id[i])
        cname <- if ("concept_name" %in% names(df))
          as.character(df$concept_name[i]) else NULL
        tryCatch({
          f <- omop_filter_has_concept(cid, tbl, cname)
          state$recipe <- recipe_add_filter(state$recipe, f)
          added <- added + 1L
        }, error = function(e) {
          errors <<- c(errors, .clean_ds_error(e))
        })
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "filter(s) to recipe"),
          type = "message", duration = 2)
      }
      for (msg in unique(errors)) {
        shiny::showNotification(msg, type = "error")
      }
    })

    # Scope info display
    output$scope_info <- shiny::renderUI({
      res <- last_result()
      if (is.null(res) || !inherits(res, "dsomop_result")) return(NULL)

      srv_badges <- lapply(res$meta$servers, function(srv) {
        shiny::span(class = "badge bg-light text-dark me-1", srv)
      })
      scope_text <- switch(state$scope %||% res$meta$scope,
        "pooled" = "Pooled", "all" = "All Servers", "Per-site")
      warns <- res$meta$warnings
      warn_ui <- if (length(warns) > 0) {
        shiny::div(class = "text-warning mt-1",
          shiny::icon("exclamation-triangle"),
          paste(warns, collapse = "; ")
        )
      }
      shiny::div(class = "scope-indicator mt-2",
        shiny::span(class = "text-muted me-1", paste0("Scope: ", scope_text, " |")),
        shiny::tagList(srv_badges),
        warn_ui
      )
    })
  })
}


# SUB-MODULE 2: Drilldown (was .mod_concept_drilldown)

.mod_explore_drilldown_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Breadcrumb navigation
    shiny::uiOutput(ns("breadcrumb")),
    bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Info", width = 260, open = "always",
      shiny::uiOutput(ns("concept_info")),
      shiny::hr(),
      shiny::actionButton(ns("add_to_set"), "Add to Concept Set",
                          class = "btn-outline-primary btn-sm w-100 mb-1"),
      shiny::uiOutput(ns("recipe_actions")),
      shiny::actionButton(ns("reload"), "Reload",
                          icon = shiny::icon("rotate-right"),
                          class = "btn-outline-secondary btn-sm w-100")
    ),

    # Summary metrics
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Summary"),
      bslib::card_body(
        shiny::uiOutput(ns("summary_metrics"))
      )
    ),

    # Numeric distribution (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_numeric"), "']"),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Numeric Distribution"),
        bslib::card_body(
          plotly::plotlyOutput(ns("histogram_plot"), height = "300px"),
          DT::DTOutput(ns("quantiles_dt"))
        )
      )
    ),

    # Categorical values (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_categorical"), "']"),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Categorical Values"),
        bslib::card_body(
          plotly::plotlyOutput(ns("categorical_plot"), height = "300px"),
          DT::DTOutput(ns("categorical_dt"))
        )
      )
    ),

    # Date coverage (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_dates"), "']"),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Date Coverage"),
        bslib::card_body(
          plotly::plotlyOutput(ns("date_plot"), height = "250px"),
          shiny::uiOutput(ns("date_range_info"))
        )
      )
    ),

    # Missingness (always shown)
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Missingness"),
      bslib::card_body(
        shiny::uiOutput(ns("missingness_content"))
      )
    )
  ))
}

.mod_explore_drilldown_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    drilldown_data <- shiny::reactiveVal(NULL)
    raw_drilldown <- shiny::reactiveVal(NULL)

    # Breadcrumb
    output$breadcrumb <- shiny::renderUI({
      cname <- state$selected_concept_name
      if (is.null(state$selected_concept_id)) return(NULL)
      label <- if (!is.null(cname) && nchar(cname) > 0) cname
               else paste("Concept", state$selected_concept_id)
      shiny::div(class = "breadcrumb-nav",
        shiny::actionLink(session$ns("back_to_prevalence"), "Prevalence"),
        shiny::span(" > Drilldown: "),
        shiny::strong(label)
      )
    })

    # Navigate back to Prevalence tab
    shiny::observeEvent(input$back_to_prevalence, {
      if (!is.null(parent_session)) {
        bslib::nav_select("explore_nav", "Prevalence",
                          session = parent_session)
      }
    })

    # Auto-trigger when concept changes
    shiny::observeEvent(state$selected_concept_id, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()
      .run_drilldown(state, drilldown_data, raw_drilldown, input)
    })

    shiny::observeEvent(input$reload, {
      .run_drilldown(state, drilldown_data, raw_drilldown, input)
    })

    # Re-extract on server/intersect change
    shiny::observeEvent(list(state$selected_servers, state$scope), {
      res <- raw_drilldown()
      if (is.null(res)) return()
      drilldown_data(.drilldown_pick_server(res, state$selected_servers))
    }, ignoreInit = TRUE)

    .run_drilldown <- function(state, drilldown_data, raw_drilldown, input) {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()

      shiny::withProgress(message = "Loading drilldown...", value = 0.3, {
        tryCatch({
          res <- ds.omop.concept.drilldown(
            table = tbl, concept_id = cid,
            concept_col = state$selected_concept_col,
            symbol = state$symbol
          )
          shiny::incProgress(0.5)
          # Accumulate code for Script tab
          if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, res$meta$call_code)
          }
          # Store full result for server switching
          raw_drilldown(res$per_site)
          drilldown_data(.drilldown_pick_server(res$per_site, state$selected_servers))
        }, error = function(e) {
          shiny::showNotification(.clean_ds_error(e), type = "error")
          drilldown_data(NULL)
          raw_drilldown(NULL)
        })
      })
    }

    # Concept info sidebar
    output$concept_info <- shiny::renderUI({
      cid <- state$selected_concept_id
      if (is.null(cid)) {
        return(shiny::p(class = "text-muted small",
          "No concept selected. Use the Prevalence tab to select a concept."))
      }
      cname <- as.character(state$selected_concept_name %||% "Unknown")
      tbl <- as.character(state$selected_table %||% "")
      shiny::div(
        shiny::p(class = "mb-1", style = "font-weight:600; font-size:0.92rem; line-height:1.3; word-wrap:break-word; overflow-wrap:break-word;",
          cname),
        shiny::div(class = "d-flex align-items-center gap-2 flex-wrap",
          shiny::span(class = "badge bg-light text-dark",
                      style = "font-size:0.78rem;", paste0("#", cid)),
          shiny::span(class = "text-muted", style = "font-size:0.78rem;",
            tbl)
        )
      )
    })

    # Recipe/plan action buttons: enabled only when a concept is selected
    output$recipe_actions <- shiny::renderUI({
      has_concept <- !is.null(state$selected_concept_id) &&
        !is.null(state$selected_table)
      disabled_attr <- if (has_concept) NULL else "disabled"
      shiny::tagList(
        shiny::actionButton(session$ns("add_to_recipe_extract"),
                            shiny::tagList(shiny::icon("plus"), "Extract to Builder"),
                            class = "btn-success text-white btn-sm w-100 mb-1",
                            disabled = disabled_attr),
        shiny::actionButton(session$ns("add_to_recipe_filter"),
                            shiny::tagList(shiny::icon("filter"), "Filter in Builder"),
                            class = "btn-warning text-white btn-sm w-100 mb-1",
                            disabled = disabled_attr),
        shiny::actionButton(session$ns("add_to_plan"), "Add to Plan",
                            class = "btn-outline-info btn-sm w-100 mb-1",
                            disabled = disabled_attr),
        if (!has_concept) shiny::p(class = "text-muted small mb-1",
          "Drill into a concept from Prevalence first.")
      )
    })

    # Add to concept set
    shiny::observeEvent(input$add_to_set, {
      cid <- state$selected_concept_id
      if (is.null(cid)) return()
      current <- state$concept_set
      if (!cid %in% current) {
        state$concept_set <- c(current, cid)
        shiny::showNotification(
          paste("Added concept", cid, "to set"),
          type = "message", duration = 2
        )
      }
    })

    # Add to recipe as extraction variable
    shiny::observeEvent(input$add_to_recipe_extract, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) {
        shiny::showNotification("No concept selected.", type = "warning")
        return()
      }
      cname <- state$selected_concept_name
      tryCatch({
        v <- omop_variable(
          table = tbl, concept_id = cid,
          concept_name = cname, format = "raw"
        )
        state$recipe <- recipe_add_variable(state$recipe, v)
        shiny::showNotification(
          paste("Added", v$name, "to recipe"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add to recipe as filter
    shiny::observeEvent(input$add_to_recipe_filter, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) {
        shiny::showNotification("No concept selected.", type = "warning")
        return()
      }
      cname <- state$selected_concept_name
      tryCatch({
        f <- omop_filter_has_concept(cid, tbl, cname)
        state$recipe <- recipe_add_filter(state$recipe, f)
        shiny::showNotification(
          paste("Added filter for concept", cid, "to recipe"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add to plan as events output
    shiny::observeEvent(input$add_to_plan, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()
      tryCatch({
        nm <- paste0("events_", cid)
        state$plan <- ds.omop.plan.events(
          state$plan, name = nm, table = tbl,
          concept_set = as.integer(cid)
        )
        shiny::showNotification(
          paste("Added events output for concept", cid),
          type = "message", duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error"
        )
      })
    })

    # Conditional panel outputs (must be text for JS condition)
    output$has_numeric <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$numeric_summary)
    })
    shiny::outputOptions(output, "has_numeric", suspendWhenHidden = FALSE)

    output$has_categorical <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$categorical_values) && is.data.frame(d$categorical_values) &&
        nrow(d$categorical_values) > 0
    })
    shiny::outputOptions(output, "has_categorical", suspendWhenHidden = FALSE)

    output$has_dates <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$date_range)
    })
    shiny::outputOptions(output, "has_dates", suspendWhenHidden = FALSE)

    # --- Summary metrics ---
    output$summary_metrics <- shiny::renderUI({
      dd <- drilldown_data()
      if (is.null(dd)) {
        return(.empty_state_ui("magnifying-glass-chart", "No concept selected",
                               "Use the Prevalence tab to select a concept for drilldown."))
      }

      # If multiple servers shown, add a server comparison row
      server_comparison <- NULL
      if (length(dd) > 1) {
        srv_rows <- lapply(names(dd), function(srv) {
          s <- dd[[srv]]$summary
          rec <- if (is.null(s$n_records) || is.na(s$n_records))
            "suppressed" else format(s$n_records, big.mark = ",")
          pers <- if (is.null(s$n_persons) || is.na(s$n_persons))
            "suppressed" else format(s$n_persons, big.mark = ",")
          shiny::tags$tr(
            shiny::tags$td(style = "font-weight:500;", srv),
            shiny::tags$td(class = "text-end", rec),
            shiny::tags$td(class = "text-end", pers)
          )
        })
        server_comparison <- shiny::div(class = "mb-3",
          shiny::tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.84rem;",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th(class = "text-muted", "Server"),
                shiny::tags$th(class = "text-muted text-end", "Records"),
                shiny::tags$th(class = "text-muted text-end", "Persons")
              )
            ),
            shiny::tags$tbody(shiny::tagList(srv_rows))
          )
        )
      }

      srv <- names(dd)[1]
      d <- dd[[srv]]
      s <- d$summary

      metrics_list <- list(
        bslib::value_box(
          title = "Records",
          value = .fmt_count(s$n_records),
          showcase = fontawesome::fa_i("database"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Persons",
          value = .fmt_count(s$n_persons),
          showcase = fontawesome::fa_i("users"),
          theme = "info"
        )
      )

      if (!is.null(s$records_per_person_mean) && !is.na(s$records_per_person_mean)) {
        metrics_list <- c(metrics_list, list(
          bslib::value_box(
            title = "Records/Person",
            value = format(round(s$records_per_person_mean, 2), nsmall = 2),
            showcase = fontawesome::fa_i("chart-line"),
            theme = "secondary"
          )
        ))
      }

      if (!is.null(s$pct_persons_multi) && !is.na(s$pct_persons_multi)) {
        # Longitudinal indicator
        longi_level <- if (s$pct_persons_multi >= 50) "High"
                       else if (s$pct_persons_multi >= 20) "Medium"
                       else "Low"
        longi_theme <- if (longi_level == "High") "success"
                       else if (longi_level == "Medium") "warning"
                       else "secondary"
        metrics_list <- c(metrics_list, list(
          bslib::value_box(
            title = "Multi-record %",
            value = paste0(format(s$pct_persons_multi, nsmall = 1), "%"),
            showcase = fontawesome::fa_i("layer-group"),
            theme = "secondary"
          ),
          bslib::value_box(
            title = "Longitudinal",
            value = longi_level,
            showcase = fontawesome::fa_i("timeline"),
            theme = longi_theme
          )
        ))
      }

      shiny::tagList(
        server_comparison,
        bslib::layout_columns(fill = FALSE, !!!metrics_list)
      )
    })

    # --- Numeric histogram ---
    output$histogram_plot <- plotly::renderPlotly({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$numeric_summary) || is.null(d$numeric_summary$histogram)) {
        return(NULL)
      }
      df <- d$numeric_summary$histogram
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plotly({
        cols <- ifelse(is.na(df$count) | df$suppressed, .studio_colors[4], .studio_colors[1])
        y <- df$count; y[is.na(y)] <- 0
        mids <- (df$bin_start + df$bin_end) / 2
        plotly::plot_ly(x = round(mids, 1), y = y, type = "bar",
                        marker = list(color = cols)) |>
          plotly::layout(xaxis = list(title = "value_as_number"),
                         yaxis = list(title = "Count")) |>
          .plotly_defaults()
      })
    })

    output$quantiles_dt <- DT::renderDT({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$numeric_summary) ||
          is.null(d$numeric_summary$quantiles)) return(NULL)
      df <- d$numeric_summary$quantiles
      if (!is.data.frame(df)) return(NULL)
      .explore_dt(df, page_length = 10L, selection = "none")
    })

    # --- Categorical values ---
    output$categorical_plot <- plotly::renderPlotly({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plotly({
        df$label <- substr(as.character(df$concept_name), 1, 30)
        df$y <- as.numeric(df$n)
        df <- df[!is.na(df$y), , drop = FALSE]
        if (nrow(df) == 0) return(NULL)

        df <- df[order(df$y, decreasing = TRUE), ]
        n <- min(nrow(df), 15)
        df <- df[seq_len(n), ]

        plotly::plot_ly(x = df$y, y = df$label, type = "bar", orientation = "h",
                        marker = list(color = .studio_colors[2])) |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults()
      })
    })

    output$categorical_dt <- DT::renderDT({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      val <- if ("n" %in% names(df)) "n" else NULL
      .explore_dt(df, default_sort = val, page_length = 10L, selection = "none")
    })

    # --- Date coverage ---
    output$date_plot <- plotly::renderPlotly({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$date_range) || is.null(d$date_range$date_counts)) {
        return(NULL)
      }
      df <- d$date_range$date_counts
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plotly({
        df <- df[order(df$period), ]
        y <- as.numeric(df$n_records); y[is.na(y)] <- 0
        sup_col <- if ("suppressed" %in% names(df)) df$suppressed else rep(FALSE, nrow(df))
        cols <- ifelse(is.na(df$n_records) | sup_col,
                       .studio_colors[4], .studio_colors[1])
        plotly::plot_ly(x = df$period, y = y, type = "bar",
                        marker = list(color = cols)) |>
          plotly::layout(xaxis = list(title = "Period"),
                         yaxis = list(title = "Records")) |>
          .plotly_defaults()
      })
    })

    output$date_range_info <- shiny::renderUI({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      dr <- d$date_range
      if (is.null(dr)) return(NULL)

      shiny::tags$dl(class = "row",
        shiny::tags$dt(class = "col-sm-4", "Date Column"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$column %||% "")),
        shiny::tags$dt(class = "col-sm-4", "Safe Min (month)"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$min_month_safe %||% dr$min_date_safe %||% "N/A")),
        shiny::tags$dt(class = "col-sm-4", "Safe Max (month)"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$max_month_safe %||% dr$max_date_safe %||% "N/A"))
      )
    })

    # --- Missingness ---
    output$missingness_content <- shiny::renderUI({
      dd <- drilldown_data()
      if (is.null(dd)) {
        return(.empty_state_ui("chart-bar", "No missingness data",
          "Select a concept from the Prevalence tab to view column-level missingness."))
      }
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$missingness
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(.empty_state_ui("chart-bar", "No missingness data",
          "Missingness analysis is not available for this concept."))
      }
      plotly::plotlyOutput(session$ns("missingness_plot"), height = "250px")
    })

    output$missingness_plot <- plotly::renderPlotly({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$missingness
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plotly({
        df <- df[order(df$missing_rate, decreasing = TRUE), ]
        plotly::plot_ly(x = df$missing_rate * 100, y = df$column_name,
                        type = "bar", orientation = "h",
                        marker = list(color = .studio_colors[3])) |>
          plotly::layout(xaxis = list(title = "Missing %", range = c(0, 100)),
                         yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults()
      })
    })
  })
}


# SUB-MODULE 3: Locator (was .mod_concept_locator)

.mod_explore_locator_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center py-2",
        shiny::span("Concept Locator"),
        shiny::div(
          bslib::tooltip(
            shiny::actionButton(ns("extract_to_recipe"),
                                shiny::tagList(shiny::icon("plus"), "Extract to recipe"),
                                class = "btn-sm btn-success text-white me-1"),
            "Add the selected concepts as variables to Builder"
          ),
          shiny::actionButton(ns("locate_btn"), "Locate",
                              icon = shiny::icon("location-dot"),
                              class = "btn-sm btn-primary")
        )
      ),
      bslib::card_body(
        class = "py-2 px-3",
        shiny::div(class = "input-group mb-2",
          shiny::tags$input(type = "text", id = ns("unified_search"),
            class = "form-control",
            placeholder = "Search by name or enter concept IDs (comma-separated)"),
          shiny::actionButton(ns("name_search_btn"), "Search",
            icon = shiny::icon("magnifying-glass"),
            class = "btn btn-primary")
        ),
        shiny::uiOutput(ns("search_content")),
        shiny::uiOutput(ns("selected_badges"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Presence Matrix"),
      bslib::card_body(
        shiny::uiOutput(ns("presence_content"))
      )
    )
  )
}

.mod_explore_locator_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    locate_data <- shiny::reactiveVal(NULL)
    search_results <- shiny::reactiveVal(NULL)
    raw_locate_result <- shiny::reactiveVal(NULL)
    selected_ids <- shiny::reactiveVal(list())

    # Unified search: detect numeric IDs vs text search
    shiny::observeEvent(input$name_search_btn, {
      term <- trimws(input$unified_search %||% "")
      if (nchar(term) == 0) return()

      # Check if input looks like numeric IDs
      tokens <- trimws(strsplit(term, "[,\\s]+")[[1]])
      numeric_tokens <- suppressWarnings(as.integer(tokens))
      all_numeric <- all(!is.na(numeric_tokens)) && length(numeric_tokens) > 0

      if (all_numeric) {
        # Add directly to selected_ids
        current <- selected_ids()
        existing_cids <- vapply(current, function(x) x$concept_id, integer(1))
        for (cid in numeric_tokens) {
          if (!cid %in% existing_cids) {
            current <- c(current, list(list(
              concept_id = as.integer(cid),
              concept_name = paste0("Concept ", cid)
            )))
          }
        }
        selected_ids(current)
        shiny::updateTextInput(session, "unified_search", value = "")
      } else {
        # Text search
        tryCatch({
          res <- ds.omop.concept.search(
            pattern = term, standard_only = TRUE,
            limit = 20, symbol = state$symbol
          )
          srv <- names(res$per_site)[1]
          search_results(res$per_site[[srv]])
        }, error = function(e) {
          shiny::showNotification(
            paste("Search error:", conditionMessage(e)),
            type = "error"
          )
        })
      }
    })

    output$search_content <- shiny::renderUI({
      df <- search_results()
      items <- selected_ids()
      if (is.null(df) && length(items) == 0) {
        return(shiny::p(class = "text-muted small mt-2",
          "Search for concepts by name, or enter concept IDs directly.",
          " Click a search result to add it, then press Locate."))
      }
      if (is.data.frame(df) && nrow(df) == 0) {
        return(.empty_state_ui("magnifying-glass", "No results found",
          "Try a different search term or check the concept name."))
      }
      DT::DTOutput(ns("search_results_dt"))
    })

    output$search_results_dt <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      # Show available columns (prefer concept_id, concept_name, domain_id)
      preferred <- c("concept_id", "concept_name", "domain_id")
      keep <- intersect(preferred, names(df))
      if (length(keep) == 0) keep <- names(df)
      DT::datatable(
        df[, keep, drop = FALSE],
        options = list(pageLength = 5, dom = "t", scrollX = TRUE,
                       scrollY = "200px"),
        rownames = FALSE, selection = "single"
      )
    })

    # Click DT row -> add to selected_ids
    shiny::observeEvent(input$search_results_dt_rows_selected, {
      idx <- input$search_results_dt_rows_selected
      df <- search_results()
      if (is.null(idx) || is.null(df) || length(idx) == 0) return()
      idx <- idx[1]
      if (idx > nrow(df)) return()
      # Safely extract concept_id and concept_name
      cid <- if ("concept_id" %in% names(df)) {
        as.integer(df$concept_id[idx])
      } else {
        # Try first numeric column as ID
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        if (length(num_cols) > 0) as.integer(df[[num_cols[1]]][idx]) else idx
      }
      cname <- if ("concept_name" %in% names(df)) {
        as.character(df$concept_name[idx])
      } else {
        # Try first character column
        char_cols <- names(df)[vapply(df, is.character, logical(1))]
        if (length(char_cols) > 0) as.character(df[[char_cols[1]]][idx])
        else paste0("Concept ", cid)
      }
      current <- selected_ids()
      existing_cids <- if (length(current) > 0) {
        vapply(current, function(x) x$concept_id, integer(1))
      } else integer(0)
      if (!cid %in% existing_cids) {
        current <- c(current, list(list(concept_id = cid, concept_name = cname)))
        selected_ids(current)
      }
    })

    # Badge removal via JS delegation
    shiny::observeEvent(input$remove_concept, {
      cid <- as.integer(input$remove_concept)
      current <- selected_ids()
      current <- Filter(function(x) x$concept_id != cid, current)
      selected_ids(current)
    })

    # Render selected concept badges
    output$selected_badges <- shiny::renderUI({
      items <- selected_ids()
      if (length(items) == 0) return(NULL)
      badges <- lapply(items, function(item) {
        cid <- item$concept_id
        label <- if (nchar(item$concept_name) > 25)
          paste0(substr(item$concept_name, 1, 22), "...") else item$concept_name
        rm_js <- sprintf(
          "Shiny.setInputValue('%s', %d, {priority:'event'})",
          ns("remove_concept"), cid)
        shiny::span(class = "badge bg-primary me-1 mb-1",
          style = "font-size: 0.8rem; cursor: default;",
          paste0("#", cid, " ", label),
          shiny::tags$span(
            class = "ms-1", style = "cursor: pointer; opacity: 0.8;",
            onclick = rm_js,
            shiny::HTML("&times;")
          )
        )
      })
      shiny::div(class = "d-flex flex-wrap gap-1 mt-2", shiny::tagList(badges))
    })

    shiny::observeEvent(input$locate_btn, {
      items <- selected_ids()
      ids <- vapply(items, function(x) x$concept_id, integer(1))
      if (length(ids) == 0) {
        shiny::showNotification("Add at least one concept above.",
                                type = "warning")
        return()
      }

      shiny::withProgress(message = "Locating concepts...", value = 0.3, {
        tryCatch({
          res <- ds.omop.concept.locate(
            concept_ids = ids, symbol = state$symbol
          )
          shiny::incProgress(0.5)
          if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, res$meta$call_code)
          }
          raw_locate_result(res)
          scope <- state$scope %||% "pooled"
          df <- .locator_extract(res, scope, state$selected_servers,
                                  intersect_only = FALSE)
          locate_data(df)
        }, error = function(e) {
          shiny::showNotification(
            .clean_ds_error(e), type = "error"
          )
        })
      })
    })

    # +Extract: add selected concepts as variables to recipe
    shiny::observeEvent(input$extract_to_recipe, {
      items <- selected_ids()
      if (length(items) == 0) {
        shiny::showNotification("Add at least one concept above.",
                                type = "warning")
        return()
      }
      # Use the presence matrix (if located) to map concept_id -> table.
      loc <- locate_data()
      table_for <- function(cid) {
        if (is.data.frame(loc) && "concept_id" %in% names(loc) &&
            "table_name" %in% names(loc)) {
          hit <- loc$table_name[loc$concept_id == cid]
          hit <- hit[!is.na(hit) & nchar(as.character(hit)) > 0]
          if (length(hit) > 0) return(as.character(hit[1]))
        }
        NULL
      }
      added <- 0L
      errors <- character(0)
      skipped <- 0L
      for (item in items) {
        cid <- as.integer(item$concept_id)
        tbl <- table_for(cid)
        if (is.null(tbl)) {
          skipped <- skipped + 1L
          next
        }
        cname <- item$concept_name
        tryCatch({
          v <- omop_variable(
            table = tbl, concept_id = cid,
            concept_name = cname, format = "raw"
          )
          state$recipe <- recipe_add_variable(state$recipe, v)
          added <- added + 1L
        }, error = function(e) {
          errors <<- c(errors, .clean_ds_error(e))
        })
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "variable(s) to recipe"),
          type = "message", duration = 2)
      }
      if (skipped > 0) {
        shiny::showNotification(
          paste(skipped, "concept(s) skipped: no table located.",
                "Click Locate first to map concepts to tables."),
          type = "warning")
      }
      for (msg in unique(errors)) {
        shiny::showNotification(msg, type = "error")
      }
    })

    # Re-extract on scope/server change
    shiny::observeEvent(list(state$scope, state$selected_servers), {
      res <- raw_locate_result()
      if (is.null(res)) return()
      scope <- state$scope %||% "pooled"
      df <- .locator_extract(res, scope, state$selected_servers,
                              intersect_only = FALSE)
      locate_data(df)
    }, ignoreInit = TRUE)

    output$presence_content <- shiny::renderUI({
      df <- locate_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(.empty_state_ui("table-cells-large", "No presence data",
          "Add concepts above and click the locate button to see where they appear."))
      }
      DT::DTOutput(ns("presence_dt"))
    })

    output$presence_dt <- DT::renderDT({
      df <- locate_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      val <- if ("n_persons" %in% names(df)) "n_persons"
             else if ("n_records" %in% names(df)) "n_records" else NULL
      .explore_dt(df, default_sort = val, page_length = 25L, selection = "none")
    })
  })
}


# SUB-MODULE 4: Vocabulary (was .mod_vocab)

.mod_explore_vocab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Search", width = 260, open = "always",
      shiny::textInput(ns("search_pattern"), "Search",
                       placeholder = "e.g. diabetes"),
      shiny::selectInput(ns("domain_filter"), "Domain",
        choices = c("All" = "", "Condition", "Drug", "Measurement",
                    "Observation", "Procedure", "Visit"),
        selected = ""),
      shiny::checkboxInput(ns("standard_only"), "Standard only", TRUE),
      shiny::numericInput(ns("limit"), "Max results", 50, 10, 500, 10),
      shiny::actionButton(ns("search_btn"), "Search",
                          icon = shiny::icon("magnifying-glass"),
                          class = "btn-primary w-100"),
      shiny::h6("Concept Set"),
      shiny::verbatimTextOutput(ns("concept_set_ids")),
      shiny::actionButton(ns("clear_set"), "Clear Set",
                          class = "btn-sm btn-outline-danger")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          "Search Results",
          shiny::div(
            shiny::actionButton(ns("add_selected_vocab"), "Add to Set",
                                class = "btn-sm btn-outline-primary me-1"),
            bslib::tooltip(
              shiny::actionButton(ns("extract_selected_vocab"),
                                  shiny::tagList(shiny::icon("plus"), "Extract"),
                                  class = "btn-sm btn-success text-white"),
              "Add selected concepts as variables to Builder"
            )
          )
        )
      ),
      bslib::card_body(
        shiny::div(class = "d-flex justify-content-end mb-2",
          shiny::div(class = "d-inline-flex align-items-center",
            style = "transform: scale(0.85); transform-origin: right center;",
            shiny::checkboxInput(ns("group_concepts"), "Group identical concepts",
                                 value = TRUE, width = "auto")
          )
        ),
        shiny::uiOutput(ns("results_table_content"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Concept Details"),
      bslib::card_body(
        shiny::uiOutput(ns("concept_detail"))
      )
    )
  )
}

.mod_explore_vocab_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    search_results <- shiny::reactiveVal(NULL)
    raw_search_result <- shiny::reactiveVal(NULL)
    has_searched <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$search_btn, {
      shiny::req(nchar(input$search_pattern) > 0)
      has_searched(TRUE)
      tryCatch({
        domain <- if (nchar(input$domain_filter) > 0)
          input$domain_filter else NULL
        res <- ds.omop.concept.search(
          pattern = input$search_pattern,
          domain = domain,
          standard_only = input$standard_only,
          limit = input$limit,
          symbol = state$symbol
        )
        # Accumulate code for Script tab
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        raw_search_result(res)
        search_results(.extract_display_data(res, "all",
          state$selected_servers,
          intersect_only = FALSE))
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error"
        )
      })
    })

    # Re-extract on server/intersect change
    shiny::observeEvent(list(state$selected_servers, state$scope), {
      res <- raw_search_result()
      if (is.null(res)) return()
      search_results(.extract_display_data(res, "all",
        state$selected_servers,
        intersect_only = FALSE))
    }, ignoreInit = TRUE)

    output$results_table_content <- shiny::renderUI({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        if (isTRUE(has_searched())) {
          return(.empty_state_ui("magnifying-glass", "No results found",
            "No concepts matched your search criteria. Try different terms or filters."))
        }
        return(.empty_state_ui("book", "No results",
          "Search for concepts using the sidebar controls."))
      }
      shiny::tagList(
        shiny::p(class = "text-muted small mb-2",
          shiny::icon("hand-pointer"),
          " Tick the checkbox in the first column to select concepts, then click +Extract."),
        DT::DTOutput(ns("results_table"))
      )
    })

    output$results_table <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      if (isTRUE(input$group_concepts) && "server" %in% names(df) &&
          "concept_id" %in% names(df)) {
        # Group identical concept_ids: replace server with count of servers
        grp <- stats::aggregate(
          server ~ concept_id, data = df,
          FUN = function(x) length(unique(x))
        )
        names(grp)[names(grp) == "server"] <- "n_servers"
        # Keep first row per concept_id for metadata
        dedup <- df[!duplicated(df$concept_id), , drop = FALSE]
        dedup$server <- NULL
        df <- merge(dedup, grp, by = "concept_id", sort = FALSE)
      }
      # Show all meaningful columns (exclude internal/redundant ones)
      exclude <- c("concept_code")
      keep <- setdiff(names(df), exclude)
      display <- df[, keep, drop = FALSE]
      chk <- vapply(seq_len(nrow(display)), function(i) {
        paste0('<input type="checkbox" class="dt-row-chk" data-row="', i, '">')
      }, character(1))
      display <- data.frame(sel = chk, display, stringsAsFactors = FALSE)
      DT::datatable(
        display,
        options = list(pageLength = 15, dom = "ftip", scrollX = TRUE,
          columnDefs = list(
            list(className = "dt-center", targets = 0,
                 orderable = FALSE, width = "30px")
          )),
        rownames = FALSE, selection = "none", escape = FALSE,
        callback = DT::JS(paste0(
          "var sel = [];",
          "table.on('change', '.dt-row-chk', function() {",
          "  var row = parseInt($(this).data('row'));",
          "  if (this.checked) { if (sel.indexOf(row)===-1) sel.push(row); }",
          "  else { sel = sel.filter(function(r){return r!==row;}); }",
          "  Shiny.setInputValue('", ns("vocab_chk_selected"), "', sel, {priority:'event'});",
          "});",
          "var hdr = $('<input type=\"checkbox\" class=\"dt-chk-all\">');",
          "$(table.column(0).header()).empty().append(hdr)",
          "  .append('<span class=\"ms-1 small text-muted\">Select</span>');",
          "hdr.on('change', function() {",
          "  var checked = this.checked;",
          "  sel = [];",
          "  table.rows({search:'applied'}).every(function() {",
          "    var cb = $(this.node()).find('.dt-row-chk');",
          "    cb.prop('checked', checked);",
          "    if (checked) sel.push(parseInt(cb.data('row')));",
          "  });",
          "  Shiny.setInputValue('", ns("vocab_chk_selected"), "', sel, {priority:'event'});",
          "});"
        ))
      )
    })

    # Bulk add selected concepts to set
    shiny::observeEvent(input$add_selected_vocab, {
      df <- search_results()
      idx <- input$vocab_chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      new_ids <- as.integer(df$concept_id[idx])
      current <- state$concept_set
      added <- setdiff(new_ids, current)
      if (length(added) > 0) {
        state$concept_set <- c(current, added)
        shiny::showNotification(
          paste("Added", length(added), "concept(s) to set"),
          type = "message", duration = 2)
      }
    })

    # +Extract: add selected concepts as variables to recipe
    shiny::observeEvent(input$extract_selected_vocab, {
      df <- search_results()
      idx <- input$vocab_chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      added <- 0L
      errors <- character(0)
      for (i in idx) {
        cid <- as.integer(df$concept_id[i])
        cname <- if ("concept_name" %in% names(df))
          as.character(df$concept_name[i]) else NULL
        domain <- if ("domain_id" %in% names(df))
          as.character(df$domain_id[i]) else NULL
        # Infer table from domain
        tbl <- .domain_to_table(domain)
        tryCatch({
          v <- omop_variable(
            table = tbl, concept_id = cid,
            concept_name = cname, format = "raw"
          )
          state$recipe <- recipe_add_variable(state$recipe, v)
          added <- added + 1L
        }, error = function(e) {
          errors <<- c(errors, .clean_ds_error(e))
        })
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "variable(s) to recipe"),
          type = "message", duration = 2)
      }
      for (msg in unique(errors)) {
        shiny::showNotification(msg, type = "error")
      }
    })

    shiny::observeEvent(input$clear_set, {
      state$concept_set <- integer(0)
    })

    output$concept_set_ids <- shiny::renderText({
      ids <- state$concept_set
      if (length(ids) == 0) return("(empty)")
      paste(ids, collapse = ", ")
    })

    output$concept_detail <- shiny::renderUI({
      df <- search_results()
      idx <- input$vocab_chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        return(.empty_state_ui("hand-pointer", "Select a concept",
                               "Tick a row in the search results to view details."))
      }
      # Show details for the first selected row
      first_idx <- idx[1]
      if (first_idx > nrow(df)) {
        return(.empty_state_ui("hand-pointer", "Select a concept",
                               "Tick a row in the search results to view details."))
      }
      row <- df[first_idx, , drop = FALSE]
      if (!is.data.frame(row) || nrow(row) == 0) {
        return(.empty_state_ui("hand-pointer", "Select a concept"))
      }
      # Safely extract fields (may not exist in all results)
      .safe_col <- function(r, col) {
        if (col %in% names(r)) as.character(r[[col]][1]) else ""
      }
      cid <- .safe_col(row, "concept_id")
      cname <- .safe_col(row, "concept_name")
      std <- .safe_col(row, "standard_concept")
      domain <- .safe_col(row, "domain_id")
      vocab <- .safe_col(row, "vocabulary_id")
      srv <- .safe_col(row, "server")

      std_badge <- if (std == "S") {
        shiny::span(class = "badge", style = "background:#065f46; color:#ecfdf5;", "Standard")
      } else if (nchar(std) > 0) {
        shiny::span(class = "badge bg-warning text-dark", std)
      }

      detail_rows <- list()
      if (nchar(domain) > 0) {
        detail_rows <- c(detail_rows, list(shiny::tags$tr(
          shiny::tags$td(class = "text-muted", style = "width:35%;", "Domain"),
          shiny::tags$td(class = "fw-semibold", domain)
        )))
      }
      if (nchar(vocab) > 0) {
        detail_rows <- c(detail_rows, list(shiny::tags$tr(
          shiny::tags$td(class = "text-muted", "Vocabulary"),
          shiny::tags$td(class = "fw-semibold", vocab)
        )))
      }
      if (nchar(srv) > 0) {
        detail_rows <- c(detail_rows, list(shiny::tags$tr(
          shiny::tags$td(class = "text-muted", "Server"),
          shiny::tags$td(shiny::span(class = "badge bg-light text-dark", srv))
        )))
      }

      shiny::div(
        shiny::div(class = "d-flex align-items-center gap-2 mb-2",
          if (nchar(cid) > 0) shiny::span(class = "badge bg-primary",
            style = "font-size:0.82rem;", paste0("#", cid)),
          std_badge
        ),
        if (nchar(cname) > 0) shiny::h6(class = "mb-2",
          style = "font-weight:600;", cname),
        if (length(detail_rows) > 0)
          shiny::tags$table(class = "table table-sm table-borderless mb-0",
            style = "font-size:0.84rem;",
            shiny::tags$tbody(shiny::tagList(detail_rows))
          )
      )
    })
  })
}


# Helper functions

# Helper: pick server(s) from drilldown per_site list
# selected_server: character vector of server names (NULL = all)
.drilldown_pick_server <- function(per_site, selected_server) {
  if (is.null(per_site)) return(NULL)
  srvs <- .resolve_servers(per_site, selected_server)
  if (length(srvs) == 0) return(per_site)
  per_site[srvs]
}

# Helper: extract locator data with scope support
# selected_server: character vector of server names (NULL = all)
.locator_extract <- function(res, scope, selected_server = NULL,
                              intersect_only = FALSE) {
  if (is.null(res)) return(NULL)
  per_site <- if (inherits(res, "dsomop_result")) res$per_site else res
  srvs <- .resolve_servers(per_site, selected_server)
  if (length(srvs) == 0) return(NULL)

  if (scope == "per_site") {
    srv <- srvs[1]
    if (srv %in% names(per_site)) return(per_site[[srv]])
    return(NULL)
  }

  if (scope == "pooled") {
    # Aggregate across selected servers
    all_df <- .locator_extract(res, "all", selected_server)
    if (is.null(all_df) || !is.data.frame(all_df) || nrow(all_df) == 0)
      return(NULL)
    group_cols <- intersect(c("table_name", "concept_column", "concept_id"),
                            names(all_df))
    if (length(group_cols) == 0) return(all_df)
    agg <- stats::aggregate(
      cbind(n_records, n_persons) ~ table_name + concept_column + concept_id,
      data = all_df, FUN = sum, na.rm = TRUE
    )
    return(agg)
  }

  # scope == "all" (default): rbind selected servers with server column
  dfs <- list()
  for (srv in srvs) {
    df <- per_site[[srv]]
    if (is.data.frame(df) && nrow(df) > 0) {
      df$server <- srv
      dfs[[srv]] <- df
    }
  }
  if (length(dfs) == 0) return(NULL)
  combined <- do.call(rbind, dfs)
  rownames(combined) <- NULL

  # Intersection filter
  if (isTRUE(intersect_only) && length(dfs) > 1 &&
      "concept_id" %in% names(combined)) {
    id_sets <- lapply(dfs, function(d) unique(d$concept_id))
    common_ids <- Reduce(intersect, id_sets)
    combined <- combined[combined$concept_id %in% common_ids, , drop = FALSE]
  }
  combined
}

# --- Consistent sort / visualization layer ---------------------------------
# Shared DT factory + chart-card toggle + typed plotly helpers. Every chart is
# built ONLY from the already-suppressed aggregate frame it is handed; nothing
# here imputes or back-fills suppressed cells.

#' DT factory: sortable table with default count-desc sort + copy/csv export
#'
#' @param df Data frame to display (already disclosure-suppressed).
#' @param default_sort Character column name to sort by, or NULL for no
#'   explicit order (DT then keeps row order).
#' @param decreasing Logical; sort direction for \code{default_sort}.
#' @param page_length Integer rows per page.
#' @param ... Extra args forwarded to \code{DT::datatable} (e.g.
#'   \code{callback=}, \code{escape=}, \code{selection=}).
#' @keywords internal
.explore_dt <- function(df, default_sort = NULL, decreasing = TRUE,
                        page_length = 15L, ...) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  order_opt <- NULL
  if (!is.null(default_sort) && default_sort %in% names(df)) {
    col_idx <- match(default_sort, names(df)) - 1L  # 0-based for DT
    order_opt <- list(list(col_idx, if (decreasing) "desc" else "asc"))
  }
  DT::datatable(
    df,
    extensions = "Buttons",
    options = list(
      pageLength = page_length,
      dom = "Bftip",
      scrollX = TRUE,
      buttons = list("copy", "csv"),
      order = order_opt
    ),
    rownames = FALSE,
    ...
  )
}

#' Chart/table card with a radio toggle between a plotly chart and a DT table
#' @keywords internal
.explore_chart_card <- function(ns, title, dt_id, plot_id) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::div(class = "d-flex justify-content-between align-items-center",
        shiny::span(title),
        shiny::radioButtons(ns(paste0(plot_id, "_view")), label = NULL,
          choices = c("Chart" = "chart", "Table" = "table"),
          selected = "chart", inline = TRUE)
      )
    ),
    bslib::card_body(
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'chart'", ns(paste0(plot_id, "_view"))),
        plotly::plotlyOutput(ns(plot_id), height = "320px")
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'table'", ns(paste0(plot_id, "_view"))),
        DT::DTOutput(ns(dt_id))
      )
    )
  )
}

#' Horizontal ranked bar chart (value vs count), top-N
#' @keywords internal
.explore_rank_plot <- function(df, label_col, value_col, top_n = 20L,
                               color = .studio_colors[1]) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(.plotly_no_data("No values to rank"))
  }
  .safe_plotly({
    d <- df
    d$.lab <- substr(as.character(d[[label_col]]), 1, 40)
    d$.val <- suppressWarnings(as.numeric(d[[value_col]]))
    d <- d[!is.na(d$.val), , drop = FALSE]
    if (nrow(d) == 0) return(.plotly_no_data("All values suppressed"))
    d <- d[order(d$.val, decreasing = TRUE), , drop = FALSE]
    d <- d[seq_len(min(nrow(d), top_n)), , drop = FALSE]
    plotly::plot_ly(x = d$.val, y = d$.lab, type = "bar", orientation = "h",
                    marker = list(color = color)) |>
      plotly::layout(xaxis = list(title = "Count"),
                     yaxis = list(title = "", categoryorder = "total ascending")) |>
      .plotly_defaults()
  })
}

#' Histogram bar chart; suppressed bins greyed (never imputed)
#' @keywords internal
.explore_hist_plot <- function(df, value_label = "value") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(.plotly_no_data("No distribution data"))
  }
  .safe_plotly({
    sup <- if ("suppressed" %in% names(df)) df$suppressed else rep(FALSE, nrow(df))
    cnt <- if ("count" %in% names(df)) df$count else df$n
    cols <- ifelse(is.na(cnt) | sup, .studio_colors[4], .studio_colors[1])
    y <- as.numeric(cnt); y[is.na(y)] <- 0
    mids <- (df$bin_start + df$bin_end) / 2
    plotly::plot_ly(x = round(mids, 2), y = y, type = "bar",
                    marker = list(color = cols)) |>
      plotly::layout(xaxis = list(title = value_label),
                     yaxis = list(title = "Count")) |>
      .plotly_defaults()
  })
}

#' Box plot reconstructed from precomputed percentiles (no raw points)
#' @param q Named numeric vector / list with p05,p25,p50,p75,p95 (any subset).
#' @keywords internal
.explore_box_plot <- function(q, value_label = "value") {
  if (is.null(q) || length(q) == 0) return(.plotly_no_data("No quantiles"))
  gv <- function(nm) {
    v <- suppressWarnings(as.numeric(q[[nm]]))
    if (length(v) == 0 || is.na(v)) NA_real_ else v
  }
  lo <- gv("p05"); q1 <- gv("p25"); md <- gv("p50"); q3 <- gv("p75"); hi <- gv("p95")
  if (all(is.na(c(lo, q1, md, q3, hi)))) return(.plotly_no_data("No quantiles"))
  .safe_plotly({
    plotly::plot_ly(type = "box", name = value_label,
                    lowerfence = list(lo), q1 = list(q1), median = list(md),
                    q3 = list(q3), upperfence = list(hi),
                    marker = list(color = .studio_colors[1]),
                    line = list(color = .studio_colors[1])) |>
      plotly::layout(yaxis = list(title = value_label),
                     xaxis = list(title = "")) |>
      .plotly_defaults()
  })
}

#' Heatmap from a (already-suppressed) cross-tab matrix; NA cells shown blank
#' @keywords internal
.explore_heatmap_plot <- function(M, title = NULL) {
  if (is.null(M) || !is.matrix(M) || length(M) == 0) {
    return(.plotly_no_data("No cross-tab data"))
  }
  .safe_plotly({
    txt <- matrix(ifelse(is.na(M), "—", as.character(M)),
                  nrow = nrow(M), ncol = ncol(M))
    plotly::plot_ly(
      x = colnames(M), y = rownames(M), z = M, type = "heatmap",
      text = txt, hoverinfo = "x+y+text",
      colorscale = "Blues", showscale = TRUE,
      colorbar = list(title = "Count")) |>
      plotly::layout(xaxis = list(title = "", type = "category"),
                     yaxis = list(title = "", type = "category")) |>
      .plotly_defaults(title = title)
  })
}

#' Missingness bar chart (missing % per column), sorted desc
#' @keywords internal
.explore_miss_plot <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(.plotly_no_data("No missingness data"))
  }
  .safe_plotly({
    pct_col <- if ("missing_pct" %in% names(df)) "missing_pct" else "missing_rate"
    d <- df
    d$.pct <- suppressWarnings(as.numeric(d[[pct_col]]))
    if (pct_col == "missing_rate") d$.pct <- d$.pct * 100
    d <- d[!is.na(d$.pct), , drop = FALSE]
    if (nrow(d) == 0) return(.plotly_no_data("All columns suppressed"))
    d <- d[order(d$.pct, decreasing = TRUE), , drop = FALSE]
    plotly::plot_ly(x = d$.pct, y = d$column_name, type = "bar",
                    orientation = "h",
                    marker = list(color = .studio_colors[3])) |>
      plotly::layout(xaxis = list(title = "Missing %", range = c(0, 100)),
                     yaxis = list(title = "", categoryorder = "total ascending")) |>
      .plotly_defaults()
  })
}


# --- Sub-module: Value Ranking (B1) ----------------------------------------
# ds.omop.value.counts over any concept/categorical column -> ranked bar + table.

#' @keywords internal
.mod_explore_value_ranking_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Value Ranking", width = 280, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("column"), "Column", choices = NULL),
      shiny::numericInput(ns("top_n"), "Top N", value = 20, min = 5, max = 200,
                          step = 5),
      shiny::actionButton(ns("run_btn"), "Run", icon = shiny::icon("play"),
                          class = "btn-primary w-100")
    ),
    shiny::uiOutput(ns("header")),
    .explore_chart_card(ns, "Value frequencies", "vr_dt", "vr_plot")
  )
}

#' @keywords internal
.mod_explore_value_ranking_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    res_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      tbls <- .get_person_tables(state$tables)
      if (length(tbls) == 0) {
        tbls <- c("condition_occurrence", "drug_exposure", "measurement",
                  "observation", "procedure_occurrence", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = .table_choices(tbls))
    })

    shiny::observeEvent(input$table, {
      cols <- tryCatch({
        cr <- ds.omop.columns(input$table, symbol = state$symbol)
        cc <- .get_concept_columns(cr[[1]])
        if (length(cc) == 0) cr[[1]]$column_name else cc
      }, error = function(e) character(0))
      shiny::updateSelectInput(session, "column", choices = cols)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$column)
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      shiny::withProgress(message = "Counting values...", value = 0.4, {
        res <- tryCatch(
          ds.omop.value.counts(table = input$table, column = input$column,
                               top_n = as.integer(input$top_n %||% 20),
                               scope = .backend_scope(scope),
                               pooling_policy = policy, symbol = state$symbol),
          error = function(e) {
            shiny::showNotification(.clean_ds_error(e), type = "error"); NULL
          })
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        res_rv(res)
      })
    })

    .vr_df <- shiny::reactive({
      res <- res_rv()
      if (is.null(res)) return(NULL)
      df <- .extract_display_data(res, state$scope %||% "per_site",
                                  state$selected_servers)
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      # Drop suppressed rows (count NA) from the displayed/charted frame.
      cnt <- if ("n" %in% names(df)) "n" else "count"
      if (cnt %in% names(df)) df <- df[!is.na(df[[cnt]]), , drop = FALSE]
      # Prefer concept_name label when available.
      lab <- if ("concept_name" %in% names(df)) "concept_name" else "value"
      keep <- intersect(c(lab, "value", "n", "count"), names(df))
      df[, unique(keep), drop = FALSE]
    })

    output$header <- shiny::renderUI({
      if (is.null(.vr_df())) {
        return(.empty_state_ui("ranking-star", "No values ranked",
          "Pick a table and column, then click Run."))
      }
      NULL
    })

    output$vr_plot <- plotly::renderPlotly({
      df <- .vr_df()
      if (is.null(df)) return(.plotly_no_data("No values to rank"))
      lab <- if ("concept_name" %in% names(df)) "concept_name" else "value"
      val <- if ("n" %in% names(df)) "n" else "count"
      .explore_rank_plot(df, lab, val, top_n = as.integer(input$top_n %||% 20))
    })

    output$vr_dt <- DT::renderDT({
      df <- .vr_df()
      val <- if (!is.null(df) && "n" %in% names(df)) "n" else "count"
      .explore_dt(df, default_sort = val, selection = "none")
    })
  })
}


# --- Sub-module: Numeric Distribution (B2) ---------------------------------
# ds.omop.value.histogram + ds.omop.value.quantiles -> histogram + box + table.

#' @keywords internal
.mod_explore_numeric_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Numeric Distribution", width = 280, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("value_col"), "Numeric column", choices = NULL),
      shiny::numericInput(ns("bins"), "Bins", value = 20, min = 5, max = 50,
                          step = 5),
      shiny::actionButton(ns("run_btn"), "Run", icon = shiny::icon("play"),
                          class = "btn-primary w-100")
    ),
    shiny::uiOutput(ns("header")),
    .explore_chart_card(ns, "Histogram", "num_hist_dt", "num_hist_plot"),
    bslib::card(full_screen = TRUE,
      bslib::card_header("Distribution (box from percentiles)"),
      bslib::card_body(
        plotly::plotlyOutput(ns("num_box_plot"), height = "260px"),
        DT::DTOutput(ns("num_quant_dt"))
      ))
  )
}

#' @keywords internal
.mod_explore_numeric_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    hist_rv <- shiny::reactiveVal(NULL)
    quant_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      tbls <- .get_person_tables(state$tables)
      if (length(tbls) == 0) {
        tbls <- c("measurement", "observation", "drug_exposure")
      }
      shiny::updateSelectInput(session, "table", choices = .table_choices(tbls))
    })

    shiny::observeEvent(input$table, {
      cols <- tryCatch({
        cr <- ds.omop.columns(input$table, symbol = state$symbol)
        cn <- cr[[1]]$column_name
        cn[grepl("value_as_number$|^quantity$|^days_supply$|_range_low$|_range_high$",
                 cn, ignore.case = TRUE)]
      }, error = function(e) character(0))
      if (length(cols) == 0) cols <- "value_as_number"
      shiny::updateSelectInput(session, "value_col", choices = cols)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$value_col)
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      shiny::withProgress(message = "Computing distribution...", value = 0.4, {
        h <- tryCatch(
          ds.omop.value.histogram(table = input$table, value_col = input$value_col,
            bins = as.integer(input$bins %||% 20),
            scope = .backend_scope(scope), pooling_policy = policy,
            symbol = state$symbol),
          error = function(e) {
            shiny::showNotification(.clean_ds_error(e), type = "error"); NULL
          })
        q <- tryCatch(
          ds.omop.value.quantiles(table = input$table, value_col = input$value_col,
            scope = .backend_scope(scope), pooling_policy = policy,
            symbol = state$symbol),
          error = function(e) NULL)
        for (r in list(h, q)) {
          if (inherits(r, "dsomop_result") && nchar(r$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, r$meta$call_code)
          }
        }
        hist_rv(h); quant_rv(q)
      })
    })

    .hist_df <- shiny::reactive({
      res <- hist_rv()
      if (is.null(res)) return(NULL)
      df <- .extract_display_data(res, state$scope %||% "per_site",
                                  state$selected_servers)
      if (is.data.frame(df) && nrow(df) > 0) df else NULL
    })

    # Pooled quantiles are NULL by design -> always pick a per-site slice.
    .quant_df <- shiny::reactive({
      res <- quant_rv()
      if (is.null(res)) return(NULL)
      ps <- res$per_site
      if (length(ps) == 0) return(NULL)
      srvs <- .resolve_servers(ps, state$selected_servers)
      srv <- if (length(srvs) > 0) srvs[1] else names(ps)[1]
      df <- ps[[srv]]
      if (is.data.frame(df) && nrow(df) > 0) df else NULL
    })

    .quant_named <- shiny::reactive({
      df <- .quant_df()
      if (is.null(df) || !"probability" %in% names(df)) return(NULL)
      pick <- function(p) {
        v <- df$value[abs(df$probability - p) < 1e-6]
        if (length(v) > 0) v[1] else NA_real_
      }
      list(p05 = pick(0.05), p25 = pick(0.25), p50 = pick(0.5),
           p75 = pick(0.75), p95 = pick(0.95))
    })

    output$header <- shiny::renderUI({
      if (is.null(.hist_df()) && is.null(.quant_df())) {
        return(.empty_state_ui("chart-area", "No distribution computed",
          "Pick a table and numeric column, then click Run."))
      }
      if (identical(state$scope %||% "per_site", "pooled")) {
        return(shiny::p(class = "text-muted small mb-2",
          shiny::icon("circle-info"),
          " Pooled quantiles unavailable — box plot uses per-site percentiles."))
      }
      NULL
    })

    output$num_hist_plot <- plotly::renderPlotly({
      .explore_hist_plot(.hist_df(), value_label = input$value_col %||% "value")
    })

    output$num_hist_dt <- DT::renderDT({
      .explore_dt(.hist_df(), selection = "none")
    })

    output$num_box_plot <- plotly::renderPlotly({
      .explore_box_plot(.quant_named(), value_label = input$value_col %||% "value")
    })

    output$num_quant_dt <- DT::renderDT({
      .explore_dt(.quant_df(), selection = "none", page_length = 10L)
    })
  })
}


# --- Sub-module: Cross-tab (B3) --------------------------------------------
# ds.omop.crosstab -> table (NA shown as "-") + heatmap; optional stratify
# produces small-multiple heatmaps. Charts built ONLY from suppressed counts.

#' Convert a cross-tab matrix to a long->wide display frame (NA -> "-")
#' @keywords internal
.ct_matrix_to_df <- function(M) {
  if (is.null(M) || !is.matrix(M) || length(M) == 0) return(NULL)
  body <- as.data.frame(matrix(
    ifelse(is.na(M), "—", format(M, trim = TRUE)),
    nrow = nrow(M), ncol = ncol(M)), stringsAsFactors = FALSE)
  names(body) <- colnames(M)
  cbind(data.frame(.row = rownames(M), check.names = FALSE,
                   stringsAsFactors = FALSE), body)
}

#' @keywords internal
.mod_explore_crosstab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Cross-tab", width = 300, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("row"), "Row column", choices = NULL),
      shiny::selectInput(ns("col"), "Column column", choices = NULL),
      shiny::radioButtons(ns("by"), "Count",
        choices = c("Distinct persons" = "persons", "Records" = "records"),
        selected = "persons"),
      shiny::selectInput(ns("stratify_by"), "Stratify by (optional)",
                         choices = c("(none)" = "")),
      shiny::actionButton(ns("run_btn"), "Run", icon = shiny::icon("play"),
                          class = "btn-primary w-100"),
      shiny::p(class = "text-muted small mt-2",
        shiny::icon("circle-info"),
        " Cross-tab is descriptive. For a multivariable association ",
        "(adjusting for confounders), use ", shiny::tags$code("ds.glm"), ".")
    ),
    shiny::uiOutput(ns("header")),
    .explore_chart_card(ns, "Contingency table", "ct_dt", "ct_plot")
  )
}

#' @keywords internal
.mod_explore_crosstab_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    res_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      tbls <- .get_person_tables(state$tables)
      if (length(tbls) == 0) {
        tbls <- c("person", "condition_occurrence", "drug_exposure",
                  "measurement", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = .table_choices(tbls))
    })

    shiny::observeEvent(input$table, {
      cols <- tryCatch({
        cr <- ds.omop.columns(input$table, symbol = state$symbol)
        cc <- .get_concept_columns(cr[[1]])
        if (length(cc) == 0) cr[[1]]$column_name else cc
      }, error = function(e) character(0))
      shiny::updateSelectInput(session, "row", choices = cols)
      shiny::updateSelectInput(session, "col",
        choices = cols, selected = if (length(cols) > 1) cols[2] else cols[1])
      shiny::updateSelectInput(session, "stratify_by",
        choices = c("(none)" = "", cols))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$row, input$col)
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      strat <- if (nzchar(input$stratify_by %||% "")) input$stratify_by else NULL
      shiny::withProgress(message = "Building cross-tab...", value = 0.4, {
        res <- tryCatch(
          ds.omop.crosstab(table = input$table, row = input$row, col = input$col,
            by = input$by %||% "persons", stratify_by = strat,
            scope = .backend_scope(scope), pooling_policy = policy,
            symbol = state$symbol),
          error = function(e) {
            shiny::showNotification(.clean_ds_error(e), type = "error"); NULL
          })
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        res_rv(res)
      })
    })

    # Pick the display object: pooled list when scoped pooled, else first site.
    .ct_obj <- shiny::reactive({
      res <- res_rv()
      if (is.null(res)) return(NULL)
      scope <- state$scope %||% "per_site"
      if (identical(scope, "pooled") && !is.null(res$pooled)) return(res$pooled)
      ps <- res$per_site
      if (length(ps) == 0) return(NULL)
      srvs <- .resolve_servers(ps, state$selected_servers)
      ps[[if (length(srvs) > 0) srvs[1] else names(ps)[1]]]
    })

    output$header <- shiny::renderUI({
      obj <- .ct_obj()
      if (is.null(obj)) {
        return(.empty_state_ui("table-cells", "No cross-tab",
          "Pick a table and two columns, then click Run."))
      }
      if (is.list(obj) && isTRUE(obj$stratified)) {
        return(shiny::p(class = "text-muted small mb-2",
          shiny::icon("layer-group"),
          sprintf(" Stratified by %s — one protected 2-way table per level.",
                  obj$stratify_by %||% "stratum")))
      }
      NULL
    })

    output$ct_plot <- plotly::renderPlotly({
      obj <- .ct_obj()
      if (is.null(obj)) return(.plotly_no_data("No cross-tab data"))
      if (is.list(obj) && isTRUE(obj$stratified)) {
        # Small multiples: one heatmap per stratum, stacked as subplots.
        strata <- obj$strata
        plots <- lapply(names(strata), function(lv) {
          M <- strata[[lv]]$counts
          .explore_heatmap_plot(M, title = lv)
        })
        plots <- Filter(Negate(is.null), plots)
        if (length(plots) == 0) return(.plotly_no_data("All strata suppressed"))
        return(plotly::subplot(plots, nrows = length(plots), shareX = FALSE,
                               titleY = TRUE, margin = 0.06))
      }
      .explore_heatmap_plot(obj$counts)
    })

    output$ct_dt <- DT::renderDT({
      obj <- .ct_obj()
      if (is.null(obj)) return(NULL)
      if (is.list(obj) && isTRUE(obj$stratified)) {
        # Concatenate per-stratum wide frames with a stratum label column.
        frames <- lapply(names(obj$strata), function(lv) {
          d <- .ct_matrix_to_df(obj$strata[[lv]]$counts)
          if (is.null(d)) return(NULL)
          cbind(data.frame(stratum = lv, stringsAsFactors = FALSE), d)
        })
        frames <- Filter(Negate(is.null), frames)
        if (length(frames) == 0) return(NULL)
        all_cols <- Reduce(union, lapply(frames, names))
        frames <- lapply(frames, function(d) {
          miss <- setdiff(all_cols, names(d))
          for (m in miss) d[[m]] <- "—"
          d[, all_cols, drop = FALSE]
        })
        df <- do.call(rbind, frames)
      } else {
        df <- .ct_matrix_to_df(obj$counts)
      }
      .explore_dt(df, default_sort = NULL, selection = "none", escape = FALSE)
    })
  })
}


# --- Sub-module: Missingness (B4) ------------------------------------------
# ds.omop.missingness table-level -> sorted-by-missing% table + bar chart.

#' @keywords internal
.mod_explore_missingness_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Missingness", width = 280, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::actionButton(ns("run_btn"), "Run", icon = shiny::icon("play"),
                          class = "btn-primary w-100")
    ),
    shiny::uiOutput(ns("header")),
    .explore_chart_card(ns, "Column missingness", "miss_dt", "miss_plot")
  )
}

#' @keywords internal
.mod_explore_missingness_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    res_rv <- shiny::reactiveVal(NULL)

    shiny::observe({
      tbls <- .get_person_tables(state$tables)
      if (length(tbls) == 0) {
        tbls <- c("person", "condition_occurrence", "drug_exposure",
                  "measurement", "visit_occurrence", "observation")
      }
      shiny::updateSelectInput(session, "table", choices = .table_choices(tbls))
    })

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table)
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      shiny::withProgress(message = "Computing missingness...", value = 0.4, {
        res <- tryCatch(
          ds.omop.missingness(table = input$table,
            scope = .backend_scope(scope), pooling_policy = policy,
            symbol = state$symbol),
          error = function(e) {
            shiny::showNotification(.clean_ds_error(e), type = "error"); NULL
          })
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        res_rv(res)
      })
    })

    .miss_df <- shiny::reactive({
      res <- res_rv()
      if (is.null(res)) return(NULL)
      df <- .extract_display_data(res, state$scope %||% "per_site",
                                  state$selected_servers)
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
      # Build a tidy display frame with a missing_pct column, sorted desc.
      if ("missing_rate" %in% names(df)) df$missing_pct <- round(df$missing_rate * 100, 1)
      keep <- intersect(c("column_name", "n_total", "n_missing", "missing_pct"),
                        names(df))
      df <- df[, keep, drop = FALSE]
      if ("missing_pct" %in% names(df)) {
        df <- df[order(df$missing_pct, decreasing = TRUE), , drop = FALSE]
      }
      df
    })

    output$header <- shiny::renderUI({
      if (is.null(.miss_df())) {
        return(.empty_state_ui("table-cells-large", "No missingness",
          "Pick a table, then click Run."))
      }
      NULL
    })

    output$miss_plot <- plotly::renderPlotly({
      .explore_miss_plot(.miss_df())
    })

    output$miss_dt <- DT::renderDT({
      .explore_dt(.miss_df(), default_sort = "missing_pct", selection = "none")
    })
  })
}


# --- Sub-module: Concept Summary -------------------------------------------
# Type-aware, disclosure-safe summary of a value column WITHIN one concept
# (ds.omop.concept.summary, 2.2.0). Returns
#   list(table, concept_id,
#        numeric    = list(<col> = list(stats=<column.stats>, quantiles=<value.quantiles>)),
#        categorical= list(<col> = <value.counts>))
# where each inner result carries $per_site / $pooled.

#' Pick the pooled element of a per_site/pooled result, else the first site
#' @keywords internal
.cs_pick_scope <- function(result, scope = "pooled") {
  if (is.null(result)) return(NULL)
  if (identical(scope, "pooled") && !is.null(result$pooled)) return(result$pooled)
  ps <- result$per_site
  if (length(ps) > 0) return(ps[[1]])
  NULL
}

#' Build the numeric display table from a ds.omop.concept.summary result
#' @keywords internal
.cs_numeric_table <- function(res, scope = "pooled") {
  if (is.null(res) || length(res$numeric) == 0) return(NULL)
  rows <- lapply(names(res$numeric), function(col) {
    nx <- res$numeric[[col]]
    st <- .cs_pick_scope(nx$stats, scope)
    q  <- .cs_pick_scope(nx$quantiles, "per_site")
    gq <- function(p) {
      if (is.data.frame(q) && "probability" %in% names(q)) {
        v <- q$value[abs(q$probability - p) < 1e-6]
        if (length(v) > 0) round(v[1], 2) else NA_real_
      } else NA_real_
    }
    data.frame(column = col,
      n = if (!is.null(st$n_total)) st$n_total else NA_real_,
      mean = if (!is.null(st$mean)) round(st$mean, 2) else NA_real_,
      p25 = gq(0.25), median = gq(0.5), p75 = gq(0.75),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#' Build the categorical display table (first categorical column) from a result
#' @keywords internal
.cs_categorical_table <- function(res, scope = "pooled") {
  if (is.null(res) || length(res$categorical) == 0) return(NULL)
  col <- names(res$categorical)[1]
  cc <- .cs_pick_scope(res$categorical[[col]], scope)
  if (!is.data.frame(cc) || nrow(cc) == 0) return(NULL)
  cc
}

#' @keywords internal
.mod_explore_concept_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Summary", width = 300, open = "always",
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      .concept_picker_ui(ns("concept"), label = "Concept"),
      shiny::selectInput(ns("value_column"), "Value column",
                         choices = c("(auto-detect)" = "")),
      shiny::actionButton(ns("run_btn"), "Run", icon = shiny::icon("play"),
                          class = "btn-primary w-100"),
      shiny::actionButton(ns("extract_to_recipe"),
                          shiny::tagList(shiny::icon("plus"), "Extract to recipe"),
                          class = "btn-success text-white w-100 mt-1")
    ),
    shiny::uiOutput(ns("header")),
    bslib::card(full_screen = TRUE,
      bslib::card_header("Numeric value statistics (no min/max — DataSHIELD-safe)"),
      bslib::card_body(DT::DTOutput(ns("numeric_dt")))),
    bslib::card(full_screen = TRUE,
      bslib::card_header("Categorical value counts"),
      bslib::card_body(DT::DTOutput(ns("categorical_dt"))))
  )
}

#' @keywords internal
.mod_explore_concept_summary_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    res_rv <- shiny::reactiveVal(NULL)
    picked_concept <- .concept_picker_server("concept", state)

    shiny::observe({
      tbls <- tryCatch(.get_person_tables(state$tables), error = function(e) NULL)
      if (length(tbls) == 0) {
        tbls <- c("measurement", "observation", "condition_occurrence",
                  "drug_exposure", "procedure_occurrence", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = tbls)
    })

    shiny::observeEvent(input$table, {
      cols <- tryCatch({
        cr <- ds.omop.columns(input$table, symbol = state$symbol)
        cn <- cr[[1]]$column_name
        cn[grepl("value_as_number$|value_as_concept_id$|^quantity$|^days_supply$",
                 cn, ignore.case = TRUE)]
      }, error = function(e) character(0))
      shiny::updateSelectInput(session, "value_column",
                               choices = c("(auto-detect)" = "", cols))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table)
      pc <- picked_concept()
      cid <- if (is.list(pc)) pc$concept_id else pc
      if (is.null(cid) || length(cid) == 0) {
        shiny::showNotification("Pick a concept first.", type = "warning")
        return()
      }
      col <- if (nzchar(input$value_column %||% "")) input$value_column else NULL
      shiny::withProgress(message = "Querying concept summary…", value = 0.4, {
        res <- tryCatch(
          ds.omop.concept.summary(table = input$table, concept_id = cid,
                                  column = col, scope = "pooled",
                                  symbol = state$symbol),
          error = function(e) {
            shiny::showNotification(.clean_ds_error(e), type = "error")
            NULL
          })
        res_rv(res)
      })
    })

    # +Extract: add the looked-up concept as a variable to recipe
    shiny::observeEvent(input$extract_to_recipe, {
      shiny::req(input$table)
      pc <- picked_concept()
      cid <- if (is.list(pc)) pc$concept_id else pc
      if (is.null(cid) || length(cid) == 0) {
        shiny::showNotification("Pick a concept first.", type = "warning")
        return()
      }
      cname <- if (is.list(pc)) pc$concept_name else NULL
      tryCatch({
        v <- omop_variable(
          table = input$table, concept_id = as.integer(cid),
          concept_name = cname, format = "raw"
        )
        state$recipe <- recipe_add_variable(state$recipe, v)
        shiny::showNotification(
          paste("Added", v$name, "to recipe"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(.clean_ds_error(e), type = "error")
      })
    })

    output$header <- shiny::renderUI({
      res <- res_rv()
      if (is.null(res)) {
        return(.empty_state_ui("database", "No concept summarized",
                               "Pick a table and concept, then click Run."))
      }
      bslib::value_box(
        title = paste0(res$table, " · concept ", res$concept_id),
        value = paste0(length(res$numeric), " numeric / ",
                       length(res$categorical), " categorical column(s)"),
        showcase = shiny::icon("database"), theme = "primary")
    })

    output$numeric_dt <- DT::renderDT({
      df <- .cs_numeric_table(res_rv(), "pooled")
      if (is.null(df)) return(NULL)
      .explore_dt(df, page_length = 10L, selection = "none")
    })

    output$categorical_dt <- DT::renderDT({
      cc <- .cs_categorical_table(res_rv(), "pooled")
      if (is.null(cc)) return(NULL)
      val <- if ("n" %in% names(cc)) "n" else NULL
      .explore_dt(cc, default_sort = val, selection = "none",
                  caption = "Categorical value counts")
    })
  })
}
