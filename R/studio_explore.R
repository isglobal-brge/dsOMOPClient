# ==============================================================================
# MODULE: Explore (consolidated)
# Wraps Prevalence, Drilldown, Locator, and Vocabulary as sub-tabs
# ==============================================================================

# ------------------------------------------------------------------------------
# Wrapper UI: navset_pill with 4 sub-tabs
# ------------------------------------------------------------------------------
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
      .mod_explore_vocab_ui(ns("vocab")))
  )
}

# ------------------------------------------------------------------------------
# Wrapper Server: calls all 4 sub-module servers
# ------------------------------------------------------------------------------
.mod_explore_server <- function(id, state, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_explore_prevalence_server("prevalence", state, session)
    .mod_explore_drilldown_server("drilldown", state, session)
    .mod_explore_locator_server("locator", state)
    .mod_explore_vocab_server("vocab", state)
  })
}


# ==============================================================================
# SUB-MODULE 1: Prevalence (was .mod_table_concepts)
# ==============================================================================

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
      DT::DTOutput(ns("results_dt"))
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
          "$(table.column(0).header()).empty().append(hdr);",
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

    # +Extract: add selected concepts as variables to cart
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
      for (i in idx) {
        cid <- as.integer(df$concept_id[i])
        cname <- if ("concept_name" %in% names(df))
          as.character(df$concept_name[i]) else NULL
        tryCatch({
          v <- omop_variable(
            table = tbl, concept_id = cid,
            concept_name = cname, format = "raw"
          )
          state$cart <- cart_add_variable(state$cart, v)
          added <- added + 1L
        }, error = function(e) NULL)
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "variable(s) to cart"),
          type = "message", duration = 2)
      }
    })

    # +Filter: add selected concepts as has_concept filters to cart
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
      for (i in idx) {
        cid <- as.integer(df$concept_id[i])
        cname <- if ("concept_name" %in% names(df))
          as.character(df$concept_name[i]) else NULL
        tryCatch({
          f <- omop_filter_has_concept(cid, tbl, cname)
          state$cart <- cart_add_filter(state$cart, f)
          added <- added + 1L
        }, error = function(e) NULL)
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "filter(s) to cart"),
          type = "message", duration = 2)
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


# ==============================================================================
# SUB-MODULE 2: Drilldown (was .mod_concept_drilldown)
# ==============================================================================

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
      shiny::actionButton(ns("add_to_cart_extract"),
                          shiny::tagList(shiny::icon("plus"), "Extract to Builder"),
                          class = "btn-success text-white btn-sm w-100 mb-1"),
      shiny::actionButton(ns("add_to_cart_filter"),
                          shiny::tagList(shiny::icon("filter"), "Filter in Builder"),
                          class = "btn-warning text-white btn-sm w-100 mb-1"),
      shiny::actionButton(ns("add_to_plan"), "Add to Plan",
                          class = "btn-outline-info btn-sm w-100 mb-1"),
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
        plotly::plotlyOutput(ns("missingness_plot"), height = "250px")
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

    # Add to cart as extraction variable
    shiny::observeEvent(input$add_to_cart_extract, {
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
        state$cart <- cart_add_variable(state$cart, v)
        shiny::showNotification(
          paste("Added", v$name, "to cart"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add to cart as filter
    shiny::observeEvent(input$add_to_cart_filter, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) {
        shiny::showNotification("No concept selected.", type = "warning")
        return()
      }
      cname <- state$selected_concept_name
      tryCatch({
        f <- omop_filter_has_concept(cid, tbl, cname)
        state$cart <- cart_add_filter(state$cart, f)
        shiny::showNotification(
          paste("Added filter for concept", cid, "to cart"),
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
      DT::datatable(df, options = list(pageLength = 10, dom = "t", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
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
      DT::datatable(df, options = list(pageLength = 10, dom = "ftip", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
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
        shiny::tags$dt(class = "col-sm-4", "Safe Min"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$min_date_safe %||% "N/A")),
        shiny::tags$dt(class = "col-sm-4", "Safe Max"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$max_date_safe %||% "N/A"))
      )
    })

    # --- Missingness ---
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


# ==============================================================================
# SUB-MODULE 3: Locator (was .mod_concept_locator)
# ==============================================================================

.mod_explore_locator_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center py-2",
        shiny::span("Concept Locator"),
        shiny::actionButton(ns("locate_btn"), NULL,
                            icon = shiny::icon("location-dot"),
                            class = "btn-sm btn-primary")
      ),
      bslib::card_body(
        class = "py-2 px-3",
        shiny::div(class = "d-flex gap-2 mb-2",
          shiny::div(style = "flex: 1;",
            shiny::textInput(ns("unified_search"), NULL,
                             placeholder = "Search by name or enter concept IDs")
          ),
          shiny::actionButton(ns("name_search_btn"), NULL,
                              icon = shiny::icon("magnifying-glass"),
                              class = "btn-sm btn-outline-secondary")
        ),
        DT::DTOutput(ns("search_results_dt")),
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

    output$search_results_dt <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      keep <- intersect(c("concept_id", "concept_name", "domain_id"), names(df))
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
      cid <- as.integer(df$concept_id[idx])
      cname <- as.character(df$concept_name[idx])
      current <- selected_ids()
      existing_cids <- vapply(current, function(x) x$concept_id, integer(1))
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
      DT::datatable(df,
        options = list(pageLength = 25, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "none")
    })
  })
}


# ==============================================================================
# SUB-MODULE 4: Vocabulary (was .mod_vocab)
# ==============================================================================

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
                                 value = FALSE, width = "auto")
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

    shiny::observeEvent(input$search_btn, {
      shiny::req(nchar(input$search_pattern) > 0)
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
        return(.empty_state_ui("book", "No results",
          "Search for concepts using the sidebar controls."))
      }
      DT::DTOutput(ns("results_table"))
    })

    output$results_table <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      if (isTRUE(input$group_concepts) && "server" %in% names(df)) {
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
        keep <- intersect(c("concept_id", "concept_name", "domain_id",
                             "vocabulary_id", "standard_concept", "n_servers"),
                          names(df))
      } else {
        keep <- intersect(c("concept_id", "concept_name", "domain_id",
                             "vocabulary_id", "standard_concept", "server"),
                          names(df))
      }
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
          "$(table.column(0).header()).empty().append(hdr);",
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

    # +Extract: add selected concepts as variables to cart
    shiny::observeEvent(input$extract_selected_vocab, {
      df <- search_results()
      idx <- input$vocab_chk_selected
      if (is.null(idx) || length(idx) == 0 || is.null(df)) {
        shiny::showNotification("Select rows first.", type = "warning")
        return()
      }
      idx <- idx[idx <= nrow(df)]
      added <- 0L
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
          state$cart <- cart_add_variable(state$cart, v)
          added <- added + 1L
        }, error = function(e) NULL)
      }
      if (added > 0) {
        shiny::showNotification(
          paste("Added", added, "variable(s) to cart"),
          type = "message", duration = 2)
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
                               "Click a row in the search results to view details."))
      }
      # Show details for the first selected row
      first_idx <- idx[1]
      if (first_idx > nrow(df)) {
        return(.empty_state_ui("hand-pointer", "Select a concept",
                               "Click a row in the search results to view details."))
      }
      row <- df[first_idx, , drop = FALSE]
      if (!is.data.frame(row) || nrow(row) == 0) {
        return(.empty_state_ui("hand-pointer", "Select a concept"))
      }
      std <- as.character(row$standard_concept[1] %||% "")
      std_badge <- if (std == "S") {
        shiny::span(class = "badge", style = "background:#065f46; color:#ecfdf5;", "Standard")
      } else if (nchar(std) > 0) {
        shiny::span(class = "badge bg-warning text-dark", std)
      }
      shiny::div(
        shiny::div(class = "d-flex align-items-center gap-2 mb-2",
          shiny::span(class = "badge bg-primary", style = "font-size:0.82rem;",
                      paste0("#", row$concept_id)),
          std_badge
        ),
        shiny::h6(class = "mb-2", style = "font-weight:600;",
                   as.character(row$concept_name)),
        shiny::tags$table(class = "table table-sm table-borderless mb-0",
          style = "font-size:0.84rem;",
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td(class = "text-muted", style = "width:35%;", "Domain"),
              shiny::tags$td(class = "fw-semibold", as.character(row$domain_id))
            ),
            shiny::tags$tr(
              shiny::tags$td(class = "text-muted", "Vocabulary"),
              shiny::tags$td(class = "fw-semibold",
                             as.character(row$vocabulary_id %||% ""))
            ),
            if ("server" %in% names(row)) shiny::tags$tr(
              shiny::tags$td(class = "text-muted", "Server"),
              shiny::tags$td(shiny::span(class = "badge bg-light text-dark",
                                         as.character(row$server)))
            )
          )
        )
      )
    })
  })
}


# ==============================================================================
# Helper functions
# ==============================================================================

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
