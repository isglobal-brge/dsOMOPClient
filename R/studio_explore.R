# ==============================================================================
# MODULE 2: Explore (Table -> Concepts)
# Replaces old Catalog + Observed Concepts tabs
# ==============================================================================

.mod_table_concepts_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Explore", width = 300,
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("concept_col"), "Concept Column", choices = NULL),
      shiny::selectInput(ns("metric"), "Metric",
        choices = c("Distinct Persons" = "persons",
                    "Total Records" = "records")),
      shiny::numericInput(ns("top_n"), "Top N", 30, 5, 200, 5),
      shiny::actionButton(ns("run_btn"), "Run",
                          class = "btn-primary w-100"),
      .scope_controls_ui(ns)
    ),
    bslib::card(
      bslib::card_header("Table Statistics"),
      bslib::card_body(
        shiny::uiOutput(ns("table_stats"))
      )
    ),
    bslib::card(
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::textOutput(ns("results_title")),
          shiny::div(
            shiny::actionButton(ns("add_selected_btn"), "Add to Set",
                                class = "btn-sm btn-outline-primary me-1"),
            shiny::actionButton(ns("extract_selected_btn"),
                                shiny::tagList(shiny::icon("plus"), "Extract"),
                                class = "btn-sm btn-outline-success me-1"),
            shiny::actionButton(ns("filter_selected_btn"),
                                shiny::tagList(shiny::icon("filter"), "Filter"),
                                class = "btn-sm btn-outline-warning")
          )
        )
      ),
      bslib::card_body(
        shiny::plotOutput(ns("bar_chart"), height = "400px"),
        shiny::div(
          DT::DTOutput(ns("results_dt"))
        ),
        shiny::uiOutput(ns("scope_info"))
      )
    )
  )
}

.mod_table_concepts_server <- function(id, state, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    prevalence_data <- shiny::reactiveVal(NULL)

    # Populate table dropdown from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) == 0) {
        tbl_choices <- c("condition_occurrence", "drug_exposure",
                         "measurement", "procedure_occurrence",
                         "observation", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = tbl_choices)
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
        srv <- names(stats_res$per_site)[1]
        s <- stats_res$per_site[[srv]]
        shiny::tags$dl(class = "row",
          shiny::tags$dt(class = "col-sm-4", "Rows"),
          shiny::tags$dd(class = "col-sm-8",
            if (isTRUE(s$rows_suppressed)) shiny::span(
              class = "suppressed", "suppressed")
            else format(s$rows, big.mark = ",")),
          if (!is.null(s$persons)) shiny::tagList(
            shiny::tags$dt(class = "col-sm-4", "Distinct Persons"),
            shiny::tags$dd(class = "col-sm-8",
              if (isTRUE(s$persons_suppressed)) shiny::span(
                class = "suppressed", "suppressed")
              else format(s$persons, big.mark = ","))
          )
        )
      }, error = function(e) {
        shiny::p(class = "text-danger", conditionMessage(e))
      })
    })

    # Track last result for scope info
    last_result <- shiny::reactiveVal(NULL)

    # Run concept prevalence
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$concept_col)
      shiny::showNotification("Querying...", type = "message",
                              duration = 2, id = "prev_loading")

      scope <- input$scope %||% "per_site"
      policy <- input$pooling_policy %||% "strict"
      # Sync scope to shared state
      state$scope <- scope
      state$pooling_policy <- policy

      tryCatch({
        res <- ds.omop.concept.prevalence(
          table = input$table, concept_col = input$concept_col,
          metric = input$metric, top_n = input$top_n,
          scope = scope, pooling_policy = policy,
          symbol = state$symbol
        )
        # Accumulate code for Script tab
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        last_result(res)
        # Use pooled data if available, otherwise first server
        if (scope == "pooled" && !is.null(res$pooled) && is.data.frame(res$pooled)) {
          prevalence_data(res$pooled)
        } else {
          srv <- names(res$per_site)[1]
          prevalence_data(res$per_site[[srv]])
        }
        shiny::removeNotification("prev_loading")
      }, error = function(e) {
        shiny::removeNotification("prev_loading")
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$results_title <- shiny::renderText({
      paste("Top concepts in", input$table)
    })

    output$bar_chart <- shiny::renderPlot({
      df <- prevalence_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      metric_col <- if (input$metric == "persons") "n_persons" else "n_records"
      if (!metric_col %in% names(df)) return(NULL)

      .safe_plot({
        label_col <- if ("concept_name" %in% names(df)) "concept_name"
          else "concept_id"
        df$label <- substr(as.character(df[[label_col]]), 1, 40)
        df$y <- as.numeric(df[[metric_col]])
        df <- df[!is.na(df$y), , drop = FALSE]
        if (nrow(df) == 0) return(NULL)

        df <- df[order(df$y, decreasing = TRUE), ]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), ]

        par(mar = c(5, 12, 2, 2))
        barplot(
          rev(df$y), names.arg = rev(df$label),
          horiz = TRUE, las = 1, col = "#3498db",
          xlab = if (input$metric == "persons")
            "Distinct Persons" else "Records",
          cex.names = 0.75
        )
      })
    }, res = 96)

    output$results_dt <- DT::renderDT({
      df <- prevalence_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df,
        options = list(pageLength = 20, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "multiple")
    })

    # Single row click -> navigate to Drilldown
    shiny::observeEvent(input$results_dt_rows_selected, {
      df <- prevalence_data()
      idx <- input$results_dt_rows_selected
      if (!is.null(idx) && !is.null(df) && length(idx) == 1 && idx <= nrow(df)) {
        state$selected_table <- input$table
        state$selected_concept_col <- input$concept_col
        state$selected_concept_id <- as.integer(df$concept_id[idx])
        state$selected_concept_name <- as.character(
          df$concept_name[idx] %||% ""
        )
        # Navigate to Drilldown tab
        shiny::updateNavbarPage(parent_session, "main_nav",
                                selected = "Drilldown")
      }
    })

    # Bulk add selected concepts to set
    shiny::observeEvent(input$add_selected_btn, {
      df <- prevalence_data()
      idx <- input$results_dt_rows_selected
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
      idx <- input$results_dt_rows_selected
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
      idx <- input$results_dt_rows_selected
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

      tags <- list()
      # Server badges
      for (srv in res$meta$servers) {
        tags <- c(tags, list(
          shiny::span(class = "server-badge server-badge-ok", srv)
        ))
      }
      scope_text <- if (res$meta$scope == "pooled") "Pooled" else "Per-site"
      warns <- res$meta$warnings
      warn_ui <- if (length(warns) > 0) {
        shiny::div(class = "text-warning mt-1",
          shiny::icon("exclamation-triangle"),
          paste(warns, collapse = "; ")
        )
      }
      shiny::div(class = "scope-indicator mt-2",
        shiny::span("Scope: ", scope_text, " | Servers: "),
        shiny::tagList(tags),
        warn_ui
      )
    })
  })
}

