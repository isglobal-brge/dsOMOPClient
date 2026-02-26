# ==============================================================================
# MODULE 9: Session (kept as-is)
# ==============================================================================

.mod_session_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
      bslib::card_header("Remote Session Information"),
      bslib::card_body(
        shiny::actionButton(ns("refresh"), "Refresh",
                            class = "btn-sm btn-outline-primary mb-3"),
        .server_selector_ui(ns),
        shiny::uiOutput(ns("session_info"))
      )
    ),
    bslib::card(
      bslib::card_header("Domain Coverage"),
      bslib::card_body(
        shiny::actionButton(ns("coverage_btn"), "Load Coverage",
                            class = "btn-sm btn-outline-info mb-2"),
        DT::DTOutput(ns("coverage_dt"))
      )
    ),
    bslib::card(
      bslib::card_header("Missingness Explorer"),
      bslib::card_body(
        shiny::selectInput(ns("miss_table"), "Table",
          choices = .table_choices(c("person", "condition_occurrence",
                      "drug_exposure", "measurement",
                      "observation_period", "visit_occurrence"))),
        shiny::actionButton(ns("miss_btn"), "Check Missingness",
                            class = "btn-sm btn-outline-info mb-2"),
        DT::DTOutput(ns("miss_dt"))
      )
    )
  )
}

.mod_session_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    .scope_sync_servers(input, session, state)

    raw_coverage <- shiny::reactiveVal(NULL)
    raw_miss     <- shiny::reactiveVal(NULL)
    coverage_data <- shiny::reactiveVal(NULL)
    miss_data     <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$refresh, {
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
        if (!is.null(state$status$servers)) {
          state$server_names <- state$status$servers
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$session_info <- shiny::renderUI({
      st <- state$status
      if (is.null(st)) return(shiny::p("Loading..."))

      items <- list(
        shiny::tags$dt(class = "col-sm-4", "Session Symbol"),
        shiny::tags$dd(class = "col-sm-8",
          as.character(st$symbol %||% "unknown"))
      )

      if (!is.null(st$servers)) {
        items <- c(items, list(
          shiny::tags$dt(class = "col-sm-4", "Servers"),
          shiny::tags$dd(class = "col-sm-8",
            paste(st$servers, collapse = ", "))
        ))
      }

      err_text <- "None"
      if (!is.null(st$errors) && length(st$errors) > 0) {
        err_text <- paste(names(st$errors), collapse = ", ")
      }
      items <- c(items, list(
        shiny::tags$dt(class = "col-sm-4", "Errors"),
        shiny::tags$dd(class = "col-sm-8", err_text)
      ))

      shiny::tags$dl(class = "row", items)
    })

    # Load coverage
    shiny::observeEvent(input$coverage_btn, {
      tryCatch({
        res <- ds.omop.domain.coverage(symbol = state$symbol)
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        raw_coverage(res)
        coverage_data(.extract_display_data(res, "all",
          input$selected_server,
          intersect_only = isTRUE(input$intersect_only)))
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    # Re-extract coverage on server/intersect change
    shiny::observeEvent(list(input$selected_server, input$intersect_only), {
      res <- raw_coverage()
      if (is.null(res)) return()
      coverage_data(.extract_display_data(res, "all",
        input$selected_server,
        intersect_only = isTRUE(input$intersect_only)))
    }, ignoreInit = TRUE)

    output$coverage_dt <- DT::renderDT({
      df <- coverage_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 20, dom = "ft", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })

    # Load missingness
    shiny::observeEvent(input$miss_btn, {
      tryCatch({
        res <- ds.omop.missingness(input$miss_table,
                                    symbol = state$symbol)
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        raw_miss(res)
        miss_data(.extract_display_data(res, "all",
          input$selected_server,
          intersect_only = isTRUE(input$intersect_only)))
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    # Re-extract missingness on server/intersect change
    shiny::observeEvent(list(input$selected_server, input$intersect_only), {
      res <- raw_miss()
      if (is.null(res)) return()
      miss_data(.extract_display_data(res, "all",
        input$selected_server,
        intersect_only = isTRUE(input$intersect_only)))
    }, ignoreInit = TRUE)

    output$miss_dt <- DT::renderDT({
      df <- miss_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 25, dom = "ft", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })
  })
}
