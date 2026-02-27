# ==============================================================================
# MODULE: Execute (consolidated wrapper)
# Sub-tabs: Script Builder + Session
# ==============================================================================

# --- Wrapper UI / Server -----------------------------------------------------

.mod_execute_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_pill(
    id = ns("execute_nav"),
    bslib::nav_panel("Script", icon = shiny::icon("code"),
      .mod_execute_script_ui(ns("script"))),
    bslib::nav_panel("Session", icon = shiny::icon("server"),
      .mod_execute_session_ui(ns("session_tab")))
  )
}

.mod_execute_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_execute_script_server("script", state)
    .mod_execute_session_server("session_tab", state)
  })
}

# ==============================================================================
# SUB-MODULE: Script Builder
# ==============================================================================
# .highlightR() and .highlightR_line() are now in studio_codegen.R
# Clipboard JS handler is now in studio.R header
# ==============================================================================

.mod_execute_script_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(
          class = "d-flex justify-content-between align-items-center",
          "Accumulated R Script",
          shiny::div(
            shiny::actionButton(ns("copy_btn"), "Copy to Clipboard",
                                class = "btn-sm btn-outline-primary me-2"),
            shiny::downloadButton(ns("download_btn"), "Download .R",
                                  class = "btn-sm btn-outline-success me-2"),
            shiny::actionButton(ns("clear_btn"), "Clear",
                                class = "btn-sm btn-outline-danger")
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("script_html"))
      )
    )
  )
}

.mod_execute_script_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    script_text <- shiny::reactive({
      lines <- state$script_lines
      if (length(lines) == 0) return("")
      paste(lines, collapse = "\n")
    })

    output$script_html <- shiny::renderUI({
      code <- trimws(script_text())
      if (nchar(code) == 0) {
        return(.empty_state_ui("code", "No operations recorded",
          "Use the Explore, Vocabulary, Achilles, or Queries tabs to generate reproducible R code."))
      }
      highlighted <- .highlightR(code)
      shiny::div(class = "code-output",
        shiny::HTML(paste0("<pre style='margin:0;padding:0;'><code>",
                           highlighted, "</code></pre>"))
      )
    })

    shiny::observeEvent(input$copy_btn, {
      session$sendCustomMessage("copyToClipboard", script_text())
    })

    output$download_btn <- shiny::downloadHandler(
      filename = function() {
        paste0("dsomop_script_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      },
      content = function(file) {
        writeLines(script_text(), file)
      },
      contentType = "text/plain"
    )

    shiny::observeEvent(input$clear_btn, {
      state$script_lines <- character(0)
      shiny::showNotification("Script cleared.", type = "message", duration = 2)
    })
  })
}

# ==============================================================================
# SUB-MODULE: Session
# ==============================================================================

.mod_execute_session_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
      bslib::card_header("Remote Session Information"),
      bslib::card_body(
        shiny::actionButton(ns("refresh"), "Refresh",
                            class = "btn-sm btn-outline-primary mb-3"),
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

.mod_execute_session_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

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
      shiny::withProgress(message = "Loading domain coverage...", value = 0.3, {
        tryCatch({
          res <- ds.omop.domain.coverage(symbol = state$symbol)
          shiny::incProgress(0.5)
          if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, res$meta$call_code)
          }
          raw_coverage(res)
          # Always show all servers with server column for comparison
          coverage_data(.extract_display_data(res, "all",
            state$selected_servers, intersect_only = FALSE))
        }, error = function(e) {
          shiny::showNotification(
            paste("Error:", conditionMessage(e)), type = "error"
          )
        })
      })
    })

    # Re-extract coverage on server change
    shiny::observeEvent(state$selected_servers, {
      res <- raw_coverage()
      if (is.null(res)) return()
      coverage_data(.extract_display_data(res, "all",
        state$selected_servers, intersect_only = FALSE))
    }, ignoreInit = TRUE)

    output$coverage_dt <- DT::renderDT({
      df <- coverage_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 20, dom = "ft", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })

    # Load missingness
    shiny::observeEvent(input$miss_btn, {
      shiny::withProgress(message = "Checking missingness...", value = 0.3, {
        tryCatch({
          res <- ds.omop.missingness(input$miss_table,
                                      symbol = state$symbol)
          shiny::incProgress(0.5)
          if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, res$meta$call_code)
          }
          raw_miss(res)
          # Always show all servers with server column for comparison
          miss_data(.extract_display_data(res, "all",
            state$selected_servers, intersect_only = FALSE))
        }, error = function(e) {
          shiny::showNotification(
            paste("Error:", conditionMessage(e)), type = "error"
          )
        })
      })
    })

    # Re-extract missingness on server change
    shiny::observeEvent(state$selected_servers, {
      res <- raw_miss()
      if (is.null(res)) return()
      miss_data(.extract_display_data(res, "all",
        state$selected_servers, intersect_only = FALSE))
    }, ignoreInit = TRUE)

    output$miss_dt <- DT::renderDT({
      df <- miss_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 25, dom = "ft", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })
  })
}
