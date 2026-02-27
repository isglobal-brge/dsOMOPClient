# ==============================================================================
# MODULE 1: Connections (kept as-is)
# ==============================================================================

.mod_connections_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    bslib::layout_columns(
      col_widths = 12,
      bslib::card(
        bslib::card_header("Server Status"),
        bslib::card_body(
          shiny::actionButton(ns("refresh"), "Refresh",
                              class = "btn-sm btn-outline-primary mb-3"),
          shiny::uiOutput(ns("status_cards"))
        )
      ),
      bslib::card(
        bslib::card_header("Dataset Identity (CDM_SOURCE)"),
        bslib::card_body(
          shiny::uiOutput(ns("cdm_source_info"))
        )
      ),
      bslib::card(
        bslib::card_header("Disclosure Settings"),
        bslib::card_body(
          shiny::htmlOutput(ns("disclosure_info"))
        )
      )
    )
  )
}

.mod_connections_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$refresh, {
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
        state$tables <- ds.omop.tables(symbol = state$symbol)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$status_cards <- shiny::renderUI({
      st <- state$status
      if (is.null(st)) return(shiny::p("Loading..."))

      srv_names <- st$servers
      if (is.null(srv_names) || length(srv_names) == 0) {
        return(shiny::p("No servers found in status."))
      }

      cards <- lapply(srv_names, function(srv) {
        alive <- FALSE
        ping_entry <- if (!is.null(st$ping)) st$ping[[srv]] else NULL
        if (!is.null(ping_entry)) {
          if (is.list(ping_entry) && !is.null(ping_entry$alive)) {
            alive <- isTRUE(ping_entry$alive)
          } else if (is.logical(ping_entry)) {
            alive <- isTRUE(ping_entry)
          } else {
            alive <- TRUE
          }
        }

        status_class <- if (alive) "status-ok" else "status-err"
        status_text <- if (alive) "Connected" else "Error"

        items <- list(shiny::p(shiny::strong("Status: "),
                               shiny::span(status_text, class = status_class)))

        caps <- if (!is.null(st$capabilities)) st$capabilities[[srv]] else NULL
        if (!is.null(caps)) {
          # Show CDM source name prominently if available
          cdm_info <- caps$cdm_info
          if (!is.null(cdm_info) && !is.null(cdm_info$source_name)) {
            items <- c(items, list(shiny::p(shiny::strong("Database: "),
                                            shiny::em(as.character(cdm_info$source_name)))))
          }
          if (!is.null(caps$dbms))
            items <- c(items, list(shiny::p(shiny::strong("DBMS: "),
                                            as.character(caps$dbms))))
          if (!is.null(caps$n_tables))
            items <- c(items, list(shiny::p(shiny::strong("Tables: "),
                                            as.character(caps$n_tables))))
        }

        bslib::card(
          bslib::card_header(shiny::span(srv, class = status_class)),
          bslib::card_body(shiny::tagList(items))
        )
      })
      shiny::tagList(cards)
    })

    output$cdm_source_info <- shiny::renderUI({
      st <- state$status
      if (is.null(st) || is.null(st$capabilities)) {
        return(shiny::p("Loading CDM source information..."))
      }

      srv_panels <- lapply(names(st$capabilities), function(srv) {
        caps <- st$capabilities[[srv]]
        cdm_info <- caps$cdm_info
        if (is.null(cdm_info)) {
          return(shiny::p(shiny::strong(srv, ": "),
                          "CDM_SOURCE not available"))
        }

        items <- list()
        .add_item <- function(label, val) {
          if (!is.null(val) && nchar(as.character(val)) > 0) {
            items[[length(items) + 1]] <<- shiny::tagList(
              shiny::tags$dt(class = "col-sm-5", label),
              shiny::tags$dd(class = "col-sm-7", as.character(val))
            )
          }
        }
        .add_item("Source Name", cdm_info$source_name)
        .add_item("Abbreviation", cdm_info$source_abbreviation)
        .add_item("CDM Version", cdm_info$cdm_version)
        .add_item("Vocabulary Version", cdm_info$vocabulary_version)

        shiny::div(class = "cdm-info mb-3",
          shiny::h6(srv),
          shiny::tags$dl(class = "row", items)
        )
      })
      shiny::tagList(srv_panels)
    })

    output$disclosure_info <- shiny::renderUI({
      shiny::tags$dl(class = "row",
        shiny::tags$dt(class = "col-sm-6", "nfilter.tab"),
        shiny::tags$dd(class = "col-sm-6",
          as.character(getOption("nfilter.tab",
            getOption("default.nfilter.tab", 3)))),
        shiny::tags$dt(class = "col-sm-6", "nfilter.subset"),
        shiny::tags$dd(class = "col-sm-6",
          as.character(getOption("nfilter.subset",
            getOption("default.nfilter.subset", 3)))),
        shiny::tags$dt(class = "col-sm-6", "nfilter.levels.max"),
        shiny::tags$dd(class = "col-sm-6",
          as.character(getOption("nfilter.levels.max",
            getOption("default.nfilter.levels.max", 40)))),
      )
    })
  })
}

