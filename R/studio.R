# ==============================================================================
# dsOMOPClient v2 - OMOP Studio (Shiny App)
# ==============================================================================
# An ATLAS-like interactive exploration + plan authoring tool.
# All data fetched via dsOMOPClient CLI -> DataSHIELD aggregate endpoints.
# ==============================================================================

#' Launch OMOP Studio
#'
#' Opens an interactive Shiny app for OMOP CDM exploration, vocabulary
#' browsing, concept prevalence analysis, cohort management, and plan
#' authoring. Requires an active DataSHIELD session created via
#' \code{\link{ds.omop.connect}}.
#'
#' @param symbol Character; OMOP session symbol (default "omop")
#' @param launch.browser Logical; open in browser (default TRUE)
#' @return A Shiny app object (runs interactively)
#' @export
ds.omop.studio <- function(symbol = "omop", launch.browser = TRUE) {
  app <- shiny::shinyApp(
    ui = .studio_ui(symbol),
    server = .studio_server(symbol)
  )
  shiny::runApp(app, launch.browser = launch.browser)
}

# ==============================================================================
# UI
# ==============================================================================

.studio_ui <- function(symbol) {
  function(request) {
    bslib::page_navbar(
      title = "OMOP Studio",
      theme = bslib::bs_theme(
        version = 5, bootswatch = "flatly",
        "navbar-bg" = "#2c3e50"
      ),
      header = shiny::tags$style(shiny::HTML("
        .card { margin-bottom: 1rem; }
        .status-ok { color: #27ae60; font-weight: bold; }
        .status-warn { color: #f39c12; font-weight: bold; }
        .status-err { color: #e74c3c; font-weight: bold; }
        .code-output { background: #2d2d2d; color: #f8f8f2;
                       padding: 1em; border-radius: 6px;
                       font-family: 'Fira Code', monospace;
                       font-size: 0.85em; white-space: pre-wrap;
                       max-height: 500px; overflow-y: auto; }
        .metric-card { text-align: center; padding: 1.5em; }
        .metric-card .value { font-size: 2em; font-weight: bold;
                              color: #2c3e50; }
        .metric-card .label { color: #7f8c8d; font-size: 0.9em; }
        .suppressed { color: #e74c3c; font-style: italic; }
      ")),

      # --- Tab A: Connections ---
      bslib::nav_panel("Connections", icon = shiny::icon("plug"),
        .mod_connections_ui("conn")
      ),

      # --- Tab B: Catalog ---
      bslib::nav_panel("Catalog", icon = shiny::icon("database"),
        .mod_catalog_ui("catalog")
      ),

      # --- Tab C: Vocabulary ---
      bslib::nav_panel("Vocabulary", icon = shiny::icon("book"),
        .mod_vocab_ui("vocab")
      ),

      # --- Tab D: Observed Concepts ---
      bslib::nav_panel("Concepts", icon = shiny::icon("chart-bar"),
        .mod_observed_ui("observed")
      ),

      # --- Tab E: Values ---
      bslib::nav_panel("Values", icon = shiny::icon("chart-line"),
        .mod_values_ui("values")
      ),

      # --- Tab F: Trends ---
      bslib::nav_panel("Trends", icon = shiny::icon("clock"),
        .mod_trends_ui("trends")
      ),

      # --- Tab G: Cohorts ---
      bslib::nav_panel("Cohorts", icon = shiny::icon("users"),
        .mod_cohorts_ui("cohorts")
      ),

      # --- Tab H: Plan Builder ---
      bslib::nav_panel("Plan Builder", icon = shiny::icon("hammer"),
        .mod_plans_ui("plans")
      ),

      # --- Tab I: Session ---
      bslib::nav_panel("Session", icon = shiny::icon("server"),
        .mod_session_ui("session_tab")
      )
    )
  }
}

# ==============================================================================
# SERVER
# ==============================================================================

.studio_server <- function(symbol) {
  function(input, output, session) {
    # Shared reactive state
    state <- shiny::reactiveValues(
      symbol = symbol,
      status = NULL,
      tables = NULL,
      concept_set = integer(0),
      plan = ds.omop.plan(),
      plan_outputs = list()
    )

    # Load initial data on startup
    shiny::observe({
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
        state$tables <- ds.omop.tables(symbol = state$symbol)
      }, error = function(e) {
        shiny::showNotification(
          paste("Connection error:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # Module servers
    .mod_connections_server("conn", state)
    .mod_catalog_server("catalog", state)
    .mod_vocab_server("vocab", state)
    .mod_observed_server("observed", state)
    .mod_values_server("values", state)
    .mod_trends_server("trends", state)
    .mod_cohorts_server("cohorts", state)
    .mod_plans_server("plans", state)
    .mod_session_server("session_tab", state)
  }
}

# ==============================================================================
# Helper: extract CDM table names with has_person_id from state$tables
# ==============================================================================

.get_person_tables <- function(tables) {
  if (is.null(tables)) return(character(0))
  srv_name <- names(tables)[1]
  df <- tables[[srv_name]]
  if (!is.data.frame(df)) return(character(0))
  # Filter to CDM tables with has_person_id = TRUE
  mask <- rep(TRUE, nrow(df))
  if ("schema_category" %in% names(df)) {
    mask <- mask & (df$schema_category == "CDM")
  }
  if ("has_person_id" %in% names(df)) {
    mask <- mask & isTRUE_vec(df$has_person_id)
  }
  sort(df$table_name[mask])
}

# Vectorised isTRUE
isTRUE_vec <- function(x) {
  !is.na(x) & x == TRUE
}

# ==============================================================================
# MODULE A: Connections
# ==============================================================================

.mod_connections_ui <- function(id) {
  ns <- shiny::NS(id)
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
      bslib::card_header("Disclosure Settings"),
      bslib::card_body(
        shiny::htmlOutput(ns("disclosure_info"))
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
        # Check ping
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

        # Build info items
        items <- list(shiny::p(shiny::strong("Status: "),
                               shiny::span(status_text, class = status_class)))

        # Capabilities (may be NULL)
        caps <- if (!is.null(st$capabilities)) st$capabilities[[srv]] else NULL
        if (!is.null(caps)) {
          if (!is.null(caps$dbms))
            items <- c(items, list(shiny::p(shiny::strong("DBMS: "),
                                            as.character(caps$dbms))))
          if (!is.null(caps$cdm_schema))
            items <- c(items, list(shiny::p(shiny::strong("CDM Schema: "),
                                            as.character(caps$cdm_schema))))
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
        shiny::tags$dt(class = "col-sm-6", "privacy level"),
        shiny::tags$dd(class = "col-sm-6",
          as.character(getOption("datashield.privacyControlLevel",
            "banana")))
      )
    })
  })
}

# ==============================================================================
# MODULE B: Catalog Explorer
# ==============================================================================

.mod_catalog_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Catalog",
      shiny::selectInput(ns("schema_filter"), "Schema Category",
        choices = c("All", "CDM", "Vocabulary", "Results"),
        selected = "CDM"
      ),
      shiny::textInput(ns("table_search"), "Search tables",
                       placeholder = "Type to filter..."),
      shiny::hr(),
      # FIX #1: replaced actionLink list with a selectInput dropdown
      shiny::selectInput(ns("selected_table"), "Select Table",
                         choices = NULL)
    ),
    bslib::card(
      bslib::card_header(shiny::textOutput(ns("selected_table_title"))),
      bslib::card_body(
        DT::DTOutput(ns("columns_table"))
      )
    ),
    bslib::card(
      bslib::card_header("Table Statistics"),
      bslib::card_body(
        shiny::uiOutput(ns("table_stats"))
      )
    )
  )
}

.mod_catalog_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filtered_tables <- shiny::reactive({
      tbls <- state$tables
      if (is.null(tbls)) return(NULL)
      # Use first server
      srv_name <- names(tbls)[1]
      df <- tbls[[srv_name]]
      if (!is.data.frame(df)) return(NULL)

      if (input$schema_filter != "All") {
        df <- df[df$schema_category == input$schema_filter, , drop = FALSE]
      }
      search <- tolower(input$table_search)
      if (nchar(search) > 0) {
        df <- df[grepl(search, df$table_name, fixed = TRUE), , drop = FALSE]
      }
      df
    })

    # FIX #1: Update the selectInput dropdown when the filtered table list
    # changes, instead of creating observeEvent inside observe.
    shiny::observe({
      df <- filtered_tables()
      if (is.null(df) || nrow(df) == 0) {
        choices <- stats::setNames(character(0), character(0))
      } else {
        # Build labels with a badge hint for has_person_id
        labels <- vapply(seq_len(nrow(df)), function(i) {
          tbl <- df$table_name[i]
          if (isTRUE(df$has_person_id[i])) {
            paste0(tbl, " [person_id]")
          } else {
            tbl
          }
        }, character(1))
        choices <- stats::setNames(df$table_name, labels)
      }
      shiny::updateSelectInput(session, "selected_table", choices = choices)
    })

    output$selected_table_title <- shiny::renderText({
      tbl <- input$selected_table
      if (is.null(tbl) || nchar(tbl) == 0) {
        "Select a table"
      } else {
        paste("Columns:", tbl)
      }
    })

    output$columns_table <- DT::renderDT({
      tbl <- input$selected_table
      if (is.null(tbl) || nchar(tbl) == 0) return(NULL)
      tryCatch({
        cols <- ds.omop.columns(tbl, symbol = state$symbol)
        srv <- names(cols)[1]
        df <- cols[[srv]]
        if (is.data.frame(df)) {
          DT::datatable(df, options = list(pageLength = 25, dom = "ft"),
                        rownames = FALSE, selection = "none")
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
        NULL
      })
    })

    output$table_stats <- shiny::renderUI({
      tbl <- input$selected_table
      if (is.null(tbl) || nchar(tbl) == 0) {
        return(shiny::p("Select a table to see statistics."))
      }
      tryCatch({
        stats_res <- ds.omop.table.stats(tbl, stats = c("rows", "persons"),
                                          symbol = state$symbol)
        srv <- names(stats_res)[1]
        s <- stats_res[[srv]]
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
  })
}

# ==============================================================================
# MODULE C: Vocabulary Browser
# ==============================================================================

.mod_vocab_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Search",
      shiny::textInput(ns("search_pattern"), "Search",
                       placeholder = "e.g. diabetes"),
      shiny::selectInput(ns("domain_filter"), "Domain",
        choices = c("All" = "", "Condition", "Drug", "Measurement",
                    "Observation", "Procedure", "Visit"),
        selected = ""),
      shiny::checkboxInput(ns("standard_only"), "Standard only", TRUE),
      shiny::numericInput(ns("limit"), "Max results", 50, 10, 500, 10),
      shiny::actionButton(ns("search_btn"), "Search",
                          class = "btn-primary w-100"),
      shiny::hr(),
      shiny::h6("Concept Set"),
      shiny::verbatimTextOutput(ns("concept_set_ids")),
      shiny::actionButton(ns("clear_set"), "Clear Set",
                          class = "btn-sm btn-outline-danger")
    ),
    bslib::card(
      bslib::card_header("Search Results"),
      bslib::card_body(
        DT::DTOutput(ns("results_table"))
      )
    ),
    bslib::card(
      bslib::card_header("Concept Details"),
      bslib::card_body(
        shiny::uiOutput(ns("concept_detail"))
      )
    )
  )
}

.mod_vocab_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    search_results <- shiny::reactiveVal(NULL)

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
        srv <- names(res)[1]
        search_results(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Search error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$results_table <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      keep <- intersect(c("concept_id", "concept_name", "domain_id",
                           "vocabulary_id", "standard_concept"),
                        names(df))
      DT::datatable(
        df[, keep, drop = FALSE],
        options = list(pageLength = 15, dom = "ftip"),
        rownames = FALSE, selection = "single",
        callback = DT::JS(
          "table.on('click', 'tr', function() {",
          "  var data = table.row(this).data();",
          "  if (data) Shiny.setInputValue('", session$ns("selected_row"),
          "', data[0], {priority: 'event'});",
          "});"
        )
      )
    })

    # Add to concept set on row select
    shiny::observeEvent(input$results_table_rows_selected, {
      df <- search_results()
      idx <- input$results_table_rows_selected
      if (!is.null(idx) && !is.null(df) && idx <= nrow(df)) {
        cid <- as.integer(df$concept_id[idx])
        current <- state$concept_set
        if (!cid %in% current) {
          state$concept_set <- c(current, cid)
          shiny::showNotification(
            paste("Added concept", cid, "to set"),
            type = "message", duration = 2
          )
        }
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
      idx <- input$results_table_rows_selected
      if (is.null(idx) || is.null(df) || idx > nrow(df)) {
        return(shiny::p("Click a concept to see details."))
      }
      row <- df[idx, ]
      shiny::tags$dl(class = "row",
        shiny::tags$dt(class = "col-sm-4", "Concept ID"),
        shiny::tags$dd(class = "col-sm-8", as.character(row$concept_id)),
        shiny::tags$dt(class = "col-sm-4", "Name"),
        shiny::tags$dd(class = "col-sm-8",
          as.character(row$concept_name)),
        shiny::tags$dt(class = "col-sm-4", "Domain"),
        shiny::tags$dd(class = "col-sm-8",
          as.character(row$domain_id)),
        shiny::tags$dt(class = "col-sm-4", "Vocabulary"),
        shiny::tags$dd(class = "col-sm-8",
          as.character(row$vocabulary_id %||% "")),
        shiny::tags$dt(class = "col-sm-4", "Standard"),
        shiny::tags$dd(class = "col-sm-8",
          as.character(row$standard_concept %||% ""))
      )
    })
  })
}

# ==============================================================================
# MODULE D: Observed Concepts
# ==============================================================================

.mod_observed_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Prevalence",
      # FIX #2: dynamic table dropdown, populated from state$tables
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("metric"), "Metric",
        choices = c("Distinct Persons" = "persons",
                    "Total Records" = "records")),
      shiny::numericInput(ns("top_n"), "Top N", 30, 5, 200, 5),
      shiny::actionButton(ns("run_btn"), "Run",
                          class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header(
        shiny::textOutput(ns("results_title"))
      ),
      bslib::card_body(
        shiny::plotOutput(ns("bar_chart"), height = "400px"),
        DT::DTOutput(ns("results_dt"))
      )
    )
  )
}

.mod_observed_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    prevalence_data <- shiny::reactiveVal(NULL)

    # FIX #2: dynamically populate table dropdown from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) == 0) {
        tbl_choices <- c("condition_occurrence", "drug_exposure",
                         "measurement", "procedure_occurrence",
                         "observation", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = tbl_choices)
    })

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table)
      shiny::showNotification("Querying...", type = "message",
                              duration = 2, id = "prev_loading")
      tryCatch({
        res <- ds.omop.concept.prevalence(
          table = input$table, metric = input$metric,
          top_n = input$top_n, symbol = state$symbol
        )
        srv <- names(res)[1]
        prevalence_data(res[[srv]])
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

      # Prepare label
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

    output$results_dt <- DT::renderDT({
      df <- prevalence_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 20, dom = "ftip"),
                    rownames = FALSE, selection = "none")
    })
  })
}

# ==============================================================================
# MODULE E: Values / Distributions
# ==============================================================================

.mod_values_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Value Explorer",
      # FIX #2/#3: dynamic table dropdown, populated from state$tables
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      # FIX #3: replaced textInput with selectInput for column
      shiny::selectInput(ns("column"), "Column", choices = NULL),
      shiny::radioButtons(ns("mode"), "Analysis",
        choices = c("Value Counts" = "counts",
                    "Histogram" = "histogram",
                    "Quantiles" = "quantiles")),
      shiny::conditionalPanel(
        paste0("input['", ns("mode"), "'] == 'histogram'"),
        shiny::numericInput(ns("bins"), "Bins", 20, 5, 100, 5)
      ),
      shiny::actionButton(ns("run_btn"), "Run",
                          class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header("Results"),
      bslib::card_body(
        shiny::plotOutput(ns("value_plot"), height = "350px"),
        DT::DTOutput(ns("value_dt"))
      )
    )
  )
}

.mod_values_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    result_data <- shiny::reactiveVal(NULL)
    result_mode <- shiny::reactiveVal("counts")

    # FIX #2: dynamically populate table dropdown from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) == 0) {
        tbl_choices <- c("measurement", "condition_occurrence",
                         "drug_exposure", "observation",
                         "procedure_occurrence", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = tbl_choices)
    })

    # FIX #3: when the table changes, fetch its columns and populate the
    # column selectInput. Default to "value_as_number" if present.
    shiny::observeEvent(input$table, {
      tbl <- input$table
      if (is.null(tbl) || nchar(tbl) == 0) return()
      tryCatch({
        cols_res <- ds.omop.columns(tbl, symbol = state$symbol)
        srv <- names(cols_res)[1]
        col_df <- cols_res[[srv]]
        if (is.data.frame(col_df) && "column_name" %in% names(col_df)) {
          col_names <- sort(col_df$column_name)
          selected <- if ("value_as_number" %in% col_names) {
            "value_as_number"
          } else {
            col_names[1]
          }
          shiny::updateSelectInput(session, "column",
                                   choices = col_names,
                                   selected = selected)
        } else {
          shiny::updateSelectInput(session, "column", choices = character(0))
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading columns:", conditionMessage(e)),
          type = "error"
        )
        shiny::updateSelectInput(session, "column", choices = character(0))
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$column)
      result_mode(input$mode)

      tryCatch({
        res <- if (input$mode == "counts") {
          ds.omop.value.counts(input$table, input$column,
                               top_n = 30, symbol = state$symbol)
        } else if (input$mode == "histogram") {
          ds.omop.value.histogram(input$table, input$column,
                                   bins = input$bins,
                                   symbol = state$symbol)
        } else {
          ds.omop.value.quantiles(input$table, input$column,
                                   symbol = state$symbol)
        }
        srv <- names(res)[1]
        result_data(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$value_plot <- shiny::renderPlot({
      df <- result_data()
      mode <- result_mode()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      if (mode == "counts") {
        df$y <- as.numeric(df$n)
        df <- df[!is.na(df$y), , drop = FALSE]
        if (nrow(df) == 0) return(NULL)
        n <- min(nrow(df), 15)
        df <- df[seq_len(n), ]
        par(mar = c(5, 10, 2, 2))
        barplot(rev(df$y), names.arg = rev(substr(df$value, 1, 30)),
                horiz = TRUE, las = 1, col = "#2ecc71",
                xlab = "Count", cex.names = 0.75)
      } else if (mode == "histogram") {
        if (!"count" %in% names(df)) return(NULL)
        cols <- ifelse(is.na(df$count) | df$suppressed, "#e74c3c", "#3498db")
        y <- df$count; y[is.na(y)] <- 0
        mids <- (df$bin_start + df$bin_end) / 2
        par(mar = c(5, 5, 2, 2))
        barplot(y, names.arg = round(mids, 1), col = cols,
                xlab = input$column, ylab = "Count",
                las = 2, cex.names = 0.7)
        legend("topright", legend = c("OK", "Suppressed"),
               fill = c("#3498db", "#e74c3c"), cex = 0.8)
      } else if (mode == "quantiles") {
        if (!"value" %in% names(df)) return(NULL)
        par(mar = c(5, 5, 2, 2))
        plot(df$probability * 100, df$value, type = "b", pch = 19,
             col = "#8e44ad", xlab = "Percentile", ylab = "Value",
             main = paste("Quantiles:", input$column))
      }
    })

    output$value_dt <- DT::renderDT({
      df <- result_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 20, dom = "ftip"),
                    rownames = FALSE, selection = "none")
    })
  })
}

# ==============================================================================
# MODULE F: Trends
# ==============================================================================

.mod_trends_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Time Trends",
      # FIX #4: dynamic table dropdown, populated from state$tables
      shiny::selectInput(ns("table"), "Table", choices = NULL),
      shiny::selectInput(ns("granularity"), "Granularity",
        choices = c("Year" = "year", "Quarter" = "quarter",
                    "Month" = "month")),
      shiny::actionButton(ns("run_btn"), "Run",
                          class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header("Record Counts Over Time"),
      bslib::card_body(
        shiny::plotOutput(ns("trend_plot"), height = "400px"),
        DT::DTOutput(ns("trend_dt"))
      )
    )
  )
}

.mod_trends_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    trend_data <- shiny::reactiveVal(NULL)

    # FIX #4: dynamically populate table dropdown from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) == 0) {
        tbl_choices <- c("condition_occurrence", "drug_exposure",
                         "measurement", "procedure_occurrence",
                         "observation", "visit_occurrence")
      }
      shiny::updateSelectInput(session, "table", choices = tbl_choices)
    })

    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table)
      tryCatch({
        res <- ds.omop.date.counts(
          table = input$table,
          granularity = input$granularity,
          symbol = state$symbol
        )
        srv <- names(res)[1]
        trend_data(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$trend_plot <- shiny::renderPlot({
      df <- trend_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      if (!"n_records" %in% names(df)) return(NULL)

      df <- df[order(df$period), ]
      y <- as.numeric(df$n_records); y[is.na(y)] <- 0
      cols <- ifelse(is.na(df$n_records) | df$suppressed,
                     "#e74c3c", "#2c3e50")
      par(mar = c(7, 5, 2, 2))
      barplot(y, names.arg = df$period, col = cols,
              ylab = "Records", las = 2, cex.names = 0.7)
    })

    output$trend_dt <- DT::renderDT({
      df <- trend_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 25, dom = "ftip"),
                    rownames = FALSE, selection = "none")
    })
  })
}

# ==============================================================================
# MODULE G: Cohorts
# ==============================================================================

.mod_cohorts_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Available Cohorts"),
      bslib::card_body(
        shiny::actionButton(ns("refresh"), "Refresh",
                            class = "btn-sm btn-outline-primary mb-2"),
        DT::DTOutput(ns("cohort_list"))
      )
    ),
    bslib::card(
      bslib::card_header("Set Active Cohort"),
      bslib::card_body(
        shiny::numericInput(ns("cohort_id"), "Cohort Definition ID",
                            value = 1, min = 1, step = 1),
        shiny::actionButton(ns("set_cohort"), "Set on Plan",
                            class = "btn-primary"),
        shiny::hr(),
        shiny::h6("Current Plan Cohort"),
        shiny::verbatimTextOutput(ns("current_cohort"))
      )
    )
  )
}

.mod_cohorts_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    cohort_data <- shiny::reactiveVal(NULL)

    refresh_cohorts <- function() {
      tryCatch({
        res <- ds.omop.cohort.list(symbol = state$symbol)
        srv <- names(res)[1]
        cohort_data(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    }

    shiny::observe(refresh_cohorts())
    shiny::observeEvent(input$refresh, refresh_cohorts())

    output$cohort_list <- DT::renderDT({
      df <- cohort_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 10, dom = "ftip"),
                    rownames = FALSE, selection = "single")
    })

    shiny::observeEvent(input$set_cohort, {
      cid <- as.integer(input$cohort_id)
      state$plan <- ds.omop.plan.cohort(state$plan,
                                         cohort_definition_id = cid)
      shiny::showNotification(
        paste("Cohort", cid, "set on plan"),
        type = "message", duration = 3
      )
    })

    output$current_cohort <- shiny::renderText({
      p <- state$plan
      if (is.null(p$cohort)) return("None")
      paste("Cohort ID:", p$cohort$cohort_definition_id %||% "custom spec")
    })
  })
}

# ==============================================================================
# MODULE H: Plan Builder
# ==============================================================================

.mod_plans_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Add Output", width = 320,
      shiny::selectInput(ns("output_type"), "Type",
        choices = c("Baseline" = "baseline",
                    "Events (long)" = "event_level",
                    "Events (sparse)" = "sparse",
                    "Survival" = "survival",
                    "Cohort Membership" = "cohort_membership",
                    "Intervals Long" = "intervals_long",
                    "Temporal Covariates" = "temporal_covariates",
                    "Concept Dictionary" = "concept_dictionary")),
      shiny::textInput(ns("output_name"), "Output Name",
                       value = "output_1"),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"),
               "'] == 'event_level' || input['",
               ns("output_type"), "'] == 'sparse'"),
        shiny::selectInput(ns("event_table"), "Table",
          choices = c("condition_occurrence", "drug_exposure",
                      "measurement", "procedure_occurrence",
                      "observation", "visit_occurrence")),
        shiny::textInput(ns("concept_ids"), "Concept IDs (comma-sep)",
                         placeholder = "201820, 255573")
      ),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"), "'] == 'survival'"),
        shiny::selectInput(ns("outcome_table"), "Outcome Table",
          choices = c("condition_occurrence", "drug_exposure",
                      "measurement")),
        shiny::textInput(ns("outcome_concepts"), "Outcome Concepts",
                         placeholder = "4000002"),
        shiny::numericInput(ns("tar_end"), "TAR End (days)", 730, 30, 3650)
      ),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"),
               "'] == 'temporal_covariates'"),
        shiny::selectInput(ns("tc_table"), "Table",
          choices = c("condition_occurrence", "drug_exposure",
                      "measurement")),
        shiny::textInput(ns("tc_concepts"), "Concept IDs",
                         placeholder = "201820, 255573"),
        shiny::numericInput(ns("tc_bin"), "Bin Width (days)", 30, 7, 365),
        shiny::numericInput(ns("tc_start"), "Window Start", -365),
        shiny::numericInput(ns("tc_end"), "Window End", 0)
      ),
      shiny::actionButton(ns("add_output"), "Add Output",
                          class = "btn-primary w-100 mt-2")
    ),
    bslib::card(
      bslib::card_header("Current Plan"),
      bslib::card_body(
        shiny::verbatimTextOutput(ns("plan_summary")),
        shiny::actionButton(ns("clear_plan"), "Clear Plan",
                            class = "btn-sm btn-outline-danger")
      )
    ),
    bslib::card(
      bslib::card_header("Generated R Code"),
      bslib::card_body(
        shiny::actionButton(ns("generate_code"), "Generate Code",
                            class = "btn-success mb-2"),
        shiny::uiOutput(ns("code_block"))
      )
    )
  )
}

.mod_plans_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$add_output, {
      otype <- input$output_type
      nm <- input$output_name
      p <- state$plan

      tryCatch({
        if (otype == "baseline") {
          p <- ds.omop.plan.baseline(p, name = nm)
        } else if (otype == "event_level") {
          cs <- .parse_ids(input$concept_ids)
          p <- ds.omop.plan.events(p, name = nm, table = input$event_table,
                                    concept_set = cs)
        } else if (otype == "sparse") {
          cs <- .parse_ids(input$concept_ids)
          p <- ds.omop.plan.events(p, name = nm, table = input$event_table,
                                    concept_set = cs,
                                    representation = list(format = "sparse"))
        } else if (otype == "survival") {
          oc <- .parse_ids(input$outcome_concepts)
          p <- ds.omop.plan.survival(p, outcome_table = input$outcome_table,
                                      outcome_concepts = oc,
                                      tar = list(start_offset = 0,
                                                 end_offset = input$tar_end),
                                      name = nm)
        } else if (otype == "cohort_membership") {
          p <- ds.omop.plan.cohort_membership(p, name = nm)
        } else if (otype == "intervals_long") {
          p <- ds.omop.plan.intervals(p, name = nm)
        } else if (otype == "temporal_covariates") {
          cs <- .parse_ids(input$tc_concepts)
          p <- ds.omop.plan.temporal_covariates(
            p, table = input$tc_table, concept_set = cs,
            bin_width = as.integer(input$tc_bin),
            window_start = as.integer(input$tc_start),
            window_end = as.integer(input$tc_end),
            name = nm
          )
        } else if (otype == "concept_dictionary") {
          p <- ds.omop.plan.concept_dictionary(p, name = nm)
        }

        state$plan <- p
        shiny::showNotification(paste("Added output:", nm),
                                type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    shiny::observeEvent(input$clear_plan, {
      state$plan <- ds.omop.plan()
    })

    output$plan_summary <- shiny::renderText({
      p <- state$plan
      paste(utils::capture.output(print(p)), collapse = "\n")
    })

    generated_code <- shiny::reactiveVal("")

    shiny::observeEvent(input$generate_code, {
      tryCatch({
        p <- state$plan
        out_names <- names(p$outputs)
        out <- stats::setNames(
          paste0("D_", out_names), out_names
        )
        code <- .studio_codegen_plan(p, out, symbol = state$symbol)
        generated_code(code)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$code_block <- shiny::renderUI({
      code <- generated_code()
      if (nchar(code) == 0) return(shiny::p("Click 'Generate Code'."))
      shiny::div(class = "code-output", code)
    })
  })
}

# Utility: parse comma-separated IDs
.parse_ids <- function(s) {
  if (is.null(s) || nchar(trimws(s)) == 0) return(NULL)
  ids <- trimws(strsplit(s, ",")[[1]])
  ids <- ids[nchar(ids) > 0]
  as.integer(ids)
}

# ==============================================================================
# MODULE I: Session Objects
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
        # Dynamic table dropdown for missingness, too
        shiny::selectInput(ns("miss_table"), "Table",
          choices = c("person", "condition_occurrence", "drug_exposure",
                      "measurement", "observation_period",
                      "visit_occurrence")),
        shiny::actionButton(ns("miss_btn"), "Check Missingness",
                            class = "btn-sm btn-outline-info mb-2"),
        DT::DTOutput(ns("miss_dt"))
      )
    )
  )
}

.mod_session_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$refresh, {
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
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

    coverage_data <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$coverage_btn, {
      tryCatch({
        res <- ds.omop.domain.coverage(symbol = state$symbol)
        srv <- names(res)[1]
        coverage_data(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$coverage_dt <- DT::renderDT({
      df <- coverage_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 20, dom = "ft"),
                    rownames = FALSE, selection = "none")
    })

    miss_data <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$miss_btn, {
      tryCatch({
        res <- ds.omop.missingness(input$miss_table,
                                    symbol = state$symbol)
        srv <- names(res)[1]
        miss_data(res[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$miss_dt <- DT::renderDT({
      df <- miss_data()
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 25, dom = "ft"),
                    rownames = FALSE, selection = "none")
    })
  })
}
