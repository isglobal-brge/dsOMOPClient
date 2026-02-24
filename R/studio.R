# ==============================================================================
# dsOMOPClient v2 - OMOP Studio (Shiny App)
# ==============================================================================
# A concept-centric interactive exploration + plan authoring tool.
# Workflow: Table -> Concept Columns -> Observed Concepts -> Drilldown.
# All data fetched via dsOMOPClient CLI -> DataSHIELD aggregate endpoints.
# ==============================================================================

#' Launch OMOP Studio
#'
#' Opens an interactive Shiny app for OMOP CDM exploration, vocabulary
#' browsing, concept drilldown analysis, cohort management, and plan
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
      id = "main_nav",
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
        .clickable-row { cursor: pointer; }
        .clickable-row:hover { background-color: #ecf0f1 !important; }
        .concept-badge { display: inline-block; padding: 0.3em 0.6em;
                         background: #3498db; color: white;
                         border-radius: 4px; font-size: 0.85em;
                         margin: 0.2em; }
      ")),

      # --- Tab 1: Connections ---
      bslib::nav_panel("Connections", icon = shiny::icon("plug"),
        .mod_connections_ui("conn")
      ),

      # --- Tab 2: Explore ---
      bslib::nav_panel("Explore", icon = shiny::icon("compass"),
        .mod_table_concepts_ui("explore")
      ),

      # --- Tab 3: Drilldown ---
      bslib::nav_panel("Drilldown", icon = shiny::icon("magnifying-glass-chart"),
        .mod_concept_drilldown_ui("drilldown")
      ),

      # --- Tab 4: Locator ---
      bslib::nav_panel("Locator", icon = shiny::icon("location-dot"),
        .mod_concept_locator_ui("locator")
      ),

      # --- Tab 5: Vocabulary ---
      bslib::nav_panel("Vocabulary", icon = shiny::icon("book"),
        .mod_vocab_ui("vocab")
      ),

      # --- Tab 6: Plan Builder ---
      bslib::nav_panel("Plan Builder", icon = shiny::icon("hammer"),
        .mod_plan_from_explorer_ui("plans")
      ),

      # --- Tab 7: Session ---
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
      selected_table = NULL,
      selected_concept_col = NULL,
      selected_concept_id = NULL,
      selected_concept_name = NULL,
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
    .mod_table_concepts_server("explore", state, session)
    .mod_concept_drilldown_server("drilldown", state)
    .mod_concept_locator_server("locator", state)
    .mod_vocab_server("vocab", state)
    .mod_plan_from_explorer_server("plans", state)
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

# Helper: get concept columns from a table's column metadata
.get_concept_columns <- function(col_df) {
  if (is.null(col_df) || !is.data.frame(col_df)) return(character(0))
  if (!"concept_role" %in% names(col_df)) return(character(0))
  col_df$column_name[col_df$concept_role != "non_concept"]
}

# Utility: parse comma-separated IDs
.parse_ids <- function(s) {
  if (is.null(s) || nchar(trimws(s)) == 0) return(NULL)
  ids <- trimws(strsplit(s, ",")[[1]])
  ids <- ids[nchar(ids) > 0]
  as.integer(ids)
}

# Utility: format number with suppression check
.fmt_count <- function(x) {
  if (is.null(x) || is.na(x)) {
    shiny::span(class = "suppressed", "suppressed")
  } else {
    format(x, big.mark = ",")
  }
}

# ==============================================================================
# MODULE 1: Connections (kept as-is)
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
                          class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header("Table Statistics"),
      bslib::card_body(
        shiny::uiOutput(ns("table_stats"))
      )
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

    # Run concept prevalence
    shiny::observeEvent(input$run_btn, {
      shiny::req(input$table, input$concept_col)
      shiny::showNotification("Querying...", type = "message",
                              duration = 2, id = "prev_loading")
      tryCatch({
        res <- ds.omop.concept.prevalence(
          table = input$table, concept_col = input$concept_col,
          metric = input$metric, top_n = input$top_n,
          symbol = state$symbol
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
                    rownames = FALSE, selection = "single")
    })

    # Row click -> navigate to Drilldown
    shiny::observeEvent(input$results_dt_rows_selected, {
      df <- prevalence_data()
      idx <- input$results_dt_rows_selected
      if (!is.null(idx) && !is.null(df) && idx <= nrow(df)) {
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
  })
}

# ==============================================================================
# MODULE 3: Concept Drilldown
# Replaces old Values + Trends tabs
# ==============================================================================

.mod_concept_drilldown_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Info", width = 300,
      shiny::uiOutput(ns("concept_info")),
      shiny::hr(),
      shiny::actionButton(ns("add_to_set"), "Add to Concept Set",
                          class = "btn-outline-primary w-100 mb-2"),
      shiny::actionButton(ns("add_to_plan"), "Add to Plan",
                          class = "btn-outline-success w-100 mb-2"),
      shiny::actionButton(ns("reload"), "Reload",
                          class = "btn-outline-secondary w-100")
    ),

    # Summary metrics
    bslib::card(
      bslib::card_header("Summary"),
      bslib::card_body(
        shiny::uiOutput(ns("summary_metrics"))
      )
    ),

    # Numeric distribution (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_numeric"), "']"),
      bslib::card(
        bslib::card_header("Numeric Distribution"),
        bslib::card_body(
          shiny::plotOutput(ns("histogram_plot"), height = "300px"),
          DT::DTOutput(ns("quantiles_dt"))
        )
      )
    ),

    # Categorical values (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_categorical"), "']"),
      bslib::card(
        bslib::card_header("Categorical Values"),
        bslib::card_body(
          shiny::plotOutput(ns("categorical_plot"), height = "300px"),
          DT::DTOutput(ns("categorical_dt"))
        )
      )
    ),

    # Date coverage (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_dates"), "']"),
      bslib::card(
        bslib::card_header("Date Coverage"),
        bslib::card_body(
          shiny::plotOutput(ns("date_plot"), height = "250px"),
          shiny::uiOutput(ns("date_range_info"))
        )
      )
    ),

    # Missingness (always shown)
    bslib::card(
      bslib::card_header("Missingness"),
      bslib::card_body(
        shiny::plotOutput(ns("missingness_plot"), height = "250px")
      )
    )
  )
}

.mod_concept_drilldown_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    drilldown_data <- shiny::reactiveVal(NULL)

    # Auto-trigger when concept changes
    shiny::observeEvent(state$selected_concept_id, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()
      .run_drilldown(state, drilldown_data)
    })

    shiny::observeEvent(input$reload, {
      .run_drilldown(state, drilldown_data)
    })

    .run_drilldown <- function(state, drilldown_data) {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()

      shiny::showNotification("Loading drilldown...", type = "message",
                              duration = 3, id = "dd_loading")
      tryCatch({
        res <- ds.omop.concept.drilldown(
          table = tbl, concept_id = cid,
          symbol = state$symbol
        )
        drilldown_data(res)
        shiny::removeNotification("dd_loading")
      }, error = function(e) {
        shiny::removeNotification("dd_loading")
        shiny::showNotification(
          paste("Drilldown error:", conditionMessage(e)), type = "error"
        )
        drilldown_data(NULL)
      })
    }

    # Concept info sidebar
    output$concept_info <- shiny::renderUI({
      cid <- state$selected_concept_id
      if (is.null(cid)) {
        return(shiny::p("No concept selected.",
                        shiny::br(),
                        "Use the Explore tab to select a concept."))
      }
      shiny::tagList(
        shiny::p(shiny::strong("Concept ID: "), as.character(cid)),
        shiny::p(shiny::strong("Name: "),
                 as.character(state$selected_concept_name %||% "")),
        shiny::p(shiny::strong("Table: "),
                 as.character(state$selected_table %||% ""))
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
          paste("Error:", conditionMessage(e)), type = "error"
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
        return(shiny::p("Select a concept from the Explore tab."))
      }
      srv <- names(dd)[1]
      d <- dd[[srv]]
      s <- d$summary

      metrics <- list(
        shiny::div(class = "metric-card d-inline-block mx-3",
          shiny::div(class = "value", .fmt_count(s$n_records)),
          shiny::div(class = "label", "Records")
        ),
        shiny::div(class = "metric-card d-inline-block mx-3",
          shiny::div(class = "value", .fmt_count(s$n_persons)),
          shiny::div(class = "label", "Persons")
        )
      )

      if (!is.null(s$records_per_person_mean) && !is.na(s$records_per_person_mean)) {
        metrics <- c(metrics, list(
          shiny::div(class = "metric-card d-inline-block mx-3",
            shiny::div(class = "value",
                       format(round(s$records_per_person_mean, 2), nsmall = 2)),
            shiny::div(class = "label", "Records/Person")
          )
        ))
      }

      if (!is.null(s$pct_persons_multi) && !is.na(s$pct_persons_multi)) {
        metrics <- c(metrics, list(
          shiny::div(class = "metric-card d-inline-block mx-3",
            shiny::div(class = "value",
                       paste0(format(s$pct_persons_multi, nsmall = 1), "%")),
            shiny::div(class = "label", "Multi-record Persons")
          )
        ))
      }

      shiny::div(class = "d-flex flex-wrap justify-content-center", metrics)
    })

    # --- Numeric histogram ---
    output$histogram_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$numeric_summary) || is.null(d$numeric_summary$histogram)) {
        return(NULL)
      }
      df <- d$numeric_summary$histogram
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      cols <- ifelse(is.na(df$count) | df$suppressed, "#e74c3c", "#3498db")
      y <- df$count; y[is.na(y)] <- 0
      mids <- (df$bin_start + df$bin_end) / 2
      par(mar = c(5, 5, 2, 2))
      barplot(y, names.arg = round(mids, 1), col = cols,
              xlab = "value_as_number", ylab = "Count",
              las = 2, cex.names = 0.7)
      legend("topright", legend = c("OK", "Suppressed"),
             fill = c("#3498db", "#e74c3c"), cex = 0.8)
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
      DT::datatable(df, options = list(pageLength = 10, dom = "t"),
                    rownames = FALSE, selection = "none")
    })

    # --- Categorical values ---
    output$categorical_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      df$label <- substr(as.character(df$concept_name), 1, 30)
      df$y <- as.numeric(df$n)
      df <- df[!is.na(df$y), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)

      df <- df[order(df$y, decreasing = TRUE), ]
      n <- min(nrow(df), 15)
      df <- df[seq_len(n), ]

      par(mar = c(5, 10, 2, 2))
      barplot(rev(df$y), names.arg = rev(df$label),
              horiz = TRUE, las = 1, col = "#2ecc71",
              xlab = "Count", cex.names = 0.75)
    })

    output$categorical_dt <- DT::renderDT({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 10, dom = "ftip"),
                    rownames = FALSE, selection = "none")
    })

    # --- Date coverage ---
    output$date_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$date_range) || is.null(d$date_range$date_counts)) {
        return(NULL)
      }
      df <- d$date_range$date_counts
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      df <- df[order(df$period), ]
      y <- as.numeric(df$n_records); y[is.na(y)] <- 0
      cols <- ifelse(is.na(df$n_records) | df$suppressed,
                     "#e74c3c", "#2c3e50")
      par(mar = c(7, 5, 2, 2))
      barplot(y, names.arg = df$period, col = cols,
              ylab = "Records", las = 2, cex.names = 0.7)
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
    output$missingness_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$missingness
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      df <- df[order(df$missing_rate, decreasing = TRUE), ]
      par(mar = c(5, 12, 2, 2))
      barplot(rev(df$missing_rate * 100),
              names.arg = rev(df$column_name),
              horiz = TRUE, las = 1, col = "#e67e22",
              xlab = "Missing %", xlim = c(0, 100),
              cex.names = 0.7)
    })
  })
}

# ==============================================================================
# MODULE 4: Concept Locator
# ==============================================================================

.mod_concept_locator_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Locate Concepts", width = 300,
      shiny::textInput(ns("concept_ids"), "Concept IDs (comma-separated)",
                       placeholder = "201820, 255573"),
      shiny::actionButton(ns("locate_btn"), "Locate",
                          class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header("Concept Presence Matrix"),
      bslib::card_body(
        DT::DTOutput(ns("presence_dt"))
      )
    )
  )
}

.mod_concept_locator_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    locate_data <- shiny::reactiveVal(NULL)

    # Auto-populate from selected concept
    shiny::observe({
      cid <- state$selected_concept_id
      if (!is.null(cid)) {
        current_text <- input$concept_ids
        if (is.null(current_text) || nchar(trimws(current_text)) == 0) {
          shiny::updateTextInput(session, "concept_ids",
                                 value = as.character(cid))
        }
      }
    })

    shiny::observeEvent(input$locate_btn, {
      ids <- .parse_ids(input$concept_ids)
      if (is.null(ids) || length(ids) == 0) {
        shiny::showNotification("Enter at least one concept ID.",
                                type = "warning")
        return()
      }

      shiny::showNotification("Locating concepts...", type = "message",
                              duration = 3, id = "locate_loading")
      tryCatch({
        res <- ds.omop.concept.locate(
          concept_ids = ids, symbol = state$symbol
        )
        # Combine results from all servers with server column
        all_results <- data.frame(
          table_name = character(0), concept_column = character(0),
          concept_id = integer(0), n_records = numeric(0),
          n_persons = numeric(0), server = character(0),
          stringsAsFactors = FALSE
        )
        for (srv in names(res)) {
          df <- res[[srv]]
          if (is.data.frame(df) && nrow(df) > 0) {
            df$server <- srv
            all_results <- rbind(all_results, df)
          }
        }
        locate_data(all_results)
        shiny::removeNotification("locate_loading")
      }, error = function(e) {
        shiny::removeNotification("locate_loading")
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    output$presence_dt <- DT::renderDT({
      df <- locate_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      DT::datatable(df, options = list(pageLength = 25, dom = "ftip"),
                    rownames = FALSE, selection = "none")
    })
  })
}

# ==============================================================================
# MODULE 5: Vocabulary Browser (kept as-is)
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
# MODULE 6: Plan Builder (reworked with explorer integration)
# Merges old Plan Builder + Cohorts
# ==============================================================================

.mod_plan_from_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Add Output", width = 320,
      # Concept set display
      bslib::card(
        bslib::card_header("Current Concept Set"),
        bslib::card_body(
          shiny::uiOutput(ns("concept_set_display")),
          shiny::actionButton(ns("add_current_concept"), "Add Current Concept",
                              class = "btn-sm btn-outline-primary w-100 mb-2"),
          shiny::actionButton(ns("add_events_for_set"),
                              "Add Events for Concept Set",
                              class = "btn-sm btn-outline-info w-100")
        )
      ),
      shiny::hr(),
      # Cohort management
      shiny::numericInput(ns("cohort_id"), "Cohort Definition ID",
                          value = 1, min = 1, step = 1),
      shiny::actionButton(ns("set_cohort"), "Set Cohort on Plan",
                          class = "btn-sm btn-outline-primary w-100 mb-2"),
      shiny::hr(),
      # Output type
      shiny::selectInput(ns("output_type"), "Output Type",
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

.mod_plan_from_explorer_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Concept set display
    output$concept_set_display <- shiny::renderUI({
      ids <- state$concept_set
      if (length(ids) == 0) return(shiny::p(shiny::em("(empty)")))
      badges <- lapply(ids, function(cid) {
        shiny::span(class = "concept-badge", as.character(cid))
      })
      shiny::div(badges)
    })

    # Add current concept from exploration state
    shiny::observeEvent(input$add_current_concept, {
      cid <- state$selected_concept_id
      if (is.null(cid)) {
        shiny::showNotification("No concept selected in Explore/Drilldown.",
                                type = "warning")
        return()
      }
      current <- state$concept_set
      if (!cid %in% current) {
        state$concept_set <- c(current, cid)
        shiny::showNotification(
          paste("Added concept", cid, "to set"),
          type = "message", duration = 2
        )
      }
    })

    # Add events for concept set
    shiny::observeEvent(input$add_events_for_set, {
      ids <- state$concept_set
      tbl <- state$selected_table
      if (length(ids) == 0) {
        shiny::showNotification("Concept set is empty.", type = "warning")
        return()
      }
      if (is.null(tbl)) {
        tbl <- "condition_occurrence"
      }
      tryCatch({
        nm <- paste0("events_set_", length(state$plan$outputs) + 1)
        state$plan <- ds.omop.plan.events(
          state$plan, name = nm, table = tbl,
          concept_set = as.integer(ids)
        )
        shiny::showNotification(
          paste("Added events output for", length(ids), "concepts"),
          type = "message", duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    # Set cohort on plan
    shiny::observeEvent(input$set_cohort, {
      cid <- as.integer(input$cohort_id)
      state$plan <- ds.omop.plan.cohort(state$plan,
                                         cohort_definition_id = cid)
      shiny::showNotification(
        paste("Cohort", cid, "set on plan"),
        type = "message", duration = 3
      )
    })

    # Add output (same as old plan builder)
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

# ==============================================================================
# MODULE 7: Session (kept as-is)
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
