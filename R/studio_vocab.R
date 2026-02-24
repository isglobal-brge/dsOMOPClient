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
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          "Search Results",
          shiny::div(
            shiny::actionButton(ns("add_selected_vocab"), "Add to Set",
                                class = "btn-sm btn-outline-primary me-1"),
            shiny::actionButton(ns("extract_selected_vocab"),
                                shiny::tagList(shiny::icon("plus"), "Extract"),
                                class = "btn-sm btn-outline-success")
          )
        )
      ),
      bslib::card_body(
        shiny::div(class = "dsomop-scroll",
          DT::DTOutput(ns("results_table"))
        )
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
        # Accumulate code for Script tab
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        srv <- names(res$per_site)[1]
        search_results(res$per_site[[srv]])
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
        options = list(pageLength = 15, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "multiple"
      )
    })

    # Bulk add selected concepts to set
    shiny::observeEvent(input$add_selected_vocab, {
      df <- search_results()
      idx <- input$results_table_rows_selected
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
      idx <- input$results_table_rows_selected
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

