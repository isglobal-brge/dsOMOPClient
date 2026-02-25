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
      shiny::hr(),
      shiny::h6("Search by Name"),
      shiny::div(class = "d-flex gap-1 mb-2",
        shiny::textInput(ns("name_search"), NULL,
                         placeholder = "e.g. diabetes",
                         width = "100%"),
        shiny::actionButton(ns("name_search_btn"), "Search",
                            class = "btn-sm btn-outline-secondary")
      ),
      DT::DTOutput(ns("search_results_dt")),
      shiny::hr(),
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
    search_results <- shiny::reactiveVal(NULL)

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

    # Concept name search
    shiny::observeEvent(input$name_search_btn, {
      term <- input$name_search
      if (is.null(term) || nchar(trimws(term)) == 0) return()

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
    })

    output$search_results_dt <- DT::renderDT({
      df <- search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      keep <- intersect(c("concept_id", "concept_name", "domain_id"), names(df))
      DT::datatable(
        df[, keep, drop = FALSE],
        options = list(pageLength = 5, dom = "t", scrollX = TRUE,
                       scrollY = "200px"),
        rownames = FALSE, selection = "multiple"
      )
    })

    # When search results selected, append to concept IDs field
    shiny::observeEvent(input$search_results_dt_rows_selected, {
      idx <- input$search_results_dt_rows_selected
      df <- search_results()
      if (is.null(idx) || is.null(df) || length(idx) == 0) return()
      idx <- idx[idx <= nrow(df)]
      new_ids <- as.integer(df$concept_id[idx])

      current_text <- trimws(input$concept_ids %||% "")
      existing_ids <- if (nchar(current_text) > 0) {
        as.integer(trimws(strsplit(current_text, ",")[[1]]))
      } else integer(0)
      existing_ids <- existing_ids[!is.na(existing_ids)]

      all_ids <- unique(c(existing_ids, new_ids))
      shiny::updateTextInput(session, "concept_ids",
                             value = paste(all_ids, collapse = ", "))
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
        # Accumulate code for Script tab
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        # Combine results from all servers with server column
        all_results <- data.frame(
          table_name = character(0), concept_column = character(0),
          concept_id = integer(0), n_records = numeric(0),
          n_persons = numeric(0), server = character(0),
          stringsAsFactors = FALSE
        )
        for (srv in names(res$per_site)) {
          df <- res$per_site[[srv]]
          if (is.data.frame(df) && nrow(df) > 0) {
            df$server <- srv
            all_results <- rbind(all_results, df)
          }
        }
        # Format table names for display
        if (nrow(all_results) > 0 && "table_name" %in% names(all_results)) {
          all_results$table_display <- .format_table_name(all_results$table_name)
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
      # Show formatted table name instead of raw
      show_df <- df
      if ("table_display" %in% names(show_df)) {
        show_df$table_name <- show_df$table_display
        show_df$table_display <- NULL
      }
      DT::datatable(show_df,
        options = list(pageLength = 25, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "none")
    })
  })
}
