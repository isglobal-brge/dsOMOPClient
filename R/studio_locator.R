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
      DT::datatable(df, options = list(pageLength = 25, dom = "ftip", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })
  })
}

