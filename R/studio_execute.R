# Module: Studio - Plan Execution
# Shiny module for executing extraction plans and displaying results.

#' Studio Session Log UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
.mod_session_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Session Log"),
    bslib::card_body(
      shiny::div(class = "d-flex justify-content-end mb-2",
        shiny::actionButton(ns("disconnect_btn"), "Close Studio",
          icon = shiny::icon("xmark"),
          class = "btn-sm btn-outline-secondary")
      ),
      shiny::uiOutput(ns("session_log"))
    )
  )
}

#' Studio Session Log Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_session_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$session_log <- shiny::renderUI({
      lines <- state$script_lines
      if (length(lines) == 0) {
        return(.empty_state_ui("terminal", "No operations recorded",
          "Use the Explore, Vocabulary, Achilles, or Queries tabs to generate reproducible R code."))
      }
      code <- paste(lines, collapse = "\n")
      highlighted <- .highlightR(code)
      shiny::div(class = "code-output",
        shiny::HTML(paste0("<pre style='margin:0;padding:0;'><code>",
                           highlighted, "</code></pre>"))
      )
    })

    # Close Studio: stop the Shiny app ONLY. The DataSHIELD connection and the
    # OMOP session stay active (no logout, no ds.omop.disconnect) so the user
    # can relaunch the Studio or keep working in R.
    shiny::observeEvent(input$disconnect_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Close OMOP Studio",
        shiny::p("This closes OMOP Studio only. Your DataSHIELD connection ",
                 "stays active — relaunch any time with ",
                 shiny::code("ds.omop.studio()"), ", or end the session in R ",
                 "with ", shiny::code("ds.omop.disconnect()"), ". Any unsaved ",
                 "recipe in the Builder will be lost."),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns("disconnect_confirm"),
            "Close Studio", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$disconnect_confirm, {
      shiny::removeModal()
      shiny::stopApp()
    })
  })
}
