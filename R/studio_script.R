# ==============================================================================
# MODULE 8: Script Builder
# ==============================================================================

.mod_script_builder_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
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
        shiny::div(class = "code-output",
          shiny::verbatimTextOutput(ns("script_output"))
        )
      )
    )
  )
}

.mod_script_builder_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    script_text <- shiny::reactive({
      lines <- state$script_lines
      if (length(lines) == 0) return("# No operations recorded yet.\n# Use the Explore, Drilldown, Locator, Vocabulary, or Session tabs\n# to generate reproducible R code.")
      header <- c(
        "# ==============================================================================",
        "# dsOMOP Studio - Reproducible Script",
        paste0("# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "# ==============================================================================",
        "library(dsOMOPClient)",
        ""
      )
      paste(c(header, lines), collapse = "\n")
    })

    output$script_output <- shiny::renderText({
      script_text()
    })

    shiny::observeEvent(input$copy_btn, {
      code <- script_text()
      tryCatch({
        if (requireNamespace("clipr", quietly = TRUE)) {
          clipr::write_clip(code)
          shiny::showNotification("Script copied to clipboard!",
                                  type = "message", duration = 2)
        } else {
          shiny::showNotification(
            "Install 'clipr' package for clipboard support.",
            type = "warning", duration = 4)
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Could not copy:", conditionMessage(e)),
          type = "warning", duration = 3)
      })
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

