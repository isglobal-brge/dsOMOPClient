# ==============================================================================
# MODULE 8: Script Builder
# ==============================================================================

#' Simple R syntax highlighting via regex -> HTML spans
#' @param code Character; R source code
#' @return Character; HTML-escaped code with span classes
#' @keywords internal
.highlightR <- function(code) {
  # Escape HTML entities first
  code <- gsub("&", "&amp;", code, fixed = TRUE)
  code <- gsub("<", "&lt;", code, fixed = TRUE)
  code <- gsub(">", "&gt;", code, fixed = TRUE)

  # Highlight comments (must be first to avoid conflicts)
  code <- gsub("(#[^\n]*)", '<span class="r-comment">\\1</span>', code)

  # Highlight strings (double-quoted)
  code <- gsub('("(?:[^"\\\\]|\\\\.)*")',
               '<span class="r-string">\\1</span>', code, perl = TRUE)

  # Highlight keywords
  keywords <- c("library", "function", "if", "else", "for", "while",
                "return", "TRUE", "FALSE", "NULL", "NA")
  for (kw in keywords) {
    pattern <- paste0("\\b(", kw, ")\\b")
    code <- gsub(pattern, '<span class="r-keyword">\\1</span>', code)
  }

  # Highlight numbers
  code <- gsub("\\b([0-9]+\\.?[0-9]*[eEL]?)\\b",
               '<span class="r-number">\\1</span>', code)

  code
}

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
        # JS clipboard handler (browser-native, no clipr dependency)
        shiny::tags$script(shiny::HTML("
          Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
            navigator.clipboard.writeText(text).then(function() {
              Shiny.setInputValue('clipboard_success', true, {priority: 'event'});
            }).catch(function(err) {
              Shiny.setInputValue('clipboard_success', false, {priority: 'event'});
            });
          });
        ")),
        shiny::uiOutput(ns("script_html"))
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

    output$script_html <- shiny::renderUI({
      code <- script_text()
      highlighted <- .highlightR(code)
      shiny::div(class = "code-output",
        shiny::HTML(paste0("<pre><code>", highlighted, "</code></pre>"))
      )
    })

    shiny::observeEvent(input$copy_btn, {
      session$sendCustomMessage("copyToClipboard", script_text())
    })

    shiny::observeEvent(input$clipboard_success, {
      if (isTRUE(input$clipboard_success)) {
        shiny::showNotification("Copied!", type = "message", duration = 2)
      } else {
        shiny::showNotification("Could not copy to clipboard.",
                                type = "warning", duration = 3)
      }
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
