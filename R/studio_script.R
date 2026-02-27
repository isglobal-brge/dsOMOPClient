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

  # Process line by line to handle comments reliably
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  processed <- vapply(lines, function(line) {
    comment_pos <- regexpr("#", line, fixed = TRUE)
    if (comment_pos > 0) {
      before <- substr(line, 1, comment_pos - 1)
      comment <- substr(line, comment_pos, nchar(line))
      before <- .highlightR_line(before)
      paste0(before, '<span class="r-comment">', comment, '</span>')
    } else {
      .highlightR_line(line)
    }
  }, character(1), USE.NAMES = FALSE)
  paste(processed, collapse = "\n")
}

# Highlight a single line of R code (no comments)
.highlightR_line <- function(line) {
  # Strings (double and single quoted)
  line <- gsub('("(?:[^"\\\\]|\\\\.)*")',
               '<span class="r-string">\\1</span>', line, perl = TRUE)
  line <- gsub("('(?:[^'\\\\]|\\\\.)*')",
               '<span class="r-string">\\1</span>', line, perl = TRUE)

  # Function calls (word followed by open paren)
  line <- gsub("\\b([a-zA-Z][a-zA-Z0-9._]*)\\s*\\(",
               '<span class="r-fn">\\1</span>(', line, perl = TRUE)

  # Keywords (override function call styling where applicable)
  keywords <- c("function", "if", "else", "for", "while", "repeat",
                "return", "TRUE", "FALSE", "NULL", "NA", "NA_real_",
                "NA_integer_", "NA_character_", "in", "next", "break")
  for (kw in keywords) {
    line <- gsub(paste0("\\b(", kw, ")\\b"),
                 paste0('<span class="r-keyword">\\1</span>'), line)
  }

  # Assignment operator (<-)
  line <- gsub("(&lt;-)", '<span class="r-assign">\\1</span>', line)

  # Pipe operators
  line <- gsub("(\\|&gt;|%&gt;%)", '<span class="r-operator">\\1</span>', line)

  # Numbers
  line <- gsub("\\b([0-9]+\\.?[0-9]*[eEL]?)\\b",
               '<span class="r-number">\\1</span>', line)

  line
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
      if (length(lines) == 0) return("")
      paste(lines, collapse = "\n")
    })

    output$script_html <- shiny::renderUI({
      code <- script_text()
      if (nchar(code) == 0) {
        return(shiny::p(class = "text-muted",
          "No operations recorded yet. Use the Explore, Vocabulary,",
          " Data Sources, or Queries tabs to generate reproducible R code."))
      }
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
