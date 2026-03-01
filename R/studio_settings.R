# Module: Studio - Settings
# Shiny module for configuring studio preferences and session options.

#' Studio Settings UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
.mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(4, 4, 4),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Disclosure Settings"),
      bslib::card_body(
        shiny::htmlOutput(ns("disclosure_info"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Session Info"),
      bslib::card_body(
        shiny::uiOutput(ns("session_info"))
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("About"),
      bslib::card_body(
        shiny::uiOutput(ns("about_info"))
      )
    )
  )
}

#' Studio Settings Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_settings_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    output$disclosure_info <- shiny::renderUI({
      shiny::tags$dl(class = "row settings-dl",
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
            getOption("default.nfilter.levels.max", 40))))
      )
    })

    output$session_info <- shiny::renderUI({
      st <- state$status
      items <- list(
        shiny::tags$dt(class = "col-sm-5", "Session Symbol"),
        shiny::tags$dd(class = "col-sm-7",
          as.character(state$symbol %||% "unknown")),
        shiny::tags$dt(class = "col-sm-5", "R Version"),
        shiny::tags$dd(class = "col-sm-7",
          paste(R.version$major, R.version$minor, sep = ".")),
        shiny::tags$dt(class = "col-sm-5", "bslib Version"),
        shiny::tags$dd(class = "col-sm-7",
          as.character(utils::packageVersion("bslib"))),
        shiny::tags$dt(class = "col-sm-5", "Shiny Version"),
        shiny::tags$dd(class = "col-sm-7",
          as.character(utils::packageVersion("shiny")))
      )

      if (!is.null(st) && !is.null(st$servers)) {
        items <- c(items, list(
          shiny::tags$dt(class = "col-sm-5", "Servers"),
          shiny::tags$dd(class = "col-sm-7",
            paste(st$servers, collapse = ", "))
        ))
      }

      err_text <- "None"
      if (!is.null(st) && !is.null(st$errors) && length(st$errors) > 0) {
        err_text <- paste(names(st$errors), collapse = ", ")
      }
      items <- c(items, list(
        shiny::tags$dt(class = "col-sm-5", "Errors"),
        shiny::tags$dd(class = "col-sm-7", err_text)
      ))

      shiny::tags$dl(class = "row settings-dl", items)
    })

    output$about_info <- shiny::renderUI({
      client_ver <- tryCatch(
        as.character(utils::packageVersion("dsOMOPClient")),
        error = function(e) "unknown"
      )
      server_ver <- tryCatch(
        as.character(utils::packageVersion("dsOMOP")),
        error = function(e) "not installed"
      )

      shiny::tags$dl(class = "row settings-dl",
        shiny::tags$dt(class = "col-sm-6", "dsOMOPClient"),
        shiny::tags$dd(class = "col-sm-6",
          shiny::span(class = "badge bg-primary", paste0("v", client_ver))),
        shiny::tags$dt(class = "col-sm-6", "dsOMOP (server)"),
        shiny::tags$dd(class = "col-sm-6",
          shiny::span(class = "badge bg-secondary", paste0("v", server_ver))),
        shiny::tags$dt(class = "col-sm-6", "Ecosystem"),
        shiny::tags$dd(class = "col-sm-6", "DataSHIELD + OMOP CDM"),
        shiny::tags$dt(class = "col-sm-6", "License"),
        shiny::tags$dd(class = "col-sm-6", "MIT")
      )
    })
  })
}
