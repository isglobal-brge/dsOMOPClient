# Module: Studio - Sidebar
# Shiny module for the main navigation sidebar and scope controls.

#' Studio Sidebar UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny sidebar element.
#' @keywords internal
.mod_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::sidebar(
    id = ns("right_sidebar"),
    position = "right",
    width = 280,
    open = FALSE,

    # --- Global Context ---
    bslib::accordion(
      id = ns("sidebar_acc"),
      open = "Context",

      bslib::accordion_panel(
        "Context",
        icon = fontawesome::fa_i("globe"),
        shiny::radioButtons(ns("scope"), "Data Scope",
          choices = c("Pooled" = "pooled", "Per Site" = "per_site"),
          selected = "pooled", inline = TRUE),

        # Per Site: single server dropdown
        shiny::conditionalPanel(
          condition = paste0("input['", ns("scope"), "'] == 'per_site'"),
          shiny::selectInput(ns("single_server"), "Server",
            choices = NULL)
        ),

        # Pooled: all-servers checkbox + optional server list
        shiny::conditionalPanel(
          condition = paste0("input['", ns("scope"), "'] == 'pooled'"),
          shiny::checkboxInput(ns("all_servers"), "All servers", TRUE),
          shiny::conditionalPanel(
            condition = paste0("!input['", ns("all_servers"), "']"),
            shiny::checkboxGroupInput(ns("pool_servers"), "Include servers",
              choices = NULL)
          ),
          shiny::radioButtons(ns("pooling_policy"), "Pooling Policy",
            choices = c("Strict" = "strict",
                        "Best Effort" = "pooled_only_ok"),
            selected = "strict", inline = TRUE)
        )
      ),

      # --- Recipe Summary ---
      bslib::accordion_panel(
        "Recipe",
        icon = fontawesome::fa_i("hammer"),
        shiny::uiOutput(ns("recipe_summary")),
        shiny::actionButton(ns("go_to_recipe"), "Go to Builder",
                            class = "btn-sm btn-outline-primary w-100 mt-2")
      )
    ),
    shiny::div(class = "text-muted text-center small mt-3",
      shiny::span(class = "badge bg-secondary",
        paste0("dsOMOPClient v",
          as.character(utils::packageVersion("dsOMOPClient"))))
    )
  )
}

#' Studio Sidebar Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @param parent_session Shiny session; the parent session for tab navigation.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_sidebar_server <- function(id, state, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {

    # --- Sync server choices from state ---
    shiny::observe({
      srvs <- state$server_names
      if (length(srvs) > 0) {
        # Per Site: single dropdown
        shiny::updateSelectInput(session, "single_server",
                                 choices = srvs, selected = srvs[1])
        # Pooled: checkbox group
        shiny::updateCheckboxGroupInput(session, "pool_servers",
                                        choices = srvs, selected = srvs)
      }
    })

    # --- Sync sidebar inputs -> state ---
    shiny::observeEvent(input$scope, {
      state$scope <- input$scope
    })

    # Per Site: sync single server selection
    shiny::observeEvent(input$single_server, {
      if (input$scope == "per_site" && !is.null(input$single_server)) {
        state$selected_servers <- input$single_server
      }
    })

    # Pooled: sync all_servers checkbox + pool_servers
    shiny::observeEvent(input$all_servers, {
      if (isTRUE(input$all_servers)) {
        state$selected_servers <- state$server_names
      }
    })

    shiny::observeEvent(input$pool_servers, {
      if (input$scope == "pooled" && !isTRUE(input$all_servers)) {
        state$selected_servers <- input$pool_servers
      }
    }, ignoreNULL = FALSE)

    # When scope changes, update selected_servers accordingly
    shiny::observeEvent(input$scope, {
      if (input$scope == "per_site") {
        state$selected_servers <- input$single_server %||% state$server_names[1]
      } else {
        # Pooled
        if (isTRUE(input$all_servers)) {
          state$selected_servers <- state$server_names
        } else {
          state$selected_servers <- input$pool_servers
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$pooling_policy, {
      state$pooling_policy <- input$pooling_policy
    })

    # --- Recipe Summary ---
    output$recipe_summary <- shiny::renderUI({
      recipe <- state$recipe
      nv <- length(recipe$variables)
      nb <- length(recipe$blocks)
      nf <- length(recipe$filters)
      no <- length(recipe$outputs)
      total <- nv + nb + nf + no

      if (total == 0) {
        return(shiny::p(class = "text-muted small", "Builder is empty."))
      }

      badges <- shiny::tagList(
        if (nv > 0) shiny::span(class = "badge bg-success me-1",
                                paste(nv, "var")),
        if (nb > 0) shiny::span(class = "badge bg-info me-1",
                                paste(nb, "block")),
        if (nf > 0) shiny::span(class = "badge bg-warning me-1",
                                paste(nf, "filter")),
        if (no > 0) shiny::span(class = "badge bg-primary me-1",
                                paste(no, "output"))
      )

      # Show first few items
      items <- character(0)
      for (nm in utils::head(names(recipe$variables), 5)) {
        items <- c(items, nm)
      }
      if (nv > 5) items <- c(items, paste0("... +", nv - 5, " more"))

      shiny::tagList(
        shiny::div(class = "mb-2", badges),
        if (length(items) > 0) shiny::div(class = "small text-muted",
          shiny::HTML(paste(items, collapse = "<br>")))
      )
    })

    # --- Navigate to Recipe ---
    shiny::observeEvent(input$go_to_recipe, {
      bslib::nav_select("main_nav", "Builder",
                        session = parent_session)
    })
  })
}
