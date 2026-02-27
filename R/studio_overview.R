# ==============================================================================
# MODULE: Overview (Landing Dashboard)
# ==============================================================================
# KPI value_boxes, unified server cards, notifications.
# ==============================================================================

.mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- KPI row ---
    bslib::layout_column_wrap(
      width = "200px",
      fixed_width = FALSE,
      bslib::value_box(
        title = "Servers",
        value = shiny::textOutput(ns("kpi_servers")),
        showcase = fontawesome::fa_i("server"),
        theme = "primary", full_screen = FALSE
      ),
      bslib::value_box(
        title = "Common Tables",
        value = shiny::textOutput(ns("kpi_tables")),
        showcase = fontawesome::fa_i("table"),
        theme = "info", full_screen = FALSE
      ),
      bslib::value_box(
        title = "Total Persons",
        value = shiny::textOutput(ns("kpi_persons")),
        showcase = fontawesome::fa_i("users"),
        theme = "success", full_screen = FALSE
      )
    ),
    # --- Refresh button ---
    shiny::div(class = "d-flex justify-content-between align-items-center mb-3",
      shiny::tags$h6(class = "mb-0 text-muted", "Connected Servers"),
      shiny::actionButton(ns("refresh"), "Refresh",
                          icon = shiny::icon("rotate"),
                          class = "btn-sm btn-outline-primary")
    ),
    # --- Server cards (one per server) ---
    shiny::uiOutput(ns("server_cards")),
    # --- System Notifications ---
    shiny::uiOutput(ns("notifications_section"))
  )
}

.mod_overview_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Refresh ---
    shiny::observeEvent(input$refresh, {
      shiny::withProgress(message = "Refreshing status...", value = 0.3, {
        tryCatch({
          state$status <- ds.omop.status(symbol = state$symbol)
          shiny::incProgress(0.4)
          state$tables <- ds.omop.tables(symbol = state$symbol)
          if (!is.null(state$status$servers)) {
            state$server_names <- state$status$servers
          }
        }, error = function(e) {
          shiny::showNotification(
            paste("Error:", conditionMessage(e)), type = "error"
          )
        })
      })
    })

    # --- KPIs ---
    output$kpi_servers <- shiny::renderText({
      st <- state$status
      if (is.null(st) || is.null(st$servers)) return("--")
      as.character(length(st$servers))
    })

    output$kpi_tables <- shiny::renderText({
      st <- state$status
      if (is.null(st) || is.null(st$capabilities)) return("--")
      # Common tables = intersection across all servers (case-insensitive)
      common <- NULL
      for (srv in names(st$capabilities)) {
        caps <- st$capabilities[[srv]]
        srv_tables <- tolower(caps$tables %||% caps$cdm_tables %||% character(0))
        if (is.null(common)) {
          common <- srv_tables
        } else {
          common <- intersect(common, srv_tables)
        }
      }
      if (is.null(common)) return("--")
      as.character(length(common))
    })

    output$kpi_persons <- shiny::renderText({
      st <- state$status
      if (is.null(st) || is.null(st$capabilities)) return("--")
      total <- 0
      for (srv in names(st$capabilities)) {
        caps <- st$capabilities[[srv]]
        if (!is.null(caps$total_persons)) {
          total <- total + as.numeric(caps$total_persons)
        }
      }
      if (total > 0) format(total, big.mark = ",") else "--"
    })

    # --- Unified Server Cards ---
    output$server_cards <- shiny::renderUI({
      st <- state$status
      if (is.null(st)) {
        return(.empty_state_ui("spinner", "Loading...",
                               "Fetching server status..."))
      }

      srv_names <- st$servers
      if (is.null(srv_names) || length(srv_names) == 0) {
        return(.empty_state_ui("server", "No servers found"))
      }

      cards <- lapply(srv_names, function(srv) {
        # Connection status
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

        status_dot <- if (alive) {
          shiny::tags$span(
            style = "display:inline-block; width:8px; height:8px; border-radius:50%; background:#059669; margin-right:6px;")
        } else {
          shiny::tags$span(
            style = "display:inline-block; width:8px; height:8px; border-radius:50%; background:#dc2626; margin-right:6px;")
        }

        caps <- if (!is.null(st$capabilities)) st$capabilities[[srv]] else NULL
        cdm_info <- if (!is.null(caps)) caps$cdm_info else NULL

        # Build info rows
        info_items <- list()

        .add_row <- function(icon_name, label, value) {
          if (!is.null(value) && nchar(as.character(value)) > 0) {
            info_items[[length(info_items) + 1]] <<- shiny::div(
              class = "d-flex align-items-center py-1",
              style = "font-size: 0.84rem;",
              shiny::icon(icon_name, class = "text-muted me-2",
                          style = "width: 14px; text-align: center;"),
              shiny::span(class = "text-muted me-1", paste0(label, ":")),
              shiny::span(style = "font-weight: 500;", as.character(value))
            )
          }
        }

        # Database / source name
        if (!is.null(cdm_info) && !is.null(cdm_info$source_name)) {
          .add_row("database", "Source", cdm_info$source_name)
        }
        # Tables count (clickable)
        if (!is.null(caps$n_tables)) {
          .add_row("table", "Tables", caps$n_tables)
        }
        # Persons
        if (!is.null(caps$total_persons)) {
          .add_row("users", "Persons", format(as.numeric(caps$total_persons), big.mark = ","))
        }
        # Vocabulary version
        if (!is.null(cdm_info) && !is.null(cdm_info$vocabulary_version) &&
            nchar(as.character(cdm_info$vocabulary_version)) > 0) {
          .add_row("book", "Vocabulary", cdm_info$vocabulary_version)
        }

        # Capability badges
        badge_list <- list()
        # CDM version badge
        if (!is.null(cdm_info) && !is.null(cdm_info$cdm_version) &&
            nchar(cdm_info$cdm_version) > 0) {
          ver <- cdm_info$cdm_version
          if (!grepl("^v", ver)) ver <- paste0("v", ver)
          badge_list <- c(badge_list, list(
            shiny::span(class = "badge bg-secondary me-1", paste("CDM", ver))))
        }
        if (!is.null(caps$achilles_available) && isTRUE(caps$achilles_available)) {
          badge_list <- c(badge_list, list(
            shiny::span(class = "badge me-1",
                        style = "background:#065f46; color:#ecfdf5;",
                        shiny::icon("check", class = "me-1"),
                        "Achilles")))
        } else {
          badge_list <- c(badge_list, list(
            shiny::span(class = "badge bg-light text-muted me-1", "No Achilles")))
        }

        # Unique button ID for this server's tables popup
        btn_id <- ns(paste0("show_tables_", gsub("[^a-zA-Z0-9]", "_", srv)))

        bslib::card(
          class = "server-card",
          bslib::card_header(
            class = "d-flex justify-content-between align-items-center",
            shiny::div(class = "d-flex align-items-center",
              status_dot,
              shiny::tags$strong(srv, style = "font-size: 0.95rem;")
            ),
            shiny::div(
              if (length(badge_list) > 0) shiny::tagList(badge_list)
            )
          ),
          bslib::card_body(
            class = "py-2 px-3",
            if (length(info_items) > 0) {
              shiny::div(shiny::tagList(info_items))
            } else {
              shiny::p(class = "text-muted small mb-0", "No capability data")
            },
            shiny::div(class = "mt-2",
              shiny::actionButton(btn_id, "View Tables",
                icon = shiny::icon("table-list"),
                class = "btn-sm btn-outline-secondary w-100")
            )
          )
        )
      })

      # Register dynamic observers for table popup buttons
      for (srv in srv_names) {
        local({
          local_srv <- srv
          btn_id <- paste0("show_tables_", gsub("[^a-zA-Z0-9]", "_", local_srv))
          shiny::observeEvent(input[[btn_id]], {
            caps <- st$capabilities[[local_srv]]
            all_tbls <- caps$tables %||% caps$cdm_tables
            if (is.null(all_tbls) || length(all_tbls) == 0) {
              shiny::showNotification("No table info available", type = "warning",
                                     duration = 2)
              return()
            }
            tbl_badges <- lapply(sort(all_tbls), function(t) {
              shiny::span(class = "badge bg-light text-dark me-1 mb-1",
                          style = "font-size: 0.8rem;", t)
            })
            shiny::showModal(shiny::modalDialog(
              title = paste(local_srv, "-", length(all_tbls), "Tables"),
              size = "m", easyClose = TRUE,
              shiny::div(class = "d-flex flex-wrap gap-1",
                shiny::tagList(tbl_badges)
              ),
              footer = shiny::modalButton("Close")
            ))
          }, ignoreInit = TRUE, once = FALSE)
        })
      }

      bslib::layout_column_wrap(
        width = "360px",
        fixed_width = FALSE,
        !!!cards
      )
    })

    # --- System Notifications ---
    output$notifications_section <- shiny::renderUI({
      st <- state$status
      msgs <- list()

      if (!is.null(st) && !is.null(st$errors) && length(st$errors) > 0) {
        for (srv in names(st$errors)) {
          msgs <- c(msgs, list(
            shiny::div(class = "alert alert-danger py-2 mb-1",
              shiny::icon("circle-exclamation"),
              shiny::strong(paste0(" ", srv, ": ")),
              as.character(st$errors[[srv]])
            )
          ))
        }
      }

      if (length(msgs) == 0) return(NULL)  # No errors = no section needed
      shiny::div(class = "mt-3", shiny::tagList(msgs))
    })
  })
}
