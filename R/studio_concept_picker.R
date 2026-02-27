# ==============================================================================
# Reusable Concept Picker Module
# ==============================================================================
# Search-as-you-type widget that searches by concept name AND concept ID.
# Shows results as "Concept Name (ID: 12345)" in a dropdown.
# Usage:
#   UI:     .concept_picker_ui(ns("picker"))
#   Server: selected <- .concept_picker_server("picker", state)
#           selected() returns list(concept_id, concept_name, domain_id)
# ==============================================================================

.concept_picker_ui <- function(id, label = "Search Concept",
                               placeholder = "Type name or ID...",
                               multiple = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "concept-picker",
      shiny::div(class = "d-flex gap-1 align-items-end",
        shiny::div(style = "flex: 1;",
          shiny::textInput(ns("search_term"), label,
                           placeholder = placeholder, width = "100%")
        ),
        shiny::actionButton(ns("search_btn"),
          shiny::icon("magnifying-glass"),
          class = "btn-sm btn-outline-primary",
          style = "margin-bottom: 15px; height: 34px;")
      ),
      shiny::uiOutput(ns("results_panel"))
    )
  )
}

.concept_picker_server <- function(id, state, on_select = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    search_results <- shiny::reactiveVal(NULL)
    is_searching <- shiny::reactiveVal(FALSE)

    # Debounced search term (trigger after 600ms of no typing)
    search_term_d <- shiny::debounce(shiny::reactive(input$search_term), 600)

    # Auto-search when typing ≥ 3 characters
    shiny::observe({
      term <- search_term_d()
      if (is.null(term) || nchar(trimws(term)) < 3) {
        search_results(NULL)
        return()
      }
      .do_search(trimws(term))
    })

    # Manual search button
    shiny::observeEvent(input$search_btn, {
      term <- trimws(input$search_term %||% "")
      if (nchar(term) < 2) {
        shiny::showNotification("Type at least 2 characters", type = "warning",
                                duration = 2)
        return()
      }
      .do_search(term)
    })

    .do_search <- function(term) {
      is_searching(TRUE)
      tryCatch({
        # Check if it's a numeric ID
        is_numeric <- grepl("^[0-9]+$", term)

        if (is_numeric) {
          # Lookup by ID
          res <- ds.omop.concept.lookup(
            concept_ids = as.integer(term),
            symbol = state$symbol
          )
        } else {
          # Search by name
          res <- ds.omop.concept.search(
            pattern = term, standard_only = TRUE,
            limit = 25, symbol = state$symbol
          )
        }

        srv <- names(res$per_site)[1]
        df <- res$per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0) {
          search_results(df)
        } else {
          search_results(data.frame(
            concept_id = integer(0), concept_name = character(0),
            domain_id = character(0), stringsAsFactors = FALSE
          ))
        }
      }, error = function(e) {
        search_results(NULL)
      })
      is_searching(FALSE)
    }

    # Render results as a compact clickable list
    output$results_panel <- shiny::renderUI({
      df <- search_results()
      if (is.null(df)) return(NULL)
      if (nrow(df) == 0) {
        return(shiny::div(class = "concept-picker-empty text-muted small py-2",
          "No concepts found."))
      }

      # Build clickable items
      items <- lapply(seq_len(min(nrow(df), 25)), function(i) {
        row <- df[i, ]
        cid <- as.character(row$concept_id)
        cname <- as.character(row$concept_name)
        domain <- if ("domain_id" %in% names(row)) as.character(row$domain_id) else ""
        vocab <- if ("vocabulary_id" %in% names(row)) as.character(row$vocabulary_id) else ""

        shiny::tags$button(
          type = "button",
          class = "concept-picker-item list-group-item list-group-item-action py-1 px-2",
          id = ns(paste0("pick_", cid)),
          onclick = paste0(
            "Shiny.setInputValue('", ns("picked"), "', '",
            cid, "|", gsub("'", "\\\\'", cname), "|", domain,
            "', {priority: 'event'});"
          ),
          shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "concept-picker-name",
                if (nchar(cname) > 50) paste0(substr(cname, 1, 47), "...") else cname
              ),
              if (nchar(domain) > 0)
                shiny::span(class = "concept-picker-domain badge bg-light text-dark ms-1",
                            style = "font-size: 0.65em;", domain)
            ),
            shiny::span(class = "concept-picker-id text-muted",
                        style = "font-size: 0.78em; font-weight: 500;",
                        cid)
          )
        )
      })

      shiny::div(class = "concept-picker-results list-group",
        style = "max-height: 240px; overflow-y: auto; border-radius: 0.5rem;
                 border: 1px solid #e2e8f0; margin-top: -0.5rem;",
        shiny::tagList(items)
      )
    })

    # Handle picks
    shiny::observeEvent(input$picked, {
      parts <- strsplit(input$picked, "|", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        result <- list(
          concept_id = as.integer(parts[1]),
          concept_name = parts[2],
          domain_id = if (length(parts) >= 3) parts[3] else ""
        )
        # Call the callback if provided
        if (is.function(on_select)) {
          on_select(result)
        }
        # Clear results after picking
        search_results(NULL)
        shiny::updateTextInput(session, "search_term",
          value = paste0(result$concept_name, " (", result$concept_id, ")"))
      }
    })

    # Return reactive with the last picked concept
    shiny::reactive({
      parts <- strsplit(input$picked %||% "", "|", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        list(
          concept_id = as.integer(parts[1]),
          concept_name = parts[2],
          domain_id = if (length(parts) >= 3) parts[3] else ""
        )
      } else {
        NULL
      }
    })
  })
}

# ==============================================================================
# Multi-concept picker (for comma-separated ID inputs)
# ==============================================================================
# Like concept_picker but accumulates multiple selections into a text field.

.concept_multi_picker_ui <- function(id, label = "Concepts",
                                     placeholder = "Search to add concepts...") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "concept-multi-picker",
      shiny::tags$label(class = "control-label", label),
      shiny::div(class = "d-flex gap-1 align-items-center mb-1",
        shiny::div(style = "flex: 1;",
          shiny::textInput(ns("search_term"), NULL,
                           placeholder = placeholder, width = "100%")
        ),
        shiny::actionButton(ns("search_btn"),
          shiny::icon("magnifying-glass"),
          class = "btn-sm btn-outline-primary",
          style = "height: 34px;")
      ),
      shiny::uiOutput(ns("results_panel")),
      shiny::div(class = "mt-1",
        shiny::uiOutput(ns("selected_chips"))
      )
    )
  )
}

.concept_multi_picker_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    search_results <- shiny::reactiveVal(NULL)
    selected_concepts <- shiny::reactiveVal(list())

    search_term_d <- shiny::debounce(shiny::reactive(input$search_term), 600)

    shiny::observe({
      term <- search_term_d()
      if (is.null(term) || nchar(trimws(term)) < 3) {
        search_results(NULL)
        return()
      }
      .do_multi_search(trimws(term), state, search_results)
    })

    shiny::observeEvent(input$search_btn, {
      term <- trimws(input$search_term %||% "")
      if (nchar(term) < 2) return()
      .do_multi_search(term, state, search_results)
    })

    output$results_panel <- shiny::renderUI({
      df <- search_results()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      items <- lapply(seq_len(min(nrow(df), 20)), function(i) {
        row <- df[i, ]
        cid <- as.character(row$concept_id)
        cname <- as.character(row$concept_name)
        domain <- if ("domain_id" %in% names(row)) as.character(row$domain_id) else ""

        shiny::tags$button(
          type = "button",
          class = "concept-picker-item list-group-item list-group-item-action py-1 px-2",
          onclick = paste0(
            "Shiny.setInputValue('", ns("multi_picked"), "', '",
            cid, "|", gsub("'", "\\\\'", cname), "|", domain,
            "', {priority: 'event'});"
          ),
          shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::span(class = "concept-picker-name",
              if (nchar(cname) > 45) paste0(substr(cname, 1, 42), "...") else cname
            ),
            shiny::span(class = "text-muted", style = "font-size: 0.78em;", cid)
          )
        )
      })

      shiny::div(class = "concept-picker-results list-group",
        style = "max-height: 200px; overflow-y: auto; border-radius: 0.5rem;
                 border: 1px solid #e2e8f0;",
        shiny::tagList(items)
      )
    })

    shiny::observeEvent(input$multi_picked, {
      parts <- strsplit(input$multi_picked, "|", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        current <- selected_concepts()
        cid <- as.integer(parts[1])
        if (!cid %in% vapply(current, function(x) x$concept_id, integer(1))) {
          current[[length(current) + 1]] <- list(
            concept_id = cid,
            concept_name = parts[2],
            domain_id = if (length(parts) >= 3) parts[3] else ""
          )
          selected_concepts(current)
        }
        search_results(NULL)
        shiny::updateTextInput(session, "search_term", value = "")
      }
    })

    # Remove chip
    shiny::observeEvent(input$remove_chip, {
      cid <- as.integer(input$remove_chip)
      current <- selected_concepts()
      current <- current[vapply(current, function(x) x$concept_id != cid, logical(1))]
      selected_concepts(current)
    })

    output$selected_chips <- shiny::renderUI({
      sel <- selected_concepts()
      if (length(sel) == 0) return(NULL)
      chips <- lapply(sel, function(c) {
        shiny::span(class = "concept-badge me-1 mb-1",
          style = "cursor: pointer;",
          onclick = paste0(
            "Shiny.setInputValue('", ns("remove_chip"), "', ",
            c$concept_id, ", {priority: 'event'});"),
          paste0(substr(c$concept_name, 1, 25), " (", c$concept_id, ")"),
          shiny::icon("xmark", class = "ms-1", style = "font-size: 0.7em;")
        )
      })
      shiny::div(class = "d-flex flex-wrap", shiny::tagList(chips))
    })

    # Return reactive with selected concept IDs
    shiny::reactive({
      sel <- selected_concepts()
      if (length(sel) == 0) return(integer(0))
      vapply(sel, function(x) x$concept_id, integer(1))
    })
  })
}

# Internal helper for multi-picker search
.do_multi_search <- function(term, state, search_results_rv) {
  tryCatch({
    is_numeric <- grepl("^[0-9]+$", term)
    if (is_numeric) {
      res <- ds.omop.concept.lookup(
        concept_ids = as.integer(term), symbol = state$symbol
      )
    } else {
      res <- ds.omop.concept.search(
        pattern = term, standard_only = TRUE,
        limit = 20, symbol = state$symbol
      )
    }
    srv <- names(res$per_site)[1]
    df <- res$per_site[[srv]]
    if (is.data.frame(df) && nrow(df) > 0) {
      search_results_rv(df)
    } else {
      search_results_rv(data.frame(
        concept_id = integer(0), concept_name = character(0),
        stringsAsFactors = FALSE
      ))
    }
  }, error = function(e) {
    search_results_rv(NULL)
  })
}
