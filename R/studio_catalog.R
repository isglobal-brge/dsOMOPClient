# ==============================================================================
# MODULE: Catalog Browser
# Browse, parameterise, and execute catalog queries with concept search
# ==============================================================================

.mod_catalog_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Catalog", width = 300,
      shiny::selectInput(ns("domain_filter"), "Domain",
        choices = c("All" = "", "Condition" = "Condition", "Drug" = "Drug",
                    "Measurement" = "Measurement", "Observation" = "Observation",
                    "Procedure" = "Procedure", "Person" = "Person",
                    "Visit" = "Visit", "Death" = "Death",
                    "Observation Period" = "Observation Period",
                    "Device" = "Device", "Care Site" = "Care Site",
                    "General" = "General"),
        selected = ""),
      DT::DTOutput(ns("query_list_dt")),
      .scope_controls_ui(ns)
    ),
    # Card 1: Query Details
    bslib::card(
      bslib::card_header("Query Details"),
      bslib::card_body(
        shiny::uiOutput(ns("query_meta"))
      )
    ),
    # Card 2: Parameters + Run
    bslib::card(
      bslib::card_header("Parameters"),
      bslib::card_body(
        shiny::uiOutput(ns("input_form"))
      )
    ),
    # Card 3: Results
    bslib::card(
      bslib::card_header("Results"),
      bslib::card_body(
        DT::DTOutput(ns("results_dt")),
        shiny::uiOutput(ns("scope_info"))
      )
    )
  )
}

.mod_catalog_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    catalog_df      <- shiny::reactiveVal(NULL)
    selected_query  <- shiny::reactiveVal(NULL)
    display_data    <- shiny::reactiveVal(NULL)
    concept_search_results <- shiny::reactiveVal(NULL)
    last_exec_meta  <- shiny::reactiveVal(NULL)

    # ── Load catalog list ──────────────────────────────────────────────────────
    load_catalog <- function() {
      domain <- input$domain_filter
      if (is.null(domain) || nchar(domain) == 0) domain <- NULL
      tryCatch({
        df <- ds.omop.catalog.list(domain = domain, symbol = state$symbol)
        catalog_df(df)
      }, error = function(e) {
        shiny::showNotification(
          paste("Catalog error:", conditionMessage(e)),
          type = "error", duration = 5
        )
        catalog_df(NULL)
      })
    }

    shiny::observeEvent(input$domain_filter, {
      load_catalog()
      selected_query(NULL)
      display_data(NULL)
      concept_search_results(NULL)
    }, ignoreNULL = FALSE)

    # ── Query list table ───────────────────────────────────────────────────────
    output$query_list_dt <- DT::renderDT({
      df <- catalog_df()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      show_cols <- intersect(c("group", "name"), names(df))
      DT::datatable(
        df[, show_cols, drop = FALSE],
        options = list(pageLength = 50, dom = "f", scrollX = TRUE,
                       scrollY = "400px"),
        rownames = FALSE, selection = "single"
      )
    })

    # ── Select query → fetch details ───────────────────────────────────────────
    shiny::observeEvent(input$query_list_dt_rows_selected, {
      idx <- input$query_list_dt_rows_selected
      df <- catalog_df()
      if (is.null(idx) || is.null(df) || idx > nrow(df)) return()

      qid <- df$id[idx]
      display_data(NULL)
      concept_search_results(NULL)
      last_exec_meta(NULL)

      tryCatch({
        meta <- ds.omop.catalog.get(qid, symbol = state$symbol)
        selected_query(meta)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading query:", conditionMessage(e)),
          type = "error"
        )
        selected_query(NULL)
      })
    })

    # ── Query metadata display ─────────────────────────────────────────────────
    output$query_meta <- shiny::renderUI({
      q <- selected_query()
      if (is.null(q)) {
        return(shiny::p(class = "text-muted", "Select a query from the list."))
      }

      # Badges
      class_badge <- shiny::span(
        class = "badge bg-primary me-1",
        q$class %||% "UNKNOWN"
      )
      pool_badge <- if (isTRUE(q$poolable)) {
        shiny::span(class = "badge bg-success me-1", "Poolable")
      }
      sensitive_text <- if (!is.null(q$sensitive_fields) &&
                            length(q$sensitive_fields) > 0) {
        shiny::p(shiny::strong("Sensitive fields: "),
                 paste(q$sensitive_fields, collapse = ", "))
      }
      cdm_text <- if (!is.null(q$cdm_version) && nchar(q$cdm_version) > 0) {
        shiny::p(shiny::strong("CDM version: "), q$cdm_version)
      }

      shiny::tagList(
        shiny::h5(q$name %||% q$id),
        shiny::p(q$description %||% ""),
        shiny::div(class = "mb-2", class_badge, pool_badge),
        sensitive_text,
        cdm_text
      )
    })

    # ── Dynamic parameter form ─────────────────────────────────────────────────
    output$input_form <- shiny::renderUI({
      q <- selected_query()
      if (is.null(q)) return(NULL)

      inputs_df <- q$inputs
      if (is.null(inputs_df) || !is.data.frame(inputs_df) || nrow(inputs_df) == 0) {
        return(shiny::tagList(
          shiny::p(class = "text-muted", "No parameters required."),
          shiny::actionButton(ns("run_btn"), "Run Query",
                              class = "btn-primary w-100 mt-2")
        ))
      }

      widgets <- lapply(seq_len(nrow(inputs_df)), function(i) {
        param   <- inputs_df$param[i]
        label   <- inputs_df$label[i] %||% param
        example <- inputs_df$example[i] %||% ""
        input_id <- ns(paste0("param_", param))

        if (param == "concept_id") {
          # Concept ID with embedded search helper
          shiny::tagList(
            shiny::numericInput(input_id, label,
              value = if (is.numeric(example) && !is.na(example))
                as.integer(example) else NA_integer_),
            shiny::div(
              class = "border rounded p-2 mb-2",
              style = "background: #f8f9fa;",
              shiny::div(class = "d-flex gap-1 mb-1",
                shiny::textInput(ns("concept_search_term"), NULL,
                  placeholder = "Search concept name...",
                  width = "100%"),
                shiny::actionButton(ns("concept_search_btn"), "Search",
                  class = "btn-sm btn-outline-secondary")
              ),
              DT::DTOutput(ns("concept_search_dt"))
            )
          )
        } else if (is.numeric(example) && !is.na(example)) {
          shiny::numericInput(input_id, label, value = as.numeric(example))
        } else {
          shiny::textInput(input_id, label,
            value = if (!is.na(example)) as.character(example) else "")
        }
      })

      shiny::tagList(
        widgets,
        shiny::actionButton(ns("run_btn"), "Run Query",
                            class = "btn-primary w-100 mt-2")
      )
    })

    # ── Concept search helper ──────────────────────────────────────────────────
    shiny::observeEvent(input$concept_search_btn, {
      term <- input$concept_search_term
      if (is.null(term) || nchar(trimws(term)) == 0) return()

      # Infer domain from query group
      q <- selected_query()
      domain <- NULL
      if (!is.null(q) && !is.null(q$group)) {
        domain <- switch(tolower(q$group),
          condition = "Condition", drug = "Drug",
          measurement = "Measurement", procedure = "Procedure",
          observation = "Observation", visit = "Visit",
          NULL
        )
      }

      tryCatch({
        res <- ds.omop.concept.search(
          pattern = term, domain = domain,
          standard_only = TRUE, limit = 20,
          symbol = state$symbol
        )
        srv <- names(res$per_site)[1]
        concept_search_results(res$per_site[[srv]])
      }, error = function(e) {
        shiny::showNotification(
          paste("Concept search error:", conditionMessage(e)),
          type = "error"
        )
      })
    })

    output$concept_search_dt <- DT::renderDT({
      df <- concept_search_results()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      keep <- intersect(c("concept_id", "concept_name", "domain_id"), names(df))
      DT::datatable(
        df[, keep, drop = FALSE],
        options = list(pageLength = 5, dom = "t", scrollX = TRUE,
                       scrollY = "150px"),
        rownames = FALSE, selection = "single"
      )
    })

    # When a concept search row is selected, fill the concept_id input
    shiny::observeEvent(input$concept_search_dt_rows_selected, {
      idx <- input$concept_search_dt_rows_selected
      df <- concept_search_results()
      if (is.null(idx) || is.null(df) || idx > nrow(df)) return()
      cid <- as.integer(df$concept_id[idx])
      shiny::updateNumericInput(session, "param_concept_id", value = cid)
    })

    # ── Run query ──────────────────────────────────────────────────────────────
    shiny::observeEvent(input$run_btn, {
      q <- selected_query()
      if (is.null(q)) {
        shiny::showNotification("Select a query first.", type = "warning")
        return()
      }

      # Collect parameter values
      inputs_df <- q$inputs
      params <- list()
      if (!is.null(inputs_df) && is.data.frame(inputs_df) && nrow(inputs_df) > 0) {
        for (i in seq_len(nrow(inputs_df))) {
          param <- inputs_df$param[i]
          val <- input[[paste0("param_", param)]]
          mandatory <- if ("mandatory" %in% names(inputs_df))
            isTRUE(inputs_df$mandatory[i]) else FALSE
          if (mandatory && (is.null(val) || is.na(val) ||
              (is.character(val) && nchar(trimws(val)) == 0))) {
            shiny::showNotification(
              paste0("Parameter '", param, "' is required."),
              type = "error"
            )
            return()
          }
          if (!is.null(val) && !is.na(val) &&
              !(is.character(val) && nchar(trimws(val)) == 0)) {
            params[[param]] <- val
          }
        }
      }

      scope  <- input$scope %||% "per_site"
      policy <- input$pooling_policy %||% "strict"
      state$scope <- scope
      state$pooling_policy <- policy

      shiny::showNotification("Running query...", type = "message",
                              duration = 2, id = "catalog_loading")

      tryCatch({
        mode <- q$mode %||% "aggregate"
        results <- ds.omop.catalog.exec(
          query_id = q$id, inputs = params,
          mode = mode, symbol = state$symbol
        )

        # Build and accumulate code for Script tab
        code <- .build_code("ds.omop.catalog.exec",
          query_id = q$id, inputs = params,
          mode = mode, symbol = state$symbol)
        state$script_lines <- c(state$script_lines, code)

        # Determine display data
        if (scope == "pooled" && isTRUE(q$poolable)) {
          pooled <- ds.omop.catalog.pool(
            results, query_id = q$id,
            policy = policy, symbol = state$symbol
          )
          pool_code <- .build_code("ds.omop.catalog.pool",
            results = "results", query_id = q$id,
            policy = policy, symbol = state$symbol)
          state$script_lines <- c(state$script_lines, pool_code)

          if (!is.null(pooled) && is.data.frame(pooled)) {
            display_data(pooled)
            last_exec_meta(list(scope = "pooled", servers = names(results)))
          } else {
            srv <- names(results)[1]
            display_data(if (is.data.frame(results[[srv]])) results[[srv]] else NULL)
            last_exec_meta(list(scope = "per_site", servers = names(results),
              warnings = "Pooling returned NULL; showing first server."))
          }
        } else {
          srv <- names(results)[1]
          display_data(if (is.data.frame(results[[srv]])) results[[srv]] else NULL)
          last_exec_meta(list(scope = "per_site", servers = names(results)))
        }

        shiny::removeNotification("catalog_loading")
      }, error = function(e) {
        shiny::removeNotification("catalog_loading")
        shiny::showNotification(
          paste("Query error:", conditionMessage(e)),
          type = "error", duration = 8
        )
      })
    })

    # ── Results table ──────────────────────────────────────────────────────────
    output$results_dt <- DT::renderDT({
      df <- display_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      # Identify sensitive columns and remove suppressed rows entirely
      q <- selected_query()
      sensitive <- q$sensitive_fields %||% character(0)
      if (length(sensitive) == 0) {
        sensitive <- names(df)[grepl("^n_|_count$|count_value|num_|n$",
                                     names(df), ignore.case = TRUE)]
      }

      for (col in intersect(sensitive, names(df))) {
        df <- df[!is.na(df[[col]]), , drop = FALSE]
      }

      if (nrow(df) == 0) return(NULL)

      DT::datatable(df,
        options = list(pageLength = 20, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "none"
      )
    })

    # ── Scope info ─────────────────────────────────────────────────────────────
    output$scope_info <- shiny::renderUI({
      meta <- last_exec_meta()
      if (is.null(meta)) return(NULL)

      tags <- lapply(meta$servers, function(srv) {
        shiny::span(class = "server-badge server-badge-ok", srv)
      })
      scope_text <- if (meta$scope == "pooled") "Pooled" else "Per-site"
      warns <- meta$warnings
      warn_ui <- if (!is.null(warns) && length(warns) > 0) {
        shiny::div(class = "text-warning mt-1",
          shiny::icon("exclamation-triangle"),
          paste(warns, collapse = "; ")
        )
      }

      shiny::div(class = "scope-indicator mt-2",
        shiny::span("Scope: ", scope_text, " | Servers: "),
        shiny::tagList(tags),
        warn_ui
      )
    })
  })
}
