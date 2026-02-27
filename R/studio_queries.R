# ==============================================================================
# MODULE: Query Library Browser
# Browse, parameterise, and execute query templates
# ==============================================================================

.mod_queries_ui <- function(id) {

  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Query Library", width = 320, open = "desktop",
      shiny::selectInput(ns("domain_filter"), "Domain",
        choices = c("All Domains" = "", "Condition" = "Condition",
                    "Drug" = "Drug", "Measurement" = "Measurement",
                    "Observation" = "Observation",
                    "Procedure" = "Procedure", "Person" = "Person",
                    "Visit" = "Visit", "Death" = "Death",
                    "General" = "General"),
        selected = ""),
      DT::DTOutput(ns("query_list_dt"))
    ),

    # --- Selected query ---
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Query"),
        shiny::actionButton(ns("run_btn"), "Run",
                            icon = shiny::icon("circle-play"),
                            class = "btn-sm btn-success text-white",
                            style = "font-weight: 600;")
      ),
      bslib::card_body(
        shiny::uiOutput(ns("query_meta")),
        shiny::uiOutput(ns("input_form"))
      )
    ),
    # --- Results ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Results"),
      bslib::card_body(
        DT::DTOutput(ns("results_dt")),
        shiny::uiOutput(ns("scope_info"))
      )
    ),
    # --- Visualization ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Visualization"),
      bslib::card_body(
        plotly::plotlyOutput(ns("auto_plot"), height = "350px")
      )
    )
  )
}

.mod_queries_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    queries_df      <- shiny::reactiveVal(NULL)
    selected_query  <- shiny::reactiveVal(NULL)
    display_data    <- shiny::reactiveVal(NULL)
    raw_results     <- shiny::reactiveVal(NULL)
    concept_search_results <- shiny::reactiveVal(NULL)
    last_exec_meta  <- shiny::reactiveVal(NULL)

    # -- Load query list -------------------------------------------------------
    load_queries <- function() {
      domain <- input$domain_filter
      if (is.null(domain) || nchar(domain) == 0) domain <- NULL
      tryCatch({
        df <- ds.omop.query.list(domain = domain, symbol = state$symbol)
        queries_df(df)
      }, error = function(e) {
        shiny::showNotification(
          paste("Query library error:", conditionMessage(e)),
          type = "error", duration = 5
        )
        queries_df(NULL)
      })
    }

    shiny::observeEvent(input$domain_filter, {
      load_queries()
      selected_query(NULL)
      display_data(NULL)
      concept_search_results(NULL)
    }, ignoreNULL = FALSE)

    # -- Query list table ------------------------------------------------------
    output$query_list_dt <- DT::renderDT({
      df <- queries_df()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
      show_cols <- intersect(c("group", "name"), names(df))
      DT::datatable(
        df[, show_cols, drop = FALSE],
        options = list(pageLength = 50, dom = "ft",
                       scrollX = TRUE, scrollY = "200px"),
        rownames = FALSE, selection = "single"
      )
    })

    # -- Select query -> fetch details -----------------------------------------
    shiny::observeEvent(input$query_list_dt_rows_selected, {
      idx <- input$query_list_dt_rows_selected
      df <- queries_df()
      if (is.null(idx) || is.null(df) || idx > nrow(df)) return()

      qid <- df$id[idx]
      display_data(NULL)
      concept_search_results(NULL)
      last_exec_meta(NULL)

      tryCatch({
        meta <- ds.omop.query.get(qid, symbol = state$symbol)
        selected_query(meta)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error loading query:", conditionMessage(e)),
          type = "error"
        )
        selected_query(NULL)
      })
    })

    # -- Query metadata display ------------------------------------------------
    output$query_meta <- shiny::renderUI({
      q <- selected_query()
      if (is.null(q)) {
        return(.empty_state_ui("book-open", "Select a query",
                               "Choose a query from the library above."))
      }

      # Badges
      class_badge <- shiny::span(
        class = "badge bg-primary me-1",
        q$class %||% "UNKNOWN"
      )
      pool_badge <- if (isTRUE(q$poolable)) {
        shiny::span(class = "badge me-1",
                    style = "background:#ecfdf5; color:#065f46;", "Poolable")
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

    # -- Dynamic parameter form (NO run button inside) -------------------------
    output$input_form <- shiny::renderUI({
      q <- selected_query()
      if (is.null(q)) return(NULL)

      inputs_df <- q$inputs
      if (is.null(inputs_df) || !is.data.frame(inputs_df) || nrow(inputs_df) == 0) {
        return(shiny::p(class = "text-muted", "No parameters required."))
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

      shiny::tagList(widgets)
    })

    # -- Concept search helper -------------------------------------------------
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

    # -- Run query -------------------------------------------------------------
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

      scope  <- state$scope %||% "pooled"
      policy <- state$pooling_policy %||% "strict"

      shiny::withProgress(message = "Running query...", value = 0.3, {
        tryCatch({
          mode <- q$mode %||% "aggregate"
          results <- ds.omop.query.exec(
            query_id = q$id, inputs = params,
            mode = mode, symbol = state$symbol
          )
          shiny::incProgress(0.4)

          # Build and accumulate code for Script tab
          code <- .build_code("ds.omop.query.exec",
            query_id = q$id, inputs = params,
            mode = mode, symbol = state$symbol)
          state$script_lines <- c(state$script_lines, code)

          # Store raw results for scope switching without re-query
          raw_results(results)

          # Pool results if scope is pooled and query is poolable
          pooled_df <- NULL
          if (scope == "pooled" && isTRUE(q$poolable)) {
            tryCatch({
              pooled_df <- ds.omop.query.pool(
                results, query_id = q$id,
                policy = policy, symbol = state$symbol
              )
              pool_code <- .build_code("ds.omop.query.pool",
                results = "results", query_id = q$id,
                policy = policy, symbol = state$symbol)
              state$script_lines <- c(state$script_lines, pool_code)
            }, error = function(e) NULL)
          }

          shiny::incProgress(0.2)

          # Determine display data based on current scope
          if (scope == "pooled" && !is.null(pooled_df) && is.data.frame(pooled_df)) {
            display_data(pooled_df)
            last_exec_meta(list(scope = "pooled", servers = names(results)))
          } else {
            # per_site: show first selected server
            display_data(.extract_display_data(results, "per_site",
              state$selected_servers))
            last_exec_meta(list(scope = scope, servers = names(results)))
          }
        }, error = function(e) {
          shiny::showNotification(
            .clean_ds_error(e),
            type = "error", duration = 5
          )
        })
      })
    })

    # -- Re-extract on scope/server change (no re-query) -----------------------
    shiny::observeEvent(list(state$scope, state$selected_servers), {
      results <- raw_results()
      if (is.null(results)) return()
      scope <- state$scope %||% "pooled"
      q <- selected_query()

      if (scope == "pooled" && isTRUE(q$poolable)) {
        policy <- state$pooling_policy %||% "strict"
        tryCatch({
          pooled_df <- ds.omop.query.pool(
            results, query_id = q$id,
            policy = policy, symbol = state$symbol
          )
          if (!is.null(pooled_df) && is.data.frame(pooled_df)) {
            display_data(pooled_df)
          } else {
            display_data(.extract_display_data(results, "per_site",
                                                state$selected_servers))
          }
        }, error = function(e) {
          display_data(.extract_display_data(results, "per_site",
                                              state$selected_servers))
        })
        last_exec_meta(list(scope = "pooled", servers = names(results)))
      } else {
        display_data(.extract_display_data(results, "per_site",
          state$selected_servers))
        last_exec_meta(list(scope = scope, servers = names(results)))
      }
    }, ignoreInit = TRUE)

    # -- Results table ---------------------------------------------------------
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

    # -- Auto-visualization -----------------------------------------------------
    output$auto_plot <- plotly::renderPlotly({
      df <- display_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0)
        return(plotly::plotly_empty() |> plotly::config(displayModeBar = FALSE))

      q <- selected_query()
      qid <- if (!is.null(q)) (q$id %||% "") else ""

      .safe_plotly(.auto_plot_query(df, qid))
    })

    # -- Scope info ------------------------------------------------------------
    output$scope_info <- shiny::renderUI({
      meta <- last_exec_meta()
      if (is.null(meta)) return(NULL)

      tags <- lapply(meta$servers, function(srv) {
        shiny::span(class = "server-badge server-badge-ok", srv)
      })
      scope_text <- switch(meta$scope,
        "pooled" = "Pooled", "all" = "All Servers", "Per-site")
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

# ==============================================================================
# Auto-plot: intelligently pick chart type from query result columns
# ==============================================================================

.auto_plot_query <- function(df, qid = "") {
  cols <- names(df)

  # --- Helpers ---------------------------------------------------------------
  has <- function(pattern) any(grepl(pattern, cols, ignore.case = TRUE))
  get_col <- function(pattern) {
    m <- grep(pattern, cols, ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) m[1] else NULL
  }
  # Truncate long labels
  trunc_label <- function(x, n = 35) {
    x <- as.character(x)
    ifelse(nchar(x) > n, paste0(substr(x, 1, n - 2), "\u2026"), x)
  }

  # Identify key columns
  count_col  <- get_col("^n_persons$|^n_records$|^count_value$|^n_values$|^num_persons$")
  name_col   <- get_col("concept_name|_name$")
  year_col   <- get_col("year$|_year$|record_year|exposure_year")
  age_col    <- get_col("age_group|age_decade")
  domain_col <- get_col("^domain$|^domain_id$")
  server_col <- get_col("^server$")
  mean_col   <- get_col("^mean_|^avg_")
  sd_col     <- get_col("^sd_|^stdev_")

  if (is.null(count_col) && is.null(mean_col)) {
    # Fallback: first numeric column
    num_cols <- cols[vapply(df, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, grep("concept_id|_id$", num_cols, value = TRUE))
    if (length(num_cols) > 0) count_col <- num_cols[1]
  }

  colors <- .studio_colors

  # ---- Pattern 1: Temporal (has year column) --------------------------------
  if (!is.null(year_col) && !is.null(count_col)) {
    df <- df[order(df[[year_col]]), ]
    group_col <- name_col %||% domain_col %||% server_col

    if (!is.null(group_col) && length(unique(df[[group_col]])) > 1 &&
        length(unique(df[[group_col]])) <= 15) {
      groups <- unique(df[[group_col]])
      p <- plotly::plot_ly()
      for (i in seq_along(groups)) {
        sub <- df[df[[group_col]] == groups[i], ]
        p <- p |> plotly::add_trace(
          x = sub[[year_col]], y = sub[[count_col]],
          type = "scatter", mode = "lines+markers",
          name = trunc_label(groups[i], 30),
          line = list(color = colors[((i - 1) %% length(colors)) + 1], width = 2),
          marker = list(color = colors[((i - 1) %% length(colors)) + 1], size = 5),
          hovertemplate = paste0(
            "<b>%{x}</b><br>", count_col, ": %{y:,.0f}<extra>",
            trunc_label(groups[i], 20), "</extra>")
        )
      }
      return(p |> .plotly_defaults("Temporal Trend"))
    } else {
      # Single line
      p <- plotly::plot_ly(df, x = ~get(year_col), y = ~get(count_col),
        type = "scatter", mode = "lines+markers",
        line = list(color = colors[1], width = 2),
        marker = list(color = colors[1], size = 5),
        hovertemplate = paste0("<b>%{x}</b><br>", count_col, ": %{y:,.0f}<extra></extra>")
      )
      return(p |> .plotly_defaults("Temporal Trend"))
    }
  }

  # ---- Pattern 2: Age distribution (has age_group column) -------------------
  if (!is.null(age_col) && !is.null(count_col)) {
    # Aggregate if multiple rows per age group (e.g. multiple concepts)
    agg <- stats::aggregate(
      stats::as.formula(paste(count_col, "~", age_col)),
      data = df, FUN = sum, na.rm = TRUE
    )
    p <- plotly::plot_ly(agg, x = ~get(age_col), y = ~get(count_col),
      type = "bar",
      marker = list(
        color = colors[1],
        line = list(color = "rgba(0,0,0,0.05)", width = 1)
      ),
      hovertemplate = paste0("<b>%{x}</b><br>", count_col, ": %{y:,.0f}<extra></extra>")
    )
    return(p |> .plotly_defaults("Distribution by Age Group"))
  }

  # ---- Pattern 3: Stats summary (has mean/sd columns) -----------------------
  if (!is.null(mean_col)) {
    label_col <- name_col %||% get_col("concept_id|_id$")
    if (is.null(label_col)) label_col <- cols[1]

    df$label <- trunc_label(df[[label_col]])
    means <- df[[mean_col]]
    sds <- if (!is.null(sd_col)) df[[sd_col]] else rep(0, nrow(df))

    # Show top 20 by mean value (descending)
    ord <- order(means, decreasing = TRUE)
    df <- df[utils::head(ord, 20), ]
    means <- df[[mean_col]]
    sds <- if (!is.null(sd_col)) df[[sd_col]] else rep(0, nrow(df))

    p <- plotly::plot_ly(df, x = ~label, y = means,
      type = "bar",
      error_y = list(type = "data", array = sds, color = "#94a3b8", thickness = 1.5),
      marker = list(color = colors[5],
        line = list(color = "rgba(0,0,0,0.05)", width = 1)),
      hovertemplate = paste0(
        "<b>%{x}</b><br>Mean: %{y:.2f}<br>SD: ",
        if (!is.null(sd_col)) "%{error_y.array:.2f}" else "N/A",
        "<extra></extra>")
    )
    title <- gsub("^mean_|^avg_", "", mean_col)
    return(p |> .plotly_defaults(paste("Mean", title, "(+/- SD)")))
  }

  # ---- Pattern 4: Categorical bar chart (name + count) ----------------------
  if (!is.null(name_col) && !is.null(count_col)) {
    # Sort descending, take top 25
    df <- df[order(df[[count_col]], decreasing = TRUE), ]
    df <- utils::head(df, 25)
    df$label <- trunc_label(df[[name_col]])

    # If there's a server column with multiple servers, use grouped bars
    if (!is.null(server_col) && length(unique(df[[server_col]])) > 1 &&
        length(unique(df[[server_col]])) <= 10) {
      servers <- unique(df[[server_col]])
      p <- plotly::plot_ly()
      for (i in seq_along(servers)) {
        sub <- df[df[[server_col]] == servers[i], ]
        p <- p |> plotly::add_trace(
          y = ~sub$label, x = ~sub[[count_col]],
          type = "bar", orientation = "h",
          name = servers[i],
          marker = list(color = colors[((i - 1) %% length(colors)) + 1]),
          hovertemplate = paste0(
            "<b>%{y}</b><br>", count_col, ": %{x:,.0f}<extra>",
            servers[i], "</extra>")
        )
      }
      p <- p |> plotly::layout(barmode = "group")
      return(p |> .plotly_defaults("Top Concepts"))
    }

    # Single series horizontal bar
    p <- plotly::plot_ly(df, y = ~reorder(label, get(count_col)),
      x = ~get(count_col),
      type = "bar", orientation = "h",
      marker = list(
        color = colors[1],
        line = list(color = "rgba(0,0,0,0.05)", width = 1)
      ),
      hovertemplate = paste0("<b>%{y}</b><br>", count_col, ": %{x:,.0f}<extra></extra>")
    )
    return(p |> .plotly_defaults("Top Concepts") |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 180)))
  }

  # ---- Pattern 5: Domain pie/donut (domain_id + count) ----------------------
  if (!is.null(domain_col) && !is.null(count_col)) {
    agg <- stats::aggregate(
      stats::as.formula(paste(count_col, "~", domain_col)),
      data = df, FUN = sum, na.rm = TRUE
    )
    p <- plotly::plot_ly(agg, labels = ~get(domain_col), values = ~get(count_col),
      type = "pie", hole = 0.45,
      marker = list(colors = colors[seq_len(nrow(agg))]),
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b><br>Count: %{value:,.0f}<extra></extra>"
    )
    return(p |> .plotly_defaults("Distribution by Domain"))
  }

  # ---- Fallback: first numeric column as bar chart --------------------------
  if (!is.null(count_col)) {
    label_col <- cols[!cols %in% c(count_col, grep("_id$", cols, value = TRUE))][1]
    if (is.null(label_col)) label_col <- cols[1]
    df <- df[order(df[[count_col]], decreasing = TRUE), ]
    df <- utils::head(df, 25)
    df$label <- trunc_label(df[[label_col]])

    p <- plotly::plot_ly(df, y = ~reorder(label, get(count_col)),
      x = ~get(count_col),
      type = "bar", orientation = "h",
      marker = list(color = colors[1]),
      hovertemplate = "<b>%{y}</b><br>%{x:,.0f}<extra></extra>"
    )
    return(p |> .plotly_defaults() |>
      plotly::layout(yaxis = list(title = ""), margin = list(l = 160)))
  }

  # Nothing plottable
  plotly::plotly_empty() |> plotly::config(displayModeBar = FALSE)
}
