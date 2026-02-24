# ==============================================================================
# MODULE 3: Concept Drilldown
# Replaces old Values + Trends tabs
# ==============================================================================

.mod_concept_drilldown_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Concept Info", width = 300,
      shiny::uiOutput(ns("concept_info")),
      shiny::hr(),
      shiny::actionButton(ns("add_to_set"), "Add to Concept Set",
                          class = "btn-outline-primary w-100 mb-2"),
      shiny::actionButton(ns("add_to_cart_extract"),
                          shiny::tagList(shiny::icon("plus"), "Extract to Cart"),
                          class = "btn-outline-success w-100 mb-2"),
      shiny::actionButton(ns("add_to_cart_filter"),
                          shiny::tagList(shiny::icon("filter"), "Filter in Cart"),
                          class = "btn-outline-warning w-100 mb-2"),
      shiny::actionButton(ns("add_to_plan"), "Add to Plan",
                          class = "btn-outline-info w-100 mb-2"),
      shiny::actionButton(ns("reload"), "Reload",
                          class = "btn-outline-secondary w-100")
    ),

    # Summary metrics
    bslib::card(
      bslib::card_header("Summary"),
      bslib::card_body(
        shiny::uiOutput(ns("summary_metrics"))
      )
    ),

    # Numeric distribution (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_numeric"), "']"),
      bslib::card(
        bslib::card_header("Numeric Distribution"),
        bslib::card_body(
          shiny::plotOutput(ns("histogram_plot"), height = "300px"),
          DT::DTOutput(ns("quantiles_dt"))
        )
      )
    ),

    # Categorical values (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_categorical"), "']"),
      bslib::card(
        bslib::card_header("Categorical Values"),
        bslib::card_body(
          shiny::plotOutput(ns("categorical_plot"), height = "300px"),
          DT::DTOutput(ns("categorical_dt"))
        )
      )
    ),

    # Date coverage (conditional)
    shiny::conditionalPanel(
      condition = paste0("output['", ns("has_dates"), "']"),
      bslib::card(
        bslib::card_header("Date Coverage"),
        bslib::card_body(
          shiny::plotOutput(ns("date_plot"), height = "250px"),
          shiny::uiOutput(ns("date_range_info"))
        )
      )
    ),

    # Missingness (always shown)
    bslib::card(
      bslib::card_header("Missingness"),
      bslib::card_body(
        shiny::plotOutput(ns("missingness_plot"), height = "250px")
      )
    )
  )
}

.mod_concept_drilldown_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    drilldown_data <- shiny::reactiveVal(NULL)

    # Auto-trigger when concept changes
    shiny::observeEvent(state$selected_concept_id, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()
      .run_drilldown(state, drilldown_data)
    })

    shiny::observeEvent(input$reload, {
      .run_drilldown(state, drilldown_data)
    })

    .run_drilldown <- function(state, drilldown_data) {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()

      shiny::showNotification("Loading drilldown...", type = "message",
                              duration = 3, id = "dd_loading")
      tryCatch({
        res <- ds.omop.concept.drilldown(
          table = tbl, concept_id = cid,
          symbol = state$symbol
        )
        # Accumulate code for Script tab
        if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
          state$script_lines <- c(state$script_lines, res$meta$call_code)
        }
        # Store per_site results for display (modules expect named list)
        drilldown_data(res$per_site)
        shiny::removeNotification("dd_loading")
      }, error = function(e) {
        shiny::removeNotification("dd_loading")
        shiny::showNotification(
          paste("Drilldown error:", conditionMessage(e)), type = "error"
        )
        drilldown_data(NULL)
      })
    }

    # Concept info sidebar
    output$concept_info <- shiny::renderUI({
      cid <- state$selected_concept_id
      if (is.null(cid)) {
        return(shiny::p("No concept selected.",
                        shiny::br(),
                        "Use the Explore tab to select a concept."))
      }
      shiny::tagList(
        shiny::p(shiny::strong("Concept ID: "), as.character(cid)),
        shiny::p(shiny::strong("Name: "),
                 as.character(state$selected_concept_name %||% "")),
        shiny::p(shiny::strong("Table: "),
                 as.character(state$selected_table %||% ""))
      )
    })

    # Add to concept set
    shiny::observeEvent(input$add_to_set, {
      cid <- state$selected_concept_id
      if (is.null(cid)) return()
      current <- state$concept_set
      if (!cid %in% current) {
        state$concept_set <- c(current, cid)
        shiny::showNotification(
          paste("Added concept", cid, "to set"),
          type = "message", duration = 2
        )
      }
    })

    # Add to cart as extraction variable
    shiny::observeEvent(input$add_to_cart_extract, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) {
        shiny::showNotification("No concept selected.", type = "warning")
        return()
      }
      cname <- state$selected_concept_name
      tryCatch({
        v <- omop_variable(
          table = tbl, concept_id = cid,
          concept_name = cname, format = "raw"
        )
        state$cart <- cart_add_variable(state$cart, v)
        shiny::showNotification(
          paste("Added", v$name, "to cart"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # Add to cart as filter
    shiny::observeEvent(input$add_to_cart_filter, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) {
        shiny::showNotification("No concept selected.", type = "warning")
        return()
      }
      cname <- state$selected_concept_name
      tryCatch({
        f <- omop_filter_has_concept(cid, tbl, cname)
        state$cart <- cart_add_filter(state$cart, f)
        shiny::showNotification(
          paste("Added filter for concept", cid, "to cart"),
          type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # Add to plan as events output
    shiny::observeEvent(input$add_to_plan, {
      cid <- state$selected_concept_id
      tbl <- state$selected_table
      if (is.null(cid) || is.null(tbl)) return()
      tryCatch({
        nm <- paste0("events_", cid)
        state$plan <- ds.omop.plan.events(
          state$plan, name = nm, table = tbl,
          concept_set = as.integer(cid)
        )
        shiny::showNotification(
          paste("Added events output for concept", cid),
          type = "message", duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error"
        )
      })
    })

    # Conditional panel outputs (must be text for JS condition)
    output$has_numeric <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$numeric_summary)
    })
    shiny::outputOptions(output, "has_numeric", suspendWhenHidden = FALSE)

    output$has_categorical <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$categorical_values) && is.data.frame(d$categorical_values) &&
        nrow(d$categorical_values) > 0
    })
    shiny::outputOptions(output, "has_categorical", suspendWhenHidden = FALSE)

    output$has_dates <- shiny::reactive({
      dd <- drilldown_data()
      if (is.null(dd)) return(FALSE)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      !is.null(d$date_range)
    })
    shiny::outputOptions(output, "has_dates", suspendWhenHidden = FALSE)

    # --- Summary metrics ---
    output$summary_metrics <- shiny::renderUI({
      dd <- drilldown_data()
      if (is.null(dd)) {
        return(shiny::p("Select a concept from the Explore tab."))
      }
      srv <- names(dd)[1]
      d <- dd[[srv]]
      s <- d$summary

      metrics <- list(
        shiny::div(class = "metric-card d-inline-block mx-3",
          shiny::div(class = "value", .fmt_count(s$n_records)),
          shiny::div(class = "label", "Records")
        ),
        shiny::div(class = "metric-card d-inline-block mx-3",
          shiny::div(class = "value", .fmt_count(s$n_persons)),
          shiny::div(class = "label", "Persons")
        )
      )

      if (!is.null(s$records_per_person_mean) && !is.na(s$records_per_person_mean)) {
        metrics <- c(metrics, list(
          shiny::div(class = "metric-card d-inline-block mx-3",
            shiny::div(class = "value",
                       format(round(s$records_per_person_mean, 2), nsmall = 2)),
            shiny::div(class = "label", "Records/Person")
          )
        ))
      }

      if (!is.null(s$pct_persons_multi) && !is.na(s$pct_persons_multi)) {
        # Longitudinal indicator
        longi_level <- if (s$pct_persons_multi >= 50) "High"
                       else if (s$pct_persons_multi >= 20) "Medium"
                       else "Low"
        longi_color <- if (longi_level == "High") "#27ae60"
                       else if (longi_level == "Medium") "#f39c12"
                       else "#95a5a6"
        metrics <- c(metrics, list(
          shiny::div(class = "metric-card d-inline-block mx-3",
            shiny::div(class = "value",
                       paste0(format(s$pct_persons_multi, nsmall = 1), "%")),
            shiny::div(class = "label", "Multi-record %")
          ),
          shiny::div(class = "metric-card d-inline-block mx-3",
            shiny::div(class = "value", style = paste0("color:", longi_color),
                       longi_level),
            shiny::div(class = "label", "Longitudinal")
          )
        ))
      }

      shiny::div(class = "d-flex flex-wrap justify-content-center", metrics)
    })

    # --- Numeric histogram ---
    output$histogram_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$numeric_summary) || is.null(d$numeric_summary$histogram)) {
        return(NULL)
      }
      df <- d$numeric_summary$histogram
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plot({
        cols <- ifelse(is.na(df$count) | df$suppressed, "#e74c3c", "#3498db")
        y <- df$count; y[is.na(y)] <- 0
        mids <- (df$bin_start + df$bin_end) / 2
        par(mar = c(5, 5, 2, 2))
        barplot(y, names.arg = round(mids, 1), col = cols,
                xlab = "value_as_number", ylab = "Count",
                las = 2, cex.names = 0.7)
        legend("topright", legend = c("OK", "Suppressed"),
               fill = c("#3498db", "#e74c3c"), cex = 0.8)
      })
    }, res = 96)

    output$quantiles_dt <- DT::renderDT({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$numeric_summary) ||
          is.null(d$numeric_summary$quantiles)) return(NULL)
      df <- d$numeric_summary$quantiles
      if (!is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 10, dom = "t", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })

    # --- Categorical values ---
    output$categorical_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plot({
        df$label <- substr(as.character(df$concept_name), 1, 30)
        df$y <- as.numeric(df$n)
        df <- df[!is.na(df$y), , drop = FALSE]
        if (nrow(df) == 0) return(NULL)

        df <- df[order(df$y, decreasing = TRUE), ]
        n <- min(nrow(df), 15)
        df <- df[seq_len(n), ]

        par(mar = c(5, 10, 2, 2))
        barplot(rev(df$y), names.arg = rev(df$label),
                horiz = TRUE, las = 1, col = "#2ecc71",
                xlab = "Count", cex.names = 0.75)
      })
    }, res = 96)

    output$categorical_dt <- DT::renderDT({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$categorical_values
      if (is.null(df) || !is.data.frame(df)) return(NULL)
      DT::datatable(df, options = list(pageLength = 10, dom = "ftip", scrollX = TRUE),
                    rownames = FALSE, selection = "none")
    })

    # --- Date coverage ---
    output$date_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      if (is.null(d$date_range) || is.null(d$date_range$date_counts)) {
        return(NULL)
      }
      df <- d$date_range$date_counts
      if (!is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plot({
        df <- df[order(df$period), ]
        y <- as.numeric(df$n_records); y[is.na(y)] <- 0
        sup_col <- if ("suppressed" %in% names(df)) df$suppressed else rep(FALSE, nrow(df))
        cols <- ifelse(is.na(df$n_records) | sup_col,
                       "#e74c3c", "#2c3e50")
        par(mar = c(7, 5, 2, 2))
        barplot(y, names.arg = df$period, col = cols,
                ylab = "Records", las = 2, cex.names = 0.7)
      })
    }, res = 96)

    output$date_range_info <- shiny::renderUI({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      dr <- d$date_range
      if (is.null(dr)) return(NULL)

      shiny::tags$dl(class = "row",
        shiny::tags$dt(class = "col-sm-4", "Date Column"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$column %||% "")),
        shiny::tags$dt(class = "col-sm-4", "Safe Min"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$min_date_safe %||% "N/A")),
        shiny::tags$dt(class = "col-sm-4", "Safe Max"),
        shiny::tags$dd(class = "col-sm-8",
                       as.character(dr$max_date_safe %||% "N/A"))
      )
    })

    # --- Missingness ---
    output$missingness_plot <- shiny::renderPlot({
      dd <- drilldown_data()
      if (is.null(dd)) return(NULL)
      srv <- names(dd)[1]
      d <- dd[[srv]]
      df <- d$missingness
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

      .safe_plot({
        df <- df[order(df$missing_rate, decreasing = TRUE), ]
        par(mar = c(5, 12, 2, 2))
        barplot(rev(df$missing_rate * 100),
                names.arg = rev(df$column_name),
                horiz = TRUE, las = 1, col = "#e67e22",
                xlab = "Missing %", xlim = c(0, 100),
                cex.names = 0.7)
      })
    }, res = 96)
  })
}

