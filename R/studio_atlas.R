# ==============================================================================
# MODULE: Atlas Data Sources (Achilles-backed statistics)
# ==============================================================================
# ATLAS-style interactive pages: Dashboard, Person, Conditions, Drugs,
# Procedures, Measurements, Observations, Visits, Trends, Data Quality.
# ==============================================================================

.mod_atlas_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Data Sources", width = 320,
      shiny::uiOutput(ns("achilles_status")),
      shiny::hr(),
      shiny::h6("Navigation"),
      shiny::selectInput(ns("atlas_nav"), NULL,
        choices = c("Dashboard", "Person", "Conditions", "Drugs",
                    "Procedures", "Measurements", "Observations",
                    "Visits", "Trends", "Data Quality"),
        selected = "Dashboard"),
      .scope_controls_ui(ns)
    ),
    bslib::card(
      bslib::card_header(shiny::textOutput(ns("page_title"))),
      bslib::card_body(
        shiny::uiOutput(ns("page_content"))
      )
    )
  )
}

.mod_atlas_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    achilles_available <- shiny::reactiveVal(FALSE)
    achilles_catalog <- shiny::reactiveVal(NULL)
    page_data <- shiny::reactiveVal(NULL)

    # Check Achilles availability and fetch catalog on load
    shiny::observe({
      tryCatch({
        status <- ds.omop.achilles.status(symbol = state$symbol)
        srv <- names(status$per_site)[1]
        avail <- isTRUE(status$per_site[[srv]]$available)
        achilles_available(avail)

        if (avail) {
          cat_res <- ds.omop.achilles.catalog(symbol = state$symbol)
          srv_cat <- cat_res$per_site[[names(cat_res$per_site)[1]]]
          achilles_catalog(srv_cat)

          shiny::isolate({
            if (inherits(cat_res, "dsomop_result") &&
                nchar(cat_res$meta$call_code) > 0) {
              state$script_lines <- c(state$script_lines, cat_res$meta$call_code)
            }
          })
        }

        # Accumulate code (ISOLATED to break reactive loop)
        shiny::isolate({
          if (inherits(status, "dsomop_result") &&
              nchar(status$meta$call_code) > 0) {
            state$script_lines <- c(state$script_lines, status$meta$call_code)
          }
        })
      }, error = function(e) achilles_available(FALSE))
    })

    # Dynamic navigation choices from catalog
    shiny::observe({
      cat <- achilles_catalog()
      if (is.null(cat)) return()

      domains <- unique(cat$domain)
      pages <- c("Dashboard")
      domain_page_map <- c(
        person = "Person", condition = "Conditions", drug = "Drugs",
        procedure = "Procedures", measurement = "Measurements",
        observation = "Observations", visit = "Visits",
        observation_period = "Observation Period"
      )
      for (d in names(domain_page_map)) {
        if (d %in% domains) pages <- c(pages, domain_page_map[d])
      }
      # Add Trends if any domain has time-stratified analyses
      if (any(!is.na(cat$stratum_2_name) & cat$stratum_2_name == "calendar_month")) {
        pages <- c(pages, "Trends")
      }
      pages <- c(pages, "Data Quality")

      shiny::updateSelectInput(session, "atlas_nav", choices = pages)
    })

    # Status display
    output$achilles_status <- shiny::renderUI({
      if (achilles_available()) {
        cat <- achilles_catalog()
        n_analyses <- if (!is.null(cat)) nrow(cat) else 0
        shiny::div(
          shiny::span(class = "status-ok", shiny::icon("check-circle"),
                      " Achilles Available"),
          shiny::p(class = "text-muted small mt-1",
                   paste0(n_analyses, " analyses loaded"))
        )
      } else {
        shiny::div(
          shiny::span(class = "status-warn", shiny::icon("exclamation-triangle"),
                      " Achilles Not Found"),
          shiny::p(class = "text-muted small mt-1",
                   "Run Achilles on the CDM to enable Data Sources")
        )
      }
    })

    output$page_title <- shiny::renderText({
      paste("Data Sources:", input$atlas_nav)
    })

    # Reactive: fetch data when page changes
    shiny::observeEvent(input$atlas_nav, {
      if (!achilles_available()) {
        page_data(NULL)
        return()
      }

      nav <- input$atlas_nav
      cat <- achilles_catalog()
      scope <- input$scope %||% "per_site"
      policy <- input$pooling_policy %||% "strict"

      tryCatch({
        data <- if (nav == "Dashboard") {
          .atlas_fetch_dashboard(state, scope, policy)
        } else if (nav == "Person") {
          .atlas_fetch_person(state, scope, policy)
        } else if (nav == "Visits") {
          .atlas_fetch_visits(state, scope, policy)
        } else if (nav == "Trends") {
          .atlas_fetch_trends_dynamic(state, cat, scope, policy)
        } else if (nav == "Data Quality") {
          .atlas_fetch_quality(state, scope, policy)
        } else {
          # Generic domain page — look up analyses from catalog
          domain <- .page_to_domain(nav)
          if (!is.null(cat) && !is.null(domain)) {
            domain_analyses <- cat[cat$domain == domain, , drop = FALSE]
            .atlas_fetch_domain_dynamic(state, domain_analyses, scope, policy)
          } else {
            # Fallback to hardcoded IDs
            switch(nav,
              "Conditions" = .atlas_fetch_domain(state, 400L, 401L, scope, policy),
              "Drugs"     = .atlas_fetch_domain(state, 700L, 701L, scope, policy),
              "Procedures" = .atlas_fetch_domain(state, 600L, 601L, scope, policy),
              "Measurements" = .atlas_fetch_domain(state, 1800L, 1801L, scope, policy),
              "Observations" = .atlas_fetch_domain(state, 800L, NULL, scope, policy),
              NULL
            )
          }
        }
        page_data(data %||% list(no_data = TRUE))
      }, error = function(e) {
        page_data(list(error = conditionMessage(e)))
      })
    })

    # Render page content
    output$page_content <- shiny::renderUI({
      if (!achilles_available()) {
        return(shiny::div(class = "text-center text-muted py-5",
          shiny::icon("database", class = "fa-3x mb-3"),
          shiny::h5("Achilles statistics not available"),
          shiny::p("Pre-computed statistics are required for Data Sources.",
                   "Run OHDSI Achilles on your CDM to generate them.")
        ))
      }

      data <- page_data()
      if (is.null(data)) {
        return(shiny::div(class = "text-center py-3",
          shiny::icon("spinner", class = "fa-spin"), " Loading..."))
      }

      if (!is.null(data$error)) {
        return(shiny::div(class = "alert alert-warning", data$error))
      }

      if (isTRUE(data$no_data)) {
        return(shiny::div(class = "text-center text-muted py-5",
          shiny::icon("circle-info", class = "fa-2x mb-3"),
          shiny::h5("No data available for this page")
        ))
      }

      nav <- input$atlas_nav
      switch(nav,
        "Dashboard"    = .atlas_render_dashboard(ns, data),
        "Person"       = .atlas_render_person(ns, data),
        "Conditions"   = .atlas_render_domain(ns, data, "Conditions"),
        "Drugs"        = .atlas_render_domain(ns, data, "Drugs"),
        "Procedures"   = .atlas_render_domain(ns, data, "Procedures"),
        "Measurements" = .atlas_render_domain(ns, data, "Measurements"),
        "Observations" = .atlas_render_domain(ns, data, "Observations"),
        "Visits"       = .atlas_render_visits(ns, data),
        "Trends"       = .atlas_render_trends(ns, data),
        "Data Quality" = .atlas_render_quality(ns, data),
        shiny::div("Unknown page")
      )
    })

    # Dashboard plot outputs
    output$dashboard_gender_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$gender)) return()
      .safe_plot({
        df <- data$gender
        df$count_value <- as.numeric(df$count_value)
        if (nrow(df) == 0) return()
        vals <- df$count_value[!is.na(df$count_value)]
        labels <- df$stratum_1
        # Map concept IDs to labels
        labels[labels == "8507"] <- "Male"
        labels[labels == "8532"] <- "Female"
        labels <- labels[!is.na(df$count_value)]
        if (length(vals) > 0) {
          pie(vals, labels = paste0(labels, " (", vals, ")"),
              col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
              main = "Gender Distribution")
        }
      })
    })

    output$dashboard_age_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$age_dist)) return()
      .safe_plot({
        df <- data$age_dist
        if (nrow(df) == 0 || all(is.na(df$avg_value))) return()
        row <- df[1, ]
        vals <- c(row$min_value, row$p10_value, row$p25_value,
                  row$median_value, row$p75_value, row$p90_value,
                  row$max_value)
        names(vals) <- c("Min", "P10", "P25", "Median", "P75", "P90", "Max")
        vals <- vals[!is.na(vals)]
        if (length(vals) > 0) {
          barplot(vals, main = "Age at First Observation",
                  col = "#3498db", ylab = "Age (years)")
        }
      })
    })

    # Person page plots
    output$person_gender_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$gender)) return()
      .safe_plot({
        df <- data$gender
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        labels <- df$stratum_1
        labels[labels == "8507"] <- "Male"
        labels[labels == "8532"] <- "Female"
        barplot(df$count_value, names.arg = labels,
                col = c("#3498db", "#e74c3c", "#2ecc71", "#f39c12"),
                main = "Gender", ylab = "Persons")
      })
    })

    output$person_yob_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$yob)) return()
      .safe_plot({
        df <- data$yob
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        barplot(df$count_value, names.arg = df$stratum_1,
                col = "#3498db", main = "Year of Birth", ylab = "Persons",
                las = 2, cex.names = 0.7)
      })
    })

    output$person_race_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$race)) return()
      .safe_plot({
        df <- data$race
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        barplot(df$count_value, names.arg = df$stratum_1,
                col = "#2ecc71", main = "Race", ylab = "Persons",
                horiz = TRUE, las = 1, cex.names = 0.8)
      })
    })

    output$person_ethnicity_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$ethnicity)) return()
      .safe_plot({
        df <- data$ethnicity
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        barplot(df$count_value, names.arg = df$stratum_1,
                col = "#f39c12", main = "Ethnicity", ylab = "Persons",
                horiz = TRUE, las = 1, cex.names = 0.8)
      })
    })

    # Domain plots (used by Conditions, Drugs, Procedures, Measurements, Observations)
    output$domain_bar_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return()
      .safe_plot({
        df <- data$concepts
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        # Sort by count descending
        df <- df[order(df$count_value, decreasing = TRUE), ]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), , drop = FALSE]
        labels <- if (!is.null(data$concept_names)) {
          nm <- data$concept_names
          vapply(df$stratum_1, function(id) {
            m <- nm$concept_name[nm$concept_id == as.integer(id)]
            if (length(m) > 0) m[1] else id
          }, character(1))
        } else df$stratum_1
        # Truncate labels
        labels <- substr(labels, 1, 30)
        par(mar = c(5, 12, 3, 2))
        barplot(rev(df$count_value), names.arg = rev(labels),
                col = "#3498db", main = paste("Top", n, "Concepts"),
                xlab = "Persons", horiz = TRUE, las = 1, cex.names = 0.7)
      })
    })

    output$domain_trend_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$trends)) return()
      .safe_plot({
        df <- data$trends
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        # Aggregate by month across concepts
        agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
        agg <- agg[order(agg$stratum_2), ]
        plot(seq_len(nrow(agg)), agg$count_value, type = "l",
             xlab = "Time", ylab = "Records", main = "Temporal Trend",
             xaxt = "n", col = "#2c3e50", lwd = 2)
        if (nrow(agg) > 0) {
          at_idx <- seq(1, nrow(agg), length.out = min(10, nrow(agg)))
          axis(1, at = at_idx, labels = agg$stratum_2[at_idx],
               las = 2, cex.axis = 0.7)
        }
      })
    })

    output$domain_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return(NULL)
      df <- data$concepts
      df$count_value <- as.numeric(df$count_value)
      df <- df[!is.na(df$count_value), , drop = FALSE]
      if (!is.null(data$concept_names)) {
        nm <- data$concept_names
        df$concept_name <- vapply(df$stratum_1, function(id) {
          m <- nm$concept_name[nm$concept_id == as.integer(id)]
          if (length(m) > 0) m[1] else ""
        }, character(1))
      }
      display <- data.frame(
        concept_id = df$stratum_1,
        concept_name = if ("concept_name" %in% names(df)) df$concept_name else "",
        count = df$count_value,
        stringsAsFactors = FALSE
      )
      DT::datatable(display, selection = "multiple", rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp"))
    })

    # Domain extract/filter buttons
    shiny::observeEvent(input$domain_extract_btn, {
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return()
      sel <- input$domain_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification("Select rows first", type = "warning")
        return()
      }
      df <- data$concepts
      df <- df[!is.na(df$count_value), , drop = FALSE]
      tbl <- data$table_name %||% "condition_occurrence"
      for (i in sel) {
        cid <- as.integer(df$stratum_1[i])
        cname <- if (!is.null(data$concept_names)) {
          nm <- data$concept_names
          m <- nm$concept_name[nm$concept_id == cid]
          if (length(m) > 0) m[1] else paste0("concept_", cid)
        } else paste0("concept_", cid)
        v <- omop_variable(table = tbl, concept_id = cid,
                           concept_name = cname, format = "raw")
        state$cart <- cart_add_variable(state$cart, v)
      }
      shiny::showNotification(paste(length(sel), "variable(s) added to cart"),
                              type = "message")
    })

    shiny::observeEvent(input$domain_filter_btn, {
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return()
      sel <- input$domain_table_rows_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification("Select rows first", type = "warning")
        return()
      }
      df <- data$concepts
      df <- df[!is.na(df$count_value), , drop = FALSE]
      cids <- as.integer(df$stratum_1[sel])
      f <- omop_filter_has_concept(concept_ids = cids)
      state$cart <- cart_add_filter(state$cart, f)
      shiny::showNotification("Filter added to cart", type = "message")
    })

    # Visits plot
    output$visits_type_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$types)) return()
      .safe_plot({
        df <- data$types
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        labels <- df$stratum_1
        labels[labels == "9201"] <- "Inpatient"
        labels[labels == "9202"] <- "Outpatient"
        barplot(df$count_value, names.arg = labels,
                col = c("#3498db", "#2ecc71", "#e74c3c", "#f39c12"),
                main = "Visit Types", ylab = "Visits")
      })
    })

    output$visits_trend_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || is.null(data$trends)) return()
      .safe_plot({
        df <- data$trends
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return()
        agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
        agg <- agg[order(agg$stratum_2), ]
        plot(seq_len(nrow(agg)), agg$count_value, type = "l",
             xlab = "Time", ylab = "Visits", main = "Visit Trend",
             xaxt = "n", col = "#2c3e50", lwd = 2)
        if (nrow(agg) > 0) {
          at_idx <- seq(1, nrow(agg), length.out = min(10, nrow(agg)))
          axis(1, at = at_idx, labels = agg$stratum_2[at_idx],
               las = 2, cex.axis = 0.7)
        }
      })
    })

    # Trends plot
    output$trends_overlay_plot <- shiny::renderPlot({
      data <- page_data()
      if (is.null(data) || length(data) == 0) return()
      .safe_plot({
        domains <- names(data)
        colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6")
        first <- TRUE
        legend_labels <- character(0)
        legend_colors <- character(0)
        for (i in seq_along(domains)) {
          df <- data[[domains[i]]]
          if (!is.data.frame(df) || nrow(df) == 0 ||
              !"stratum_2" %in% names(df)) next
          df$count_value <- as.numeric(df$count_value)
          df <- df[!is.na(df$count_value), , drop = FALSE]
          if (nrow(df) == 0) next
          agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
          agg <- agg[order(agg$stratum_2), ]
          if (first) {
            plot(seq_len(nrow(agg)), agg$count_value, type = "l",
                 xlab = "Time", ylab = "Records",
                 main = "Cross-Domain Trends",
                 xaxt = "n", col = colors[i], lwd = 2,
                 ylim = c(0, max(agg$count_value) * 1.2))
            if (nrow(agg) > 0) {
              at_idx <- seq(1, nrow(agg), length.out = min(8, nrow(agg)))
              axis(1, at = at_idx, labels = agg$stratum_2[at_idx],
                   las = 2, cex.axis = 0.7)
            }
            first <- FALSE
          } else {
            lines(seq_len(nrow(agg)), agg$count_value,
                  col = colors[i], lwd = 2)
          }
          legend_labels <- c(legend_labels, domains[i])
          legend_colors <- c(legend_colors, colors[i])
        }
        if (length(legend_labels) > 0) {
          legend("topright", legend = legend_labels, col = legend_colors,
                 lwd = 2, bty = "n", cex = 0.8)
        } else {
          plot.new()
          text(0.5, 0.5, "No trend data available", cex = 1.1, col = "#7f8c8d")
        }
      })
    })

    # Data Quality table
    output$quality_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$warnings)) return(NULL)
      DT::datatable(data$warnings, rownames = FALSE,
                    options = list(pageLength = 20, dom = "ftp"))
    })
  })
}

# ==============================================================================
# Data fetching helpers (each returns a list consumed by render functions)
# ==============================================================================

.atlas_fetch_dashboard <- function(state, scope, policy) {
  # Total persons (analysis 0)
  persons_res <- ds.omop.achilles.results(
    analysis_ids = 0L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, persons_res)

  # Gender (analysis 1)
  gender_res <- ds.omop.achilles.results(
    analysis_ids = 1L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, gender_res)

  # Age distribution (analysis 113)
  age_res <- ds.omop.achilles.distribution(
    analysis_ids = 113L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, age_res)

  srv <- names(persons_res$per_site)[1]
  persons_df <- .atlas_pick_result(persons_res, scope, srv)
  gender_df <- .atlas_pick_result(gender_res, scope, srv)
  age_df <- .atlas_pick_result(age_res, scope, srv)

  total <- if (is.data.frame(persons_df) && nrow(persons_df) > 0) {
    persons_df$count_value[1]
  } else NA

  list(total_persons = total, gender = gender_df, age_dist = age_df)
}

.atlas_fetch_person <- function(state, scope, policy) {
  ids <- c(1L, 2L, 5L, 8L)
  res <- ds.omop.achilles.results(
    analysis_ids = ids, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, res)

  dist_res <- ds.omop.achilles.distribution(
    analysis_ids = 113L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, dist_res)

  srv <- names(res$per_site)[1]
  df <- .atlas_pick_result(res, scope, srv)
  dist_df <- .atlas_pick_result(dist_res, scope, srv)

  if (!is.data.frame(df)) df <- data.frame()

  list(
    gender = df[df$analysis_id == 1L, , drop = FALSE],
    yob = df[df$analysis_id == 2L, , drop = FALSE],
    ethnicity = df[df$analysis_id == 5L, , drop = FALSE],
    race = df[df$analysis_id == 8L, , drop = FALSE],
    age_dist = if (is.data.frame(dist_df)) dist_df else data.frame()
  )
}

.atlas_fetch_domain <- function(state, count_analysis, trend_analysis,
                                 scope, policy) {
  ids <- count_analysis
  res <- ds.omop.achilles.results(
    analysis_ids = ids, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, res)

  srv <- names(res$per_site)[1]
  concepts_df <- .atlas_pick_result(res, scope, srv)

  # Resolve concept names
  concept_names <- NULL
  if (is.data.frame(concepts_df) && nrow(concepts_df) > 0) {
    cids <- as.integer(concepts_df$stratum_1[!is.na(concepts_df$stratum_1)])
    if (length(cids) > 0) {
      tryCatch({
        name_res <- ds.omop.concept.lookup(cids, symbol = state$symbol)
        name_srv <- names(name_res$per_site)[1]
        concept_names <- name_res$per_site[[name_srv]]
      }, error = function(e) NULL)
    }
  }

  trends_df <- NULL
  if (!is.null(trend_analysis)) {
    trend_res <- ds.omop.achilles.results(
      analysis_ids = trend_analysis, scope = scope, pooling_policy = policy,
      symbol = state$symbol)
    .atlas_accumulate_code(state, trend_res)
    trends_df <- .atlas_pick_result(trend_res, scope, srv)
  }

  # Determine table name from analysis_id
  table_name <- switch(as.character(count_analysis),
    "400" = "condition_occurrence",
    "700" = "drug_exposure",
    "600" = "procedure_occurrence",
    "1800" = "measurement",
    "800" = "observation",
    "condition_occurrence"
  )

  list(concepts = concepts_df, trends = trends_df,
       concept_names = concept_names, table_name = table_name)
}

.atlas_fetch_visits <- function(state, scope, policy) {
  type_res <- ds.omop.achilles.results(
    analysis_ids = 200L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, type_res)

  trend_res <- ds.omop.achilles.results(
    analysis_ids = 201L, scope = scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, trend_res)

  srv <- names(type_res$per_site)[1]
  list(
    types = .atlas_pick_result(type_res, scope, srv),
    trends = .atlas_pick_result(trend_res, scope, srv)
  )
}

.atlas_fetch_trends <- function(state, scope, policy) {
  trend_ids <- c(401L, 701L, 601L, 1801L)
  result <- list()
  domain_names <- c("Conditions", "Drugs", "Procedures", "Measurements")

  for (i in seq_along(trend_ids)) {
    tryCatch({
      res <- ds.omop.achilles.results(
        analysis_ids = trend_ids[i], scope = scope, pooling_policy = policy,
        symbol = state$symbol)
      .atlas_accumulate_code(state, res)
      srv <- names(res$per_site)[1]
      result[[domain_names[i]]] <- .atlas_pick_result(res, scope, srv)
    }, error = function(e) NULL)
  }

  result
}

.atlas_fetch_quality <- function(state, scope, policy) {
  # Heel results are retrieved via a custom status call — reuse server result
  tryCatch({
    status <- ds.omop.achilles.status(symbol = state$symbol)
    srv <- names(status$per_site)[1]

    # Get heel warnings via the results endpoint (analysis_id stored in heel table)
    # Use direct achilles results query for heel_results
    # Since heel results aren't in achilles_results, we query them as a special call
    # For now, return status info and any available heel data
    list(
      warnings = data.frame(
        analysis_id = integer(0),
        warning = character(0),
        rule_id = integer(0),
        record_count = numeric(0),
        stringsAsFactors = FALSE
      ),
      status = status$per_site[[srv]]
    )
  }, error = function(e) {
    list(warnings = data.frame(), status = NULL, error = conditionMessage(e))
  })
}

# ==============================================================================
# Dynamic fetch helpers (catalog-driven)
# ==============================================================================

.page_to_domain <- function(page_name) {
  map <- c(
    "Conditions" = "condition", "Drugs" = "drug",
    "Procedures" = "procedure", "Measurements" = "measurement",
    "Observations" = "observation", "Visits" = "visit",
    "Person" = "person", "Observation Period" = "observation_period"
  )
  map[page_name]
}

.atlas_fetch_domain_dynamic <- function(state, analyses_df, scope, policy) {
  # Find the "count by concept" analysis (stratum_1 = concept_id, no stratum_2)
  count_row <- analyses_df[!is.na(analyses_df$stratum_1_name) &
                            analyses_df$stratum_1_name == "concept_id" &
                            (is.na(analyses_df$stratum_2_name) |
                             analyses_df$stratum_2_name == ""),
                           , drop = FALSE]
  count_id <- if (nrow(count_row) > 0) count_row$analysis_id[1] else NULL

  # Find the "trend by concept+month" analysis
  trend_row <- analyses_df[!is.na(analyses_df$stratum_2_name) &
                            analyses_df$stratum_2_name == "calendar_month",
                           , drop = FALSE]
  trend_id <- if (nrow(trend_row) > 0) trend_row$analysis_id[1] else NULL

  # Delegate to existing fetch logic with dynamically discovered IDs
  if (!is.null(count_id)) {
    .atlas_fetch_domain(state, count_id, trend_id, scope, policy)
  } else {
    NULL
  }
}

.atlas_fetch_trends_dynamic <- function(state, cat, scope, policy) {
  if (is.null(cat)) return(.atlas_fetch_trends(state, scope, policy))

  # Find all trend analyses (stratum_2 = calendar_month)
  trend_rows <- cat[!is.na(cat$stratum_2_name) &
                     cat$stratum_2_name == "calendar_month", , drop = FALSE]

  if (nrow(trend_rows) == 0) return(.atlas_fetch_trends(state, scope, policy))

  result <- list()
  domain_labels <- c(
    condition = "Conditions", drug = "Drugs",
    procedure = "Procedures", measurement = "Measurements",
    visit = "Visits", observation = "Observations"
  )

  for (i in seq_len(nrow(trend_rows))) {
    domain <- trend_rows$domain[i]
    label <- domain_labels[domain]
    if (is.na(label)) label <- domain
    if (label %in% names(result)) next  # one per domain

    tryCatch({
      res <- ds.omop.achilles.results(
        analysis_ids = trend_rows$analysis_id[i], scope = scope,
        pooling_policy = policy, symbol = state$symbol)
      .atlas_accumulate_code(state, res)
      srv <- names(res$per_site)[1]
      result[[label]] <- .atlas_pick_result(res, scope, srv)
    }, error = function(e) NULL)
  }

  result
}

# ==============================================================================
# Render helpers (return shiny::tagList for each page)
# ==============================================================================

.atlas_render_dashboard <- function(ns, data) {
  total_txt <- .fmt_count(data$total_persons)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(4,
        bslib::card(bslib::card_body(
          shiny::div(class = "metric-card",
            shiny::div(class = "value", total_txt),
            shiny::div(class = "label", "Total Persons"))
        ))
      ),
      shiny::column(4,
        bslib::card(bslib::card_body(
          shiny::div(class = "metric-card",
            shiny::div(class = "value", {
              if (is.data.frame(data$age_dist) && nrow(data$age_dist) > 0 &&
                  !is.na(data$age_dist$avg_value[1])) {
                round(data$age_dist$avg_value[1], 1)
              } else "N/A"
            }),
            shiny::div(class = "label", "Avg Age at First Obs"))
        ))
      ),
      shiny::column(4,
        bslib::card(bslib::card_body(
          shiny::div(class = "metric-card",
            shiny::div(class = "value", {
              if (is.data.frame(data$gender) && nrow(data$gender) > 0) {
                nrow(data$gender[!is.na(data$gender$count_value), ])
              } else "N/A"
            }),
            shiny::div(class = "label", "Gender Categories"))
        ))
      )
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput(ns("dashboard_gender_plot"),
                                          height = "300px")),
      shiny::column(6, shiny::plotOutput(ns("dashboard_age_plot"),
                                          height = "300px"))
    )
  )
}

.atlas_render_person <- function(ns, data) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput(ns("person_gender_plot"),
                                          height = "300px")),
      shiny::column(6, shiny::plotOutput(ns("person_yob_plot"),
                                          height = "300px"))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput(ns("person_race_plot"),
                                          height = "250px")),
      shiny::column(6, shiny::plotOutput(ns("person_ethnicity_plot"),
                                          height = "250px"))
    )
  )
}

.atlas_render_domain <- function(ns, data, domain_label) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(12,
        shiny::div(class = "d-flex justify-content-end mb-2",
          shiny::actionButton(ns("domain_extract_btn"),
            shiny::tagList(shiny::icon("plus"), "Extract"),
            class = "btn-sm btn-outline-success me-1"),
          shiny::actionButton(ns("domain_filter_btn"),
            shiny::tagList(shiny::icon("filter"), "Filter"),
            class = "btn-sm btn-outline-warning")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput(ns("domain_bar_plot"),
                                          height = "400px")),
      shiny::column(6,
        if (!is.null(data$trends) && is.data.frame(data$trends) &&
            nrow(data$trends) > 0) {
          shiny::plotOutput(ns("domain_trend_plot"), height = "400px")
        } else {
          shiny::div(class = "text-muted text-center py-5",
                     "No trend data available")
        }
      )
    ),
    shiny::fluidRow(
      shiny::column(12, DT::DTOutput(ns("domain_table")))
    )
  )
}

.atlas_render_visits <- function(ns, data) {
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(6, shiny::plotOutput(ns("visits_type_plot"),
                                          height = "350px")),
      shiny::column(6, shiny::plotOutput(ns("visits_trend_plot"),
                                          height = "350px"))
    )
  )
}

.atlas_render_trends <- function(ns, data) {
  shiny::tagList(
    shiny::plotOutput(ns("trends_overlay_plot"), height = "500px")
  )
}

.atlas_render_quality <- function(ns, data) {
  if (is.null(data$warnings) || nrow(data$warnings) == 0) {
    return(shiny::div(class = "text-muted text-center py-5",
      shiny::icon("check-circle", class = "fa-2x text-success mb-2"),
      shiny::h5("No Achilles Heel warnings"),
      shiny::p("Data quality checks passed or heel results unavailable.")
    ))
  }
  DT::DTOutput(ns("quality_table"))
}

# ==============================================================================
# Internal utilities
# ==============================================================================

.atlas_accumulate_code <- function(state, res) {
  if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
    shiny::isolate({
      state$script_lines <- c(state$script_lines, res$meta$call_code)
    })
  }
}

.atlas_pick_result <- function(res, scope, srv) {
  if (scope == "pooled" && !is.null(res$pooled)) {
    res$pooled
  } else if (length(res$per_site) > 0) {
    res$per_site[[srv %||% names(res$per_site)[1]]]
  } else {
    NULL
  }
}
