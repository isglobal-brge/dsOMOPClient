# ==============================================================================
# MODULE: Atlas Data Sources (Achilles-backed statistics)
# ==============================================================================
# ATLAS-style interactive pages: Dashboard, Conditions, Drugs,
# Procedures, Measurements, Observations, Visits, Death, Trends, Data Quality.
# ==============================================================================

.mod_atlas_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- Achilles status + page selector (horizontal) ---
    shiny::div(class = "d-flex align-items-center gap-3 mb-3 flex-wrap",
      shiny::uiOutput(ns("achilles_status")),
      shiny::div(style = "min-width: 180px;",
        shiny::selectInput(ns("atlas_nav"), NULL,
          choices = c("Dashboard", "Conditions", "Drugs",
                      "Procedures", "Measurements", "Observations",
                      "Visits", "Trends", "Data Quality"),
          selected = "Dashboard")
      )
    ),
    # --- Page content (Dashboard manages own cards; other pages get card wrapper) ---
    shiny::uiOutput(ns("page_content"))
  )
}

.mod_atlas_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    achilles_available <- shiny::reactiveVal(FALSE)
    achilles_per_server <- shiny::reactiveVal(list())
    achilles_catalog <- shiny::reactiveVal(NULL)
    page_data <- shiny::reactiveVal(NULL)

    # Send tab disabled/enabled message to parent session
    .toggle_achilles_tab <- function(disabled) {
      target <- parent_session %||% session
      target$sendCustomMessage("toggleAchillesTab", list(
        disabled = disabled,
        tooltip = if (disabled)
          "Run OHDSI Achilles on your CDM to enable this tab" else ""
      ))
    }

    # Check Achilles availability across ALL servers and fetch catalog
    shiny::observe({
      tryCatch({
        status <- ds.omop.achilles.status(symbol = state$symbol)
        per_srv <- list()
        any_avail <- FALSE
        for (srv in names(status$per_site)) {
          avail <- isTRUE(status$per_site[[srv]]$available)
          per_srv[[srv]] <- avail
          if (avail) any_avail <- TRUE
        }
        achilles_per_server(per_srv)
        achilles_available(any_avail)
        .toggle_achilles_tab(!any_avail)

        if (any_avail) {
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
      }, error = function(e) {
        achilles_available(FALSE)
        .toggle_achilles_tab(TRUE)
      })
    })

    # Dynamic navigation choices from catalog
    shiny::observe({
      cat <- achilles_catalog()
      if (is.null(cat)) return()

      domains <- unique(cat$domain)
      pages <- c("Dashboard")
      domain_page_map <- c(
        condition = "Conditions", drug = "Drugs",
        procedure = "Procedures", measurement = "Measurements",
        observation = "Observations", visit = "Visits",
        death = "Death",
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
                   "Run Achilles on the CDM to enable this tab")
        )
      }
    })

    # Re-fetch when scope or server changes
    shiny::observeEvent(list(state$scope, state$selected_servers), {
      if (!achilles_available() || is.null(input$atlas_nav)) return()
      nav <- input$atlas_nav
      cat <- achilles_catalog()
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"

      tryCatch({
        data <- .atlas_dispatch_fetch(nav, state, cat, scope, policy,
                                       state$selected_servers)
        page_data(data %||% list(no_data = TRUE))
      }, error = function(e) {
        page_data(list(error = .clean_ds_error(e)))
      })
    }, ignoreInit = TRUE)

    # Reactive: fetch data when page changes
    shiny::observeEvent(input$atlas_nav, {
      if (!achilles_available()) {
        page_data(NULL)
        return()
      }

      nav <- input$atlas_nav
      cat <- achilles_catalog()
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      selected_srv <- state$selected_servers

      shiny::withProgress(message = paste("Loading", nav, "..."), value = 0.2, {
      tryCatch({
        data <- .atlas_dispatch_fetch(nav, state, cat, scope, policy, selected_srv)
        shiny::incProgress(0.6)
        page_data(data %||% list(no_data = TRUE))
      }, error = function(e) {
        page_data(list(error = .clean_ds_error(e)))
      })
      })
    })

    # Render page content
    output$page_content <- shiny::renderUI({
      if (!achilles_available()) {
        return(.empty_state_ui("chart-bar", "Achilles Statistics Not Available",
          "No connected servers have pre-computed Achilles statistics. Run OHDSI Achilles on your CDM to enable this tab."))
      }

      # Partial data warning banner
      per_srv <- achilles_per_server()
      missing_srvs <- names(per_srv)[!vapply(per_srv, isTRUE, logical(1))]
      warn_banner <- NULL
      if (length(missing_srvs) > 0 && length(missing_srvs) < length(per_srv)) {
        warn_banner <- shiny::div(class = "alert alert-warning py-2 mb-3",
          shiny::icon("exclamation-triangle"),
          paste0(" Achilles data missing on: ", paste(missing_srvs, collapse = ", "),
                 ". Results are based on available servers only.")
        )
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

      if (nav == "Dashboard") {
        # Dashboard manages its own card layout
        page_ui <- .atlas_render_dashboard(ns, data)
        shiny::tagList(warn_banner, page_ui)
      } else {
        # All other pages: wrap in a card
        page_ui <- switch(nav,
          "Conditions"   = .atlas_render_domain(ns, data, "Conditions"),
          "Drugs"        = .atlas_render_domain(ns, data, "Drugs"),
          "Procedures"   = .atlas_render_domain(ns, data, "Procedures"),
          "Measurements" = .atlas_render_domain(ns, data, "Measurements"),
          "Observations" = .atlas_render_domain(ns, data, "Observations"),
          "Visits"       = .atlas_render_visits(ns, data),
          "Death"        = .atlas_render_death(ns, data),
          "Trends"       = .atlas_render_trends(ns, data),
          "Data Quality" = .atlas_render_quality(ns, data),
          shiny::div("Unknown page")
        )
        shiny::tagList(
          warn_banner,
          bslib::card(full_screen = TRUE,
            bslib::card_header(paste("Achilles:", nav)),
            bslib::card_body(page_ui)
          )
        )
      }
    })

    # ========================================================================
    # Dashboard plot outputs
    # ========================================================================

    output$dash_gender_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$gender)) return(.plotly_no_data("No sex/gender data available"))
      .safe_plotly({
        df <- data$gender
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Sex data suppressed (small cell counts)", "\U0001F6E1"))
        cmap <- data$concept_map %||% list()
        labels <- .atlas_label_stratum(df$stratum_1, cmap)
        df <- df[order(df$count_value, decreasing = TRUE), ]
        labels <- labels[order(as.numeric(df$count_value), decreasing = TRUE)]
        plotly::plot_ly(x = df$count_value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[c(1, 3, 2, 4)]),
                        hovertemplate = "<b>%{y}</b><br>Persons: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Sex Distribution")
      })
    })

    output$dash_age_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data)) return(.plotly_no_data("No age data available"))
      toggle <- input$dash_age_toggle %||% "Age Bands"

      .safe_plotly({
        if (toggle == "Percentiles") {
          # Percentile bar from analysis 103 dist
          df <- data$age_dist
          if (!is.data.frame(df) || nrow(df) == 0) return(.plotly_no_data("No age distribution data available"))
          row <- df[1, ]
          vals <- c(row$p10_value, row$p25_value,
                    row$median_value, row$p75_value, row$p90_value)
          nms <- c("P10", "P25", "Median", "P75", "P90")
          keep <- !is.na(vals)
          vals <- vals[keep]; nms <- nms[keep]
          if (length(vals) == 0) {
            if (!is.na(row$avg_value)) {
              vals <- row$avg_value; nms <- "Mean"
            } else return(.plotly_no_data("Age percentiles suppressed (pooled data)", "\U0001F6E1"))
          }
          plotly::plot_ly(x = nms, y = vals, type = "bar",
                          marker = list(color = .studio_colors[1])) |>
            plotly::layout(
              xaxis = list(categoryorder = "array", categoryarray = nms),
              yaxis = list(title = "Age (years)")) |>
            .plotly_defaults("Age at First Observation")
        } else {
          # Age pyramid from analysis 102 (gender x age)
          pyramid <- data$age_pyramid
          if (!is.null(pyramid) && nrow(pyramid) > 0) {
            plotly::plot_ly() |>
              plotly::add_trace(
                y = pyramid$band_label, x = -pyramid$male_count,
                type = "bar", orientation = "h", name = "Male",
                marker = list(color = .studio_colors[1]),
                hovertemplate = "<b>%{y}</b><br>Male: %{customdata:,.0f}<extra></extra>",
                customdata = pyramid$male_count) |>
              plotly::add_trace(
                y = pyramid$band_label, x = pyramid$female_count,
                type = "bar", orientation = "h", name = "Female",
                marker = list(color = .studio_colors[3]),
                hovertemplate = "<b>%{y}</b><br>Female: %{x:,.0f}<extra></extra>") |>
              plotly::layout(
                barmode = "overlay",
                xaxis = list(
                  title = "Persons",
                  tickvals = NULL,
                  ticktext = NULL,
                  zeroline = TRUE, zerolinecolor = "#e2e8f0",
                  tickformat = ",d",
                  # Show absolute values on axis
                  tickprefix = ""),
                yaxis = list(
                  title = "",
                  categoryorder = "array",
                  categoryarray = pyramid$band_label),
                legend = list(orientation = "h", y = -0.15, x = 0.5,
                              xanchor = "center")) |>
              .plotly_defaults("Age Distribution")
          } else {
            # Fallback: YOB converted to age bands (no sex split)
            bands <- data$age_bands_fallback
            if (!is.null(bands) && nrow(bands) > 0) {
              plotly::plot_ly(x = bands$count, y = bands$label,
                              type = "bar", orientation = "h",
                              marker = list(color = .studio_colors[1]),
                              hovertemplate = "<b>%{y}</b><br>Persons: %{x:,.0f}<extra></extra>") |>
                plotly::layout(
                  yaxis = list(categoryorder = "array",
                               categoryarray = bands$label)) |>
                .plotly_defaults("Age Distribution (estimated)")
            } else {
              .plotly_no_data("No age distribution data available")
            }
          }
        }
      })
    })

    output$dash_birth_cohort_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$birth_cohorts)) return(.plotly_no_data("Birth cohort data suppressed (small cell counts)", "\U0001F6E1"))
      .safe_plotly({
        df <- data$birth_cohorts
        if (nrow(df) == 0) return(.plotly_no_data("Birth cohort data suppressed (small cell counts)", "\U0001F6E1"))
        plotly::plot_ly(x = df$label, y = df$count, type = "bar",
                        marker = list(color = .studio_colors[5]),
                        hovertemplate = "<b>%{x}</b><br>Persons: %{y:,.0f}<extra></extra>") |>
          plotly::layout(
            xaxis = list(categoryorder = "array", categoryarray = df$label)) |>
          .plotly_defaults("Birth Cohorts")
      })
    })

    output$dash_race_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$race)) return(.plotly_no_data("No race data available"))
      .safe_plotly({
        df <- data$race
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Race data suppressed (small cell counts)", "\U0001F6E1"))
        cmap <- data$concept_map %||% list()
        labels <- .atlas_label_stratum(df$stratum_1, cmap)
        plotly::plot_ly(x = df$count_value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[2]),
                        hovertemplate = "<b>%{y}</b><br>Persons: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Race")
      })
    })

    output$dash_ethnicity_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$ethnicity)) return(.plotly_no_data("No ethnicity data available"))
      .safe_plotly({
        df <- data$ethnicity
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Ethnicity data suppressed (small cell counts)", "\U0001F6E1"))
        cmap <- data$concept_map %||% list()
        labels <- .atlas_label_stratum(df$stratum_1, cmap)
        plotly::plot_ly(x = df$count_value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[4]),
                        hovertemplate = "<b>%{y}</b><br>Persons: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Ethnicity")
      })
    })

    output$dash_obs_year_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$obs_by_year)) return(.plotly_no_data("No observation period data available"))
      .safe_plotly({
        df <- data$obs_by_year
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Observation data suppressed", "\U0001F6E1"))
        df <- df[order(as.character(df$stratum_1)), ]
        plotly::plot_ly(x = as.character(df$stratum_1), y = df$count_value,
                        type = "scatter", mode = "lines+markers",
                        fill = "tozeroy",
                        line = list(color = .studio_colors[1], width = 2),
                        marker = list(color = .studio_colors[1], size = 5),
                        fillcolor = "rgba(37,99,235,0.1)",
                        hovertemplate = "<b>%{x}</b><br>Persons: %{y:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(title = "Persons")) |>
          .plotly_defaults("Persons by Calendar Year")
      })
    })

    output$dash_obs_length_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$obs_length_dist)) return(.plotly_no_data("No observation length data available"))
      .safe_plotly({
        df <- data$obs_length_dist
        if (!is.data.frame(df) || nrow(df) == 0) return(.plotly_no_data("No observation length data available"))
        row <- df[1, ]
        vals <- c(row$p10_value, row$p25_value,
                  row$median_value, row$p75_value, row$p90_value)
        nms <- c("P10", "P25", "Median", "P75", "P90")
        keep <- !is.na(vals)
        vals <- vals[keep]; nms <- nms[keep]
        if (length(vals) == 0) return(.plotly_no_data("Observation length percentiles suppressed (pooled data)", "\U0001F6E1"))
        # Convert days to human-readable hover
        hover <- vapply(vals, function(d) {
          yrs <- floor(d / 365.25)
          mos <- round((d %% 365.25) / 30.44)
          if (yrs > 0) paste0(yrs, "y ", mos, "m") else paste0(round(d), "d")
        }, character(1))
        plotly::plot_ly(x = nms, y = vals, type = "bar",
                        marker = list(color = .studio_colors[5]),
                        text = hover, textposition = "outside",
                        hovertemplate = "<b>%{x}</b><br>%{text} (%{y:,.0f} days)<extra></extra>") |>
          plotly::layout(
            xaxis = list(categoryorder = "array", categoryarray = nms),
            yaxis = list(title = "Days")) |>
          .plotly_defaults("Observation Period Length")
      })
    })

    output$dash_coverage_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$domain_coverage)) return(.plotly_no_data("No domain coverage data available"))
      .safe_plotly({
        df <- data$domain_coverage
        if (!is.data.frame(df) || nrow(df) == 0) return(.plotly_no_data("No domain coverage data available"))
        total <- as.numeric(data$total_persons)
        if (is.na(total) || total == 0) return(.plotly_no_data("Total persons unknown — cannot compute coverage"))
        df$n_persons <- as.numeric(df$n_persons)
        df <- df[!is.na(df$n_persons), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Coverage data suppressed", "\U0001F6E1"))
        df$pct <- round(df$n_persons / total * 100, 1)
        df <- df[order(df$pct, decreasing = TRUE), ]
        labels <- .format_table_name(df$table_name)
        plotly::plot_ly(x = df$pct, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[6]),
                        text = paste0(df$pct, "%"), textposition = "auto",
                        hovertemplate = "<b>%{y}</b><br>%{x:.1f}% of persons<extra></extra>") |>
          plotly::layout(
            xaxis = list(title = "% of Persons", range = c(0, 105)),
            yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Domain Coverage")
      })
    })

    # ========================================================================
    # Domain plots (Conditions, Drugs, Procedures, Measurements, Observations)
    # ========================================================================

    output$domain_bar_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return(.plotly_no_data("No concept data available for this domain"))
      .safe_plotly({
        df <- data$concepts
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Concept data suppressed (small cell counts)", "\U0001F6E1"))
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
        labels <- substr(labels, 1, 30)
        plotly::plot_ly(x = df$count_value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[1])) |>
          plotly::layout(title = list(text = paste("Top", n, "Concepts")),
                         xaxis = list(title = "Persons"),
                         yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults(paste("Top", n, "Concepts"))
      })
    })

    output$domain_trend_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$trends)) return(.plotly_no_data("No temporal trend data available"))
      .safe_plotly({
        df <- data$trends
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Trend data suppressed (small cell counts)", "\U0001F6E1"))
        agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
        agg <- agg[order(agg$stratum_2), ]
        plotly::plot_ly(x = agg$stratum_2, y = agg$count_value,
                        type = "scatter", mode = "lines",
                        line = list(color = .studio_colors[1], width = 2)) |>
          plotly::layout(title = list(text = "Temporal Trend"),
                         xaxis = list(title = "Time", tickangle = -45),
                         yaxis = list(title = "Persons")) |>
          .plotly_defaults("Temporal Trend")
      })
    })

    output$domain_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return(NULL)
      df <- data$concepts
      df$count_value <- as.numeric(df$count_value)
      df <- df[!is.na(df$count_value), , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      if (!is.null(data$concept_names)) {
        nm <- data$concept_names
        df$concept_name <- vapply(df$stratum_1, function(id) {
          m <- nm$concept_name[nm$concept_id == as.integer(id)]
          if (length(m) > 0) m[1] else ""
        }, character(1))
      }
      chk <- vapply(seq_len(nrow(df)), function(i) {
        paste0('<input type="checkbox" class="dt-row-chk" data-row="', i, '">')
      }, character(1))
      display <- data.frame(
        sel = chk,
        concept_id = df$stratum_1,
        concept_name = if ("concept_name" %in% names(df)) df$concept_name else "",
        count = df$count_value,
        stringsAsFactors = FALSE
      )
      DT::datatable(display, selection = "none", rownames = FALSE,
                    escape = FALSE,
                    options = list(pageLength = 15, dom = "ftp",
                      columnDefs = list(
                        list(className = "dt-center", targets = 0,
                             orderable = FALSE, width = "30px")
                      )),
                    callback = DT::JS(paste0(
                      "var sel = [];",
                      "table.on('change', '.dt-row-chk', function() {",
                      "  var row = parseInt($(this).data('row'));",
                      "  if (this.checked) { if (sel.indexOf(row)===-1) sel.push(row); }",
                      "  else { sel = sel.filter(function(r){return r!==row;}); }",
                      "  Shiny.setInputValue('", ns("domain_chk_selected"), "', sel, {priority:'event'});",
                      "});",
                      "var hdr = $('<input type=\"checkbox\" class=\"dt-chk-all\">');",
                      "$(table.column(0).header()).empty().append(hdr);",
                      "hdr.on('change', function() {",
                      "  var checked = this.checked;",
                      "  sel = [];",
                      "  table.rows({search:'applied'}).every(function() {",
                      "    var cb = $(this.node()).find('.dt-row-chk');",
                      "    cb.prop('checked', checked);",
                      "    if (checked) sel.push(parseInt(cb.data('row')));",
                      "  });",
                      "  Shiny.setInputValue('", ns("domain_chk_selected"), "', sel, {priority:'event'});",
                      "});"
                    )))
    })

    # Domain extract/filter buttons
    shiny::observeEvent(input$domain_extract_btn, {
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return()
      sel <- input$domain_chk_selected
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
      shiny::showNotification(paste(length(sel), "variable(s) added to Builder"),
                              type = "message")
    })

    shiny::observeEvent(input$domain_filter_btn, {
      data <- page_data()
      if (is.null(data) || is.null(data$concepts)) return()
      sel <- input$domain_chk_selected
      if (is.null(sel) || length(sel) == 0) {
        shiny::showNotification("Select rows first", type = "warning")
        return()
      }
      df <- data$concepts
      df <- df[!is.na(df$count_value), , drop = FALSE]
      cids <- as.integer(df$stratum_1[sel])
      nav <- input$atlas_nav
      tbl <- switch(tolower(nav %||% ""),
        condition = "condition_occurrence",
        drug = "drug_exposure",
        measurement = "measurement",
        procedure = "procedure_occurrence",
        observation = "observation",
        visit = "visit_occurrence",
        "condition_occurrence"
      )
      added <- 0L
      for (cid in cids) {
        cname <- data$concept_map[[as.character(cid)]]
        tryCatch({
          f <- omop_filter_has_concept(cid, tbl, cname)
          state$cart <- cart_add_filter(state$cart, f)
          added <- added + 1L
        }, error = function(e) NULL)
      }
      if (added > 0) {
        shiny::showNotification(
          paste(added, "filter(s) added to Builder"),
          type = "message", duration = 2)
      }
    })

    # ========================================================================
    # Visits plot outputs
    # ========================================================================

    output$visits_type_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$types)) return(.plotly_no_data("No visit type data available"))
      .safe_plotly({
        df <- data$types
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Visit type data suppressed", "\U0001F6E1"))
        cmap <- data$concept_map %||% list()
        labels <- .atlas_label_stratum(df$stratum_1, cmap)
        plotly::plot_ly(x = labels, y = df$count_value, type = "bar",
                        marker = list(color = .studio_colors[1:4])) |>
          plotly::layout(title = list(text = "Visit Types"),
                         yaxis = list(title = "Persons")) |>
          .plotly_defaults("Visit Types")
      })
    })

    output$visits_trend_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$trends)) return(.plotly_no_data("No visit trend data available"))
      .safe_plotly({
        df <- data$trends
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Visit trend data suppressed", "\U0001F6E1"))
        # Analysis 220: stratum_1 = YYYYMM
        if ("stratum_1" %in% names(df) &&
            (!"stratum_2" %in% names(df) || all(is.na(df$stratum_2)))) {
          agg <- stats::aggregate(count_value ~ stratum_1, data = df, FUN = sum)
          agg <- agg[order(agg$stratum_1), ]
          plotly::plot_ly(x = agg$stratum_1, y = agg$count_value,
                          type = "scatter", mode = "lines",
                          line = list(color = .studio_colors[1], width = 2)) |>
            plotly::layout(title = list(text = "Visit Records Over Time"),
                           xaxis = list(title = "Time", tickangle = -45),
                           yaxis = list(title = "Visit Records")) |>
            .plotly_defaults("Visit Records Over Time")
        } else {
          agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
          agg <- agg[order(agg$stratum_2), ]
          plotly::plot_ly(x = agg$stratum_2, y = agg$count_value,
                          type = "scatter", mode = "lines",
                          line = list(color = .studio_colors[1], width = 2)) |>
            plotly::layout(title = list(text = "Visit Trend"),
                           xaxis = list(title = "Time", tickangle = -45),
                           yaxis = list(title = "Visits")) |>
            .plotly_defaults("Visit Trend")
        }
      })
    })

    # ========================================================================
    # Death plot output
    # ========================================================================

    output$death_cause_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$causes)) return(.plotly_no_data("No cause-of-death data available"))
      .safe_plotly({
        df <- data$causes
        df$count_value <- as.numeric(df$count_value)
        df <- df[!is.na(df$count_value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Death data suppressed (small cell counts)", "\U0001F6E1"))
        df <- df[order(df$count_value, decreasing = TRUE), ]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), , drop = FALSE]
        labels <- if (!is.null(data$concept_names)) {
          nm <- data$concept_names
          vapply(df$stratum_1, function(id) {
            m <- nm$concept_name[nm$concept_id == as.integer(id)]
            if (length(m) > 0) substr(m[1], 1, 40) else id
          }, character(1))
        } else df$stratum_1
        plotly::plot_ly(x = df$count_value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[6]),
                        hovertemplate = "<b>%{y}</b><br>Deaths: %{x:,.0f}<extra></extra>") |>
          plotly::layout(
            xaxis = list(title = "Deaths"),
            yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults(paste("Top", n, "Causes of Death"))
      })
    })

    # ========================================================================
    # Trends overlay plot
    # ========================================================================

    output$trends_overlay_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || length(data) == 0) return(.plotly_no_data("No trend data available"))
      .safe_plotly({
        domains <- names(data)
        colors <- .studio_colors
        p <- plotly::plot_ly()
        trace_added <- FALSE
        for (i in seq_along(domains)) {
          df <- data[[domains[i]]]
          if (!is.data.frame(df) || nrow(df) == 0 ||
              !"stratum_2" %in% names(df)) next
          df$count_value <- as.numeric(df$count_value)
          df <- df[!is.na(df$count_value), , drop = FALSE]
          if (nrow(df) == 0) next
          agg <- stats::aggregate(count_value ~ stratum_2, data = df, FUN = sum)
          agg <- agg[order(agg$stratum_2), ]
          col_idx <- ((i - 1) %% length(colors)) + 1
          p <- p |> plotly::add_trace(
            x = agg$stratum_2, y = agg$count_value,
            type = "scatter", mode = "lines",
            name = domains[i],
            line = list(color = colors[col_idx], width = 2)
          )
          trace_added <- TRUE
        }
        if (!trace_added) {
          p <- .plotly_no_data("No trend data available for any domain")
        } else {
          p <- p |> plotly::layout(
            title = list(text = "Cross-Domain Trends"),
            xaxis = list(title = "Time", tickangle = -45),
            yaxis = list(title = "Persons"),
            legend = list(orientation = "h", y = -0.2)
          ) |> .plotly_defaults("Cross-Domain Trends")
        }
        p
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
# Fetch dispatcher (centralizes nav -> fetch mapping)
# ==============================================================================

.atlas_dispatch_fetch <- function(nav, state, cat, scope, policy, selected_srv) {
  if (nav == "Dashboard") {
    .atlas_fetch_dashboard(state, scope, policy, selected_srv)
  } else if (nav == "Visits") {
    .atlas_fetch_visits(state, scope, policy, selected_srv)
  } else if (nav == "Death") {
    .atlas_fetch_death(state, scope, policy, selected_srv)
  } else if (nav == "Trends") {
    .atlas_fetch_trends_dynamic(state, cat, scope, policy, selected_srv)
  } else if (nav == "Data Quality") {
    .atlas_fetch_quality(state, scope, policy)
  } else {
    # Generic domain page — try dynamic catalog first, then hardcoded fallback
    domain <- .page_to_domain(nav)

    # Hardcoded domain config (standard OHDSI Achilles IDs)
    domain_config <- list(
      "Conditions"   = list(count = 400L, trend = 402L),
      "Drugs"        = list(count = 700L, trend = 702L),
      "Procedures"   = list(count = 600L, trend = 602L),
      "Measurements" = list(count = 1800L, trend = 1802L),
      "Observations" = list(count = 800L, trend = NULL)
    )
    config <- domain_config[[nav]]

    result <- NULL
    if (!is.null(cat) && !is.null(domain) && !is.na(domain)) {
      domain_analyses <- cat[cat$domain == domain, , drop = FALSE]
      if (nrow(domain_analyses) > 0) {
        result <- .atlas_fetch_domain_dynamic(state, domain_analyses, scope,
                                               policy, selected_srv)
      }
    }
    if (is.null(result) && !is.null(config)) {
      # Full fallback to hardcoded IDs
      result <- .atlas_fetch_domain(state, config$count, config$trend, scope,
                                     policy, selected_srv)
    } else if (!is.null(result) && is.null(result$trends) &&
               !is.null(config) && !is.null(config$trend)) {
      # Dynamic found concepts but missed trends — supplement with hardcoded
      tryCatch({
        api_scope <- .backend_scope(scope)
        trend_res <- ds.omop.achilles.results(
          analysis_ids = config$trend, scope = api_scope,
          pooling_policy = policy, symbol = state$symbol)
        .atlas_accumulate_code(state, trend_res)
        srv <- selected_srv %||% names(trend_res$per_site)[1]
        result$trends <- .atlas_pick_result(trend_res, scope, srv)
      }, error = function(e) NULL)
    }
    result
  }
}

# ==============================================================================
# Utility functions (age bands, birth cohorts, age pyramid)
# ==============================================================================

# Compute age bands from analysis 3 (YOB) data with adaptive binning
# Returns data.frame(label, count) sorted by band
.atlas_compute_age_bands <- function(yob_df, current_year = NULL, initial_width = 5L) {
  if (!is.data.frame(yob_df) || nrow(yob_df) == 0) return(NULL)

  if (is.null(current_year)) current_year <- as.integer(format(Sys.Date(), "%Y"))

  df <- yob_df
  df$count_value <- suppressWarnings(as.numeric(df$count_value))
  df$yob <- suppressWarnings(as.integer(df$stratum_1))
  df <- df[!is.na(df$count_value) & !is.na(df$yob) & df$count_value > 0, ,
           drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  # Convert YOB to approximate current age
  df$age <- current_year - df$yob

  .bin_ages <- function(ages, counts, width) {
    max_age <- max(ages, na.rm = TRUE)
    breaks <- seq(0, max_age + width, by = width)
    bins <- cut(ages, breaks = breaks, right = FALSE, include.lowest = TRUE)
    agg <- stats::aggregate(counts, by = list(band = bins), FUN = sum)
    names(agg) <- c("band", "count")
    agg <- agg[!is.na(agg$band), , drop = FALSE]
    agg$label <- as.character(agg$band)
    agg
  }

  result <- .bin_ages(df$age, df$count_value, initial_width)

  # Adaptive: if too many sparse bins, widen
  if (nrow(result) > 20 && initial_width < 10L) {
    result <- .bin_ages(df$age, df$count_value, 10L)
  }
  if (nrow(result) > 15 && initial_width < 20L) {
    result <- .bin_ages(df$age, df$count_value, 20L)
  }

  result[order(result$band), , drop = FALSE]
}

# Compute birth cohorts from analysis 3 (YOB) data
# Bins into decades, top-codes <= 1920
# Returns data.frame(decade, label, count)
.atlas_compute_birth_cohorts <- function(yob_df) {
  if (!is.data.frame(yob_df) || nrow(yob_df) == 0) return(NULL)

  df <- yob_df
  df$count_value <- suppressWarnings(as.numeric(df$count_value))
  df$yob <- suppressWarnings(as.integer(df$stratum_1))
  df <- df[!is.na(df$count_value) & !is.na(df$yob) & df$count_value > 0, ,
           drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  # Top-code <= 1920
  df$decade <- floor(df$yob / 10) * 10
  df$decade[df$decade <= 1920] <- 1920L

  agg <- stats::aggregate(count_value ~ decade, data = df, FUN = sum)
  agg <- agg[order(agg$decade), ]
  agg$label <- ifelse(agg$decade == 1920, "\u22641920s",
                       paste0(agg$decade, "s"))
  names(agg)[names(agg) == "count_value"] <- "count"
  agg
}

# Compute age pyramid from analysis 102 (gender x age) data
# Returns data.frame(band_label, male_count, female_count) or NULL
.atlas_compute_age_pyramid <- function(df_102, concept_map = list(),
                                        width = 5L) {
  if (!is.data.frame(df_102) || nrow(df_102) == 0) return(NULL)

  df <- df_102
  df$count_value <- suppressWarnings(as.numeric(df$count_value))
  df$age <- suppressWarnings(as.integer(df$stratum_2))
  df <- df[!is.na(df$count_value) & !is.na(df$age), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)

  # Determine gender labels from concept_map
  genders <- unique(as.character(df$stratum_1))
  gender_labels <- vapply(genders, function(g) {
    nm <- concept_map[[g]]
    if (!is.null(nm) && !is.na(nm)) tolower(nm) else g
  }, character(1))

  # Identify male and female concept IDs
  male_id <- genders[grepl("\\bmale\\b", gender_labels, ignore.case = TRUE) &
                      !grepl("female", gender_labels, ignore.case = TRUE)]
  female_id <- genders[grepl("female", gender_labels, ignore.case = TRUE)]

  # Fallback: standard OMOP concept IDs (8507=MALE, 8532=FEMALE)
  if (length(male_id) == 0 && "8507" %in% genders) male_id <- "8507"
  if (length(female_id) == 0 && "8532" %in% genders) female_id <- "8532"

  if (length(male_id) == 0 || length(female_id) == 0) return(NULL)
  male_id <- male_id[1]
  female_id <- female_id[1]

  # Bin ages
  max_age <- max(df$age, na.rm = TRUE)
  breaks <- seq(0, max_age + width, by = width)
  df$band <- cut(df$age, breaks = breaks, right = FALSE, include.lowest = TRUE)

  agg <- stats::aggregate(count_value ~ band + stratum_1, data = df, FUN = sum)
  n_bands <- length(unique(agg$band[!is.na(agg$band)]))

  # Adaptive: try wider bins if too many
  if (n_bands > 20 && width < 10L) {
    return(.atlas_compute_age_pyramid(df_102, concept_map, 10L))
  }
  if (n_bands > 15 && width < 20L) {
    return(.atlas_compute_age_pyramid(df_102, concept_map, 20L))
  }

  all_bands <- sort(unique(agg$band[!is.na(agg$band)]))
  male_agg <- agg[agg$stratum_1 == male_id, , drop = FALSE]
  female_agg <- agg[agg$stratum_1 == female_id, , drop = FALSE]

  result <- data.frame(
    band_label = as.character(all_bands),
    male_count = 0,
    female_count = 0,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(result))) {
    b <- all_bands[i]
    m <- male_agg$count_value[male_agg$band == b]
    f <- female_agg$count_value[female_agg$band == b]
    if (length(m) > 0) result$male_count[i] <- sum(m)
    if (length(f) > 0) result$female_count[i] <- sum(f)
  }

  result
}

# ==============================================================================
# Data fetching helpers
# ==============================================================================

.atlas_fetch_dashboard <- function(state, scope, policy,
                                    selected_server = NULL) {
  api_scope <- .backend_scope(scope)

  # Batch 1: count analyses (1=total, 2=gender, 3=yob, 4=race, 5=ethnicity,
  #          102=gender*age, 109=obs by year)
  count_res <- ds.omop.achilles.results(
    analysis_ids = c(1L, 2L, 3L, 4L, 5L, 102L, 109L),
    scope = api_scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, count_res)

  # Batch 2: distributions (103=age at first obs, 105=obs period length)
  # Always request per_site so we can fall back when pooled percentiles are NA
  dist_res <- ds.omop.achilles.distribution(
    analysis_ids = c(103L, 105L), scope = api_scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, dist_res)

  # Batch 3: domain coverage (optional)
  domain_coverage <- tryCatch({
    cov_res <- ds.omop.domain.coverage(
      scope = if (api_scope == "pooled") "pooled" else "per_site",
      pooling_policy = policy, symbol = state$symbol)
    .atlas_accumulate_code(state, cov_res)
    srv <- selected_server %||% names(cov_res$per_site)[1]
    .atlas_pick_result(cov_res, scope, srv)
  }, error = function(e) NULL)

  srv <- selected_server %||% names(count_res$per_site)[1]
  count_df <- .atlas_pick_result(count_res, scope, srv)
  dist_df <- .atlas_pick_result(dist_res, scope, srv)

  # Fix: when pooled, percentiles are set to NA by pooling logic.
  # Fall back to first server's per-site data for distributions (already
  # disclosure-controlled by server).
  if (scope == "pooled" && is.data.frame(dist_df) && nrow(dist_df) > 0) {
    all_pct_na <- all(is.na(dist_df$median_value)) &&
                  all(is.na(dist_df$p10_value))
    if (all_pct_na && length(dist_res$per_site) > 0) {
      fallback_srv <- names(dist_res$per_site)[1]
      dist_df_fb <- dist_res$per_site[[fallback_srv]]
      if (is.data.frame(dist_df_fb) && nrow(dist_df_fb) > 0 &&
          !all(is.na(dist_df_fb$median_value))) {
        dist_df <- dist_df_fb
      }
    }
  }

  if (!is.data.frame(count_df) || nrow(count_df) == 0) {
    count_df <- data.frame(
      analysis_id = integer(0), stratum_1 = character(0),
      stratum_2 = character(0), count_value = numeric(0),
      stringsAsFactors = FALSE)
  }
  if (!is.data.frame(dist_df) || nrow(dist_df) == 0) {
    dist_df <- data.frame(
      analysis_id = integer(0), count_value = numeric(0),
      avg_value = numeric(0), stdev_value = numeric(0),
      median_value = numeric(0), p10_value = numeric(0),
      p25_value = numeric(0), p75_value = numeric(0),
      p90_value = numeric(0), stringsAsFactors = FALSE)
  }

  # Split count results by analysis_id
  total_persons <- {
    r <- count_df[count_df$analysis_id == 1L, , drop = FALSE]
    if (nrow(r) > 0) as.numeric(r$count_value[1]) else NA
  }
  gender_df <- count_df[count_df$analysis_id == 2L, , drop = FALSE]
  yob_df <- count_df[count_df$analysis_id == 3L, , drop = FALSE]
  race_df <- count_df[count_df$analysis_id == 4L, , drop = FALSE]
  ethnicity_df <- count_df[count_df$analysis_id == 5L, , drop = FALSE]
  gender_age_df <- count_df[count_df$analysis_id == 102L, , drop = FALSE]
  obs_year_df <- count_df[count_df$analysis_id == 109L, , drop = FALSE]

  age_dist <- dist_df[dist_df$analysis_id == 103L, , drop = FALSE]
  obs_length_dist <- dist_df[dist_df$analysis_id == 105L, , drop = FALSE]

  # Resolve concept IDs for gender, race, ethnicity strata
  all_strata <- c(
    if (nrow(gender_df) > 0) gender_df$stratum_1 else character(0),
    if (nrow(race_df) > 0) race_df$stratum_1 else character(0),
    if (nrow(ethnicity_df) > 0) ethnicity_df$stratum_1 else character(0),
    if (nrow(gender_age_df) > 0) unique(gender_age_df$stratum_1) else character(0)
  )
  concept_map <- .atlas_resolve_concept_ids(all_strata, state)

  # Compute derived data
  age_pyramid <- .atlas_compute_age_pyramid(gender_age_df, concept_map)
  age_bands_fallback <- if (is.null(age_pyramid) && nrow(yob_df) > 0) {
    .atlas_compute_age_bands(yob_df)
  } else NULL
  birth_cohorts <- .atlas_compute_birth_cohorts(yob_df)

  list(
    total_persons = total_persons,
    gender = gender_df,
    yob = yob_df,
    race = race_df,
    ethnicity = ethnicity_df,
    gender_age = gender_age_df,
    obs_by_year = obs_year_df,
    age_dist = age_dist,
    obs_length_dist = obs_length_dist,
    domain_coverage = domain_coverage,
    concept_map = concept_map,
    age_pyramid = age_pyramid,
    age_bands_fallback = age_bands_fallback,
    birth_cohorts = birth_cohorts
  )
}

.atlas_fetch_domain <- function(state, count_analysis, trend_analysis,
                                 scope, policy, selected_server = NULL) {
  api_scope <- .backend_scope(scope)

  # Fetch concept counts + total persons in one call
  ids <- c(1L, count_analysis)
  res <- ds.omop.achilles.results(
    analysis_ids = ids, scope = api_scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, res)

  srv <- selected_server %||% names(res$per_site)[1]
  all_df <- .atlas_pick_result(res, scope, srv)
  if (!is.data.frame(all_df) || nrow(all_df) == 0) {
    all_df <- data.frame(
      analysis_id = integer(0), stratum_1 = character(0),
      stratum_2 = character(0), count_value = numeric(0),
      stringsAsFactors = FALSE)
  }

  total_persons <- {
    r <- all_df[all_df$analysis_id == 1L, , drop = FALSE]
    if (nrow(r) > 0) as.numeric(r$count_value[1]) else NA
  }
  concepts_df <- all_df[all_df$analysis_id == count_analysis, , drop = FALSE]

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
    tryCatch({
      trend_res <- ds.omop.achilles.results(
        analysis_ids = trend_analysis, scope = api_scope, pooling_policy = policy,
        symbol = state$symbol)
      .atlas_accumulate_code(state, trend_res)
      trends_df <- .atlas_pick_result(trend_res, scope, srv)
    }, error = function(e) NULL)
  }

  table_name <- switch(as.character(count_analysis),
    "400" = "condition_occurrence",
    "700" = "drug_exposure",
    "600" = "procedure_occurrence",
    "1800" = "measurement",
    "800" = "observation",
    "condition_occurrence"
  )

  # Compute domain-level KPIs
  n_concepts <- if (is.data.frame(concepts_df)) nrow(concepts_df) else 0
  coverage_pct <- NA
  if (!is.na(total_persons) && total_persons > 0 &&
      is.data.frame(concepts_df) && nrow(concepts_df) > 0) {
    max_persons <- max(as.numeric(concepts_df$count_value), na.rm = TRUE)
    coverage_pct <- round(max_persons / total_persons * 100, 1)
  }

  list(concepts = concepts_df, trends = trends_df,
       concept_names = concept_names, table_name = table_name,
       total_persons = total_persons, n_concepts = n_concepts,
       coverage_pct = coverage_pct)
}

.atlas_fetch_visits <- function(state, scope, policy,
                                 selected_server = NULL) {
  api_scope <- .backend_scope(scope)

  # Fetch visit types (200) and total persons (1)
  type_res <- ds.omop.achilles.results(
    analysis_ids = c(1L, 200L), scope = api_scope, pooling_policy = policy,
    symbol = state$symbol)
  .atlas_accumulate_code(state, type_res)

  # Visit records by month (220) for overall trend
  trend_res <- tryCatch({
    r <- ds.omop.achilles.results(
      analysis_ids = 220L, scope = api_scope, pooling_policy = policy,
      symbol = state$symbol)
    .atlas_accumulate_code(state, r)
    r
  }, error = function(e) NULL)

  srv <- selected_server %||% names(type_res$per_site)[1]
  all_df <- .atlas_pick_result(type_res, scope, srv)
  if (!is.data.frame(all_df) || nrow(all_df) == 0) {
    all_df <- data.frame(
      analysis_id = integer(0), stratum_1 = character(0),
      stratum_2 = character(0), count_value = numeric(0),
      stringsAsFactors = FALSE)
  }

  total_persons <- {
    r <- all_df[all_df$analysis_id == 1L, , drop = FALSE]
    if (nrow(r) > 0) as.numeric(r$count_value[1]) else NA
  }
  types_df <- all_df[all_df$analysis_id == 200L, , drop = FALSE]

  # Resolve visit type concept IDs
  concept_map <- list()
  if (is.data.frame(types_df) && nrow(types_df) > 0) {
    concept_map <- .atlas_resolve_concept_ids(types_df$stratum_1, state)
  }

  trends_df <- if (!is.null(trend_res)) {
    .atlas_pick_result(trend_res, scope,
      selected_server %||% names(trend_res$per_site)[1])
  } else NULL

  # KPIs
  n_visit_types <- if (is.data.frame(types_df)) nrow(types_df) else 0
  total_visits <- if (is.data.frame(types_df) && nrow(types_df) > 0) {
    sum(as.numeric(types_df$count_value), na.rm = TRUE)
  } else NA

  list(
    types = types_df,
    trends = trends_df,
    concept_map = concept_map,
    total_persons = total_persons,
    n_visit_types = n_visit_types,
    total_visits = total_visits
  )
}

.atlas_fetch_death <- function(state, scope, policy,
                                selected_server = NULL) {
  api_scope <- .backend_scope(scope)

  # Analysis 500: Death by cause (stratum_1 = cause_concept_id)
  res <- tryCatch({
    r <- ds.omop.achilles.results(
      analysis_ids = 500L, scope = api_scope, pooling_policy = policy,
      symbol = state$symbol)
    .atlas_accumulate_code(state, r)
    r
  }, error = function(e) NULL)

  if (is.null(res)) return(list(no_data = TRUE))

  srv <- selected_server %||% names(res$per_site)[1]
  causes_df <- .atlas_pick_result(res, scope, srv)

  if (!is.data.frame(causes_df) || nrow(causes_df) == 0) {
    return(list(no_data = TRUE))
  }

  # Resolve cause concept names
  concept_names <- NULL
  cids <- as.integer(causes_df$stratum_1[!is.na(causes_df$stratum_1)])
  if (length(cids) > 0) {
    tryCatch({
      name_res <- ds.omop.concept.lookup(cids, symbol = state$symbol)
      name_srv <- names(name_res$per_site)[1]
      concept_names <- name_res$per_site[[name_srv]]
    }, error = function(e) NULL)
  }

  total_deaths <- sum(as.numeric(causes_df$count_value), na.rm = TRUE)

  list(causes = causes_df, concept_names = concept_names,
       total_deaths = total_deaths)
}

.atlas_fetch_trends <- function(state, scope, policy,
                                 selected_server = NULL) {
  api_scope <- .backend_scope(scope)
  # Fixed: use persons-by-month analyses (402/702/602/1802)
  trend_ids <- c(402L, 702L, 602L, 1802L)
  result <- list()
  domain_names <- c("Conditions", "Drugs", "Procedures", "Measurements")

  for (i in seq_along(trend_ids)) {
    tryCatch({
      res <- ds.omop.achilles.results(
        analysis_ids = trend_ids[i], scope = api_scope, pooling_policy = policy,
        symbol = state$symbol)
      .atlas_accumulate_code(state, res)
      srv <- selected_server %||% names(res$per_site)[1]
      result[[domain_names[i]]] <- .atlas_pick_result(res, scope, srv)
    }, error = function(e) NULL)
  }

  result
}

.atlas_fetch_quality <- function(state, scope, policy) {
  tryCatch({
    status <- ds.omop.achilles.status(symbol = state$symbol)
    srv <- names(status$per_site)[1]

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
    "Death" = "death", "Observation Period" = "observation_period"
  )
  map[page_name]
}

.atlas_fetch_domain_dynamic <- function(state, analyses_df, scope, policy,
                                         selected_server = NULL) {
  count_row <- analyses_df[!is.na(analyses_df$stratum_1_name) &
                            analyses_df$stratum_1_name == "concept_id" &
                            (is.na(analyses_df$stratum_2_name) |
                             analyses_df$stratum_2_name == ""),
                           , drop = FALSE]
  count_id <- if (nrow(count_row) > 0) count_row$analysis_id[1] else NULL

  trend_row <- analyses_df[!is.na(analyses_df$stratum_2_name) &
                            analyses_df$stratum_2_name == "calendar_month",
                           , drop = FALSE]
  trend_id <- if (nrow(trend_row) > 0) trend_row$analysis_id[1] else NULL

  if (!is.null(count_id)) {
    .atlas_fetch_domain(state, count_id, trend_id, scope, policy,
                         selected_server)
  } else {
    NULL
  }
}

.atlas_fetch_trends_dynamic <- function(state, cat, scope, policy,
                                         selected_server = NULL) {
  if (is.null(cat)) return(.atlas_fetch_trends(state, scope, policy,
                                                 selected_server))

  trend_rows <- cat[!is.na(cat$stratum_2_name) &
                     cat$stratum_2_name == "calendar_month", , drop = FALSE]

  if (nrow(trend_rows) == 0) return(.atlas_fetch_trends(state, scope, policy,
                                                          selected_server))

  api_scope <- .backend_scope(scope)
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
    if (label %in% names(result)) next

    tryCatch({
      res <- ds.omop.achilles.results(
        analysis_ids = trend_rows$analysis_id[i], scope = api_scope,
        pooling_policy = policy, symbol = state$symbol)
      .atlas_accumulate_code(state, res)
      srv <- selected_server %||% names(res$per_site)[1]
      result[[label]] <- .atlas_pick_result(res, scope, srv)
    }, error = function(e) NULL)
  }

  result
}

# ==============================================================================
# Render helpers
# ==============================================================================

.atlas_render_dashboard <- function(ns, data) {
  total_txt <- .fmt_count(data$total_persons)

  # Median age
  median_age_val <- {
    if (is.data.frame(data$age_dist) && nrow(data$age_dist) > 0) {
      med <- data$age_dist$median_value[1]
      avg <- data$age_dist$avg_value[1]
      if (!is.na(med)) round(med, 1)
      else if (!is.na(avg)) paste0("~", round(avg, 1))
      else "N/A"
    } else "N/A"
  }

  # Sex ratio: "X% F / Y% M" style
  sex_ratio_val <- {
    if (is.data.frame(data$gender) && nrow(data$gender) > 0) {
      g <- data$gender
      g$count_value <- as.numeric(g$count_value)
      g <- g[!is.na(g$count_value), , drop = FALSE]
      if (nrow(g) > 0) {
        total_g <- sum(g$count_value)
        if (total_g > 0) {
          cmap <- data$concept_map %||% list()
          g$label <- vapply(g$stratum_1, function(id) {
            nm <- cmap[[as.character(id)]]
            if (!is.null(nm)) nm else id
          }, character(1))
          g <- g[order(g$count_value, decreasing = TRUE), ]
          parts <- vapply(seq_len(min(nrow(g), 3)), function(i) {
            pct <- round(g$count_value[i] / total_g * 100)
            lbl <- substr(g$label[i], 1, 1)
            paste0(pct, "% ", lbl)
          }, character(1))
          paste(parts, collapse = " / ")
        } else "N/A"
      } else "N/A"
    } else "N/A"
  }

  # Observation span: year range from analysis 109
  obs_span_val <- {
    if (is.data.frame(data$obs_by_year) && nrow(data$obs_by_year) > 0) {
      yrs <- suppressWarnings(as.integer(data$obs_by_year$stratum_1))
      yrs <- yrs[!is.na(yrs)]
      if (length(yrs) > 0) {
        paste0(min(yrs), "\u2013", max(yrs))
      } else "N/A"
    } else "N/A"
  }

  shiny::tagList(
    # Panel B: KPIs
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      bslib::value_box(
        title = "Total Persons", value = total_txt,
        showcase = fontawesome::fa_i("users"), theme = "primary"
      ),
      bslib::value_box(
        title = "Median Age", value = median_age_val,
        showcase = fontawesome::fa_i("calendar"), theme = "info"
      ),
      bslib::value_box(
        title = "Sex Ratio", value = sex_ratio_val,
        showcase = fontawesome::fa_i("venus-mars"), theme = "success"
      ),
      bslib::value_box(
        title = "Observation Span", value = obs_span_val,
        showcase = fontawesome::fa_i("clock"), theme = "warning"
      )
    ),

    # Panel C: Sex Distribution
    bslib::card(
      bslib::card_header("Sex Distribution"),
      bslib::card_body(
        plotly::plotlyOutput(ns("dash_gender_plot"), height = "280px")
      )
    ),

    # Panel D + D2: Age Distribution + Birth Cohorts side by side
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header(
          shiny::div(class = "d-flex justify-content-between align-items-center",
            shiny::span("Age Distribution"),
            shiny::radioButtons(ns("dash_age_toggle"), NULL,
              choices = c("Age Bands", "Percentiles"),
              selected = "Age Bands", inline = TRUE)
          )
        ),
        bslib::card_body(
          plotly::plotlyOutput(ns("dash_age_plot"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Birth Cohorts"),
        bslib::card_body(
          if (is.data.frame(data$birth_cohorts) && nrow(data$birth_cohorts) > 0) {
            plotly::plotlyOutput(ns("dash_birth_cohort_plot"), height = "350px")
          } else {
            shiny::div(class = "text-muted text-center py-4",
              shiny::icon("shield-halved", class = "me-1"),
              "Birth cohort data suppressed (small cell counts)")
          }
        )
      )
    ),

    # Panel E: Race + Ethnicity side by side
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Race"),
        bslib::card_body(
          if (is.data.frame(data$race) && nrow(data$race) > 0) {
            plotly::plotlyOutput(ns("dash_race_plot"), height = "280px")
          } else {
            shiny::div(class = "text-muted text-center py-4", "No race data")
          }
        )
      ),
      bslib::card(
        bslib::card_header("Ethnicity"),
        bslib::card_body(
          if (is.data.frame(data$ethnicity) && nrow(data$ethnicity) > 0) {
            plotly::plotlyOutput(ns("dash_ethnicity_plot"), height = "280px")
          } else {
            shiny::div(class = "text-muted text-center py-4", "No ethnicity data")
          }
        )
      )
    ),

    # Panel F: Observation Period (by year + length distribution)
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Persons by Calendar Year"),
        bslib::card_body(
          if (is.data.frame(data$obs_by_year) && nrow(data$obs_by_year) > 0) {
            plotly::plotlyOutput(ns("dash_obs_year_plot"), height = "280px")
          } else {
            shiny::div(class = "text-muted text-center py-4",
                       "No observation period data")
          }
        )
      ),
      bslib::card(
        bslib::card_header("Observation Period Length"),
        bslib::card_body(
          if (is.data.frame(data$obs_length_dist) && nrow(data$obs_length_dist) > 0) {
            plotly::plotlyOutput(ns("dash_obs_length_plot"), height = "280px")
          } else {
            shiny::div(class = "text-muted text-center py-4",
                       "No observation length data")
          }
        )
      )
    ),

    # Panel G: Domain Coverage
    if (!is.null(data$domain_coverage) &&
        is.data.frame(data$domain_coverage) &&
        nrow(data$domain_coverage) > 0) {
      bslib::card(
        bslib::card_header("Domain Coverage"),
        bslib::card_body(
          plotly::plotlyOutput(ns("dash_coverage_plot"), height = "300px")
        )
      )
    }
  )
}

.atlas_render_domain <- function(ns, data, domain_label) {
  has_concepts <- !is.null(data$concepts) && is.data.frame(data$concepts) &&
    nrow(data$concepts) > 0

  # KPI row
  kpi_row <- {
    coverage_txt <- if (!is.na(data$coverage_pct %||% NA)) {
      paste0(data$coverage_pct, "%")
    } else "N/A"
    n_concepts_txt <- if (!is.null(data$n_concepts) && data$n_concepts > 0) {
      format(data$n_concepts, big.mark = ",")
    } else "0"

    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Coverage", value = coverage_txt,
        showcase = fontawesome::fa_i("chart-pie"), theme = "info",
        p = shiny::p(class = "text-muted small", "% persons with any record")
      ),
      bslib::value_box(
        title = "Distinct Concepts", value = n_concepts_txt,
        showcase = fontawesome::fa_i("tags"), theme = "primary"
      ),
      bslib::value_box(
        title = "Total Persons",
        value = .fmt_count(data$total_persons),
        showcase = fontawesome::fa_i("users"), theme = "secondary"
      )
    )
  }

  shiny::tagList(
    kpi_row,
    if (has_concepts) {
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(6, plotly::plotlyOutput(ns("domain_bar_plot"),
                                                height = "400px")),
          shiny::column(6,
            if (!is.null(data$trends) && is.data.frame(data$trends) &&
                nrow(data$trends) > 0) {
              plotly::plotlyOutput(ns("domain_trend_plot"), height = "400px")
            } else {
              shiny::div(class = "text-muted text-center py-5",
                         "No trend data available")
            }
          )
        ),
        shiny::div(class = "d-flex justify-content-end mb-2 mt-3",
          bslib::tooltip(
            shiny::actionButton(ns("domain_extract_btn"),
              shiny::tagList(shiny::icon("plus"), "Extract"),
              class = "btn-sm btn-success text-white me-1"),
            "Add selected concepts as variables to Builder"
          ),
          bslib::tooltip(
            shiny::actionButton(ns("domain_filter_btn"),
              shiny::tagList(shiny::icon("filter"), "Filter"),
              class = "btn-sm btn-warning text-white"),
            "Add selected concepts as filters to Builder"
          )
        ),
        DT::DTOutput(ns("domain_table"))
      )
    } else {
      .empty_state_ui("chart-bar", "No data available for this domain",
        "This domain may not have Achilles results or concepts available.")
    }
  )
}

.atlas_render_visits <- function(ns, data) {
  # KPI row
  n_types_txt <- if (!is.null(data$n_visit_types) && data$n_visit_types > 0) {
    format(data$n_visit_types, big.mark = ",")
  } else "0"
  total_visits_txt <- .fmt_count(data$total_visits)

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Visit Types", value = n_types_txt,
        showcase = fontawesome::fa_i("hospital"), theme = "primary"
      ),
      bslib::value_box(
        title = "Total Visit Records", value = total_visits_txt,
        showcase = fontawesome::fa_i("clipboard-list"), theme = "info"
      ),
      bslib::value_box(
        title = "Total Persons",
        value = .fmt_count(data$total_persons),
        showcase = fontawesome::fa_i("users"), theme = "secondary"
      )
    ),
    shiny::fluidRow(
      shiny::column(6, plotly::plotlyOutput(ns("visits_type_plot"),
                                            height = "350px")),
      shiny::column(6, plotly::plotlyOutput(ns("visits_trend_plot"),
                                            height = "350px"))
    )
  )
}

.atlas_render_death <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.empty_state_ui("skull-crossbones", "No Death Data",
      "Death records are not available in this CDM or Achilles did not compute death analyses."))
  }

  total_txt <- .fmt_count(data$total_deaths)
  n_causes <- if (!is.null(data$causes) && is.data.frame(data$causes)) {
    nrow(data$causes)
  } else 0

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Total Deaths", value = total_txt,
        showcase = fontawesome::fa_i("skull-crossbones"), theme = "danger"
      ),
      bslib::value_box(
        title = "Distinct Causes", value = format(n_causes, big.mark = ","),
        showcase = fontawesome::fa_i("tags"), theme = "warning"
      )
    ),
    if (n_causes > 0) {
      plotly::plotlyOutput(ns("death_cause_plot"), height = "450px")
    } else {
      shiny::div(class = "text-muted text-center py-5",
                 "No cause-of-death data available")
    }
  )
}

.atlas_render_trends <- function(ns, data) {
  shiny::tagList(
    plotly::plotlyOutput(ns("trends_overlay_plot"), height = "500px")
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

.atlas_resolve_concept_ids <- function(ids, state) {
  ids <- unique(as.integer(ids[!is.na(ids)]))
  if (length(ids) == 0) return(list())
  tryCatch({
    res <- ds.omop.concept.lookup(ids, symbol = state$symbol)
    srv <- names(res$per_site)[1]
    df <- res$per_site[[srv]]
    if (is.data.frame(df) && nrow(df) > 0) {
      result <- stats::setNames(as.character(df$concept_name),
                                as.character(df$concept_id))
      return(as.list(result))
    }
    list()
  }, error = function(e) list())
}

.atlas_label_stratum <- function(stratum_ids, concept_map, max_len = 28) {
  vapply(as.character(stratum_ids), function(id) {
    nm <- concept_map[[id]]
    label <- if (!is.null(nm) && !is.na(nm) && nchar(nm) > 0) nm else id
    if (is.na(label)) label <- id
    if (nchar(label) > max_len) paste0(substr(label, 1, max_len - 3), "...")
    else label
  }, character(1), USE.NAMES = FALSE)
}

.atlas_accumulate_code <- function(state, res) {
  if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
    shiny::isolate({
      state$script_lines <- c(state$script_lines, res$meta$call_code)
    })
  }
}

.atlas_pick_result <- function(res, scope, srv) {
  if (scope == "pooled" && !is.null(res$pooled)) {
    return(res$pooled)
  }
  if (length(res$per_site) == 0) return(NULL)

  srvs <- .resolve_servers(res$per_site, srv)
  if (length(srvs) == 0) return(NULL)

  if (scope == "per_site") {
    return(res$per_site[[srvs[1]]])
  }

  # scope == "all": rbind selected servers with server column
  dfs <- list()
  for (nm in srvs) {
    df <- res$per_site[[nm]]
    if (is.data.frame(df) && nrow(df) > 0) {
      df$server <- nm
      dfs[[nm]] <- df
    }
  }
  if (length(dfs) == 0) return(NULL)
  combined <- do.call(rbind, dfs)
  rownames(combined) <- NULL
  combined
}
