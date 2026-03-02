# Module: Studio - OHDSI Results Consumer
# Shiny module for displaying pre-computed OHDSI tool result tables
# (DQD, CohortDiagnostics, CohortIncidence, Characterization,
#  CohortMethod, SCCS, PLP, EvidenceSynthesis).

#' Studio OHDSI Results UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
.mod_ohdsi_results_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # --- Status + page selector (horizontal) ---
    shiny::div(class = "d-flex align-items-center gap-3 mb-3 flex-wrap",
      shiny::uiOutput(ns("ohdsi_status")),
      shiny::div(style = "min-width: 200px;",
        shiny::selectInput(ns("ohdsi_nav"), NULL,
          choices = "Overview", selected = "Overview")
      )
    ),
    # --- Page content ---
    shiny::uiOutput(ns("ohdsi_page_content"))
  )
}

#' Studio OHDSI Results Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @param parent_session Shiny session; the parent session for tab navigation.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_ohdsi_results_server <- function(id, state, parent_session = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ohdsi_available <- shiny::reactiveVal(FALSE)
    ohdsi_per_server <- shiny::reactiveVal(list())
    ohdsi_tool_status <- shiny::reactiveVal(list())
    page_data <- shiny::reactiveVal(NULL)

    # Send tab disabled/enabled message to parent session
    .toggle_ohdsi_tab <- function(disabled) {
      target <- parent_session %||% session
      target$sendCustomMessage("toggleOhdsiResultsTab", list(
        disabled = disabled,
        tooltip = if (disabled)
          "No OHDSI result tables found on any server" else ""
      ))
    }

    # Check OHDSI tool availability across ALL servers
    shiny::observe({
      tryCatch({
        status <- ds.omop.ohdsi.status(symbol = state$symbol)
        .ohdsi_accumulate_code(state, status)

        per_srv <- list()
        any_avail <- FALSE
        tool_avail <- list(
          dqd = FALSE, cohort_diagnostics = FALSE,
          cohort_incidence = FALSE, characterization = FALSE,
          cohort_method = FALSE, sccs = FALSE,
          plp = FALSE, evidence_synthesis = FALSE
        )

        for (srv in names(status$per_site)) {
          srv_status <- status$per_site[[srv]]
          per_srv[[srv]] <- srv_status
          for (tid in names(tool_avail)) {
            if (!is.null(srv_status[[tid]]) &&
                isTRUE(srv_status[[tid]]$available)) {
              tool_avail[[tid]] <- TRUE
              any_avail <- TRUE
            }
          }
        }

        ohdsi_per_server(per_srv)
        ohdsi_tool_status(tool_avail)
        ohdsi_available(any_avail)
        .toggle_ohdsi_tab(!any_avail)
      }, error = function(e) {
        ohdsi_available(FALSE)
        .toggle_ohdsi_tab(TRUE)
        shiny::showNotification(
          paste0("OHDSI status check failed: ", .clean_ds_error(e)),
          type = "warning", duration = 6
        )
      })
    })

    # Dynamic navigation choices based on available tools.
    # Guard: skip the initial empty list() to avoid a flash of "Overview" only.
    shiny::observe({
      tool_avail <- ohdsi_tool_status()
      if (length(tool_avail) == 0) return()  # not yet loaded

      choices <- list("General" = "Overview")

      if (isTRUE(tool_avail$dqd)) {
        choices[["DQD"]] <- c(choices[["DQD"]], "Data Quality")
      }

      cd_pages <- character(0)
      if (isTRUE(tool_avail$cohort_diagnostics)) {
        cd_pages <- c("Cohort Counts", "Index Events", "Visit Context",
                       "Temporal Covariates", "Time Series", "Concept Coverage")
      }
      if (length(cd_pages) > 0) choices[["CohortDiagnostics"]] <- cd_pages

      if (isTRUE(tool_avail$cohort_diagnostics) ||
          isTRUE(tool_avail$cohort_incidence)) {
        choices[["CohortIncidence"]] <- "Incidence"
      }

      char_pages <- character(0)
      if (isTRUE(tool_avail$characterization)) {
        char_pages <- c("Characterization", "Continuous Covariates",
                         "Time to Event", "Dechallenge/Rechallenge")
      }
      if (length(char_pages) > 0) choices[["Characterization"]] <- char_pages

      if (isTRUE(tool_avail$cohort_method)) {
        choices[["CohortMethod"]] <- "Estimation"
      }
      if (isTRUE(tool_avail$sccs)) {
        choices[["SCCS"]] <- "Self-Controlled"
      }
      if (isTRUE(tool_avail$plp)) {
        choices[["PLP"]] <- "Prediction"
      }

      shiny::updateSelectInput(session, "ohdsi_nav", choices = choices)
    })

    # Status display
    output$ohdsi_status <- shiny::renderUI({
      if (ohdsi_available()) {
        tool_avail <- ohdsi_tool_status()
        n_tools <- sum(vapply(tool_avail, isTRUE, logical(1)))
        shiny::div(
          shiny::span(class = "status-ok", shiny::icon("check-circle"),
                      " OHDSI Results Available"),
          shiny::p(class = "text-muted small mt-1",
                   paste0(n_tools, " of ", length(tool_avail), " tools detected"))
        )
      } else {
        shiny::div(
          shiny::span(class = "status-warn", shiny::icon("exclamation-triangle"),
                      " No OHDSI Results Found"),
          shiny::p(class = "text-muted small mt-1",
                   "Run OHDSI tools on your CDM to populate result tables")
        )
      }
    })

    # Re-fetch when scope or server changes
    shiny::observeEvent(list(state$scope, state$selected_servers), {
      if (!ohdsi_available() || is.null(input$ohdsi_nav)) return()
      nav <- input$ohdsi_nav
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"

      tryCatch({
        data <- .ohdsi_dispatch_fetch(nav, state, scope, policy,
                                       state$selected_servers)
        page_data(data %||% list(no_data = TRUE))
      }, error = function(e) {
        err_msg <- .clean_ds_error(e)
        page_data(list(error = err_msg))
        shiny::showNotification(
          paste0("Failed to load OHDSI data: ", err_msg),
          type = "error", duration = 6
        )
      })
    }, ignoreInit = TRUE)

    # Fetch data when page changes
    shiny::observeEvent(input$ohdsi_nav, {
      if (!ohdsi_available()) {
        page_data(NULL)
        return()
      }

      nav <- input$ohdsi_nav
      scope <- state$scope %||% "per_site"
      policy <- state$pooling_policy %||% "strict"
      selected_srv <- state$selected_servers

      shiny::withProgress(message = paste("Loading", nav, "..."), value = 0.2, {
        tryCatch({
          data <- .ohdsi_dispatch_fetch(nav, state, scope, policy, selected_srv)
          shiny::incProgress(0.6)
          page_data(data %||% list(no_data = TRUE))
        }, error = function(e) {
          err_msg <- .clean_ds_error(e)
          page_data(list(error = err_msg))
          shiny::showNotification(
            paste0("Failed to load ", nav, ": ", err_msg),
            type = "error", duration = 6
          )
        })
      })
    })

    # Render page content
    output$ohdsi_page_content <- shiny::renderUI({
      if (!ohdsi_available()) {
        return(.empty_state_ui("database", "OHDSI Results Not Available",
          "No connected servers have pre-computed OHDSI result tables."))
      }

      # Partial data warning banner
      per_srv <- ohdsi_per_server()
      tool_avail <- ohdsi_tool_status()
      missing_srvs <- character(0)
      for (srv in names(per_srv)) {
        srv_has_any <- FALSE
        srv_status <- per_srv[[srv]]
        for (tid in names(tool_avail)) {
          if (!is.null(srv_status[[tid]]) &&
              isTRUE(srv_status[[tid]]$available)) {
            srv_has_any <- TRUE
            break
          }
        }
        if (!srv_has_any) missing_srvs <- c(missing_srvs, srv)
      }
      warn_banner <- NULL
      if (length(missing_srvs) > 0 && length(missing_srvs) < length(per_srv)) {
        warn_banner <- shiny::div(class = "alert alert-warning py-2 mb-3",
          shiny::icon("exclamation-triangle"),
          paste0(" OHDSI results missing on: ",
                 paste(missing_srvs, collapse = ", "),
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

      # Generic no_data only for pages without their own empty-state handler.
      # Tool pages (DQD, Cohort Counts, etc.) handle their own tool_missing
      # with specific "Run Tool X on your CDM" messages.
      if (isTRUE(data$no_data) && is.null(data$tool_missing)) {
        return(shiny::div(class = "text-center text-muted py-5",
          shiny::icon("circle-info", class = "fa-2x mb-3"),
          shiny::h5("No data available for this page")
        ))
      }

      nav <- input$ohdsi_nav

      page_ui <- switch(nav,
        "Overview"              = .ohdsi_render_overview(ns, data),
        "Data Quality"          = .ohdsi_render_dqd(ns, data),
        "Cohort Counts"         = .ohdsi_render_cohort_counts(ns, data),
        "Incidence"             = .ohdsi_render_incidence(ns, data),
        "Characterization"      = .ohdsi_render_characterization(ns, data),
        "Index Events"          = .ohdsi_render_index_events(ns, data),
        "Visit Context"         = .ohdsi_render_visit_context(ns, data),
        "Temporal Covariates"   = .ohdsi_render_temporal_covariates(ns, data),
        "Time Series"           = .ohdsi_render_time_series(ns, data),
        "Concept Coverage"      = .ohdsi_render_concept_coverage(ns, data),
        "Continuous Covariates" = .ohdsi_render_continuous_covariates(ns, data),
        "Time to Event"         = .ohdsi_render_time_to_event(ns, data),
        "Dechallenge/Rechallenge" = .ohdsi_render_dechallenge(ns, data),
        "Estimation"            = .ohdsi_render_estimation(ns, data),
        "Self-Controlled"       = .ohdsi_render_self_controlled(ns, data),
        "Prediction"            = .ohdsi_render_prediction(ns, data),
        shiny::div("Unknown page")
      )

      if (nav == "Overview") {
        shiny::tagList(warn_banner, page_ui)
      } else {
        shiny::tagList(
          warn_banner,
          bslib::card(full_screen = TRUE,
            bslib::card_header(paste("OHDSI Results:", nav)),
            bslib::card_body(page_ui)
          )
        )
      }
    })

    # ---- Plot outputs ----

    output$dqd_category_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$results)) {
        return(.plotly_no_data("No DQD results available"))
      }
      .safe_plotly({
        df <- data$results
        if (!"check_type" %in% names(df) || !"failed" %in% names(df)) {
          return(.plotly_no_data("DQD results missing expected columns"))
        }
        # Guard: coerce failed to logical, replace NA with FALSE
        df$failed <- ifelse(is.na(df$failed), FALSE, as.logical(df$failed))
        agg <- stats::aggregate(
          cbind(n_pass = !df$failed, n_fail = df$failed) ~ df$check_type,
          data = df, FUN = sum, na.rm = TRUE
        )
        names(agg)[1] <- "category"
        if (nrow(agg) == 0) return(.plotly_no_data("No DQD check categories"))

        plotly::plot_ly(agg, x = ~category, y = ~n_pass, type = "bar",
                        name = "Pass",
                        marker = list(color = .studio_colors[2])) |>
          plotly::add_trace(y = ~n_fail, name = "Fail",
                            marker = list(color = .studio_colors[6])) |>
          plotly::layout(barmode = "stack",
                         xaxis = list(title = ""),
                         yaxis = list(title = "Checks")) |>
          .plotly_defaults("DQD Checks by Category")
      })
    })

    output$cohort_bar_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$cohort_counts)) {
        return(.plotly_no_data("No cohort count data available"))
      }
      .safe_plotly({
        df <- data$cohort_counts
        count_col <- intersect(c("cohort_subjects", "cohort_entries"),
                               names(df))
        if (length(count_col) == 0) {
          return(.plotly_no_data("Cohort count columns not found"))
        }
        count_col <- count_col[1]
        df$count <- as.numeric(df[[count_col]])
        df <- df[!is.na(df$count) & df$count > 0, , drop = FALSE]
        if (nrow(df) == 0) {
          return(.plotly_no_data("Cohort data suppressed", ""))
        }
        df <- df[order(df$count, decreasing = TRUE), , drop = FALSE]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), , drop = FALSE]
        label_col <- if ("cohort_name" %in% names(df)) "cohort_name"
                     else "cohort_id"
        labels <- as.character(df[[label_col]])
        labels <- substr(labels, 1, 35)

        plotly::plot_ly(x = df$count, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[1]),
                        hovertemplate = "<b>%{y}</b><br>Subjects: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults(paste("Top", n, "Cohorts"))
      })
    })

    output$incidence_bar_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$incidence)) {
        return(.plotly_no_data("No incidence data available"))
      }
      .safe_plotly({
        df <- data$incidence
        rate_col <- intersect(
          c("incidence_proportion_p100p", "incidence_rate_p100py",
            "incidence_proportion", "incidence_rate"),
          names(df))
        if (length(rate_col) == 0) {
          return(.plotly_no_data("Incidence rate columns not found"))
        }
        rate_col <- rate_col[1]
        df$rate <- as.numeric(df[[rate_col]])
        df <- df[!is.na(df$rate) & df$rate > 0, , drop = FALSE]
        if (nrow(df) == 0) {
          return(.plotly_no_data("Incidence data suppressed", ""))
        }

        group_col <- intersect(c("subgroup_name", "strata_name", "age_group"),
                               names(df))
        if (length(group_col) > 0) {
          group_col <- group_col[1]
          # Grouped bar
          df <- df[order(df$rate, decreasing = TRUE), , drop = FALSE]
          n <- min(nrow(df), 30)
          df <- df[seq_len(n), , drop = FALSE]
          plotly::plot_ly(x = as.character(df[[group_col]]), y = df$rate,
                          type = "bar",
                          marker = list(color = .studio_colors[1]),
                          hovertemplate = "<b>%{x}</b><br>Rate: %{y:.2f}<extra></extra>") |>
            plotly::layout(xaxis = list(title = "", tickangle = -45),
                           yaxis = list(title = "Rate")) |>
            .plotly_defaults("Incidence Rates by Group")
        } else {
          # Simple bar by outcome
          id_col <- if ("outcome_id" %in% names(df)) "outcome_id"
                    else if ("target_cohort_id" %in% names(df)) "target_cohort_id"
                    else NULL
          if (is.null(id_col)) {
            return(.plotly_no_data("No grouping column found"))
          }
          df <- df[order(df$rate, decreasing = TRUE), , drop = FALSE]
          n <- min(nrow(df), 20)
          df <- df[seq_len(n), , drop = FALSE]
          plotly::plot_ly(x = as.character(df[[id_col]]), y = df$rate,
                          type = "bar",
                          marker = list(color = .studio_colors[1])) |>
            plotly::layout(xaxis = list(title = id_col),
                           yaxis = list(title = "Rate")) |>
            .plotly_defaults("Incidence Rates")
        }
      })
    })

    output$char_covariate_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$covariates)) {
        return(.plotly_no_data("No covariate data available"))
      }
      .safe_plotly({
        df <- data$covariates
        val_col <- intersect(c("average_value", "mean_value", "mean"),
                             names(df))
        if (length(val_col) == 0) {
          return(.plotly_no_data("Covariate value columns not found"))
        }
        val_col <- val_col[1]
        df$value <- as.numeric(df[[val_col]])
        df <- df[!is.na(df$value) & df$value > 0, , drop = FALSE]
        if (nrow(df) == 0) {
          return(.plotly_no_data("Covariate data empty or suppressed",
                                 ""))
        }

        name_col <- intersect(c("covariate_name", "concept_name"), names(df))
        if (length(name_col) == 0) name_col <- "covariate_id"
        else name_col <- name_col[1]

        df <- df[order(df$value, decreasing = TRUE), , drop = FALSE]
        n <- min(nrow(df), 15)
        df <- df[seq_len(n), , drop = FALSE]
        labels <- substr(as.character(df[[name_col]]), 1, 40)

        plotly::plot_ly(x = df$value, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[4]),
                        hovertemplate = "<b>%{y}</b><br>Mean: %{x:.4f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults(paste("Top", n, "Covariates"))
      })
    })

    # ---- New page plot outputs ----

    output$index_events_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$index_events)) {
        return(.plotly_no_data("No index event data available"))
      }
      .safe_plotly({
        df <- data$index_events
        count_col <- intersect(c("subject_count", "concept_count"), names(df))
        if (length(count_col) == 0) return(.plotly_no_data("Count columns not found"))
        df$count <- as.numeric(df[[count_col[1]]])
        df <- df[!is.na(df$count) & df$count > 0, , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Index event data suppressed", ""))
        df <- df[order(df$count, decreasing = TRUE), , drop = FALSE]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), , drop = FALSE]
        name_col <- if ("concept_name" %in% names(df)) "concept_name" else "concept_id"
        labels <- substr(as.character(df[[name_col]]), 1, 40)
        plotly::plot_ly(x = df$count, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[1]),
                        hovertemplate = "<b>%{y}</b><br>Count: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults(paste("Top", n, "Index Event Concepts"))
      })
    })

    output$visit_context_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$visit_context)) {
        return(.plotly_no_data("No visit context data available"))
      }
      .safe_plotly({
        df <- data$visit_context
        if (!"subjects" %in% names(df)) return(.plotly_no_data("Subjects column not found"))
        df$count <- as.numeric(df$subjects)
        df <- df[!is.na(df$count) & df$count > 0, , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Visit context data suppressed", ""))
        label_col <- if ("visit_context" %in% names(df)) "visit_context" else "visit_concept_id"
        labels <- substr(as.character(df[[label_col]]), 1, 35)
        plotly::plot_ly(x = df$count, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[2]),
                        hovertemplate = "<b>%{y}</b><br>Subjects: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Visit Context")
      })
    })

    output$temporal_cov_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$temporal_covariates)) {
        return(.plotly_no_data("No temporal covariate data available"))
      }
      .safe_plotly({
        df <- data$temporal_covariates
        if (!all(c("time_id", "mean") %in% names(df))) {
          return(.plotly_no_data("Expected columns (time_id, mean) not found"))
        }
        df$time_id <- as.numeric(df$time_id)
        df$mean <- as.numeric(df$mean)
        df <- df[!is.na(df$time_id) & !is.na(df$mean), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Temporal data empty", ""))
        name_col <- if ("covariate_name" %in% names(df)) "covariate_name" else "covariate_id"
        top_covs <- unique(df[[name_col]])[seq_len(min(10, length(unique(df[[name_col]]))))]
        df <- df[df[[name_col]] %in% top_covs, , drop = FALSE]
        p <- plotly::plot_ly()
        colors <- rep(.studio_colors, length.out = length(top_covs))
        for (i in seq_along(top_covs)) {
          sub <- df[df[[name_col]] == top_covs[i], , drop = FALSE]
          sub <- sub[order(sub$time_id), , drop = FALSE]
          p <- plotly::add_trace(p, x = sub$time_id, y = sub$mean,
                                  type = "scatter", mode = "lines+markers",
                                  name = substr(as.character(top_covs[i]), 1, 30),
                                  line = list(color = colors[i]),
                                  marker = list(color = colors[i]))
        }
        p |> plotly::layout(xaxis = list(title = "Time (days)"),
                            yaxis = list(title = "Mean")) |>
          .plotly_defaults("Temporal Covariate Values")
      })
    })

    output$time_series_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$time_series)) {
        return(.plotly_no_data("No time series data available"))
      }
      .safe_plotly({
        df <- data$time_series
        if (!all(c("calendar_year", "calendar_month") %in% names(df))) {
          return(.plotly_no_data("Expected columns not found"))
        }
        val_col <- intersect(c("records", "subjects"), names(df))
        if (length(val_col) == 0) return(.plotly_no_data("Count column not found"))
        df$value <- as.numeric(df[[val_col[1]]])
        df$date <- paste0(df$calendar_year, "-",
                          sprintf("%02d", as.integer(df$calendar_month)), "-01")
        df <- df[!is.na(df$value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Time series data empty", ""))
        df <- df[order(df$date), , drop = FALSE]
        plotly::plot_ly(x = df$date, y = df$value, type = "scatter",
                        mode = "lines+markers",
                        line = list(color = .studio_colors[1]),
                        marker = list(color = .studio_colors[1]),
                        hovertemplate = "<b>%{x}</b><br>Count: %{y:,.0f}<extra></extra>") |>
          plotly::layout(xaxis = list(title = "Date"),
                         yaxis = list(title = val_col[1])) |>
          .plotly_defaults("Cohort Time Series")
      })
    })

    output$included_concepts_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$included_concepts)) {
        return(.plotly_no_data("No included concept data available"))
      }
      .safe_plotly({
        df <- data$included_concepts
        count_col <- intersect(c("concept_count", "subject_count"), names(df))
        if (length(count_col) == 0) return(.plotly_no_data("Count column not found"))
        df$count <- as.numeric(df[[count_col[1]]])
        df <- df[!is.na(df$count) & df$count > 0, , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Included concept data suppressed", ""))
        df <- df[order(df$count, decreasing = TRUE), , drop = FALSE]
        n <- min(nrow(df), 20)
        df <- df[seq_len(n), , drop = FALSE]
        name_col <- if ("concept_name" %in% names(df)) "concept_name" else "concept_id"
        labels <- substr(as.character(df[[name_col]]), 1, 40)
        plotly::plot_ly(x = df$count, y = labels, type = "bar",
                        orientation = "h",
                        marker = list(color = .studio_colors[3]),
                        hovertemplate = "<b>%{y}</b><br>Count: %{x:,.0f}<extra></extra>") |>
          plotly::layout(yaxis = list(categoryorder = "total ascending")) |>
          .plotly_defaults("Included Source Concepts")
      })
    })

    output$continuous_cov_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$continuous_covariates)) {
        return(.plotly_no_data("No continuous covariate data available"))
      }
      .safe_plotly({
        df <- data$continuous_covariates
        needed <- c("min_value", "p25_value", "median_value", "p75_value", "max_value")
        if (!all(needed %in% names(df))) {
          return(.plotly_no_data("Expected distribution columns not found"))
        }
        if (nrow(df) == 0) return(.plotly_no_data("No continuous covariate data", ""))
        name_col <- if ("covariate_name" %in% names(df)) "covariate_name"
                     else if ("covariate_id" %in% names(df)) "covariate_id"
                     else NULL
        n <- min(nrow(df), 15)
        df <- df[seq_len(n), , drop = FALSE]
        labels <- if (!is.null(name_col)) substr(as.character(df[[name_col]]), 1, 35)
                  else paste("Covariate", seq_len(n))
        plotly::plot_ly(y = labels, type = "box",
                        lowerfence = as.numeric(df$min_value),
                        q1 = as.numeric(df$p25_value),
                        median = as.numeric(df$median_value),
                        q3 = as.numeric(df$p75_value),
                        upperfence = as.numeric(df$max_value),
                        orientation = "h",
                        marker = list(color = .studio_colors[4]),
                        line = list(color = .studio_colors[4])) |>
          plotly::layout(yaxis = list(categoryorder = "trace")) |>
          .plotly_defaults("Continuous Covariate Distributions")
      })
    })

    output$time_to_event_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$time_to_event)) {
        return(.plotly_no_data("No time-to-event data available"))
      }
      .safe_plotly({
        df <- data$time_to_event
        if (!all(c("time_value", "value") %in% names(df))) {
          return(.plotly_no_data("Expected columns not found"))
        }
        df$time_value <- as.numeric(df$time_value)
        df$value <- as.numeric(df$value)
        df <- df[!is.na(df$time_value) & !is.na(df$value), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Time-to-event data empty", ""))
        group_col <- if ("outcome_id" %in% names(df)) "outcome_id" else NULL
        if (!is.null(group_col)) {
          groups <- unique(df[[group_col]])
          p <- plotly::plot_ly()
          colors <- rep(.studio_colors, length.out = length(groups))
          for (i in seq_along(groups)) {
            sub <- df[df[[group_col]] == groups[i], , drop = FALSE]
            sub <- sub[order(sub$time_value), , drop = FALSE]
            p <- plotly::add_trace(p, x = sub$time_value, y = sub$value,
                                    type = "scatter", mode = "lines+markers",
                                    name = paste("Outcome", groups[i]),
                                    line = list(color = colors[i]))
          }
          p |> plotly::layout(xaxis = list(title = "Days"),
                              yaxis = list(title = "Survival Probability")) |>
            .plotly_defaults("Time to Event")
        } else {
          df <- df[order(df$time_value), , drop = FALSE]
          plotly::plot_ly(x = df$time_value, y = df$value,
                          type = "scatter", mode = "lines+markers",
                          line = list(color = .studio_colors[1])) |>
            plotly::layout(xaxis = list(title = "Days"),
                           yaxis = list(title = "Value")) |>
            .plotly_defaults("Time to Event")
        }
      })
    })

    output$dechallenge_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$dechallenge)) {
        return(.plotly_no_data("No dechallenge data available"))
      }
      .safe_plotly({
        df <- data$dechallenge
        needed <- c("num_dechallenge_attempt", "num_dechallenge_success")
        if (!all(needed %in% names(df))) {
          return(.plotly_no_data("Expected columns not found"))
        }
        dech_att <- as.numeric(df$num_dechallenge_attempt)
        dech_suc <- as.numeric(df$num_dechallenge_success)
        df$dechallenge_rate <- ifelse(
          !is.na(dech_att) & dech_att > 0,
          dech_suc / dech_att * 100, 0)
        df$rechallenge_rate <- if ("num_rechallenge_attempt" %in% names(df) &&
                                     "num_rechallenge_success" %in% names(df)) {
          rech_att <- as.numeric(df$num_rechallenge_attempt)
          rech_suc <- as.numeric(df$num_rechallenge_success)
          ifelse(!is.na(rech_att) & rech_att > 0,
                 rech_suc / rech_att * 100, 0)
        } else {
          rep(0, nrow(df))
        }
        label <- if ("cohort_id" %in% names(df)) paste("Cohort", df$cohort_id)
                 else paste("Row", seq_len(nrow(df)))
        plotly::plot_ly(x = label, y = df$dechallenge_rate, type = "bar",
                        name = "Dechallenge Success %",
                        marker = list(color = .studio_colors[2])) |>
          plotly::add_trace(y = df$rechallenge_rate, name = "Rechallenge Success %",
                            marker = list(color = .studio_colors[5])) |>
          plotly::layout(barmode = "group",
                         xaxis = list(title = ""),
                         yaxis = list(title = "Success Rate (%)")) |>
          .plotly_defaults("Dechallenge / Rechallenge Success Rates")
      })
    })

    output$estimation_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$cm_results)) {
        return(.plotly_no_data("No estimation results available"))
      }
      .safe_plotly({
        .ohdsi_forest_plot(data$cm_results, "rr", "ci_95_lb", "ci_95_ub",
                            title = "Cohort Method: Hazard Ratios")
      })
    })

    output$estimation_meta_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$es_cm_results)) {
        return(.plotly_no_data("No meta-analysis results available"))
      }
      .safe_plotly({
        .ohdsi_forest_plot(data$es_cm_results, "rr", "ci_95_lb", "ci_95_ub",
                            title = "Evidence Synthesis: Meta-Analysis")
      })
    })

    output$self_controlled_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$sccs_results)) {
        return(.plotly_no_data("No SCCS results available"))
      }
      .safe_plotly({
        .ohdsi_forest_plot(data$sccs_results, "rr", "ci_95_lb", "ci_95_ub",
                            title = "SCCS: Incidence Rate Ratios")
      })
    })

    output$self_controlled_meta_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$es_sccs_results)) {
        return(.plotly_no_data("No SCCS meta-analysis results available"))
      }
      .safe_plotly({
        .ohdsi_forest_plot(data$es_sccs_results, "rr", "ci_95_lb", "ci_95_ub",
                            title = "Evidence Synthesis: SCCS Meta-Analysis")
      })
    })

    output$prediction_plot <- plotly::renderPlotly({
      data <- page_data()
      if (is.null(data) || is.null(data$plp_results)) {
        return(.plotly_no_data("No prediction results available"))
      }
      .safe_plotly({
        df <- data$plp_results
        if (!"auc" %in% names(df)) return(.plotly_no_data("AUC column not found"))
        df$auc <- as.numeric(df$auc)
        df <- df[!is.na(df$auc), , drop = FALSE]
        if (nrow(df) == 0) return(.plotly_no_data("Prediction data empty", ""))
        label <- if ("model_design_id" %in% names(df)) {
          paste("Model", df$model_design_id)
        } else paste("Row", seq_len(nrow(df)))
        has_auprc <- "auprc" %in% names(df) && any(!is.na(df$auprc))
        p <- plotly::plot_ly(x = label, y = df$auc, type = "bar",
                              name = "AUC",
                              marker = list(color = .studio_colors[1]),
                              hovertemplate = "<b>%{x}</b><br>AUC: %{y:.3f}<extra></extra>")
        if (has_auprc) {
          p <- plotly::add_trace(p, y = as.numeric(df$auprc), name = "AUPRC",
                                  marker = list(color = .studio_colors[3]))
        }
        p |> plotly::layout(barmode = "group",
                             xaxis = list(title = ""),
                             yaxis = list(title = "Performance", range = c(0, 1))) |>
          .plotly_defaults("Model Performance")
      })
    })

    # ---- DT table outputs ----

    output$dqd_results_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$results)) return(NULL)
      df <- data$results
      display_cols <- intersect(
        c("check_name", "check_type", "cdm_table_name",
          "cdm_field_name", "num_violated_rows", "num_denominator_rows",
          "failed", "threshold_value"),
        names(df))
      if (length(display_cols) == 0) return(NULL)
      DT::datatable(df[, display_cols, drop = FALSE],
                    rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp",
                                   scrollX = TRUE))
    })

    output$cohort_results_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$cohort_counts)) return(NULL)
      DT::datatable(data$cohort_counts, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp",
                                   scrollX = TRUE))
    })

    output$incidence_results_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$incidence)) return(NULL)
      DT::datatable(data$incidence, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp",
                                   scrollX = TRUE))
    })

    output$char_results_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$covariates)) return(NULL)
      DT::datatable(data$covariates, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp",
                                   scrollX = TRUE))
    })

    # ---- New page DT outputs ----

    output$index_events_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$index_events)) return(NULL)
      DT::datatable(data$index_events, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$visit_context_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$visit_context)) return(NULL)
      DT::datatable(data$visit_context, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$temporal_cov_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$temporal_covariates)) return(NULL)
      DT::datatable(data$temporal_covariates, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$time_series_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$time_series)) return(NULL)
      DT::datatable(data$time_series, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$orphan_concepts_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$orphan_concepts)) return(NULL)
      DT::datatable(data$orphan_concepts, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$continuous_cov_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$continuous_covariates)) return(NULL)
      DT::datatable(data$continuous_covariates, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$time_to_event_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$time_to_event)) return(NULL)
      DT::datatable(data$time_to_event, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$dechallenge_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$dechallenge)) return(NULL)
      DT::datatable(data$dechallenge, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$estimation_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$cm_results)) return(NULL)
      DT::datatable(data$cm_results, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$self_controlled_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$sccs_results)) return(NULL)
      DT::datatable(data$sccs_results, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })

    output$prediction_table <- DT::renderDT({
      data <- page_data()
      if (is.null(data) || is.null(data$plp_results)) return(NULL)
      DT::datatable(data$plp_results, rownames = FALSE,
                    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
    })
  })
}

# ==============================================================================
# Dispatch
# ==============================================================================

.ohdsi_dispatch_fetch <- function(nav, state, scope, policy, selected_srv) {
  switch(nav,
    "Overview"              = .ohdsi_fetch_overview(state),
    "Data Quality"          = .ohdsi_fetch_dqd(state, scope, policy, selected_srv),
    "Cohort Counts"         = .ohdsi_fetch_cohort_counts(state, scope, policy,
                                                          selected_srv),
    "Incidence"             = .ohdsi_fetch_incidence(state, scope, policy,
                                                      selected_srv),
    "Characterization"      = .ohdsi_fetch_characterization(state, scope, policy,
                                                              selected_srv),
    "Index Events"          = .ohdsi_fetch_index_events(state, scope, policy,
                                                         selected_srv),
    "Visit Context"         = .ohdsi_fetch_visit_context(state, scope, policy,
                                                          selected_srv),
    "Temporal Covariates"   = .ohdsi_fetch_temporal_covariates(state, scope, policy,
                                                                selected_srv),
    "Time Series"           = .ohdsi_fetch_time_series(state, scope, policy,
                                                        selected_srv),
    "Concept Coverage"      = .ohdsi_fetch_concept_coverage(state, scope, policy,
                                                              selected_srv),
    "Continuous Covariates" = .ohdsi_fetch_continuous_covariates(state, scope, policy,
                                                                  selected_srv),
    "Time to Event"         = .ohdsi_fetch_time_to_event(state, scope, policy,
                                                          selected_srv),
    "Dechallenge/Rechallenge" = .ohdsi_fetch_dechallenge(state, scope, policy,
                                                           selected_srv),
    "Estimation"            = .ohdsi_fetch_estimation(state, scope, policy,
                                                       selected_srv),
    "Self-Controlled"       = .ohdsi_fetch_self_controlled(state, scope, policy,
                                                            selected_srv),
    "Prediction"            = .ohdsi_fetch_prediction(state, scope, policy,
                                                       selected_srv),
    list(no_data = TRUE)
  )
}

# ==============================================================================
# Fetch functions
# ==============================================================================

.ohdsi_fetch_overview <- function(state) {
  status <- tryCatch(
    ds.omop.ohdsi.status(symbol = state$symbol),
    error = function(e) NULL)
  if (is.null(status)) return(list(no_data = TRUE))
  .ohdsi_accumulate_code(state, status)

  # Also get table catalog for row counts
  tables_res <- tryCatch({
    r <- ds.omop.ohdsi.tables(symbol = state$symbol)
    .ohdsi_accumulate_code(state, r)
    r
  }, error = function(e) NULL)

  tool_names <- list(
    dqd = "Data Quality Dashboard",
    cohort_diagnostics = "CohortDiagnostics",
    cohort_incidence = "CohortIncidence",
    characterization = "Characterization",
    cohort_method = "CohortMethod",
    sccs = "Self-Controlled Case Series",
    plp = "Patient-Level Prediction",
    evidence_synthesis = "Evidence Synthesis"
  )
  tool_icons <- list(
    dqd = "clipboard-check", cohort_diagnostics = "users",
    cohort_incidence = "chart-line", characterization = "microscope",
    cohort_method = "balance-scale", sccs = "clock-rotate-left",
    plp = "brain", evidence_synthesis = "layer-group"
  )
  tool_themes <- list(
    dqd = "success", cohort_diagnostics = "primary",
    cohort_incidence = "info", characterization = "warning",
    cohort_method = "danger", sccs = "purple",
    plp = "dark", evidence_synthesis = "secondary"
  )

  tools_summary <- list()
  for (tid in names(tool_names)) {
    per_srv_avail <- list()
    total_rows <- 0L
    n_tables <- 0L
    for (srv in names(status$per_site)) {
      srv_status <- status$per_site[[srv]]
      avail <- !is.null(srv_status[[tid]]) &&
               isTRUE(srv_status[[tid]]$available)
      per_srv_avail[[srv]] <- avail
      if (avail && !is.null(srv_status[[tid]]$total_rows)) {
        total_rows <- total_rows + srv_status[[tid]]$total_rows
      }
      if (avail && !is.null(srv_status[[tid]]$n_tables)) {
        n_tables <- max(n_tables, srv_status[[tid]]$n_tables)
      }
    }
    tools_summary[[tid]] <- list(
      name = tool_names[[tid]],
      icon = tool_icons[[tid]],
      theme = tool_themes[[tid]],
      available = any(vapply(per_srv_avail, isTRUE, logical(1))),
      per_server = per_srv_avail,
      n_tables = n_tables,
      total_rows = total_rows
    )
  }

  list(tools_summary = tools_summary, tables_res = tables_res)
}

.ohdsi_fetch_dqd <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  res <- tryCatch(
    ds.omop.ohdsi.results(
      "dqdashboard_results",
      scope = api_scope, pooling_policy = policy,
      symbol = state$symbol),
    error = function(e) NULL)

  if (!is.null(res)) .ohdsi_accumulate_code(state, res)

  srv <- selected_srv %||% if (!is.null(res)) names(res$per_site)[1]
  df <- .ohdsi_pick_result(res, scope, srv)

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(no_data = TRUE, tool_missing = "dqd"))
  }

  # Compute pass/fail (with NA guards)
  df$failed <- if ("failed" %in% names(df)) {
    f <- as.logical(df$failed)
    ifelse(is.na(f), FALSE, f)
  } else if ("num_violated_rows" %in% names(df) &&
             "num_denominator_rows" %in% names(df)) {
    viol <- as.numeric(df$num_violated_rows)
    denom <- as.numeric(df$num_denominator_rows)
    thresh <- if ("threshold_value" %in% names(df)) {
      t <- as.numeric(df$threshold_value)
      ifelse(is.na(t), 0, t)
    } else rep(0, nrow(df))
    ifelse(is.na(denom) | denom == 0, FALSE,
           ifelse(is.na(viol), FALSE, (viol / denom) > thresh))
  } else {
    rep(FALSE, nrow(df))
  }

  # Ensure check_type column exists
  if (!"check_type" %in% names(df)) {
    if ("category" %in% names(df)) {
      df$check_type <- df$category
    } else {
      df$check_type <- "Unknown"
    }
  }

  total_checks <- nrow(df)
  n_pass <- sum(!df$failed, na.rm = TRUE)
  pass_rate <- if (total_checks > 0) round(n_pass / total_checks * 100, 1) else 0
  n_categories <- length(unique(df$check_type))

  list(
    results = df,
    total_checks = total_checks,
    n_pass = n_pass,
    pass_rate = pass_rate,
    n_categories = n_categories
  )
}

.ohdsi_fetch_cohort_counts <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  res <- tryCatch(
    ds.omop.ohdsi.results(
      "cohort_count",
      scope = api_scope, pooling_policy = policy,
      symbol = state$symbol),
    error = function(e) NULL)

  if (!is.null(res)) .ohdsi_accumulate_code(state, res)

  srv <- selected_srv %||% if (!is.null(res)) names(res$per_site)[1]
  df <- .ohdsi_pick_result(res, scope, srv)

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(no_data = TRUE, tool_missing = "cohort_diagnostics"))
  }

  subj_col <- intersect(c("cohort_subjects", "cohort_entries"), names(df))
  total_subjects <- if (length(subj_col) > 0) {
    sum(as.numeric(df[[subj_col[1]]]), na.rm = TRUE)
  } else NA
  n_cohorts <- if ("cohort_id" %in% names(df)) {
    length(unique(df$cohort_id))
  } else nrow(df)

  list(
    cohort_counts = df,
    total_subjects = total_subjects,
    n_cohorts = n_cohorts
  )
}

.ohdsi_fetch_incidence <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  # Try incidence_summary first (CohortIncidence), fall back to incidence_rate
  inc_res <- NULL
  for (tbl in c("incidence_summary", "incidence_rate")) {
    inc_res <- tryCatch(
      ds.omop.ohdsi.results(
        tbl,
        scope = api_scope, pooling_policy = policy,
        symbol = state$symbol),
      error = function(e) NULL)
    if (!is.null(inc_res)) {
      .ohdsi_accumulate_code(state, inc_res)
      break
    }
  }

  srv <- selected_srv %||% if (!is.null(inc_res)) names(inc_res$per_site)[1]
  df <- .ohdsi_pick_result(inc_res, scope, srv)

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(no_data = TRUE, tool_missing = "cohort_incidence"))
  }

  # Count records
  n_records <- nrow(df)

  list(
    incidence = df,
    n_records = n_records
  )
}

.ohdsi_fetch_characterization <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  # Fetch cohort counts
  counts_res <- tryCatch(
    ds.omop.ohdsi.results(
      "c_cohort_counts",
      scope = api_scope, pooling_policy = policy,
      symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(counts_res)) .ohdsi_accumulate_code(state, counts_res)

  # Fetch covariates
  cov_res <- tryCatch(
    ds.omop.ohdsi.results(
      "c_covariates",
      scope = api_scope, pooling_policy = policy,
      symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(cov_res)) .ohdsi_accumulate_code(state, cov_res)

  srv <- selected_srv %||%
    if (!is.null(cov_res)) names(cov_res$per_site)[1] else
    if (!is.null(counts_res)) names(counts_res$per_site)[1]

  counts_df <- .ohdsi_pick_result(counts_res, scope, srv)
  cov_df <- .ohdsi_pick_result(cov_res, scope, srv)

  has_counts <- is.data.frame(counts_df) && nrow(counts_df) > 0
  has_covariates <- is.data.frame(cov_df) && nrow(cov_df) > 0

  if (!has_counts && !has_covariates) {
    return(list(no_data = TRUE, tool_missing = "characterization"))
  }

  n_cohorts <- if (has_counts && "cohort_id" %in% names(counts_df)) {
    length(unique(counts_df$cohort_id))
  } else 0
  n_covariates <- if (has_covariates) nrow(cov_df) else 0

  list(
    cohort_counts = counts_df,
    covariates = cov_df,
    n_cohorts = n_cohorts,
    n_covariates = n_covariates
  )
}

# ==============================================================================
# Render functions
# ==============================================================================

.ohdsi_render_overview <- function(ns, data) {
  if (isTRUE(data$no_data) || is.null(data$tools_summary)) {
    return(.empty_state_ui("database", "OHDSI Overview Not Available",
      "Could not load tool status information. Check server connections."))
  }
  tools <- data$tools_summary

  tool_cards <- lapply(names(tools), function(tid) {
    tool <- tools[[tid]]
    # Per-server badges
    srv_badges <- lapply(names(tool$per_server), function(srv) {
      avail <- tool$per_server[[srv]]
      cls <- if (avail) "server-badge server-badge-ok"
             else "server-badge server-badge-err"
      shiny::span(class = cls, srv)
    })

    bslib::value_box(
      title = tool$name,
      value = if (tool$available) "Available" else "Not Found",
      showcase = fontawesome::fa_i(tool$icon),
      theme = if (tool$available) tool$theme else "secondary",
      p = shiny::tagList(
        if (tool$available)
          shiny::span(class = "text-muted small",
                      paste0(tool$n_tables, " tables, ",
                             format(tool$total_rows, big.mark = ","),
                             " rows")),
        shiny::div(class = "mt-1", srv_badges)
      )
    )
  })

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      !!!tool_cards
    )
  )
}

.ohdsi_render_dqd <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Data Quality Dashboard",
      "Run the OHDSI Data Quality Dashboard on your CDM to populate these tables."))
  }

  shiny::tagList(
    # KPI row
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      bslib::value_box(
        title = "Pass Rate", value = paste0(data$pass_rate, "%"),
        showcase = fontawesome::fa_i("check-circle"),
        theme = if (data$pass_rate >= 80) "success" else "warning"
      ),
      bslib::value_box(
        title = "Total Checks", value = format(data$total_checks, big.mark = ","),
        showcase = fontawesome::fa_i("list-check"), theme = "primary"
      ),
      bslib::value_box(
        title = "Passed", value = format(data$n_pass, big.mark = ","),
        showcase = fontawesome::fa_i("circle-check"), theme = "success"
      ),
      bslib::value_box(
        title = "Categories", value = data$n_categories,
        showcase = fontawesome::fa_i("tags"), theme = "info"
      )
    ),
    # Chart + table
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Checks by Category"),
        bslib::card_body(
          plotly::plotlyOutput(ns("dqd_category_plot"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Check Results"),
        bslib::card_body(
          DT::DTOutput(ns("dqd_results_table"))
        )
      )
    )
  )
}

.ohdsi_render_cohort_counts <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate these tables."))
  }

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Cohorts", value = format(data$n_cohorts, big.mark = ","),
        showcase = fontawesome::fa_i("users"), theme = "primary"
      ),
      bslib::value_box(
        title = "Total Subjects",
        value = .fmt_count(data$total_subjects),
        showcase = fontawesome::fa_i("user-group"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Cohort Sizes"),
        bslib::card_body(
          plotly::plotlyOutput(ns("cohort_bar_plot"), height = "400px")
        )
      ),
      bslib::card(
        bslib::card_header("Cohort Details"),
        bslib::card_body(
          DT::DTOutput(ns("cohort_results_table"))
        )
      )
    )
  )
}

.ohdsi_render_incidence <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortIncidence",
      "Run OHDSI CohortIncidence on your CDM to populate these tables."))
  }

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Records", value = format(data$n_records, big.mark = ","),
        showcase = fontawesome::fa_i("chart-line"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Incidence Rates"),
        bslib::card_body(
          plotly::plotlyOutput(ns("incidence_bar_plot"), height = "400px")
        )
      ),
      bslib::card(
        bslib::card_header("Incidence Details"),
        bslib::card_body(
          DT::DTOutput(ns("incidence_results_table"))
        )
      )
    )
  )
}

.ohdsi_render_characterization <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Characterization",
      "Run OHDSI Characterization on your CDM to populate these tables."))
  }

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Cohorts", value = format(data$n_cohorts, big.mark = ","),
        showcase = fontawesome::fa_i("users"), theme = "primary"
      ),
      bslib::value_box(
        title = "Covariates", value = format(data$n_covariates, big.mark = ","),
        showcase = fontawesome::fa_i("microscope"), theme = "warning"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header("Top Covariates"),
        bslib::card_body(
          plotly::plotlyOutput(ns("char_covariate_plot"), height = "400px")
        )
      ),
      bslib::card(
        bslib::card_header("Covariate Details"),
        bslib::card_body(
          DT::DTOutput(ns("char_results_table"))
        )
      )
    )
  )
}

# ==============================================================================
# New Fetch functions
# ==============================================================================

.ohdsi_fetch_single_table <- function(state, table_name, scope, policy,
                                       selected_srv, tool_missing_id) {
  api_scope <- .backend_scope(scope)
  res <- tryCatch(
    ds.omop.ohdsi.results(table_name, scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(res)) .ohdsi_accumulate_code(state, res)
  srv <- selected_srv %||% if (!is.null(res)) names(res$per_site)[1]
  df <- .ohdsi_pick_result(res, scope, srv)
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(list(no_data = TRUE, tool_missing = tool_missing_id))
  }
  df
}

.ohdsi_fetch_index_events <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "index_event_breakdown", scope,
                                    policy, selected_srv, "cohort_diagnostics")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  n_concepts <- if ("concept_id" %in% names(df)) length(unique(df$concept_id)) else nrow(df)
  list(index_events = df, n_concepts = n_concepts)
}

.ohdsi_fetch_visit_context <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "visit_context", scope,
                                    policy, selected_srv, "cohort_diagnostics")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  n_visit_types <- if ("visit_concept_id" %in% names(df)) {
    length(unique(df$visit_concept_id))
  } else nrow(df)
  list(visit_context = df, n_visit_types = n_visit_types)
}

.ohdsi_fetch_temporal_covariates <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "temporal_covariate_value", scope,
                                    policy, selected_srv, "cohort_diagnostics")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  name_col <- intersect(c("covariate_name", "covariate_id"), names(df))
  n_covariates <- if (length(name_col) > 0) length(unique(df[[name_col[1]]])) else 0
  list(temporal_covariates = df, n_covariates = n_covariates)
}

.ohdsi_fetch_time_series <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "time_series", scope,
                                    policy, selected_srv, "cohort_diagnostics")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  n_records <- nrow(df)
  list(time_series = df, n_records = n_records)
}

.ohdsi_fetch_concept_coverage <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  inc_res <- tryCatch(
    ds.omop.ohdsi.results("included_source_concept", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(inc_res)) .ohdsi_accumulate_code(state, inc_res)

  orp_res <- tryCatch(
    ds.omop.ohdsi.results("orphan_concept", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(orp_res)) .ohdsi_accumulate_code(state, orp_res)

  srv <- selected_srv %||%
    if (!is.null(inc_res)) names(inc_res$per_site)[1] else
    if (!is.null(orp_res)) names(orp_res$per_site)[1]

  inc_df <- .ohdsi_pick_result(inc_res, scope, srv)
  orp_df <- .ohdsi_pick_result(orp_res, scope, srv)

  has_inc <- is.data.frame(inc_df) && nrow(inc_df) > 0
  has_orp <- is.data.frame(orp_df) && nrow(orp_df) > 0

  if (!has_inc && !has_orp) {
    return(list(no_data = TRUE, tool_missing = "cohort_diagnostics"))
  }

  n_included <- if (has_inc) nrow(inc_df) else 0
  n_orphans <- if (has_orp) nrow(orp_df) else 0

  list(
    included_concepts = inc_df, orphan_concepts = orp_df,
    n_included = n_included, n_orphans = n_orphans
  )
}

.ohdsi_fetch_continuous_covariates <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "c_covariates_continuous", scope,
                                    policy, selected_srv, "characterization")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  n_covariates <- nrow(df)
  list(continuous_covariates = df, n_covariates = n_covariates)
}

.ohdsi_fetch_time_to_event <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "c_time_to_event", scope,
                                    policy, selected_srv, "characterization")
  if (is.list(df) && isTRUE(df$no_data)) return(df)
  n_outcomes <- if ("outcome_id" %in% names(df)) length(unique(df$outcome_id)) else 0
  list(time_to_event = df, n_outcomes = n_outcomes)
}

.ohdsi_fetch_dechallenge <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "c_dechallenge_rechallenge", scope,
                                    policy, selected_srv, "characterization")
  if (is.list(df) && isTRUE(df$no_data)) return(df)

  .safe_sum <- function(col) if (col %in% names(df)) sum(as.numeric(df[[col]]), na.rm = TRUE) else 0
  total_cases <- .safe_sum("num_cases")
  total_dech <- .safe_sum("num_dechallenge_attempt")
  total_dech_success <- .safe_sum("num_dechallenge_success")
  dech_rate <- if (total_dech > 0) round(total_dech_success / total_dech * 100, 1) else 0

  list(
    dechallenge = df, total_cases = total_cases,
    total_dech = total_dech, dech_rate = dech_rate
  )
}

.ohdsi_fetch_estimation <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  cm_res <- tryCatch(
    ds.omop.ohdsi.results("cm_result", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(cm_res)) .ohdsi_accumulate_code(state, cm_res)

  # Optionally fetch evidence synthesis meta-analysis
  es_res <- tryCatch(
    ds.omop.ohdsi.results("es_cm_result", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(es_res)) .ohdsi_accumulate_code(state, es_res)

  srv <- selected_srv %||%
    if (!is.null(cm_res)) names(cm_res$per_site)[1]

  cm_df <- .ohdsi_pick_result(cm_res, scope, srv)
  es_df <- .ohdsi_pick_result(es_res, scope, srv)

  has_cm <- is.data.frame(cm_df) && nrow(cm_df) > 0

  if (!has_cm) {
    return(list(no_data = TRUE, tool_missing = "cohort_method"))
  }

  n_analyses <- if ("analysis_id" %in% names(cm_df)) length(unique(cm_df$analysis_id)) else 0
  n_outcomes <- if ("outcome_id" %in% names(cm_df)) length(unique(cm_df$outcome_id)) else 0
  n_sig <- sum(as.numeric(cm_df$p) < 0.05, na.rm = TRUE)
  has_es <- is.data.frame(es_df) && nrow(es_df) > 0

  list(
    cm_results = cm_df,
    es_cm_results = if (has_es) es_df else NULL,
    n_analyses = n_analyses, n_outcomes = n_outcomes,
    n_significant = n_sig, has_meta = has_es
  )
}

.ohdsi_fetch_self_controlled <- function(state, scope, policy, selected_srv) {
  api_scope <- .backend_scope(scope)

  sccs_res <- tryCatch(
    ds.omop.ohdsi.results("sccs_result", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(sccs_res)) .ohdsi_accumulate_code(state, sccs_res)

  es_res <- tryCatch(
    ds.omop.ohdsi.results("es_sccs_result", scope = api_scope,
                           pooling_policy = policy, symbol = state$symbol),
    error = function(e) NULL)
  if (!is.null(es_res)) .ohdsi_accumulate_code(state, es_res)

  srv <- selected_srv %||%
    if (!is.null(sccs_res)) names(sccs_res$per_site)[1]

  sccs_df <- .ohdsi_pick_result(sccs_res, scope, srv)
  es_df <- .ohdsi_pick_result(es_res, scope, srv)

  has_sccs <- is.data.frame(sccs_df) && nrow(sccs_df) > 0

  if (!has_sccs) {
    return(list(no_data = TRUE, tool_missing = "sccs"))
  }

  n_analyses <- if ("analysis_id" %in% names(sccs_df)) length(unique(sccs_df$analysis_id)) else 0
  n_outcomes <- if ("exposures_outcome_set_id" %in% names(sccs_df)) {
    length(unique(sccs_df$exposures_outcome_set_id))
  } else 0
  has_es <- is.data.frame(es_df) && nrow(es_df) > 0

  list(
    sccs_results = sccs_df,
    es_sccs_results = if (has_es) es_df else NULL,
    n_analyses = n_analyses, n_outcomes = n_outcomes, has_meta = has_es
  )
}

.ohdsi_fetch_prediction <- function(state, scope, policy, selected_srv) {
  df <- .ohdsi_fetch_single_table(state, "plp_performances", scope,
                                    policy, selected_srv, "plp")
  if (is.list(df) && isTRUE(df$no_data)) return(df)

  n_models <- if ("model_design_id" %in% names(df)) length(unique(df$model_design_id)) else nrow(df)

  auc_vals <- if ("auc" %in% names(df)) as.numeric(df$auc) else numeric(0)
  auc_valid <- auc_vals[!is.na(auc_vals)]
  best_auc <- if (length(auc_valid) > 0) max(auc_valid) else NA_real_

  best_model <- if ("model_design_id" %in% names(df) && length(auc_valid) > 0) {
    idx <- which.max(auc_vals)
    if (length(idx) > 0) paste("Model", df$model_design_id[idx]) else "N/A"
  } else "N/A"

  list(
    plp_results = df, n_models = n_models,
    best_auc = if (!is.na(best_auc)) round(best_auc, 3) else NA,
    best_model = best_model
  )
}

# ==============================================================================
# New Render functions
# ==============================================================================

.ohdsi_render_index_events <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate index event data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Concepts", value = format(data$n_concepts, big.mark = ","),
        showcase = fontawesome::fa_i("tags"), theme = "primary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Top Index Event Concepts"),
        bslib::card_body(plotly::plotlyOutput(ns("index_events_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Index Event Details"),
        bslib::card_body(DT::DTOutput(ns("index_events_table"))))
    )
  )
}

.ohdsi_render_visit_context <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate visit context data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Visit Types", value = data$n_visit_types,
        showcase = fontawesome::fa_i("hospital"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Visit Context"),
        bslib::card_body(plotly::plotlyOutput(ns("visit_context_plot"), height = "350px"))),
      bslib::card(bslib::card_header("Visit Context Details"),
        bslib::card_body(DT::DTOutput(ns("visit_context_table"))))
    )
  )
}

.ohdsi_render_temporal_covariates <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate temporal covariate data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Covariates", value = data$n_covariates,
        showcase = fontawesome::fa_i("chart-line"), theme = "primary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Temporal Covariate Trends"),
        bslib::card_body(plotly::plotlyOutput(ns("temporal_cov_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Temporal Covariate Values"),
        bslib::card_body(DT::DTOutput(ns("temporal_cov_table"))))
    )
  )
}

.ohdsi_render_time_series <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate time series data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Records", value = format(data$n_records, big.mark = ","),
        showcase = fontawesome::fa_i("calendar"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Cohort Time Series"),
        bslib::card_body(plotly::plotlyOutput(ns("time_series_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Time Series Data"),
        bslib::card_body(DT::DTOutput(ns("time_series_table"))))
    )
  )
}

.ohdsi_render_concept_coverage <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortDiagnostics",
      "Run OHDSI CohortDiagnostics on your CDM to populate concept coverage data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Included Concepts", value = format(data$n_included, big.mark = ","),
        showcase = fontawesome::fa_i("check"), theme = "success"
      ),
      bslib::value_box(
        title = "Orphan Concepts", value = format(data$n_orphans, big.mark = ","),
        showcase = fontawesome::fa_i("triangle-exclamation"),
        theme = if (data$n_orphans > 0) "warning" else "success"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Included Source Concepts"),
        bslib::card_body(plotly::plotlyOutput(ns("included_concepts_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Orphan Concepts"),
        bslib::card_body(DT::DTOutput(ns("orphan_concepts_table"))))
    )
  )
}

.ohdsi_render_continuous_covariates <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Characterization",
      "Run OHDSI Characterization on your CDM to populate continuous covariate data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Covariates", value = format(data$n_covariates, big.mark = ","),
        showcase = fontawesome::fa_i("chart-bar"), theme = "warning"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Covariate Distributions"),
        bslib::card_body(plotly::plotlyOutput(ns("continuous_cov_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Continuous Covariate Details"),
        bslib::card_body(DT::DTOutput(ns("continuous_cov_table"))))
    )
  )
}

.ohdsi_render_time_to_event <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Characterization",
      "Run OHDSI Characterization on your CDM to populate time-to-event data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Outcomes", value = data$n_outcomes,
        showcase = fontawesome::fa_i("clock"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Time to Event Curves"),
        bslib::card_body(plotly::plotlyOutput(ns("time_to_event_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Time to Event Data"),
        bslib::card_body(DT::DTOutput(ns("time_to_event_table"))))
    )
  )
}

.ohdsi_render_dechallenge <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Characterization",
      "Run OHDSI Characterization on your CDM to populate dechallenge/rechallenge data."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      bslib::value_box(
        title = "Cases", value = format(data$total_cases, big.mark = ","),
        showcase = fontawesome::fa_i("user-injured"), theme = "primary"
      ),
      bslib::value_box(
        title = "Dechallenge Attempts", value = format(data$total_dech, big.mark = ","),
        showcase = fontawesome::fa_i("stop"), theme = "info"
      ),
      bslib::value_box(
        title = "Dechallenge Success", value = paste0(data$dech_rate, "%"),
        showcase = fontawesome::fa_i("circle-check"),
        theme = if (data$dech_rate >= 50) "success" else "warning"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Success Rates"),
        bslib::card_body(plotly::plotlyOutput(ns("dechallenge_plot"), height = "350px"))),
      bslib::card(bslib::card_header("Dechallenge/Rechallenge Details"),
        bslib::card_body(DT::DTOutput(ns("dechallenge_table"))))
    )
  )
}

.ohdsi_render_estimation <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("CohortMethod",
      "Run OHDSI CohortMethod on your CDM to populate estimation results."))
  }

  meta_ui <- if (isTRUE(data$has_meta)) {
    bslib::card(bslib::card_header("Meta-Analysis (Evidence Synthesis)"),
      bslib::card_body(plotly::plotlyOutput(ns("estimation_meta_plot"), height = "300px")))
  } else NULL

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      bslib::value_box(
        title = "Analyses", value = data$n_analyses,
        showcase = fontawesome::fa_i("flask"), theme = "primary"
      ),
      bslib::value_box(
        title = "Outcomes", value = data$n_outcomes,
        showcase = fontawesome::fa_i("bullseye"), theme = "info"
      ),
      bslib::value_box(
        title = "Significant (p<0.05)", value = data$n_significant,
        showcase = fontawesome::fa_i("star"),
        theme = if (data$n_significant > 0) "danger" else "secondary"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Forest Plot: Hazard Ratios"),
        bslib::card_body(plotly::plotlyOutput(ns("estimation_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Estimation Results"),
        bslib::card_body(DT::DTOutput(ns("estimation_table"))))
    ),
    meta_ui
  )
}

.ohdsi_render_self_controlled <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Self-Controlled Case Series",
      "Run OHDSI SCCS on your CDM to populate self-controlled case series results."))
  }

  meta_ui <- if (isTRUE(data$has_meta)) {
    bslib::card(bslib::card_header("Meta-Analysis (Evidence Synthesis)"),
      bslib::card_body(plotly::plotlyOutput(ns("self_controlled_meta_plot"), height = "300px")))
  } else NULL

  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/3, fill = FALSE,
      bslib::value_box(
        title = "Analyses", value = data$n_analyses,
        showcase = fontawesome::fa_i("flask"), theme = "primary"
      ),
      bslib::value_box(
        title = "Outcome Sets", value = data$n_outcomes,
        showcase = fontawesome::fa_i("bullseye"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(bslib::card_header("Forest Plot: Incidence Rate Ratios"),
        bslib::card_body(plotly::plotlyOutput(ns("self_controlled_plot"), height = "400px"))),
      bslib::card(bslib::card_header("SCCS Results"),
        bslib::card_body(DT::DTOutput(ns("self_controlled_table"))))
    ),
    meta_ui
  )
}

.ohdsi_render_prediction <- function(ns, data) {
  if (isTRUE(data$no_data)) {
    return(.ohdsi_empty_tool("Patient-Level Prediction",
      "Run OHDSI PLP on your CDM to populate prediction performance results."))
  }
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1/4, fill = FALSE,
      bslib::value_box(
        title = "Models", value = data$n_models,
        showcase = fontawesome::fa_i("brain"), theme = "primary"
      ),
      bslib::value_box(
        title = "Best AUC", value = if (is.na(data$best_auc)) "N/A" else data$best_auc,
        showcase = fontawesome::fa_i("trophy"),
        theme = if (!is.na(data$best_auc) && data$best_auc >= 0.8) "success" else "warning"
      ),
      bslib::value_box(
        title = "Best Model", value = data$best_model,
        showcase = fontawesome::fa_i("star"), theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(bslib::card_header("Model Performance"),
        bslib::card_body(plotly::plotlyOutput(ns("prediction_plot"), height = "400px"))),
      bslib::card(bslib::card_header("Performance Details"),
        bslib::card_body(DT::DTOutput(ns("prediction_table"))))
    )
  )
}

# ==============================================================================
# Helpers
# ==============================================================================

#' Forest plot helper for CM, SCCS, Evidence Synthesis
#' @keywords internal
.ohdsi_forest_plot <- function(df, estimate_col = "rr", lower_col = "ci_95_lb",
                                upper_col = "ci_95_ub", title = "Forest Plot") {
  if (!all(c(estimate_col, lower_col, upper_col) %in% names(df))) {
    return(.plotly_no_data("Expected columns not found for forest plot"))
  }
  df$estimate <- as.numeric(df[[estimate_col]])
  df$lower <- as.numeric(df[[lower_col]])
  df$upper <- as.numeric(df[[upper_col]])
  df <- df[!is.na(df$estimate) & !is.na(df$lower) & !is.na(df$upper), , drop = FALSE]
  # Filter non-positive values (log scale can't handle zero/negative)
  df <- df[df$estimate > 0 & df$lower > 0 & df$upper > 0, , drop = FALSE]
  if (nrow(df) == 0) return(.plotly_no_data("No valid estimates for forest plot", ""))

  # Build labels
  label_parts <- character(nrow(df))
  if ("analysis_id" %in% names(df)) label_parts <- paste0("A", df$analysis_id)
  if ("outcome_id" %in% names(df)) {
    label_parts <- paste0(label_parts, " O", df$outcome_id)
  } else if ("exposures_outcome_set_id" %in% names(df)) {
    label_parts <- paste0(label_parts, " EO", df$exposures_outcome_set_id)
  }
  if ("target_id" %in% names(df) && "comparator_id" %in% names(df)) {
    label_parts <- paste0(label_parts, " T", df$target_id, "vC", df$comparator_id)
  }
  if (all(nchar(label_parts) == 0)) label_parts <- paste("Row", seq_len(nrow(df)))
  df$label <- label_parts

  plotly::plot_ly(df, y = ~label, x = ~estimate, type = "scatter",
                  mode = "markers",
                  marker = list(size = 8, color = .studio_colors[1]),
                  error_x = list(type = "data",
                                 symmetric = FALSE,
                                 arrayminus = pmax(0, df$estimate - df$lower),
                                 array = pmax(0, df$upper - df$estimate),
                                 color = .studio_colors[1]),
                  hovertemplate = paste0("<b>%{y}</b><br>",
                                         "Estimate: %{x:.3f}<br>",
                                         "CI: [", sprintf("%.3f", df$lower),
                                         ", ", sprintf("%.3f", df$upper), "]",
                                         "<extra></extra>")) |>
    plotly::layout(
      shapes = list(list(type = "line", x0 = 1, x1 = 1,
                          y0 = -0.5, y1 = nrow(df) - 0.5,
                          line = list(color = "grey", dash = "dash"))),
      xaxis = list(title = "Estimate (ratio scale)", type = "log"),
      yaxis = list(title = "", categoryorder = "trace")
    ) |>
    .plotly_defaults(title)
}

.ohdsi_empty_tool <- function(tool_name, message) {
  .empty_state_ui("database", paste(tool_name, "Results Not Found"), message)
}

.ohdsi_accumulate_code <- function(state, res) {
  code <- if (inherits(res, "dsomop_result")) res$meta$call_code else NULL
  if (!is.null(code) && is.character(code) && length(code) == 1 && nchar(code) > 0) {
    shiny::isolate({
      state$script_lines <- c(state$script_lines, code)
    })
  }
}

.ohdsi_pick_result <- function(res, scope, srv) {
  if (is.null(res)) return(NULL)
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
