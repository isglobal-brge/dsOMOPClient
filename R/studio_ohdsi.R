# Module: Studio - OHDSI Results Consumer
# Shiny module for displaying pre-computed OHDSI tool result tables
# (DQD, CohortDiagnostics, CohortIncidence, Characterization).

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
          cohort_incidence = FALSE, characterization = FALSE
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

      pages <- "Overview"

      if (isTRUE(tool_avail$dqd)) {
        pages <- c(pages, "Data Quality")
      }
      if (isTRUE(tool_avail$cohort_diagnostics)) {
        pages <- c(pages, "Cohort Counts")
      }
      if (isTRUE(tool_avail$cohort_diagnostics) ||
          isTRUE(tool_avail$cohort_incidence)) {
        pages <- c(pages, "Incidence")
      }
      if (isTRUE(tool_avail$characterization)) {
        pages <- c(pages, "Characterization")
      }

      shiny::updateSelectInput(session, "ohdsi_nav", choices = pages)
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
                   paste0(n_tools, " of 4 tools detected"))
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
        "Overview"         = .ohdsi_render_overview(ns, data),
        "Data Quality"     = .ohdsi_render_dqd(ns, data),
        "Cohort Counts"    = .ohdsi_render_cohort_counts(ns, data),
        "Incidence"        = .ohdsi_render_incidence(ns, data),
        "Characterization" = .ohdsi_render_characterization(ns, data),
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
  })
}

# ==============================================================================
# Dispatch
# ==============================================================================

.ohdsi_dispatch_fetch <- function(nav, state, scope, policy, selected_srv) {
  switch(nav,
    "Overview"         = .ohdsi_fetch_overview(state),
    "Data Quality"     = .ohdsi_fetch_dqd(state, scope, policy, selected_srv),
    "Cohort Counts"    = .ohdsi_fetch_cohort_counts(state, scope, policy,
                                                     selected_srv),
    "Incidence"        = .ohdsi_fetch_incidence(state, scope, policy,
                                                 selected_srv),
    "Characterization" = .ohdsi_fetch_characterization(state, scope, policy,
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
    characterization = "Characterization"
  )
  tool_icons <- list(
    dqd = "clipboard-check", cohort_diagnostics = "users",
    cohort_incidence = "chart-line", characterization = "microscope"
  )
  tool_themes <- list(
    dqd = "success", cohort_diagnostics = "primary",
    cohort_incidence = "info", characterization = "warning"
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
      width = 1/2, fill = FALSE,
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
# Helpers
# ==============================================================================

.ohdsi_empty_tool <- function(tool_name, message) {
  .empty_state_ui("database", paste(tool_name, "Results Not Found"), message)
}

.ohdsi_accumulate_code <- function(state, res) {
  if (inherits(res, "dsomop_result") && nchar(res$meta$call_code) > 0) {
    shiny::isolate({
      state$script_lines <- c(state$script_lines, res$meta$call_code)
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
