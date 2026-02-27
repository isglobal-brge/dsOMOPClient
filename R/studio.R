# ==============================================================================
# dsOMOPClient v2 - OMOP Studio (Shiny App)
# ==============================================================================
# A concept-centric interactive exploration + plan authoring tool.
# Workflow: Browse -> Tick -> Build (basket-first paradigm).
# All data fetched via dsOMOPClient CLI -> DataSHIELD aggregate endpoints.
# ==============================================================================

#' Launch OMOP Studio
#'
#' Opens an interactive Shiny app for OMOP CDM exploration, vocabulary
#' browsing, concept drilldown analysis, cohort management, and plan
#' authoring. Requires an active DataSHIELD session created via
#' \code{\link{ds.omop.connect}}.
#'
#' @param symbol Character; OMOP session symbol (default "omop")
#' @param launch.browser Logical; open in browser (default TRUE)
#' @return A Shiny app object (runs interactively)
#' @export
ds.omop.studio <- function(symbol = "omop", launch.browser = TRUE) {
  # Eagerly capture the session BEFORE Shiny starts. This protects against

  # namespace reloads (devtools::load_all) that would recreate an empty
  # .dsomop_client_env, losing the registered session.
  captured_session <- .get_session(symbol)

  app <- shiny::shinyApp(
    ui = .studio_ui(symbol),
    server = .studio_server(symbol, captured_session)
  )
  shiny::runApp(app, launch.browser = launch.browser)
}

# ==============================================================================
# UI
# ==============================================================================

.studio_ui <- function(symbol) {
  function(request) {
    bslib::page_navbar(
      title = shiny::strong("dsOMOP Studio"),
      id = "main_nav",
      fillable = FALSE,
      theme = bslib::bs_theme(
        version = 5, bootswatch = "flatly",
        "navbar-bg" = "#2c3e50"
      ),
      header = shiny::tags$style(shiny::HTML("
        .card { margin-bottom: 1rem; }
        .card-body { overflow-x: auto; }
        .status-ok { color: #27ae60; font-weight: bold; }
        .status-warn { color: #f39c12; font-weight: bold; }
        .status-err { color: #e74c3c; font-weight: bold; }
        .code-output { background: #2d2d2d; color: #f8f8f2;
                       padding: 1em; border-radius: 6px;
                       font-family: 'Fira Code', monospace;
                       font-size: 0.85em; white-space: pre-wrap;
                       max-height: 500px; overflow-y: auto; }
        .metric-card { text-align: center; padding: 1.5em; }
        .metric-card .value { font-size: 2em; font-weight: bold;
                              color: #2c3e50; }
        .metric-card .label { color: #7f8c8d; font-size: 0.9em; }
        .suppressed { color: #e74c3c; font-style: italic; }
        .clickable-row { cursor: pointer; }
        .clickable-row:hover { background-color: #ecf0f1 !important; }
        .concept-badge { display: inline-block; padding: 0.3em 0.6em;
                         background: #3498db; color: white;
                         border-radius: 4px; font-size: 0.85em;
                         margin: 0.2em; }
        .server-badge { display: inline-block; padding: 0.15em 0.5em;
                        border-radius: 3px; font-size: 0.75em;
                        margin: 0.1em; font-weight: 600; }
        .server-badge-ok { background: #d5f5e3; color: #1e8449; }
        .server-badge-err { background: #fadbd8; color: #c0392b; }
        .scope-indicator { font-size: 0.8em; color: #7f8c8d;
                           font-style: italic; margin-top: 0.5em; }
        .cdm-info { font-size: 0.9em; }
        .cdm-info dt { font-weight: 600; color: #2c3e50; }
        .cart-item { padding: 0.5em; margin: 0.3em 0;
                     border: 1px solid #dee2e6; border-radius: 4px;
                     background: #fdfdfe; font-size: 0.85em; }
        .cart-item .cart-item-name { font-weight: 600; color: #2c3e50; }
        .cart-item .cart-item-meta { color: #7f8c8d; font-size: 0.85em; }
        .cart-section-header { font-weight: 600; font-size: 0.9em;
                               color: #2c3e50; border-bottom: 1px solid #dee2e6;
                               padding-bottom: 0.3em; margin-top: 0.8em; }
        .cart-badge { display: inline-block; padding: 0.15em 0.5em;
                      border-radius: 3px; font-size: 0.7em;
                      font-weight: 600; margin-left: 0.3em; }
        .cart-badge-var { background: #d4edda; color: #155724; }
        .cart-badge-filter { background: #fff3cd; color: #856404; }
        .cart-badge-output { background: #cce5ff; color: #004085; }
        .btn-quick-action { padding: 0.1em 0.4em; font-size: 0.7em;
                            margin: 0 0.1em; border-radius: 3px; }
        .r-comment { color: #6a9955; font-style: italic; }
        .r-string { color: #ce9178; }
        .r-keyword { color: #569cd6; font-weight: bold; }
        .r-number { color: #b5cea8; }
        .r-fn { color: #dcdcaa; }
        .r-operator { color: #d4d4d4; font-weight: bold; }
        .r-assign { color: #d4d4d4; }
        .code-output pre { margin: 0; background: transparent;
                           color: inherit; border: none; padding: 0; }
        .code-output code { font-family: inherit; color: inherit; }
        .code-output .shiny-text-output { background: transparent;
                           color: inherit; border: none; padding: 0; }
      ")),

      # --- Tab 1: Connections ---
      bslib::nav_panel("Connections", icon = shiny::icon("plug"),
        .mod_connections_ui("conn")
      ),

      # --- Tab 2: Explore ---
      bslib::nav_panel("Explore", icon = shiny::icon("compass"),
        .mod_table_concepts_ui("explore")
      ),

      # --- Tab 3: Drilldown ---
      bslib::nav_panel("Drilldown", icon = shiny::icon("magnifying-glass-chart"),
        .mod_concept_drilldown_ui("drilldown")
      ),

      # --- Tab 4: Locator ---
      bslib::nav_panel("Locator", icon = shiny::icon("location-dot"),
        .mod_concept_locator_ui("locator")
      ),

      # --- Tab 5: Vocabulary ---
      bslib::nav_panel("Vocabulary", icon = shiny::icon("book"),
        .mod_vocab_ui("vocab")
      ),

      # --- Tab 6: Data Sources (Atlas) ---
      bslib::nav_panel("Data Sources", icon = shiny::icon("chart-bar"),
        .mod_atlas_ui("atlas")
      ),

      # --- Tab 7: Queries ---
      bslib::nav_panel("Queries", icon = shiny::icon("book-open"),
        .mod_queries_ui("queries")
      ),

      # --- Tab 8: Basket / Cart ---
      bslib::nav_panel("Basket", icon = shiny::icon("cart-shopping"),
        .mod_basket_ui("basket")
      ),

      # --- Tab 9: Plan Builder ---
      bslib::nav_panel("Plan Builder", icon = shiny::icon("hammer"),
        .mod_plan_from_explorer_ui("plans")
      ),

      # --- Tab 10: Script ---
      bslib::nav_panel("Script", icon = shiny::icon("code"),
        .mod_script_builder_ui("script")
      ),

      # --- Tab 11: Session ---
      bslib::nav_panel("Session", icon = shiny::icon("server"),
        .mod_session_ui("session_tab")
      )
    )
  }
}

# ==============================================================================
# SERVER
# ==============================================================================

.studio_server <- function(symbol, captured_session = NULL) {
  function(input, output, session) {
    # Re-ensure the session is registered in .dsomop_client_env.
    # This is necessary because devtools::load_all or namespace reloads
    # can recreate the environment, losing previously registered sessions.
    if (!is.null(captured_session)) {
      if (!exists(symbol, envir = .dsomop_client_env)) {
        assign(symbol, captured_session, envir = .dsomop_client_env)
      }
    }

    # Shared reactive state
    state <- shiny::reactiveValues(
      symbol = symbol,
      status = NULL,
      tables = NULL,
      selected_table = NULL,
      selected_concept_col = NULL,
      selected_concept_id = NULL,
      selected_concept_name = NULL,
      concept_set = integer(0),
      cart = omop_cart(),
      plan = ds.omop.plan(),
      plan_outputs = list(),
      script_lines = character(0),
      scope = "per_site",
      pooling_policy = "strict",
      server_names = character(0)
    )

    # Load initial data on startup
    shiny::observe({
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
        if (!is.null(state$status$servers)) {
          state$server_names <- state$status$servers
        }
        state$tables <- ds.omop.tables(symbol = state$symbol)
      }, error = function(e) {
        shiny::showNotification(
          paste("Connection error:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # Module servers
    .mod_connections_server("conn", state)
    .mod_table_concepts_server("explore", state, session)
    .mod_concept_drilldown_server("drilldown", state)
    .mod_concept_locator_server("locator", state)
    .mod_vocab_server("vocab", state)
    .mod_atlas_server("atlas", state)
    .mod_queries_server("queries", state)
    .mod_basket_server("basket", state)
    .mod_plan_from_explorer_server("plans", state)
    .mod_script_builder_server("script", state)
    .mod_session_server("session_tab", state)
  }
}

# ==============================================================================
# Helper: extract CDM table names with has_person_id from state$tables
# ==============================================================================

.get_person_tables <- function(tables) {
  if (is.null(tables)) return(character(0))
  srv_name <- names(tables)[1]
  df <- tables[[srv_name]]
  if (!is.data.frame(df)) return(character(0))
  # Filter to CDM tables with has_person_id = TRUE
  mask <- rep(TRUE, nrow(df))
  if ("schema_category" %in% names(df)) {
    mask <- mask & (df$schema_category == "CDM")
  }
  if ("has_person_id" %in% names(df)) {
    mask <- mask & isTRUE_vec(df$has_person_id)
  }
  sort(df$table_name[mask])
}

# Vectorised isTRUE
isTRUE_vec <- function(x) {
  !is.na(x) & x == TRUE
}

# Helper: get concept columns from a table's column metadata
.get_concept_columns <- function(col_df) {
  if (is.null(col_df) || !is.data.frame(col_df)) return(character(0))
  if (!"concept_role" %in% names(col_df)) return(character(0))
  col_df$column_name[col_df$concept_role != "non_concept"]
}

# Utility: infer OMOP table from domain_id
.domain_to_table <- function(domain) {
  if (is.null(domain) || is.na(domain)) return("condition_occurrence")
  domain <- tolower(trimws(domain))
  switch(domain,
    condition = "condition_occurrence",
    drug = "drug_exposure",
    measurement = "measurement",
    procedure = "procedure_occurrence",
    observation = "observation",
    visit = "visit_occurrence",
    device = "device_exposure",
    "condition_occurrence"  # default fallback
  )
}

# Utility: format table name for display (condition_occurrence -> Condition Occurrence)
.format_table_name <- function(x) {
  vapply(x, function(nm) {
    words <- strsplit(tolower(nm), "_", fixed = TRUE)[[1]]
    paste(vapply(words, function(w) {
      paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w)))
    }, character(1)), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

# Utility: create named choices for selectInput (use raw table names)
.table_choices <- function(tables) {
  choices <- tables
  names(choices) <- tables
  choices
}

# Utility: parse comma-separated IDs
.parse_ids <- function(s) {
  if (is.null(s) || nchar(trimws(s)) == 0) return(NULL)
  ids <- trimws(strsplit(s, ",")[[1]])
  ids <- ids[nchar(ids) > 0]
  as.integer(ids)
}

# Utility: format number with suppression check
.fmt_count <- function(x) {
  if (is.null(x) || is.na(x)) {
    shiny::span(class = "suppressed", "suppressed")
  } else {
    format(x, big.mark = ",")
  }
}

# Safe plot wrapper: catches errors and resets graphics state
.safe_plot <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste("Plot unavailable:", conditionMessage(e)),
                     cex = 0.9, col = "#7f8c8d")
    }
  )
}

# Scope control UI (reusable across modules)
# show_pooled: whether to offer the "Pooled" radio option
.scope_controls_ui <- function(ns, default = "per_site", show_pooled = TRUE) {
  scope_choices <- c("All Servers" = "all", "Per Site" = "per_site")
  if (show_pooled) scope_choices <- c(scope_choices, "Pooled" = "pooled")
  shiny::tagList(
    shiny::hr(),
    shiny::h6("Data Scope"),
    shiny::radioButtons(ns("scope"), NULL,
      choices = scope_choices,
      selected = default, inline = TRUE),
    shiny::selectizeInput(ns("selected_server"), "Servers",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All servers")),
    shiny::checkboxInput(ns("intersect_only"),
      "Intersection only (common to selected)", FALSE),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("scope"), "'] == 'pooled'"),
      shiny::radioButtons(ns("pooling_policy"), "Pooling Policy",
        choices = c("Strict" = "strict", "Best Effort" = "pooled_only_ok"),
        selected = "strict", inline = TRUE)
    )
  )
}

# Simple server selector for modules that don't need full scope radio
# (session, drilldown, vocab)
.server_selector_ui <- function(ns) {
  shiny::tagList(
    shiny::hr(),
    shiny::selectizeInput(ns("selected_server"), "Servers",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "All servers")),
    shiny::checkboxInput(ns("intersect_only"),
      "Intersection only (common to selected)", FALSE)
  )
}

# Server dropdown synchroniser — called once per module server
.scope_sync_servers <- function(input, session, state) {
  shiny::observe({
    srvs <- state$server_names
    if (length(srvs) > 0) {
      shiny::updateSelectizeInput(session, "selected_server",
                                  choices = srvs, selected = srvs,
                                  server = FALSE)
    }
  })
}

# Map UI scope to backend scope ("all" → "per_site" since backend only knows per_site/pooled)
.backend_scope <- function(scope) {
  if (is.null(scope) || scope == "all") "per_site" else scope
}

# Resolve which server names to include based on selected_server input
# Returns character vector of server names
.resolve_servers <- function(per_site, selected_server) {
  all_srvs <- names(per_site)
  if (is.null(selected_server) || length(selected_server) == 0)
    return(all_srvs)
  intersect(selected_server, all_srvs)
}

# Utility: extract display data from dsomop_result or raw named list
# selected_server: character vector of selected server names (NULL = all)
# intersect_only: if TRUE and scope is "all", only keep rows whose key
#   (e.g. concept_id) appears in ALL selected servers
.extract_display_data <- function(res, scope, selected_server = NULL,
                                   intersect_only = FALSE,
                                   intersect_col = "concept_id",
                                   server_col = "server") {
  if (is.null(res)) return(NULL)
  per_site <- if (inherits(res, "dsomop_result")) res$per_site else res

  if (scope == "pooled" && inherits(res, "dsomop_result") &&
      !is.null(res$pooled) && is.data.frame(res$pooled)) {
    return(res$pooled)
  }

  srvs <- .resolve_servers(per_site, selected_server)
  if (length(srvs) == 0) return(NULL)

  if (scope == "per_site") {
    srv <- srvs[1]
    if (srv %in% names(per_site)) return(per_site[[srv]])
    return(NULL)
  }

  # scope == "all" (or any non-pooled, non-per_site)
  dfs <- list()
  for (nm in srvs) {
    df <- per_site[[nm]]
    if (is.data.frame(df) && nrow(df) > 0) {
      df[[server_col]] <- nm
      dfs[[nm]] <- df
    }
  }
  if (length(dfs) == 0) return(NULL)
  combined <- do.call(rbind, dfs)
  rownames(combined) <- NULL

  # Intersection filter: keep only rows with key present in ALL selected servers
  if (isTRUE(intersect_only) && length(dfs) > 1 &&
      intersect_col %in% names(combined)) {
    id_sets <- lapply(dfs, function(d) unique(d[[intersect_col]]))
    common_ids <- Reduce(intersect, id_sets)
    combined <- combined[combined[[intersect_col]] %in% common_ids, , drop = FALSE]
  }
  combined
}

