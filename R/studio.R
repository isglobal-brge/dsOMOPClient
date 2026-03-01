# Module: OMOP Studio
# Main Shiny application for interactive OMOP CDM exploration.

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
#' @examples
#' \dontrun{
#' ds.omop.studio(session)
#' }
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

#' OMOP Studio UI
#'
#' @param symbol Character; OMOP session symbol.
#' @return A function returning a Shiny UI definition.
#' @keywords internal
.studio_ui <- function(symbol) {
  function(request) {
    shiny::addResourcePath("dsomop-assets",
      system.file("www", package = "dsOMOPClient"))
    bslib::page_navbar(
      title = shiny::tags$span(class = "d-inline-flex align-items-center gap-2",
        shiny::tags$img(src = "dsomop-assets/logo.png", height = "32px",
          style = "margin-right: 4px;"),
        shiny::span("dsOMOP", style = "font-weight:700;"),
        shiny::span("Studio", style = "font-weight:400; opacity:0.8;")),
      id = "main_nav",
      fillable = FALSE,
      theme = bslib::bs_theme(
        version = 5,
        bg = "#f8fafc", fg = "#1e293b",
        primary = "#2563eb",
        secondary = "#64748b",
        success = "#059669",
        info = "#0891b2",
        warning = "#d97706",
        danger = "#dc2626",
        "navbar-bg" = "#0f172a",
        "border-radius" = "0.625rem",
        "card-border-radius" = "0.75rem",
        "card-cap-bg" = "transparent",
        "card-border-color" = "rgba(0,0,0,0.06)",
        "enable-shadows" = TRUE,
        "input-border-color" = "#e2e8f0",
        "input-focus-border-color" = "#3b82f6",
        "input-border-radius" = "0.5rem",
        "btn-border-radius" = "0.5rem",
        "accordion-border-color" = "rgba(0,0,0,0.04)",
        "accordion-button-active-bg" = "rgba(37,99,235,0.04)",
        "accordion-button-active-color" = "#2563eb",
        "table-hover-bg" = "rgba(37,99,235,0.03)",
        base_font = bslib::font_google("Inter"),
        code_font = bslib::font_google("Fira Code")
      ),
      sidebar = .mod_sidebar_ui("sidebar"),

      header = shiny::tagList(
        shiny::tags$style(shiny::HTML(.studio_css())),
        # JS clipboard handler with execCommand fallback for RStudio viewer / HTTP
        shiny::tags$script(shiny::HTML("
          function showMiniToast(msg) {
            var el = document.createElement('div');
            el.className = 'mini-toast';
            el.innerHTML = '<i class=\"fa-solid fa-check me-1\"></i>' + msg;
            document.body.appendChild(el);
            setTimeout(function() { el.remove(); }, 2000);
          }
          Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
            if (navigator.clipboard && window.isSecureContext) {
              navigator.clipboard.writeText(text).then(function() {
                showMiniToast('Copied!');
              }).catch(function() { fallbackCopy(text); });
            } else { fallbackCopy(text); }
          });
          function fallbackCopy(text) {
            var ta = document.createElement('textarea');
            ta.value = text; ta.style.position = 'fixed'; ta.style.left = '-9999px';
            document.body.appendChild(ta); ta.select();
            try { document.execCommand('copy'); showMiniToast('Copied!');
            } catch(e) { showMiniToast('Copy failed'); }
            document.body.removeChild(ta);
          }
          Shiny.addCustomMessageHandler('showMiniToast', function(msg) {
            showMiniToast(msg);
          });
          Shiny.addCustomMessageHandler('toggleAchillesTab', function(data) {
            var tabs = document.querySelectorAll('#main_nav .nav-link');
            tabs.forEach(function(tab) {
              if (tab.textContent.trim().indexOf('Achilles') !== -1) {
                if (data.disabled) {
                  tab.classList.add('achilles-disabled');
                  tab.setAttribute('title', data.tooltip || 'Run OHDSI Achilles on your CDM to enable this tab');
                  tab.setAttribute('data-achilles-disabled', 'true');
                } else {
                  tab.classList.remove('achilles-disabled');
                  tab.removeAttribute('title');
                  tab.removeAttribute('data-achilles-disabled');
                }
              }
            });
          });
          $(document).on('click', '.achilles-disabled', function(e) {
            e.preventDefault(); e.stopPropagation(); return false;
          });
        "))
      ),

      # Push tabs to the right (title stays left)
      bslib::nav_spacer(),

      # --- Tab 1: Overview ---
      bslib::nav_panel("Overview", icon = shiny::icon("house"),
        .mod_overview_ui("overview")
      ),

      # --- Tab 2: Explore (consolidated: Prevalence, Drilldown, Locator, Vocabulary) ---
      bslib::nav_panel("Explore", icon = shiny::icon("compass"),
        .mod_explore_ui("explore")
      ),

      # --- Tab 3: Achilles ---
      bslib::nav_panel("Achilles", icon = shiny::icon("chart-bar"),
        value = "achilles_tab",
        .mod_atlas_ui("atlas")
      ),

      # --- Tab 4: Queries ---
      bslib::nav_panel("Queries", icon = shiny::icon("terminal"),
        .mod_queries_ui("queries")
      ),

      # --- Tab 5: Builder (consolidated: Builder + Plan) ---
      bslib::nav_panel("Builder", icon = shiny::icon("hammer"),
        .mod_build_ui("build")
      ),

      # --- Tab 6: Session (read-only log) ---
      bslib::nav_panel("Session", icon = shiny::icon("gear"),
        .mod_session_ui("session_log")
      ),

      # --- Dark Mode Toggle in navbar ---
      bslib::nav_item(bslib::input_dark_mode(id = "dark_mode"))
    )
  }
}

#' OMOP Studio Server
#'
#' @param symbol Character; OMOP session symbol.
#' @param captured_session Captured session object (protection against namespace reloads).
#' @return A Shiny server function.
#' @keywords internal
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
      recipe = omop_recipe(),
      plan = ds.omop.plan(),
      plan_outputs = list(),
      script_lines = character(0),
      scope = "pooled",
      pooling_policy = "strict",
      server_names = character(0),
      selected_servers = character(0),
      history = list()
    )

    # Load initial data on startup
    shiny::observe({
      tryCatch({
        state$status <- ds.omop.status(symbol = state$symbol)
        if (!is.null(state$status$servers)) {
          state$server_names <- state$status$servers
          state$selected_servers <- state$status$servers
        }
        state$tables <- ds.omop.tables(symbol = state$symbol)
      }, error = function(e) {
        shiny::showNotification(
          paste("Connection error:", conditionMessage(e)),
          type = "error", duration = 10
        )
      })
    })

    # Clipboard toast is now handled entirely in JS (no round-trip needed)

    # Module servers
    .mod_sidebar_server("sidebar", state, session)
    .mod_overview_server("overview", state)
    .mod_explore_server("explore", state, session)
    .mod_atlas_server("atlas", state, session)
    .mod_queries_server("queries", state)
    .mod_build_server("build", state)
    .mod_session_server("session_log", state)
    # .mod_settings_server("settings", state)  # Removed from nav
  }
}

#' Studio CSS stylesheet
#'
#' @return Character; CSS styles for the Studio app.
#' @keywords internal
.studio_css <- function() {
  "
    /* ==========================================================
       dsOMOP Studio - Modern Clean Design
       Inspired by shadcn/tailwind aesthetic
       ========================================================== */

    /* ========== Global reset / base ========== */
    body { letter-spacing: -0.01em; }
    .container-fluid { padding: 1.25rem; }

    /* Status utility classes */
    .status-ok { color: #059669; font-weight: 600; }
    .status-warn { color: #d97706; font-weight: 600; }
    .status-err { color: #dc2626; font-weight: 600; }
    .suppressed { color: #dc2626; font-style: italic; }

    /* ========== Navbar ========== */
    .navbar {
      background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%) !important;
      border-bottom: 1px solid rgba(255,255,255,0.06);
      box-shadow: 0 1px 3px rgba(0,0,0,0.12);
      padding-top: 0.5rem; padding-bottom: 0.5rem;
    }
    .navbar .nav-link {
      transition: all 0.2s ease; border-radius: 0.375rem;
      padding: 0.4rem 0.75rem !important; font-size: 0.875rem;
      font-weight: 500; color: rgba(255,255,255,0.7) !important;
    }
    .navbar .nav-link:hover {
      background: rgba(255,255,255,0.08); color: #e2e8f0 !important;
    }
    .navbar .nav-link.active {
      background: rgba(59,130,246,0.2); color: #fff !important;
    }
    .navbar-brand {
      font-weight: 700; letter-spacing: -0.02em;
      display: inline-flex !important; align-items: center;
      gap: 0.5rem; font-size: 1.05rem;
    }

    /* ========== Disabled Achilles tab ========== */
    .navbar .nav-link.achilles-disabled {
      opacity: 0.4 !important; cursor: not-allowed !important;
      pointer-events: auto !important;
    }
    .navbar .nav-link.achilles-disabled:hover {
      background: transparent !important; color: rgba(255,255,255,0.4) !important;
    }

    /* ========== Cards - glassmorphism ========== */
    .card {
      margin-bottom: 1rem;
      background: rgba(255,255,255,0.8);
      backdrop-filter: blur(12px); -webkit-backdrop-filter: blur(12px);
      border: 1px solid rgba(0,0,0,0.06);
      border-radius: 0.75rem;
      box-shadow: 0 1px 2px rgba(0,0,0,0.04), 0 1px 3px rgba(0,0,0,0.06);
      transition: box-shadow 0.25s ease, transform 0.25s ease;
    }
    .card:hover {
      box-shadow: 0 4px 16px rgba(0,0,0,0.08), 0 1px 3px rgba(0,0,0,0.06);
    }
    .card-body { overflow-x: auto; }
    .card-header {
      background: transparent; border-bottom: 1px solid rgba(0,0,0,0.06);
      font-weight: 600; font-size: 0.875rem; color: #1e293b;
      padding: 0.75rem 1rem;
    }

    /* ========== Value boxes ========== */
    .bslib-value-box {
      border-radius: 0.75rem !important;
      transition: transform 0.25s ease, box-shadow 0.25s ease;
    }
    .bslib-value-box:hover {
      transform: translateY(-3px);
      box-shadow: 0 8px 24px rgba(0,0,0,0.1);
    }
    .bslib-value-box .value-box-title {
      text-transform: uppercase; letter-spacing: 0.06em;
      font-size: 0.7em; font-weight: 600; opacity: 0.85;
    }
    .bslib-value-box .value-box-value {
      font-weight: 700; font-size: 1.75rem;
    }

    /* ========== DT tables ========== */
    .dataTables_wrapper { font-size: 0.875rem; }
    .dataTables_wrapper .dataTables_filter input {
      border-radius: 0.5rem; border: 1px solid #e2e8f0;
      padding: 0.4rem 0.75rem; font-size: 0.85rem;
      transition: border-color 0.2s, box-shadow 0.2s;
      background: rgba(255,255,255,0.9);
    }
    .dataTables_wrapper .dataTables_filter input:focus {
      border-color: #3b82f6; outline: none;
      box-shadow: 0 0 0 3px rgba(59,130,246,0.12);
    }
    table.dataTable thead th {
      text-transform: uppercase; font-size: 0.7em; font-weight: 600;
      letter-spacing: 0.06em; color: #64748b;
      border-bottom: 2px solid #e2e8f0 !important;
      padding: 0.6rem 0.75rem !important;
    }
    table.dataTable tbody td {
      padding: 0.5rem 0.75rem !important; vertical-align: middle;
    }
    table.dataTable tbody tr { transition: background 0.15s ease; }
    table.dataTable tbody tr:hover { background-color: #f8fafc !important; }
    table.dataTable tbody tr.selected { background-color: rgba(59,130,246,0.06) !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      border-radius: 0.375rem; margin: 0 2px; transition: all 0.2s;
    }
    .dataTables_wrapper .dataTables_paginate .paginate_button.current {
      background: #2563eb !important; color: #fff !important;
      border-color: #2563eb !important; border-radius: 0.375rem;
    }

    /* Long concept names: truncate with ellipsis in DT cells */
    table.dataTable td { max-width: 320px; overflow: hidden;
      text-overflow: ellipsis; white-space: nowrap; }
    table.dataTable td:hover { white-space: normal; word-break: break-word; }

    /* Hide DataTables sizing ghost row (targets both td AND th) */
    table.dataTable tr[style*='height: 0px'] td,
    table.dataTable tr[style*='height: 0px'] th {
      padding: 0 !important; border: none !important;
      line-height: 0 !important; font-size: 0 !important;
      overflow: hidden !important; height: 0 !important;
    }
    table.dataTable tr[style*='height: 0px'] th .dataTables_sizing,
    table.dataTable tr[style*='height: 0px'] td .dataTables_sizing {
      height: 0 !important; overflow: hidden !important;
      visibility: hidden !important;
    }

    /* ========== Nav pills (sub-tabs) ========== */
    .nav-pills {
      gap: 0.25rem; padding: 0.25rem;
      background: #f1f5f9; border-radius: 0.625rem;
      margin-bottom: 1rem;
    }
    .nav-pills .nav-link {
      border-radius: 0.5rem; font-weight: 500; font-size: 0.85rem;
      padding: 0.4rem 0.85rem;
      transition: all 0.2s ease; color: #64748b;
    }
    .nav-pills .nav-link:hover { background: rgba(255,255,255,0.7); color: #1e293b; }
    .nav-pills .nav-link.active {
      background: #fff; color: #1e293b !important; font-weight: 600;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
    }

    /* ========== Buttons ========== */
    .btn {
      border-radius: 0.5rem; font-weight: 500; font-size: 0.85rem;
      transition: all 0.2s ease; letter-spacing: 0.01em;
    }
    .btn-primary {
      background: linear-gradient(135deg, #2563eb, #1d4ed8);
      border: none; color: #fff;
    }
    .btn-primary:hover {
      transform: translateY(-1px);
      box-shadow: 0 4px 12px rgba(37,99,235,0.3);
    }
    .btn-outline-primary { color: #2563eb; border-color: #2563eb; }
    .btn-outline-primary:hover, .btn-outline-primary:active {
      background: #2563eb; color: #fff !important; border-color: #2563eb;
      transform: translateY(-1px); box-shadow: 0 2px 8px rgba(37,99,235,0.2);
    }
    .btn-outline-success { color: #059669; border-color: #059669; }
    .btn-outline-success:hover, .btn-outline-success:active {
      background: #059669; color: #fff !important; border-color: #059669;
      transform: translateY(-1px); box-shadow: 0 2px 8px rgba(5,150,105,0.2);
    }
    .btn-outline-warning { color: #d97706; border-color: #d97706; }
    .btn-outline-warning:hover, .btn-outline-warning:active {
      background: #d97706; color: #fff !important; border-color: #d97706;
      transform: translateY(-1px); box-shadow: 0 2px 8px rgba(217,119,6,0.2);
    }
    .btn-outline-info { color: #0891b2; border-color: #0891b2; }
    .btn-outline-info:hover, .btn-outline-info:active {
      background: #0891b2; color: #fff !important; border-color: #0891b2;
      transform: translateY(-1px); box-shadow: 0 2px 8px rgba(8,145,178,0.2);
    }
    .btn-outline-danger { color: #dc2626; border-color: #dc2626; }
    .btn-outline-danger:hover, .btn-outline-danger:active {
      background: #dc2626; color: #fff !important; border-color: #dc2626;
    }
    .btn-outline-secondary { color: #64748b; border-color: #cbd5e1; }
    .btn-outline-secondary:hover {
      background: #f8fafc; color: #1e293b; border-color: #94a3b8;
    }
    .btn-sm { font-size: 0.78rem; padding: 0.3rem 0.6rem; }

    /* ========== Sidebar (overlay mode) ========== */
    .bslib-sidebar-layout > .sidebar {
      border-color: rgba(0,0,0,0.06) !important;
      background: rgba(248,250,252,0.97) !important;
      backdrop-filter: blur(16px); -webkit-backdrop-filter: blur(16px);
    }
    .bslib-sidebar-layout:has(> .sidebar[aria-expanded='true']) > .sidebar {
      position: absolute !important; right: 0; top: 0; bottom: 0;
      z-index: 100;
      box-shadow: -4px 0 20px rgba(0,0,0,0.1);
    }
    .bslib-sidebar-layout:has(> .sidebar[aria-expanded='true']) > .main {
      margin-right: 0 !important;
    }
    .accordion-button {
      font-size: 0.85rem; font-weight: 600; border-radius: 0.375rem;
    }
    .accordion-button:not(.collapsed) {
      border-left: 3px solid #2563eb;
      background: rgba(37,99,235,0.04);
    }

    /* ========== Code output ========== */
    .code-output {
      background: #0f172a; color: #e2e8f0;
      padding: 1rem; border-radius: 0.625rem;
      font-family: 'Fira Code', 'JetBrains Mono', monospace;
      font-size: 0.82em; white-space: pre-wrap; line-height: 1.6;
      max-height: 500px; overflow-y: auto;
      border: 1px solid rgba(255,255,255,0.06);
    }
    .code-output pre { margin: 0; background: transparent;
                       color: inherit; border: none; padding: 0; }
    .code-output code { font-family: inherit; color: inherit;
                        padding: 0; text-indent: 0; display: block; }
    .code-output .shiny-text-output { background: transparent;
                       color: inherit; border: none; padding: 0; }
    .r-comment { color: #6a9955; font-style: italic; }
    .r-string  { color: #ce9178; }
    .r-keyword { color: #7dd3fc; font-weight: 600; }
    .r-number  { color: #b5cea8; }
    .r-fn      { color: #dcdcaa; }
    .r-operator { color: #94a3b8; font-weight: 600; }
    .r-assign  { color: #94a3b8; }

    /* ========== Metric cards ========== */
    .metric-card { text-align: center; padding: 1.25rem; }
    .metric-card .value { font-size: 1.75em; font-weight: 700; color: #1e293b; }
    .metric-card .label { color: #64748b; font-size: 0.82em; font-weight: 500;
                          text-transform: uppercase; letter-spacing: 0.05em; }

    /* ========== Badges & items ========== */
    .clickable-row { cursor: pointer; }
    .clickable-row:hover { background-color: #f8fafc !important; }
    .concept-badge {
      display: inline-block; padding: 0.2em 0.55em;
      background: linear-gradient(135deg, #2563eb, #1d4ed8);
      color: white; border-radius: 0.375rem; font-size: 0.8em;
      margin: 0.15em; font-weight: 500;
    }
    .server-badge {
      display: inline-block; padding: 0.15em 0.5em;
      border-radius: 9999px; font-size: 0.72em;
      margin: 0.1em; font-weight: 600;
    }
    .server-badge-ok { background: #ecfdf5; color: #065f46; }
    .server-badge-err { background: #fef2f2; color: #991b1b; }
    .badge {
      font-weight: 500; border-radius: 9999px; padding: 0.25em 0.6em;
      font-size: 0.72em;
    }

    .scope-indicator {
      font-size: 0.78em; color: #64748b;
      font-style: italic; margin-top: 0.5em;
    }
    .cdm-info { font-size: 0.875rem; }
    .cdm-info dt { font-weight: 600; color: #374151; }

    /* Recipe items */
    .recipe-item {
      padding: 0.6em 0.75em; margin: 0.35em 0;
      border: 1px solid #e5e7eb; border-radius: 0.5rem;
      background: #fff; font-size: 0.84em;
      transition: border-color 0.15s, box-shadow 0.15s;
    }
    .recipe-item:hover {
      border-color: #cbd5e1; box-shadow: 0 1px 4px rgba(0,0,0,0.04);
    }
    .recipe-item .recipe-item-name {
      font-weight: 600; color: #1e293b;
      max-width: 260px; overflow: hidden;
      text-overflow: ellipsis; white-space: nowrap;
      display: inline-block;
    }
    .recipe-item .recipe-item-meta { color: #64748b; font-size: 0.82em; }
    .recipe-section-header {
      font-weight: 600; font-size: 0.85em; color: #475569;
      border-bottom: 1px solid #e5e7eb;
      padding-bottom: 0.35em; margin-top: 1em; margin-bottom: 0.4em;
      text-transform: uppercase; letter-spacing: 0.04em;
    }
    .recipe-badge {
      display: inline-block; padding: 0.15em 0.55em;
      border-radius: 9999px; font-size: 0.7em;
      font-weight: 600; margin-left: 0.25em;
    }
    .recipe-badge-var { background: #ecfdf5; color: #065f46; }
    .recipe-badge-filter { background: #fffbeb; color: #92400e; }
    .recipe-badge-output { background: #eff6ff; color: #1e40af; }
    .btn-quick-action {
      padding: 0.1em 0.4em; font-size: 0.7em;
      margin: 0 0.1em; border-radius: 0.25rem;
    }

    /* ========== Empty states ========== */
    .empty-state {
      text-align: center; padding: 3rem 1.5rem; color: #94a3b8;
    }
    .empty-state .fa, .empty-state .fas, .empty-state .far,
    .empty-state .fab, .empty-state i {
      font-size: 2.5rem; margin-bottom: 0.75rem;
      display: block; opacity: 0.35;
    }
    .empty-state h5 { color: #64748b; margin-bottom: 0.5rem; font-weight: 600; }
    .empty-state p { color: #94a3b8; font-size: 0.875em; max-width: 320px;
                     margin: 0 auto; }

    /* ========== Animations ========== */
    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(6px); }
      to { opacity: 1; transform: translateY(0); }
    }
    .fade-in { animation: fadeIn 0.3s ease-out; }

    /* Animate cards and tab panes on load */
    .card { animation: fadeIn 0.35s ease-out; }
    .bslib-value-box { animation: fadeIn 0.4s ease-out; }
    .tab-pane.active { animation: fadeSlideIn 0.25s ease-out; }
    @keyframes fadeSlideIn {
      from { opacity: 0; transform: translateY(6px); }
      to   { opacity: 1; transform: translateY(0); }
    }

    /* ========== Settings dl styling ========== */
    .settings-dl dt { font-weight: 600; font-size: 0.875rem; color: #475569; }
    .settings-dl dd { font-size: 0.875rem; }

    /* ========== Concept Picker ========== */
    .concept-picker-item {
      font-size: 0.82rem; cursor: pointer;
      transition: background 0.15s; border: none;
      border-bottom: 1px solid rgba(0,0,0,0.04);
    }
    .concept-picker-item:hover { background: #f1f5f9 !important; }
    .concept-picker-item:last-child { border-bottom: none; }
    .concept-picker-name { font-weight: 500; color: #1e293b; }
    .concept-picker-results { animation: fadeIn 0.2s ease-out; }

    /* ========== Breadcrumb ========== */
    .breadcrumb-nav { font-size: 0.82em; color: #64748b; margin-bottom: 0.75rem; }
    .breadcrumb-nav a { color: #2563eb; text-decoration: none; font-weight: 500; }
    .breadcrumb-nav a:hover { text-decoration: underline; }

    /* ========== Mini toast (clipboard) ========== */
    .mini-toast {
      position: fixed; bottom: 1.5rem; right: 1.5rem; z-index: 9999;
      background: #0f172a; color: #e2e8f0; padding: 0.4rem 0.85rem;
      border-radius: 9999px; font-size: 0.78rem; font-weight: 500;
      box-shadow: 0 4px 12px rgba(0,0,0,0.2);
      animation: toastIn 0.2s ease-out, toastOut 0.3s 1.4s ease-in forwards;
      display: flex; align-items: center; gap: 0.25rem;
    }
    @keyframes toastIn { from { opacity:0; transform:translateY(8px) scale(0.95); }
                          to   { opacity:1; transform:translateY(0) scale(1); } }
    @keyframes toastOut { from { opacity:1; } to { opacity:0; } }

    /* ========== Shiny notification overrides (mini-toast style) ========== */
    #shiny-notification-panel {
      width: auto; max-width: 380px; top: auto; bottom: 1rem; right: 1rem;
      position: fixed; z-index: 9998;
    }
    .shiny-notification {
      border-radius: 9999px; font-size: 0.78rem; font-weight: 500;
      padding: 0.45rem 0.9rem; border: none;
      box-shadow: 0 4px 12px rgba(0,0,0,0.2);
      background: #0f172a; color: #e2e8f0;
      animation: toastIn 0.2s ease-out;
      margin-bottom: 0.35rem; width: fit-content;
      max-width: 380px; overflow: hidden;
      text-overflow: ellipsis; white-space: nowrap;
    }
    .shiny-notification-close { display: none; }
    .shiny-notification-message { background: #0f172a; color: #e2e8f0; }
    .shiny-notification-warning { background: #78350f; color: #fef3c7; }
    .shiny-notification-error { background: #991b1b; color: #fee2e2; }

    /* ========== Plotly chart improvements ========== */
    .plotly .modebar-container { display: none !important; }
    .js-plotly-plot .plotly .cursor-crosshair,
    .js-plotly-plot .plotly .cursor-ew-resize,
    .js-plotly-plot .plotly .cursor-ns-resize,
    .js-plotly-plot .plotly .cursor-nw-resize,
    .js-plotly-plot .plotly .cursor-ne-resize,
    .js-plotly-plot .plotly .cursor-sw-resize,
    .js-plotly-plot .plotly .cursor-se-resize,
    .js-plotly-plot .plotly .cursor-w-resize,
    .js-plotly-plot .plotly .cursor-e-resize,
    .js-plotly-plot .plotly .nsewdrag,
    .js-plotly-plot .plotly .nsdrag,
    .js-plotly-plot .plotly .ewdrag { cursor: default !important; }
    .js-plotly-plot .plotly .main-svg { border-radius: 0.5rem; }

    /* ========== Forms (selects, inputs) ========== */
    .form-select, .form-control {
      border-radius: 0.5rem; border-color: #e2e8f0;
      font-size: 0.85rem; transition: border-color 0.2s, box-shadow 0.2s;
    }
    .form-select:focus, .form-control:focus {
      border-color: #3b82f6; box-shadow: 0 0 0 3px rgba(59,130,246,0.1);
    }
    .form-label, .control-label { font-size: 0.82rem; font-weight: 600;
      color: #475569; margin-bottom: 0.25rem; }
    .form-check-label { font-size: 0.85rem; }

    /* ========== Scrollbar ========== */
    ::-webkit-scrollbar { width: 6px; height: 6px; }
    ::-webkit-scrollbar-track { background: transparent; }
    ::-webkit-scrollbar-thumb {
      background: #cbd5e1; border-radius: 3px;
    }
    ::-webkit-scrollbar-thumb:hover { background: #94a3b8; }

    /* ========== Alert overrides ========== */
    .alert { border-radius: 0.5rem; font-size: 0.85rem; border: none; }

    /* ==========================================================
       DARK MODE
       ========================================================== */
    [data-bs-theme='dark'] body { background: #0f172a; }
    [data-bs-theme='dark'] .navbar {
      background: linear-gradient(135deg, #020617 0%, #0f172a 100%) !important;
      border-bottom-color: rgba(255,255,255,0.04);
    }
    [data-bs-theme='dark'] .navbar .nav-link:hover { background: rgba(255,255,255,0.06); }
    [data-bs-theme='dark'] .card {
      background: rgba(30,41,59,0.7);
      border-color: rgba(255,255,255,0.06) !important;
      box-shadow: 0 1px 3px rgba(0,0,0,0.3);
    }
    [data-bs-theme='dark'] .card:hover {
      box-shadow: 0 4px 16px rgba(0,0,0,0.3);
    }
    [data-bs-theme='dark'] .card-header {
      background: transparent; border-bottom-color: rgba(255,255,255,0.06);
      color: #e2e8f0;
    }
    [data-bs-theme='dark'] .code-output { background: #020617; color: #c9d1d9;
      border-color: rgba(255,255,255,0.04); }
    [data-bs-theme='dark'] .metric-card .value { color: #e2e8f0; }
    [data-bs-theme='dark'] .metric-card .label { color: #94a3b8; }
    [data-bs-theme='dark'] .recipe-item {
      background: rgba(30,41,59,0.8); border-color: #334155;
    }
    [data-bs-theme='dark'] .recipe-item:hover {
      border-color: #475569; box-shadow: 0 1px 4px rgba(0,0,0,0.2);
    }
    [data-bs-theme='dark'] .recipe-item .recipe-item-name { color: #e2e8f0; }
    [data-bs-theme='dark'] .recipe-item .recipe-item-meta { color: #94a3b8; }
    [data-bs-theme='dark'] .recipe-section-header { color: #94a3b8; border-color: #334155; }
    [data-bs-theme='dark'] .cdm-info dt { color: #e2e8f0; }
    [data-bs-theme='dark'] .clickable-row:hover { background-color: #1e293b !important; }
    [data-bs-theme='dark'] .server-badge-ok { background: #064e3b; color: #6ee7b7; }
    [data-bs-theme='dark'] .server-badge-err { background: #7f1d1d; color: #fca5a5; }
    [data-bs-theme='dark'] table.dataTable thead th {
      color: #94a3b8; border-bottom-color: #334155 !important;
    }
    [data-bs-theme='dark'] table.dataTable tbody tr:hover { background-color: rgba(30,41,59,0.8) !important; }
    [data-bs-theme='dark'] table.dataTable tbody tr.selected { background-color: rgba(59,130,246,0.12) !important; }
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_filter input {
      background: #1e293b; border-color: #475569; color: #e2e8f0;
    }
    [data-bs-theme='dark'] .dataTables_wrapper .dataTables_filter input:focus {
      border-color: #3b82f6; box-shadow: 0 0 0 3px rgba(59,130,246,0.15);
    }
    [data-bs-theme='dark'] .nav-pills { background: #1e293b; }
    [data-bs-theme='dark'] .nav-pills .nav-link { color: #94a3b8; }
    [data-bs-theme='dark'] .nav-pills .nav-link:hover { background: rgba(255,255,255,0.05); color: #e2e8f0; }
    [data-bs-theme='dark'] .nav-pills .nav-link.active {
      background: #334155; color: #fff !important;
      box-shadow: 0 1px 3px rgba(0,0,0,0.2);
    }
    [data-bs-theme='dark'] .mini-toast { background: #334155; color: #e2e8f0; }
    [data-bs-theme='dark'] .accordion-button:not(.collapsed) {
      border-left-color: #3b82f6; background: rgba(59,130,246,0.06);
    }
    [data-bs-theme='dark'] .bslib-sidebar-layout > .sidebar {
      background: rgba(15,23,42,0.97) !important;
      border-color: rgba(255,255,255,0.04) !important;
    }
    [data-bs-theme='dark'] .bslib-sidebar-layout:has(> .sidebar[aria-expanded='true']) > .sidebar {
      box-shadow: -4px 0 20px rgba(0,0,0,0.4);
    }
    [data-bs-theme='dark'] .empty-state h5 { color: #94a3b8; }
    [data-bs-theme='dark'] .empty-state p { color: #64748b; }
    [data-bs-theme='dark'] .settings-dl dt { color: #94a3b8; }
    [data-bs-theme='dark'] .breadcrumb-nav { color: #94a3b8; }
    [data-bs-theme='dark'] .breadcrumb-nav a { color: #60a5fa; }
    [data-bs-theme='dark'] .form-select, [data-bs-theme='dark'] .form-control {
      background: #1e293b; border-color: #334155; color: #e2e8f0;
    }
    [data-bs-theme='dark'] .form-label, [data-bs-theme='dark'] .control-label { color: #94a3b8; }
    [data-bs-theme='dark'] .shiny-notification {
      box-shadow: 0 4px 12px rgba(0,0,0,0.4);
    }
    [data-bs-theme='dark'] .shiny-notification-message { background: #334155; color: #e2e8f0; }
    [data-bs-theme='dark'] .shiny-notification-warning { background: #78350f; color: #fef3c7; }
    [data-bs-theme='dark'] .shiny-notification-error { background: #7f1d1d; color: #fecaca; }
    [data-bs-theme='dark'] ::-webkit-scrollbar-thumb { background: #475569; }
    [data-bs-theme='dark'] ::-webkit-scrollbar-thumb:hover { background: #64748b; }
    [data-bs-theme='dark'] .concept-badge {
      background: linear-gradient(135deg, #3b82f6, #2563eb);
    }
    [data-bs-theme='dark'] .recipe-badge-var { background: #064e3b; color: #6ee7b7; }
    [data-bs-theme='dark'] .recipe-badge-filter { background: #78350f; color: #fcd34d; }
    [data-bs-theme='dark'] .recipe-badge-output { background: #1e3a5f; color: #93c5fd; }
  "
}

#' Clean DataSHIELD error messages for UI display
#'
#' @param e A condition object.
#' @return Character; cleaned error message.
#' @keywords internal
.clean_ds_error <- function(e) {
  msg <- conditionMessage(e)
  # Strip ANSI escape codes
  msg <- gsub("\033\\[[0-9;]*m", "", msg)
  msg <- gsub("\\[1m|\\[22m|\\[0m|\\[31m|\\[39m", "", msg)
  # Try to get actual datashield errors
  if (grepl("datashield\\.errors", msg, ignore.case = TRUE)) {
    errs <- tryCatch(DSI::datashield.errors(), error = function(e2) NULL)
    if (!is.null(errs) && length(errs) > 0) {
      err_txt <- paste(vapply(seq_along(errs), function(i) {
        srv <- names(errs)[i]
        detail <- gsub("\033\\[[0-9;]*m", "", as.character(errs[[i]]))
        detail <- gsub("\\[1m|\\[22m|\\[0m|\\[31m|\\[39m", "", detail)
        paste0(srv, ": ", substr(trimws(detail), 1, 120))
      }, character(1)), collapse = " | ")
      return(err_txt)
    }
    return("Server error (check connection)")
  }
  substr(msg, 1, 200)
}

#' Extract CDM table names with person_id from state tables
#'
#' @param tables Named list of table metadata per server.
#' @return Character vector of table names.
#' @keywords internal
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

# Silent plotly_empty: suppresses "No trace type specified" warnings
.plotly_empty_silent <- function() {
  suppressWarnings(plotly::plotly_empty())
}

# Informative empty plotly: shows a centred message explaining why data is absent
.plotly_no_data <- function(msg = "No data available",
                            icon = "\U0001F6C8") {
  suppressWarnings(plotly::plotly_empty()) |>
    plotly::layout(
      annotations = list(list(
        text = paste(icon, msg),
        xref = "paper", yref = "paper", x = 0.5, y = 0.5,
        showarrow = FALSE,
        font = list(color = "#94a3b8", size = 13)
      )),
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE)
    )
}

# Safe plotly wrapper: catches errors and returns a placeholder
.safe_plotly <- function(expr) {
  suppressWarnings(tryCatch(
    expr,
    error = function(e) {
      .plotly_empty_silent() |>
        plotly::layout(title = list(
          text = paste("Chart unavailable:", conditionMessage(e)),
          font = list(color = "#6b7280", size = 12)
        ))
    }
  ))
}

# Map UI scope to backend scope
.backend_scope <- function(scope) {
  if (is.null(scope)) "pooled" else scope
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

.studio_colors <- c("#2563eb", "#059669", "#d97706", "#7c3aed", "#0891b2",
                    "#dc2626", "#65a30d", "#c026d3", "#ea580c", "#0d9488")

.plotly_defaults <- function(p, title = NULL) {
  p |> plotly::layout(
    font = list(family = "Inter, -apple-system, sans-serif", size = 12, color = "#374151"),
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    dragmode = FALSE,
    margin = list(l = 50, r = 20,
                  t = if (!is.null(title)) 48 else 20, b = 40),
    title = if (!is.null(title)) list(
      text = title, font = list(size = 14, color = "#1e293b", family = "Inter"),
      x = 0, xanchor = "left", y = 0.98
    ),
    xaxis = list(
      gridcolor = "rgba(0,0,0,0.04)", gridwidth = 1,
      linecolor = "#e2e8f0", linewidth = 1, zeroline = FALSE,
      fixedrange = TRUE, showline = TRUE, mirror = FALSE, ticks = "",
      tickfont = list(size = 11, color = "#64748b")
    ),
    yaxis = list(
      gridcolor = "rgba(0,0,0,0.04)", gridwidth = 1,
      linecolor = "#e2e8f0", linewidth = 1, zeroline = FALSE,
      fixedrange = TRUE, showline = TRUE, mirror = FALSE, ticks = "",
      tickfont = list(size = 11, color = "#64748b")
    ),
    hoverlabel = list(
      bgcolor = "#0f172a", font = list(color = "#e2e8f0", size = 12, family = "Inter"),
      bordercolor = "transparent", padding = list(t = 6, b = 6, l = 10, r = 10),
      namelength = -1
    ),
    legend = list(
      font = list(size = 11, color = "#475569"),
      bgcolor = "transparent", borderwidth = 0,
      orientation = "h", y = -0.15, x = 0.5, xanchor = "center"
    ),
    bargap = 0.3
  ) |> plotly::config(displayModeBar = FALSE, scrollZoom = FALSE)
}

.empty_state_ui <- function(icon_name, title, desc = NULL) {
  shiny::div(class = "empty-state fade-in",
    shiny::icon(icon_name),
    shiny::h5(title),
    if (!is.null(desc)) shiny::p(desc)
  )
}

.history_add <- function(state, action) {
  entry <- list(time = Sys.time(), action = action)
  state$history <- c(state$history, list(entry))
}

