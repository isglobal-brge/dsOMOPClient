# Module: Studio - Recipe Builder
# Shiny module for interactive recipe construction.

#' Studio Recipe Builder UI
#'
#' @param id Character; Shiny module namespace ID.
#' @return A Shiny UI element.
#' @keywords internal
.mod_build_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_pill(
    id = ns("build_nav"),
    bslib::nav_panel("Recipe", icon = shiny::icon("hammer"),
      .mod_build_recipe_ui(ns("recipe"))),
    bslib::nav_panel("Plan", icon = shiny::icon("drafting-compass"),
      .mod_build_plan_ui(ns("plan")))
  )
}

#' Studio Recipe Builder Server
#'
#' @param id Character; Shiny module namespace ID.
#' @param state Reactive values; the shared OMOP session state.
#' @return NULL (Shiny module server, called for side effects).
#' @keywords internal
.mod_build_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_build_recipe_server("recipe", state)
    .mod_build_plan_server("plan", state)
  })
}

.mod_build_recipe_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Recipe Actions", width = 320,

      bslib::accordion(
        id = ns("builder_accordion"),
        multiple = FALSE, open = "Add Variable",

        # --- Quick Add Variable ---
        bslib::accordion_panel(
          "Add Variable", icon = shiny::icon("plus"),
          shiny::selectInput(ns("add_table"), "Table",
            choices = .table_choices(c("condition_occurrence", "drug_exposure",
                        "measurement", "procedure_occurrence",
                        "observation", "visit_occurrence", "person"))),
          .concept_picker_ui(ns("add_concept"), label = "Concept",
                             placeholder = "Type name or ID..."),
          shiny::selectInput(ns("add_format"), "Format",
            choices = c("Raw" = "raw", "Binary (0/1)" = "binary",
                        "Count" = "count", "First Value" = "first_value",
                        "Last Value" = "last_value", "Mean" = "mean",
                        "Min" = "min", "Max" = "max",
                        "Time Since" = "time_since", "Binned" = "binned",
                        "Sum" = "sum", "Distinct Concepts" = "n_distinct",
                        "Std Dev" = "sd", "Coeff. of Variation" = "cv",
                        "Slope (trend)" = "slope",
                        "Drug Duration" = "drug_duration",
                        "Abnormal High (count)" = "abnormal_high",
                        "Abnormal Low (count)" = "abnormal_low",
                        "Max Gap (days)" = "gap_max",
                        "Mean Gap (days)" = "gap_mean",
                        "Duration Sum (days)" = "duration_sum")),
          bslib::tooltip(
            shiny::checkboxInput(ns("add_include_descendants"),
              "Include descendant concepts", value = FALSE),
            "Match this concept plus all of its hierarchical descendants (via concept_ancestor). Resolved server-side at execution time."
          ),
          shiny::actionButton(ns("add_var_btn"), "Add Variable",
                              class = "btn-primary w-100")
        ),

        # --- Variable Block (batch add) ---
        bslib::accordion_panel(
          "Add Variable Block", icon = shiny::icon("layer-group"),
          shiny::selectInput(ns("block_table"), "Table",
            choices = .table_choices(c("condition_occurrence", "drug_exposure",
                        "measurement", "procedure_occurrence",
                        "observation"))),
          shiny::textAreaInput(ns("block_concept_ids"),
            "Concept IDs (comma-separated)",
            placeholder = "201820, 4229440, 316139", rows = 2),
          shiny::selectInput(ns("block_format"), "Format for All",
            choices = c("Binary (0/1)" = "binary", "Count" = "count",
                        "Raw" = "raw", "Mean" = "mean", "Min" = "min",
                        "Max" = "max", "Sum" = "sum",
                        "Distinct Concepts" = "n_distinct",
                        "Std Dev" = "sd", "Coeff. of Variation" = "cv",
                        "Slope (trend)" = "slope",
                        "Drug Duration" = "drug_duration",
                        "Abnormal High (count)" = "abnormal_high",
                        "Abnormal Low (count)" = "abnormal_low",
                        "Max Gap (days)" = "gap_max",
                        "Mean Gap (days)" = "gap_mean",
                        "Duration Sum (days)" = "duration_sum")),
          bslib::tooltip(
            shiny::checkboxInput(ns("block_include_descendants"),
              "Include descendant concepts", value = FALSE),
            "Match each concept plus all of its hierarchical descendants (via concept_ancestor). Resolved server-side at execution time."
          ),
          shiny::actionButton(ns("add_block_btn"), "Add Block",
                              class = "btn-primary w-100")
        ),

        # --- Add Derived Variable (no concept picker needed) ---
        bslib::accordion_panel(
          "Add Derived Variable", icon = shiny::icon("calculator"),
          shiny::selectInput(ns("derived_type"), "Derived Variable",
            choices = c(
              "Age" = "age",
              "Sex (M/F)" = "sex_mf",
              "Observation duration (days)" = "obs_duration",
              "Prior observation (days)" = "prior_obs",
              "Follow-up (days)" = "followup",
              "Demographics missingness" = "demo_missingness",
              "Charlson Comorbidity Index" = "charlson",
              "CHADS2 score" = "chads2",
              "CHA2DS2-VASc score" = "chadsvasc",
              "DCSI (diabetes complications)" = "dcsi",
              "HFRS (hospital frailty)" = "hfrs",
              "Distinct concepts (table-wide)" = "n_distinct",
              "Drug duration" = "drug_duration")),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("derived_type"),
                               "'] == 'age'"),
            shiny::selectInput(ns("derived_age_ref"), "Age reference",
              choices = c("Today" = "today", "Index date" = "index"))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("derived_type"),
                               "'] == 'n_distinct'"),
            shiny::selectInput(ns("derived_nd_table"), "Table",
              choices = .table_choices(c("condition_occurrence",
                          "drug_exposure", "measurement",
                          "procedure_occurrence", "observation")))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("derived_type"),
                               "'] == 'drug_duration'"),
            shiny::textInput(ns("derived_dd_cid"), "Drug Concept ID"),
            shiny::selectInput(ns("derived_dd_agg"), "Aggregation",
              choices = c("Mean" = "mean", "Sum" = "sum", "Max" = "max"))
          ),
          shiny::textInput(ns("derived_name"), "Name (optional)",
            placeholder = "leave blank for default"),
          shiny::actionButton(ns("add_derived_btn"), "Add Derived Variable",
                              class = "btn-primary w-100")
        ),

        # --- Quick Add Filter ---
        bslib::accordion_panel(
          "Add Filter", icon = shiny::icon("filter"),
          shiny::selectInput(ns("filter_type"), "Filter Type",
            choices = c("Sex" = "sex", "Age Group" = "age_group",
                        "Age Range" = "age_range",
                        "Has Concept" = "has_concept",
                        "Not Has Concept" = "not_has_concept",
                        "Concept Count" = "concept_count",
                        "Prior Observation" = "prior_observation",
                        "Follow-up" = "followup",
                        "Visit Count" = "visit_count",
                        "Has Measurement" = "has_measurement",
                        "Missing Measurement" = "missing_measurement",
                        "Date Range" = "date_range",
                        "Cohort Membership" = "cohort",
                        "Value Threshold (safe bins)" = "value_bin")),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"), "'] == 'sex'"),
            shiny::selectInput(ns("sex_value"), "Sex",
              choices = c("Female" = "F", "Male" = "M"))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'age_group'"),
            shiny::checkboxGroupInput(ns("age_groups"), "Age Groups",
              choices = c("0-4", "5-9", "10-14", "15-17", "18-24",
                          "25-34", "35-44", "45-54", "55-64",
                          "65-74", "75-84", "85+"),
              selected = NULL, inline = TRUE)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'age_range'"),
            shiny::numericInput(ns("age_min"), "Min Age", 0, 0, 150),
            shiny::numericInput(ns("age_max"), "Max Age", 150, 0, 150)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'has_concept'"),
            shiny::textInput(ns("filter_concept_id"), "Concept ID(s)",
              placeholder = "201820, 316139"),
            shiny::selectInput(ns("filter_table"), "In Table",
              choices = .table_choices(c("condition_occurrence", "drug_exposure",
                          "measurement", "procedure_occurrence"))),
            shiny::numericInput(ns("filter_min_count"), "Min occurrences",
              1, 1, NA),
            shiny::numericInput(ns("filter_window_start"),
              "Window start (days from index, optional)", value = NA),
            shiny::numericInput(ns("filter_window_end"),
              "Window end (days from index, optional)", value = NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'not_has_concept'"),
            shiny::textInput(ns("nhc_concept_id"), "Concept ID(s)",
              placeholder = "201820, 316139"),
            shiny::selectInput(ns("nhc_table"), "In Table",
              choices = .table_choices(c("condition_occurrence", "drug_exposure",
                          "measurement", "procedure_occurrence")))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'concept_count'"),
            shiny::textInput(ns("cc_concept_id"), "Concept ID(s)",
              placeholder = "201820, 316139"),
            shiny::selectInput(ns("cc_table"), "In Table",
              choices = .table_choices(c("condition_occurrence", "drug_exposure",
                          "measurement", "procedure_occurrence"))),
            shiny::numericInput(ns("cc_min_count"), "Min Count", 2, 1, NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'prior_observation'"),
            shiny::numericInput(ns("po_min_days"), "Min Prior Observation (days)",
              365, 0, NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'followup'"),
            shiny::numericInput(ns("fu_min_days"), "Min Follow-up (days)",
              30, 0, NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'visit_count'"),
            shiny::numericInput(ns("vc_min_count"), "Min Visits", 1, 1, NA),
            shiny::textInput(ns("vc_visit_concept_id"), "Visit Concept ID (optional)")
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'has_measurement'"),
            shiny::textInput(ns("hm_concept_id"), "Measurement Concept ID(s)"),
            shiny::numericInput(ns("hm_min_value"), "Min Value (optional)",
              value = NA),
            shiny::numericInput(ns("hm_max_value"), "Max Value (optional)",
              value = NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'missing_measurement'"),
            shiny::textInput(ns("mm_concept_id"), "Measurement Concept ID(s)")
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'date_range'"),
            shiny::dateInput(ns("date_start"), "Start Date"),
            shiny::dateInput(ns("date_end"), "End Date")
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'cohort'"),
            shiny::numericInput(ns("cohort_def_id"), "Cohort Definition ID",
              1, 1, NA)
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'value_bin'"),
            shiny::selectInput(ns("vb_table"), "Table",
              choices = .table_choices(c("measurement", "observation"))),
            shiny::textInput(ns("vb_column"), "Column",
              value = "value_as_number"),
            shiny::textInput(ns("vb_concept_id"), "Concept ID (optional)"),
            shiny::numericInput(ns("vb_threshold"), "Threshold", value = NA),
            shiny::selectInput(ns("vb_direction"), "Direction",
              choices = c("Above" = "above", "Below" = "below")),
            shiny::helpText(
              "Uses disclosure-safe bin edges fetched from the server",
              "(ds.omop.safe.cutpoints), so an active connection is required.")
          ),
          shiny::actionButton(ns("add_filter_btn"), "Add Filter",
                              class = "btn-warning w-100")
        ),

        # --- Group Filters (combine existing filters with AND/OR) ---
        bslib::accordion_panel(
          "Group Filters", icon = shiny::icon("object-group"),
          shiny::helpText(
            "Combine two or more existing filters into one AND/OR group.",
            "Selecting an existing group nests it (arbitrary AND/OR trees)."),
          shiny::selectInput(ns("group_operator"), "Combine with",
            choices = c("AND (all must match)" = "AND",
                        "OR (any may match)" = "OR"),
            selected = "AND"),
          shiny::checkboxGroupInput(ns("group_members"),
            "Filters to group", choices = character(0)),
          shiny::textInput(ns("group_label"), "Group label (optional)",
            placeholder = "auto-generated from members"),
          shiny::actionButton(ns("combine_filters_btn"),
            "Combine into Group", class = "btn-warning w-100")
        ),

        # --- Add Output ---
        bslib::accordion_panel(
          "Add Output", icon = shiny::icon("table"),
          shiny::textInput(ns("output_name"), "Output Name",
                           value = "output_1"),
          shiny::selectInput(ns("output_type"), "Output Type",
            choices = c("Wide (one row/person)" = "wide",
                        "Baseline (person table)" = "baseline",
                        "Long (events)" = "long",
                        "Features (sparse)" = "features",
                        "Covariates Sparse" = "covariates_sparse",
                        "Survival" = "survival",
                        "Intervals" = "intervals")),
          shiny::selectInput(ns("output_pop"), "Population",
            choices = c("base")),
          shiny::selectizeInput(ns("output_vars"), "Variables (empty = all)",
            choices = character(0), multiple = TRUE),
          # --- Event-output knobs (long only) ---
          shiny::conditionalPanel(
            condition = paste0("input['", ns("output_type"),
              "'] == 'long'"),
            bslib::tooltip(
              shiny::selectInput(ns("output_date_handling"), "Date handling",
                choices = c("Relative days from index" = "relative",
                            "Binned (calendar period)" = "binned",
                            "Remove all dates" = "remove"),
                selected = "relative"),
              "How event date columns are transformed. Absolute dates are omitted here because they require server disclosure permission."
            ),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("output_date_handling"),
                "'] == 'binned'"),
              shiny::selectInput(ns("output_date_bin"), "Bin width",
                choices = c("Month" = "month", "Week" = "week",
                            "Year" = "year"),
                selected = "month")
            ),
            bslib::tooltip(
              shiny::selectInput(ns("output_event_select"),
                "Keep events per person",
                choices = c("All events" = "all", "First N" = "first",
                            "Last N" = "last"),
                selected = "all"),
              "Restrict to the first or last N qualifying events per person (temporal event_select). Resolved server-side."
            ),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("output_event_select"),
                "'] != 'all'"),
              shiny::numericInput(ns("output_event_n"), "N events",
                value = 1, min = 1, step = 1)
            )
          ),
          # --- Survival knobs: time-at-risk window ---
          shiny::conditionalPanel(
            condition = paste0("input['", ns("output_type"),
              "'] == 'survival'"),
            shiny::numericInput(ns("output_tar_start"),
              "TAR start offset (days)", value = 0, step = 1),
            shiny::numericInput(ns("output_tar_end"),
              "TAR end offset (days)", value = 730, min = 1, step = 1)
          ),
          shiny::textInput(ns("output_symbol"), "Result Symbol",
                           placeholder = "D_output_1"),
          shiny::actionButton(ns("add_output_btn"), "Add Output",
                              class = "btn-info w-100")
        ),

        # --- Plan Options ---
        bslib::accordion_panel(
          "Plan Options", icon = shiny::icon("sliders"),
          bslib::tooltip(
            shiny::checkboxInput(ns("opt_translate_concepts"),
              "Translate concept IDs to labels",
              value = FALSE),
            "Replace numeric *_concept_id columns with human-readable concept names. Recommended for categorical data."
          ),
          bslib::tooltip(
            shiny::checkboxInput(ns("opt_block_sensitive"),
              "Block sensitive columns",
              value = TRUE),
            "Exclude exact dates and free-text note columns from outputs (disclosure safety)."
          ),
          bslib::tooltip(
            shiny::checkboxInput(ns("opt_factor_concepts"),
              "Harmonize concept factors across servers",
              value = TRUE),
            "Recode *_concept_id columns as factors with identical level coding on every server, so pooled ds.glm / ds.table align."
          ),
          bslib::tooltip(
            shiny::numericInput(ns("opt_min_persons"),
              "Minimum persons per cell (blank = none)",
              value = NA, min = 0, step = 1),
            "Suppress cells/rows backed by fewer than this many persons. Leave blank for no extra suppression."
          ),
          bslib::tooltip(
            shiny::checkboxInput(ns("opt_output_mode_staged"),
              "Staged output (Parquet, zero-copy)", value = FALSE),
            "Write each output to a Parquet file on the server and return a FlowerDatasetDescriptor instead of an in-memory data.frame. Use for very large extractions; requires arrow on the server. Results panel shows descriptor metadata, not ds.dim."
          ),
          shiny::hr(class = "my-2"),
          bslib::tooltip(
            shiny::numericInput(ns("base_cohort_id"),
              "Base cohort definition ID (blank = all persons)",
              value = NA, min = 1, step = 1),
            "Restrict the base population to an existing OMOP cohort (cohort table). Leave blank and click Set to clear."
          ),
          shiny::actionButton(ns("set_base_cohort"), "Set Base Cohort",
                              class = "btn-sm btn-outline-primary w-100")
        )
      ),

      # --- Save / load recipe (download as YAML/JSON, or upload one) ---
      shiny::hr(class = "my-2"),
      shiny::div(class = "fw-semibold small mb-1",
        shiny::icon("floppy-disk"), " Save / load recipe"),
      shiny::div(class = "d-flex gap-1 mb-2",
        shiny::selectInput(ns("recipe_export_format"), NULL,
          choices = c("YAML" = "yaml", "JSON" = "json"),
          selected = "yaml", width = "92px"),
        bslib::tooltip(
          shiny::downloadButton(ns("export_json"),
            shiny::tagList(shiny::icon("download"), " Download"),
            class = "btn-sm btn-outline-primary flex-fill"),
          "Download this recipe as a YAML or JSON file to re-load later or share."),
        bslib::tooltip(
          shiny::actionButton(ns("import_json_btn"),
            shiny::tagList(shiny::icon("upload"), " Load"),
            class = "btn-sm btn-outline-secondary flex-fill"),
          "Load a recipe from a .yml / .yaml / .json file.")
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("import_json_btn"), "'] > 0"),
        shiny::fileInput(ns("import_file"), NULL,
                         accept = c(".json", ".yml", ".yaml"),
                         buttonLabel = "Choose file")
      ),
      # --- Clear ---
      shiny::actionButton(ns("clear_recipe"),
        shiny::tagList(shiny::icon("trash-can"), " Clear"),
        class = "btn-sm btn-outline-danger w-100")
    ),

    # === Main panel ===

    # --- Population Tree ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::span("Populations"),
          bslib::tooltip(
            shiny::actionButton(ns("add_pop_btn"),
              shiny::tagList(shiny::icon("plus"), " Add"),
              class = "btn-sm btn-outline-primary"),
            "Add a new sub-population with filters"
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("pop_tree"))
      )
    ),

    # --- Recipe Contents ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::span("Recipe Contents"),
          shiny::uiOutput(ns("recipe_counts"), inline = TRUE)
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("recipe_display"))
      )
    ),

    # --- Schema Preview ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Schema Preview"),
      bslib::card_body(
        shiny::uiOutput(ns("schema_preview_content"))
      )
    ),

    # --- Generated Code ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Generated Code"),
        shiny::div(class = "d-flex gap-1",
          shiny::actionButton(ns("run_recipe"),
            shiny::tagList(shiny::icon("play"), " Run"),
            class = "btn-sm btn-primary"),
          shiny::actionButton(ns("copy_recipe_code"), NULL,
            icon = shiny::icon("copy"),
            class = "btn-sm btn-outline-secondary")
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("recipe_code_html"))
      )
    ),

    # --- Execution Results ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Execution Results"),
      bslib::card_body(
        shiny::uiOutput(ns("run_results"))
      )
    )
  )
}

.mod_build_recipe_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Concept picker for Quick Add Variable
    picked_concept <- .concept_picker_server("add_concept", state)

    # Populate table dropdowns from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) > 0) {
        formatted <- .table_choices(tbl_choices)
        shiny::updateSelectInput(session, "add_table", choices = formatted)
        shiny::updateSelectInput(session, "block_table", choices = formatted)
        shiny::updateSelectInput(session, "filter_table", choices = formatted)
      }
    })

    # Update population dropdown when populations change. Non-base
    # populations are flagged: the PHASE-1 compile guard rejects outputs
    # targeting them (recipe_to_plan).
    shiny::observe({
      pop_ids <- names(state$recipe$populations)
      shiny::updateSelectInput(session, "output_pop",
        choices = stats::setNames(pop_ids,
          ifelse(pop_ids == "base", pop_ids,
                 paste0(pop_ids, " (not yet executable)"))))
    })

    # Keep the per-output variable selector in sync with recipe variables
    shiny::observe({
      shiny::updateSelectizeInput(session, "output_vars",
        choices = names(state$recipe$variables) %||% character(0))
    })

    # Add Variable (using concept picker)
    shiny::observeEvent(input$add_var_btn, {
      tbl <- input$add_table
      fmt <- input$add_format
      picked <- picked_concept()

      cid <- if (!is.null(picked)) picked$concept_id else NULL
      cname_val <- if (!is.null(picked)) picked$concept_name else NULL

      if (is.null(cid)) {
        shiny::showNotification("Pick a concept first", type = "warning")
        return()
      }

      tryCatch({
        v <- omop_variable(
          table = tbl, concept_id = cid,
          concept_name = cname_val, format = fmt
        )
        v$expand <- isTRUE(input$add_include_descendants)
        state$recipe <- recipe_add_variable(state$recipe, v)
        shiny::showNotification(
          paste("Added variable:", v$name), type = "message", duration = 2)
        shiny::updateCheckboxInput(session, "add_include_descendants",
          value = FALSE)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add Variable Block
    shiny::observeEvent(input$add_block_btn, {
      tbl <- input$block_table
      ids_text <- trimws(input$block_concept_ids)
      fmt <- input$block_format

      tryCatch({
        ids <- .parse_ids(ids_text)
        if (is.null(ids) || length(ids) == 0) {
          shiny::showNotification("Enter at least one concept ID",
                                  type = "warning")
          return()
        }
        b <- omop_variable_block(
          table = tbl, concept_ids = ids, format = fmt
        )
        b$expand <- isTRUE(input$block_include_descendants)
        state$recipe <- recipe_add_block(state$recipe, b)
        shiny::showNotification(
          paste("Added block with", length(ids), "concepts"),
          type = "message", duration = 2)
        shiny::updateTextAreaInput(session, "block_concept_ids", value = "")
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add Derived Variable (person-level, no concept picker)
    shiny::observeEvent(input$add_derived_btn, {
      dtype <- input$derived_type
      nm <- trimws(input$derived_name %||% "")
      tryCatch({
        v <- switch(dtype,
          age              = omop_variable_age(
                               name = if (nchar(nm) > 0) nm else "age",
                               reference = input$derived_age_ref %||% "today"),
          sex_mf           = if (nchar(nm) > 0) omop_variable_sex(name = nm) else omop_variable_sex(),
          obs_duration     = if (nchar(nm) > 0) omop_variable_obs_duration(name = nm) else omop_variable_obs_duration(),
          prior_obs        = if (nchar(nm) > 0) omop_variable_prior_obs(name = nm) else omop_variable_prior_obs(),
          followup         = if (nchar(nm) > 0) omop_variable_followup(name = nm) else omop_variable_followup(),
          demo_missingness = if (nchar(nm) > 0) omop_variable_demo_missingness(name = nm) else omop_variable_demo_missingness(),
          charlson         = if (nchar(nm) > 0) omop_variable_charlson(name = nm) else omop_variable_charlson(),
          chads2           = if (nchar(nm) > 0) omop_variable_chads2(name = nm) else omop_variable_chads2(),
          chadsvasc        = if (nchar(nm) > 0) omop_variable_chadsvasc(name = nm) else omop_variable_chadsvasc(),
          dcsi             = if (nchar(nm) > 0) omop_variable_dcsi(name = nm) else omop_variable_dcsi(),
          hfrs             = if (nchar(nm) > 0) omop_variable_hfrs(name = nm) else omop_variable_hfrs(),
          n_distinct       = omop_variable_n_distinct(input$derived_nd_table,
                               name = if (nchar(nm) > 0) nm else NULL),
          drug_duration    = {
            dd_cid <- .parse_ids(input$derived_dd_cid)
            if (is.null(dd_cid) || any(is.na(dd_cid))) {
              shiny::showNotification("Enter a numeric drug concept ID",
                                      type = "warning")
              return()
            }
            omop_variable_drug_duration(dd_cid[1],
              name = if (nchar(nm) > 0) nm else NULL,
              agg = input$derived_dd_agg %||% "mean")
          }
        )
        state$recipe <- recipe_add_variable(state$recipe, v)
        shiny::showNotification(paste("Added derived variable:", v$name), type = "message", duration = 2)
        shiny::updateTextInput(session, "derived_name", value = "")
      }, error = function(e) {
        shiny::showNotification(.clean_ds_error(e), type = "error")
      })
    })

    # --- Plan Options -> persist onto the recipe (single source of truth) ---
    # recipe_to_plan reads recipe$options, so the existing recipe_execute call
    # carries these automatically with no signature change.
    shiny::observeEvent(
      list(input$opt_translate_concepts, input$opt_block_sensitive,
           input$opt_factor_concepts, input$opt_min_persons),
      {
        mp <- suppressWarnings(as.integer(input$opt_min_persons))
        if (length(mp) == 0 || is.na(mp)) mp <- NULL
        # Merge (not replace) so option keys set elsewhere (e.g. via an
        # imported recipe's recipe_set_options) are preserved.
        opts <- state$recipe$options %||% list()
        opts$translate_concepts <- isTRUE(input$opt_translate_concepts)
        opts$block_sensitive    <- isTRUE(input$opt_block_sensitive)
        opts$min_persons        <- mp
        opts$factor_concepts    <- isTRUE(input$opt_factor_concepts)
        state$recipe$options <- opts
      },
      ignoreInit = TRUE
    )

    # Base cohort (recipe-level, so it survives recompiles and round-trips
    # through export / generated code; recipe_to_plan consumes it).
    shiny::observeEvent(input$set_base_cohort, {
      cid <- suppressWarnings(as.integer(input$base_cohort_id))
      if (length(cid) == 0 || is.na(cid)) cid <- NULL
      state$recipe <- recipe_set_cohort(state$recipe, cid)
      shiny::showNotification(
        if (is.null(cid)) "Base cohort cleared."
        else paste("Base population restricted to cohort", cid),
        type = "message", duration = 2)
    })

    # Add Filter
    shiny::observeEvent(input$add_filter_btn, {
      ftype <- input$filter_type
      tryCatch({
        f <- switch(ftype,
          sex = omop_filter_sex(input$sex_value),
          age_group = {
            groups <- input$age_groups
            if (length(groups) == 0) {
              shiny::showNotification("Select at least one age group",
                                      type = "warning")
              return()
            }
            omop_filter_age_group(groups)
          },
          age_range = {
            amin <- input$age_min
            amax <- input$age_max
            if (is.null(amin) || is.null(amax) ||
                is.na(amin) || is.na(amax)) {
              shiny::showNotification(
                "Enter both a min and max age", type = "warning")
              return()
            }
            if (amin > amax) {
              shiny::showNotification(
                "Min age must be less than or equal to max age",
                type = "warning")
              return()
            }
            omop_filter_age(min = amin, max = amax)
          },
          has_concept = {
            ids <- .parse_ids(input$filter_concept_id)
            if (is.null(ids) || any(is.na(ids))) {
              shiny::showNotification(
                "Enter at least one valid numeric concept ID",
                type = "warning")
              return()
            }
            ws <- input$filter_window_start
            we <- input$filter_window_end
            win <- if (!is.null(ws) && !is.null(we) &&
                       !is.na(ws) && !is.na(we))
              list(start = as.integer(ws), end = as.integer(we)) else NULL
            mc <- suppressWarnings(as.integer(input$filter_min_count))
            if (length(mc) == 0 || is.na(mc)) mc <- 1L
            omop_filter_has_concept(ids, input$filter_table,
              window = win, min_count = mc)
          },
          not_has_concept = {
            ids <- .parse_ids(input$nhc_concept_id)
            if (is.null(ids) || any(is.na(ids))) {
              shiny::showNotification(
                "Enter at least one valid numeric concept ID",
                type = "warning")
              return()
            }
            omop_filter_not_has_concept(ids, input$nhc_table)
          },
          concept_count = {
            ids <- .parse_ids(input$cc_concept_id)
            if (is.null(ids) || any(is.na(ids))) {
              shiny::showNotification(
                "Enter at least one valid numeric concept ID",
                type = "warning")
              return()
            }
            omop_filter_concept_count(ids, input$cc_table,
              min_count = as.integer(input$cc_min_count))
          },
          prior_observation =
            omop_filter_prior_observation(
              min_days = as.integer(input$po_min_days)),
          followup =
            omop_filter_followup(min_days = as.integer(input$fu_min_days)),
          visit_count = {
            vcid <- .parse_ids(input$vc_visit_concept_id %||% "")
            if (!is.null(vcid) && any(is.na(vcid))) vcid <- NULL
            omop_filter_visit_count(
              min_count = as.integer(input$vc_min_count),
              visit_concept_id = vcid)
          },
          has_measurement = {
            ids <- .parse_ids(input$hm_concept_id)
            if (is.null(ids) || any(is.na(ids))) {
              shiny::showNotification(
                "Enter at least one valid numeric concept ID",
                type = "warning")
              return()
            }
            minv <- if (!is.null(input$hm_min_value) &&
                        !is.na(input$hm_min_value)) input$hm_min_value else NULL
            maxv <- if (!is.null(input$hm_max_value) &&
                        !is.na(input$hm_max_value)) input$hm_max_value else NULL
            omop_filter_has_measurement(ids,
              min_value = minv, max_value = maxv)
          },
          missing_measurement = {
            ids <- .parse_ids(input$mm_concept_id)
            if (is.null(ids) || any(is.na(ids))) {
              shiny::showNotification(
                "Enter at least one valid numeric concept ID",
                type = "warning")
              return()
            }
            omop_filter_missing_measurement(ids)
          },
          date_range = omop_filter_date_range(
            start = as.character(input$date_start),
            end = as.character(input$date_end)),
          cohort = omop_filter_cohort(as.integer(input$cohort_def_id)),
          value_bin = {
            thr <- input$vb_threshold
            if (is.null(thr) || is.na(thr)) {
              shiny::showNotification("Enter a threshold value",
                                      type = "warning")
              return()
            }
            session_obj <- tryCatch(.get_session(state$symbol),
                                    error = function(e) NULL)
            if (is.null(session_obj) || length(session_obj$conns) == 0) {
              shiny::showNotification(
                "Value-threshold filters need disclosure-safe bins fetched from the server (ds.omop.safe.cutpoints). Connect with ds.omop.connect() first.",
                type = "error", duration = 6)
              return()
            }
            vb_cid <- .parse_ids(input$vb_concept_id %||% "")
            if (!is.null(vb_cid) && any(is.na(vb_cid))) vb_cid <- NULL
            ds.omop.safe.filter.value(
              input$vb_table,
              trimws(input$vb_column %||% "value_as_number"),
              threshold = thr,
              direction = input$vb_direction %||% "above",
              concept_id = if (!is.null(vb_cid)) vb_cid[1] else NULL,
              symbol = state$symbol)
          }
        )
        state$recipe <- recipe_add_filter(state$recipe, f)
        shiny::showNotification(
          paste("Added filter:", f$label), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Keep the 'Group Filters' checkbox choices in sync with the recipe's
    # current TOP-LEVEL filters. Existing groups may be selected too:
    # they nest into the new group, allowing arbitrary AND/OR trees.
    shiny::observe({
      fl <- state$recipe$filters
      ids <- names(fl) %||% character(0)
      choices <- if (length(ids)) {
        stats::setNames(
          ids,
          vapply(ids, function(fid) {
            f <- fl[[fid]]
            lbl <- f$label %||% fid
            if (inherits(f, "omop_filter_group"))
              lbl <- paste0("[", f$operator, " group] ", lbl)
            paste0(lbl, "  (", fid, ")")
          }, character(1)))
      } else {
        character(0)
      }
      shiny::updateCheckboxGroupInput(session, "group_members",
        choices = choices)
    })

    # Combine the checked filters into a single omop_filter_group, remove the
    # originals, and add the group back. The group flows through
    # recipe_to_plan() -> plan$cohort$filter_tree automatically.
    shiny::observeEvent(input$combine_filters_btn, {
      member_ids <- input$group_members
      if (length(member_ids) < 2) {
        shiny::showNotification(
          "Select at least two filters to combine", type = "warning")
        return()
      }
      tryCatch({
        recipe <- state$recipe
        # Preserve display order as shown in the recipe list.
        member_ids <- names(recipe$filters)[
          names(recipe$filters) %in% member_ids]
        children <- unname(recipe$filters[member_ids])
        lbl <- trimws(input$group_label %||% "")
        grp <- do.call(omop_filter_group, c(
          children,
          list(operator = input$group_operator,
               label = if (nchar(lbl) > 0) lbl else NULL)))
        # Remove originals, then add the group (auto-id f{N}_{operator}).
        for (mid in member_ids)
          recipe <- recipe_remove_filter(recipe, mid)
        recipe <- recipe_add_filter(recipe, grp)
        state$recipe <- recipe
        shiny::showNotification(
          paste("Grouped", length(children), "filters with",
                input$group_operator),
          type = "message", duration = 2)
        shiny::updateTextInput(session, "group_label", value = "")
      }, error = function(e) {
        shiny::showNotification(.clean_ds_error(e), type = "error")
      })
    })

    # Add Output
    shiny::observeEvent(input$add_output_btn, {
      nm <- trimws(input$output_name)
      if (nchar(nm) == 0) {
        shiny::showNotification("Enter an output name", type = "warning")
        return()
      }
      if (nm %in% names(state$recipe$outputs)) {
        shiny::showNotification(
          paste0("Output '", nm,
                 "' already exists - pick another name or use its edit button."),
          type = "warning")
        return()
      }
      pop_id <- input$output_pop %||% "base"
      if (!identical(pop_id, "base")) {
        shiny::showNotification(
          "Non-base populations are not yet executable; this output will block plan compilation until it targets 'base'.",
          type = "warning", duration = 8)
      }
      sym <- trimws(input$output_symbol %||% "")
      tryCatch({
        # Event-output knobs (long only): date_handling + temporal
        opts <- list()
        if (identical(input$output_type, "long")) {
          dmode <- input$output_date_handling %||% "relative"
          opts$date_handling <- omop.date_handling(
            mode = dmode, reference = "index",
            bin_width = if (identical(dmode, "binned"))
                          input$output_date_bin else NULL)
          esel <- input$output_event_select %||% "all"
          if (!identical(esel, "all")) {
            opts$temporal <- omop.temporal(
              event_select = list(order = esel,
                                  n = as.integer(input$output_event_n %||% 1)))
          }
        }
        # Survival: time-at-risk window
        if (identical(input$output_type, "survival")) {
          ts <- suppressWarnings(as.integer(input$output_tar_start))
          te <- suppressWarnings(as.integer(input$output_tar_end))
          opts$tar <- list(
            start_offset = if (length(ts) == 1 && !is.na(ts)) ts else 0L,
            end_offset   = if (length(te) == 1 && !is.na(te)) te else 730L)
        }
        o <- omop_output(name = nm, type = input$output_type,
                          variables = if (length(input$output_vars) > 0)
                            input$output_vars else NULL,
                          population_id = pop_id, options = opts,
                          result_symbol = if (nchar(sym) > 0) sym else NULL)
        state$recipe <- recipe_add_output(state$recipe, o)
        shiny::showNotification(
          paste("Added output:", nm), type = "message", duration = 2)
        shiny::updateTextInput(session, "output_name",
          value = paste0("output_", length(state$recipe$outputs) + 1))
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Add Population (modal)
    shiny::observeEvent(input$add_pop_btn, {
      fl <- state$recipe$filters
      filter_choices <- if (length(fl) > 0) {
        stats::setNames(
          names(fl),
          vapply(names(fl), function(fid) {
            paste0(fl[[fid]]$label %||% fid, "  (", fid, ")")
          }, character(1)))
      } else {
        character(0)
      }
      shiny::showModal(shiny::modalDialog(
        title = "Add Population",
        shiny::textInput(ns("pop_id"), "Population ID",
                         placeholder = "e.g. adults"),
        shiny::textInput(ns("pop_label"), "Label",
                         placeholder = "e.g. Adults 18-65"),
        shiny::selectInput(ns("pop_parent"), "Parent Population",
          choices = names(state$recipe$populations)),
        shiny::numericInput(ns("pop_cohort_id"),
          "Cohort Definition ID (optional)", value = NA, min = 1, step = 1),
        if (length(filter_choices) > 0)
          shiny::checkboxGroupInput(ns("pop_filters"),
            "Move existing filters into this population",
            choices = filter_choices),
        shiny::helpText(
          "Note: only the base population is executable today;",
          "outputs targeting other populations will not compile yet."),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_add_pop"), "Add",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$confirm_add_pop, {
      pid <- trimws(input$pop_id)
      plabel <- trimws(input$pop_label)
      parent <- input$pop_parent
      if (nchar(pid) == 0) {
        shiny::showNotification("Population ID is required", type = "warning")
        return()
      }
      tryCatch({
        sel <- input$pop_filters %||% character(0)
        sel <- sel[sel %in% names(state$recipe$filters)]
        pcid <- suppressWarnings(as.integer(input$pop_cohort_id))
        if (length(pcid) == 0 || is.na(pcid)) pcid <- NULL
        p <- omop_population(id = pid, label = plabel, parent_id = parent,
                             filters = unname(state$recipe$filters[sel]),
                             cohort_definition_id = pcid)
        recipe <- recipe_add_population(state$recipe, p)
        # Selected filters move into the population (no longer base-level).
        for (fid in sel) recipe <- recipe_remove_filter(recipe, fid)
        state$recipe <- recipe
        shiny::removeModal()
        shiny::showNotification(
          paste("Added population:", pid), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # Clear Recipe (with confirmation)
    shiny::observeEvent(input$clear_recipe, {
      shiny::showModal(shiny::modalDialog(
        title = "Clear Recipe?",
        "This will remove all variables, filters, and outputs. This cannot be undone.",
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_clear"), "Clear All",
                              class = "btn-danger")
        )
      ))
    })
    shiny::observeEvent(input$confirm_clear, {
      state$recipe <- omop_recipe()
      state$plan <- ds.omop.plan()
      state$plan_compile_error <- NULL
      # Reset the Plan Options inputs to the recipe defaults so the sidebar
      # does not show stale values from the cleared recipe.
      shiny::updateCheckboxInput(session, "opt_translate_concepts",
        value = FALSE)
      shiny::updateCheckboxInput(session, "opt_block_sensitive", value = TRUE)
      shiny::updateCheckboxInput(session, "opt_factor_concepts", value = TRUE)
      shiny::updateNumericInput(session, "opt_min_persons", value = NA)
      shiny::updateNumericInput(session, "base_cohort_id", value = NA)
      shiny::removeModal()
      shiny::showNotification("Recipe cleared.", type = "message", duration = 2)
    })

    # Auto-generate Plan from Recipe on changes. Compile errors (e.g. the
    # PHASE-1 multi-population guard) are recorded in
    # state$plan_compile_error instead of being silently swallowed, so the
    # Generated Code card can warn and the Run button can refuse early.
    shiny::observe({
      recipe <- state$recipe
      has_content <- length(recipe$variables) > 0 || length(recipe$blocks) > 0 ||
                     length(recipe$filters) > 0 || length(recipe$outputs) > 0
      if (has_content) {
        tryCatch({
          plan <- recipe_to_plan(recipe)
          state$plan <- plan
          state$plan_compile_error <- NULL
        }, error = function(e) {
          state$plan_compile_error <- conditionMessage(e)
        })
      } else {
        state$plan_compile_error <- NULL
      }
    })

    # Recipe Export/Import
    output$export_json <- shiny::downloadHandler(
      filename = function() {
        ext <- if (identical(input$recipe_export_format, "yaml")) "yml" else "json"
        paste0("omop_recipe_", format(Sys.Date(), "%Y%m%d"), ".", ext)
      },
      content = function(file) {
        if (identical(input$recipe_export_format, "yaml")) {
          recipe_export_yaml(state$recipe, file = file)
        } else {
          recipe_export_json(state$recipe, file = file)
        }
      }
    )

    shiny::observeEvent(input$import_file, {
      shiny::req(input$import_file)
      tryCatch({
        import_name <- input$import_file$name %||% ""
        ext <- tolower(sub("^.*\\.([^.]+)$", "\\1", import_name))
        imported <- if (ext %in% c("yml", "yaml")) {
          recipe_import_yaml(input$import_file$datapath)
        } else {
          recipe_import_json(input$import_file$datapath)
        }
        state$recipe <- imported
        # Sync the Plan Options inputs to the imported recipe; otherwise the
        # options observer would clobber the imported values from stale UI
        # state on the next toggle.
        opts <- imported$options %||% list()
        shiny::updateCheckboxInput(session, "opt_translate_concepts",
          value = isTRUE(opts$translate_concepts))
        shiny::updateCheckboxInput(session, "opt_block_sensitive",
          value = !identical(opts$block_sensitive, FALSE))
        shiny::updateCheckboxInput(session, "opt_factor_concepts",
          value = !identical(opts$factor_concepts, FALSE))
        shiny::updateNumericInput(session, "opt_min_persons",
          value = opts$min_persons %||% NA)
        shiny::updateNumericInput(session, "base_cohort_id",
          value = imported$populations$base$cohort_definition_id %||% NA)
        shiny::showNotification(
          paste("Imported recipe:",
                length(imported$variables), "variables,",
                length(imported$filters), "filters,",
                length(imported$outputs), "outputs"),
          type = "message", duration = 3)
      }, error = function(e) {
        shiny::showNotification(
          paste("Import error:", conditionMessage(e)), type = "error")
      })
    })

    # Recipe Counts Badge
    output$recipe_counts <- shiny::renderUI({
      recipe <- state$recipe
      np <- length(recipe$populations)
      nb <- length(recipe$blocks)
      nv <- length(recipe$variables)
      nf <- length(recipe$filters)
      no <- length(recipe$outputs)
      shiny::tagList(
        if (np > 1) shiny::span(class = "recipe-badge",
          style = "background: #e8daef; color: #6c3483;",
          paste(np, "pop")),
        if (nb > 0) shiny::span(class = "recipe-badge",
          style = "background: #d6eaf8; color: #1a5276;",
          paste(nb, "block")),
        shiny::span(class = "recipe-badge recipe-badge-var",
                    paste(nv, "var")),
        shiny::span(class = "recipe-badge recipe-badge-filter",
                    paste(nf, "filter")),
        shiny::span(class = "recipe-badge recipe-badge-output",
                    paste(no, "output"))
      )
    })

    # Population Tree
    output$pop_tree <- shiny::renderUI({
      recipe <- state$recipe
      pops <- recipe$populations
      if (length(pops) == 0) return(shiny::p("No populations."))

      # Build tree (simple indented list)
      .render_pop_node <- function(pid, depth = 0) {
        p <- pops[[pid]]
        indent <- paste0("padding-left: ", (depth * 1.5) + 0.5, "em;")
        children_ids <- names(pops)[vapply(pops, function(pp) {
          identical(pp$parent_id, pid)
        }, logical(1))]

        node <- shiny::div(
          style = indent,
          class = "recipe-item d-flex justify-content-between align-items-center",
          shiny::div(
            shiny::span(class = "recipe-item-name",
              if (depth > 0) shiny::icon("arrow-right", class = "me-1"),
              pid
            ),
            shiny::br(),
            shiny::span(class = "recipe-item-meta", p$label)
          ),
          if (pid != "base") {
            local({
              eid <- gsub("'", "\\\\'", pid)
              rm_js <- sprintf(
                "Shiny.setInputValue('%s', JSON.stringify({type:'population',id:'%s'}), {priority:'event'})",
                ns("recipe_remove_item"), eid)
              shiny::tags$button(
                class = "btn btn-sm btn-outline-danger",
                style = "padding: 0.1em 0.4em;",
                onclick = rm_js,
                shiny::icon("xmark")
              )
            })
          }
        )

        child_nodes <- lapply(children_ids, function(cid) {
          .render_pop_node(cid, depth + 1)
        })

        shiny::tagList(node, child_nodes)
      }

      # Start from roots (no parent or parent == NULL)
      root_ids <- names(pops)[vapply(pops, function(p) {
        is.null(p$parent_id)
      }, logical(1))]

      shiny::tagList(lapply(root_ids, .render_pop_node))
    })

    # Recipe Display (JS delegation for edit/remove buttons)
    output$recipe_display <- shiny::renderUI({
      recipe <- state$recipe
      sections <- list()

      # Helper: edit + remove buttons via JS delegation
      .action_btns <- function(item_type, item_id) {
        eid <- gsub("'", "\\\\'", item_id)
        edit_js <- sprintf(
          "Shiny.setInputValue('%s', JSON.stringify({type:'%s',id:'%s'}), {priority:'event'})",
          ns("recipe_edit_item"), item_type, eid)
        rm_js <- sprintf(
          "Shiny.setInputValue('%s', JSON.stringify({type:'%s',id:'%s'}), {priority:'event'})",
          ns("recipe_remove_item"), item_type, eid)
        shiny::div(class = "d-flex gap-1",
          shiny::tags$button(
            class = "btn btn-sm btn-outline-secondary",
            style = "padding: 0.1em 0.4em;",
            onclick = edit_js,
            shiny::icon("pen")
          ),
          shiny::tags$button(
            class = "btn btn-sm btn-outline-danger",
            style = "padding: 0.1em 0.4em;",
            onclick = rm_js,
            shiny::icon("xmark")
          )
        )
      }
      # Remove-only button
      .rm_btn <- function(item_type, item_id) {
        eid <- gsub("'", "\\\\'", item_id)
        rm_js <- sprintf(
          "Shiny.setInputValue('%s', JSON.stringify({type:'%s',id:'%s'}), {priority:'event'})",
          ns("recipe_remove_item"), item_type, eid)
        shiny::tags$button(
          class = "btn btn-sm btn-outline-danger",
          style = "padding: 0.1em 0.4em;",
          onclick = rm_js,
          shiny::icon("xmark")
        )
      }

      # --- Blocks section ---
      if (length(recipe$blocks) > 0) {
        block_items <- lapply(names(recipe$blocks), function(bid) {
          b <- recipe$blocks[[bid]]
          shiny::div(
            class = "recipe-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "recipe-item-name", bid),
              shiny::br(),
              shiny::span(class = "recipe-item-meta",
                paste0(b$table, " | ", length(b$concept_ids),
                       " concepts | ", b$format))
            ),
            .action_btns("block", bid)
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "recipe-section-header",
            shiny::icon("layer-group"), " Variable Blocks"),
          shiny::tagList(block_items)
        ))
      }

      # --- Variables section ---
      if (length(recipe$variables) > 0) {
        var_items <- lapply(names(recipe$variables), function(nm) {
          v <- recipe$variables[[nm]]
          concept_text <- if (!is.null(v$concept_id))
            paste0("concept ", v$concept_id,
                   if (!is.null(v$concept_name))
                     paste0(" (", substr(v$concept_name, 1, 30), ")")
                   else "") else ""
          tw_text <- if (!is.null(v$time_window))
            paste0(" | window: ", v$time_window$start, " to ",
                   v$time_window$end) else ""
          shiny::div(
            class = "recipe-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "recipe-item-name", nm),
              shiny::br(),
              shiny::span(class = "recipe-item-meta",
                paste0(v$table, " | ", v$format,
                       if (nchar(concept_text) > 0)
                         paste0(" | ", concept_text) else "",
                       tw_text))
            ),
            .action_btns("variable", nm)
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "recipe-section-header",
            shiny::icon("table-columns"), " Variables (",
            length(recipe$variables), ")"),
          shiny::tagList(var_items)
        ))
      }

      # --- Filters section ---
      if (length(recipe$filters) > 0) {
        filter_items <- lapply(names(recipe$filters), function(fid) {
          f <- recipe$filters[[fid]]
          is_group <- inherits(f, "omop_filter_group")
          type_text <- if (is_group)
            paste0("[", f$operator, " group] ",
                   paste(vapply(f$children, function(ch) ch$label %||% "?",
                                character(1)), collapse = " | "))
          else
            paste0("[", f$level, "] ", f$type)

          # Safety badge
          safety_badge <- NULL
          if (!is_group) {
            safety <- .classifyFilterClient(f$type, f$params)
            safety_badge <- switch(safety,
              "allowed" = shiny::span(
                class = "badge bg-success ms-1",
                style = "font-size: 0.7em;", "safe"),
              "constrained" = shiny::span(
                class = "badge bg-warning ms-1",
                style = "font-size: 0.7em;", "constrained"),
              "blocked" = shiny::span(
                class = "badge bg-danger ms-1",
                style = "font-size: 0.7em;", "blocked")
            )
          }

          shiny::div(
            class = "recipe-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "recipe-item-name", f$label, safety_badge),
              shiny::br(),
              shiny::span(class = "recipe-item-meta", type_text)
            ),
            .action_btns("filter", fid)
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "recipe-section-header",
            shiny::icon("filter"), " Filters"),
          shiny::tagList(filter_items)
        ))
      }

      # --- Outputs section ---
      if (length(recipe$outputs) > 0) {
        output_items <- lapply(names(recipe$outputs), function(nm) {
          o <- recipe$outputs[[nm]]
          sym <- o$result_symbol %||% paste0("D_", nm)
          shiny::div(
            class = "recipe-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "recipe-item-name", nm),
              shiny::br(),
              shiny::span(class = "recipe-item-meta",
                          paste0("Type: ", o$type,
                                 " | pop: ", o$population_id,
                                 " | symbol: ", sym))
            ),
            .action_btns("output", nm)
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "recipe-section-header",
            shiny::icon("table"), " Outputs"),
          shiny::tagList(output_items)
        ))
      }

      if (length(sections) == 0) {
        return(.empty_state_ui("hammer", "Recipe is empty",
          "Add variables from the Explore or Vocabulary tabs, or use the sidebar controls."))
      }

      shiny::div(shiny::tagList(sections))
    })

    # Static remove observer (JS delegation — works for all item types)
    shiny::observeEvent(input$recipe_remove_item, {
      data <- jsonlite::fromJSON(input$recipe_remove_item)
      item_type <- data$type
      item_id <- data$id
      tryCatch({
        if (item_type == "population") {
          rec <- recipe_remove_population(state$recipe, item_id)
          # Re-parent orphaned children and re-target outputs so nothing
          # dangles (a dangling child disappears from the tree; a dangling
          # output blocks plan compilation forever).
          for (pid in names(rec$populations)) {
            if (identical(rec$populations[[pid]]$parent_id, item_id))
              rec$populations[[pid]]$parent_id <- "base"
          }
          for (onm in names(rec$outputs)) {
            if (identical(rec$outputs[[onm]]$population_id, item_id)) {
              rec$outputs[[onm]]$population_id <- "base"
              shiny::showNotification(
                paste0("Output '", onm, "' retargeted to base population"),
                type = "warning", duration = 4)
            }
          }
          state$recipe <- rec
        } else if (item_type == "block") {
          state$recipe$blocks[[item_id]] <- NULL
          state$recipe$meta$modified <- Sys.time()
        } else if (item_type == "variable") {
          state$recipe <- recipe_remove_variable(state$recipe, item_id)
        } else if (item_type == "filter") {
          state$recipe <- recipe_remove_filter(state$recipe, item_id)
        } else if (item_type == "output") {
          state$recipe <- recipe_remove_output(state$recipe, item_id)
        }
      }, error = function(e) {
        shiny::showNotification(
          paste("Error removing item:", conditionMessage(e)), type = "error")
      })
    }, ignoreInit = TRUE)

    # Static edit observer (JS delegation — universal edit modal)
    shiny::observeEvent(input$recipe_edit_item, {
      data <- jsonlite::fromJSON(input$recipe_edit_item)
      item_type <- data$type
      item_id <- data$id
      session$userData$edit_context <- list(type = item_type, id = item_id)

      recipe <- state$recipe
      tbl_choices <- .table_choices(
        .get_person_tables(state$tables) %||%
          c("condition_occurrence", "drug_exposure", "measurement",
            "procedure_occurrence", "observation", "visit_occurrence", "person")
      )
      pop_ids <- names(recipe$populations)

      # Build type-specific modal content
      modal_content <- if (item_type == "variable") {
        v <- recipe$variables[[item_id]]
        if (is.null(v)) return()
        shiny::tagList(
          shiny::textInput(ns("edit_name"), "Name", value = v$name),
          shiny::selectInput(ns("edit_table"), "Table",
            choices = tbl_choices, selected = v$table),
          shiny::textInput(ns("edit_column"), "Source Column (optional)",
            value = v$column %||% ""),
          shiny::numericInput(ns("edit_concept_id"), "Concept ID",
            value = v$concept_id),
          shiny::textInput(ns("edit_concept_name"), "Concept Name",
            value = v$concept_name %||% ""),
          shiny::selectInput(ns("edit_type"), "Type",
            choices = c("auto", "numeric", "categorical", "date",
                        "boolean", "integer", "character"),
            selected = v$type),
          shiny::selectInput(ns("edit_format"), "Format",
            choices = union(c("raw", "binary", "count", "first_value",
                        "last_value", "mean", "min", "max",
                        "time_since", "binned", "sum", "n_distinct",
                        "sd", "cv", "slope", "drug_duration",
                        "abnormal_high", "abnormal_low", "gap_max",
                        "gap_mean", "duration_sum"), v$format),
            selected = v$format),
          shiny::textInput(ns("edit_value_source"), "Value Source",
            value = v$value_source %||% ""),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("edit_tw_start"), "Window Start (days)",
                value = if (!is.null(v$time_window)) v$time_window$start else NA)
            ),
            shiny::column(6,
              shiny::numericInput(ns("edit_tw_end"), "Window End (days)",
                value = if (!is.null(v$time_window)) v$time_window$end else NA)
            )
          ),
          shiny::selectInput(ns("edit_suffix_mode"), "Suffix Mode",
            choices = c("index", "range", "label"),
            selected = v$suffix_mode),
          shiny::checkboxInput(ns("edit_include_descendants"),
            "Include descendant concepts", value = isTRUE(v$expand))
        )
      } else if (item_type == "block") {
        b <- recipe$blocks[[item_id]]
        if (is.null(b)) return()
        shiny::tagList(
          shiny::textInput(ns("edit_name"), "Block ID", value = b$id),
          shiny::selectInput(ns("edit_table"), "Table",
            choices = tbl_choices, selected = b$table),
          shiny::textAreaInput(ns("edit_concept_ids_text"), "Concept IDs",
            value = paste(b$concept_ids, collapse = ", "), rows = 2),
          shiny::selectInput(ns("edit_format"), "Format",
            choices = c("raw", "binary", "count", "first_value",
                        "last_value", "mean", "min", "max",
                        "time_since", "binned", "sum", "n_distinct",
                        "sd", "cv", "slope", "drug_duration",
                        "abnormal_high", "abnormal_low", "gap_max",
                        "gap_mean", "duration_sum"),
            selected = b$format),
          shiny::textInput(ns("edit_value_source"), "Value Source",
            value = b$value_source %||% ""),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("edit_tw_start"), "Window Start (days)",
                value = if (!is.null(b$time_window)) b$time_window$start else NA)
            ),
            shiny::column(6,
              shiny::numericInput(ns("edit_tw_end"), "Window End (days)",
                value = if (!is.null(b$time_window)) b$time_window$end else NA)
            )
          ),
          shiny::selectInput(ns("edit_suffix_mode"), "Suffix Mode",
            choices = c("index", "range", "label"),
            selected = b$suffix_mode),
          shiny::checkboxInput(ns("edit_block_include_descendants"),
            "Include descendant concepts", value = isTRUE(b$expand))
        )
      } else if (item_type == "filter") {
        f <- recipe$filters[[item_id]]
        if (is.null(f)) return()
        is_group <- inherits(f, "omop_filter_group")
        if (is_group) {
          shiny::tagList(
            shiny::textInput(ns("edit_label"), "Label",
              value = f$label %||% ""),
            shiny::selectInput(ns("edit_filter_operator"), "Operator",
              choices = c("AND", "OR"), selected = f$operator)
          )
        } else {
          # Base fields
          base_fields <- shiny::tagList(
            shiny::textInput(ns("edit_label"), "Label",
              value = f$label %||% ""),
            # "output" level is not consumed by recipe_to_plan, so offering
            # it here would silently turn the filter into a no-op.
            shiny::selectInput(ns("edit_filter_level"), "Level",
              choices = c("population", "row"),
              selected = f$level)
          )
          # Type-specific param fields
          param_fields <- switch(f$type,
            "sex" = shiny::selectInput(ns("edit_sex_value"), "Sex",
              choices = c("Female" = "F", "Male" = "M"),
              selected = f$params$value %||% "F"),
            "age_range" = shiny::tagList(
              shiny::numericInput(ns("edit_age_min"), "Min Age",
                value = f$params$min %||% 0, min = 0, max = 150),
              shiny::numericInput(ns("edit_age_max"), "Max Age",
                value = f$params$max %||% 150, min = 0, max = 150)
            ),
            "age_group" = shiny::checkboxGroupInput(ns("edit_age_groups"),
              "Age Groups",
              choices = c("0-4", "5-9", "10-14", "15-17", "18-24",
                          "25-34", "35-44", "45-54", "55-64",
                          "65-74", "75-84", "85+"),
              selected = f$params$groups, inline = TRUE),
            "has_concept" = shiny::tagList(
              shiny::textInput(ns("edit_filter_cid"), "Concept ID(s)",
                value = paste(f$params$concept_id, collapse = ", ")),
              shiny::selectInput(ns("edit_filter_table"), "Table",
                choices = tbl_choices, selected = f$params$table),
              shiny::numericInput(ns("edit_filter_min_count"),
                "Min Count", value = f$params$min_count %||% 1L,
                min = 1),
              shiny::numericInput(ns("edit_filter_ws"),
                "Window start (days from index, optional)",
                value = f$params$window$start %||% NA),
              shiny::numericInput(ns("edit_filter_we"),
                "Window end (days from index, optional)",
                value = f$params$window$end %||% NA)
            ),
            "date_range" = shiny::tagList(
              shiny::dateInput(ns("edit_date_start"), "Start Date",
                value = f$params$start),
              shiny::dateInput(ns("edit_date_end"), "End Date",
                value = f$params$end)
            ),
            "not_has_concept" = shiny::tagList(
              shiny::textInput(ns("edit_filter_cid"), "Concept ID(s)",
                value = paste(f$params$concept_id, collapse = ", ")),
              shiny::selectInput(ns("edit_filter_table"), "Table",
                choices = tbl_choices, selected = f$params$table)
            ),
            "concept_count" = shiny::tagList(
              shiny::textInput(ns("edit_filter_cid"), "Concept ID(s)",
                value = paste(f$params$concept_id, collapse = ", ")),
              shiny::selectInput(ns("edit_filter_table"), "Table",
                choices = tbl_choices, selected = f$params$table),
              shiny::numericInput(ns("edit_filter_min_count"), "Min Count",
                value = f$params$min_count %||% 2L, min = 1)
            ),
            "prior_observation" = shiny::numericInput(
              ns("edit_min_days"), "Min Prior Observation (days)",
              value = f$params$min_days %||% 365L, min = 0),
            "followup" = shiny::numericInput(
              ns("edit_min_days"), "Min Follow-up (days)",
              value = f$params$min_days %||% 30L, min = 0),
            "visit_count" = shiny::tagList(
              shiny::numericInput(ns("edit_visit_min_count"), "Min Visits",
                value = f$params$min_count %||% 1L, min = 1),
              shiny::textInput(ns("edit_visit_concept_id"),
                "Visit Concept ID(s) (optional)",
                value = paste(f$params$visit_concept_id, collapse = ", "))
            ),
            "has_measurement" = shiny::tagList(
              shiny::textInput(ns("edit_meas_cid"),
                "Measurement Concept ID(s)",
                value = paste(f$params$concept_id, collapse = ", ")),
              shiny::numericInput(ns("edit_meas_min"), "Min Value",
                value = f$params$min_value %||% NA),
              shiny::numericInput(ns("edit_meas_max"), "Max Value",
                value = f$params$max_value %||% NA)
            ),
            "missing_measurement" = shiny::textInput(
              ns("edit_meas_cid"), "Measurement Concept ID(s)",
              value = paste(f$params$concept_id, collapse = ", ")),
            "cohort" = shiny::numericInput(ns("edit_cohort_def_id"),
              "Cohort Definition ID",
              value = f$params$cohort_definition_id %||% 1, min = 1),
            NULL
          )
          shiny::tagList(base_fields, param_fields)
        }
      } else if (item_type == "output") {
        o <- recipe$outputs[[item_id]]
        if (is.null(o)) return()
        cur_dh <- o$options$date_handling$mode %||% "relative"
        cur_bin <- o$options$date_handling$bin_width %||% "month"
        cur_esel <- o$options$temporal$event_select$order %||% "all"
        cur_en <- o$options$temporal$event_select$n %||% 1
        event_knobs <- if (identical(o$type, "long")) {
          shiny::tagList(
            shiny::selectInput(ns("edit_output_date_handling"), "Date handling",
              choices = c("Relative days from index" = "relative",
                          "Binned (calendar period)" = "binned",
                          "Remove all dates" = "remove"),
              selected = cur_dh),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("edit_output_date_handling"),
                "'] == 'binned'"),
              shiny::selectInput(ns("edit_output_date_bin"), "Bin width",
                choices = c("Month" = "month", "Week" = "week",
                            "Year" = "year"),
                selected = cur_bin)
            ),
            shiny::selectInput(ns("edit_output_event_select"),
              "Keep events per person",
              choices = c("All events" = "all", "First N" = "first",
                          "Last N" = "last"),
              selected = cur_esel),
            shiny::conditionalPanel(
              condition = paste0("input['", ns("edit_output_event_select"),
                "'] != 'all'"),
              shiny::numericInput(ns("edit_output_event_n"), "N events",
                value = cur_en, min = 1, step = 1)
            )
          )
        } else NULL
        tar_knobs <- if (identical(o$type, "survival")) {
          cur_tar <- o$options$tar %||% list(start_offset = 0,
                                             end_offset = 730)
          shiny::tagList(
            shiny::numericInput(ns("edit_output_tar_start"),
              "TAR start offset (days)",
              value = cur_tar$start_offset %||% 0, step = 1),
            shiny::numericInput(ns("edit_output_tar_end"),
              "TAR end offset (days)",
              value = cur_tar$end_offset %||% 730, min = 1, step = 1)
          )
        } else NULL
        shiny::tagList(
          shiny::textInput(ns("edit_name"), "Output Name", value = o$name),
          shiny::selectInput(ns("edit_output_type"), "Type",
            choices = c("wide", "long", "features", "survival",
                        "intervals", "baseline",
                        "covariates_sparse"),
            selected = o$type),
          shiny::selectInput(ns("edit_output_pop"), "Population",
            choices = stats::setNames(pop_ids,
              ifelse(pop_ids == "base", pop_ids,
                     paste0(pop_ids, " (not yet executable)"))),
            selected = o$population_id),
          shiny::selectizeInput(ns("edit_output_vars"),
            "Variables (empty = all)",
            choices = names(recipe$variables),
            selected = o$variables, multiple = TRUE),
          event_knobs,
          tar_knobs,
          shiny::textInput(ns("edit_result_symbol"), "Result Symbol",
            value = o$result_symbol %||% paste0("D_", o$name))
        )
      } else {
        return()
      }

      title <- paste("Edit", tools::toTitleCase(item_type), "-", item_id)
      shiny::showModal(shiny::modalDialog(
        title = title, size = "m", easyClose = TRUE,
        modal_content,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("confirm_edit"), "Save Changes",
                              class = "btn-primary")
        )
      ))
    }, ignoreInit = TRUE)

    # Confirm edit (universal handler)
    shiny::observeEvent(input$confirm_edit, {
      ctx <- session$userData$edit_context
      if (is.null(ctx)) return()
      item_type <- ctx$type
      item_id <- ctx$id
      recipe <- state$recipe

      tryCatch({
        if (item_type == "variable") {
          v <- recipe$variables[[item_id]]
          if (is.null(v)) { shiny::removeModal(); return() }
          new_name <- trimws(input$edit_name)
          if (nchar(new_name) == 0) new_name <- item_id
          # Build updated variable
          tw_start <- input$edit_tw_start
          tw_end <- input$edit_tw_end
          tw <- if (!is.null(tw_start) && !is.null(tw_end) &&
                    !is.na(tw_start) && !is.na(tw_end))
            list(start = tw_start, end = tw_end) else NULL
          cid_val <- input$edit_concept_id
          cname_val <- input$edit_concept_name
          vs_val <- input$edit_value_source
          cl_val <- trimws(input$edit_column %||% "")
          v$table <- input$edit_table
          v$column <- if (nchar(cl_val) > 0) cl_val else NULL
          v$concept_id <- if (!is.null(cid_val) && !is.na(cid_val))
            as.integer(cid_val) else NULL
          v$concept_name <- if (!is.null(cname_val) && nchar(cname_val) > 0) cname_val else NULL
          v$type <- input$edit_type
          v$format <- input$edit_format
          v$value_source <- if (!is.null(vs_val) && nchar(vs_val) > 0) vs_val else NULL
          v$time_window <- tw
          v$suffix_mode <- input$edit_suffix_mode
          v$expand <- isTRUE(input$edit_include_descendants)
          # Handle rename
          if (new_name != item_id) {
            if (new_name %in% names(recipe$variables)) {
              shiny::showNotification("Name already in use",
                type = "warning", duration = 4)
              return()
            }
            recipe$variables[[item_id]] <- NULL
            v$name <- new_name
            recipe$variables[[new_name]] <- v
          } else {
            v$name <- new_name
            recipe$variables[[item_id]] <- v
          }

        } else if (item_type == "block") {
          b <- recipe$blocks[[item_id]]
          if (is.null(b)) { shiny::removeModal(); return() }
          new_id <- trimws(input$edit_name)
          if (nchar(new_id) == 0) new_id <- item_id
          tw_start <- input$edit_tw_start
          tw_end <- input$edit_tw_end
          tw <- if (!is.null(tw_start) && !is.null(tw_end) &&
                    !is.na(tw_start) && !is.na(tw_end))
            list(start = tw_start, end = tw_end) else NULL
          vs_val <- input$edit_value_source
          ids_text <- trimws(input$edit_concept_ids_text %||% "")
          new_ids <- .parse_ids(ids_text)
          b$table <- input$edit_table
          b$concept_ids <- if (!is.null(new_ids)) as.integer(new_ids)
                           else b$concept_ids
          b$format <- input$edit_format
          b$value_source <- if (!is.null(vs_val) && nchar(vs_val) > 0) vs_val else NULL
          b$time_window <- tw
          b$suffix_mode <- input$edit_suffix_mode
          b$expand <- isTRUE(input$edit_block_include_descendants)
          if (new_id != item_id) {
            recipe$blocks[[item_id]] <- NULL
            b$id <- new_id
            recipe$blocks[[new_id]] <- b
          } else {
            recipe$blocks[[item_id]] <- b
          }

        } else if (item_type == "filter") {
          f <- recipe$filters[[item_id]]
          if (is.null(f)) { shiny::removeModal(); return() }
          is_group <- inherits(f, "omop_filter_group")
          if (is_group) {
            f$label <- trimws(input$edit_label)
            f$operator <- input$edit_filter_operator
          } else {
            f$label <- trimws(input$edit_label)
            f$level <- input$edit_filter_level
            # Update type-specific params
            if (f$type == "sex") {
              f$params$value <- input$edit_sex_value
            } else if (f$type == "age_range") {
              f$params$min <- input$edit_age_min
              f$params$max <- input$edit_age_max
            } else if (f$type == "age_group") {
              f$params$groups <- input$edit_age_groups
            } else if (f$type == "has_concept") {
              ids <- .parse_ids(input$edit_filter_cid)
              if (!is.null(ids) && !any(is.na(ids)))
                f$params$concept_id <- ids
              f$params$table <- input$edit_filter_table
              f$params$min_count <- as.integer(input$edit_filter_min_count)
              ws <- input$edit_filter_ws
              we <- input$edit_filter_we
              f$params$window <- if (!is.null(ws) && !is.null(we) &&
                                     !is.na(ws) && !is.na(we))
                list(start = as.integer(ws), end = as.integer(we)) else NULL
            } else if (f$type == "date_range") {
              f$params$start <- as.character(input$edit_date_start)
              f$params$end <- as.character(input$edit_date_end)
            } else if (f$type == "not_has_concept") {
              ids <- .parse_ids(input$edit_filter_cid)
              if (!is.null(ids) && !any(is.na(ids)))
                f$params$concept_id <- ids
              f$params$table <- input$edit_filter_table
            } else if (f$type == "concept_count") {
              ids <- .parse_ids(input$edit_filter_cid)
              if (!is.null(ids) && !any(is.na(ids)))
                f$params$concept_id <- ids
              f$params$table <- input$edit_filter_table
              f$params$min_count <- as.integer(input$edit_filter_min_count)
            } else if (f$type == "prior_observation" ||
                       f$type == "followup") {
              f$params$min_days <- as.integer(input$edit_min_days)
            } else if (f$type == "visit_count") {
              f$params$min_count <- as.integer(input$edit_visit_min_count)
              vcid <- .parse_ids(input$edit_visit_concept_id %||% "")
              f$params$visit_concept_id <- if (!is.null(vcid) &&
                !any(is.na(vcid))) vcid else NULL
            } else if (f$type == "has_measurement") {
              ids <- .parse_ids(input$edit_meas_cid)
              if (!is.null(ids) && !any(is.na(ids)))
                f$params$concept_id <- ids
              f$params$min_value <- if (!is.null(input$edit_meas_min) &&
                !is.na(input$edit_meas_min)) input$edit_meas_min else NULL
              f$params$max_value <- if (!is.null(input$edit_meas_max) &&
                !is.na(input$edit_meas_max)) input$edit_meas_max else NULL
            } else if (f$type == "missing_measurement") {
              ids <- .parse_ids(input$edit_meas_cid)
              if (!is.null(ids) && !any(is.na(ids)))
                f$params$concept_id <- ids
            } else if (f$type == "cohort") {
              f$params$cohort_definition_id <-
                as.integer(input$edit_cohort_def_id)
            }
          }
          recipe$filters[[item_id]] <- f

        } else if (item_type == "output") {
          o <- recipe$outputs[[item_id]]
          if (is.null(o)) { shiny::removeModal(); return() }
          new_name <- trimws(input$edit_name)
          if (nchar(new_name) == 0) new_name <- item_id
          # Validate against the DSL enum: a stale UI choice must never
          # produce an output type that omop_output()/import would reject.
          o$type <- match.arg(input$edit_output_type,
            c("wide", "long", "features", "survival",
              "intervals", "baseline", "covariates_sparse"))
          o$population_id <- input$edit_output_pop
          if (!identical(o$population_id, "base")) {
            shiny::showNotification(
              "Non-base populations are not yet executable; this output will block plan compilation until it targets 'base'.",
              type = "warning", duration = 8)
          }
          o$variables <- if (length(input$edit_output_vars) > 0)
            input$edit_output_vars else NULL
          # Event-output knobs (long only)
          if (identical(o$type, "long")) {
            dmode <- input$edit_output_date_handling %||% "relative"
            o$options$date_handling <- omop.date_handling(
              mode = dmode, reference = "index",
              bin_width = if (identical(dmode, "binned"))
                            input$edit_output_date_bin else NULL)
            esel <- input$edit_output_event_select %||% "all"
            o$options$temporal <- if (!identical(esel, "all"))
              omop.temporal(event_select = list(order = esel,
                n = as.integer(input$edit_output_event_n %||% 1)))
              else NULL
          } else {
            o$options$date_handling <- NULL
            o$options$temporal <- NULL
          }
          # Survival: time-at-risk window
          if (identical(o$type, "survival")) {
            ts <- suppressWarnings(as.integer(input$edit_output_tar_start))
            te <- suppressWarnings(as.integer(input$edit_output_tar_end))
            o$options$tar <- list(
              start_offset = if (length(ts) == 1 && !is.na(ts)) ts else 0L,
              end_offset   = if (length(te) == 1 && !is.na(te)) te else 730L)
          } else {
            o$options$tar <- NULL
          }
          o$result_symbol <- trimws(input$edit_result_symbol)
          if (nchar(o$result_symbol) == 0)
            o$result_symbol <- paste0("D_", new_name)
          if (new_name != item_id) {
            recipe$outputs[[item_id]] <- NULL
            o$name <- new_name
            recipe$outputs[[new_name]] <- o
          } else {
            recipe$outputs[[item_id]] <- o
          }
        }

        recipe$meta$modified <- Sys.time()
        state$recipe <- recipe
        shiny::removeModal()
        shiny::showNotification("Updated.", type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    }, ignoreInit = TRUE)

    # Schema Preview
    output$schema_preview_content <- shiny::renderUI({
      recipe <- state$recipe
      if (length(recipe$outputs) == 0 || length(recipe$variables) == 0) {
        return(.empty_state_ui("table-columns", "No schema preview",
          "Add variables and outputs to preview the data schema."))
      }
      shiny::tagList(
        DT::DTOutput(ns("schema_preview_dt")),
        shiny::uiOutput(ns("schema_info"))
      )
    })

    output$schema_preview_dt <- DT::renderDT({
      recipe <- state$recipe
      if (length(recipe$outputs) == 0 || length(recipe$variables) == 0)
        return(NULL)
      schemas <- recipe_preview_schema(recipe)
      all_dfs <- lapply(names(schemas), function(nm) schemas[[nm]])
      df <- do.call(rbind, all_dfs)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      DT::datatable(df,
        options = list(pageLength = 20, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "none")
    })

    output$schema_info <- shiny::renderUI({
      recipe <- state$recipe
      if (length(recipe$outputs) == 0) return(NULL)
      schemas <- recipe_preview_schema(recipe)
      info_items <- lapply(names(schemas), function(nm) {
        s <- schemas[[nm]]
        tables <- attr(s, "tables") %||% character(0)
        otype <- attr(s, "output_type") %||% "?"
        pop <- attr(s, "population_id") %||% "base"
        shiny::p(
          shiny::strong(nm, ": "),
          paste0(otype, " | ", nrow(s), " columns | ",
                 "pop=", pop, " | join_key=person_id | tables: ",
                 paste(tables, collapse = ", "))
        )
      })
      shiny::div(class = "mt-2", shiny::tagList(info_items))
    })

    # Generated Code
    shiny::observeEvent(input$copy_recipe_code, {
      recipe <- state$recipe
      if (length(recipe$variables) == 0 && length(recipe$filters) == 0 &&
          length(recipe$outputs) == 0 && length(recipe$blocks) == 0) {
        shiny::showNotification("Nothing to copy.", type = "warning",
                                duration = 2)
        return()
      }
      code <- recipe_to_code(recipe)
      session$sendCustomMessage("copyToClipboard", code)
    })

    output$recipe_code_html <- shiny::renderUI({
      recipe <- state$recipe
      if (length(recipe$variables) == 0 && length(recipe$filters) == 0 &&
          length(recipe$outputs) == 0 && length(recipe$blocks) == 0) {
        return(.empty_state_ui("code", "No code yet",
          "Add variables, filters, and outputs to generate code."))
      }
      compile_alert <- if (!is.null(state$plan_compile_error)) {
        shiny::div(class = "alert alert-danger",
          shiny::strong("Plan does not compile: "),
          state$plan_compile_error)
      } else NULL
      code <- recipe_to_code(recipe)
      highlighted <- .highlightR(code)
      shiny::tagList(
        compile_alert,
        shiny::div(class = "code-output",
          shiny::HTML(paste0("<pre><code>", highlighted, "</code></pre>"))
        )
      )
    })

    # --- Recipe execution (Run button) ---
    # Holds the last execution outcome for the run_results renderUI.
    run_state <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$run_recipe, {
      recipe <- state$recipe

      if (length(recipe$variables) == 0 && length(recipe$filters) == 0 &&
          length(recipe$blocks) == 0 && length(recipe$outputs) == 0) {
        shiny::showNotification(
          "Recipe is empty - add variables and at least one output first.",
          type = "warning", duration = 4)
        return()
      }

      if (length(recipe$outputs) == 0) {
        shiny::showNotification(
          "Recipe has no outputs - add an output (e.g. a 'wide' table) to materialize a result.",
          type = "warning", duration = 5)
        return()
      }

      if (!is.null(state$plan_compile_error)) {
        shiny::showNotification(
          paste("Cannot run:", state$plan_compile_error),
          type = "error", duration = 6)
        return()
      }

      session_obj <- tryCatch(.get_session(state$symbol), error = function(e) NULL)
      conns <- session_obj$conns
      if (is.null(session_obj) || is.null(conns) || length(conns) == 0) {
        shiny::showNotification(
          "No active DataSHIELD connection. Connect with ds.omop.connect() first.",
          type = "error", duration = 6)
        return()
      }

      # Execute federated: assigns result data frames SERVER-SIDE and returns
      # only the output->symbol mapping (metadata, never row-level data).
      have_dsbase <- requireNamespace("dsBaseClient", quietly = TRUE)
      # Staged output mode: outputs are Parquet FlowerDatasetDescriptors on the
      # server, NOT in-memory data.frames, so ds.dim / ds.colnames do not apply.
      mode <- if (isTRUE(input$opt_output_mode_staged)) "staged" else "memory"
      staged <- identical(mode, "staged")
      shiny::withProgress(message = "Executing recipe on server...", value = 0.5, {
        res <- tryCatch({
          out_map <- recipe_execute(recipe, symbol = state$symbol, conns = conns,
                                    output_mode = mode)
          if (length(out_map) == 0) {
            list(ok = FALSE, empty = TRUE,
                 message = "Recipe produced no outputs - add at least one variable to your output table before running.")
          } else {
            symbols_per_server <- tryCatch(
              DSI::datashield.symbols(conns), error = function(e) NULL)
            dims <- NULL; cols <- NULL
            if (have_dsbase && !staged) {
              dims <- lapply(unname(out_map), function(sym) tryCatch(
                dsBaseClient::ds.dim(sym, type = "both", datasources = conns),
                error = function(e) NULL))
              cols <- lapply(unname(out_map), function(sym) tryCatch(
                dsBaseClient::ds.colnames(sym, datasources = conns),
                error = function(e) NULL))
              names(dims) <- unname(out_map); names(cols) <- unname(out_map)
            }
            list(ok = TRUE, out = out_map, server_names = names(conns),
                 symbols = symbols_per_server, dims = dims, cols = cols,
                 have_dsbase = have_dsbase, staged = staged)
          }
        }, error = function(e) {
          list(ok = FALSE, message = conditionMessage(e))
        })
      })

      run_state(res)

      if (isTRUE(res$ok)) {
        n_sym <- length(res$out)
        shiny::showNotification(
          sprintf("Recipe executed. %d result table%s assigned server-side: %s",
                  n_sym, if (n_sym == 1) "" else "s",
                  paste(unname(res$out), collapse = ", ")),
          type = "message", duration = 5)
        .history_add(state, "Executed recipe via Run button")
      } else if (isTRUE(res$empty)) {
        shiny::showNotification(res$message, type = "warning", duration = 6)
      } else {
        shiny::showNotification(
          paste("Execution failed:", res$message),
          type = "error", duration = 8)
      }
    })

    output$run_results <- shiny::renderUI({
      res <- run_state()
      if (is.null(res)) {
        return(.empty_state_ui("play", "No execution yet",
          "Build a recipe with an output, then click Run. Results are materialized on the server; only dimensions and column names are shown here."))
      }

      if (isTRUE(res$empty)) {
        return(shiny::div(class = "alert alert-warning",
          shiny::strong("No result tables produced. "),
          shiny::span(res$message)))
      }

      if (!isTRUE(res$ok)) {
        return(shiny::div(class = "alert alert-danger",
          shiny::strong("Execution failed. "),
          shiny::span(res$message)))
      }

      out_map <- res$out                 # named: output_name -> server_symbol
      server_names <- res$server_names %||% names(res$symbols) %||% character(0)

      # --- Staged mode: outputs are Parquet FlowerDatasetDescriptors, not
      # data.frames. ds.dim / ds.colnames do not apply; report presence + that
      # the symbol is a Parquet descriptor for downstream dsFlower analysis. ---
      if (isTRUE(res$staged)) {
        staged_blocks <- lapply(seq_along(out_map), function(i) {
          sym <- unname(out_map)[i]
          out_nm <- names(out_map)[i]
          rows_per_server <- lapply(server_names, function(srv) {
            present <- !is.null(res$symbols) &&
              sym %in% (res$symbols[[srv]] %||% character(0))
            shiny::tags$tr(
              shiny::tags$td(srv),
              shiny::tags$td(if (present)
                shiny::span(class = "badge bg-success", "assigned")
                else shiny::span(class = "badge bg-secondary", "not found")),
              shiny::tags$td(shiny::em("Parquet descriptor"))
            )
          })
          bslib::card(class = "mb-2",
            bslib::card_header(
              shiny::tagList(shiny::icon("box-archive"), " ",
                shiny::strong(sym),
                shiny::span(class = "text-muted",
                  paste0("  (output: ", out_nm, ")")))),
            bslib::card_body(
              shiny::tags$table(class = "table table-sm mb-2",
                shiny::tags$thead(shiny::tags$tr(
                  shiny::tags$th("Server"), shiny::tags$th("Status"),
                  shiny::tags$th("Storage"))),
                shiny::tags$tbody(rows_per_server)),
              shiny::div(class = "text-muted",
                shiny::em("Staged as a FlowerDatasetDescriptor (Parquet). Use dsFlower column projection for federated analysis; ds.dim / ds.colnames do not apply to descriptors."))
            )
          )
        })
        return(shiny::tagList(
          shiny::div(class = "alert alert-success",
            shiny::strong("Execution complete (staged). "),
            sprintf("%d output(s) written to Parquet server-side.", length(out_map))),
          staged_blocks,
          shiny::p(class = "text-muted small",
            shiny::icon("shield-halved"),
            " Row-level data never leaves the server. Staged outputs are zero-copy Parquet descriptors.")
        ))
      }

      blocks <- lapply(seq_along(out_map), function(i) {
        sym <- unname(out_map)[i]
        out_nm <- names(out_map)[i]

        rows_per_server <- lapply(server_names, function(srv) {
          present <- !is.null(res$symbols) &&
            sym %in% (res$symbols[[srv]] %||% character(0))
          dimtxt <- NA_character_
          if (!is.null(res$dims[[sym]])) {
            key <- paste0("dimensions of ", sym, " in ", srv)
            d <- res$dims[[sym]][[key]]
            if (!is.null(d) && length(d) >= 2)
              dimtxt <- paste0(d[1], " rows x ", d[2], " cols")
          }
          shiny::tags$tr(
            shiny::tags$td(srv),
            shiny::tags$td(if (present)
              shiny::span(class = "badge bg-success", "assigned")
              else shiny::span(class = "badge bg-secondary", "not found")),
            shiny::tags$td(if (is.na(dimtxt)) shiny::em("-") else dimtxt)
          )
        })

        colnames_vec <- NULL
        if (!is.null(res$cols[[sym]])) {
          cl <- res$cols[[sym]]
          non_null <- which(!vapply(cl, is.null, logical(1)))
          if (length(non_null) > 0) colnames_vec <- cl[[non_null[1]]]
        }

        bslib::card(class = "mb-2",
          bslib::card_header(
            shiny::tagList(shiny::icon("table"), " ",
              shiny::strong(sym),
              shiny::span(class = "text-muted",
                paste0("  (output: ", out_nm, ")")))),
          bslib::card_body(
            shiny::tags$table(class = "table table-sm mb-2",
              shiny::tags$thead(shiny::tags$tr(
                shiny::tags$th("Server"), shiny::tags$th("Status"),
                shiny::tags$th("Dimensions"))),
              shiny::tags$tbody(rows_per_server)),
            if (!is.null(colnames_vec))
              shiny::div(
                shiny::strong("Columns: "),
                shiny::span(paste(colnames_vec, collapse = ", ")))
            else if (isTRUE(res$have_dsbase))
              shiny::div(class = "text-muted",
                shiny::em("Column names unavailable for this symbol."))
            else
              shiny::div(class = "text-muted",
                shiny::em("Install dsBaseClient to show dimensions and column names."))
          )
        )
      })

      shiny::tagList(
        shiny::div(class = "alert alert-success",
          shiny::strong("Execution complete. "),
          sprintf("%d result table(s) assigned server-side.", length(out_map))),
        blocks,
        shiny::p(class = "text-muted small",
          shiny::icon("shield-halved"),
          " Row-level data never leaves the server. Use ds.* functions (e.g. ds.summary, ds.glm) on these symbols for federated analysis.")
      )
    })
  })
}

.mod_build_plan_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Add Output", width = 320,
      # Concept set display
      bslib::card(
        bslib::card_header("Current Concept Set"),
        bslib::card_body(
          shiny::uiOutput(ns("concept_set_display")),
          shiny::actionButton(ns("add_current_concept"), "Add Current Concept",
                              class = "btn-sm btn-outline-primary w-100 mb-2"),
          shiny::actionButton(ns("add_events_for_set"),
                              "Add Events for Concept Set",
                              class = "btn-sm btn-outline-info w-100")
        )
      ),
      shiny::hr(),
      # Cohort management
      shiny::numericInput(ns("cohort_id"), "Cohort Definition ID",
                          value = 1, min = 1, step = 1),
      shiny::actionButton(ns("set_cohort"), "Set Cohort on Plan",
                          class = "btn-sm btn-outline-primary w-100 mb-2"),
      shiny::hr(),
      # Output type
      shiny::selectInput(ns("output_type"), "Output Type",
        choices = c("Baseline" = "baseline",
                    "Events (long)" = "event_level",
                    "Events (sparse)" = "sparse",
                    "Survival" = "survival",
                    "Cohort Membership" = "cohort_membership",
                    "Intervals Long" = "intervals_long",
                    "Temporal Covariates" = "temporal_covariates",
                    "Concept Dictionary" = "concept_dictionary")),
      shiny::textInput(ns("output_name"), "Output Name",
                       value = "output_1"),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"),
               "'] == 'event_level' || input['",
               ns("output_type"), "'] == 'sparse'"),
        shiny::selectInput(ns("event_table"), "Table",
          choices = .table_choices(c("condition_occurrence", "drug_exposure",
                      "measurement", "procedure_occurrence",
                      "observation", "visit_occurrence"))),
        shiny::textInput(ns("concept_ids"), "Concept IDs (comma-sep)",
                         placeholder = "201820, 255573")
      ),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"), "'] == 'survival'"),
        shiny::selectInput(ns("outcome_table"), "Outcome Table",
          choices = .table_choices(c("condition_occurrence", "drug_exposure",
                      "measurement"))),
        shiny::textInput(ns("outcome_concepts"), "Outcome Concepts",
                         placeholder = "4000002"),
        shiny::numericInput(ns("tar_end"), "TAR End (days)", 730, 30, 3650)
      ),
      shiny::conditionalPanel(
        paste0("input['", ns("output_type"),
               "'] == 'temporal_covariates'"),
        shiny::selectInput(ns("tc_table"), "Table",
          choices = .table_choices(c("condition_occurrence", "drug_exposure",
                      "measurement"))),
        shiny::textInput(ns("tc_concepts"), "Concept IDs",
                         placeholder = "201820, 255573"),
        shiny::numericInput(ns("tc_bin"), "Bin Width (days)", 30, 7, 365),
        shiny::numericInput(ns("tc_start"), "Window Start", -365),
        shiny::numericInput(ns("tc_end"), "Window End", 0)
      ),
      shiny::actionButton(ns("add_output"), "Add Output",
                          class = "btn-primary w-100 mt-2")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Current Plan"),
      bslib::card_body(
        shiny::div(class = "alert alert-warning py-1 small",
          shiny::icon("triangle-exclamation"),
          " Changes here edit the compiled plan directly and are overwritten whenever the Recipe changes. Prefer the Recipe tab; Plan-tab outputs are not part of the recipe download."),
        shiny::uiOutput(ns("plan_summary_content")),
        shiny::actionButton(ns("clear_plan"), "Clear Plan",
                            class = "btn-sm btn-outline-danger")
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Generated R Code"),
        shiny::div(
          shiny::actionButton(ns("copy_plan_code"), NULL,
            icon = shiny::icon("copy"),
            class = "btn-sm btn-outline-secondary me-1"),
          shiny::actionButton(ns("reset_plan"), NULL,
            icon = shiny::icon("rotate-left"),
            class = "btn-sm btn-outline-danger")
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("code_block"))
      )
    )
  )
}

.mod_build_plan_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {

    # Concept set display
    output$concept_set_display <- shiny::renderUI({
      ids <- state$concept_set
      if (length(ids) == 0) return(shiny::p(shiny::em("(empty)")))
      badges <- lapply(ids, function(cid) {
        shiny::span(class = "concept-badge", as.character(cid))
      })
      shiny::div(badges)
    })

    # Add current concept from exploration state
    shiny::observeEvent(input$add_current_concept, {
      cid <- state$selected_concept_id
      if (is.null(cid)) {
        shiny::showNotification("No concept selected in Explore/Drilldown.",
                                type = "warning")
        return()
      }
      current <- state$concept_set
      if (!cid %in% current) {
        state$concept_set <- c(current, cid)
        shiny::showNotification(
          paste("Added concept", cid, "to set"),
          type = "message", duration = 2
        )
      }
    })

    # Add events for concept set
    shiny::observeEvent(input$add_events_for_set, {
      ids <- state$concept_set
      tbl <- state$selected_table
      if (length(ids) == 0) {
        shiny::showNotification("Concept set is empty.", type = "warning")
        return()
      }
      if (is.null(tbl)) {
        tbl <- "condition_occurrence"
      }
      tryCatch({
        nm <- paste0("events_set_", length(state$plan$outputs) + 1)
        state$plan <- ds.omop.plan.events(
          state$plan, name = nm, table = tbl,
          concept_set = as.integer(ids)
        )
        shiny::showNotification(
          paste("Added events output for", length(ids), "concepts"),
          type = "message", duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error"
        )
      })
    })

    # Set cohort on plan. Also persisted on the recipe so it survives the
    # Recipe-tab auto-recompile and round-trips through export / codegen.
    shiny::observeEvent(input$set_cohort, {
      cid <- as.integer(input$cohort_id)
      state$recipe <- recipe_set_cohort(state$recipe, cid)
      state$plan <- ds.omop.plan.cohort(state$plan,
                                         cohort_definition_id = cid)
      shiny::showNotification(
        paste("Cohort", cid, "set on plan"),
        type = "message", duration = 3
      )
    })

    # Add output (same as old plan builder)
    shiny::observeEvent(input$add_output, {
      otype <- input$output_type
      nm <- input$output_name
      p <- state$plan

      tryCatch({
        if (otype == "baseline") {
          p <- ds.omop.plan.baseline(p, name = nm)
        } else if (otype == "event_level") {
          cs <- .parse_ids(input$concept_ids)
          p <- ds.omop.plan.events(p, name = nm, table = input$event_table,
                                    concept_set = cs)
        } else if (otype == "sparse") {
          cs <- .parse_ids(input$concept_ids)
          p <- ds.omop.plan.events(p, name = nm, table = input$event_table,
                                    concept_set = cs,
                                    representation = list(format = "sparse"))
        } else if (otype == "survival") {
          oc <- .parse_ids(input$outcome_concepts)
          p <- ds.omop.plan.survival(p, outcome_table = input$outcome_table,
                                      outcome_concepts = oc,
                                      tar = list(start_offset = 0,
                                                 end_offset = input$tar_end),
                                      name = nm)
        } else if (otype == "cohort_membership") {
          p <- ds.omop.plan.cohort_membership(p, name = nm)
        } else if (otype == "intervals_long") {
          p <- ds.omop.plan.intervals(p, name = nm)
        } else if (otype == "temporal_covariates") {
          cs <- .parse_ids(input$tc_concepts)
          p <- ds.omop.plan.temporal_covariates(
            p, table = input$tc_table, concept_set = cs,
            bin_width = as.integer(input$tc_bin),
            window_start = as.integer(input$tc_start),
            window_end = as.integer(input$tc_end),
            name = nm
          )
        } else if (otype == "concept_dictionary") {
          p <- ds.omop.plan.concept_dictionary(p, name = nm)
        }

        state$plan <- p
        shiny::showNotification(paste("Added output:", nm),
                                type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error"
        )
      })
    })

    shiny::observeEvent(input$clear_plan, {
      state$plan <- ds.omop.plan()
    })

    output$plan_summary_content <- shiny::renderUI({
      p <- state$plan
      if (is.null(p) || length(p$outputs) == 0) {
        return(.empty_state_ui("drafting-compass", "No plan yet",
          "Add outputs from the sidebar to build your plan."))
      }
      shiny::verbatimTextOutput(session$ns("plan_summary"))
    })

    output$plan_summary <- shiny::renderText({
      p <- state$plan
      paste(utils::capture.output(print(p)), collapse = "\n")
    })

    generated_code <- shiny::reactive({
      p <- state$plan
      if (is.null(p) || length(p$outputs) == 0) return("")
      tryCatch({
        out_names <- names(p$outputs)
        # Use result_symbol from recipe outputs when available
        recipe <- state$recipe
        symbols <- vapply(out_names, function(nm) {
          o <- recipe$outputs[[nm]]
          if (!is.null(o) && !is.null(o$result_symbol)) o$result_symbol
          else paste0("D_", nm)
        }, character(1))
        out <- stats::setNames(symbols, out_names)
        .studio_codegen_plan(p, out, symbol = state$symbol)
      }, error = function(e) "")
    })

    # Copy plan code to clipboard
    shiny::observeEvent(input$copy_plan_code, {
      code <- generated_code()
      if (nchar(code) > 0) {
        session$sendCustomMessage("copyToClipboard", code)
      } else {
        shiny::showNotification("Generate code first.", type = "warning",
                                duration = 2)
      }
    })

    # Reset plan
    shiny::observeEvent(input$reset_plan, {
      state$plan <- ds.omop.plan()
      state$plan_outputs <- list()
      shiny::showNotification("Plan reset.", type = "message", duration = 2)
    })

    output$code_block <- shiny::renderUI({
      code <- generated_code()
      if (nchar(code) == 0) {
        return(.empty_state_ui("code", "No code generated",
          "Add outputs to the plan to generate a reproducible R script."))
      }
      highlighted <- .highlightR(code)
      shiny::div(class = "code-output",
        shiny::HTML(paste0("<pre><code>", highlighted, "</code></pre>"))
      )
    })
  })
}
