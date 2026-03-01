# ==============================================================================
# MODULE: Build (consolidated Recipe + Plan)
# ==============================================================================
# Wraps the Recipe (formerly Basket) and Plan (formerly Plan from Explorer)
# sub-modules into a single navset_pill interface.
# ==============================================================================

# ==============================================================================
# Top-level Build wrapper
# ==============================================================================

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

.mod_build_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_build_recipe_server("recipe", state)
    .mod_build_plan_server("plan", state)
  })
}

# ==============================================================================
# SUB-MODULE: Recipe (formerly Basket)
# ==============================================================================
# Full recipe management: populations, variable blocks, filters (with groups),
# enhanced outputs, variable wizard, JSON import/export, schema preview.
# ==============================================================================

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
                        "Time Since" = "time_since")),
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
                        "Raw" = "raw", "Mean" = "mean")),
          shiny::actionButton(ns("add_block_btn"), "Add Block",
                              class = "btn-primary w-100")
        ),

        # --- Quick Add Filter ---
        bslib::accordion_panel(
          "Add Filter", icon = shiny::icon("filter"),
          shiny::selectInput(ns("filter_type"), "Filter Type",
            choices = c("Sex" = "sex", "Age Group" = "age_group",
                        "Age Range" = "age_range",
                        "Has Concept" = "has_concept",
                        "Date Range" = "date_range")),
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
            shiny::textInput(ns("filter_concept_id"), "Concept ID"),
            shiny::selectInput(ns("filter_table"), "In Table",
              choices = .table_choices(c("condition_occurrence", "drug_exposure",
                          "measurement", "procedure_occurrence")))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'date_range'"),
            shiny::dateInput(ns("date_start"), "Start Date"),
            shiny::dateInput(ns("date_end"), "End Date")
          ),
          shiny::actionButton(ns("add_filter_btn"), "Add Filter",
                              class = "btn-warning w-100")
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
                        "Joined Long (multi-table)" = "joined_long",
                        "Features (sparse)" = "features",
                        "Covariates Sparse" = "covariates_sparse",
                        "Survival" = "survival",
                        "Intervals" = "intervals")),
          shiny::selectInput(ns("output_pop"), "Population",
            choices = c("base")),
          shiny::textInput(ns("output_symbol"), "Result Symbol",
                           placeholder = "D_output_1"),
          shiny::actionButton(ns("add_output_btn"), "Add Output",
                              class = "btn-info w-100")
        )
      ),

      # --- Import/Export (compact row) ---
      shiny::div(class = "d-flex gap-1 mb-2",
        shiny::downloadButton(ns("export_json"), "Export",
          class = "btn-sm btn-outline-secondary flex-fill"),
        shiny::actionButton(ns("import_json_btn"),
          shiny::tagList(shiny::icon("upload"), " Import"),
          class = "btn-sm btn-outline-secondary flex-fill")
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("import_json_btn"), "'] > 0"),
        shiny::fileInput(ns("import_file"), NULL,
                         accept = ".json", buttonLabel = "Choose file")
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
        shiny::actionButton(ns("copy_recipe_code"), NULL,
          icon = shiny::icon("copy"),
          class = "btn-sm btn-outline-secondary")
      ),
      bslib::card_body(
        shiny::uiOutput(ns("recipe_code_html"))
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

    # Update population dropdown when populations change
    shiny::observe({
      pop_ids <- names(state$recipe$populations)
      shiny::updateSelectInput(session, "output_pop", choices = pop_ids)
    })

    # =========================================================================
    # Add Variable (using concept picker)
    # =========================================================================
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
        state$recipe <- recipe_add_variable(state$recipe, v)
        shiny::showNotification(
          paste("Added variable:", v$name), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # =========================================================================
    # Add Variable Block
    # =========================================================================
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

    # =========================================================================
    # Add Filter
    # =========================================================================
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
            if (!is.null(input$age_min) && !is.null(input$age_max) &&
                input$age_min > input$age_max) {
              shiny::showNotification(
                "Min age must be less than or equal to max age",
                type = "warning")
              return()
            }
            omop_filter_age(min = input$age_min, max = input$age_max)
          },
          has_concept = {
            raw_cid <- trimws(input$filter_concept_id)
            if (nchar(raw_cid) == 0 ||
                is.na(suppressWarnings(as.integer(raw_cid)))) {
              shiny::showNotification(
                "Enter a valid numeric concept ID", type = "warning")
              return()
            }
            cid <- as.integer(raw_cid)
            omop_filter_has_concept(cid, input$filter_table)
          },
          date_range = omop_filter_date_range(
            start = as.character(input$date_start),
            end = as.character(input$date_end))
        )
        state$recipe <- recipe_add_filter(state$recipe, f)
        shiny::showNotification(
          paste("Added filter:", f$label), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # =========================================================================
    # Add Output
    # =========================================================================
    shiny::observeEvent(input$add_output_btn, {
      nm <- trimws(input$output_name)
      if (nchar(nm) == 0) {
        shiny::showNotification("Enter an output name", type = "warning")
        return()
      }
      pop_id <- input$output_pop %||% "base"
      sym <- trimws(input$output_symbol %||% "")
      tryCatch({
        o <- omop_output(name = nm, type = input$output_type,
                          population_id = pop_id,
                          result_symbol = if (nchar(sym) > 0) sym else NULL)
        state$recipe <- recipe_add_output(state$recipe, o)
        shiny::showNotification(
          paste("Added output:", nm), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # =========================================================================
    # Add Population (modal)
    # =========================================================================
    shiny::observeEvent(input$add_pop_btn, {
      shiny::showModal(shiny::modalDialog(
        title = "Add Population",
        shiny::textInput(ns("pop_id"), "Population ID",
                         placeholder = "e.g. adults"),
        shiny::textInput(ns("pop_label"), "Label",
                         placeholder = "e.g. Adults 18-65"),
        shiny::selectInput(ns("pop_parent"), "Parent Population",
          choices = names(state$recipe$populations)),
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
        p <- omop_population(id = pid, label = plabel, parent_id = parent)
        state$recipe <- recipe_add_population(state$recipe, p)
        shiny::removeModal()
        shiny::showNotification(
          paste("Added population:", pid), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          .clean_ds_error(e), type = "error")
      })
    })

    # =========================================================================
    # Clear Recipe (with confirmation)
    # =========================================================================
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
      shiny::removeModal()
      shiny::showNotification("Recipe cleared.", type = "message", duration = 2)
    })

    # =========================================================================
    # Auto-generate Plan from Recipe on changes
    # =========================================================================
    shiny::observe({
      recipe <- state$recipe
      has_content <- length(recipe$variables) > 0 || length(recipe$blocks) > 0 ||
                     length(recipe$filters) > 0 || length(recipe$outputs) > 0
      if (has_content) {
        tryCatch({
          plan <- recipe_to_plan(recipe)
          state$plan <- plan
        }, error = function(e) NULL)
      }
    })

    # =========================================================================
    # JSON Export/Import
    # =========================================================================
    output$export_json <- shiny::downloadHandler(
      filename = function() {
        paste0("omop_recipe_", format(Sys.Date(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        recipe_export_json(state$recipe, file = file)
      }
    )

    shiny::observeEvent(input$import_file, {
      req(input$import_file)
      tryCatch({
        imported <- recipe_import_json(input$import_file$datapath)
        state$recipe <- imported
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

    # =========================================================================
    # Recipe Counts Badge
    # =========================================================================
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

    # =========================================================================
    # Population Tree
    # =========================================================================
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

    # =========================================================================
    # Recipe Display (JS delegation for edit/remove buttons)
    # =========================================================================
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
            paste0("[", f$operator, " group]")
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

    # =========================================================================
    # Static remove observer (JS delegation — works for all item types)
    # =========================================================================
    shiny::observeEvent(input$recipe_remove_item, {
      data <- jsonlite::fromJSON(input$recipe_remove_item)
      item_type <- data$type
      item_id <- data$id
      tryCatch({
        if (item_type == "population") {
          state$recipe <- recipe_remove_population(state$recipe, item_id)
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

    # =========================================================================
    # Static edit observer (JS delegation — universal edit modal)
    # =========================================================================
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
          shiny::numericInput(ns("edit_concept_id"), "Concept ID",
            value = v$concept_id),
          shiny::textInput(ns("edit_concept_name"), "Concept Name",
            value = v$concept_name %||% ""),
          shiny::selectInput(ns("edit_type"), "Type",
            choices = c("auto", "numeric", "categorical", "date",
                        "boolean", "integer", "character"),
            selected = v$type),
          shiny::selectInput(ns("edit_format"), "Format",
            choices = c("raw", "binary", "count", "first_value",
                        "last_value", "mean", "min", "max",
                        "time_since", "binned"),
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
            selected = v$suffix_mode)
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
                        "time_since", "binned"),
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
            selected = b$suffix_mode)
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
            shiny::selectInput(ns("edit_filter_level"), "Level",
              choices = c("population", "row", "output"),
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
              shiny::numericInput(ns("edit_filter_cid"), "Concept ID",
                value = f$params$concept_id),
              shiny::selectInput(ns("edit_filter_table"), "Table",
                choices = tbl_choices, selected = f$params$table),
              shiny::numericInput(ns("edit_filter_min_count"),
                "Min Count", value = f$params$min_count %||% 1L,
                min = 1)
            ),
            "date_range" = shiny::tagList(
              shiny::dateInput(ns("edit_date_start"), "Start Date",
                value = f$params$start),
              shiny::dateInput(ns("edit_date_end"), "End Date",
                value = f$params$end)
            ),
            NULL
          )
          shiny::tagList(base_fields, param_fields)
        }
      } else if (item_type == "output") {
        o <- recipe$outputs[[item_id]]
        if (is.null(o)) return()
        shiny::tagList(
          shiny::textInput(ns("edit_name"), "Output Name", value = o$name),
          shiny::selectInput(ns("edit_output_type"), "Type",
            choices = c("wide", "long", "features", "survival",
                        "intervals", "baseline", "joined_long",
                        "covariates_sparse"),
            selected = o$type),
          shiny::selectInput(ns("edit_output_pop"), "Population",
            choices = pop_ids, selected = o$population_id),
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

    # =========================================================================
    # Confirm edit (universal handler)
    # =========================================================================
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
          tw <- if (!is.na(tw_start) && !is.na(tw_end))
            list(start = tw_start, end = tw_end) else NULL
          cid_val <- input$edit_concept_id
          cname_val <- input$edit_concept_name
          vs_val <- input$edit_value_source
          v$table <- input$edit_table
          v$concept_id <- if (!is.null(cid_val) && !is.na(cid_val))
            as.integer(cid_val) else NULL
          v$concept_name <- if (nchar(cname_val) > 0) cname_val else NULL
          v$type <- input$edit_type
          v$format <- input$edit_format
          v$value_source <- if (nchar(vs_val) > 0) vs_val else NULL
          v$time_window <- tw
          v$suffix_mode <- input$edit_suffix_mode
          # Handle rename
          if (new_name != item_id) {
            if (new_name %in% names(recipe$variables)) {
              shiny::showNotification("Name already in use",
                type = "warning", duration = 2)
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
          tw <- if (!is.na(tw_start) && !is.na(tw_end))
            list(start = tw_start, end = tw_end) else NULL
          vs_val <- input$edit_value_source
          ids_text <- trimws(input$edit_concept_ids_text)
          new_ids <- .parse_ids(ids_text)
          b$table <- input$edit_table
          b$concept_ids <- if (!is.null(new_ids)) as.integer(new_ids)
                           else b$concept_ids
          b$format <- input$edit_format
          b$value_source <- if (nchar(vs_val) > 0) vs_val else NULL
          b$time_window <- tw
          b$suffix_mode <- input$edit_suffix_mode
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
              f$params$concept_id <- as.integer(input$edit_filter_cid)
              f$params$table <- input$edit_filter_table
              f$params$min_count <- as.integer(input$edit_filter_min_count)
            } else if (f$type == "date_range") {
              f$params$start <- as.character(input$edit_date_start)
              f$params$end <- as.character(input$edit_date_end)
            }
          }
          recipe$filters[[item_id]] <- f

        } else if (item_type == "output") {
          o <- recipe$outputs[[item_id]]
          if (is.null(o)) { shiny::removeModal(); return() }
          new_name <- trimws(input$edit_name)
          if (nchar(new_name) == 0) new_name <- item_id
          o$type <- input$edit_output_type
          o$population_id <- input$edit_output_pop
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

    # =========================================================================
    # Schema Preview
    # =========================================================================
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

    # =========================================================================
    # Generated Code
    # =========================================================================
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
      code <- recipe_to_code(recipe)
      highlighted <- .highlightR(code)
      shiny::div(class = "code-output",
        shiny::HTML(paste0("<pre><code>", highlighted, "</code></pre>"))
      )
    })
  })
}

# ==============================================================================
# SUB-MODULE: Plan (formerly Plan from Explorer)
# ==============================================================================
# Merges old Plan Builder + Cohorts with explorer integration.
# ==============================================================================

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

    # Set cohort on plan
    shiny::observeEvent(input$set_cohort, {
      cid <- as.integer(input$cohort_id)
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
