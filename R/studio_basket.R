# ==============================================================================
# MODULE 6: Basket / Cart (Enhanced)
# ==============================================================================
# Full cart management: populations, variable blocks, filters (with groups),
# enhanced outputs, variable wizard, JSON import/export, schema preview.
# ==============================================================================

.mod_basket_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Cart Actions", width = 320,

      # --- Quick Add Variable ---
      bslib::card(
        bslib::card_header("Quick Add Variable"),
        bslib::card_body(
          shiny::selectInput(ns("add_table"), "Table",
            choices = c("condition_occurrence", "drug_exposure",
                        "measurement", "procedure_occurrence",
                        "observation", "visit_occurrence", "person")),
          shiny::textInput(ns("add_concept_id"), "Concept ID",
                           placeholder = "e.g. 201820"),
          shiny::textInput(ns("add_concept_name"), "Concept Name (optional)",
                           placeholder = "e.g. Type 2 diabetes"),
          shiny::selectInput(ns("add_format"), "Format",
            choices = c("Raw" = "raw", "Binary (0/1)" = "binary",
                        "Count" = "count", "First Value" = "first_value",
                        "Last Value" = "last_value", "Mean" = "mean",
                        "Min" = "min", "Max" = "max",
                        "Time Since" = "time_since")),
          shiny::actionButton(ns("add_var_btn"), "Add Variable",
                              class = "btn-primary w-100")
        )
      ),

      # --- Variable Block (batch add) ---
      bslib::card(
        bslib::card_header("Add Variable Block"),
        bslib::card_body(
          shiny::selectInput(ns("block_table"), "Table",
            choices = c("condition_occurrence", "drug_exposure",
                        "measurement", "procedure_occurrence",
                        "observation")),
          shiny::textAreaInput(ns("block_concept_ids"),
            "Concept IDs (comma-separated)",
            placeholder = "201820, 4229440, 316139", rows = 2),
          shiny::selectInput(ns("block_format"), "Format for All",
            choices = c("Binary (0/1)" = "binary", "Count" = "count",
                        "Raw" = "raw", "Mean" = "mean")),
          shiny::actionButton(ns("add_block_btn"), "Add Block",
                              class = "btn-primary w-100")
        )
      ),

      # --- Quick Add Filter ---
      bslib::card(
        bslib::card_header("Quick Add Filter"),
        bslib::card_body(
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
              choices = c("condition_occurrence", "drug_exposure",
                          "measurement", "procedure_occurrence"))
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("filter_type"),
                               "'] == 'date_range'"),
            shiny::dateInput(ns("date_start"), "Start Date"),
            shiny::dateInput(ns("date_end"), "End Date")
          ),
          shiny::actionButton(ns("add_filter_btn"), "Add Filter",
                              class = "btn-warning w-100")
        )
      ),

      # --- Add Output ---
      bslib::card(
        bslib::card_header("Add Output"),
        bslib::card_body(
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
          shiny::actionButton(ns("add_output_btn"), "Add Output",
                              class = "btn-info w-100")
        )
      ),

      shiny::hr(),

      # --- Import/Export ---
      shiny::div(class = "d-flex gap-1 mb-2",
        shiny::downloadButton(ns("export_json"), "Export JSON",
                              class = "btn-sm btn-outline-secondary flex-fill"),
        shiny::actionButton(ns("import_json_btn"), "Import JSON",
                            class = "btn-sm btn-outline-secondary flex-fill")
      ),
      # Hidden file input for import
      shiny::conditionalPanel(
        condition = paste0("input['", ns("import_json_btn"), "'] > 0"),
        shiny::fileInput(ns("import_file"), NULL,
                         accept = ".json",
                         buttonLabel = "Choose file")
      ),

      shiny::hr(),
      shiny::actionButton(ns("clear_cart"), "Clear Cart",
                          class = "btn-outline-danger w-100 mb-2"),
      shiny::actionButton(ns("cart_to_plan_btn"), "Generate Plan from Cart",
                          class = "btn-success w-100")
    ),

    # === Main panel ===

    # --- Population Tree ---
    bslib::card(
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::span("Populations"),
          shiny::actionButton(ns("add_pop_btn"),
            shiny::tagList(shiny::icon("plus"), " Add"),
            class = "btn-sm btn-outline-primary")
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("pop_tree"))
      )
    ),

    # --- Cart Contents ---
    bslib::card(
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::span("Cart Contents"),
          shiny::div(
            shiny::uiOutput(ns("cart_counts"), inline = TRUE),
            shiny::actionButton(ns("wizard_btn"),
              shiny::tagList(shiny::icon("wand-magic-sparkles"), " Wizard"),
              class = "btn-sm btn-outline-info ms-2")
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("cart_display"))
      )
    ),

    # --- Variable Wizard Modal ---
    shiny::tags$div(id = ns("wizard_container")),

    # --- Schema Preview ---
    bslib::card(
      bslib::card_header("Schema Preview"),
      bslib::card_body(
        DT::DTOutput(ns("schema_preview_dt")),
        shiny::uiOutput(ns("schema_info"))
      )
    ),

    # --- Generated Code ---
    bslib::card(
      bslib::card_header("Generated Code"),
      bslib::card_body(
        shiny::div(class = "code-output",
          shiny::verbatimTextOutput(ns("cart_code"))
        )
      )
    )
  )
}

.mod_basket_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate table dropdowns from state$tables
    shiny::observe({
      tbl_choices <- .get_person_tables(state$tables)
      if (length(tbl_choices) > 0) {
        shiny::updateSelectInput(session, "add_table", choices = tbl_choices)
        shiny::updateSelectInput(session, "block_table", choices = tbl_choices)
        shiny::updateSelectInput(session, "filter_table", choices = tbl_choices)
      }
    })

    # Update population dropdown when populations change
    shiny::observe({
      pop_ids <- names(state$cart$populations)
      shiny::updateSelectInput(session, "output_pop", choices = pop_ids)
    })

    # =========================================================================
    # Add Variable
    # =========================================================================
    shiny::observeEvent(input$add_var_btn, {
      tbl <- input$add_table
      cid_text <- trimws(input$add_concept_id)
      cname <- trimws(input$add_concept_name)
      fmt <- input$add_format

      cid <- if (nchar(cid_text) > 0) as.integer(cid_text) else NULL
      cname_val <- if (nchar(cname) > 0) cname else NULL

      tryCatch({
        v <- omop_variable(
          table = tbl, concept_id = cid,
          concept_name = cname_val, format = fmt
        )
        state$cart <- cart_add_variable(state$cart, v)
        shiny::showNotification(
          paste("Added variable:", v$name), type = "message", duration = 2)
        shiny::updateTextInput(session, "add_concept_id", value = "")
        shiny::updateTextInput(session, "add_concept_name", value = "")
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
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
        state$cart <- cart_add_block(state$cart, b)
        shiny::showNotification(
          paste("Added block with", length(ids), "concepts"),
          type = "message", duration = 2)
        shiny::updateTextAreaInput(session, "block_concept_ids", value = "")
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
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
          age_range = omop_filter_age(
            min = input$age_min, max = input$age_max),
          has_concept = {
            cid <- as.integer(trimws(input$filter_concept_id))
            omop_filter_has_concept(cid, input$filter_table)
          },
          date_range = omop_filter_date_range(
            start = as.character(input$date_start),
            end = as.character(input$date_end))
        )
        state$cart <- cart_add_filter(state$cart, f)
        shiny::showNotification(
          paste("Added filter:", f$label), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # =========================================================================
    # Add Output
    # =========================================================================
    shiny::observeEvent(input$add_output_btn, {
      nm <- trimws(input$output_name)
      if (nchar(nm) == 0) nm <- "output_1"
      pop_id <- input$output_pop %||% "base"
      tryCatch({
        o <- omop_output(name = nm, type = input$output_type,
                          population_id = pop_id)
        state$cart <- cart_add_output(state$cart, o)
        shiny::showNotification(
          paste("Added output:", nm), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
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
          choices = names(state$cart$populations)),
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
        state$cart <- cart_add_population(state$cart, p)
        shiny::removeModal()
        shiny::showNotification(
          paste("Added population:", pid), type = "message", duration = 2)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # =========================================================================
    # Variable Wizard (bulk apply format/time_window)
    # =========================================================================
    shiny::observeEvent(input$wizard_btn, {
      cart <- state$cart
      var_names <- names(cart$variables)
      if (length(var_names) == 0) {
        shiny::showNotification("Cart has no variables to edit.",
                                type = "warning")
        return()
      }
      shiny::showModal(shiny::modalDialog(
        title = "Variable Wizard",
        size = "l",
        shiny::p("Select variables and apply settings in bulk."),
        shiny::checkboxGroupInput(ns("wizard_vars"), "Variables",
          choices = var_names, selected = var_names),
        shiny::hr(),
        shiny::selectInput(ns("wizard_format"), "Set Format",
          choices = c("(no change)" = "", "Raw" = "raw",
                      "Binary (0/1)" = "binary", "Count" = "count",
                      "First Value" = "first_value",
                      "Last Value" = "last_value",
                      "Mean" = "mean", "Min" = "min", "Max" = "max",
                      "Time Since" = "time_since")),
        shiny::selectInput(ns("wizard_type"), "Set Type",
          choices = c("(no change)" = "", "Auto" = "auto",
                      "Numeric" = "numeric", "Categorical" = "categorical",
                      "Date" = "date", "Boolean" = "boolean",
                      "Integer" = "integer", "Character" = "character")),
        shiny::textInput(ns("wizard_value_source"), "Set Value Source",
          placeholder = "e.g. value_as_number (blank = no change)"),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("wizard_apply"), "Apply",
                              class = "btn-primary")
        )
      ))
    })

    shiny::observeEvent(input$wizard_apply, {
      selected <- input$wizard_vars
      new_fmt <- input$wizard_format
      new_type <- input$wizard_type
      new_vs <- trimws(input$wizard_value_source)

      cart <- state$cart
      for (nm in selected) {
        if (!nm %in% names(cart$variables)) next
        if (nchar(new_fmt) > 0) cart$variables[[nm]]$format <- new_fmt
        if (nchar(new_type) > 0) cart$variables[[nm]]$type <- new_type
        if (nchar(new_vs) > 0) cart$variables[[nm]]$value_source <- new_vs
      }
      cart$meta$modified <- Sys.time()
      state$cart <- cart

      shiny::removeModal()
      shiny::showNotification(
        paste("Updated", length(selected), "variables"),
        type = "message", duration = 2)
    })

    # =========================================================================
    # Clear Cart
    # =========================================================================
    shiny::observeEvent(input$clear_cart, {
      state$cart <- omop_cart()
      shiny::showNotification("Cart cleared.", type = "message", duration = 2)
    })

    # =========================================================================
    # Generate Plan from Cart
    # =========================================================================
    shiny::observeEvent(input$cart_to_plan_btn, {
      tryCatch({
        plan <- cart_to_plan(state$cart)
        state$plan <- plan
        code <- cart_to_code(state$cart)
        if (nchar(code) > 0) {
          state$script_lines <- c(state$script_lines,
            "# --- Cart to Plan ---", code)
        }
        shiny::showNotification(
          "Plan generated from cart! See Plan Builder tab.",
          type = "message", duration = 3)
      }, error = function(e) {
        shiny::showNotification(
          paste("Error:", conditionMessage(e)), type = "error")
      })
    })

    # =========================================================================
    # JSON Export/Import
    # =========================================================================
    output$export_json <- shiny::downloadHandler(
      filename = function() {
        paste0("omop_cart_", format(Sys.Date(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        cart_export_json(state$cart, file = file)
      }
    )

    shiny::observeEvent(input$import_file, {
      req(input$import_file)
      tryCatch({
        imported <- cart_import_json(input$import_file$datapath)
        state$cart <- imported
        shiny::showNotification(
          paste("Imported cart:",
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
    # Cart Counts Badge
    # =========================================================================
    output$cart_counts <- shiny::renderUI({
      cart <- state$cart
      np <- length(cart$populations)
      nb <- length(cart$blocks)
      nv <- length(cart$variables)
      nf <- length(cart$filters)
      no <- length(cart$outputs)
      shiny::tagList(
        if (np > 1) shiny::span(class = "cart-badge",
          style = "background: #e8daef; color: #6c3483;",
          paste(np, "pop")),
        if (nb > 0) shiny::span(class = "cart-badge",
          style = "background: #d6eaf8; color: #1a5276;",
          paste(nb, "block")),
        shiny::span(class = "cart-badge cart-badge-var",
                    paste(nv, "var")),
        shiny::span(class = "cart-badge cart-badge-filter",
                    paste(nf, "filter")),
        shiny::span(class = "cart-badge cart-badge-output",
                    paste(no, "output"))
      )
    })

    # =========================================================================
    # Population Tree
    # =========================================================================
    output$pop_tree <- shiny::renderUI({
      cart <- state$cart
      pops <- cart$populations
      if (length(pops) == 0) return(shiny::p("No populations."))

      # Build tree (simple indented list)
      .render_pop_node <- function(pid, depth = 0) {
        p <- pops[[pid]]
        indent <- paste0("padding-left: ", depth * 1.5, "em;")
        children_ids <- names(pops)[vapply(pops, function(pp) {
          identical(pp$parent_id, pid)
        }, logical(1))]

        node <- shiny::div(
          style = indent,
          class = "cart-item d-flex justify-content-between align-items-center",
          shiny::div(
            shiny::span(class = "cart-item-name",
              if (depth > 0) shiny::icon("arrow-right", class = "me-1"),
              pid
            ),
            shiny::br(),
            shiny::span(class = "cart-item-meta", p$label)
          ),
          if (pid != "base") {
            shiny::actionButton(
              ns(paste0("rm_pop_", pid)),
              shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 0.1em 0.4em;"
            )
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
    # Cart Display
    # =========================================================================
    output$cart_display <- shiny::renderUI({
      cart <- state$cart
      sections <- list()

      # --- Blocks section ---
      if (length(cart$blocks) > 0) {
        block_items <- lapply(names(cart$blocks), function(bid) {
          b <- cart$blocks[[bid]]
          shiny::div(
            class = "cart-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "cart-item-name", bid),
              shiny::br(),
              shiny::span(class = "cart-item-meta",
                paste0(b$table, " | ", length(b$concept_ids),
                       " concepts | ", b$format))
            ),
            shiny::actionButton(
              ns(paste0("rm_block_", bid)),
              shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 0.1em 0.4em;"
            )
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "cart-section-header",
            shiny::icon("layer-group"), " Variable Blocks"),
          shiny::tagList(block_items)
        ))
      }

      # --- Variables section ---
      if (length(cart$variables) > 0) {
        var_items <- lapply(names(cart$variables), function(nm) {
          v <- cart$variables[[nm]]
          concept_text <- if (!is.null(v$concept_id))
            paste0("concept ", v$concept_id,
                   if (!is.null(v$concept_name))
                     paste0(" (", substr(v$concept_name, 1, 30), ")")
                   else "") else ""
          tw_text <- if (!is.null(v$time_window))
            paste0(" | window: ", v$time_window$start, " to ",
                   v$time_window$end) else ""
          shiny::div(
            class = "cart-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "cart-item-name", nm),
              shiny::br(),
              shiny::span(class = "cart-item-meta",
                paste0(v$table, " | ", v$format,
                       if (nchar(concept_text) > 0)
                         paste0(" | ", concept_text) else "",
                       tw_text))
            ),
            shiny::actionButton(
              ns(paste0("rm_var_", nm)),
              shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 0.1em 0.4em;"
            )
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "cart-section-header",
            shiny::icon("table-columns"), " Variables (",
            length(cart$variables), ")"),
          shiny::tagList(var_items)
        ))
      }

      # --- Filters section ---
      if (length(cart$filters) > 0) {
        filter_items <- lapply(names(cart$filters), function(fid) {
          f <- cart$filters[[fid]]
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
            class = "cart-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "cart-item-name", f$label, safety_badge),
              shiny::br(),
              shiny::span(class = "cart-item-meta", type_text)
            ),
            shiny::actionButton(
              ns(paste0("rm_filter_", fid)),
              shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 0.1em 0.4em;"
            )
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "cart-section-header",
            shiny::icon("filter"), " Filters"),
          shiny::tagList(filter_items)
        ))
      }

      # --- Outputs section ---
      if (length(cart$outputs) > 0) {
        output_items <- lapply(names(cart$outputs), function(nm) {
          o <- cart$outputs[[nm]]
          shiny::div(
            class = "cart-item d-flex justify-content-between align-items-center",
            shiny::div(
              shiny::span(class = "cart-item-name", nm),
              shiny::br(),
              shiny::span(class = "cart-item-meta",
                          paste0("Type: ", o$type,
                                 " | pop: ", o$population_id))
            ),
            shiny::actionButton(
              ns(paste0("rm_output_", nm)),
              shiny::icon("xmark"),
              class = "btn-sm btn-outline-danger",
              style = "padding: 0.1em 0.4em;"
            )
          )
        })
        sections <- c(sections, list(
          shiny::div(class = "cart-section-header",
            shiny::icon("table"), " Outputs"),
          shiny::tagList(output_items)
        ))
      }

      if (length(sections) == 0) {
        return(shiny::p(shiny::em(
          "Cart is empty. Add variables from the Explore or Vocabulary tabs,",
          " or use the sidebar controls.")))
      }

      shiny::div(shiny::tagList(sections))
    })

    # =========================================================================
    # Dynamic remove buttons
    # =========================================================================
    shiny::observe({
      cart <- state$cart

      # Population removes
      for (pid in names(cart$populations)) {
        if (pid == "base") next
        local({
          pop_id <- pid
          btn_id <- paste0("rm_pop_", pop_id)
          shiny::observeEvent(input[[btn_id]], {
            state$cart <- cart_remove_population(state$cart, pop_id)
          }, ignoreInit = TRUE, once = TRUE)
        })
      }

      # Block removes
      for (bid in names(cart$blocks)) {
        local({
          block_id <- bid
          btn_id <- paste0("rm_block_", block_id)
          shiny::observeEvent(input[[btn_id]], {
            state$cart$blocks[[block_id]] <- NULL
            state$cart$meta$modified <- Sys.time()
          }, ignoreInit = TRUE, once = TRUE)
        })
      }

      # Variable removes
      for (nm in names(cart$variables)) {
        local({
          var_name <- nm
          btn_id <- paste0("rm_var_", var_name)
          shiny::observeEvent(input[[btn_id]], {
            state$cart <- cart_remove_variable(state$cart, var_name)
          }, ignoreInit = TRUE, once = TRUE)
        })
      }

      # Filter removes
      for (fid in names(cart$filters)) {
        local({
          filter_id <- fid
          btn_id <- paste0("rm_filter_", filter_id)
          shiny::observeEvent(input[[btn_id]], {
            state$cart <- cart_remove_filter(state$cart, filter_id)
          }, ignoreInit = TRUE, once = TRUE)
        })
      }

      # Output removes
      for (nm in names(cart$outputs)) {
        local({
          out_name <- nm
          btn_id <- paste0("rm_output_", out_name)
          shiny::observeEvent(input[[btn_id]], {
            state$cart <- cart_remove_output(state$cart, out_name)
          }, ignoreInit = TRUE, once = TRUE)
        })
      }
    })

    # =========================================================================
    # Schema Preview
    # =========================================================================
    output$schema_preview_dt <- DT::renderDT({
      cart <- state$cart
      if (length(cart$outputs) == 0 || length(cart$variables) == 0)
        return(NULL)
      schemas <- cart_preview_schema(cart)
      all_dfs <- lapply(names(schemas), function(nm) schemas[[nm]])
      df <- do.call(rbind, all_dfs)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      DT::datatable(df,
        options = list(pageLength = 20, dom = "ftip", scrollX = TRUE),
        rownames = FALSE, selection = "none")
    })

    output$schema_info <- shiny::renderUI({
      cart <- state$cart
      if (length(cart$outputs) == 0) return(NULL)
      schemas <- cart_preview_schema(cart)
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
    output$cart_code <- shiny::renderText({
      cart <- state$cart
      if (length(cart$variables) == 0 && length(cart$filters) == 0 &&
          length(cart$outputs) == 0 && length(cart$blocks) == 0) {
        return("# Cart is empty. Add variables, filters, and outputs.")
      }
      cart_to_code(cart)
    })
  })
}
