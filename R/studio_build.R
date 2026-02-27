# ==============================================================================
# MODULE: Build (consolidated Cart + Plan)
# ==============================================================================
# Wraps the Cart (formerly Basket) and Plan (formerly Plan from Explorer)
# sub-modules into a single navset_pill interface.
# ==============================================================================

# ==============================================================================
# Top-level Build wrapper
# ==============================================================================

.mod_build_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_pill(
    id = ns("build_nav"),
    bslib::nav_panel("Builder", icon = shiny::icon("hammer"),
      .mod_build_cart_ui(ns("cart"))),
    bslib::nav_panel("Plan", icon = shiny::icon("drafting-compass"),
      .mod_build_plan_ui(ns("plan")))
  )
}

.mod_build_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    .mod_build_cart_server("cart", state)
    .mod_build_plan_server("plan", state)
  })
}

# ==============================================================================
# SUB-MODULE: Cart (formerly Basket)
# ==============================================================================
# Full cart management: populations, variable blocks, filters (with groups),
# enhanced outputs, variable wizard, JSON import/export, schema preview.
# ==============================================================================

.mod_build_cart_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Builder Actions", width = 320,

      bslib::accordion(
        id = ns("builder_accordion"),
        multiple = FALSE, open = FALSE,

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
      shiny::actionButton(ns("clear_cart"),
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

    # --- Cart Contents ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        shiny::div(class = "d-flex justify-content-between align-items-center",
          shiny::span("Builder Contents"),
          shiny::uiOutput(ns("cart_counts"), inline = TRUE)
        )
      ),
      bslib::card_body(
        shiny::uiOutput(ns("cart_display"))
      )
    ),

    # --- Schema Preview ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Schema Preview"),
      bslib::card_body(
        DT::DTOutput(ns("schema_preview_dt")),
        shiny::uiOutput(ns("schema_info"))
      )
    ),

    # --- Generated Code ---
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Generated Code"),
        shiny::actionButton(ns("copy_cart_code"), NULL,
          icon = shiny::icon("copy"),
          class = "btn-sm btn-outline-secondary")
      ),
      bslib::card_body(
        shiny::uiOutput(ns("cart_code_html"))
      )
    )
  )
}

.mod_build_cart_server <- function(id, state) {
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
      pop_ids <- names(state$cart$populations)
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

      tryCatch({
        v <- omop_variable(
          table = tbl, concept_id = cid,
          concept_name = cname_val, format = fmt
        )
        state$cart <- cart_add_variable(state$cart, v)
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
        state$cart <- cart_add_block(state$cart, b)
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
          .clean_ds_error(e), type = "error")
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
          .clean_ds_error(e), type = "error")
      })
    })

    # =========================================================================
    # Clear Cart
    # =========================================================================
    shiny::observeEvent(input$clear_cart, {
      state$cart <- omop_cart()
      shiny::showNotification("Builder cleared.", type = "message", duration = 2)
    })

    # =========================================================================
    # Auto-generate Plan from Cart on changes
    # =========================================================================
    shiny::observe({
      cart <- state$cart
      has_content <- length(cart$variables) > 0 || length(cart$blocks) > 0 ||
                     length(cart$filters) > 0 || length(cart$outputs) > 0
      if (has_content) {
        tryCatch({
          plan <- cart_to_plan(cart)
          state$plan <- plan
        }, error = function(e) NULL)
      }
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
        indent <- paste0("padding-left: ", (depth * 1.5) + 0.5, "em;")
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
            shiny::div(class = "d-flex gap-1",
              shiny::actionButton(
                ns(paste0("edit_var_", nm)),
                shiny::icon("pen"),
                class = "btn-sm btn-outline-secondary",
                style = "padding: 0.1em 0.4em;"
              ),
              shiny::actionButton(
                ns(paste0("rm_var_", nm)),
                shiny::icon("xmark"),
                class = "btn-sm btn-outline-danger",
                style = "padding: 0.1em 0.4em;"
              )
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
        return(.empty_state_ui("hammer", "Builder is empty",
          "Add variables from the Explore or Vocabulary tabs, or use the sidebar controls."))
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

      # Variable removes + renames
      for (nm in names(cart$variables)) {
        local({
          var_name <- nm
          btn_id <- paste0("rm_var_", var_name)
          edit_id <- paste0("edit_var_", var_name)
          shiny::observeEvent(input[[btn_id]], {
            state$cart <- cart_remove_variable(state$cart, var_name)
          }, ignoreInit = TRUE, once = TRUE)
          shiny::observeEvent(input[[edit_id]], {
            shiny::showModal(shiny::modalDialog(
              title = "Rename Variable",
              shiny::textInput(ns("rename_var_new"), "Variable Name",
                               value = var_name),
              size = "s", easyClose = TRUE,
              footer = shiny::tagList(
                shiny::modalButton("Cancel"),
                shiny::actionButton(ns("rename_var_confirm"), "Rename",
                                    class = "btn-primary")
              )
            ))
            session$userData$renaming_var <- var_name
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
    # Variable Rename (confirm from modal)
    # =========================================================================
    shiny::observeEvent(input$rename_var_confirm, {
      old_name <- session$userData$renaming_var
      new_name <- trimws(input$rename_var_new)
      if (is.null(old_name) || nchar(new_name) == 0) return()
      if (new_name == old_name) { shiny::removeModal(); return() }
      cart <- state$cart
      if (!old_name %in% names(cart$variables)) {
        shiny::removeModal(); return()
      }
      if (new_name %in% names(cart$variables)) {
        shiny::showNotification("Name already in use", type = "warning",
                                duration = 2)
        return()
      }
      cart$variables[[new_name]] <- cart$variables[[old_name]]
      cart$variables[[new_name]]$name <- new_name
      cart$variables[[old_name]] <- NULL
      cart$meta$modified <- Sys.time()
      state$cart <- cart
      shiny::removeModal()
      shiny::showNotification(
        paste0(old_name, " -> ", new_name), type = "message", duration = 2)
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
    shiny::observeEvent(input$copy_cart_code, {
      cart <- state$cart
      if (length(cart$variables) == 0 && length(cart$filters) == 0 &&
          length(cart$outputs) == 0 && length(cart$blocks) == 0) {
        shiny::showNotification("Nothing to copy.", type = "warning",
                                duration = 2)
        return()
      }
      code <- cart_to_code(cart)
      session$sendCustomMessage("copyToClipboard", code)
    })

    output$cart_code_html <- shiny::renderUI({
      cart <- state$cart
      if (length(cart$variables) == 0 && length(cart$filters) == 0 &&
          length(cart$outputs) == 0 && length(cart$blocks) == 0) {
        return(.empty_state_ui("code", "No code yet",
          "Add variables, filters, and outputs to generate code."))
      }
      code <- cart_to_code(cart)
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
        shiny::verbatimTextOutput(ns("plan_summary")),
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

    output$plan_summary <- shiny::renderText({
      p <- state$plan
      paste(utils::capture.output(print(p)), collapse = "\n")
    })

    generated_code <- shiny::reactive({
      p <- state$plan
      if (is.null(p) || length(p$outputs) == 0) return("")
      tryCatch({
        out_names <- names(p$outputs)
        out <- stats::setNames(
          paste0("D_", out_names), out_names
        )
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
