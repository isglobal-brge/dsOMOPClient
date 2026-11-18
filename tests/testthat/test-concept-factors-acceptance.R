# Three-site publication acceptance for concept categories. Session inspection
# and database edits below are test-only; no clinical rows cross the client API.
.cf_setup <- function(env = parent.frame(), values = NULL) {
  ctx <- .clf_setup(env = env)
  .numeric_lexer(env)
  if (is.null(values)) values <- list(
    c(rep(45000123L, 16L), 45000124L, 45000125L),
    c(rep(45000123L, 15L), 45000124L, 45000125L, 45000127L),
    c(rep(45000123L, 17L), 45000124L))
  for (i in seq_along(ctx$paths)) {
    db <- DBI::dbConnect(RSQLite::SQLite(), ctx$paths[[i]])
    DBI::dbExecute(db, "DELETE FROM death")
    DBI::dbWriteTable(db, "death", data.frame(person_id = seq_along(values[[i]]),
      death_date = "2021-01-01", death_type_concept_id = 0L,
      cause_concept_id = values[[i]]), append = TRUE)
    DBI::dbExecute(db, paste0("INSERT INTO concept SELECT 45000127, 'Additional category', ",
      "domain_id, vocabulary_id, concept_class_id, standard_concept, '45000127', ",
      "valid_start_date, valid_end_date, invalid_reason FROM concept WHERE concept_id = 45000124"))
    DBI::dbDisconnect(db)
    ctx$servers[[i]]$option("nfilter.levels.density", "0.33")
  }
  ctx$values <- values
  ctx
}

.cf_recipe <- function(translate = TRUE, spec = NULL, labels = NULL) {
  variable <- omop_variable("cause", event = omop_event("death"),
    reducer = omop_reduce("first_value", value = "cause_concept_id"))
  if (!is.null(spec)) variable <- omop_category(variable, spec)
  omop_recipe(variables = list(variable), outputs = omop_output("study", type = "wide"),
    labels = labels, options = list(translate_concepts = translate))
}

.cf_shared <- function(ctx, symbol, column, expected = NULL) {
  frames <- .clf_frames(ctx, symbol)
  reference <- levels(frames[[1L]][[column]])
  for (frame in frames) {
    expect_s3_class(frame[[column]], "factor", info = paste(symbol, column))
    expect_false(is.numeric(frame[[column]]), info = paste(symbol, column))
    expect_identical(levels(frame[[column]]), reference)
    if (!is.null(expected)) expect_identical(levels(frame[[column]]), expected)
  }
  invisible(frames)
}

.cf_refusal <- function(ctx, recipe, symbol, rule, mode = "memory") {
  error <- tryCatch({
    recipe_execute(recipe, out = symbol, symbol = "labels", conns = ctx$conns,
      output_mode = mode)
    NULL
  }, error = identity)
  expect_s3_class(error, "error")
  if (inherits(error, "error")) {
    message <- conditionMessage(error)
    for (part in c("cause", "site2", rule, "declare", "omop_category_recode",
                   "omop_category_rollup", "concepts")) {
      expect_match(tolower(message), tolower(part), fixed = TRUE)
    }
    expect_false(grepl("[0-9]+ (levels|rows|persons)|counts?[:=]", message))
  }
  inventory <- DSI::datashield.symbols(ctx$conns)
  expect_true(all(vapply(inventory, function(x) !symbol %in% x, logical(1))))
}

test_that("concept factors admit rare levels with identical translated and ID-labelled contracts", {
  ctx <- .cf_setup()
  for (translate in c(TRUE, FALSE)) for (mode in c("memory", "staged")) {
    target <- paste0("Rare_", translate, "_", mode)
    recipe <- .cf_recipe(translate)
    result <- recipe_execute(recipe, out = target, symbol = "labels", conns = ctx$conns,
      output_mode = mode)
    frames <- .cf_shared(ctx, target, "cause", if (!translate)
      as.character(c(45000123L, 45000124L, 45000125L, 45000127L)))
    matrices <- lapply(frames, function(frame) stats::model.matrix(~ cause, frame))
    expect_true(all(vapply(matrices, function(x)
      identical(colnames(x), colnames(matrices[[1L]])), logical(1))))
    if (!translate) for (i in seq_along(frames))
      expect_identical(as.character(frames[[i]]$cause), as.character(ctx$values[[i]]))
    for (book in list(recipe_codebook(recipe), recipe_codebook(result))) {
      columns <- book[[1L]]$columns
      expect_identical(columns$r_type[columns$name == "cause"], "factor")
    }
    expect_match(paste(recipe_explain(recipe)$text, collapse = "\n"), "factor")
  }
})

test_that("observed max and density refusals name the rule and revoke all sites", {
  ctx <- .cf_setup()
  for (rule in c("nfilter.levels.max", "nfilter.levels.density")) {
    ctx$servers$site2$option("nfilter.levels.max", if (rule == "nfilter.levels.max") "3" else "40")
    ctx$servers$site2$option("nfilter.levels.density", if (rule == "nfilter.levels.density") "0.1" else "0.33")
    for (translate in c(TRUE, FALSE)) for (mode in c("memory", "staged"))
      .cf_refusal(ctx, .cf_recipe(translate), paste0("Refused_", translate, "_", mode), rule, mode)
  }
})

test_that("observed density uses output rows including missing values", {
  ctx <- .cf_setup(values = rep(list(c(45000123L, 45000124L, rep(NA_integer_, 16L))), 3L))
  for (mode in c("memory", "staged")) {
    recipe_execute(.cf_recipe(FALSE), out = paste0("Missing_", mode), symbol = "labels",
      conns = ctx$conns, output_mode = mode)
    frames <- .cf_shared(ctx, paste0("Missing_", mode), "cause", c("45000123", "45000124"))
    for (frame in frames) expect_equal(sum(is.na(frame$cause)), 16L)
  }
})

test_that("a shared union larger than any site cap remains assignable", {
  ctx <- .cf_setup(values = list(c(rep(45000123L, 17L), 45000124L),
    c(rep(45000125L, 17L), 45000126L), c(rep(45000127L, 17L), 45000128L)))
  for (server in ctx$servers) server$option("nfilter.levels.max", "2")
  for (translate in c(TRUE, FALSE)) for (mode in c("memory", "staged")) {
    target <- paste0("Union_", translate, "_", mode)
    recipe_execute(.cf_recipe(translate), out = target, symbol = "labels",
      conns = ctx$conns, output_mode = mode)
    frames <- .cf_shared(ctx, target, "cause")
    expect_length(levels(frames[[1L]]$cause), 6L)
  }
})

test_that("complete recodes admit rare sources and report no unmatched observed identities", {
  ctx <- .cf_setup()
  for (server in ctx$servers) {
    server$option("nfilter.levels.max", "1")
    server$option("nfilter.levels.density", "0.01")
  }
  spec <- omop_category_spec(recode = omop_category_recode(
    c(`concept:45000123` = "category:a", `concept:45000124` = "category:b",
      `concept:45000125` = "category:b"),
    levels = c("category:a", "category:b", "category:unused"),
    labels = c("Group A", "Group B", "Unused"), unmatched = "missing"))
  for (mode in c("memory", "staged")) {
    target <- paste0("Recoded_", mode)
    recipe_execute(.cf_recipe(spec = spec), out = target, symbol = "labels",
      conns = ctx$conns, output_mode = mode)
    frames <- .cf_shared(ctx, target, "cause", c("Group A", "Group B", "Unused"))
    expect_equal(sum(is.na(frames$site2$cause)), 1L)
    reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol(target)))
    for (report in reports) {
      expect_length(report$unsafe, 0L)
      expect_false(grepl("45000127", jsonlite::toJSON(report, auto_unbox = TRUE), fixed = TRUE))
      expect_identical(report$levels$cause, reports[[1L]]$levels$cause)
    }
  }
})

test_that("declared levels and label maps report their public domain independent of occurrence", {
  ctx <- .cf_setup()
  for (server in ctx$servers) {
    server$option("nfilter.levels.max", "1")
    server$option("nfilter.levels.density", "0.01")
  }
  ids <- as.character(c(45000123L, 45000124L, 45000125L, 45000126L, 45000127L))
  recipes <- list(
    levels = .cf_recipe(spec = omop_category_spec(levels = paste0("concept:", ids))),
    labels = .cf_recipe(labels = setNames(paste("Declared", seq_along(ids)), ids)))
  for (kind in names(recipes)) for (mode in c("memory", "staged")) {
    target <- paste0("Declared_", kind, "_", mode)
    recipe_execute(recipes[[kind]], out = target, symbol = "labels", conns = ctx$conns,
      output_mode = mode)
    .cf_shared(ctx, target, "cause")
    reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol(target)))
    for (report in reports) {
      expect_length(report$unsafe, 0L)
      expect_setequal(report$levels$cause, if (kind == "levels") paste0("concept:", ids) else ids)
      expect_identical(report$levels$cause, reports[[1L]]$levels$cause)
      expect_null(report$observed_ids)
    }
  }
})

test_that("partial declarations and unmatched keep still discover observed categories", {
  ctx <- .cf_setup()
  ctx$servers$site2$option("nfilter.levels.max", "2")
  .cf_refusal(ctx, .cf_recipe(labels = c(`45000123` = "Declared")),
    "PartialLabels", "nfilter.levels.max")
  spec <- omop_category_spec(recode = omop_category_recode(
    c(`concept:45000123` = "category:a"), levels = "category:a", unmatched = "keep"))
  .cf_refusal(ctx, .cf_recipe(spec = spec), "UnmatchedKeep", "nfilter.levels.max")
})

test_that("binary categories report their declared domain without discovery", {
  ctx <- .cf_setup()
  for (server in ctx$servers) {
    server$option("nfilter.levels.max", "1")
    server$option("nfilter.levels.density", "0.01")
  }
  recipe <- omop_recipe(variables = list(flag = omop_category(
    omop_variable(event = omop_event("death"), reducer = omop_reduce("binary")),
    omop_category_spec(binary = omop_category_binary("factor")))))
  recipe_execute(recipe, out = "DeclaredFlag", symbol = "labels", conns = ctx$conns)
  .cf_shared(ctx, "DeclaredFlag", "flag", c("no", "yes"))
  reports <- DSI::datashield.aggregate(ctx$conns,
    call("omopFactorLevelsDS", as.symbol("DeclaredFlag")))
  for (report in reports) {
    expect_length(report$unsafe, 0L)
    expect_identical(report$levels$flag, reports[[1L]]$levels$flag)
    expect_length(report$levels$flag, 2L)
  }
})

.cf_format_recipe <- function() {
  event <- omop_event("observation", 45000120L, name = "e")
  cause <- omop_variable(event = event,
    reducer = omop_reduce("first_value", value = "value_as_concept_id"), broadcast = TRUE)
  outputs <- list(
    omop_output("wide", format = "wide", rows = "p", columns = "cause"),
    omop_output("baseline", type = "baseline", rows = "p", columns = "cause"),
    omop_output("features", type = "features", rows = "p", columns = "cause"),
    omop_output("long", format = "long", rows = "p", sources = list(event), columns = "cause"),
    omop_output("intervals", format = "intervals", rows = "p", columns = "category",
      options = list(sources = list(event))),
    omop_output("membership", format = "membership", rows = "p", columns = character()),
    omop_output("dictionary", format = "dictionary", rows = "p", options = list(source_outputs = "long")),
    omop_output("panel", format = "person_period", rows = "p", columns = "cause",
      options = list(sources = list(event), window = omop_window(0, 20), bins = list(width = 10), analyses = "count")),
    omop_output("sparse", format = "sparse", rows = "p", columns = character(),
      options = list(sources = list(event), analyses = "binary")),
    omop_output("temporal", format = "temporal_covariates", rows = "p",
      options = list(sources = list(event), analyses = "binary", bin_width = 365L, window = omop_window(0, 365))))
  for (style in c("survival", "competing_risk", "counting_process", "recurrent_events", "multi_state")) {
    options <- list(format = style, outcomes = list(lab = event), tar = omop_window(0, 365),
      censor = list("observation_end"))
    if (style == "multi_state") {
      options$transitions <- list(list(from = "index", to = "lab"))
      options$initial_state <- "index"
    }
    outputs[[length(outputs) + 1L]] <- omop_output(style, format = "survival", rows = "p",
      columns = "cause", options = options)
  }
  outputs[[length(outputs) + 1L]] <- omop_output("joined", format = "joined", rows = "p", grain = "episode",
    options = list(from = list(list(output = "membership"), list(output = "wide")), cardinality = "one_to_one"))
  omop_recipe(events = list(event), populations = omop_population("p", index = omop_index(event)),
    variables = list(cause = cause, category = omop_category_rows(event, "value_as_concept_id")),
    outputs = outputs, options = list(translate_concepts = FALSE))
}

test_that("every output layout preserves concept factors in memory and staged publication", {
  ctx <- .cf_setup()
  recipe <- .cf_format_recipe()
  for (mode in c("memory", "staged")) {
    targets <- setNames(paste0("Format_", names(recipe$outputs), "_", mode), names(recipe$outputs))
    result <- recipe_execute(recipe, out = targets, symbol = "labels", conns = ctx$conns, output_mode = mode)
    books <- recipe_codebook(result)
    observed <- character()
    for (key in names(books)) {
      parts <- strsplit(key, "/", fixed = TRUE)[[1L]]
      symbol <- paste0(targets[[parts[[1L]]]], if (parts[[2L]] != parts[[1L]]) paste0(".", parts[[2L]]))
      columns <- books[[key]]$columns
      concepts <- columns$name[columns$semantic_type == "concept"]
      for (column in concepts) {
        .cf_shared(ctx, symbol, column)
        expect_identical(columns$r_type[columns$name == column], "factor", info = key)
        observed <- union(observed, parts[[1L]])
      }
    }
    expect_true(all(c("wide", "baseline", "features", "long", "intervals", "panel", "sparse", "temporal",
      "survival", "competing_risk", "counting_process", "recurrent_events", "multi_state", "joined") %in% observed))
  }
})

test_that("legacy untagged reports refuse instead of leaving concept IDs numeric", {
  ctx <- .cf_setup()
  recipe_execute(.cf_recipe(FALSE), out = "Untagged", symbol = "labels", conns = ctx$conns)
  for (server in ctx$servers) {
    session <- server$getSession(server$getSessionIds()[[1L]])
    frame <- server$getSessionData(server$getSessionIds()[[1L]], "Untagged")
    frame$cause_concept_id <- as.numeric(as.character(frame$cause))
    frame$cause <- NULL
    for (name in c("omop_concepts", "omop_generated_columns", "omop_numbers", "omop_codebook_schema"))
      attr(frame, name) <- NULL
    attr(frame, "omop_concept_cols") <- "cause_concept_id"
    assign("Untagged", frame, envir = session)
  }
  ctx$servers$site2$option("nfilter.levels.max", "3")
  reports <- DSI::datashield.aggregate(ctx$conns, call("omopFactorLevelsDS", as.symbol("Untagged")))
  expect_true("cause_concept_id" %in% reports$site2$unsafe)
  expect_null(reports$site2$levels$cause_concept_id)
  expect_error(dsOMOPClient:::.unionConceptLevels(reports), "cause_concept_id")
})

test_that("factor_concepts FALSE is rejected for both label policies", {
  for (translate in c(TRUE, FALSE)) {
    recipe <- .cf_recipe(translate)
    recipe$options$factor_concepts <- FALSE
    expect_error(recipe_to_plan(recipe), "concept IDs are always categories since 2[.]10[.]6")
    recipe$options$factor_concepts <- TRUE
    expect_silent(recipe_to_plan(recipe))
  }
})
