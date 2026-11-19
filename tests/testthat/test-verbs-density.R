# dsOMOP 2.10.8: copies of a person's value cannot loosen DataSHIELD's density
# rule. Levels are weighed against distinct person/value observations, so rows
# stacked, joined or broadcast onto event rows count once. site1 holds 30
# causes over 30 persons, refused by density only. Fixture:
# helper-concept-levels.R.

.dens_deaths <- list(910101:910130, rep(910001:910008, each = 5L), rep(910001:910008, each = 5L))
.dens_column <- "cause_of_death_first_value"

.dens_report <- function(conns, symbol, ...) {
  # DSI captures its expression: build the call first.
  expr <- as.call(c(list(as.name("omopFactorLevelsDS"), as.symbol(symbol)), list(...)))
  DSI::datashield.aggregate(conns["site1"], expr)[[1L]]
}

# Stacks a site1 object onto itself twice: four copies of every row.
.dens_stack <- function(conns, from) {
  DSI::datashield.assign.expr(conns["site1"], "Twice", call("omopBindRowsDS", as.symbol(from), as.symbol(from)))
  DSI::datashield.assign.expr(conns["site1"], "Four", call("omopBindRowsDS", as.symbol("Twice"), as.symbol("Twice")))
  "Four"
}

# Runs `attack` on site1's raw output, before the client reconciles it.
.dens_raw <- function(ctx, attack) {
  seen <- NULL
  testthat::local_mocked_bindings(.harmonizePublicationFactors = function(inventories, conns) {
    raw <- unique(unlist(lapply(inventories, function(x) unlist(x, use.names = FALSE))))[[1L]]
    seen <<- attack(conns, raw)
    stop("stopped before reconciliation")
  }, .package = "dsOMOPClient")
  expect_error(recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)),
    out = c(study = "Raw"), symbol = "cvl", conns = ctx$conns), "stopped before reconciliation")
  seen
}

test_that("a raw output stacked onto itself is refused like its source", {
  ctx <- .cvl_setup(deaths = .dens_deaths, auto_max = "0")
  seen <- .dens_raw(ctx, function(conns, raw) list(source = .dens_report(conns, raw),
    stacked = .dens_report(conns, .dens_stack(conns, raw))))
  expect_true(.dens_column %in% seen$source$unsafe)
  expect_true(.dens_column %in% seen$stacked$unsafe)
  expect_null(seen$stacked$levels[[.dens_column]])
})

test_that("a select copy stacked onto itself is refused like its source", {
  ctx <- .cvl_setup(deaths = .dens_deaths, auto_max = "0")
  seen <- .dens_raw(ctx, function(conns, raw) {
    DSI::datashield.assign.expr(conns["site1"], "Picked",
      call("omopSelectDS", as.symbol(raw), .ds_encode(c("person_id", .dens_column))))
    list(source = .dens_report(conns, "Picked"), stacked = .dens_report(conns, .dens_stack(conns, "Picked")))
  })
  expect_true(.dens_column %in% seen$source$unsafe)
  expect_true(.dens_column %in% seen$stacked$unsafe)
  expect_null(seen$stacked$levels[[.dens_column]])
})

test_that("levels in use of a published column stay refused on stacked copies", {
  ctx <- .cvl_setup(deaths = .dens_deaths)
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)),
    out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  occurring <- function(symbol) .dens_report(ctx$conns, symbol, .ds_encode(list(columns = list("*"))))
  expect_true(.dens_column %in% occurring("D")$unsafe)
  report <- occurring(.dens_stack(ctx$conns, "D"))
  expect_true(.dens_column %in% report$unsafe)
  expect_null(report$occurring[[.dens_column]])
  # Through the client, a stacked copy's model copy equals the source's.
  ds.omop.bind_rows("D", "D", newobj = "DD", symbol = "cvl", conns = ctx$conns)
  ds.omop.bind_rows("DD", "DD", newobj = "DDDD", symbol = "cvl", conns = ctx$conns)
  ds.omop.drop_empty_levels("D", symbol = "cvl", conns = ctx$conns)
  ds.omop.drop_empty_levels("DDDD", symbol = "cvl", conns = ctx$conns)
  expect_identical(.cvl_shared(ctx, "DDDD_model", .dens_column), .cvl_shared(ctx, "D_model", .dens_column))
})

# Ten observation rows per person at every site.
.dens_obs <- omop_event("observation", name = "obs")
.dens_observations <- function(ctx) for (path in ctx$paths) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  persons <- DBI::dbGetQuery(con, "SELECT person_id FROM death")$person_id
  DBI::dbExecute(con, "DELETE FROM observation")
  rows <- expand.grid(position = seq_len(10L), person_id = persons)
  rows$observation_id <- seq_len(nrow(rows)); rows$observation_concept_id <- 0L
  rows$observation_date <- as.character(as.Date("2020-01-01") + rows$position)
  rows$observation_type_concept_id <- 0L; rows$value_as_concept_id <- NA_integer_; rows$position <- NULL
  DBI::dbWriteTable(con, "observation", rows, append = TRUE)
  DBI::dbDisconnect(con)
}

test_that("a join that repeats a person's value is weighed against its people", {
  ctx <- .cvl_setup(deaths = .dens_deaths)
  .dens_observations(ctx)
  recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = FALSE)),
    out = c(study = "D"), symbol = "cvl", conns = ctx$conns)
  counts <- omop_recipe(variables = list(n = omop_variable(event = .dens_obs, reducer = omop_reduce("count"), broadcast = TRUE)),
    outputs = omop_output("long", format = "long", rows = "base", sources = list(.dens_obs), columns = "n"),
    options = list(translate_concepts = FALSE))
  recipe_execute(counts, out = c(long = "Y"), symbol = "cvl", conns = ctx$conns)
  ds.omop.merge("D", "Y", newobj = "M", symbol = "cvl", conns = ctx$conns)
  expect_identical(nrow(.cvl_frames(ctx, "M")$site1), 300L)
  report <- .dens_report(ctx$conns, "M")
  expect_true(.dens_column %in% report$unsafe)
  expect_null(report$levels[[.dens_column]])
})

.dens_long <- omop_recipe(variables = list(cause = omop_variable(event = .cvl_event, reducer = .cvl_first, broadcast = TRUE)),
  outputs = omop_output("long", format = "long", rows = "base", sources = list(.dens_obs), columns = "cause"),
  options = list(translate_concepts = FALSE))

test_that("a person-level value broadcast onto event rows is weighed against its people", {
  ctx <- .cvl_setup(deaths = .dens_deaths, auto_max = "0")
  .dens_observations(ctx)
  # Without automatic lists the column is refused at site1, as in a wide output.
  expect_error(recipe_execute(.dens_long, out = c(long = "Long"), symbol = "cvl", conns = ctx$conns), "refused")
})

test_that("a broadcast column published with its automatic list keeps levels in use refused", {
  ctx <- .cvl_setup(deaths = .dens_deaths)
  .dens_observations(ctx)
  recipe_execute(.dens_long, out = c(long = "Long"), symbol = "cvl", conns = ctx$conns)
  expect_length(.cvl_shared(ctx, "Long", "cause"), 69L)
  occurring <- .dens_report(ctx$conns, "Long", .ds_encode(list(columns = list("*"))))
  expect_true("cause" %in% occurring$unsafe)
})

