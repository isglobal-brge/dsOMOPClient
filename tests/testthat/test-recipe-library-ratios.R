test_that("documented adherence ratios preserve keys, missing denominators and protected mutation", {
  # This is the separate verb example in the T13 README, not another template.
  spec <- .library_variant_specs()[["drug.adherence_components/union_4_of_4"]]
  ctx <- .library_cdm_setup(two_sites = TRUE, mutate = spec$mutate)
  recipe_execute(.library_variant_recipe(spec), out = c(adherence = "D_adherence"),
    symbol = ctx$symbol, conns = ctx$conns)
  original <- .library_frames(ctx, "D_adherence")
  ratios <- function(input, output) ds.omop.mutate(input,
    pdc = ifelse(observed_days > 0, effective_covered_days / observed_days, NA_real_),
    mpr = ifelse(observed_days > 0, supply_days / observed_days, NA_real_),
    newobj = output, symbol = ctx$symbol, conns = ctx$conns)
  expect_identical(ratios("D_adherence", "D_ratios"), "D_ratios")
  assigned <- .library_frames(ctx, "D_ratios")
  keys <- c("person_id", "row_id", "cohort_row_id")
  for (site in names(assigned)) {
    expect_equal(assigned[[site]]$pdc, rep(1, 24)) # 4/4
    expect_equal(assigned[[site]]$mpr, rep(2.5, 24)) # 10/4; never clamped to 1
    for (key in keys) expect_identical(assigned[[site]][[key]], original[[site]][[key]])
    expect_null(attr(assigned[[site]], "dsomop_dp_provenance"))
  }
  ds.omop.mutate("D_adherence", observed_days = 0L, newobj = "D_zero",
    symbol = ctx$symbol, conns = ctx$conns)
  ds.omop.mutate("D_adherence", observed_days = NA_integer_, newobj = "D_unknown",
    symbol = ctx$symbol, conns = ctx$conns)
  for (input in c("D_zero", "D_unknown")) {
    output <- paste0(input, "_ratios")
    expect_identical(ratios(input, output), output)
    frames <- .library_frames(ctx, output)
    for (site in names(frames)) {
      expect_true(all(is.na(frames[[site]]$pdc)))
      expect_true(all(is.na(frames[[site]]$mpr)))
      for (key in keys) expect_identical(frames[[site]][[key]], original[[site]][[key]])
    }
  }
  # The verb's ordinary final publication boundary still protects keys and
  # restores the existing assigned result at both sites on a refused edit.
  inventory <- DSI::datashield.symbols(ctx$conns)
  expect_error(ds.omop.mutate("D_ratios", person_id = 1L, newobj = "D_ratios",
    symbol = ctx$symbol, conns = ctx$conns), "left unchanged")
  expect_identical(.library_frames(ctx, "D_ratios"), assigned)
  expect_identical(DSI::datashield.symbols(ctx$conns), inventory)
})
