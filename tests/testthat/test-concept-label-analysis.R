test_that('A27 pooled ds.table and protected-object ds.glm use aligned categories', {
  testthat::skip_if_not_installed('dsBaseClient')
  testthat::skip_if_not_installed('dsBase')
  # DSLite 1.5's lexer rejects commas and spaces even inside quoted strings
  # (strict=FALSE does not change it). Adapt only quoted-string tokenisation
  # in this test; keep method whitelists and every disclosure check intact.
  lexer <- get('Lexer',asNamespace('DSLite'))
  old_double <- lexer$public_fields$t_DBLSTRING
  old_single <- lexer$public_fields$t_STRING
  withr::defer({ lexer$public_fields$t_DBLSTRING <- old_double; lexer$public_fields$t_STRING <- old_single })
  lexer$public_fields$t_DBLSTRING <- r"("([^"\\]|\\.)*")"
  lexer$public_fields$t_STRING <- r"('([^'\\]|\\.)*')"
  ctx <- .clf_setup()
  p <- recipe_to_plan(.clf_recipe())
  p$variables$age <- list(derived=list(kind='age',reference_date='2025-01-01'))
  ds.omop.plan.execute(p,out=c(output='Analysis'),symbol='labels',conns=ctx$conns)
  frames <- .clf_frames(ctx,'Analysis')
  expect_true(all(vapply(frames,inherits,logical(1),'omop.table')))
  aggregate <- DSI::datashield.aggregate
  testthat::local_mocked_bindings(datashield.aggregate=function(conns,expr,...) {
    tryCatch(aggregate(conns,expr,...),error=function(e) {
      stop(paste('Failed call:',paste(deparse(expr),collapse=' '),conditionMessage(e),
        paste(capture.output(print(DSI::datashield.errors())),collapse='; ')))
    })
  },.package='DSI')
  pooled <- as.table(ds.omop.table('Analysis','education',conns=ctx$conns))
  expected <- Reduce('+',lapply(frames,function(x) table(x$education)))
  names(dimnames(expected)) <- 'Analysis$education'
  expect_equal(pooled,expected)
  expect_equal(as.numeric(pooled),c(24,24,6,0))
  expect_identical(dimnames(pooled)[[1L]],levels(frames[[1L]]$education))
  expect_equal(as.table(ds.omop.table('Analysis','education',conns=rev(ctx$conns))),pooled)
  # No key extraction or class/seal stripping: dsBase receives the published
  # protected table through its normal data= route.
  # A category absent globally is not estimable. The declared recode can
  # intentionally merge that declaration into an observed category.
  p$options$labels <- list('45000123'='education','45000126'='education')
  ds.omop.plan.execute(p,out=c(output='Model'),symbol='labels',conns=ctx$conns)
  fit <- dsBaseClient::ds.glm(formula='age ~ education',data='Model',family='gaussian',datasources=ctx$conns)
  expect_true(is.list(fit))
  expect_true(any(grepl('education',rownames(fit$coefficients))))
  model <- do.call(rbind,.clf_frames(ctx,'Model'))
  reference <- stats::glm(age ~ education,data=model,family=stats::gaussian())
  expect_equal(unname(fit$coefficients[,1L]),unname(stats::coef(reference)),tolerance=1e-6)
})


test_that('table completion restores labels and declared zero categories, and refuses odd counts', {
  counts <- array(c(24, 24, 6), dim = 3, dimnames = list(c('C1', 'C2', 'C3')))
  by_site <- array(c(20, 4, 6, 4, 20, 0), dim = c(3, 2), dimnames = list(c('C1', 'C2', 'C3'), c('site1', 'site2')))
  result <- list(validity.message = 'Data in all studies were valid', output.list = list(
    TABLE_rvar.by.study_row.props = by_site / rowSums(by_site),
    TABLE_rvar.by.study_col.props = sweep(by_site, 2, colSums(by_site), "/"),
    TABLE_rvar.by.study_counts = by_site,
    TABLES.COMBINED_all.sources_proportions = counts / sum(counts),
    TABLES.COMBINED_all.sources_counts = counts))
  labels <- list(C = c('a', 'b', 'c', 'declared'))
  out <- .conceptTableComplete(result, labels)$output.list
  expect_identical(as.numeric(out$TABLES.COMBINED_all.sources_counts), c(24, 24, 6, 0))
  expect_identical(dimnames(out$TABLES.COMBINED_all.sources_counts)[[1L]], labels$C)
  expect_identical(out$TABLE_rvar.by.study_counts['declared', ], c(site1 = 0, site2 = 0))
  expect_identical(colnames(out$TABLE_rvar.by.study_counts), c('site1', 'site2'))
  expect_true(all(is.na(out$TABLE_rvar.by.study_row.props['declared', ])))
  expect_identical(unname(out$TABLE_rvar.by.study_col.props['declared', ]), c(0, 0))
  expect_identical(unname(out$TABLES.COMBINED_all.sources_proportions[['declared']]), 0)
  # As ds.table(): valid sites with the validity message (a failed site is left out, never zero-filled).
  partial <- result; partial$validity.message <- 'At least one study failed'
  expect_identical(.conceptTableComplete(partial, labels)$validity.message, 'At least one study failed')
  odd <- result; odd$output.list$TABLES.COMBINED_all.sources_counts[1L] <- NA_real_
  expect_error(.conceptTableComplete(odd, labels), 'do not match')
  unknown <- result; dimnames(unknown$output.list$TABLES.COMBINED_all.sources_counts)[[1L]][3L] <- 'C9'
  expect_error(.conceptTableComplete(unknown, labels), 'do not match')
  # Two-way tables: first column C, second column D.
  two <- array(c(3, 4, 5, 6), dim = c(2, 2), dimnames = list(c('C1', 'C2'), c('D1', 'D2')))
  both <- .conceptTableComplete(list(validity.message = 'Data in all studies were valid',
    output.list = list(TABLES.COMBINED_all.sources_counts = two)), list(C = c('x', 'y'), D = c('u', 'v', 'w')))
  expect_identical(dimnames(both$output.list$TABLES.COMBINED_all.sources_counts), list(c('x', 'y'), c('u', 'v', 'w')))
  expect_identical(as.numeric(both$output.list$TABLES.COMBINED_all.sources_counts[, 'w']), c(0, 0))
})


test_that('A27 public pooled tables preserve free text and delimiter collisions', {
  testthat::skip_if_not_installed('dsBaseClient')
  testthat::skip_if_not_installed('dsBase')
  lexer <- get('Lexer', asNamespace('DSLite'))
  old_double <- lexer$public_fields$t_DBLSTRING
  old_single <- lexer$public_fields$t_STRING
  withr::defer({ lexer$public_fields$t_DBLSTRING <- old_double; lexer$public_fields$t_STRING <- old_single })
  lexer$public_fields$t_DBLSTRING <- r"("([^"\\]|\\.)*")"
  lexer$public_fields$t_STRING <- r"('([^'\\]|\\.)*')"
  ctx <- .clf_setup()
  conn <- DBI::dbConnect(RSQLite::SQLite(), ctx$paths[[1L]])
  DBI::dbExecute(conn, 'UPDATE observation SET value_as_concept_id = NULL WHERE observation_id IN (1,2,7,8,13,14)')
  DBI::dbDisconnect(conn)
  cases <- list(
    SimpleComma = list('45000123' = 'comma,label', '45000126' = 'declared only'),
    DelimiterCollision = list('45000123' = 'comma,label', '45000126' = 'comma', '45000127' = 'label'),
    FreeText = list('45000123' = 'NA', '45000124' = 'C1',
      '45000125' = ' \\"quoted,\n\u00e9" ', '45000126' = 'declared,only'),
    MergedComma = list('45000123' = 'comma,label', '45000124' = 'comma,label',
      '45000126' = 'comma', '45000127' = 'label'))
  for (symbol in names(cases)) {
    p <- recipe_to_plan(.clf_recipe())
    p$options$labels <- cases[[symbol]]
    ds.omop.plan.execute(p, out = c(output = symbol), symbol = 'labels', conns = ctx$conns)
    frames <- .clf_frames(ctx, symbol)
    expected <- Reduce('+', lapply(frames, function(f) table(f$education)))
    names(dimnames(expected)) <- paste0(symbol, '$education')
    explicit <- switch(symbol,
      SimpleComma = c('comma,label'=22, 'Same [45000124]'=22, 'Same [45000125]'=4, 'declared only'=0),
      DelimiterCollision = c('comma,label'=22, 'Same [45000124]'=22, 'Same [45000125]'=4, comma=0, label=0),
      FreeText = setNames(c(22,22,4,0), unlist(cases[[symbol]], use.names=FALSE)),
      MergedComma = c('comma,label'=44, Same=4, comma=0, label=0))
    inventory <- DSI::datashield.symbols(ctx$conns)
    pooled <- as.table(ds.omop.table(symbol, 'education', ctx$conns))
    expect_equal(pooled, expected, info = symbol)
    expect_equal(setNames(as.numeric(pooled), dimnames(pooled)[[1L]]), explicit, info = symbol)
    expect_equal(sum(pooled), 48, info = symbol)
    expect_identical(vapply(frames, function(f) sum(is.na(f$education)), 0L), c(site1=6L,site2=0L,site3=0L))
    expect_equal(as.table(ds.omop.table(symbol, 'education', rev(ctx$conns))), pooled)
    expect_identical(.clf_frames(ctx, symbol), frames)
    expect_equal(DSI::datashield.symbols(ctx$conns), inventory)
  }
  # Inspect the actual protected copy on the route to dsBase, then simulate
  # count suppression. Cleanup must also run when analysis refuses a result.
  frames <- .clf_frames(ctx, 'DelimiterCollision')
  inventory <- DSI::datashield.symbols(ctx$conns)
  testthat::local_mocked_bindings(ds.table = function(rvar, ...) {
    symbol <- strsplit(rvar, '$', fixed=TRUE)[[1L]][[1L]]
    copies <- .clf_frames(ctx, symbol)
    for (i in seq_along(copies)) {
      expect_s3_class(copies[[i]], 'omop.table')
      expect_identical(copies[[i]]$person_id, frames[[i]]$person_id)
      expect_identical(is.na(copies[[i]]$education), is.na(frames[[i]]$education))
      expect_identical(as.integer(copies[[i]]$education), as.integer(frames[[i]]$education))
      expect_identical(levels(copies[[i]]$education), paste0('C',1:5))
    }
    list(validity.message='At least one study failed')
  }, .package='dsBaseClient')
  # As ds.table(): a failed site is reported in the validity message, never zero-filled.
  refused <- ds.omop.table('DelimiterCollision', 'education', ctx$conns)
  expect_identical(refused$validity.message, 'At least one study failed')
  expect_null(refused$output.list)
  expect_error(as.table(refused), 'At least one study failed')
  expect_equal(DSI::datashield.symbols(ctx$conns), inventory)
  expect_identical(.clf_frames(ctx, 'DelimiterCollision'), frames)
})
