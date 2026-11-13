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
  pooled <- ds.omop.table('Analysis','education',conns=ctx$conns)
  expected <- Reduce('+',lapply(frames,function(x) table(x$education)))
  names(dimnames(expected)) <- 'Analysis$education'
  expect_equal(pooled,expected)
  expect_equal(as.numeric(pooled),c(24,24,6,0))
  expect_identical(dimnames(pooled)[[1L]],levels(frames[[1L]]$education))
  expect_equal(ds.omop.table('Analysis','education',conns=rev(ctx$conns)),pooled)
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


 test_that('A27 table completion refuses partial and suppressed dsBase results', {
  counts <- array(c(24,24,6),dim=3,dimnames=list(c('a','b','c')))
  result <- list(validity.message='Data in all studies were valid',
    output.list=list(TABLES.COMBINED_all.sources_counts=counts))
  complete <- .conceptCompleteTable(result,c('a','b','c','declared'),'x')
  expect_identical(as.numeric(complete),c(24,24,6,0))
  expect_identical(dimnames(complete)[[1L]],c('a','b','c','declared'))
  partial <- result; partial$validity.message <- 'At least one study failed'
  expect_error(.conceptCompleteTable(partial,c('a','b','c','declared'),'x'),'Every retained site')
  suppressed <- result; suppressed$output.list[[1L]][1L] <- NA_real_
  expect_error(.conceptCompleteTable(suppressed,c('a','b','c','declared'),'x'),'do not match')
  expect_error(.conceptCompleteTable(result,c('a','b','declared'),'x'),'do not match')
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
    pooled <- ds.omop.table(symbol, 'education', ctx$conns)
    expect_equal(pooled, expected, info = symbol)
    expect_equal(setNames(as.numeric(pooled), dimnames(pooled)[[1L]]), explicit, info = symbol)
    expect_equal(sum(pooled), 48, info = symbol)
    expect_identical(vapply(frames, function(f) sum(is.na(f$education)), 0L), c(site1=6L,site2=0L,site3=0L))
    expect_equal(ds.omop.table(symbol, 'education', rev(ctx$conns)), pooled)
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
  expect_error(ds.omop.table('DelimiterCollision', 'education', ctx$conns), 'Every retained site')
  expect_equal(DSI::datashield.symbols(ctx$conns), inventory)
  expect_identical(.clf_frames(ctx, 'DelimiterCollision'), frames)
})
