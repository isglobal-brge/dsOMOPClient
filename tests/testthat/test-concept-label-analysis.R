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
  tables <- tryCatch(dsBaseClient::ds.table(rvar='Analysis$education',useNA='no',datasources=ctx$conns),error=function(e) stop(paste(c(conditionMessage(e),capture.output(print(DSI::datashield.errors()))),collapse='; ')))
  expect_true(is.list(tables))
  pooled <- tables$output.list$TABLES.COMBINED_all.sources_counts
  expect_equal(as.numeric(pooled),c(24,24,6))
  expect_identical(dimnames(pooled)[[1L]],levels(frames[[1L]]$education)[1:3])
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

