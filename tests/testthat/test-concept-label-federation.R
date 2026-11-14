test_that('A27 three-site education and cause factors pool by identity including zero cells', {
  ctx <- .clf_setup()
  recipe <- .clf_recipe()
  plan <- recipe_to_plan(recipe)
  out <- setNames('A27', names(plan$outputs))
  ds.omop.plan.execute(plan, out = out, symbol = 'labels', conns = ctx$conns)
  frames <- .clf_frames(ctx, 'A27')
  explained <- recipe_explain(recipe, conns=ctx$conns, symbol='labels')
  expect_match(paste(explained$text,collapse='\n'),'vocabulary conflict: New education / Old education')
  expect_match(paste(explained$text,collapse='\n'),'version 2026-01-01')
  expected <- c('New education','Same [45000124]','Same [45000125]','declared only')
  for (f in frames) {
    expect_identical(levels(f$education), expected)
    expect_identical(levels(f$cause), expected)
    expect_equal(unname(table(f$education)['declared only']), 0)
  }
  expect_equal(unname(table(frames$site2$education)['Same [45000125]']), 0)
  pooled <- Reduce('+', lapply(frames, function(f) table(f$education)))
  expect_equal(as.integer(pooled), c(24L,24L,6L,0L))
  # The shared level ordering also gives identical model-matrix columns.
  matrices <- lapply(frames, function(f) stats::model.matrix(~ education, f))
  expect_true(all(vapply(matrices, function(m) identical(colnames(m),colnames(matrices[[1L]])), logical(1))))
  ds.omop.plan.execute(plan, out = setNames('A27reverse', names(plan$outputs)), symbol = 'labels', conns = rev(ctx$conns))
  reversed <- .clf_frames(ctx, 'A27reverse')
  expect_identical(lapply(reversed, function(f) levels(f$education)), lapply(frames, function(f) levels(f$education)))
  ds.omop.plan.execute(plan, out = setNames('A27stage', names(plan$outputs)), symbol = 'labels', conns = ctx$conns, output_mode = 'staged')
  staged <- .clf_frames(ctx, 'A27stage')
  expect_identical(lapply(staged, function(f) levels(f$education)), lapply(frames, function(f) levels(f$education)))
  expect_equal(Reduce('+', lapply(staged, function(f) table(f$education))), pooled)
})

test_that('A27 numeric-only generated names reconcile with declared empty features', {
  ctx <- .clf_setup()
  p <- recipe_to_plan(.clf_recipe())
  source <- names(p$events)[[1L]]
  p$populations$base <- list(kind='index',index=list(source=list(event=source),select='all',end=list(strategy='event_end')))
  p$events$features <- list(table='observation',concept_set=list(concepts=as.list(c(45000123L,45000124L,45000125L))))
  p$variables <- NULL
  p$outputs <- list(output=list(rows='base',format='person_period',options=list(
    sources=list(list(event='features')),bins=list(width=10L),window=list(start=0L,end=9L,anchor='index'),analyses=list('count'))))
  for (mode in c('memory','staged')) {
    symbol <- paste0('Named_',mode)
    ds.omop.plan.execute(p,out=c(output=symbol),symbol='labels',conns=ctx$conns,output_mode=mode)
    frames <- .clf_frames(ctx,symbol)
    expected <- c('features_new_education_count','features_same_count_45000124','features_same_count_45000125')
    for(f in frames) {
      expect_true(all(expected %in% names(f)))
      expect_false(any(vapply(f,is.factor,logical(1))))
      expect_false(any(grepl('45000126',names(f))))
    }
    expect_equal(sum(as.numeric(frames$site2[[expected[[3L]]]])),0)
    expect_equal(sum(as.numeric(frames$site1[[expected[[3L]]]])),6)
  }
})

test_that('A27 legacy aliased baseline and features have staged factor parity', {
  ctx <- .clf_setup()
  recipe <- omop_recipe(variables=list(
    omop_variable('education',table='observation',concept_id=45000120L,format='last_value',value_source='value_as_concept_id'),
    omop_variable('cause',table='death',format='last_value',value_source='cause_concept_id')))
  p <- recipe_to_plan(recipe,version=1L)
  for(mode in c('memory','staged')) {
    out <- setNames(paste0('Legacy_',mode),names(p$outputs))
    ds.omop.plan.execute(p,out=out,symbol='labels',conns=ctx$conns,output_mode=mode)
    frames <- .clf_frames(ctx,unname(out[[1L]]))
    for(f in frames) {
      expect_true(is.factor(f$education))
      expect_true(is.factor(f$cause))
      expect_identical(levels(f$education),c('New education','Same [45000124]','Same [45000125]'))
    }
  }
})

test_that('A27 strict/error/skip reconciliation uses each retained component only', {
  for(strict in c(TRUE,FALSE)) local({
    ctx <- .clf_setup(strict=strict,excluded=TRUE)
    p <- recipe_to_plan(.clf_recipe())
    p$events$few <- list(table='observation',concept_set=list(concepts=list(45000125L)))
    p$populations$few <- list(kind='index',index=list(source=list(event='few'),select='all',end=list(strategy='event_end')))
    p$outputs$small <- list(rows='few',format='wide',grain='episode')
    p$publish <- list('output','small')
    for(policy in c('error','skip')) {
      symbols <- c(output=paste0('Retained_',policy),small=paste0('Small_',policy))
      execute <- function() ds.omop.plan.execute(p,out=symbols,symbol='labels',conns=ctx$conns,on_small_site=policy)
      if(strict && policy=='error') {
        expect_error(execute(),'insufficient_support')
        inventories <- DSI::datashield.symbols(ctx$conns)
        expect_true(all(vapply(inventories,function(x) !any(symbols %in% x),logical(1))))
      } else {
        suppressWarnings(execute())
        one <- ctx$servers$site1$getSessionData(ctx$servers$site1$getSessionIds()[[1L]],symbols[['small']])
        expect_false(any(grepl('Excluded',levels(one$education))))
        expect_false('45000127' %in% attr(one,'omop_concepts')$education$resolved$ids)
        if(policy=='skip') {
          base <- ctx$servers$site1$getSessionData(ctx$servers$site1$getSessionIds()[[1L]],symbols[['output']])
          expect_true('Old education' %in% levels(base$education))
          expect_false(any(grepl('Excluded',levels(base$education))))
        }
      }
    }
  })
})

test_that('A27 incomplete and excessive shared contracts roll back every site', {
  ctx <- .clf_setup()
  original <- .conceptUnionReports
  for(kind in c('incomplete','cardinality')) local({
    testthat::local_mocked_bindings(.conceptUnionReports=function(reports) {
      if(kind=='cardinality') reports[[1L]]$nfilter_levels_max <- 1L
      spec <- original(reports)
      if(kind=='incomplete') spec$columns$education <- NULL
      spec
    })
    p <- recipe_to_plan(.clf_recipe())
    expect_error(ds.omop.plan.execute(p,out=c(output='Rollback'),symbol='labels',conns=ctx$conns,on_small_site='skip'),
      if(kind=='cardinality') 'factor-level cap' else 'recoding failed')
    inventories <- DSI::datashield.symbols(ctx$conns)
    expect_true(all(vapply(inventories,function(x) !'Rollback' %in% x,logical(1))))
  })
})

test_that('A27 changed vocabulary evidence is reconciled again before final assignment', {
  ctx <- .clf_setup()
  original <- .conceptUnionReports; changed <- FALSE
  testthat::local_mocked_bindings(.conceptUnionReports=function(reports) {
    spec <- original(reports)
    if(!changed) {
      conn <- DBI::dbConnect(RSQLite::SQLite(),ctx$paths[['site2']]); on.exit(DBI::dbDisconnect(conn))
      DBI::dbExecute(conn,"UPDATE concept SET concept_name = 'Renewed education' WHERE concept_id = 45000123")
      DBI::dbExecute(conn,"UPDATE vocabulary SET vocabulary_version = '2027-01-01'")
      changed <<- TRUE
    }
    spec
  })
  ds.omop.plan.execute(recipe_to_plan(.clf_recipe()),out=c(output='Renewed'),symbol='labels',conns=ctx$conns)
  for(f in .clf_frames(ctx,'Renewed')) expect_identical(levels(f$education)[[1L]],'Renewed education')
})


 test_that('A27 memory and CSV preserve literal NA labels and genuine missing cells', {
  testthat::local_mocked_bindings(.arrowAvailable=function() FALSE,.package='dsOMOP')
  ctx <- .clf_setup()
  # Six real missing values at site 1, while each observed category still meets support.
  conn <- DBI::dbConnect(RSQLite::SQLite(),ctx$paths[[1L]])
  DBI::dbExecute(conn,'UPDATE observation SET value_as_concept_id = NULL WHERE observation_id IN (1,2,7,8,13,14)')
  DBI::dbDisconnect(conn)
  labels <- c('NA','\\NA','quoted \"text\", comma\nnext line')
  for (i in seq_along(labels)) {
    label <- labels[[i]]
    recipe <- .clf_recipe()
    recipe$options$labels <- c('45000123'=label,'45000126'='declared only')
    p <- recipe_to_plan(recipe)
    memory_symbol <- paste0('CsvMemory',i); staged_symbol <- paste0('CsvStaged',i)
    ds.omop.plan.execute(p,out=c(output=memory_symbol),symbol='labels',conns=ctx$conns)
    ds.omop.plan.execute(p,out=c(output=staged_symbol),symbol='labels',conns=ctx$conns,output_mode='staged')
    mem <- .clf_frames(ctx,memory_symbol); staged <- .clf_frames(ctx,staged_symbol)
    for (site in names(mem)) {
      expect_identical(as.character(staged[[site]]$education),as.character(mem[[site]]$education))
      expect_identical(levels(staged[[site]]$education),levels(mem[[site]]$education))
    }
    pooled <- function(frames) Reduce('+',lapply(frames,function(x) table(x$education,useNA='always')))
    expect_identical(pooled(staged),pooled(mem))
    expect_equal(as.numeric(pooled(staged)),c(22,22,4,0,6))
    expect_identical(dimnames(pooled(staged))[[1L]],c(label,'Same [45000124]','Same [45000125]','declared only',NA_character_))
  }
})
