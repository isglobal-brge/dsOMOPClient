test_that('A27 baseline and long streams use factors in memory and staged output', {
  ctx <- .clf_setup()
  p <- ds.omop.plan.cohort(ds.omop.plan(),cohort_definition_id=1L)
  p <- ds.omop.plan.baseline(p,columns=c(education='gender_concept_id'),derived=character())
  for(mode in c('memory','staged')) {
    target <- paste0('Baseline_',mode)
    ds.omop.plan.execute(p,out=c(baseline=target),symbol='labels',conns=ctx$conns,output_mode=mode)
    frames <- .clf_frames(ctx,target)
    expect_true(all(vapply(frames,function(f) is.factor(f$education),logical(1))))
  }
  long <- ds.omop.plan()
  long$outputs <- list(events=list(type='event_level',table='observation',columns='value_as_concept_id',
    filters=list(concept_set=list(ids=45000120L)),representation=list(format='long')))
  for(mode in c('memory','staged')) {
    target <- paste0('Long_',mode)
    ds.omop.plan.execute(long,out=c(events=target),symbol='labels',conns=ctx$conns,output_mode=mode)
    frames <- .clf_frames(ctx,target)
    expect_true(all(vapply(frames,function(f) is.factor(f$value_as_concept_id),logical(1))))
  }
  raw <- recipe_to_plan(.clf_recipe());raw$options$translate_concepts <- FALSE
  ds.omop.plan.execute(raw,out=c(output='Numeric'),symbol='labels',conns=ctx$conns)
  for(f in .clf_frames(ctx,'Numeric')) {
    expect_true(is.numeric(f$education));expect_false(is.factor(f$cause))
    expect_true(all(na.omit(f$education) %in% c(45000123L,45000124L,45000125L)))
  }
})

test_that('A27 legacy sparse references share vocabulary names and staged categories', {
  ctx <- .clf_setup()
  p <- ds.omop.plan()
  p$outputs <- list(sparse=list(type='event_level',table='observation',
    filters=list(concept_set=list(ids=c(45000123L,45000124L,45000125L))),
    representation=list(format='sparse')))
  runs <- list()
  for(mode in c('memory','staged')) {
    target <- paste0('Sparse_',mode)
    ds.omop.plan.execute(p,out=c(sparse=target),symbol='labels',conns=ctx$conns,output_mode=mode)
    frames <- .clf_frames(ctx,paste0(target,'.covariateRef'))
    expect_true(all(vapply(frames,function(f) is.factor(f$conceptId),logical(1))))
    expect_identical(levels(frames[[1]]$conceptId),levels(frames[[2]]$conceptId))
    for (f in frames) {
      expect_true(any(grepl('new_education',f$covariateName)))
      expect_true(any(grepl('same_45000124',f$covariateName)))
      expect_false(is.factor(f$covariateId))
      expect_true(all(as.character(f$covariateId) %in% as.character(outer(c(45000123,45000124,45000125)*1000,1:5,'+'))))
    }
    runs[[mode]] <- frames
  }
  for(i in 1:3) expect_identical(runs$memory[[i]]$covariateName,runs$staged[[i]]$covariateName)
})
