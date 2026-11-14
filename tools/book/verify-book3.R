# Independent oracles over the public synthetic source, never an analyst route.
verify_book3 <- function(input, env, get_data) {
  if (!grepl("book-15-labs|book-06-events|book-08-outputs", input)) return(invisible(NULL))
  db <- DBI::dbConnect(RSQLite::SQLite(), env$practice$database)
  on.exit(DBI::dbDisconnect(db), add=TRUE)
  m <- DBI::dbGetQuery(db, paste("SELECT person_id, measurement_id, measurement_date,",
    "value_as_number, range_low, range_high FROM measurement",
    "WHERE measurement_concept_id=3000963 AND unit_concept_id=8713",
    "ORDER BY person_id, measurement_date, measurement_id"))
  oracle <- do.call(rbind, lapply(split(m, m$person_id), function(x) {
    v <- x$value_as_number; dates <- as.numeric(as.Date(x$measurement_date))
    index <- dates[1L] + 7
    eligible <- which(dates >= index-7 & dates <= index & !is.na(v))
    closest <- eligible[order(abs(dates[eligible]-index), dates[eligible], x$measurement_id[eligible])][1L]
    data.frame(person_id=x$person_id[1L], first=v[1L], last=tail(v,1), mean=mean(v),
      minimum=min(v), maximum=max(v), at_index=v[closest],
      per_day=if (length(unique(dates))>1L) unname(coef(lm(v ~ dates))[[2L]]) else NA_real_,
      high_count=sum(v>x$range_high,na.rm=TRUE), low_count=sum(v<x$range_low,na.rm=TRUE))
  }))
  same <- function(x, y, tolerance=1e-8) stopifnot(isTRUE(all.equal(
    sort(as.numeric(x), na.last=TRUE), sort(as.numeric(y), na.last=TRUE), tolerance=tolerance)))
  summary_matches <- function(actual, expected) {
    values <- c(quantile(expected, c(.05,.1,.25,.5,.75,.9,.95), na.rm=TRUE), mean(expected,na.rm=TRUE))
    stopifnot(isTRUE(all.equal(book_summary(actual)$Value, unname(values))))
  }
  if (grepl("book-06-events", input)) {
    same(get_data("D_event_change")$change, oracle$last-oracle$first)
    summary_matches(env$event_change, oracle$last-oracle$first)
  }
  if (grepl("book-08-outputs", input)) summary_matches(env$output_lab, oracle$mean)
  if (!grepl("book-15-labs", input)) return(invisible(NULL))
  d <- get_data("D_labs")
  stopifnot(nrow(d)==28L, length(unique(d$person_id))==28L, nrow(m)==108L,
    all(is.na(m$range_low)), all(is.na(m$range_high)))
  for (col in c("first","last","mean","minimum","maximum")) same(d[[col]],oracle[[col]])
  for (col in c("first","last","mean","minimum","maximum")) summary_matches(env[[paste0(col,"_summary")]],oracle[[col]])
  same(get_data("D_lab_index")$at_index, oracle$at_index)
  summary_matches(env$index_summary, oracle$at_index)
  same(get_data("D_lab_slopes")$per_day, oracle$per_day)
  summary_matches(env$slope_summary, oracle$per_day)
  same(get_data("D_lab_annual")$per_365_days, 365*oracle$per_day)
  summary_matches(env$annual_summary, 365*oracle$per_day)
  same(get_data("D_lab_gl")$mean_gl, 10*oracle$mean)
  summary_matches(env$conversion_summary, 10*oracle$mean)
  same(get_data("D_lab_change")$change,oracle$last-oracle$first)
  summary_matches(env$change_summary,oracle$last-oracle$first)
  same(get_data("D_lab_spread")$spread,oracle$maximum-oracle$minimum)
  summary_matches(env$spread_summary,oracle$maximum-oracle$minimum)
  for (col in c("high_count","low_count")) same(get_data("D_lab_ranges")[[col]],oracle[[col]])
  flags <- get_data("D_lab_flags")
  stopifnot(identical(flags$high,as.integer(d$mean>=15)),identical(flags$low,as.integer(d$mean<14)))
  band <- cut(oracle$mean,c(0,14,15,100),labels=c("below 14","14 to below 15","15 or above"),right=FALSE)
  stopifnot(identical(sort(as.character(flags$band)), sort(as.character(band))))
  expected_counts <- function(x) as.numeric(table(factor(x, levels=sort(unique(x))), useNA="always"))
  same(book_counts(env$bin_counts)$Count,expected_counts(as.character(band)))
  same(book_counts(env$high_counts)$Count,expected_counts(as.integer(oracle$mean>=15)))
  same(book_counts(env$low_counts)$Count,expected_counts(as.integer(oracle$mean<14)))
  for (flag in c("recorded_high","recorded_low")) {
    same(get_data("D_recorded_flags")[[flag]],rep(0,28))
    same(book_counts(env[[flag]])$Count,c(28,0))
  }
  # Verify the actual disjoint fixture files, not just their stated group lists.
  stopifnot(all(file.exists(env$pair$path)))
  ids <- lapply(env$pair$path,function(path) {
    con <- DBI::dbConnect(RSQLite::SQLite(),path);on.exit(DBI::dbDisconnect(con))
    DBI::dbReadTable(con,"person")$person_id
  })
  stopifnot(all(lengths(ids)==14L), length(intersect(ids[[1]],ids[[2]]))==0L,
    setequal(unlist(ids),oracle$person_id))
  frames <- DSLite::getDSLiteData(env$two_conns,"D_pool_labs")
  site_oracles <- lapply(ids,function(x) oracle[oracle$person_id %in% x,])
  for (site in names(frames)) for (col in c("first","last","mean","minimum","maximum")) {
    same(frames[[site]][[col]],site_oracles[[site]][[col]])
  }
  same(env$pooled_mean$Global.Mean[1L,1L],mean(oracle$mean))
  same(env$pooled_first$Global.Mean[1L,1L],mean(oracle$first))
  quantiles <- Reduce(`+`,lapply(site_oracles,function(x)
    quantile(x$mean,c(.05,.1,.25,.5,.75,.9,.95))*nrow(x)))/nrow(oracle)
  stopifnot(isTRUE(all.equal(unname(env$pooled_quantiles[1:7]),unname(quantiles))))
  same(env$pooled_quantiles[["Mean"]],mean(oracle$mean))
  reference <- glm(last ~ first,data=oracle,family=gaussian())
  stopifnot(isTRUE(all.equal(unname(env$pooled_model$coefficients[,1]),
    unname(coef(reference)),tolerance=1e-6)))
  message("BOOK3 PASS: source oracles, released summaries/counts, disjoint 14+14 sites, weighted quantiles and Gaussian coefficients")
}
