# ==============================================================================
# dsOMOPClient v2 - Client-Side Pooling Engine
# ==============================================================================
# Pooling formulas for multi-server aggregation + policy enforcement.
# ==============================================================================

# --- Policies -----------------------------------------------------------------

# "strict":        Only pool if ALL servers return valid data.
# "pooled_only_ok": Pool what's available, skip servers with NA/errors.

# --- Pooling Formulas ---------------------------------------------------------

#' Pool counts by summing across servers
#' @param per_site_counts Named numeric vector or list of counts
#' @param policy Character; "strict" or "pooled_only_ok"
#' @return List with $result and $warnings
#' @keywords internal
.pool_counts <- function(per_site_counts, policy = "strict") {
  vals <- unlist(per_site_counts)
  warnings <- character(0)

  has_na <- is.na(vals)
  if (any(has_na)) {
    bad <- names(vals)[has_na]
    if (policy == "strict") {
      return(list(
        result = NULL,
        warnings = paste0("Strict pooling failed: NA counts from server(s) ",
                          paste(bad, collapse = ", "))
      ))
    }
    warnings <- paste0("Dropped server(s) with NA counts: ",
                       paste(bad, collapse = ", "))
    vals <- vals[!has_na]
  }

  if (length(vals) == 0) {
    return(list(result = NULL, warnings = c(warnings, "No valid counts to pool")))
  }

  list(result = sum(vals), warnings = warnings)
}

#' Pool means weighted by count
#' @param per_site_means Named numeric vector of means
#' @param per_site_counts Named numeric vector of counts (same names)
#' @param policy Character; pooling policy
#' @return List with $result and $warnings
#' @keywords internal
.pool_means <- function(per_site_means, per_site_counts, policy = "strict") {
  means <- unlist(per_site_means)
  counts <- unlist(per_site_counts)
  warnings <- character(0)

  # Align by common names
  common <- intersect(names(means), names(counts))
  means <- means[common]
  counts <- counts[common]

  invalid <- is.na(means) | is.na(counts)
  if (any(invalid)) {
    bad <- common[invalid]
    if (policy == "strict") {
      return(list(
        result = NULL,
        warnings = paste0("Strict pooling failed: invalid mean/count from server(s) ",
                          paste(bad, collapse = ", "))
      ))
    }
    warnings <- paste0("Dropped server(s): ", paste(bad, collapse = ", "))
    means <- means[!invalid]
    counts <- counts[!invalid]
  }

  if (length(means) == 0 || sum(counts) == 0) {
    return(list(result = NULL, warnings = c(warnings, "No valid data to pool means")))
  }

  list(result = sum(counts * means) / sum(counts), warnings = warnings)
}

#' Pool variance using Cochrane formula
#' @param per_site_var Named numeric vector of variances
#' @param per_site_means Named numeric vector of means
#' @param per_site_counts Named numeric vector of counts
#' @param policy Character; pooling policy
#' @return List with $result and $warnings
#' @keywords internal
.pool_variance <- function(per_site_var, per_site_means, per_site_counts,
                           policy = "strict") {
  vars <- unlist(per_site_var)
  means <- unlist(per_site_means)
  counts <- unlist(per_site_counts)
  warnings <- character(0)

  common <- Reduce(intersect, list(names(vars), names(means), names(counts)))
  vars <- vars[common]
  means <- means[common]
  counts <- counts[common]

  invalid <- is.na(vars) | is.na(means) | is.na(counts)
  if (any(invalid)) {
    bad <- common[invalid]
    if (policy == "strict") {
      return(list(
        result = NULL,
        warnings = paste0("Strict pooling failed: invalid data from server(s) ",
                          paste(bad, collapse = ", "))
      ))
    }
    warnings <- paste0("Dropped server(s): ", paste(bad, collapse = ", "))
    vars <- vars[!invalid]
    means <- means[!invalid]
    counts <- counts[!invalid]
  }

  N <- sum(counts)
  if (length(vars) == 0 || N <= 1) {
    return(list(result = NULL, warnings = c(warnings, "Insufficient data for pooled variance")))
  }

  pooled_mean <- sum(counts * means) / N
  within_ss <- sum((counts - 1) * vars)
  between_ss <- sum(counts * (means - pooled_mean)^2)
  pooled_var <- (within_ss + between_ss) / (N - 1)

  list(result = pooled_var, warnings = warnings)
}

#' Pool proportions weighted by denominator
#' @param per_site_num Named numeric vector of numerators
#' @param per_site_denom Named numeric vector of denominators
#' @param policy Character; pooling policy
#' @return List with $result and $warnings
#' @keywords internal
.pool_proportions <- function(per_site_num, per_site_denom, policy = "strict") {
  nums <- unlist(per_site_num)
  denoms <- unlist(per_site_denom)
  warnings <- character(0)

  common <- intersect(names(nums), names(denoms))
  nums <- nums[common]
  denoms <- denoms[common]

  invalid <- is.na(nums) | is.na(denoms)
  if (any(invalid)) {
    bad <- common[invalid]
    if (policy == "strict") {
      return(list(
        result = NULL,
        warnings = paste0("Strict pooling failed: invalid data from server(s) ",
                          paste(bad, collapse = ", "))
      ))
    }
    warnings <- paste0("Dropped server(s): ", paste(bad, collapse = ", "))
    nums <- nums[!invalid]
    denoms <- denoms[!invalid]
  }

  if (length(nums) == 0 || sum(denoms) == 0) {
    return(list(result = NULL, warnings = c(warnings, "No valid data to pool proportions")))
  }

  list(result = sum(nums) / sum(denoms), warnings = warnings)
}

#' Pool histograms by bin-wise sum
#' @param per_site_histograms Named list of histogram data frames
#'   (each with bin_start, bin_end, count, suppressed)
#' @param policy Character; pooling policy
#' @return List with $result (data.frame) and $warnings
#' @keywords internal
.pool_histograms <- function(per_site_histograms, policy = "strict") {
  warnings <- character(0)

  # Filter valid histograms
  valid <- list()
  for (srv in names(per_site_histograms)) {
    df <- per_site_histograms[[srv]]
    if (is.data.frame(df) && nrow(df) > 0 &&
        all(c("bin_start", "bin_end", "count") %in% names(df))) {
      valid[[srv]] <- df
    } else {
      if (policy == "strict") {
        return(list(
          result = NULL,
          warnings = paste0("Strict pooling failed: invalid histogram from server ", srv)
        ))
      }
      warnings <- c(warnings, paste0("Dropped server with invalid histogram: ", srv))
    }
  }

  if (length(valid) == 0) {
    return(list(result = NULL, warnings = c(warnings, "No valid histograms to pool")))
  }

  # Use first server's bins as reference
  ref <- valid[[1]]
  pooled <- data.frame(
    bin_start = ref$bin_start,
    bin_end = ref$bin_end,
    count = rep(0, nrow(ref)),
    stringsAsFactors = FALSE
  )

  eps <- 1e-6  # tolerance for floating point bin edge comparison

  for (srv in names(valid)) {
    df <- valid[[srv]]
    # Validate bin count match
    if (nrow(df) != nrow(pooled)) {
      if (policy == "strict") {
        return(list(
          result = NULL,
          warnings = paste0("Strict pooling failed: mismatched bin count from server ", srv)
        ))
      }
      warnings <- c(warnings, paste0("Skipped server with mismatched bins: ", srv))
      next
    }
    # Validate bin edges match within epsilon tolerance
    edges_ok <- all(abs(df$bin_start - pooled$bin_start) < eps) &&
                all(abs(df$bin_end - pooled$bin_end) < eps)
    if (!edges_ok) {
      if (policy == "strict") {
        return(list(
          result = NULL,
          warnings = paste0("Strict pooling failed: mismatched bin edges from server ", srv)
        ))
      }
      warnings <- c(warnings, paste0("Skipped server with mismatched bin edges: ", srv))
      next
    }
    pooled$count <- pooled$count + ifelse(is.na(df$count), 0, df$count)
  }

  list(result = pooled, warnings = warnings)
}

#' Pool top-K concept results using two-pass merge
#' @param per_site_dfs Named list of data frames with concept_id and a metric column
#' @param metric_col Character; name of the metric column to sum
#' @param k Integer; number of top results to return
#' @param policy Character; pooling policy
#' @return List with $result (data.frame) and $warnings
#' @keywords internal
.pool_top_k <- function(per_site_dfs, metric_col, k, policy = "strict") {
  warnings <- character(0)

  valid <- list()
  for (srv in names(per_site_dfs)) {
    df <- per_site_dfs[[srv]]
    if (is.data.frame(df) && nrow(df) > 0 && "concept_id" %in% names(df) &&
        metric_col %in% names(df)) {
      valid[[srv]] <- df
    } else {
      if (policy == "strict") {
        return(list(
          result = NULL,
          warnings = paste0("Strict pooling failed: invalid data from server ", srv)
        ))
      }
      warnings <- c(warnings, paste0("Dropped server with invalid data: ", srv))
    }
  }

  if (length(valid) == 0) {
    return(list(result = NULL, warnings = c(warnings, "No valid data for top-K pooling")))
  }

  # Pass 1: Union all concept_ids
  all_ids <- unique(unlist(lapply(valid, function(df) df$concept_id)))

  # Build merged data frame
  merged <- data.frame(concept_id = all_ids, stringsAsFactors = FALSE)
  merged[[metric_col]] <- 0

  # Carry concept_name from any server that has it
  has_names <- any(vapply(valid, function(df) "concept_name" %in% names(df), logical(1)))
  if (has_names) merged$concept_name <- NA_character_

  # Track which concepts appear in each server (for suppression propagation)
  n_servers <- length(valid)
  concept_server_count <- rep(0L, nrow(merged))
  names(concept_server_count) <- as.character(merged$concept_id)

  for (srv in names(valid)) {
    df <- valid[[srv]]
    idx <- match(df$concept_id, merged$concept_id)
    vals <- as.numeric(df[[metric_col]])
    has_na <- is.na(vals)
    vals[has_na] <- 0
    merged[[metric_col]][idx] <- merged[[metric_col]][idx] + vals
    # Count servers that have this concept with a non-NA value
    present_idx <- idx[!has_na]
    concept_server_count[as.character(merged$concept_id[present_idx])] <-
      concept_server_count[as.character(merged$concept_id[present_idx])] + 1L
    if (has_names && "concept_name" %in% names(df)) {
      na_names <- is.na(merged$concept_name[idx])
      merged$concept_name[idx[na_names]] <- as.character(df$concept_name[na_names])
    }
  }

  # Mark concepts as suppressed if missing from any server or having NA counts
  suppressed <- concept_server_count < n_servers
  merged$suppressed <- suppressed[as.character(merged$concept_id)]
  # Set suppressed concept counts to NA
  merged[[metric_col]][merged$suppressed] <- NA_real_

  # Pass 2: Re-rank and take top K (non-suppressed first, then suppressed)
  merged <- merged[order(merged$suppressed, -merged[[metric_col]],
                          na.last = TRUE), ]
  n <- min(nrow(merged), k)
  merged <- merged[seq_len(n), , drop = FALSE]
  rownames(merged) <- NULL

  list(result = merged, warnings = warnings)
}

# --- Dispatcher ---------------------------------------------------------------

#' Dispatch pooling by result type
#'
#' @param per_site Named list of per-server results
#' @param result_type Character; one of the recognized result types
#' @param policy Character; "strict" or "pooled_only_ok"
#' @return List with $result and $warnings
#' @keywords internal
.pool_result <- function(per_site, result_type, policy = "strict") {
  switch(result_type,

    "table_stats" = {
      # per_site[[srv]] is a list with $rows, $persons, etc.
      row_counts <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$rows) && !isTRUE(s$rows_suppressed)) s$rows
        else NA_real_
      }, numeric(1))
      names(row_counts) <- names(per_site)
      pool_rows <- .pool_counts(row_counts, policy)

      person_counts <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$persons) && !isTRUE(s$persons_suppressed)) s$persons
        else NA_real_
      }, numeric(1))
      names(person_counts) <- names(per_site)
      pool_persons <- .pool_counts(person_counts, policy)

      warnings <- c(pool_rows$warnings, pool_persons$warnings)
      result <- if (!is.null(pool_rows$result)) {
        list(rows = pool_rows$result, persons = pool_persons$result)
      } else NULL

      list(result = result, warnings = warnings)
    },

    "column_stats" = {
      # per_site[[srv]] is a list with count, mean, sd, etc.
      counts <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$count)) s$count else NA_real_
      }, numeric(1))
      names(counts) <- names(per_site)

      means <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$mean)) s$mean else NA_real_
      }, numeric(1))
      names(means) <- names(per_site)

      vars <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$sd)) s$sd^2 else NA_real_
      }, numeric(1))
      names(vars) <- names(per_site)

      pool_n <- .pool_counts(counts, policy)
      pool_mean <- .pool_means(means, counts, policy)
      pool_var <- .pool_variance(vars, means, counts, policy)

      warnings <- c(pool_n$warnings, pool_mean$warnings, pool_var$warnings)
      result <- if (!is.null(pool_n$result)) {
        list(count = pool_n$result, mean = pool_mean$result,
             sd = if (!is.null(pool_var$result)) sqrt(pool_var$result) else NULL)
      } else NULL

      list(result = result, warnings = warnings)
    },

    "domain_coverage" = {
      # per_site[[srv]] is a data.frame with table_name, n_records, n_persons, etc.
      all_dfs <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0) {
          df$.server <- srv
          all_dfs[[srv]] <- df
        }
      }
      if (length(all_dfs) == 0) {
        return(list(result = NULL, warnings = "No valid domain coverage data"))
      }
      combined <- do.call(rbind, all_dfs)
      rownames(combined) <- NULL

      # Sum counts per table (propagate NA if any server has NA)
      tables <- unique(combined$table_name)
      pooled_rows <- list()
      for (tbl in tables) {
        sub <- combined[combined$table_name == tbl, , drop = FALSE]
        row <- list(table_name = tbl)
        if ("n_records" %in% names(sub)) {
          row$n_records <- if (any(is.na(sub$n_records))) NA_real_
                           else sum(sub$n_records)
        }
        if ("n_persons" %in% names(sub)) {
          row$n_persons <- if (any(is.na(sub$n_persons))) NA_real_
                           else sum(sub$n_persons)
        }
        pooled_rows[[length(pooled_rows) + 1]] <- as.data.frame(row,
          stringsAsFactors = FALSE)
      }
      result <- do.call(rbind, pooled_rows)
      rownames(result) <- NULL
      list(result = result, warnings = character(0))
    },

    "missingness" = {
      # per_site[[srv]] is a data.frame with column_name, n_total, n_missing, missing_rate
      all_dfs <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0) all_dfs[[srv]] <- df
      }
      if (length(all_dfs) == 0) {
        return(list(result = NULL, warnings = "No valid missingness data"))
      }

      # Pool per column: sum n_missing / sum n_total
      combined <- do.call(rbind, all_dfs)
      cols <- unique(combined$column_name)
      pooled_rows <- list()
      for (col in cols) {
        sub <- combined[combined$column_name == col, , drop = FALSE]
        total <- sum(sub$n_total, na.rm = TRUE)
        missing <- sum(sub$n_missing, na.rm = TRUE)
        rate <- if (total > 0) missing / total else NA_real_
        pooled_rows[[length(pooled_rows) + 1]] <- data.frame(
          column_name = col, n_total = total, n_missing = missing,
          missing_rate = rate, stringsAsFactors = FALSE
        )
      }
      result <- do.call(rbind, pooled_rows)
      rownames(result) <- NULL
      list(result = result, warnings = character(0))
    },

    "value_counts" = {
      # Top-K value counts (data frames with value, n columns)
      metric <- if (any(vapply(per_site, function(df) {
        is.data.frame(df) && "n" %in% names(df)
      }, logical(1)))) "n" else "count"

      # Need concept_id for pool_top_k; value_counts use "value" instead
      # Convert to concept_id temporarily
      adapted <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0) {
          if ("value" %in% names(df) && !"concept_id" %in% names(df)) {
            df$concept_id <- df$value
          }
          adapted[[srv]] <- df
        }
      }

      k <- if (length(adapted) > 0) max(vapply(adapted, nrow, integer(1))) else 20
      pool_out <- .pool_top_k(adapted, metric, k, policy)

      # Rename back
      if (!is.null(pool_out$result) && "concept_id" %in% names(pool_out$result)) {
        pool_out$result$value <- pool_out$result$concept_id
        pool_out$result$concept_id <- NULL
      }
      pool_out
    },

    "concept_prevalence" = {
      metric <- "n_persons"
      # Check if any per_site has n_persons column
      has_persons <- any(vapply(per_site, function(df) {
        is.data.frame(df) && "n_persons" %in% names(df)
      }, logical(1)))
      if (!has_persons) metric <- "n_records"

      k <- 50
      if (length(per_site) > 0) {
        first_valid <- per_site[[which(vapply(per_site, is.data.frame, logical(1)))[1]]]
        if (!is.null(first_valid)) k <- nrow(first_valid)
      }
      .pool_top_k(per_site, metric, k, policy)
    },

    "histogram" = {
      .pool_histograms(per_site, policy)
    },

    "date_counts" = {
      # Same bin-sum pattern as histograms but column names differ
      # Adapt to histogram format
      adapted <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0 && "period" %in% names(df)) {
          hdf <- data.frame(
            bin_start = seq_len(nrow(df)),
            bin_end = seq_len(nrow(df)),
            count = if ("n_records" %in% names(df)) df$n_records else df$count,
            suppressed = if ("suppressed" %in% names(df)) df$suppressed
                         else rep(FALSE, nrow(df)),
            period = df$period,
            stringsAsFactors = FALSE
          )
          adapted[[srv]] <- hdf
        }
      }
      pool_out <- .pool_histograms(adapted, policy)
      # Restore period column
      if (!is.null(pool_out$result) && length(adapted) > 0) {
        ref <- adapted[[1]]
        pool_out$result$period <- ref$period
        pool_out$result$n_records <- pool_out$result$count
      }
      pool_out
    },

    "concept_drilldown" = {
      # Composite: pool summary counts; skip quantiles
      warnings <- character(0)

      # Pool summary (n_records, n_persons)
      rec_counts <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$summary) && !is.null(s$summary$n_records))
          s$summary$n_records else NA_real_
      }, numeric(1))
      names(rec_counts) <- names(per_site)
      pool_rec <- .pool_counts(rec_counts, policy)

      per_counts <- vapply(per_site, function(s) {
        if (is.list(s) && !is.null(s$summary) && !is.null(s$summary$n_persons))
          s$summary$n_persons else NA_real_
      }, numeric(1))
      names(per_counts) <- names(per_site)
      pool_per <- .pool_counts(per_counts, policy)

      pooled_summary <- if (!is.null(pool_rec$result)) {
        list(n_records = pool_rec$result, n_persons = pool_per$result)
      } else NULL
      warnings <- c(warnings, pool_rec$warnings, pool_per$warnings)

      # Pool histograms if present
      hist_data <- list()
      for (srv in names(per_site)) {
        s <- per_site[[srv]]
        if (is.list(s) && !is.null(s$numeric_summary) &&
            !is.null(s$numeric_summary$histogram)) {
          hist_data[[srv]] <- s$numeric_summary$histogram
        }
      }
      pooled_histogram <- NULL
      if (length(hist_data) > 0) {
        ph <- .pool_histograms(hist_data, policy)
        pooled_histogram <- ph$result
        warnings <- c(warnings, ph$warnings)
      }

      # Pool categorical values if present
      cat_data <- list()
      for (srv in names(per_site)) {
        s <- per_site[[srv]]
        if (is.list(s) && !is.null(s$categorical_values) &&
            is.data.frame(s$categorical_values) && nrow(s$categorical_values) > 0) {
          cat_data[[srv]] <- s$categorical_values
        }
      }
      pooled_categorical <- NULL
      if (length(cat_data) > 0) {
        metric <- if ("n" %in% names(cat_data[[1]])) "n" else "count"
        k <- max(vapply(cat_data, nrow, integer(1)))
        # Need concept_id column for pool_top_k
        for (srv in names(cat_data)) {
          if (!"concept_id" %in% names(cat_data[[srv]]) &&
              "value_as_concept_id" %in% names(cat_data[[srv]])) {
            cat_data[[srv]]$concept_id <- cat_data[[srv]]$value_as_concept_id
          }
        }
        pc <- .pool_top_k(cat_data, metric, k, policy)
        pooled_categorical <- pc$result
        warnings <- c(warnings, pc$warnings)
      }

      # Quantiles: NOT pooled
      warnings <- c(warnings, "Quantiles cannot be safely pooled without individual-level data")

      result <- list(
        summary = pooled_summary,
        numeric_summary = if (!is.null(pooled_histogram))
          list(histogram = pooled_histogram, quantiles = NULL) else NULL,
        categorical_values = pooled_categorical
      )
      list(result = result, warnings = warnings)
    },

    "concept_locate" = {
      # Row-bind + sum counts per concept/table pair
      all_dfs <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0) {
          df$.server <- srv
          all_dfs[[srv]] <- df
        }
      }
      if (length(all_dfs) == 0) {
        return(list(result = NULL, warnings = "No valid locator data"))
      }
      combined <- do.call(rbind, all_dfs)
      rownames(combined) <- NULL

      # Aggregate by concept_id + table_name + concept_column
      group_cols <- intersect(c("concept_id", "table_name", "concept_column"),
                              names(combined))
      if (length(group_cols) == 0) {
        return(list(result = combined, warnings = character(0)))
      }

      groups <- split(combined, combined[group_cols])
      pooled_rows <- lapply(groups, function(sub) {
        row <- sub[1, group_cols, drop = FALSE]
        if ("n_records" %in% names(sub)) {
          row$n_records <- sum(sub$n_records, na.rm = TRUE)
        }
        if ("n_persons" %in% names(sub)) {
          row$n_persons <- sum(sub$n_persons, na.rm = TRUE)
        }
        row
      })
      result <- do.call(rbind, pooled_rows)
      rownames(result) <- NULL
      list(result = result, warnings = character(0))
    },

    "achilles_results" = {
      # Sum count_value per (analysis_id, stratum_1..5) across servers
      all_dfs <- list()
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0 &&
            all(c("analysis_id", "count_value") %in% names(df))) {
          all_dfs[[srv]] <- df
        } else {
          if (policy == "strict") {
            return(list(
              result = NULL,
              warnings = paste0("Strict pooling failed: invalid Achilles results from server ", srv)
            ))
          }
        }
      }
      if (length(all_dfs) == 0) {
        return(list(result = NULL, warnings = "No valid Achilles results to pool"))
      }

      combined <- do.call(rbind, all_dfs)
      rownames(combined) <- NULL

      # Group by analysis_id + all strata
      key_cols <- c("analysis_id", "stratum_1", "stratum_2", "stratum_3",
                     "stratum_4", "stratum_5")
      key_cols <- intersect(key_cols, names(combined))

      # Replace NA strata with sentinel for grouping
      for (kc in key_cols[-1]) {
        combined[[kc]][is.na(combined[[kc]])] <- "__NA__"
      }

      groups <- split(combined, combined[key_cols], drop = TRUE)
      pooled_rows <- lapply(groups, function(sub) {
        row <- sub[1, key_cols, drop = FALSE]
        vals <- sub$count_value
        has_na <- any(is.na(vals))
        if (has_na && policy == "strict") {
          row$count_value <- NA_real_
        } else {
          row$count_value <- sum(vals, na.rm = TRUE)
        }
        row
      })
      result <- do.call(rbind, pooled_rows)
      rownames(result) <- NULL

      # Restore NA sentinels
      for (kc in key_cols[-1]) {
        result[[kc]][result[[kc]] == "__NA__"] <- NA_character_
      }

      list(result = result, warnings = character(0))
    },

    "achilles_distribution" = {
      # Weighted distribution aggregation across servers
      all_dfs <- list()
      warnings <- character(0)
      for (srv in names(per_site)) {
        df <- per_site[[srv]]
        if (is.data.frame(df) && nrow(df) > 0 &&
            all(c("analysis_id", "count_value", "avg_value") %in% names(df))) {
          all_dfs[[srv]] <- df
        } else {
          if (policy == "strict") {
            return(list(
              result = NULL,
              warnings = paste0("Strict pooling failed: invalid Achilles dist from server ", srv)
            ))
          }
          warnings <- c(warnings, paste0("Dropped server with invalid dist data: ", srv))
        }
      }
      if (length(all_dfs) == 0) {
        return(list(result = NULL, warnings = c(warnings, "No valid Achilles distributions to pool")))
      }

      combined <- do.call(rbind, all_dfs)
      rownames(combined) <- NULL

      key_cols <- c("analysis_id", "stratum_1", "stratum_2", "stratum_3",
                     "stratum_4", "stratum_5")
      key_cols <- intersect(key_cols, names(combined))

      for (kc in key_cols[-1]) {
        combined[[kc]][is.na(combined[[kc]])] <- "__NA__"
      }

      groups <- split(combined, combined[key_cols], drop = TRUE)
      pooled_rows <- lapply(groups, function(sub) {
        row <- sub[1, key_cols, drop = FALSE]
        counts <- sub$count_value
        has_na <- any(is.na(counts))
        if (has_na && policy == "strict") {
          row$count_value <- NA_real_
          row$min_value <- NA_real_
          row$max_value <- NA_real_
          row$avg_value <- NA_real_
          row$stdev_value <- NA_real_
          row$median_value <- NA_real_
          row$p10_value <- NA_real_
          row$p25_value <- NA_real_
          row$p75_value <- NA_real_
          row$p90_value <- NA_real_
          return(row)
        }

        valid <- !is.na(counts)
        n <- counts[valid]
        N <- sum(n)
        row$count_value <- N

        # Weighted mean
        avgs <- sub$avg_value[valid]
        row$avg_value <- if (N > 0 && !any(is.na(avgs))) {
          sum(n * avgs) / N
        } else NA_real_

        # Global min/max
        row$min_value <- if ("min_value" %in% names(sub)) {
          min(sub$min_value[valid], na.rm = TRUE)
        } else NA_real_
        row$max_value <- if ("max_value" %in% names(sub)) {
          max(sub$max_value[valid], na.rm = TRUE)
        } else NA_real_

        # Pooled stdev via Cochrane formula
        if ("stdev_value" %in% names(sub) && N > 1 && !any(is.na(avgs))) {
          sds <- sub$stdev_value[valid]
          if (!any(is.na(sds))) {
            pooled_mean <- row$avg_value
            within_ss <- sum((n - 1) * sds^2)
            between_ss <- sum(n * (avgs - pooled_mean)^2)
            row$stdev_value <- sqrt((within_ss + between_ss) / (N - 1))
          } else {
            row$stdev_value <- NA_real_
          }
        } else {
          row$stdev_value <- NA_real_
        }

        # Median and percentiles: NOT poolable from summary stats
        row$median_value <- NA_real_
        row$p10_value <- NA_real_
        row$p25_value <- NA_real_
        row$p75_value <- NA_real_
        row$p90_value <- NA_real_

        row
      })
      result <- do.call(rbind, pooled_rows)
      rownames(result) <- NULL

      for (kc in key_cols[-1]) {
        result[[kc]][result[[kc]] == "__NA__"] <- NA_character_
      }

      warnings <- c(warnings,
        "Median and percentiles cannot be pooled from summary statistics; set to NA")
      list(result = result, warnings = warnings)
    },

    # Default: no pooling
    {
      list(result = NULL,
           warnings = paste0("Unknown result type for pooling: ", result_type))
    }
  )
}
