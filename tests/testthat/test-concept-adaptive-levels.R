# dsOMOP 2.10.8: adaptive containers across three DSLite sites. Each site
# climbs its public hierarchy only as far as the rule needs; the union of their
# lists is shared. Fixture: helper-concept-levels.R plus a synthetic hierarchy
# (root, 5 groups, 50 subgroups, 5,000 leaves).

.adapt_leaves <- 990001000L + 0:4999

.adapt_hierarchy <- function(ctx) {
  root <- 990000000L; groups <- 990000001:990000005
  subgroups <- 990000100L + 0:49
  ids <- c(root, groups, subgroups, .adapt_leaves)
  group_of_sub <- groups[(seq_along(subgroups) - 1L) %/% 10L + 1L]
  sub_of_leaf <- subgroups[(seq_along(.adapt_leaves) - 1L) %/% 100L + 1L]
  ca <- rbind(
    data.frame(a = ids, d = ids, s = 0L),
    data.frame(a = root, d = c(groups, subgroups, .adapt_leaves), s = c(rep(1L, 5L), rep(2L, 50L), rep(3L, 5000L))),
    data.frame(a = group_of_sub, d = subgroups, s = 1L),
    data.frame(a = group_of_sub[match(sub_of_leaf, subgroups)], d = .adapt_leaves, s = 2L),
    data.frame(a = sub_of_leaf, d = .adapt_leaves, s = 1L))
  names(ca) <- c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation")
  ca$max_levels_of_separation <- ca$min_levels_of_separation
  for (path in ctx$paths) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    DBI::dbWriteTable(con, "concept", data.frame(concept_id = ids, concept_name = paste("Finding", ids),
      domain_id = "Condition", vocabulary_id = "Hier", concept_class_id = "Clinical Finding", standard_concept = "S",
      concept_code = as.character(ids), valid_start_date = "2000-01-01", valid_end_date = "2099-12-31",
      invalid_reason = NA_character_), append = TRUE)
    DBI::dbWriteTable(con, "concept_ancestor", ca, append = TRUE)
    DBI::dbDisconnect(con)
  }
}

test_that("adaptive containers keep automatic lists small and shared across sites", {
  in_subgroups <- function(groups) unlist(lapply(groups, function(g) .adapt_leaves[g * 100L + 1:10]))
  site1 <- rep(in_subgroups(0:5), each = 4L)    # 60 leaves in subgroups 1-6
  site2 <- rep(.adapt_leaves[1:8], each = 5L)  # 8 leaves: within the rule, no list
  site3 <- rep(in_subgroups(6:11), each = 4L)   # 60 leaves in subgroups 7-12
  ctx <- .cvl_setup(deaths = list(site1, site2, site3))
  .adapt_hierarchy(ctx)
  for (mode in c("memory", "staged")) for (translate in c(FALSE, TRUE)) {
    target <- paste0("Adaptive_", mode, "_", translate)
    recipe_execute(.cvl_wide(.cvl_event, options = list(translate_concepts = translate)),
      out = c(study = target), symbol = "cvl", conns = ctx$conns, output_mode = mode)
    levels <- .cvl_shared(ctx, target, "cause_of_death_first_value")
    # Twelve subgroups with their leaves, not the 5,056 concepts of the kind.
    expect_length(levels, 12L * 101L)
    if (!translate) expect_true(all(as.character(c(site1, site2, site3)) %in% levels))
  }
})
