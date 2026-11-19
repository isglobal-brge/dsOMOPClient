# Observed-level discovery uses exactly DataSHIELD's asFactorDS1 rule at each
# site. A stricter rule protects nothing: ds.asFactor releases the same levels,
# and means of public per-concept 0/1 flags already give exact counts. A looser
# rule would expose levels DataSHIELD refuses. Shared assignment has no union cap.
.conceptRefuseUnsafe <- function(per_server) {
  sites <- names(per_server) %||% as.character(seq_along(per_server))
  refused <- character()
  all_rules <- character()
  for (i in seq_along(per_server)) {
    report <- per_server[[i]]
    for (column in report$unsafe %||% character()) {
      rules <- as.character(unlist(report$unsafe_rules[[column]], use.names = FALSE))
      if (!length(rules)) rules <- c("nfilter.levels.max", "nfilter.levels.density")
      all_rules <- c(all_rules, rules)
      refused <- c(refused, paste0("column '", column, "' on site '", sites[[i]],
        "' (", paste(unique(rules), collapse = " / "), ")"))
    }
  }
  # Each server applies its own DataSHIELD options, so its administrator may
  # also admit more levels; declared levels need no server change at all.
  # dsOMOP 2.10.7 servers declare a short public vocabulary list by themselves;
  # a longer one is reported, and grouping is then the analyst's decision.
  long <- any(all_rules == "dsomop.auto_declared_levels_max")
  admin <- c(if (any(grepl("^nfilter[.]levels", all_rules))) "nfilter.levels.max / nfilter.levels.density",
             if (long) "dsomop.auto_declared_levels_max")
  if (length(refused)) stop("Concept factor publication refused: ",
    paste(refused, collapse = "; "), ". ",
    if (long) paste0("The public vocabulary list that holds these values is longer than the server declares ",
      "automatically (dsomop.auto_declared_levels_max). ") else "",
    "Declare the levels (omop_category_spec(levels = ), for example from omop_concept_levels()), ",
    "group them with omop_category_recode() / omop_category_rollup(), or narrow the events with concepts =",
    if (length(admin)) paste0("; otherwise the server's administrator can raise ", paste(admin, collapse = " or ")) else "",
    ".", call. = FALSE)
  invisible(NULL)
}

# Assignment needs concept identity support even when labels are decimal IDs.
# Older suffix-only reports cannot prove coverage of aliased concept columns;
# the 2.10.6 minimum also covers untranslated and catalog-loader identities.
.conceptAssertSupport <- function(session, servers) {
  language <- .v2_server_language(session, servers)
  missing <- servers[!vapply(language, function(x)
    "output.concept_labels" %in% x$operations, logical(1))]
  if (length(missing)) stop("Concept factors require server support for output.concept_labels on: ",
    paste(missing, collapse = ", "), ". Upgrade the server and publish its methods.", call. = FALSE)
  versions <- session$server_versions %||% character()
  unsupported <- servers[!vapply(servers, function(site) {
    version <- if (site %in% names(versions)) versions[[site]] else NULL
    is.character(version) && length(version) == 1L && !is.na(version) &&
      grepl("^[0-9]+([.][0-9]+)+$", version) &&
      tryCatch(utils::compareVersion(version, "2.10.6") >= 0L, error = function(e) FALSE)
  }, logical(1))]
  if (length(unsupported)) stop("Concept factor publication requires dsOMOP 2.10.6 or later on: ",
    paste(unsupported, collapse = ", "),
    ". Upgrade the server: concept IDs are always categories since 2.10.6.", call. = FALSE)
  invisible(NULL)
}

# Uses only omopFactorLevelsDS metadata from the retained sites of a component.
.conceptUnionReports <- function(per_server) {
  if (any(vapply(per_server, function(x) !identical(as.integer(x$concept_protocol), 1L), logical(1)))) {
    stop("Incomplete concept presentation protocol across retained sites.", call. = FALSE)
  }
  .conceptRefuseUnsafe(per_server)
  numbers <- per_server[[1L]]$numbers %||% list()
  for(site in per_server) .numberValidateShared(numbers,site$numbers)
  columns <- names(per_server[[1L]]$concepts)
  if (any(!vapply(per_server, function(x) setequal(names(x$concepts), columns), logical(1)))) {
    stop("Incompatible concept-column schemas across retained sites.", call. = FALSE)
  }
  explanation <- character()
  result <- setNames(lapply(columns, function(col) {
    records <- unlist(lapply(per_server, function(x) x$concepts[[col]]), recursive = FALSE)
    calendars <- lapply(per_server, function(x) x$calendars[[col]])
    categories<-lapply(per_server,function(x)x$categories[[col]])
    if(any(!vapply(categories,is.null,logical(1)))) {
      if(any(vapply(categories,is.null,logical(1)))||length(unique(vapply(categories,.jcsCanonical,"")))!=1L) .categoryReject("Category semantics differ across retained sites.","category_domain_incompatible")
      resolved<-.categoryResolveRecords(records,identity=categories[[1L]])
    } else if (any(!vapply(calendars, is.null, logical(1)))) {
      if (any(vapply(calendars, is.null, logical(1))) || length(unique(vapply(calendars, .jcsCanonical, ""))) != 1L) .dateReject("Calendar/policy mismatch across retained sites.")
      resolved <- .dateResolveRecords(records, calendars[[1L]])
    } else resolved <- .conceptResolve(records)
    for (decision in resolved$decisions) {
      candidates <- sort(unique(vapply(Filter(function(x) identical(x$id,decision$id) && !is.null(x$name), records),
        function(x) x$name, "")), method="radix")
      explanation <<- c(explanation, paste0(col, ": ", decision$id, " -> ", decision$label,
        if (length(candidates)>1L) paste0("; vocabulary conflict: ",paste(candidates,collapse=" / ")),
        if (!is.null(decision$version)) paste0("; version ",decision$version), "; ",decision$rule))
    }
    shared <- resolved[intersect(names(resolved), c("ids", "labels", "levels", "vocabulary", "kind", "calendar", "number", "category", "keys", "aliases", "ordered", "reference", "contrasts"))]
    shared$provenance<-resolved$decisions
    naming_ids <- unique(unlist(lapply(per_server,function(x) x$reference_names[[col]]),use.names=FALSE))
    if (length(naming_ids)) shared$naming_ids <- naming_ids[.conceptIdOrder(naming_ids)]
    shared
  }), columns)
  keys <- per_server[[1L]]$keys
  if (any(!vapply(per_server, function(x) setequal(x$keys, keys), logical(1)))) {
    stop("Incompatible component schemas; presentation cannot add missing feature columns.", call. = FALSE)
  }
  generated <- per_server[[1L]]$generated %||% list()
  if (any(!vapply(per_server, function(x) setequal(names(x$generated), names(generated)), logical(1)))) {
    stop("Incomplete generated-column identity contract.", call. = FALSE)
  }
  for (key in names(generated)) {
    rows <- lapply(per_server, function(x) x$generated[[key]])
    identity <- lapply(rows, function(g) g[c("id", "prefix", "suffix", "owner", "explicit")])
    if (any(!vapply(identity, identical, logical(1), identity[[1L]]))) {
      stop("Inconsistent generated-column identity or explicit name.", call. = FALSE)
    }
    generated[[key]]$candidates <- unlist(lapply(rows, function(g) g$candidates), recursive = FALSE)
  }
  for (key in names(generated)) {
    choice <- .conceptChooseName(generated[[key]]$candidates)
    explanation <- c(explanation, paste0("Generated key ",key,": ",choice$name %||% generated[[key]]$id,
      if (!is.null(choice$version)) paste0("; version ",choice$version),"; ",choice$rule))
  }
  spec <- list(concept_protocol = 1L, columns = result, numbers = numbers,
       evidence = unique(unlist(lapply(per_server, function(x) x$evidence), use.names = FALSE)),
       names = if (length(generated)) as.list(.conceptGeneratedNames(keys, generated)) else list())
  schemas<-lapply(per_server,function(x) {
    schema<-x$schema
    if(!is.null(schema))schema$columns<-lapply(schema$columns,function(c){c$name<-c$stable_key;c})
    schema
  })
  if(any(!vapply(schemas,is.null,logical(1)))) {
    if(any(vapply(schemas,is.null,logical(1))) || length(unique(vapply(schemas,.jcsCanonical,"")))!=1L)
      .categoryReject("Published column types/derivations differ across retained sites.","category_domain_incompatible")
    spec$schema<-schemas[[1L]]
    spec$schema$columns<-lapply(spec$schema$columns,function(c) {
      c$name<-spec$names[[c$stable_key]] %||% c$stable_key;c
    })
  }
  attr(spec,"concept_explain") <- explanation
  spec
}

# This receipt is display-only client metadata from a completed publication.
# It is never consulted by planning, validation, support checks or execution.
.conceptPlanKey <- function(plan) {
  if (.v2_is_plan_v2(plan)) .planNormalizedDigest(plan)$sha256 else .plan_harmonization_signature(plan)
}

.conceptPublishedExplain <- function(session, plan, conns) {
  receipt <- session$concept_presentation
  if (is.null(receipt) || !length(receipt$components) || !identical(receipt$key,.conceptPlanKey(plan)) ||
      !setequal(receipt$sites,names(conns))) return(character())
  lines <- "Concept resolutions from the last successful publication (not a new presence query):"
  for (component in names(receipt$components)) {
    lines <- c(lines,paste0(component,": ",receipt$components[[component]]))
  }
  lines
}
