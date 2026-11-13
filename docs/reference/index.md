# Package index

## All functions

<!-- end list -->

  - `as.data.frame(<dsomop_result>)` : Convert dsomop\_result to
    data.frame
  - `` `$`(<dsomop_result>) `` : Access dsomop\_result elements with
    backward compatibility
  - `ds.omop.achilles.analyses()` : List available Achilles analyses
  - `ds.omop.achilles.catalog()` : Get Achilles analysis catalog
  - `ds.omop.achilles.distribution()` : Get Achilles distribution
    results
  - `ds.omop.achilles.heel()` : Get Achilles Heel data-quality warnings
    (per site)
  - `ds.omop.achilles.results()` : Get Achilles count results
  - `ds.omop.achilles.status()` : Check Achilles availability
  - `ds.omop.analysis.get()` : Get unified analysis catalog entry
    metadata
  - `ds.omop.analysis.list()` : List unified analysis catalog entries
  - `ds.omop.analysis.run()` : Run a unified analysis catalog entry
  - `ds.omop.bind_rows()` : Row-bind two schema-identical server-side
    omop.table data frames
  - `ds.omop.cdm.source()` : Get the CDM source description from each
    server
  - `ds.omop.cdm.version()` : Get the CDM version reported by each
    server
  - `ds.omop.client.version()` : dsOMOPClient version
  - `ds.omop.code()` : Get the R code that produced a result
  - `ds.omop.cohort.combine()` : Combine two cohorts with set operations
  - `ds.omop.cohort.create()` : Create a cohort from a structured
    specification
  - `ds.omop.cohort.definition()` : Get a cohort definition by ID
  - `ds.omop.cohort.from_table()` : Build a cohort from the persons in a
    server-side omop.table symbol
  - `ds.omop.cohort.list()` : List available cohort definitions
  - `ds.omop.cohort.ref()` : Create a cohort reference for the plan DSL
    (client-only)
  - `ds.omop.column.stats()` : Get column-level statistics
  - `ds.omop.columns()` : List columns in a table
  - `ds.omop.comorbidity()` : Disclosure-safe two-by-two comorbidity
    (person co-occurrence)
  - `ds.omop.compare()` : Compare schemas across servers
  - `ds.omop.concept.ancestors()` : Get a concept's ancestors and
    descendants (hierarchy)
  - `ds.omop.concept.descendants()` : Get descendant concepts via the
    concept\_ancestor table
  - `ds.omop.concept.drilldown()` : Get concept drilldown profile
  - `ds.omop.concept.expand()` : Expand a concept set to a full list of
    concept IDs
  - `ds.omop.concept.list()` : Browse the concept catalog with
    pagination
  - `ds.omop.concept.locate()` : Locate concept across all CDM tables
  - `ds.omop.concept.lookup()` : Look up OMOP concepts by ID
  - `ds.omop.concept.prevalence()` : Get concept prevalence for a table
  - `ds.omop.concept.relationships()` : Get the relationships of one or
    more concepts
  - `ds.omop.concept.search()` : Search for OMOP concepts by name
  - `ds.omop.concept.set()` : Build a concept set specification
    (client-only)
  - `ds.omop.concept.summary()` : Summarise a value column scoped to one
    concept of one table
  - `ds.omop.concept.synonyms()` : Get the synonyms of one or more
    concepts
  - `ds.omop.connect()` : Connect to an OMOP CDM resource on DataSHIELD
    servers
  - `ds.omop.copy_code()` : Copy reproducible R code to clipboard
  - `ds.omop.crosstab()` : Disclosure-safe two-way cross-tabulation
  - `ds.omop.date.counts()` : Get record counts by time period
  - `ds.omop.disclosure.settings()` : Inspect the active disclosure
    thresholds on each server
  - `ds.omop.disconnect()` : Disconnect an OMOP session
  - `ds.omop.distribution()` : Continuous-value distribution over a
    cohort, in one call
  - `ds.omop.domain.coverage()` : Get cross-table domain coverage
  - `ds.omop.dp.prepare()` : Prepare a person-local table for sticky DP
    releases
  - `ds.omop.dp.release()` : Request a sticky privacy release
  - `ds.omop.dp.status()` : Inspect sticky privacy-release services
  - `ds.omop.filter()` : Filter the rows of a server-side omop.table
    data frame
  - `ds.omop.joins()` : Get the join relationship graph
  - `ds.omop.login()` : Log in and open an OMOP CDM session in one call
  - `ds.omop.merge()` : Merge two server-side omop.table data frames on
    the person key
  - `ds.omop.meta.effect_estimate()` : Meta-analyze a comparative effect
    estimate across databases (evidence synthesis)
  - `ds.omop.missingness()` : Get missingness rates for columns
  - `ds.omop.mutate()` : Derive columns of a server-side omop.table
  - `ds.omop.ohdsi.results()` : Query an OHDSI result table
  - `ds.omop.ohdsi.status()` : Check OHDSI result tool availability
  - `ds.omop.ohdsi.summary()` : Get OHDSI tool summary
  - `ds.omop.ohdsi.tables()` : List discovered OHDSI result tables
  - `ds.omop.plan()` : Create a new extraction plan
  - `ds.omop.plan.baseline()` : Add a baseline demographics output to
    the plan
  - `ds.omop.plan.cohort()` : Set a cohort filter on the plan
  - `ds.omop.plan.cohort_membership()` : Add a cohort membership output
    to the plan
  - `ds.omop.plan.concept_dictionary()` : Add a concept dictionary
    output to the plan
  - `ds.omop.plan.events()` : Add an event-level extraction to the plan
  - `ds.omop.plan.execute()` : Execute a plan and create server-side
    tables
  - `ds.omop.plan.features()` : Add feature extraction with feature
    specifications
  - `ds.omop.plan.harmonize()` : Harmonize a plan for multi-server
    execution
  - `ds.omop.plan.intervals()` : Add an intervals (long) output to the
    plan
  - `ds.omop.plan.load()` : Load an extraction plan from JSON or YAML
  - `ds.omop.plan.options()` : Set plan-wide options
  - `ds.omop.plan.outcome()` : Add an outcome extraction (convenience
    wrapper)
  - `ds.omop.plan.person_level()` : Add cardinality-safe person-level
    tables to the plan
  - `ds.omop.plan.person_period()` : Add a regular episode-by-period
    panel to an extraction plan
  - `ds.omop.plan.preview()` : Preview a plan (server-side dry run)
  - `ds.omop.plan.save()` : Save an extraction plan to JSON or YAML
  - `ds.omop.plan.survival()` : Add a survival (time-to-event) output to
    the plan
  - `ds.omop.plan.temporal_covariates()` : Add a temporal (time-binned)
    covariates output to the plan
  - `ds.omop.plan.validate()` : Validate an extraction plan
  - `ds.omop.prevalence()` : Covariate prevalence over a cohort, in one
    call
  - `ds.omop.query.exec()` : Execute a query template (DEPRECATED)
  - `ds.omop.query.get()` : Get query template details (DEPRECATED)
  - `ds.omop.query.list()` : List available query templates (DEPRECATED)
  - `ds.omop.query.pool()` : Pool query template results across servers
  - `ds.omop.querylibrary.sticky.release()` : Release a pinned
    QueryLibrary sticky redesign
  - `ds.omop.safe.cutpoints()` : Get safe numeric cutpoints for a column
  - `ds.omop.safe.filter.measurement()` : Create a safe population
    filter for a numeric measurement interval
  - `ds.omop.safe.filter.value()` : Create a safe numeric value filter
    using server-computed bins
  - `ds.omop.select()` : Keep a subset of columns of a server-side
    omop.table data frame
  - `ds.omop.server.version()` : dsOMOP version on each server
  - `ds.omop.snapshot()` : Get a full schema snapshot
  - `ds.omop.status()` : Get OMOP session status
  - `ds.omop.table()` : Pool a published concept category with its
    complete level contract
  - `ds.omop.table.stats()` : Get table-level statistics
  - `ds.omop.tables()` : List tables in the OMOP CDM database
  - `ds.omop.value.counts()` : Get value frequencies for a column
  - `ds.omop.value.histogram()` : Get a disclosure-safe numeric
    histogram
  - `ds.omop.value.quantiles()` : Get disclosure-safe numeric quantiles
  - `ds.omop.vocab.classes()` : List the concept classes available on
    each server
  - `ds.omop.vocab.domains()` : List the domains available on each
    server
  - `ds.omop.vocab.vocabularies()` : List the vocabularies available on
    each server
  - `omop.date_handling()` : Build a date handling specification
  - `omop.feature.abnormal_high()` : Create an abnormal-high count
    feature specification
  - `omop.feature.abnormal_low()` : Create an abnormal-low count feature
    specification
  - `omop.feature.boolean()` : Create a binary (presence/absence)
    feature specification
  - `omop.feature.count()` : Create an event count feature specification
  - `omop.feature.cv_value()` : Create a coefficient of variation
    feature specification
  - `omop.feature.drug_duration()` : Create a drug duration feature
    specification
  - `omop.feature.duration_sum()` : Create a duration sum feature
    specification
  - `omop.feature.first_value()` : Create a first-recorded-value feature
    specification
  - `omop.feature.gap_max_days()` : Create a maximum gap (days) feature
    specification
  - `omop.feature.gap_mean_days()` : Create a mean gap (days) feature
    specification
  - `omop.feature.latest_value()` : Create a most-recent-value feature
    specification
  - `omop.feature.max_value()` : Create a maximum value feature
    specification
  - `omop.feature.mean_value()` : Create a mean value feature
    specification
  - `omop.feature.min_value()` : Create a minimum value feature
    specification
  - `omop.feature.n_distinct()` : Create a distinct concept count
    feature specification
  - `omop.feature.sd_value()` : Create a standard deviation feature
    specification
  - `omop.feature.slope_value()` : Create a slope (linear trend) feature
    specification
  - `omop.feature.sum_value()` : Create a sum value feature
    specification
  - `omop.feature.time_since()` : Create a time-since-event feature
    specification
  - `omop.temporal()` : Build a temporal filtering specification
  - `omop_age()` `omop_sex()` `omop_race()` `omop_ethnicity()`
    `omop_birth_year_band()` `omop_died()` `omop_days_to_death()`
    `omop_n_observation_periods()` `omop_obs_duration()`
    `omop_prior_obs()` `omop_followup()` `omop_observed_days()`
    `omop_score()` `omop_population_column()` : Person-derived and
    population-column variables
  - `omop_censor()` : Censoring by an event or era
  - `omop_concept_set()` : Define a concept set
  - `omop_cost()` : Cost rows linked to an event
  - `omop_date_spec()` `omop_date()` `omop_date_components()`
    `omop_date_parts()` `omop_time_between()` `omop_age_at()` :
    Composable calendar presentation and anchored ages
  - `omop_episode_members()` : Events linked to oncology/treatment
    episodes
  - `omop_era()` : Eras (intervals) built from an event
  - `omop_event()` : Define an event source
  - `omop_ever()` `omop_count()` `omop_first_value()`
    `omop_last_value()` `omop_mean_value()` `omop_min_value()`
    `omop_max_value()` `omop_value_at_index()` `omop_days_since()`
    `omop_days_until()` `omop_first_date()` `omop_last_date()` :
    Simple-layer variables (shortcuts)
  - `omop_filter()` `omop_filter_sex()` `omop_filter_age()`
    `omop_filter_age_group()` `omop_filter_cohort()`
    `omop_filter_has_concept()` `omop_filter_date_range()`
    `omop_filter_value()` `omop_filter_value_concept()`
    `omop_filter_concept_set()` `omop_filter_not_has_concept()`
    `omop_filter_concept_count()` `omop_filter_prior_observation()`
    `omop_filter_followup()` `omop_filter_visit_count()`
    `omop_filter_has_measurement()` `omop_filter_missing_measurement()`
    : Create a filter specification
  - `omop_filter_group()` : Create an AND/OR group of filters
  - `omop_has()` `omop_has_not()` `omop_count_is()` `omop_not()`
    `omop_all()` `omop_any()` `omop_person()` `omop_age_is()`
    `omop_observed()` `omop_in_cohort()` : Population criteria
  - `omop_history()` : Medical history covariates (simple layer)
  - `omop_index()` `omop_any_of()` `omop_end()` `omop_coverage()` :
    Index definitions, episode ends and observation coverage
  - `omop_index_event()` : Define the OMOP event that anchors a
    longitudinal population
  - `omop_match()` `omop_age_band()` `omop_strata()` : Matched and
    nested case-control sampling
  - `omop_new_users()` : New users of a drug (simple layer)
  - `omop_output()` : Create an output specification
  - `recipe_merge()` `omop_param()` `recipe_params()` `recipe_bind()` :
    Recipe parameters: placeholders, binding and merging
  - `omop_population()` : Create a population node
  - `omop_privacy()` : Define a server-owned sticky privacy release
  - `omop_querylibrary_sticky()` : Build an executable sticky redesign
    of an OHDSI QueryLibrary question
  - `omop_querylibrary_sticky_catalog()` : List audited sticky redesign
    mappings for OHDSI QueryLibrary
  - `omop_recipe()` : Create an extraction recipe declaratively
  - `omop_reduce()` : Reducers: how an event stream becomes one value
    per person or episode
  - `omop_study()` : A study in one call (simple layer)
  - `omop_unit()` : Unit normalisation for an event's numeric value
  - `omop_variable()` : Create a variable specification
  - `omop_variable_age()` : Create an age variable
  - `omop_variable_block()` : Create a variable block
  - `omop_variable_chads2()` : Create a CHADS2 score variable
  - `omop_variable_chadsvasc()` : Create a CHA2DS2-VASc score variable
  - `omop_variable_charlson()` : Create a Charlson Comorbidity Index
    variable
  - `omop_variable_cv()` : Create a coefficient of variation variable
  - `omop_variable_dcsi()` : Create a DCSI score variable
  - `omop_variable_demo_missingness()` : Create a demographics
    missingness variable
  - `omop_variable_drug_duration()` : Create a drug duration variable
  - `omop_variable_followup()` : Create a followup duration variable
  - `omop_variable_hfrs()` : Create an HFRS score variable
  - `omop_variable_n_distinct()` : Create a distinct-concept-count
    variable
  - `omop_variable_obs_duration()` : Create an observation duration
    variable
  - `omop_variable_prior_obs()` : Create a prior observation duration
    variable
  - `omop_variable_sd()` : Create a standard deviation variable
  - `omop_variable_sex()` : Create a sex (M/F) variable
  - `omop_variable_slope()` : Create a slope (linear trend) variable
  - `omop_variable_sum()` : Create a sum variable
  - `omop_where()` `omop_where_tree()` : Row predicates over an event's
    columns
  - `omop_window()` `omop_bound()` `omop_anchor()` : Windows, bounds and
    anchors
  - `plot(<omop_plan>)` : Plot an extraction plan as a dependency graph
  - `print(<dsomop_result>)` : Print a dsomop\_result
  - `print(<omop_dp_status>)` : Print the release policy on each DP
    server
  - `print(<omop_filter>)` : Print an omop\_filter
  - `print(<omop_filter_group>)` : Print an omop\_filter\_group
  - `print(<omop_output>)` : Print an omop\_output
  - `print(<omop_plan>)` : Print method for extraction plans
  - `print(<omop_population>)` : Print an omop\_population
  - `print(<omop_recipe>)` : Print an omop\_recipe
  - `print(<omop_variable>)` : Print an omop\_variable
  - `print(<omop_variable_block>)` : Print an omop\_variable\_block
  - `recipe_add_block()` : Add a variable block to the recipe
  - `recipe_add_concept_set()` `recipe_add_event()`
    `recipe_add_interval()` `recipe_remove_concept_set()`
    `recipe_remove_event()` `recipe_remove_interval()`
    `recipe_set_publish()` : Language-2 recipe registries
  - `recipe_add_filter()` : Add a filter to the recipe
  - `recipe_add_output()` : Add an output specification to the recipe
  - `recipe_add_population()` : Add a population to the recipe
  - `recipe_add_variable()` : Add a variable to the recipe
  - `recipe_concept_sets()` `recipe_export_concept_sets()`
    `recipe_import_concept_sets()` : Concept sets of a recipe; OHDSI
    concept-set exchange
  - `recipe_execute()` : Execute a recipe: compile to plan and run
  - `recipe_explain()` : Explain a recipe: its semantics and every
    default it relies on
  - `recipe_export_circe()` : Export a recipe population to an OHDSI
    Circe cohort expression
  - `recipe_export_json()` : Export a recipe to JSON
  - `recipe_export_yaml()` : Export a recipe to YAML
  - `recipe_import_circe()` : Import an OHDSI Circe cohort expression as
    a recipe population
  - `recipe_import_json()` : Import a recipe from JSON
  - `recipe_import_yaml()` : Import a recipe from YAML
  - `recipe_lint()` : Lint a recipe for common authoring mistakes (pure
    client-side)
  - `recipe_load()` : Load a recipe from JSON or YAML
  - `recipe_preview()` : Preview a recipe on the server
  - `recipe_preview_schema()` : Preview the output schema for a recipe
  - `recipe_preview_stats()` : Preview aggregate stats for a recipe
    (without materializing)
  - `recipe_remove_filter()` : Remove a filter from the recipe
  - `recipe_remove_output()` : Remove an output specification from the
    recipe
  - `recipe_remove_population()` : Remove a population from the recipe
  - `recipe_remove_variable()` : Remove a variable from the recipe
  - `recipe_save()` : Save a recipe to JSON or YAML
  - `recipe_set_options()` : Set plan-wide options on a recipe
  - `recipe_set_scope()` : Set the recipe-level population scope
  - `recipe_to_code()` : Generate reproducible R code from a recipe
  - `recipe_to_plan()` : Convert a recipe to an extraction plan
  - `recipe_validate()` : Validate a recipe on the server
  - `summary(<omop_plan>)` : Summarise an extraction plan
