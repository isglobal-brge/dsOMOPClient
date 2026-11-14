# Trusted, immutable language-2 constructor definitions, catalogue 1.0.0.
# No analyst text or downloaded code is evaluated.
.library_build <- function(id) {
  switch(id,
    "demographics.baseline" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      lookback <- omop_param("lookback", "integer", default = 365L)
      variables <- list(
        age = omop_age(reference = "index", bands = FALSE),
        sex = omop_sex(), race = omop_race(), ethnicity = omop_ethnicity(),
        prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
      outputs <- list(omop_output("baseline", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "baseline"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "socioeconomic.education" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      education <- omop_param("education", "concept_set")
      lookback <- omop_param("lookback", "integer", default = 365L)
      edu <- omop_event("observation", education,
                        name = "education")
      events <- list(edu)
      variables <- list(
        education_concept_id = omop_variable(event = edu,
          reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                                missing = "keep")),
        education_records = omop_variable(event = edu,
          reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
      outputs <- list(omop_output("education", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "education"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "socioeconomic.employment" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      employment <- omop_param("employment", "concept_set")
      lookback <- omop_param("lookback", "integer", default = 365L)
      job <- omop_event("observation", employment,
                        name = "employment")
      events <- list(job)
      variables <- list(
        employment_concept_id = omop_variable(event = job,
          reducer = omop_reduce("last_value", value = "value_as_concept_id", window = omop_window(-lookback, -1L),
                                missing = "keep")),
        employment_records = omop_variable(event = job,
          reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
      outputs <- list(omop_output("employment", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "employment"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "socioeconomic.numeric_observation" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      measure <- omop_param("measure", "event")
      lookback <- omop_param("lookback", "integer", default = 365L)
      variables <- list(
        numeric_value = omop_variable(event = measure,
          reducer = omop_reduce("last_value", window = omop_window(-lookback, -1L),
                                missing = "keep")),
        source_records = omop_variable(event = measure,
          reducer = omop_reduce("count", window = omop_window(-lookback, -1L))))
      outputs <- list(omop_output("numeric_observation", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "numeric_observation"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "demographics.death_cause" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      horizon <- omop_param("horizon", "integer", default = 365L)
      death <- omop_event("death", name = "death")
      events <- list(death)
      variables <- list(
        death_recorded = omop_variable(event = death,
          reducer = omop_reduce("binary", window = omop_window(0L, horizon))),
        death_days = omop_variable(event = death,
          reducer = omop_reduce("first_date", window = omop_window(0L, horizon))),
        cause_concept_id = omop_variable(event = death,
          reducer = omop_reduce("first_value", value = "concept_id", window = omop_window(0L, horizon),
                                missing = "keep")))
      outputs <- list(omop_output("death_cause", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "death_cause"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.charlson" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      variables <- list(charlson = omop_score("charlson", reference_date = NULL))
      outputs <- list(omop_output("score", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "score"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.chads2" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      variables <- list(chads2 = omop_score("chads2", reference_date = NULL))
      outputs <- list(omop_output("score", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "score"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.chadsvasc" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      variables <- list(chadsvasc = omop_score("chadsvasc", reference_date = NULL))
      outputs <- list(omop_output("score", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "score"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.dcsi" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      variables <- list(dcsi = omop_score("dcsi", reference_date = NULL))
      outputs <- list(omop_output("score", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "score"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.hfrs" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      variables <- list(hfrs = omop_score("hfrs", reference_date = NULL))
      outputs <- list(omop_output("score", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "score"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "comorbidity.condition_history" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      conditions <- omop_param("conditions", "concept_set")
      lookback <- omop_param("lookback", "integer", default = 365L)
      dx <- omop_event("condition_occurrence", conditions, name = "history")
      events <- list(dx)
      variables <- list(
        history_present = omop_variable(event = dx,
          reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))),
        history_days = omop_variable(event = dx,
          reducer = omop_reduce("count", distinct = "day",
                                window = omop_window(-lookback, -1L))))
      outputs <- list(omop_output("history", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "history"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "drug.era_summary" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      drug <- omop_param("drug", "concept_set")
      gap <- omop_param("gap", "integer", default = 30L)
      persistence <- omop_param("persistence", "integer", default = 0L)
      window <- omop_param("window", "window", default = omop_window(0L, 365L))
      rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                       name = "rx")
      course <- omop_era(rx, gap = gap, persistence = persistence, min_days = 0L,
                         by = "event", end = "recorded_end", fallback = "days_supply",
                         stockpile = FALSE, name = "course")
      events <- list(rx)
      intervals <- list(course)
      variables <- list(
        era_count = omop_variable(event = course,
          reducer = omop_reduce("era_count", window = window)),
        era_days = omop_variable(event = course,
          reducer = omop_reduce("era_days", window = window)),
        first_era_days = omop_variable(event = course,
          reducer = omop_reduce("first_era_days", window = window)),
        covered_days = omop_variable(event = course,
          reducer = omop_reduce("covered_days", window = window, coverage = "recorded")))
      outputs <- list(omop_output("eras", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "eras"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "drug.adherence_components" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      drug <- omop_param("drug", "concept_set")
      horizon <- omop_param("horizon", "integer", default = 365L)
      window <- omop_window(0L, horizon - 1L)
      rx <- omop_event("drug_exposure", drug, end = list(fallback = "days_supply"),
                       name = "rx")
      stock <- omop_era(rx, gap = 0L, persistence = 0L, min_days = 0L, by = "event",
                        end = "recorded_end", fallback = "days_supply",
                        stockpile = TRUE, name = "stock")
      events <- list(rx)
      intervals <- list(stock)
      variables <- list(
        observed_days = omop_observed_days(window),
        recorded_covered_days = omop_variable(event = stock,
          reducer = omop_reduce("covered_days", window = window,
                                coverage = "recorded", observed_only = TRUE)),
        effective_covered_days = omop_variable(event = stock,
          reducer = omop_reduce("covered_days", window = window,
                                coverage = "effective_supply", observed_only = TRUE)),
        supply_days = omop_variable(event = rx,
          reducer = omop_reduce("supply_days", window = window, fills = "clipped")))
      outputs <- list(omop_output("adherence", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "adherence"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "cohort.new_users" = {
      events <- list()
      intervals <- list()
      variables <- list()
      drug <- omop_param("drug", "concept_set")
      washout <- omop_param("washout_days", "integer", default = 365L)
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      by <- omop_param("by", "string", default = "ingredient")
      nu <- omop_new_users(drug, washout_days = washout, select = select, by = by)
      populations <- list(nu)
      variables <- list(age = omop_age(), sex = omop_sex(),
                         index_source = omop_population_column("index_source"))
      outputs <- list(omop_output("new_users", rows = "new_users", format = "wide",
        grain = "episode", columns = names(variables),
        options = list(date_handling = "relative")))
      publish <- "new_users"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "cohort.active_comparator" = {
      events <- list()
      intervals <- list()
      variables <- list()
      drug_a <- omop_param("drug_a", "concept_set")
      drug_b <- omop_param("drug_b", "concept_set")
      outcome <- omop_param("outcome", "event")
      washout <- omop_param("washout_days", "integer", default = 365L)
      horizon <- omop_param("horizon", "integer", default = 365L)
      grace <- omop_param("grace_days", "integer", default = 30L)
      a <- omop_event("drug_exposure", drug_a, name = "rx_a")
      b <- omop_event("drug_exposure", drug_b, name = "rx_b")
      ea <- omop_era(a, by = "event", gap = 30L, persistence = 0L,
                     end = "recorded_end", fallback = "days_supply", name = "era_a")
      eb <- omop_era(b, by = "event", gap = 30L, persistence = 0L,
                     end = "recorded_end", fallback = "days_supply", name = "era_b")
      events <- list(a, b)
      intervals <- list(ea, eb)
      populations <- list(omop_population("study",
        index = omop_index(omop_any_of(ea, eb, order = c("era_a", "era_b")),
          select = "first_eligible", end = omop_end("interval_end")),
        coverage = omop_coverage(), filters = list(
          omop_observed(before = washout),
          omop_has_not(a, window = omop_window(-washout, -1L)),
          omop_has_not(b, window = omop_window(-washout, -1L)))))
      variables <- list(arm = omop_population_column("index_source"),
                         age = omop_age(), sex = omop_sex())
      outputs <- list(
        omop_output("baseline", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")),
        omop_output("itt", rows = "study", format = "survival", grain = "episode",
          options = list(outcomes = list(outcome = outcome),
            tar = omop_window(1L, horizon), format = "survival", event_order = "first",
            prior_outcome = list(lookback = 365L, action = "exclude"),
            censor = list("observation_end", "death"), same_day_priority = "event")),
        omop_output("on_treatment", rows = "study", format = "survival", grain = "episode",
          options = list(outcomes = list(outcome = outcome),
            tar = omop_window(1L, horizon), format = "survival", event_order = "first",
            prior_outcome = list(lookback = 365L, action = "exclude"),
            censor = list("observation_end", "death",
              omop_censor(ea, which = "containing_entry_end", offset = grace,
                          missing = "ineligible", when_index_source = "era_a"),
              omop_censor(eb, which = "containing_entry_end", offset = grace,
                          missing = "ineligible", when_index_source = "era_b"),
              omop_censor(eb, which = "first_start_after_entry", when_index_source = "era_a"),
              omop_censor(ea, which = "first_start_after_entry", when_index_source = "era_b")),
            same_day_priority = "event")),
        omop_output("itt_analysis", rows = "study", format = "joined", grain = "episode",
          options = list(from = list(list(output = "itt"), list(output = "baseline")),
                          how = "left", cardinality = "many_to_one")),
        omop_output("on_treatment_analysis", rows = "study", format = "joined", grain = "episode",
          options = list(from = list(list(output = "on_treatment"), list(output = "baseline")),
                          how = "left", cardinality = "many_to_one")))
      publish <- c("itt_analysis", "on_treatment_analysis")
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "outcome.incident_event" = {
      events <- list()
      intervals <- list()
      variables <- list()
      outcome <- omop_param("outcome", "event")
      lookback <- omop_param("lookback", "integer", default = 365L)
      populations <- list(omop_population("study",
        index = omop_index(outcome, select = "first_eligible",
                           end = omop_end("fixed", days = 0L)),
        coverage = omop_coverage(), filters = list(
          omop_observed(before = lookback),
          omop_has_not(outcome, window = omop_window(-lookback, -1L)))))
      variables <- list(age = omop_age(), sex = omop_sex(),
        prior_observed_days = omop_observed_days(omop_window(-lookback, -1L)))
      outputs <- list(omop_output("incident_events", rows = "study", format = "wide", grain = "episode",
        columns = names(variables), options = list(date_handling = "relative")))
      publish <- "incident_events"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "survival.first_event" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      outcome <- omop_param("outcome", "event")
      start <- omop_param("start", "integer", default = 1L)
      horizon <- omop_param("horizon", "integer", default = 365L)
      prior_days <- omop_param("prior_days", "integer", default = 365L)
      outputs <- list(omop_output("survival", rows = "study", format = "survival",
        grain = "episode", options = list(outcomes = list(outcome = outcome),
          tar = omop_window(start, horizon), format = "survival", event_order = "first",
          censor = list("observation_end", "death"),
          prior_outcome = list(lookback = prior_days, action = "exclude"),
          tie_policy = "priority", same_day_priority = "event")))
      publish <- "survival"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "survival.competing_risk" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      outcome_a <- omop_param("outcome_a", "event")
      outcome_b <- omop_param("outcome_b", "event")
      horizon <- omop_param("horizon", "integer", default = 365L)
      outputs <- list(omop_output("competing_risk", rows = "study", format = "survival",
        grain = "episode", options = list(outcomes = list(cause_a = outcome_a, cause_b = outcome_b),
          tar = omop_window(1L, horizon), format = "competing_risk", event_order = "first",
          censor = list("observation_end", "death"),
          prior_outcome = list(lookback = 365L, action = "exclude"),
          tie_policy = "priority", same_day_priority = "event")))
      publish <- "competing_risk"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "survival.recurrent_events" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      outcome <- omop_param("outcome", "event")
      horizon <- omop_param("horizon", "integer", default = 365L)
      washout <- omop_param("washout_days", "integer", default = 0L)
      outputs <- list(omop_output("recurrent", rows = "study", format = "survival",
        grain = "episode", options = list(outcomes = list(outcome = outcome),
          tar = omop_window(1L, horizon), format = "recurrent_events", event_order = "all",
          washout_days = washout, censor = list("observation_end", "death"),
          tie_policy = "priority", same_day_priority = "event", complete = TRUE)))
      publish <- "recurrent"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "panel.measurements" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      measure <- omop_param("measure", "event")
      horizon <- omop_param("horizon", "integer", default = 365L)
      bin_days <- omop_param("bin_days", "integer", default = 30L)
      lookback <- omop_param("lookback", "integer", default = 90L)
      expire <- omop_param("expire", "integer", default = 30L)
      outputs <- list(omop_output("measurements", rows = "study", format = "person_period",
        grain = "episode", options = list(representation = "dense", sources = list(measure),
          bins = list(width = bin_days), window = omop_window(0L, horizon),
          analyses = c("count", "mean", "last"),
          carry_forward = list(measure = list(lookback = lookback, expire = expire)))))
      publish <- "measurements"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "panel.incidence" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      outcome <- omop_param("outcome", "event")
      horizon <- omop_param("horizon", "integer", default = 365L)
      outputs <- list(omop_output("incidence", rows = "study", format = "person_period",
        grain = "episode", options = list(representation = "dense",
          bins = list(calendar = "year"), window = omop_window(0L, horizon),
          outcomes = list(outcome = outcome))))
      publish <- "incidence"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "panel.sccs" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      drug <- omop_param("drug", "concept_set")
      outcome <- omop_param("outcome", "event")
      horizon <- omop_param("horizon", "integer", default = 365L)
      age_width <- omop_param("age_width", "integer", default = 5L)
      rx <- omop_event("drug_exposure", drug, name = "rx")
      exposure <- omop_era(rx, gap = 0L, persistence = 0L, by = "event",
                           end = "recorded_end", fallback = "days_supply", name = "exposure")
      events <- list(rx)
      intervals <- list(exposure)
      outputs <- list(omop_output("sccs", rows = "study", format = "person_period",
        grain = "episode", options = list(representation = "dense",
          bins = list(calendar = "year"), window = omop_window(0L, horizon),
          partition = list(boundaries = list(exposure), age_bands = age_width, calendar = "year"),
          exposures = list(drug = exposure), outcomes = list(outcome = outcome))))
      publish <- "sccs"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "longitudinal.event_log" = {
      events <- list()
      intervals <- list()
      variables <- list()
      index <- omop_param("index", "event", description = "Event or era defining entry")
      select <- omop_param("select", "string", default = "first_eligible",
                           description = "Selection among index candidates")
      populations <- list(omop_population("study",
        index = omop_index(index, select = select,
                           end = omop_end("observation_end")),
        coverage = omop_coverage(policy = "unique", on_ambiguous = "drop",
                                 on_missing = "drop", gap = 0L)))
      
      source_a <- omop_param("source_a", "event")
      source_b <- omop_param("source_b", "event")
      outputs <- list(omop_output("event_log", rows = "study", format = "long",
        grain = "episode", sources = list(source_a, source_b),
        options = list(event_log = TRUE, unit = "episode_event", date_handling = "relative")))
      publish <- "event_log"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    "matching.nested_case_control" = {
      events <- list()
      intervals <- list()
      variables <- list()
      outcome <- omop_param("outcome", "event")
      exposure <- omop_param("exposure", "event")
      lookback <- omop_param("lookback", "integer", default = 365L)
      m <- omop_param("controls_per_case", "integer", default = 4L)
      age_width <- omop_param("age_width", "integer", default = 5L)
      seed <- omop_param("seed", "integer", default = 1L)
      populations <- list(
        omop_population("cases",
          index = omop_index(outcome, select = "first_eligible",
                             end = omop_end("fixed", days = 0L)),
          coverage = omop_coverage(), filters = list(
            omop_observed(before = lookback),
            omop_has_not(outcome, window = omop_window(NULL, -1L)))),
        omop_population("controls", coverage = omop_coverage(),
          filters = list(omop_observed(before = lookback))),
        omop_population("matched", match = omop_match(cases = "cases", controls = "controls",
          outcome = outcome, at = "index", match = list("sex", omop_age_band(age_width)),
          controls_per_case = m, prior_outcome = "exclude", future_cases = "eligible",
          replacement = "between_sets", seed = seed, insufficient = "keep_available",
          missing_strata = "ineligible", max_candidates = 1000000L)))
      variables <- list(set_id = omop_population_column("set_id"),
        is_case = omop_population_column("is_case"),
        sampling_weight = omop_population_column("sampling_weight"),
        age = omop_age(), sex = omop_sex(),
        exposure_history = omop_variable(event = exposure,
          reducer = omop_reduce("binary", window = omop_window(-lookback, -1L))))
      outputs <- list(omop_output("matched", rows = "matched", format = "wide",
        grain = "episode", columns = names(variables),
        options = list(date_handling = "relative")))
      publish <- "matched"
      omop_recipe(events = events, intervals = intervals,
        populations = populations, variables = variables,
        outputs = outputs, publish = publish)
    },
    stop("Unknown installed recipe library definition.", call. = FALSE))
}
