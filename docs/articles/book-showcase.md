# What you can do with dsOMOP

**What you will learn**

See six useful results, then choose the chapter that teaches your next
step.

Read the cards as a tour, or run them in order after the Opal login in
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
The first four cards use the 48-person synthetic questionnaire study on
two sites; the last two use Synthea27Nj labs.

``` r
library(dsOMOPClient)
conns <- ds.omop.login(rep("https://localhost:8443", 2), user = "administrator",
  password = "password",
  server = c("site", "site2"),
  resource = c(site = "omop.questionnaire", site2 = "omop.questionnaire_site2"))$conns
```

## Named concepts across sites

The education question is **900001**, found in [chapter 4’s
catalogue](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.html#find-the-questionnaire-questions-answers-and-causes).
The recipe takes each person’s latest answer. By default, the same
concept ID gets the same name across sites, using the newest available
vocabulary wording.

``` r
education <- omop_variable(event = omop_event("observation", 900001L),
  reducer = omop_reduce("last_value", value = "value_as_concept_id"))
named <- omop_recipe(variables = list(education = education))
recipe_execute(named, out = c(output = "D_tour_named"), datasources = conns)
education_counts <- dsBaseClient::ds.table("D_tour_named$education",
  useNA = "always", datasources = conns)
education_counts$output.list$TABLE_rvar.by.study_counts
```

|                           | site | site2 |
| :------------------------ | ---: | ----: |
| Secondary\_education      |   12 |     0 |
| University\_qualification |   12 |    24 |
| NA                        |   24 |    24 |

Site 1 has 12 Secondary and 12 University answers; site 2 has 0 and 24.
Both use “University qualification”, the newer name for concept
**900102**. Each site also has 24 missing answers. A shared label does
not imply equal counts. These two teaching sites are related copies for
comparison, not independent study samples. [Learn concepts and
harmonisation
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-10-harmonise.md)

The next three cards use one questionnaire site:

``` r
ds.omop.disconnect(datasources = conns)
DSI::datashield.logout(conns)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.questionnaire", server = "site")$conns
```

## Dates and completed ages

Completed age at death counts birthdays reached, using the available
birth and death dates. Incomplete dates stay missing by default.

``` r
dates <- omop_recipe(variables = list(age = omop_age_at("death")))
recipe_execute(dates, out = c(output = "D_tour_dates"), datasources = conns)
dsBaseClient::ds.quantileMean("D_tour_dates$age", datasources = conns)
```

    #>    5%   10%   25%   50%   75%   90%   95%  Mean
    #> 32.00 32.00 32.75 37.50 42.25 43.00 43.00 37.50

The 24 complete ages are 32, 33, 42 and 43, with mean and median 37.5
years. The median is the midpoint of the middle two ages, 33 and 42; it
is not an observed age. The other 24 people lack a complete birth date
or a death record. Missing age at death is not an age of zero. Dates can
also be grouped by month, quarter or year. [Learn dates and ages
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-09-dates.md)

## Choose the output shape

A wide table has one row per participant; long retains visits; a panel
repeats time bins; survival records follow-up until an outcome or
censoring. Visit settings **9201** (inpatient) and **9202** (outpatient)
come from
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.html#find-inpatient-and-outpatient-visits).

``` r
visits <- omop_event("visit_occurrence", c(9201L, 9202L), name = "visits")
visitors <- omop_population("visitors", index = omop_index(visits))
shapes <- list(
  omop_output("wide", rows = "visitors", format = "wide"),
  omop_output("long", rows = "visitors", format = "long", sources = "visits"),
  omop_output("panel", rows = "visitors", format = "person_period",
    options = list(sources = list(visits), bins = list(width = 30L),
      window = omop_window(1, 90), analyses = "count")),
  omop_output("survival", rows = "visitors", format = "survival",
    options = list(outcomes = list(revisit = visits), tar = omop_window(1, 365),
      censor = list("observation_end", "death"))))
study <- omop_recipe(events = list(visits), populations = list(visitors),
  variables = list(n_visits = omop_variable(event = visits, reducer = omop_reduce("count"))),
  outputs = shapes)
targets <- c(wide = "D_tour_wide", long = "D_tour_long",
  panel = "D_tour_panel", survival = "D_tour_survival")
recipe_execute(study, out = targets, datasources = conns)
shape_sizes <- do.call(rbind, lapply(names(targets), function(shape)
  data.frame(Shape = shape, Rows = dsBaseClient::ds.dim(targets[[shape]],
    datasources = conns)[[1L]][1L])))
shape_sizes
```

| Shape    | Rows |
| :------- | ---: |
| wide     |   36 |
| long     |   60 |
| panel    |  108 |
| survival |   36 |

All four use the same 36 visitors. Long has 60 visits; the panel has 108
rows (36 people × three 30-day bins). Wide and survival each have 36
rows. Repeated rows are not independent participants. [Learn output
formats
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-08-outputs.md)

## A cause-of-death wide table

Give every person one row with their coded cause of death, then count
the causes. The capstone adds age, education, fever and visit periods to
the same table; the cause concepts come from
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.html#find-the-questionnaire-questions-answers-and-causes).

``` r
cause <- omop_variable(event = omop_event("death"),
  reducer = omop_reduce("first_value", value = "cause_concept_id"))
causes <- omop_recipe(variables = list(cause = cause),
  outputs = omop_output("people", format = "wide", grain = "person"))
recipe_execute(causes, out = c(people = "D_tour_causes"), datasources = conns)
dsBaseClient::ds.table("D_tour_causes$cause",
  useNA = "always", datasources = conns)$output.list$TABLES.COMBINED_all.sources_counts
```

    #> D_tour_causes$cause
    #> Myocardial_infarction             Pneumonia                    NA
    #>                    24                    12                    12

There are 24 myocardial infarction deaths, 12 pneumonia deaths and 12
people without a death record. The person key and individual answers
stay private. [Build and analyse this table
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-18-capstone.md)

Switch to Synthea27Nj for numeric values:

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.synthea", datasources = conns)
```

## Numbers and their groupings

Take each person’s mean haemoglobin, then compare means within three
teaching bands. Measurement **3000963** and g/dL unit **8713** are
identified in
[chapter 4](https://isglobal-brge.github.io/dsOMOPClient/articles/book-04-vocabulary.html#find-haemoglobin-glucose-and-their-units-in-synthea).

``` r
hb <- omop_variable(event = omop_event("measurement", 3000963L,
  unit = omop_unit(8713L)), reducer = omop_reduce("mean"))
band_names <- c("below 14", "14 to below 15", "15 or above")
labs <- omop_recipe(variables = list(hb = hb,
  band = omop_number_bins(hb, breaks = c(-Inf, 14, 15, Inf), labels = band_names)))
recipe_execute(labs, out = c(output = "D_tour_labs"), datasources = conns)
groups <- dsBaseClient::ds.meanSdGp("D_tour_labs$hb", "D_tour_labs$band",
  type = "combine", datasources = conns)
data.frame(`Band (g/dL)` = band_names,
  People = as.numeric(groups$Nvalid_gp), `Mean (g/dL)` = as.numeric(groups$Mean_gp),
  check.names = FALSE)
```

| Band (g/dL)    | People | Mean (g/dL) |
| :------------- | -----: | ----------: |
| below 14       |      9 |       13.34 |
| 14 to below 15 |      9 |       14.32 |
| 15 or above    |     10 |       15.61 |

The groups contain 9, 9 and 10 people, with means 13.34, 14.32 and 15.61
g/dL respectively. These are actual within-band means of person-level
means. The breaks are teaching choices, not clinical reference ranges.
[Learn lab values, units and groupings
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-14-labs.md)

## Derive and filter without downloading rows

Convert the person means to g/L, then keep people at or above the
teaching cutoff of 14 g/dL. Find the target unit by its exact name:

``` r
target_units <- ds.omop.concept.search("gram per liter", domain = "Unit",
  standard_only = FALSE, datasources = conns)$pooled
target_unit <- target_units[target_units$concept_name == "gram per liter", ]
target_unit[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name  |
| ----------: | :------------- |
|        8636 | gram per liter |

``` r
gl_id <- target_unit$concept_id
```

The selected unit is **8636**, gram per liter. One g/dL is ten g/L; the
conversion changes the scale, while the filter below changes which
people are included.

``` r
converted <- omop_recipe(variables = list(hb = hb,
  hb_gl = omop_number(hb, omop_number_spec(
    unit = omop_number_unit("unit:8713", paste0("unit:", gl_id), 10)))))
recipe_execute(converted, out = c(output = "D_tour_converted"), datasources = conns)
ds.omop.mutate("D_tour_converted", above_cutoff = ifelse(hb >= 14, 1L, 0L),
  newobj = "D_tour_flagged", datasources = conns)
ds.omop.filter("D_tour_flagged", expr = omop_where(above_cutoff == 1L),
  newobj = "D_tour_selected", datasources = conns)
dsBaseClient::ds.mean("D_tour_selected$hb_gl", datasources = conns)$Mean.by.Study
```

| EstimatedMean | Nmissing | Nvalid | Ntotal |
| ------------: | -------: | -----: | -----: |
|           150 |        0 |     19 |     19 |

The selected group has 19 people with no missing haemoglobin means. Its
mean is 150.00 g/L (15.00 g/dL), higher than the all-person mean of
144.67 g/L because the nine people below 14 g/dL were excluded. This is
a teaching rule, not a diagnosis. [Learn derived columns and filters
→](https://isglobal-brge.github.io/dsOMOPClient/articles/book-11-parameters.md)

## Recap

Choose meanings, time, units and table shape; compute at the server and
interpret the released summaries. Each linked chapter develops one of
these decisions.

**Next:** [0. Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
