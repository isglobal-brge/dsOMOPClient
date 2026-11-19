# 4\. Finding the concept IDs you need

**What you will learn**

Search for a meaning, inspect its hierarchy and clinical use, then save
a concept set.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter starts with `omop.gibleed`; later sections switch to the
questionnaire resources and `omop.synthea`.

## Start this chapter

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

## Search a name, then narrow the meaning

A concept is a public vocabulary meaning with a stable numeric ID. Its
**domain** is the kind of fact, such as Condition or Measurement; its
**vocabulary** is the coding system. Standard concepts (`S`) are
preferred OMOP targets. Classification concepts (`C`) group meanings;
non-standard source codes may map to standard ones.

``` r
hits <- ds.omop.concept.search("Asthma", datasources = conns)$pooled
hits[c("concept_id", "concept_name", "domain_id", "vocabulary_id")]
```

| concept\_id | concept\_name    | domain\_id | vocabulary\_id |
| ----------: | :--------------- | :--------- | :------------- |
|     4051466 | Childhood asthma | Condition  | SNOMED         |
|      317009 | Asthma           | Condition  | SNOMED         |
|     4062501 | Asthma screening | Procedure  | SNOMED         |

The word matches Asthma, Childhood asthma and Asthma screening, a
procedure. Keep the ID beside the name: a familiar word alone does not
specify which meaning you intend.

``` r
conditions <- ds.omop.concept.search("Asthma", domain = "Condition",
  vocabulary = "SNOMED", standard_only = TRUE, valid = TRUE, datasources = conns)$pooled
conditions[c("concept_id", "concept_name", "standard_concept", "concept_class_id")]
```

| concept\_id | concept\_name    | standard\_concept | concept\_class\_id |
| ----------: | :--------------- | :---------------- | :----------------- |
|     4051466 | Childhood asthma | S                 | Clinical Finding   |
|      317009 | Asthma           | S                 | Clinical Finding   |

``` r
asthma_id <- conditions$concept_id[conditions$concept_name == "Asthma"]
```

Asthma is **317009** and Childhood asthma is **4051466**. Both are
standard SNOMED clinical findings in the Condition domain. The filters
state the intended kind of code; they have not yet selected any people.

## Read a concept and its mappings

Look up the chosen ID to confirm its identity:

``` r
asthma_info <- ds.omop.concept.lookup(asthma_id, datasources = conns)$pooled
asthma_info
```

| concept\_id | concept\_name | domain\_id | vocabulary\_id | standard\_concept |
| ----------: | :------------ | :--------- | :------------- | :---------------- |
|      317009 | Asthma        | Condition  | SNOMED         | S                 |

The lookup confirms that 317009 means Asthma. For vocabulary validity
dates, use the richer search metadata:

``` r
conditions[conditions$concept_id == asthma_id,
  c("concept_id", "concept_name", "valid_start_date", "valid_end_date", "invalid_reason")]
```

| concept\_id | concept\_name | valid\_start\_date | valid\_end\_date | invalid\_reason |
| ----------: | :------------ | :----------------- | :--------------- | :-------------- |
|      317009 | Asthma        | 1970-01-01         | 2099-12-31       | NA              |

The validity interval is about the vocabulary entry, not patients’
diagnosis dates. A missing `invalid_reason` means the entry has not been
marked invalid.

A **Maps to** relationship connects a source code to its standard
target. Find gastrointestinal haemorrhage to see a real mapping in this
vocabulary:

``` r
bleeding <- ds.omop.concept.search("Gastrointestinal hemorrhage", domain = "Condition",
  datasources = conns)$pooled
bleeding[c("concept_id", "concept_name", "vocabulary_id", "standard_concept")]
```

| concept\_id | concept\_name               | vocabulary\_id | standard\_concept |
| ----------: | :-------------------------- | :------------- | :---------------- |
|      192671 | Gastrointestinal hemorrhage | SNOMED         | S                 |

``` r
bleeding_id <- bleeding$concept_id[bleeding$concept_name == "Gastrointestinal hemorrhage"]
```

The standard SNOMED concept is **192671**. Inspect the mapping edges
touching it:

``` r
mappings <- ds.omop.concept.relationships(bleeding_id,
  relationship_id = "Maps to", datasources = conns)$pooled
mappings[c("concept_id", "related_concept_id", "related_concept_name",
  "relationship_id", "direction")]
```

| concept\_id | related\_concept\_id | related\_concept\_name                   | relationship\_id | direction |
| :---------- | :------------------- | :--------------------------------------- | :--------------- | :-------- |
| 192671      | 35208414             | Gastrointestinal hemorrhage, unspecified | Maps to          | reverse   |

`reverse` means the related concept **35208414** is the source of the
mapping into the requested target **192671**. Inspect that source code
explicitly:

``` r
source_code <- ds.omop.concept.search(NULL, concept_id = 35208414L,
  standard_only = FALSE, datasources = conns)$pooled
source_code[c("concept_id", "concept_name", "vocabulary_id",
  "concept_code", "standard_concept")]
```

| concept\_id | concept\_name                            | vocabulary\_id | concept\_code | standard\_concept |
| ----------: | :--------------------------------------- | :------------- | :------------ | :---------------- |
|    35208414 | Gastrointestinal hemorrhage, unspecified | ICD10CM        | K92.2         | NA                |

The source is ICD10CM **K92.2**, Gastrointestinal hemorrhage,
unspecified; its standard status is missing. It maps to the standard
SNOMED target. A mapping translates a code; adding descendants instead
broadens a clinical definition.

## Walk down and up the hierarchy

Descendants are more specific meanings; ancestors are broader meanings.
Inspect them before including a whole branch in your study.

``` r
desc <- ds.omop.concept.descendants(asthma_id, include_self = FALSE, datasources = conns)$pooled
desc[c("concept_id", "concept_name")]
```

| concept\_id | concept\_name    |
| ----------: | :--------------- |
|     4051466 | Childhood asthma |

``` r
child_id <- desc$concept_id[desc$concept_name == "Childhood asthma"]
```

Childhood asthma (4051466) is a descendant of Asthma (317009). A set
that includes descendants will include both meanings unless you exclude
one.

``` r
parents <- ds.omop.concept.ancestors(child_id, datasources = conns)$pooled
parents[parents$direction == "ancestor",
  c("concept_id", "concept_name", "direction")]
```

| concept\_id | concept\_name | direction |
| ----------: | :------------ | :-------- |
|      317009 | Asthma        | ancestor  |

The `ancestor` row leads back to Asthma. This API can return both
directions; selecting `direction == "ancestor"` is what makes this an
upward walk.

## Check that the concept occurs here

A catalogue entry does not establish clinical use. Despite its name,
`concept.prevalence()` returns protected **person counts** by default. A
prevalence estimate would also need a defined population, period and
denominator.

``` r
use <- ds.omop.concept.prevalence("condition_occurrence", metric = "persons",
  top_n = 20, type = "both", datasources = conns)$pooled
use[use$concept_id %in% c(asthma_id, child_id, bleeding_id),
  c("concept_id", "concept_name", "n_persons")]
```

| concept\_id | concept\_name               | n\_persons |
| ----------: | :-------------------------- | ---------: |
|      192671 | Gastrointestinal hemorrhage |        475 |

Only gastrointestinal haemorrhage appears in these top 20 results, with
a band of 475–479 people. Neither asthma concept made this top list.
Their absence is not evidence of zero use; the targeted locator below
answers that question.

Locate both IDs to check which tables and columns actually use them:

``` r
locations <- ds.omop.concept.locate(c(asthma_id, child_id), type = "both", datasources = conns)$pooled
locations$concept_name <- conditions$concept_name[match(locations$concept_id, conditions$concept_id)]
locations[locations$table_name == "condition_occurrence",
  c("concept_id", "concept_name", "table_name", "n_records", "n_persons")]
```

| concept\_id | concept\_name    | table\_name           | n\_records | n\_persons |
| ----------: | :--------------- | :-------------------- | ---------: | ---------: |
|      317009 | Asthma           | condition\_occurrence |          5 |          5 |
|     4051466 | Childhood asthma | condition\_occurrence |         95 |         95 |

The record bands begin at 5 for Asthma and 95 for Childhood asthma: 5–9
and 95–99 records respectively. Looking only for the seed would miss
most records in this branch. Counts are rounded down to multiples of
five, so do not treat these as exact counts or calculate a precise
percentage from them.

## Save the definition, including exclusions

A concept set records your selection rule, including whether descendants
belong. It is a local definition, not a list of patients.

``` r
asthma <- omop_concept_set(asthma_id, descendants = TRUE)
concept_file <- "book-concepts.rds"
saveRDS(list(asthma = asthma), concept_file)
asthma
```

    #> omop_concept_set : 317009
    #>   includes: descendants

The printed definition includes the seed and requests its descendants.
`book-concepts.rds` now stores that definition for reuse. Setting
`descendants = FALSE` would select only the seed.

**Exercise 1.** Keep Asthma’s children but exclude the seed itself. Save
the set and read it back. Does the saved definition preserve the
exclusion?

Solution

``` r
children_only <- omop_concept_set(asthma_id, descendants = TRUE,
  exclude = omop_concept_set(asthma_id, descendants = FALSE))
saved_sets <- readRDS(concept_file)
saved_sets$children_only <- children_only
saveRDS(saved_sets, concept_file)
saved_sets <- readRDS(concept_file)
saved_sets$children_only
```

    #> omop_concept_set : 317009
    #>   includes: descendants, exclusions

The reloaded definition still requests descendants and excludes 317009
itself. An excluded set could also include descendants if you wanted to
remove an entire branch. This file contains definitions, not patient
data.

## Find inpatient and outpatient visits

Use `visit_concept_id` for the setting; `visit_type_concept_id`
describes the record’s provenance. Discover the settings before using
their IDs:

``` r
visit_hits <- ds.omop.concept.search("Visit", domain = "Visit", datasources = conns)$pooled
visit_catalogue <- visit_hits[visit_hits$concept_name %in%
  c("Inpatient Visit", "Outpatient Visit"), ]
visit_catalogue[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name    | standard\_concept |
| ----------: | :--------------- | :---------------- |
|        9201 | Inpatient Visit  | S                 |
|        9202 | Outpatient Visit | S                 |

``` r
visit_ids <- setNames(visit_catalogue$concept_id, visit_catalogue$concept_name)
visit_sets <- lapply(visit_ids, omop_concept_set)
```

Inpatient Visit is **9201** and Outpatient Visit is **9202**. Both exist
in the vocabulary. Check their clinical use separately:

``` r
visit_use <- ds.omop.concept.locate(unname(visit_ids), type = "both", datasources = conns)$pooled
visit_use$concept_name <- names(visit_ids)[match(visit_use$concept_id, visit_ids)]
visit_use[c("concept_id", "concept_name", "n_records", "n_persons")]
```

| concept\_id | concept\_name   | n\_records | n\_persons |
| ----------: | :-------------- | ---------: | ---------: |
|        9201 | Inpatient Visit |       1035 |        890 |

Only Inpatient Visit is returned: its bands begin at 1,035 records and
890 people. The public GiBleed data record only inpatient visits;
Outpatient Visit is unused. In an unfamiliar private dataset, an absent
locator row could also be withheld.

## Find the questionnaire questions, answers and causes

Switch to the separate synthetic dataset described in
[chapter 0](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#the-resources-on-the-server).
Its local question and answer codes are teaching examples, not codes to
copy into a clinical study.

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.questionnaire", datasources = conns)
```

``` r
questionnaire <- ds.omop.concept.search(NULL, standard_only = FALSE,
  limit = 100, datasources = conns)$pooled
questionnaire[c("concept_id", "concept_name", "domain_id")]
```

| concept\_id | concept\_name                | domain\_id  |
| ----------: | :--------------------------- | :---------- |
|      900001 | Education question           | Observation |
|      900002 | Fever question               | Observation |
|      900101 | Secondary education          | Meas Value  |
|      900102 | University education         | Meas Value  |
|      900201 | Myocardial infarction        | Condition   |
|      900202 | Pneumonia                    | Condition   |
|      900301 | Yes                          | Meas Value  |
|        8532 | FEMALE                       | Gender      |
|        8507 | MALE                         | Gender      |
|      900302 | No                           | Meas Value  |
|        9201 | Inpatient Visit              | Visit       |
|        9202 | Outpatient Visit             | Visit       |
|      950001 | Questionnaire study medicine | Drug        |
|      950002 | Body weight (questionnaire)  | Measurement |
|        9529 | kilogram                     | Unit        |

``` r
question_ids <- setNames(questionnaire$concept_id, questionnaire$concept_name)
education_set <- omop_concept_set(unname(question_ids["Education question"]))
cause_sets <- lapply(question_ids[c("Myocardial infarction", "Pneumonia")], omop_concept_set)
```

Education question **900001** selects observations; Secondary **900101**
and University **900102** are its coded answers. Fever question
**900002** uses Yes **900301** and No **900302**. The death causes are
Myocardial infarction **900201** and Pneumonia **900202**. Visits use
the Visit domain. Study medicine **950001** supports drug-history
lessons; weight **950002**, with kilogram **9529**, supports numeric
exercises. The default local question and cause codes have no external
mapping or hierarchy.

``` r
question_presence <- ds.omop.concept.prevalence("observation", type = "both", datasources = conns)$pooled
question_presence[c("concept_id", "concept_name", "n_persons")]
```

| concept\_id | concept\_name      | n\_persons |
| ----------: | :----------------- | ---------: |
|      900001 | Education question |         20 |
|      900002 | Fever question     |         20 |

The person bands for education and fever both begin at 20, meaning 20–24
people. The public dataset has 24 respondents to each question. Later
categorical tables can release those exact counts when their cells pass
the table’s checks.

``` r
cause_use <- ds.omop.concept.locate(unname(question_ids[c("Myocardial infarction", "Pneumonia")]),
  type = "both", datasources = conns)$pooled
cause_use$concept_name <- names(question_ids)[match(cause_use$concept_id, question_ids)]
cause_use[c("concept_id", "concept_name", "table_name", "n_records", "n_persons")]
```

| concept\_id | concept\_name         | table\_name | n\_records | n\_persons |
| ----------: | :-------------------- | :---------- | ---------: | ---------: |
|      900201 | Myocardial infarction | death       |         20 |         20 |
|      900202 | Pneumonia             | death       |         10 |         10 |

The cause bands begin at 20 for infarction and 10 for pneumonia: 20–24
and 10–14 people. They contain the dataset’s exact totals of 24 and 12.
The different representations are not conflicting counts.

**Exercise 2.** ATC classes are classification concepts. Can a
standard-only search miss them even when they are present? Use the
optional teaching catalogue.

Solution

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.questionnaire_vocab", datasources = conns)
```

``` r
atc_default <- ds.omop.concept.search(NULL, vocabulary = "ATC", datasources = conns)$pooled
atc_default
```

| concept\_id | concept\_name | domain\_id | vocabulary\_id | standard\_concept | concept\_class\_id | concept\_code | valid\_start\_date | valid\_end\_date | invalid\_reason |
| ----------- | ------------- | ---------- | -------------- | ----------------- | ------------------ | ------------- | ------------------ | ---------------- | --------------- |

`NULL` means the search returned no matching concepts: the default asks
for standard concepts (`S`). Request classification concepts (`C`)
instead:

``` r
atc <- ds.omop.concept.search(NULL, vocabulary = "ATC", standard = "C", datasources = conns)$pooled
atc[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name             | standard\_concept |
| ----------: | :------------------------ | :---------------- |
|      960001 | Teaching ATC first level  | C                 |
|      960002 | Teaching ATC second level | C                 |

Two classes appear: **960001** and **960002**. These are explicitly
invented teaching classes, used in the drug-utilisation lesson. The
lesson here is the search filter: a missing standard-only hit does not
prove a class is absent.

## Find haemoglobin, glucose and their units in Synthea

Switch to the numeric resource. The vocabulary spells haemoglobin
“Hemoglobin”. Select a precise test and specimen, rather than every
name containing the word.

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.synthea", datasources = conns)
```

``` r
lab_hits <- ds.omop.concept.search("Hemoglobin", domain = "Measurement", datasources = conns)$pooled
lab_hits[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name                                                                         | standard\_concept |
| ----------: | :------------------------------------------------------------------------------------ | :---------------- |
|     3001643 | Hemoglobin and Hematocrit panel - Blood                                               | S                 |
|     3004410 | Hemoglobin A1c/Hemoglobin.total in Blood                                              | S                 |
|    40761014 | Hemoglobin.gastrointestinal.lower \[Presence\] in Stool by Immunoassay --1st specimen | S                 |
|     3011397 | Hemoglobin \[Presence\] in Urine by Test strip                                        | S                 |
|     3000963 | Hemoglobin \[Mass/volume\] in Blood                                                   | S                 |

``` r
hb_id <- lab_hits$concept_id[lab_hits$concept_name == "Hemoglobin [Mass/volume] in Blood"]
```

Choose **3000963**, Hemoglobin \[Mass/volume\] in Blood. A panel and a
haemoglobin fraction are different measurements even though their names
share the word.

``` r
glucose_hits <- ds.omop.concept.search("Glucose", domain = "Measurement", datasources = conns)$pooled
glucose_hits[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name                                  | standard\_concept |
| ----------: | :--------------------------------------------- | :---------------- |
|     4012477 | Glucose tolerance test                         | S                 |
|     4149386 | Urine screening for glucose                    | S                 |
|     4146454 | Self-monitoring of blood glucose               | S                 |
|     3000483 | Glucose \[Mass/volume\] in Blood               | S                 |
|     3004501 | Glucose \[Mass/volume\] in Serum or Plasma     | S                 |
|     3009261 | Glucose \[Presence\] in Urine by Test strip    | S                 |
|     3024629 | Glucose \[Mass/volume\] in Urine by Test strip | S                 |

``` r
glucose_id <- glucose_hits$concept_id[glucose_hits$concept_name == "Glucose [Mass/volume] in Blood"]
lab_sets <- list(haemoglobin = omop_concept_set(hb_id), glucose = omop_concept_set(glucose_id))
```

Blood glucose is **3000483**. Serum or Plasma glucose and urine glucose
are separate concepts; do not combine all search hits without deciding
which specimens and concentration measures answer your question.

``` r
units <- ds.omop.value.counts("measurement", "unit_concept_id", concept_id = hb_id,
  type = "both", datasources = conns)$pooled
names(units)[names(units) == "value"] <- "unit_concept_id"
units[c("unit_concept_id", "concept_name", "n")]
```

| unit\_concept\_id | concept\_name      |   n |
| :---------------- | :----------------- | --: |
| 8713              | gram per deciliter | 105 |

Haemoglobin uses **8713**, gram per deciliter (g/dL), for 105–109
recorded values. The count describes recorded unit use; a unit’s mere
presence in the vocabulary would not establish this.

### When a unit count is withheld

For Serum or Plasma glucose, the requested unit count lacks enough
support. Keep the server’s explanation instead of treating an
unavailable result as zero:

``` r
serum_glucose_id <- glucose_hits$concept_id[glucose_hits$concept_name ==
  "Glucose [Mass/volume] in Serum or Plasma"]
serum_glucose_units <- ds.omop.value.counts("measurement", "unit_concept_id",
  concept_id = serum_glucose_id, type = "both", datasources = conns)
serum_glucose_units$meta$warnings
```

    #> [1] "Server errors: site: Disclosive: operation blocked - insufficient individuals to meet disclosure threshold. No further details available."
    #> [2] "No complete disclosure-safe pooled value counts were available."

The aggregate is unavailable; this does not establish that the concept
has no records. Use the separately selected Blood glucose for the next
exercise.

**Exercise 3.** Check Blood glucose’s unit and add the lab and
questionnaire sets to `book-concepts.rds` without discarding the saved
asthma exclusions.

Solution

``` r
glucose_units <- ds.omop.value.counts("measurement", "unit_concept_id",
  concept_id = glucose_id, type = "both", datasources = conns)$pooled
names(glucose_units)[names(glucose_units) == "value"] <- "unit_concept_id"
glucose_units[c("unit_concept_id", "concept_name", "n")]
```

| unit\_concept\_id | concept\_name           |   n |
| :---------------- | :---------------------- | --: |
| 8840              | milligram per deciliter | 225 |

``` r
saved_sets <- readRDS(concept_file)
saved_sets$sets <- lab_sets
saved_sets$visit_sets <- visit_sets
saved_sets$education <- education_set
saved_sets$causes <- cause_sets
saved_sets$questionnaire_catalogue <- questionnaire
saved_sets$glucose_catalogue <- glucose_hits
saveRDS(saved_sets, concept_file)
```

Blood glucose uses **8840**, milligram per deciliter (mg/dL), for
225–229 records. Reading the file before adding entries preserves
`asthma` and `children_only`. To reuse education in your own later
session, read `readRDS("book-concepts.rds")$education`. The following
chapters name the IDs from this catalogue so they can also run
independently in a fresh session.

## Find measured BMI and its recorded unit

Measured BMI is a recorded value, distinct from calculating it from
height and weight.

``` r
bmi_hits <- ds.omop.concept.search("Body mass index", domain = "Measurement",
  vocabulary = "LOINC", datasources = conns)$pooled
bmi_hits[c("concept_id", "concept_name", "standard_concept")]
```

| concept\_id | concept\_name                                        | standard\_concept |
| ----------: | :--------------------------------------------------- | :---------------- |
|     3038553 | Body mass index (BMI) \[Ratio\]                      | S                 |
|    40762638 | Body mass index (BMI) \[Percentile\] Per age and sex | S                 |

``` r
bmi_id <- bmi_hits$concept_id[bmi_hits$concept_name == "Body mass index (BMI) [Ratio]"]
```

Choose the standard LOINC measurement **3038553**, Body mass index (BMI)
\[Ratio\]. The repeated-measures chapter uses this definition.

``` r
bmi_use <- ds.omop.concept.locate(bmi_id, type = "both", datasources = conns)$pooled
bmi_use$concept_name <- bmi_hits$concept_name[match(bmi_use$concept_id, bmi_hits$concept_id)]
bmi_use[c("concept_id", "concept_name", "n_records", "n_persons")]
```

| concept\_id | concept\_name                   | n\_records | n\_persons |
| ----------: | :------------------------------ | ---------: | ---------: |
|     3038553 | Body mass index (BMI) \[Ratio\] |        435 |         25 |

The bands begin at 435 records and 25 people, showing repeated
measurements. Check the recorded unit before summarising those values:

``` r
bmi_units <- ds.omop.value.counts("measurement", "unit_concept_id",
  concept_id = bmi_id, type = "both", datasources = conns)$pooled
names(bmi_units)[names(bmi_units) == "value"] <- "unit_concept_id"
bmi_units[c("unit_concept_id", "concept_name", "n")]
```

| unit\_concept\_id | concept\_name             |   n |
| :---------------- | :------------------------ | --: |
| 9531              | kilogram per square meter | 435 |

``` r
saved_sets <- readRDS(concept_file)
saved_sets$measured_bmi <- omop_concept_set(bmi_id)
saved_sets$bmi_units <- bmi_units
saveRDS(saved_sets, concept_file)
```

BMI uses **9531**, kilogram per square meter. Its record band also
begins at 435. The saved set and unit record preserve both parts of the
measurement definition.

## Recap

Search, inspect meanings and mappings, check the hierarchy and clinical
use, then save the definition. Keep IDs alongside names during
discovery; analysis tables can then present the selected concepts as
named categories.

**Next:** [5. A first
recipe](https://isglobal-brge.github.io/dsOMOPClient/articles/book-05-recipe.md).
