# Synthetic questionnaire: four patterns, twelve people per pattern (48 total).
# A: Secondary -> University, two visits, MI; B: University -> Secondary,
# two visits, pneumonia; C: no answers, one visit, MI, birth year only;
# D: no answers, visits or death. A/B also answer Yes/No to fever.
# There are 48 education rows from 24 respondents, 24 fever rows, 60 visits
# (24 inpatient / 36 outpatient), and 36 deaths. Each year, 2023 and 2024,
# has 12 MI deaths (4 F / 8 M) and 6 pneumonia deaths (3 F / 3 M).
# Survivors: 6 F / 6 M. Total: 20 F / 28 M. Observation: 2019--2025.
# Copies 0--5 die in 2023, copies 6--11 in 2024. Within each half A/C
# have two women and four men; B/D have three women and three men.
# Site2 replaces ALL Secondary answers with University, leaving that level
# absent in its records; its newer wording/version also tests federation.
# Optional formulas add one +12 kg same-day weight for every A person.
# The table below describes these public synthetic inputs, not a data release.
book_fixture_structure <- function() {
  data.frame(
    Item = c("People", "Education respondents", "Education answer rows",
      "Fever answer rows", "Inpatient visits", "Outpatient visits",
      "2023: myocardial infarction", "2023: pneumonia",
      "2024: myocardial infarction", "2024: pneumonia", "Survivors"),
    Total = c(48L, 24L, 48L, 24L, 24L, 36L, 12L, 6L, 12L, 6L, 12L),
    Female = c(20L, 10L, 20L, 10L, 10L, 14L, 4L, 3L, 4L, 3L, 6L),
    Male = c(28L, 14L, 28L, 14L, 14L, 22L, 8L, 3L, 8L, 3L, 6L))
}
book_fixture <- function(two_sites=FALSE, threshold=5L, levels_max=NULL, categories=FALSE, formulas=FALSE, symbol="omop", env=parent.frame(), drug_histories=FALSE) {
library(dsOMOP)
library(dsOMOPClient)
library(DSLite)
library(DBI)
# DSLite 1.5's quoted-string lexer rejects dsBase expressions containing commas.
# Same test-only tokenizer repair as test-concept-label-analysis.R; no method,
# result, policy or disclosure check is replaced.
lexer <- get("Lexer", asNamespace("DSLite"))
old_double <- lexer$public_fields$t_DBLSTRING
old_single <- lexer$public_fields$t_STRING
withr::defer({ lexer$public_fields$t_DBLSTRING <- old_double
              lexer$public_fields$t_STRING <- old_single }, envir=env)
lexer$public_fields$t_DBLSTRING <- r"("([^"\\]|\\.)*")"
lexer$public_fields$t_STRING <- r"('([^'\\]|\\.)*')"
withr::local_options(list(nfilter.subset = 5, default.nfilter.subset = 5,
        dsomop.pseudonym_provider = "injected",
        dsomop.pseudonym_root = as.raw(openssl::sha256(charToRaw("synthetic-questionnaire-fixture")))),
        .local_envir = env)
fixture_path <- tempfile(fileext = ".sqlite")
db <- DBI::dbConnect(RSQLite::SQLite(), fixture_path)
DBI::dbWriteTable(db, "person", data.frame(person_id = 1:4,
  gender_concept_id = 8532L, year_of_birth = c(1980L, 1990L, 2000L, 1975L),
  month_of_birth = c(12L, 3L, NA_integer_, 6L), day_of_birth = c(31L, 15L, NA_integer_, 1L), race_concept_id = 0L,
  ethnicity_concept_id = 0L))
DBI::dbWriteTable(db, "observation_period", data.frame(observation_period_id = 1:4,
  person_id = 1:4, observation_period_start_date = "2019-01-01",
  observation_period_end_date = "2025-12-31", period_type_concept_id = 0L))
DBI::dbWriteTable(db, "observation", data.frame(observation_id = 1:5,
  person_id = c(1L, 1L, 2L, 2L, 1L),
  observation_concept_id = c(900001L, 900001L, 900001L, 900001L, 900002L),
  observation_date = c("2020-01-01", "2022-01-01", "2020-02-01", "2023-02-01", "2022-06-01"),
  observation_type_concept_id = 0L, value_as_number = NA_real_,
  value_as_concept_id = c(900101L, 900102L, 900102L, 900101L, 900301L)))
DBI::dbWriteTable(db, "visit_occurrence", data.frame(visit_occurrence_id = 1:5,
  person_id = c(1L, 1L, 2L, 2L, 3L), visit_concept_id = 0L,
  visit_start_date = c("2023-10-01", "2020-06-15", "2021-03-15", "2020-03-14", "2020-07-20"),
  visit_end_date = c("2023-10-01", "2020-06-15", "2021-03-15", "2020-03-14", "2020-07-20"),
  visit_type_concept_id = 0L))
DBI::dbWriteTable(db, "death", data.frame(person_id = 1:3,
  death_date = c("2024-12-01", "2024-03-14", "2024-07-20"), death_type_concept_id = 0L,
  cause_concept_id = c(900201L, 900202L, 900201L)))
DBI::dbWriteTable(db, "concept", data.frame(
  concept_id = c(900001L, 900002L, 900101L, 900102L, 900201L, 900202L, 900301L),
  concept_name = c("Education question", "Fever question", "Secondary education", "University education", "Myocardial infarction", "Pneumonia", "Yes"),
  domain_id = c("Observation", "Observation", "Meas Value", "Meas Value", "Condition", "Condition", "Meas Value"),
  vocabulary_id = "Questionnaire", concept_class_id = "Questionnaire", standard_concept = "S",
  concept_code = as.character(c(900001L, 900002L, 900101L, 900102L, 900201L, 900202L, 900301L)),
  valid_start_date = "1970-01-01", valid_end_date = "2099-12-31", invalid_reason = NA_character_))
DBI::dbWriteTable(db, "cdm_source", data.frame(cdm_source_name = "Synthetic questionnaire dataset",
  cdm_source_abbreviation = "questionnaire", cdm_holder = "Practice", source_description = "Synthetic only",
  source_documentation_reference = "", cdm_etl_reference = "", source_release_date = "2025-01-01",
  cdm_release_date = "2025-01-01", cdm_version = "5.4", cdm_version_concept_id = 0L,
  vocabulary_version = "2025-01-01"))
# Repeat the four patterns twelve times; all cause/year/sex cells pass nfilter.tab.
for (tab in c("person", "observation_period", "observation", "visit_occurrence", "death")) {
  base <- DBI::dbReadTable(db, tab)
  key <- switch(tab, person = "person_id", observation_period = "observation_period_id",
    observation = "observation_id", visit_occurrence = "visit_occurrence_id", death = "person_id")
  copies <- lapply(0:11, function(i) {
    d <- base
    if (key != "person_id") d[[key]] <- d[[key]] + i * nrow(base)
    d$person_id <- d$person_id + i * 4L
    if (tab == "person") {
      female <- ifelse(base$person_id %in% c(1L, 3L), i %% 6L < 2L, i %% 6L < 3L)
      d$gender_concept_id <- ifelse(female, 8532L, 8507L)
    }
    if (tab == "death" && i < 6L) d$death_date <- sub("2024", "2023", d$death_date)
    d
  })
  DBI::dbWriteTable(db, tab, do.call(rbind, copies), overwrite = TRUE)
}
sex_concepts <- DBI::dbReadTable(db, "concept")[rep(1, 2), ]
sex_concepts$concept_id <- c(8532L, 8507L)
sex_concepts$concept_name <- c("FEMALE", "MALE")
sex_concepts$domain_id <- "Gender"
sex_concepts$vocabulary_id <- "Gender"
sex_concepts$concept_class_id <- "Gender"
sex_concepts$concept_code <- c("F", "M")
DBI::dbWriteTable(db, "concept", sex_concepts, append = TRUE)
# Extend the released VA fixture with a second fever answer and visit settings.
DBI::dbExecute(db, "UPDATE visit_occurrence SET visit_concept_id = CASE WHEN visit_occurrence_id % 5 IN (1,3) THEN 9201 ELSE 9202 END")
DBI::dbWriteTable(db, "observation", data.frame(observation_id=61:72,
  person_id=seq(2L,46L,4L), observation_concept_id=900002L,
  observation_date="2022-06-01", observation_type_concept_id=0L,
  value_as_number=NA_real_, value_as_concept_id=900302L), append=TRUE)
extra <- DBI::dbReadTable(db,"concept")[rep(7,3),]
extra$concept_id <- c(900302L,9201L,9202L)
extra$concept_name <- c("No","Inpatient Visit","Outpatient Visit")
extra$concept_code <- as.character(extra$concept_id)
extra[extra$concept_id %in% c(9201L, 9202L),
      c("domain_id", "vocabulary_id", "concept_class_id")] <- "Visit"
DBI::dbWriteTable(db,"concept",extra,append=TRUE)
DBI::dbWriteTable(db,"concept_ancestor",data.frame(
  ancestor_concept_id=integer(),descendant_concept_id=integer(),
  min_levels_of_separation=integer(),max_levels_of_separation=integer()))
DBI::dbWriteTable(db,"vocabulary",data.frame(vocabulary_id="Questionnaire",vocabulary_version="2025-01-01"))
# Weight (950002, kg) supports event examples and duplicate-collapse practice.
# Medicine (950001) supports new users in chapter 7 and supply histories in 22.
# Cohort 1 contains groups A and C.
measurements <- data.frame(measurement_id=1:5, person_id=c(1L,1L,2L,2L,3L),
  measurement_concept_id=950002L,
  measurement_date=c("2020-06-14","2020-06-16","2020-03-13","2020-03-14","2020-07-19"),
  measurement_type_concept_id=0L, value_as_number=c(60,66,80,NA,100),
  unit_concept_id=9529L)
drugs <- data.frame(drug_exposure_id=1:3, person_id=1:3,
  drug_concept_id=950001L, drug_exposure_start_date="2020-08-01",
  drug_exposure_end_date="2020-08-30", drug_type_concept_id=0L, days_supply=30L)
cohort <- data.frame(cohort_definition_id=1L, subject_id=c(1L,3L),
  cohort_start_date="2020-01-01", cohort_end_date="2025-12-31")
for (tab in c("measurement", "drug_exposure", "cohort")) {
  base <- switch(tab, measurement=measurements, drug_exposure=drugs, cohort=cohort)
  rows <- lapply(0:11, function(i) {
    d <- base
    key <- if (tab == "cohort") "subject_id" else "person_id"
    d[[key]] <- d[[key]] + i*4L
    if (tab != "cohort") d[[paste0(tab,"_id")]] <- d[[paste0(tab,"_id")]] + i*nrow(base)
    d
  })
  DBI::dbWriteTable(db, tab, do.call(rbind, rows))
}
extra <- DBI::dbReadTable(db,"concept")[rep(1,3),]
extra$concept_id <- c(950001L,950002L,9529L)
extra$concept_name <- c("Questionnaire study medicine","Body weight (questionnaire)","kilogram")
extra$domain_id <- c("Drug","Measurement","Unit")
extra$concept_code <- as.character(extra$concept_id)
DBI::dbWriteTable(db,"concept",extra,append=TRUE)
# Optional public vocabulary for category teaching; source patient rows are unchanged.
if (categories) {
  DBI::dbExecute(db,"UPDATE concept SET concept_code = CASE concept_id WHEN 900101 THEN 'secondary' WHEN 900102 THEN 'tertiary' ELSE concept_code END")
  concepts <- data.frame(
    concept_id=c(900103L,4180186L,960001L,960002L,960010L,960011L,960020L,960021L),
    concept_name=c("Primary education","English language","Teaching ATC first level",
      "Teaching ATC second level","Teaching circulatory code","Teaching respiratory code",
      "Teaching circulatory chapter","Teaching respiratory chapter"),
    domain_id=c("Meas Value","Language","Drug","Drug",rep("Condition",4)),
    vocabulary_id=c("Questionnaire","Language","ATC","ATC",rep("ICD10",4)),
    concept_class_id=c("Questionnaire","Language","ATC 1st","ATC 2nd",
      "ICD10 Hierarchy","ICD10 Hierarchy","ICD10 Chapter","ICD10 Chapter"),
    standard_concept=c("S","S","C","C",NA,NA,"C","C"),
    concept_code=c("primary","en","T","T01","T-C","T-R","T-IX","T-X"),
    valid_start_date="1970-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_)
  DBI::dbWriteTable(db,"concept",concepts,append=TRUE)
  DBI::dbWriteTable(db,"concept_synonym",data.frame(concept_id=c(900101L,900102L),
    concept_synonym_name=c("Secondary schooling","Tertiary schooling"),language_concept_id=4180186L))
  DBI::dbWriteTable(db,"concept_ancestor",data.frame(
    ancestor_concept_id=c(960001L,960002L,960001L),descendant_concept_id=c(950001L,950001L,960002L),
    min_levels_of_separation=c(2L,1L,1L),max_levels_of_separation=c(2L,1L,1L)),append=TRUE)
  DBI::dbWriteTable(db,"concept_relationship",data.frame(
    concept_id_1=c(960010L,960011L,960010L,960011L),
    concept_id_2=c(900201L,900202L,960020L,960021L),
    relationship_id=c("Maps to","Maps to","Is a","Is a"),
    valid_start_date="1970-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_),overwrite=TRUE)
}
# Optional longitudinal supply example; default chapter fixtures stay unchanged.
if (drug_histories) {
  stopifnot(categories)
  # Twelve people per public pattern: continuous supply, overlapping fills,
  # and a restart after a gap longer than the 30-day persistence rule.
  patterns <- list(list(start=c(0L,100L,220L), supply=c(120L,120L,125L)),
    list(start=c(0L,30L), supply=c(60L,60L)),
    list(start=c(0L,120L), supply=c(30L,30L)))
  histories <- lapply(0:11, function(i) do.call(rbind, lapply(1:3, function(j) {
    p <- patterns[[j]]
    data.frame(person_id=i*4L+j, drug_concept_id=950001L,
      drug_exposure_start_date=as.character(as.Date("2020-08-01")+p$start),
      drug_exposure_end_date=as.character(as.Date("2020-08-01")+p$start+p$supply-1L),
      drug_type_concept_id=0L, days_supply=p$supply)
  })))
  histories <- do.call(rbind,histories)
  histories$drug_exposure_id <- seq_len(nrow(histories))
  DBI::dbWriteTable(db,"drug_exposure",histories,overwrite=TRUE)
}
# Optional explicitly synthetic formula practice, separate from Synthea.
if (formulas) {
  new <- data.frame(concept_id=c(970001L,8840L),
    concept_name=c("Teaching standardized serum creatinine","milligram per deciliter"),
    domain_id=c("Measurement","Unit"),
    vocabulary_id="Questionnaire",concept_class_id="Questionnaire",standard_concept="S",
    concept_code=c("scr","mg/dL"),valid_start_date="1970-01-01",
    valid_end_date="2099-12-31",invalid_reason=NA_character_)
  DBI::dbWriteTable(db,"concept",new,append=TRUE)
  DBI::dbWriteTable(db,"measurement",data.frame(measurement_id=1001:1048,person_id=1:48,
    measurement_concept_id=970001L,measurement_date="2022-06-01",measurement_type_concept_id=0L,
    value_as_number=rep(c(.7,.9,1.4,1.8),12),unit_concept_id=8840L),append=TRUE)
  duplicates <- DBI::dbGetQuery(db, "SELECT * FROM measurement WHERE measurement_concept_id = 950002 AND measurement_date = '2020-06-14'")
  duplicates$measurement_id <- 2000L + seq_len(nrow(duplicates))
  duplicates$value_as_number <- duplicates$value_as_number + 12
  DBI::dbWriteTable(db, "measurement", duplicates, append=TRUE)
}
DBI::dbDisconnect(db)

config <- DSLite::defaultDSConfiguration(include = c("dsOMOP","dsBase"))
config$Options$nfilter.tab <- 3L
config$Options$nfilter.subset <- threshold
config$Options$default.nfilter.subset <- threshold
if(!is.null(levels_max)) config$Options$nfilter.levels.max <- levels_max
server <- DSLite::newDSLiteServer(
  resources = list(fixture = resourcer::newResource(name = "fixture",
    url = paste0("omop+dbi:sqlite://", fixture_path), format = "omop.dbi.db")),
  config = config)
second_path<-NULL
if(two_sites) {
  second_path<-tempfile(fileext=".sqlite");file.copy(fixture_path,second_path)
  db<-DBI::dbConnect(RSQLite::SQLite(),second_path)
  DBI::dbExecute(db,"UPDATE concept SET concept_name = 'University qualification' WHERE concept_id = 900102")
  DBI::dbExecute(db,"UPDATE vocabulary SET vocabulary_version = '2026-01-01'")
  DBI::dbExecute(db,"UPDATE cdm_source SET vocabulary_version = '2026-01-01'")
  DBI::dbExecute(db,"UPDATE observation SET value_as_concept_id = 900102 WHERE observation_concept_id = 900001 AND value_as_concept_id = 900101")
  DBI::dbDisconnect(db)
  server2<-DSLite::newDSLiteServer(resources=list(fixture=resourcer::newResource(name="fixture",url=paste0("omop+dbi:sqlite://",second_path),format="omop.dbi.db")),config=config)
}
withr::local_options(list(datashield.env = environment()), .local_envir = env)
builder <- DSI::newDSLoginBuilder()
builder$append(server = "site", url = "server", driver = "DSLiteDriver")
if(two_sites) builder$append(server="site2",url="server2",driver="DSLiteDriver")
conns <- DSI::datashield.login(builder$build(), assign = FALSE)
ds.omop.connect(resource = "fixture", datasources = conns, symbol = symbol)


 list(conns=conns, server=server, path=c(fixture_path,second_path))
}
