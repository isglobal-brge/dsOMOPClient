# Synthetic-only fixture derived from tests/testthat/helper-dates-va.R.
book_fixture <- function(two_sites=FALSE, threshold=5L, levels_max=NULL, categories=FALSE, formulas=FALSE, symbol="omop", env=parent.frame()) {
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
  observation_date = c("2020-01-01", "2022-01-01", "2020-02-01", "2023-02-01", "2024-01-01"),
  observation_type_concept_id = 0L, value_as_number = NA_real_,
  value_as_concept_id = c(900101L, 900102L, 900102L, 900101L, 900301L)))
DBI::dbWriteTable(db, "visit_occurrence", data.frame(visit_occurrence_id = 1:5,
  person_id = c(1L, 1L, 2L, 2L, 3L), visit_concept_id = 0L,
  visit_start_date = c("2023-10-01", "2020-06-15", "2021-03-15", "2020-03-14", "2020-07-20"),
  visit_end_date = c("2023-10-01", "2020-06-15", "2021-03-15", "2020-03-14", "2020-07-20"),
  visit_type_concept_id = 0L))
DBI::dbWriteTable(db, "death", data.frame(person_id = 1:3,
  death_date = c("2024-06-01", "2024-03-14", "2024-05-10"), death_type_concept_id = 0L,
  cause_concept_id = c(900201L, 900202L, 900201L)))
DBI::dbWriteTable(db, "concept", data.frame(
  concept_id = c(900001L, 900002L, 900101L, 900102L, 900201L, 900202L, 900301L),
  concept_name = c("Education question", "Fever question", "Secondary education", "University education", "Myocardial infarction", "Pneumonia", "Yes"),
  domain_id = c("Observation", "Observation", "Meas Value", "Meas Value", "Condition", "Condition", "Meas Value"),
  vocabulary_id = "Fixture", concept_class_id = "Fixture", standard_concept = "S",
  concept_code = as.character(c(900001L, 900002L, 900101L, 900102L, 900201L, 900202L, 900301L)),
  valid_start_date = "1970-01-01", valid_end_date = "2099-12-31", invalid_reason = NA_character_))
DBI::dbWriteTable(db, "cdm_source", data.frame(cdm_source_name = "Synthetic questionnaire fixture",
  cdm_source_abbreviation = "fixture", cdm_holder = "test", source_description = "Synthetic only",
  source_documentation_reference = "", cdm_etl_reference = "", source_release_date = "2025-01-01",
  cdm_release_date = "2025-01-01", cdm_version = "5.4", cdm_version_concept_id = 0L,
  vocabulary_version = "fixture"))
# Repeat four patterns six times: enough persons for the ordinary subset and
# factor-level density gates, without weakening either gate for rare categories.
for (tab in c("person", "observation_period", "observation", "visit_occurrence", "death")) {
  base <- DBI::dbReadTable(db, tab)
  key <- switch(tab, person = "person_id", observation_period = "observation_period_id",
    observation = "observation_id", visit_occurrence = "visit_occurrence_id", death = "person_id")
  copies <- lapply(0:5, function(i) {
    d <- base
    if (key != "person_id") d[[key]] <- d[[key]] + i * nrow(base)
    d$person_id <- d$person_id + i * 4L
    if (tab == "person") d$gender_concept_id <- if (i %% 2L == 0L) 8532L else 8507L
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
DBI::dbWriteTable(db, "observation", data.frame(observation_id=31:36,
  person_id=seq(2L,22L,4L), observation_concept_id=900002L,
  observation_date="2024-01-01", observation_type_concept_id=0L,
  value_as_number=NA_real_, value_as_concept_id=900302L), append=TRUE)
extra <- DBI::dbReadTable(db,"concept")[rep(7,4),]
extra$concept_id <- c(900302L,9201L,9202L,900100L)
extra$concept_name <- c("No","Inpatient Visit","Outpatient Visit","Education answer")
extra$concept_code <- as.character(extra$concept_id)
DBI::dbWriteTable(db,"concept",extra,append=TRUE)
DBI::dbWriteTable(db,"concept_ancestor",data.frame(
  ancestor_concept_id=c(900100L,900100L),descendant_concept_id=c(900101L,900102L),
  min_levels_of_separation=1L,max_levels_of_separation=1L))
DBI::dbWriteTable(db,"vocabulary",data.frame(vocabulary_id="Fixture",vocabulary_version="2025-01-01"))
# Book 2 adds independent synthetic numeric measurements, prescriptions and a
# pre-existing results cohort. The original book-1 records are unchanged.
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
  rows <- lapply(0:5, function(i) {
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
extra$concept_name <- c("Practice medicine","Practice measurement","kilogram")
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
    vocabulary_id=c("Fixture","Language","ATC","ATC",rep("ICD10",4)),
    concept_class_id=c("Fixture","Language","ATC 1st","ATC 2nd",
      "ICD10 Hierarchy","ICD10 Hierarchy","ICD10 Chapter","ICD10 Chapter"),
    standard_concept=c("S","S","C","C",NA,NA,"C","C"),
    concept_code=c("primary","en","T","T01","T-C","T-R","T-IX","T-X"),
    valid_start_date="1970-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_)
  DBI::dbWriteTable(db,"concept",concepts,append=TRUE)
  DBI::dbWriteTable(db,"concept_synonym",data.frame(concept_id=c(900101L,900102L),
    concept_synonym_name=c("Secondary schooling","Tertiary schooling"),language_concept_id=4180186L))
  DBI::dbWriteTable(db,"concept_ancestor",data.frame(
    ancestor_concept_id=c(960001L,960002L,960001L),descendant_concept_id=c(950001L,950001L,960002L),
    min_levels_of_separation=c(2L,1L,1L),max_levels_of_separation=c(2L,1L,1L)),overwrite=TRUE)
  DBI::dbWriteTable(db,"concept_relationship",data.frame(
    concept_id_1=c(960010L,960011L,960010L,960011L),
    concept_id_2=c(900201L,900202L,960020L,960021L),
    relationship_id=c("Maps to","Maps to","Is a","Is a"),
    valid_start_date="1970-01-01",valid_end_date="2099-12-31",invalid_reason=NA_character_),overwrite=TRUE)
}
# Optional explicitly synthetic formula practice, separate from Synthea.
if (formulas) {
  new <- data.frame(concept_id=c(970001L,8840L),
    concept_name=c("Teaching standardized serum creatinine","milligram per deciliter"),
    domain_id=c("Measurement","Unit"),
    vocabulary_id="Fixture",concept_class_id="Fixture",standard_concept="S",
    concept_code=c("scr","mg/dL"),valid_start_date="1970-01-01",
    valid_end_date="2099-12-31",invalid_reason=NA_character_)
  DBI::dbWriteTable(db,"concept",new,append=TRUE)
  DBI::dbWriteTable(db,"measurement",data.frame(measurement_id=1001:1024,person_id=1:24,
    measurement_concept_id=970001L,measurement_date="2024-06-01",measurement_type_concept_id=0L,
    value_as_number=rep(c(.7,.9,1.4,1.8),6),unit_concept_id=8840L),append=TRUE)
}
DBI::dbDisconnect(db)

config <- DSLite::defaultDSConfiguration(include = c("dsOMOP","dsBase"))
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
  DBI::dbDisconnect(db)
  server2<-DSLite::newDSLiteServer(resources=list(fixture=resourcer::newResource(name="fixture",url=paste0("omop+dbi:sqlite://",second_path),format="omop.dbi.db")),config=config)
}
withr::local_options(list(datashield.env = environment()), .local_envir = env)
builder <- DSI::newDSLoginBuilder()
builder$append(server = "site", url = "server", driver = "DSLiteDriver")
if(two_sites) builder$append(server="site2",url="server2",driver="DSLiteDriver")
conns <- DSI::datashield.login(builder$build(), assign = FALSE)
ds.omop.connect(resource = "fixture", conns = conns, symbol = symbol)


 list(conns=conns, server=server, path=c(fixture_path,second_path))
}
