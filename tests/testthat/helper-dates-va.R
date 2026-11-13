dates_va_fixture <- function(two_sites=FALSE, threshold=5L) {
library(dsOMOP)
library(dsOMOPClient)
library(DSLite)
library(DBI)
options(nfilter.subset = 5, default.nfilter.subset = 5,
        dsomop.pseudonym_provider = "injected",
        dsomop.pseudonym_root = as.raw(openssl::sha256(charToRaw("synthetic-colleague-fixture"))))
fixture_path <- tempfile(fileext = ".sqlite")
db <- DBI::dbConnect(RSQLite::SQLite(), fixture_path)
DBI::dbWriteTable(db, "person", data.frame(person_id = 1:4,
  gender_concept_id = 0L, year_of_birth = c(1980L, 1990L, 2000L, 1975L),
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
DBI::dbWriteTable(db, "cdm_source", data.frame(cdm_source_name = "Synthetic colleague fixture",
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
    d
  })
  DBI::dbWriteTable(db, tab, do.call(rbind, copies), overwrite = TRUE)
}
DBI::dbDisconnect(db)

config <- DSLite::defaultDSConfiguration(include = c("dsOMOP","dsBase"))
config$Options$nfilter.subset <- threshold
config$Options$default.nfilter.subset <- threshold
server <- DSLite::newDSLiteServer(
  resources = list(fixture = resourcer::newResource(name = "fixture",
    url = paste0("omop+dbi:sqlite://", fixture_path), format = "omop.dbi.db")),
  config = config)
second_path<-NULL
if(two_sites) {
  second_path<-tempfile(fileext=".sqlite");file.copy(fixture_path,second_path)
  db<-DBI::dbConnect(RSQLite::SQLite(),second_path)
  DBI::dbExecute(db,"UPDATE visit_occurrence SET visit_start_date = '2020-06-15', visit_end_date = '2020-06-15' WHERE person_id % 4 = 2")
  DBI::dbDisconnect(db)
  server2<-DSLite::newDSLiteServer(resources=list(fixture=resourcer::newResource(name="fixture",url=paste0("omop+dbi:sqlite://",second_path),format="omop.dbi.db")),config=config)
}
options(datashield.env = environment())
builder <- DSI::newDSLoginBuilder()
builder$append(server = "site", url = "server", driver = "DSLiteDriver")
if(two_sites) builder$append(server="site2",url="server2",driver="DSLiteDriver")
conns <- DSI::datashield.login(builder$build(), assign = FALSE)
ds.omop.connect(resource = "fixture", conns = conns)


 list(conns=conns, server=server, path=c(fixture_path,second_path))
}
