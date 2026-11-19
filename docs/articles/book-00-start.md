# 0\. Getting started

**What you will learn**

Install the client packages, connect to the Opal server and choose a
resource.

## Before you start

The book uses an Opal server at `https://localhost:8443`. Its
administrator has already loaded the data as OMOP CDM resources and
installed dsOMOP and dsBase on its R server. You work only on the client
side: you send requests and receive aggregate results, never patient
rows. The data are synthetic, so the results teach methods; they are not
clinical findings.

You need R 4.1 or newer. Install the client packages once:

``` r
install.packages(c("remotes", "DSI", "DSOpal"))
remotes::install_github("datashield/dsBaseClient@6.3.5", upgrade = "never")
remotes::install_github("isglobal-brge/dsOMOPClient@2.10.5", upgrade = "never")
```

## Connect to the Opal server

`ds.omop.login()` logs in to the server and attaches one OMOP CDM
resource:

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

Use the user name and password your administrator gave you, and keep
them out of shared scripts; with a personal access token, pass `token =
"<token>"` instead. `omop.gibleed` is the resource: project `omop`,
resource `gibleed`. `site` is the name this book gives the connection.
`conns` holds the connection, not patient rows; every later call sends
its request through it.

## Check the client and the server

``` r
ds.omop.client.version()
```

    #> [1] "2.10.9"

``` r
ds.omop.server.version(datasources = conns)[c("server", "published", "running", "ready")]
```

| server | published | running | ready |
| :----- | :-------- | :------ | :---- |
| site   | 2.10.9    | 2.10.9  | TRUE  |

The first line is the client package on your computer. `ready` confirms
that the server’s installed dsOMOP and its available methods agree.

## The resources on the server

Each chapter names the resource it uses. All of them are in the project
`omop`:

| Resource                                   | Contents                                                                                     |
| :----------------------------------------- | :------------------------------------------------------------------------------------------- |
| `omop.gibleed`                             | GiBleed: 2,694 synthetic people with visits, conditions and medicines; no numeric lab values |
| `omop.synthea`                             | Synthea27Nj: 28 synthetic people with numeric lab values and units                           |
| `omop.questionnaire`                       | A synthetic questionnaire study of 48 people, described below                                |
| `omop.questionnaire_vocab`                 | The questionnaire with a richer teaching vocabulary: codes, synonyms and hierarchies         |
| `omop.questionnaire_drugs`                 | The questionnaire vocabulary plus medicine supply histories                                  |
| `omop.questionnaire_formulas`              | The questionnaire plus serum creatinine and repeated same-day weights                        |
| `omop.questionnaire_site2`                 | A second site’s questionnaire, with different answers and wording                            |
| `omop.synthea_north`, `omop.synthea_south` | Synthea27Nj split into two sites with different people                                       |
| `omop.gibleed_north`, `omop.gibleed_south` | GiBleed split into two sites with different people                                           |

GiBleed and Synthea27Nj are the public OHDSI Eunomia datasets. Analyst
results report counts in five-person bands, so you will see 2,690 rather
than 2,694.

The questionnaire study has four groups of twelve people. Two groups
answer the education question twice, swapping Secondary and University
between their first and last answers; they also answer a yes/no fever
question. One group has only a birth year; one has neither visits nor a
death record. Knowing this design lets you predict the results of later
chapters:

| Item                        | Total | Female | Male |
| :-------------------------- | ----: | -----: | ---: |
| People                      |    48 |     20 |   28 |
| Education respondents       |    24 |     10 |   14 |
| Education answer rows       |    48 |     20 |   28 |
| Fever answer rows           |    24 |     10 |   14 |
| Inpatient visits            |    24 |     10 |   14 |
| Outpatient visits           |    36 |     14 |   22 |
| 2023: myocardial infarction |    12 |      4 |    8 |
| 2023: pneumonia             |     6 |      3 |    3 |
| 2024: myocardial infarction |    12 |      4 |    8 |
| 2024: pneumonia             |     6 |      3 |    3 |
| Survivors                   |    12 |      6 |    6 |

There are 24 education respondents but 48 education answer rows: an
answer row is not a person.

## Exercise 1: choose the lab resource

Attach `omop.synthea` instead of GiBleed and request its person count.
How can you tell that you are no longer querying GiBleed?

Solution

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.synthea", datasources = conns)
ds.omop.table.stats("person", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $rows
    #> [1] 25
    #>
    #> $persons
    #> [1] 25
    #>
    #>
    #> $pooled: the same as $site (one server)

The person count is 25, meaning 25–29 people. That band contains
Synthea27Nj’s 28 people and is far below GiBleed’s 2,694. Chapter 1
explains these bands. `ds.omop.disconnect()` releases the attached
resource; the login stays open.

## Exercise 2: log in again

End the session, then log in again with GiBleed. Confirm the database
with a person count, rather than checking only that a connection has a
name.

Solution

``` r
ds.omop.disconnect(datasources = conns)
DSI::datashield.logout(conns)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

``` r
ds.omop.table.stats("person", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $rows
    #> [1] 2690
    #>
    #> $persons
    #> [1] 2690
    #>
    #>
    #> $pooled: the same as $site (one server)

The count is back to the 2,690–2,694 band. Disconnect releases the
resource; logout closes the DataSHIELD session. A new login starts a new
session.

## Recap

The client packages are installed, `ds.omop.login()` opens a resource on
the server, and `conns` carries your requests.

**Next:** [1. What DataSHIELD and OMOP
mean](https://isglobal-brge.github.io/dsOMOPClient/articles/book-01-mental-model.md).
