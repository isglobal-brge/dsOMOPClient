# 2\. Resources, connections and symbols

**What you will learn**

Attach a resource and learn the two kinds of name used in a session.

This chapter uses the public synthetic GiBleed practice connection from
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
**You need:** the packages and the matching practice connection
described in [Before you
start](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Run the sections in order in one R session. All printed results below
were computed with dsOMOP and dsOMOPClient 2.10.4. They are teaching
data, not clinical findings.

## One connection list

A *connection* is a logged-in session. Our DSLite helper made one, named
`site`.

``` r
names(conns)
```

    #> [1] "site"

Most calls accept `conns`. Passing it explicitly makes clear where a
request goes.

## The Opal login path

**Requires a real Opal.** This is the single connection configuration to
change when moving to another practice host. Use it instead of the
DSLite login from chapter 0. Set the three `BOOK_OPAL_*` environment
variables in your secure local configuration; the URL comes from your
administrator or easy-opal setup. This deployment block is not part of
the recorded DSLite execution.

``` r
opal_url <- Sys.getenv("BOOK_OPAL_URL")
opal_project <- "practice"
opal_resources <- c(gibleed = "gibleed", numeric = "numeric")
lesson_resource <- "gibleed"  # choose "numeric" for numeric chapters
library(DSOpal)
builder <- DSI::newDSLoginBuilder()
builder$append(server = "site", url = opal_url,
               user = Sys.getenv("BOOK_OPAL_USER"),
               password = Sys.getenv("BOOK_OPAL_PASSWORD"), driver = "OpalDriver")
conns <- DSI::datashield.login(builder$build(), assign = FALSE)
ds.omop.connect(resource = paste(opal_project, opal_resources[[lesson_resource]],
                                 sep = "."), conns = conns)
```

Reuse this login for every practice lesson, choosing its named resource.
The questionnaire-fixture chapters explicitly use their separate local
helper. Credentials belong in secure configuration, never a shared
notebook. The executed DSLite login is in `tools/book/practice.R`.

## A resource is a route to a database

A *resource* stores the connection details on the server. On the
practice server its name is `gibleed`. In the optional Opal setup it is
`practice.gibleed`.

A *symbol* is an R object name inside the DataSHIELD session. Attach the
resource and deliberately name the session handle `omop`:

``` r
ds.omop.disconnect(conns = conns)
ds.omop.connect(resource = "gibleed", symbol = "omop", conns = conns)
```

The server opens its database connection. This does not copy the
database into R on your laptop. On Opal, replace only `gibleed` with the
resource name you were given.

## Reuse the handle

``` r
ds.omop.columns("person", symbol = "omop", conns = conns)$site
```

    #>                    column_name cdm_datatype db_datatype      concept_role
    #> 1                    person_id      integer        real       non_concept
    #> 2            gender_concept_id      integer        real attribute_concept
    #> 3                year_of_birth      integer        real       non_concept
    #> 4               month_of_birth      integer        real       non_concept
    #> 5                 day_of_birth      integer        real       non_concept
    #> 6               birth_datetime     datetime        text       non_concept
    #> 7              race_concept_id      integer        real attribute_concept
    #> 8         ethnicity_concept_id      integer        real attribute_concept
    #> 9                  location_id      integer        real       non_concept
    #> 10                 provider_id      integer        real       non_concept
    #> 11                care_site_id      integer        real       non_concept
    #> 12         person_source_value  varchar(50)        text       non_concept
    #> 13         gender_source_value  varchar(50)        text       non_concept
    #> 14    gender_source_concept_id      integer        real    source_concept
    #> 15           race_source_value  varchar(50)        text       non_concept
    #> 16      race_source_concept_id      integer        real    source_concept
    #> 17      ethnicity_source_value  varchar(50)        text       non_concept
    #> 18 ethnicity_source_concept_id      integer        real    source_concept
    #>    semantic_class fk_domain is_date is_extension is_untyped_identifier is_sensitive
    #> 1        ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 2         concept    Gender   FALSE        FALSE                 FALSE        FALSE
    #> 3        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 4        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 5        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 6        ordinary              TRUE        FALSE                 FALSE         TRUE
    #> 7         concept      Race   FALSE        FALSE                 FALSE        FALSE
    #> 8         concept Ethnicity   FALSE        FALSE                 FALSE        FALSE
    #> 9        ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 10       ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 11       ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 12       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 13       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 14        concept             FALSE        FALSE                 FALSE         TRUE
    #> 15       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 16        concept             FALSE        FALSE                 FALSE         TRUE
    #> 17       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 18        concept             FALSE        FALSE                 FALSE         TRUE
    #>    is_blocked is_quasi_identifier
    #> 1       FALSE               FALSE
    #> 2       FALSE               FALSE
    #> 3        TRUE                TRUE
    #> 4        TRUE                TRUE
    #> 5        TRUE                TRUE
    #> 6        TRUE                TRUE
    #> 7       FALSE               FALSE
    #> 8       FALSE               FALSE
    #> 9       FALSE               FALSE
    #> 10      FALSE               FALSE
    #> 11      FALSE               FALSE
    #> 12       TRUE               FALSE
    #> 13       TRUE               FALSE
    #> 14       TRUE               FALSE
    #> 15       TRUE               FALSE
    #> 16       TRUE               FALSE
    #> 17       TRUE               FALSE
    #> 18       TRUE               FALSE

Subsequent calls can omit `symbol` because its default is `omop`. An
output symbol such as `D_people` is different: it names a table built
later by a recipe.

## End a session deliberately

At the end of your own work, call `ds.omop.disconnect(conns = conns)`
and then `DSI::datashield.logout(conns)`. Start again with the practice
helper for another chapter. The reproduction script performs both
cleanup calls after each chapter.

## Switch between the two practice resources

The same practice server also offers the numeric Synthea27Nj database.
Disconnect the current OMOP handle, then attach that resource on the
same login connection:

``` r
ds.omop.disconnect(conns = conns)
numeric_session <- ds.omop.connect(resource = "numeric", conns = conns)
class(numeric_session)
```

    #> [1] "omop_session"

This changes the database behind the OMOP handle. It does not copy or
combine patient rows between resources. On Opal the corresponding name
is `practice.numeric`. Return to GiBleed for the next introductory
chapters:

``` r
ds.omop.disconnect(conns = conns)
session <- ds.omop.connect(resource = "gibleed", conns = conns)
class(session)
```

    #> [1] "omop_session"

The numeric chapters will open their own fresh `numeric` session.
Keeping resource selection explicit prevents accidentally running a lab
recipe against GiBleed’s empty numeric-value fields.

## Recap

The resource identifies a database; `omop` is its session handle;
`D_people` will name a private result.

## Try it

Repeat the column request without specifying the symbol.

Solution

``` r
ds.omop.columns("person", conns = conns)$site
```

    #>                    column_name cdm_datatype db_datatype      concept_role
    #> 1                    person_id      integer        real       non_concept
    #> 2            gender_concept_id      integer        real attribute_concept
    #> 3                year_of_birth      integer        real       non_concept
    #> 4               month_of_birth      integer        real       non_concept
    #> 5                 day_of_birth      integer        real       non_concept
    #> 6               birth_datetime     datetime        text       non_concept
    #> 7              race_concept_id      integer        real attribute_concept
    #> 8         ethnicity_concept_id      integer        real attribute_concept
    #> 9                  location_id      integer        real       non_concept
    #> 10                 provider_id      integer        real       non_concept
    #> 11                care_site_id      integer        real       non_concept
    #> 12         person_source_value  varchar(50)        text       non_concept
    #> 13         gender_source_value  varchar(50)        text       non_concept
    #> 14    gender_source_concept_id      integer        real    source_concept
    #> 15           race_source_value  varchar(50)        text       non_concept
    #> 16      race_source_concept_id      integer        real    source_concept
    #> 17      ethnicity_source_value  varchar(50)        text       non_concept
    #> 18 ethnicity_source_concept_id      integer        real    source_concept
    #>    semantic_class fk_domain is_date is_extension is_untyped_identifier is_sensitive
    #> 1        ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 2         concept    Gender   FALSE        FALSE                 FALSE        FALSE
    #> 3        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 4        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 5        ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 6        ordinary              TRUE        FALSE                 FALSE         TRUE
    #> 7         concept      Race   FALSE        FALSE                 FALSE        FALSE
    #> 8         concept Ethnicity   FALSE        FALSE                 FALSE        FALSE
    #> 9        ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 10       ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 11       ordinary             FALSE        FALSE                 FALSE        FALSE
    #> 12       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 13       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 14        concept             FALSE        FALSE                 FALSE         TRUE
    #> 15       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 16        concept             FALSE        FALSE                 FALSE         TRUE
    #> 17       ordinary             FALSE        FALSE                 FALSE         TRUE
    #> 18        concept             FALSE        FALSE                 FALSE         TRUE
    #>    is_blocked is_quasi_identifier
    #> 1       FALSE               FALSE
    #> 2       FALSE               FALSE
    #> 3        TRUE                TRUE
    #> 4        TRUE                TRUE
    #> 5        TRUE                TRUE
    #> 6        TRUE                TRUE
    #> 7       FALSE               FALSE
    #> 8       FALSE               FALSE
    #> 9       FALSE               FALSE
    #> 10      FALSE               FALSE
    #> 11      FALSE               FALSE
    #> 12       TRUE               FALSE
    #> 13       TRUE               FALSE
    #> 14       TRUE               FALSE
    #> 15       TRUE               FALSE
    #> 16       TRUE               FALSE
    #> 17       TRUE               FALSE
    #> 18       TRUE               FALSE
