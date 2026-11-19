# 2\. Resources, connections and symbols

**What you will learn**

Distinguish a connection, a resource and a symbol, and change databases
deliberately.

**You need:** the client packages and the Opal login from [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.md).
This chapter uses `omop.gibleed` and `omop.synthea`.

## Start this chapter

``` r
library(dsOMOPClient)
conns <- ds.omop.login("https://localhost:8443", user = "administrator",
  password = "password", resource = "omop.gibleed", server = "site")$conns
```

## Three names with different jobs

A **connection** is a logged-in DataSHIELD session. A **resource**
describes how the server opens a database. A **symbol** is an object
name in that session. Here `site` names the connection, `omop.gibleed`
selects the resource and `omop` names the attached database handle.

``` r
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "omop"

Under `site`, `omop` is the private handle that `ds.omop.login()`
created. Later, `D_people` will name a table created by a recipe, rather
than a database handle.

## Use a handle explicitly

``` r
ds.omop.table.stats("person", symbol = "omop", datasources = conns)
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

The request goes to GiBleed through `omop`: its person count is in the
2,690–2,694 band. Most dsOMOP calls default to `symbol = "omop"`, so you
can omit it when you have just one attached database.
`ds.omop.server.version()` is different: it reports the server’s
installed and available methods, independently of any database handle.

## Switch resources

Release the attached resource, then attach another one through the same
login:

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

The person band is now 25–29, containing Synthea27Nj’s 28 people. The
handle is still called `omop` even though its database changed.
Switching does not combine the two populations.

``` r
ds.omop.disconnect(datasources = conns)
ds.omop.connect(resource = "omop.gibleed", datasources = conns)
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

The 2,690–2,694 band confirms that GiBleed is attached again. Choosing
the resource explicitly prevents running a lab analysis against its
empty numeric fields.

## Exercise 1: keep two handles in one session

Attach `omop.synthea` as `labs`, keeping `omop` attached to GiBleed.
List the names and count people through each. Does attaching `labs`
combine them?

Solution

``` r
ds.omop.connect(resource = "omop.synthea", symbol = "labs", datasources = conns)
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "labs" "omop"

`labs` and `omop` are two distinct handles in the same session.

``` r
ds.omop.table.stats("person", stats = "persons", symbol = "omop", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $persons
    #> [1] 2690
    #>
    #>
    #> $pooled: the same as $site (one server)

``` r
ds.omop.table.stats("person", stats = "persons", symbol = "labs", datasources = conns)
```

    #> <dsomop_result>
    #> Servers: site
    #> Type: both (per-server and pooled results)
    #>
    #> $site
    #> $persons
    #> [1] 25
    #>
    #>
    #> $pooled: the same as $site (one server)

``` r
ds.omop.disconnect(symbol = "labs", datasources = conns)
```

`omop` gives 2690 and `labs` gives 25. Each is a separate protected
count; the populations were not combined. Disconnecting `labs` leaves
`omop` available.

## Exercise 2: diagnose a wrong name

Ask for a person count through `missing_handle`. What needs fixing: the
table name or the handle name?

Solution

``` r
ds.omop.table.stats("person", symbol = "missing_handle", datasources = conns)
```

    #> Error: No OMOP session 'missing_handle'. Call ds.omop.connect() first.

The message identifies a missing session handle. It says nothing about
whether the database has a person table. Use `omop`, or attach a
resource under the name you intend to query.

## End the session

``` r
ds.omop.disconnect(datasources = conns)
DSI::datashield.logout(conns)
```

Disconnect releases the database handle; logout ends the DataSHIELD
session. The next chapter logs in again.

## Recap

A resource selects a database, a connection carries requests, and a
symbol names a private server object. Check the selected resource before
analysing it.

**Next:** [3. Exploring the
CDM](https://isglobal-brge.github.io/dsOMOPClient/articles/book-03-explore.md).
