# 2\. Resources, connections and symbols

**What you will learn**

Distinguish a connection, a resource and a symbol, and change databases
deliberately.

**You need:** the packages, helpers and loaded practice data from
[Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#before-you-start).
Start a fresh R session in `book-work` and run the sections in order.

## Start this chapter

``` r
library(dsOMOPClient)
source("tools/book/display.R")
source("tools/book/connection.R")
practice <- book_connect("gibleed")
conns <- practice$conns
```

## Three names with different jobs

A **connection** is a logged-in DataSHIELD session. A **resource**
describes how the server opens a database. A **symbol** is an object
name in that session. Here `site` names the connection, `gibleed`
selects the resource and `omop` names the attached database handle.

``` r
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "omop"

Under `site`, `omop` is the private handle created by `book_connect()`.
Later, `D_people` will name a table created by a recipe, rather than a
database handle.

For an Opal study server, use the [login block in Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#connect-to-your-opal-server).
Your administrator provides resource names including their project
prefix.

## Use a handle explicitly

``` r
knitr::kable(as.data.frame(ds.omop.table.stats("person", symbol = "omop", conns = conns)$per_site$site))
```

| rows | persons |
| ---: | ------: |
| 2690 |    2690 |

The request goes to GiBleed through `omop`: its person count is in the
2,690–2,694 band. Most dsOMOP calls default to `symbol = "omop"`, so you
can omit it when you have just one attached database.
`ds.omop.server.version()` is different: it reports the server’s
installed and available methods, independently of any database handle.

## Switch resources

The practice helper closes the current session and opens the requested
resource. Always replace `conns` with the returned connection:

``` r
practice <- book_switch("numeric")
conns <- practice$conns
knitr::kable(as.data.frame(ds.omop.table.stats("person", conns = conns)$per_site$site))
```

| rows | persons |
| ---: | ------: |
|   25 |      25 |

The person band is now 25–29, containing Synthea27Nj’s 28 people. The
handle can still be called `omop` even though its database changed.
Switching does not combine the two populations. On Opal, use the
disconnect/attach pair in [Getting
started](https://isglobal-brge.github.io/dsOMOPClient/articles/book-00-start.html#connect-to-your-opal-server)
instead.

``` r
practice <- book_switch("gibleed")
conns <- practice$conns
knitr::kable(as.data.frame(ds.omop.table.stats("person", conns = conns)$per_site$site))
```

| rows | persons |
| ---: | ------: |
| 2690 |    2690 |

The 2,690–2,694 band confirms that GiBleed is attached again. Choosing
the resource explicitly prevents running a lab analysis against its
empty numeric fields.

## Exercise 1: keep two handles in one session

Attach the numeric resource as `labs`, keeping `omop` attached to
GiBleed. List the names and count people through each. Does attaching
`labs` combine them? For Opal, substitute the supplied numeric resource
name.

Solution

``` r
labs_session <- ds.omop.connect(resource = "numeric", symbol = "labs", conns = conns)
DSI::datashield.symbols(conns)
```

    #> $site
    #> [1] "labs" "omop"

`labs` and `omop` are two distinct handles in the same session.

``` r
handle_counts <- lapply(c("omop", "labs"), function(handle) {
  counts <- ds.omop.table.stats("person", stats = "persons",
    symbol = handle, conns = conns)$per_site$site
  data.frame(Handle = handle, counts)
})
knitr::kable(do.call(rbind, handle_counts), row.names = FALSE)
```

| Handle | persons |
| :----- | ------: |
| omop   |    2690 |
| labs   |      25 |

``` r
ds.omop.disconnect(symbol = "labs", conns = conns)
```

`omop` gives 2690 and `labs` gives 25. Each is a separate protected
count; the populations were not combined. Disconnecting `labs` leaves
`omop` available.

## Exercise 2: diagnose a wrong name

Ask for a person count through `missing_handle`. What needs fixing: the
table name or the handle name?

Solution

``` r
tryCatch(ds.omop.table.stats("person", symbol = "missing_handle", conns = conns),
  error = function(e) cat(conditionMessage(e), "\n"))
```

    #> No OMOP session 'missing_handle'. Call ds.omop.connect() first.

The message identifies a missing session handle. It says nothing about
whether the database has a person table. Use `omop`, or attach a
resource under the name you intend to query.

## End the session

``` r
ds.omop.disconnect(conns = conns)
DSI::datashield.logout(conns)
```

Disconnect releases the database handle; logout ends the DataSHIELD
session. The next chapter opens a fresh connection.

## Recap

A resource selects a database, a connection carries requests, and a
symbol names a private server object. Check the selected resource before
analysing it.

**Next:** [3. Exploring the
CDM](https://isglobal-brge.github.io/dsOMOPClient/articles/book-03-explore.md).
