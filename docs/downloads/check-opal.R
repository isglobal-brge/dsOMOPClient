# Run as the analyst after opal-admin.R. Only approved metadata is requested.
library(dsOMOPClient)
source("tools/book/connection.R")
stopifnot(book_driver == "Opal")
local({
  practice <- book_connect("gibleed")
  conns <- practice$conns
  on.exit({try(ds.omop.disconnect(conns=conns), silent=TRUE)
           DSI::datashield.logout(conns)})
  print(names(conns))
  print(ds.omop.server.version(conns=conns)[c("server", "published", "running", "ready")])
  for (key in names(resource_names)) {
    ds.omop.disconnect(conns=conns)
    ds.omop.connect(resource=resource_names[[key]], conns=conns)
    columns <- ds.omop.columns("measurement", conns=conns)$site
    stopifnot(is.data.frame(columns), nrow(columns)>0L)
    print(columns)
    cat("PASS: ", resource_names[[key]], " connected; measurement schema returned\n", sep="")
  }
})
