# Targeted checks for changed reference examples and the saved study record.
library(dsOMOPClient)
source("tools/book/fixture.R")
practice <- book_fixture()
conns <- practice$conns
local({
  on.exit({ds.omop.disconnect(conns=conns); DSI::datashield.logout(conns); unlink(practice$path)})
  lines <- readLines("vignettes/v03_cohorts.Rmd")
  version_line <- grep('^ds[.]omop[.]server[.]version', lines, value=TRUE)
  version <- eval(parse(text=version_line))
  stopifnot(identical(names(version), c("server", "published", "running", "ready")), all(version$ready))
  lines <- readLines("vignettes/v07_querylibrary_sticky.Rmd")
  start <- match('```{r execute, eval=FALSE}', lines)
  end <- start + which(lines[(start+1):length(lines)] == '```')[1]
  eval(parse(text=lines[(start+1):(end-1)]))
  stopifnot(inherits(recipe, "omop_recipe"), !is.null(result),
    identical(recipe_explain(recipe)$selected, "v2"))
  message("PASS: changed reference version projection and current recipe protected release")
})
