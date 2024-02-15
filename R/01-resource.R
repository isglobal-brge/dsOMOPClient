OMOPCDMDatabase$set("public", "assignResource", function(symbol) {
  datashield.assign.resource(
    self$connections, 
    symbol,
    self$resource
  )
})

generateResourceSymbol <- function(resource) {
  randomString <- paste0(sample(c(0:9, letters, LETTERS), 8, replace = TRUE), collapse = "")
  resourceSymbol <- paste("dsOMOP", resource, randomString, sep = ".")
  return(resourceSymbol)
}
