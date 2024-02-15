OMOPCDMDatabase$set("public", "concepts", function(table) {
  self$assignResource(self$resourceSymbol)
  datashield.aggregate(
    self$connections,
    expr = paste0("getConceptCatalogDS(", self$resourceSymbol, ", table = '", table, "')")
  )
})