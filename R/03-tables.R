OMOPCDMDatabase$set("public", "tables", function() {
  self$assignResource(self$resourceSymbol)
  datashield.aggregate(
    self$connections,
    expr = paste0("getTableCatalogDS(", self$resourceSymbol, ")")
  )
})