OMOPCDMDatabase$set("public", "columns", function(table, dropNA = TRUE) {
  self$assignResource(self$resourceSymbol)
  datashield.aggregate(
    self$connections,
    expr = paste0("getColumnCatalogDS(", self$resourceSymbol, ", table = '", table, "', dropNA = ", dropNA, ")")
  )
})