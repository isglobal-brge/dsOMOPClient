#' @title Get Tables Catalog
#' @name OMOPCDMDatabase-tables
#'
#' @description This function returns a list of available tables in an OMOP CDM database.
#'
#' @return A list of table names available in the OMOP CDM database.
#'
OMOPCDMDatabase$set("public", "tables", function() {
  self$assignResource(self$resourceSymbol)
  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getTableCatalogDS(", self$resourceSymbol, ")"),
    # Removes the connection resource from the environment after the call
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
