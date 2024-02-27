#' @title Get Columns Catalog
#' @name OMOPCDMDatabase-columns
#'
#' @description This function returns a list of available columns for a specified table in an OMOP CDM database.
#'
#' @param table The name of the table for which to retrieve the columns catalog.
#' @param dropNA A boolean indicating whether to skip empty columns.
#'
#' @return A list of column names available in the specified table of the OMOP CDM database.
#'
OMOPCDMDatabase$set("public", "columns", function(table, dropNA = FALSE) {
  self$assignResource(self$resourceSymbol)

  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getColumnCatalogDS(", self$resourceSymbol, ", table = '", table, "', dropNA = ", dropNA, ")"),
    # Removes the connection resource from the environment after the call
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
