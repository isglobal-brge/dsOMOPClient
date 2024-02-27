#' @title Get Concepts Catalog
#' @name OMOPCDMDatabase-concepts
#'
#' @description This function returns a data frame of available concept IDs and their names for a specified table in an OMOP CDM database.
#'
#' @param table The name of the table for which to retrieve the concepts catalog.
#'
#' @return A data frame containing concept_id and concept_name columns representing available concepts in the specified
#' table of the OMOP CDM database.
#'
OMOPCDMDatabase$set("public", "concepts", function(table) {
  self$assignResource(self$resourceSymbol)

  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getConceptCatalogDS(", self$resourceSymbol, ", table = '", table, "')"),
    # Removes the connection resource from the environment after the call
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
