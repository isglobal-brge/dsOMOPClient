OMOPCDMDatabase <- R6::R6Class(
  "OMOPCDMDatabase",
  public = list(
    connections = NULL,
    resource = NULL,
    resourceSymbol = NULL,

    initialize = function(connections, resource) {
      self$connections = connections
      self$resource = resource
      self$resourceSymbol = paste0("dsOMOP.", self$resource)
    },

    assignResource = function(symbol) {
      datashield.assign.resource(
        self$connections, 
        symbol,
        self$resource)
    },

    get = function(table,
                   symbol = NULL,
                   conceptFilter = NULL, 
                   columnFilter = NULL, 
                   personFilter = NULL, 
                   mergeColumn = NULL,
                   dropNA = FALSE) {

      # If a symbol is not provided, use the table name as the symbol
      if (is.null(symbol)) {
        symbol <- table
      }

      # Assigns a provisional symbol for the connection resource
      self$assignResource(self$resourceSymbol)

      call <- getTableCall(self$resourceSymbol, table, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA)
      
      tryCatch({
        datashield.assign.expr(
          conns = self$connections,
          symbol = symbol,
          expr = call
        )
      }, error = function(e) {
        datashield.rm(self$connections, self$resourceSymbol)
        stop(e)
      })

      # Removes the provisional symbol after the table has been assigned
      datashield.rm(self$connections, self$resourceSymbol)
    },

    tables = function() {
      self$assignResource(self$resourceSymbol)
      tryCatch({
          datashield.aggregate(
          self$connections,
          expr = paste0("getTableCatalogDS(", self$resourceSymbol, ")"),
        )
      }, error = function(e) {
        datashield.rm(self$connections, self$resourceSymbol)
        stop(e)
      })
    },

    columns = function(table) {
      self$assignResource(self$resourceSymbol)
      tryCatch({
          datashield.aggregate(
          self$connections,
          expr = paste0("getColumnCatalogDS(", self$resourceSymbol, ", table = '", table, "')")
        )
      }, error = function(e) {
        datashield.rm(self$connections, self$resourceSymbol)
        stop(e)
      })
    },

    concepts = function(table) {
      self$assignResource(self$resourceSymbol)
      tryCatch({
          datashield.aggregate(
          self$connections,
          expr = paste0("getConceptCatalogDS(", self$resourceSymbol, ", table = '", table, "')")
        )
      }, error = function(e) {
        datashield.rm(self$connections, self$resourceSymbol)
        stop(e)
      })
    }
  )
)

#' @export
ds.omop <- function(connections, resource) {
  return(OMOPCDMDatabase$new(connections, resource))
}
