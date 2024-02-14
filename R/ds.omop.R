OMOPCDMDatabase <- R6::R6Class(
  "OMOPCDMDatabase",
  public = list(
    connections = NULL,
    resource = NULL,
    resourceSymbol = NULL,

    initialize = function(connections, resource) {
      self$connections = connections
      self$resource = resource
      self$resourceSymbol = "dsOMOP"
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

      self$assignResource(self$resourceSymbol)
      call <- getTableCall(self$resourceSymbol, table, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA)
      datashield.assign.expr(self$connections, symbol, call)
    },

    tables = function() {
      self$assignResource(self$resourceSymbol)
      datashield.aggregate(
        self$connections,
        expr = paste0("getTableCatalogDS(", self$resourceSymbol, ")")
      )
    },

    columns = function(table) {
      self$assignResource(self$resourceSymbol)
      datashield.aggregate(
        self$connections,
        expr = paste0("getColumnCatalogDS(", self$resourceSymbol, ", table = '", table, "')")
      )
    },

    concepts = function(table) {
      self$assignResource(self$resourceSymbol)
      datashield.aggregate(
        self$connections,
        expr = paste0("getConceptCatalogDS(", self$resourceSymbol, ", table = '", table, "')")
      )
    }
  )
)

#' @export
ds.omop <- function(connections, resource) {
  return(OMOPCDMDatabase$new(connections, resource))
}
