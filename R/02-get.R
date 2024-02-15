OMOPCDMDatabase$set("public", "get", function(table,
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
})

getTableCall <- function(resource,
                         table, 
                         conceptFilter = NULL, 
                         columnFilter = NULL, 
                         personFilter = NULL, 
                         mergeColumn = NULL, 
                         dropNA = FALSE) {
                        
  call <- paste0("getOMOPCDMTableDS(", resource, ", table = '", table, "'")

  if (!is.null(conceptFilter)) {
    if (!is.vector(conceptFilter)) {
      stop("conceptFilter must be a vector of concept IDs.")
    }
    call <- paste0(call, ", conceptFilter = c(", paste(conceptFilter, collapse = ", "), ")")
  }

  if (!is.null(columnFilter)) {
    if (!is.vector(columnFilter)) {
      stop("columnFilter must be a vector of column names.")
    }
    call <- paste0(call, ", columnFilter = c('", paste(columnFilter, collapse = "', '"), "')")
  }

  if (!is.null(personFilter)) {
    call <- paste0(call, ", personFilter = ", personFilter)
  }

  if (!is.null(mergeColumn)) {
    call <- paste0(call, ", mergeColumn = '", mergeColumn, "'")
  }

  if (dropNA) {
    call <- paste0(call, ", dropNA = TRUE")
  }

  call <- paste0(call, ")")

  return(call)
}
