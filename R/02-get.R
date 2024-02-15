#' @title Get Table from OMOP CDM Database
#' @name OMOPCDMDatabase-get 
#'
#' @description This function assigns a specified table by name (string) from an OMOP CDM database to the DataSHIELD environment.
#' The user can specify the symbol (string) for the table assignment in the DataSHIELD environment,
#' a vector of concept IDs (numeric) to filter if dealing with a table categorized by concept IDs,
#' a vector of column names (strings) of the table to filter (i.e., select),
#' the name of a symbol (string) in the environment of a table from which to obtain its person IDs 
#' (from the unique values of their person_id),
#' and the name of the column (string) by which to perform merging operations with other database tables.
#' If not specified, the "person_id" column is assumed to be the merging column.
#' The user is also provided with the option to drop empty columns.
#'
#' @param table A string specifying the name of the table to be assigned.
#' @param symbol A string specifying the symbol for the table assignment in the DataSHIELD environment.
#' @param conceptFilter A numeric vector specifying concept IDs to filter the table by.
#' @param columnFilter A string vector specifying column names to filter (select) in the table.
#' @param personFilter A string specifying the symbol in the environment of a table from which to obtain person IDs.
#' @param mergeColumn A string specifying the column name for merging operations with other tables. 
#'                    Defaults to "person_id" if not specified.
#' @param dropNA A boolean indicating whether to drop empty columns. Defaults to FALSE.
#'
OMOPCDMDatabase$set("public", "get", function(table,
                                              symbol = NULL,
                                              conceptFilter = NULL, 
                                              columnFilter = NULL, 
                                              personFilter = NULL, 
                                              mergeColumn = NULL,
                                              dropNA = FALSE) {
  # If a symbol is not provided, uses the table name as the symbol
  if (is.null(symbol)) {
    symbol <- table
  }
  
  self$assignResource(self$resourceSymbol)
  call <- getTableCall(self$resourceSymbol, table, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA)
  DSI::datashield.assign.expr(
    self$connections, 
    symbol, 
    call, 
    # Removes the connection resource from the environment after the call
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})


#' Construct the Call for Table Assignment
#'
#' This function assists in constructing the call that will be sent from the DataSHIELD client to the server
#' to perform the assignment of a table from an OMOP CDM database based on the parameters specified by the user.
#'
#' @param resource The resource symbol representing the connection to the OMOP CDM database.
#' @param table A string specifying the name of the table to be assigned.
#' @param conceptFilter A numeric vector specifying concept IDs to filter the table by.
#' @param columnFilter A string vector specifying column names to filter (select) in the table.
#' @param personFilter A string specifying the symbol in the environment of a table from which to obtain person IDs.
#' @param mergeColumn A string specifying the column name for merging operations with other tables.
#'                    Defaults to "person_id" if not specified.
#' @param dropNA A boolean indicating whether to drop empty columns. Defaults to FALSE.
#'
#' @return A string representing the call to be executed on the DataSHIELD server for table assignment.
#' 
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
