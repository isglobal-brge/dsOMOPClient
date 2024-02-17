#' @title OMOP CDM Database class
#' @name OMOPCDMDatabase
#'
#' @description This class represents a connection to an OMOP CDM database, providing
#' various methods for the user to interact with the database. It allows for the retrieval 
#' of tables as well as fetching catalogs of available data within the database.
#' 
OMOPCDMDatabase <- R6::R6Class(
  "OMOPCDMDatabase",
  public = list(
    connections = NULL,
    resource = NULL,
    resourceSymbol = NULL,
    
    #' @title Constructor for OMOP CDM Database
    #' @name OMOPCDMDatabase-initialize
    #'
    #' @description This method initializes a new instance of the OMOPCDMDatabase class.
    #' It sets up the connection to the OMOP CDM database given the provided
    #' DataSHIELD connection object and the OMOP CDM database resource identifier.
    #'
    #' @param connections Connection object to the DataSHIELD server.
    #' @param resource Identifier for the specific resource within the DataSHIELD server.
    #' 
    #' @return A new instance of the OMOPCDMDatabase class.
    #' 
    initialize = function(connections, resource) {
      self$connections = connections
      self$resource = resource
      self$resourceSymbol = generateResourceSymbol(resource)
      
      # Checks if the connection to the OMOP CDM database can be established
      tryCatch({
        if(!self$checkConnection()) {
          stop("Connection failed.")
        }
      }, error = function(error) {
        stop(paste0("Unable to establish connection to the OMOP CDM database `", self$resource), "`.")
      })
    }
  )
)


#' Factory function for OMOP CDM Database
#'
#' This function creates a new instance of the OMOPCDMDatabase class, allowing
#' for interaction with an OMOP CDM database.
#'
#' @param connections Connection object to the DataSHIELD server.
#' @param resource Identifier for the specific resource within the DataSHIELD server.
#' 
#' @return A new instance of the OMOPCDMDatabase class.
#' 
#' @export
#' 
ds.omop <- function(connections, resource) {
  return(OMOPCDMDatabase$new(connections, resource))
}
