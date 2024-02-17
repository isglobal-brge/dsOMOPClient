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
    #' @param resource Either an identifier or a named list of identifiers for the specific resource(s) within the DataSHIELD 
    #' server(s). If a named list, the name of each resource identifier should correspond to the server name in the connections.
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
          stop()
        }
      }, error = function(error) {
        if (is.list(resource)) { # If resource is a list (multiple resource identifiers)
          stop("Unable to establish connections to all the OMOP CDM databases.")
        } else { # If resource is not a list (a single resource identifier)
          stop("Unable to establish a connection to the OMOP CDM database.")
        }
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
