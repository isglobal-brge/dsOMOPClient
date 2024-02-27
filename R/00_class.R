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
      self$connections <- connections
      self$resource <- resource
      self$resourceSymbol <- generateResourceSymbol(resource)

      # Checks if the connections are valid
      tryCatch({
        self$checkConnection()
      }, error = function(error) {
        errors <- DSI::datashield.errors()
        stop(paste(
          paste(crayon::red("\nERROR: Some of the connections could not be established!"), "The following errors were raised:"),
          paste(crayon::bgRed(paste0("[", names(errors), "]")), errors, collapse = "\n"),
          "Please check the connection details and try again.",
          sep = "\n"
        ))
      })

      # Checks if the privacy control level is permissive enough
      privacyWarnings <- self$checkPrivacyControlLevel()
      if (length(privacyWarnings) > 0) {
        warning(paste(
          paste(crayon::yellow("\nWARNING: The privacy control level may not be permissive enough to allow some of the dsOMOP operations!"), "The following warnings were raised:"),
          paste(crayon::bgYellow(paste0("[", names(privacyWarnings), "]")), privacyWarnings, collapse = "\n"),
          "This is a setting that should be configured by the server administrator. Please contact them for more information.",
          sep = "\n"
        ))
      }
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
