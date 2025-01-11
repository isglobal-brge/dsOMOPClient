#' @title OMOP CDM Database Class
#' @name OMOPCDMDatabase
#' @description A class that provides an interface to interact with OMOP Common Data Model (CDM) databases 
#' through DataSHIELD. This class handles database connections and provides methods to access and query 
#' OMOP CDM data in a secure and privacy-preserving way.
#'
#' The class implements functionality to:
#' * Establish and validate database connections
#' * Check privacy control levels
#' * Access OMOP CDM tables and catalogs
#' 
#' @details The OMOP CDM standardizes the format and content of observational data, enabling 
#' large-scale analytics while ensuring consistent data representation across different 
#' healthcare databases.
#'
#' @section Privacy and Security:
#' The class implements privacy checks to ensure operations comply with DataSHIELD's 
#' privacy controls. Warnings are raised if privacy settings may restrict certain operations.
#'
#' @examples
#' \dontrun{
#' # Create a connection to a single server
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = "study1", url = "https://opal-demo.obiba.org",
#'               user = "dsuser", password = "P@ssw0rd")
#' connections <- DSI::datashield.login(builder)
#'
#' # Initialize OMOP database connection
#' db <- ds.omop(connections, resource = "mimiciv")
#' }
#'
OMOPCDMDatabase <- R6::R6Class(
  "OMOPCDMDatabase",
  public = list(
    #' @field connections DataSHIELD connection object
    connections = NULL,
    
    #' @field resource Database resource identifier(s)
    resource = NULL,
    
    #' @field resourceSymbol Internal symbol for resource reference
    resourceSymbol = NULL,

    #' @description Initialize a new OMOP CDM database connection
    #' 
    #' This constructor establishes and validates the connection to an OMOP CDM database
    #' through DataSHIELD. It performs connection validation and privacy control checks.
    #'
    #' @param connections A DataSHIELD connection object created via \code{DSI::datashield.login()}
    #' @param resource Either a single resource identifier (character string) or a named list of 
    #'   resource identifiers. When using multiple servers, the list names should match server names
    #'   in the connections object.
    #'
    #' @return A new \code{OMOPCDMDatabase} object
    #' 
    #' @examples
    #' \dontrun{
    #' # Single server connection
    #' db <- OMOPCDMDatabase$new(connections, "omop_db")
    #'
    #' # Multiple server connection
    #' db <- OMOPCDMDatabase$new(connections, 
    #'                           list(study1 = "omop_db1", 
    #'                                study2 = "omop_db2"))
    #' }
    #'
    initialize = function(connections, resource) {
      # Store connection parameters
      self$connections <- connections
      self$resource <- resource
      self$resourceSymbol <- generateResourceSymbol(resource)

      # Validate connections and handle potential errors
      tryCatch(
        {
          self$checkConnection()
        },
        error = function(error) {
          # Collect and format error messages from all servers
          errors <- DSI::datashield.errors()
          stop(paste(
            paste(crayon::red("\nSome connections could not be established!"), 
                  "The following errors were raised:"),
            paste(crayon::bgRed(crayon::white(paste0("[", names(errors), "]"))), 
                  errors, collapse = "\n"),
            crayon::red("Please check the connection details and try again."),
            sep = "\n"
          ))
        }
      )

      # Check privacy settings and issue warnings if needed
      privacyWarnings <- self$checkPrivacyControlLevel()
      if (length(privacyWarnings) > 0) {
        warning(paste(
          paste(crayon::yellow("\nThe privacy control level may not be permissive enough to allow some operations!"), 
                "The following warnings were raised:"),
          paste(crayon::bgYellow(crayon::black(paste0("[", names(privacyWarnings), "]"))), 
                privacyWarnings, collapse = "\n"),
          crayon::yellow("This should be configured by the server administrator. Please contact them for more information."),
          sep = "\n"
        ))
      }
    }
  )
)

#' Create a new OMOP CDM Database connection
#'
#' @description
#' Factory function that creates and returns a new instance of the OMOPCDMDatabase class.
#' This is the recommended way to create a new OMOP CDM database connection.
#'
#' @param connections A DataSHIELD connection object created via \code{DSI::datashield.login()}
#' @param resource Either a single resource identifier (character string) or a named list of 
#'   resource identifiers for multiple servers
#'
#' @return A new \code{OMOPCDMDatabase} object
#'
#' @examples
#' \dontrun{
#' # Set up connection parameters
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = "study1", url = "https://study1.org",
#'               user = "user1", password = "password1")
#' connections <- DSI::datashield.login(builder)
#'
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase}} for the full class documentation
#' * \code{\link[DSI]{datashield.login}} for creating DataSHIELD connections
#'
#' @export
#'
ds.omop <- function(connections, resource) {
  return(OMOPCDMDatabase$new(connections, resource))
}
