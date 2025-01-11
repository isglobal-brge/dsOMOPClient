#' @title Check Database Connection
#' @name OMOPCDMDatabase-checkConnection
#'
#' @description Validates the connection between the OMOPCDMDatabase object and an OMOP Common 
#' Data Model (CDM) database. This function ensures that the database connection is active and 
#' properly configured before attempting any data operations.
#'
#' @details The function performs the following steps:
#' 1. Assigns the database resource to the DataSHIELD environment
#' 2. Executes a connection test through DataSHIELD's aggregate function
#' 3. Cleans up by removing the resource from the environment
#' 4. Handles any connection errors that may occur
#'
#' The connection check is performed in a privacy-preserving way through DataSHIELD's
#' aggregate functions.
#'
#' @return A logical value:
#' * TRUE - If the connection is valid and operational
#' * Error - If the connection is invalid or cannot be established
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Check if connection is valid
#' connection_status <- db$checkConnection()
#' 
#' # Print connection status
#' print(connection_status)
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-checkPrivacyControlLevel}} for privacy settings validation
#' * \code{\link{OMOPCDMDatabase-assignResource}} for resource management details
#'
OMOPCDMDatabase$set("public", "checkConnection", function() {
  # Step 1: Assign the database resource to the DataSHIELD environment
  self$assignResource(self$resourceSymbol)
  
  # Step 2: Execute connection test through DataSHIELD's aggregate function
  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("checkConnectionDS(", self$resourceSymbol, ")"),
    # Step 3: Clean up by removing the resource, whether successful or not
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})

#' @title Check Privacy Control Level
#' @name OMOPCDMDatabase-checkPrivacyControlLevel
#'
#' @description Validates the privacy control level settings on the DataSHIELD server to ensure 
#' they are compatible with dsOMOP operations. This function helps identify potential privacy 
#' configuration issues that might prevent proper functioning of the package.
#'
#' @details The function examines the server's privacy settings and generates warnings for any 
#' configurations that may be too restrictive for dsOMOP operations. This includes checking:
#' * Access control settings
#' * Data disclosure controls
#' * Aggregation limits
#' * Other privacy-related parameters
#'
#' @return A named list containing:
#' * Server names as list names
#' * Warning messages for each server where privacy settings may be too restrictive
#' * Empty list if all privacy settings are appropriately configured
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Check privacy control levels
#' privacy_warnings <- db$checkPrivacyControlLevel()
#' 
#' # Print any warnings
#' if(length(privacy_warnings) > 0) {
#'   print("Privacy control warnings found:")
#'   print(privacy_warnings)
#' } else {
#'   print("Privacy controls appropriately configured")
#' }
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-checkConnection}} for connection validation
#' * DataSHIELD privacy documentation for detailed privacy control information
#'
OMOPCDMDatabase$set("public", "checkPrivacyControlLevel", function() {
  # Execute privacy control check through DataSHIELD's aggregate function
  DSI::datashield.aggregate(
    self$connections,
    expr = "checkPrivacyControlLevelDS()"
  )
})
