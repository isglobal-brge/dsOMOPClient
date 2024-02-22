#' @title Check Database Connection
#' @name OMOPCDMDatabase-checkConnection
#'
#' @description This function checks if the OMOPCDMDatabase object points to a valid connection to an OMOP CDM database.
#'
#' @return A boolean value indicating whether the connection to the database is valid (TRUE). Otherwise, an error is raised.
#'
OMOPCDMDatabase$set("public", "checkConnection", function() {
  self$assignResource(self$resourceSymbol)

  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("checkConnectionDS(", self$resourceSymbol, ")"),
    # Removes the connection resource from the environment after the call
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})


#' @title Check Privacy Control Level
#' @name OMOPCDMDatabase-checkPrivacyControlLevel
#'
#' @description This function checks if the privacy control level settings of the server configuration
#' allow the use of dsOMOP operations.
#'
#' @return A list of warnings for each server where the privacy control level may not be permissive enough.
#' Each element of the list is named by the server and contains the warning message.
#'
OMOPCDMDatabase$set("public", "checkPrivacyControlLevel", function() {
  DSI::datashield.aggregate(
    self$connections,
    expr = "checkPrivacyControlLevelDS()"
  )
})
