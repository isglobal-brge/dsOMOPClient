#' @title Assign the OMOP CDM database resource to a DataSHIELD environment
#' @name OMOPCDMDatabase-assignResource
#'
#' @description This function assigns the resource of the OMOP CDM database connection
#' to the DataSHIELD environment, facilitating the connection to the database from the
#' server side.
#'
#' @param symbol A character string representing the symbol to which the
#' resource will be assigned.
#'
OMOPCDMDatabase$set("public", "assignResource", function(symbol) {
  DSI::datashield.assign.resource(
    self$connections,
    symbol,
    self$resource
  )
})


#' @title Remove the OMOP CDM database resource from a DataSHIELD environment
#' @name OMOPCDMDatabase-removeResource
#'
#' @description This function removes the resource of the OMOP CDM database connection
#' from the DataSHIELD environment. It is commonly used after having performed operations
#' related to the connection with the database.
#'
OMOPCDMDatabase$set("public", "removeResource", function() {
  tryCatch(
    {
      DSI::datashield.rm(self$connections, self$resourceSymbol)
    },
    # Hide DataSHIELD's error/warning messages as they may be ambiguous in this case
    error = function(error) {},
    warning = function(warning) {}
  )
})


#' Generate a Unique Resource Symbol for an OMOP CDM Database Resource
#'
#' This function generates a unique symbol name for a connection resource to an OMOP CDM database.
#' The generated symbol will be stored in the OMOPCDMDatabase object. It includes a random string
#' to prevent overwriting other objects in the DataSHIELD environment where the user is working.
#'
#' @param resource A character string representing the resource for which the symbol is generated.
#'
#' @return A character string representing the unique symbol for the resource.
#'
generateResourceSymbol <- function(resource) {
  randomString <- paste0(sample(c(0:9, letters, LETTERS), 8, replace = TRUE), collapse = "")
  resourceSymbol <- paste("dsOMOP", randomString, sep = ".")
  return(resourceSymbol)
}
