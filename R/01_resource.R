#' @title Assign the OMOP CDM database resource to a DataSHIELD environment
#' @name OMOPCDMDatabase-assignResource
#'
#' @description This function assigns the resource of the OMOP CDM database connection
#' to the DataSHIELD environment, facilitating the connection to the database from the
#' server side. The resource assignment is necessary for subsequent database operations.
#'
#' @param symbol A character string representing the symbol to which the
#' resource will be assigned in the DataSHIELD environment.
#'
#' @return No return value, called for side effect of assigning the resource.
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Assign resource with custom symbol
#' db$assignResource("my_db_resource")
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-removeResource}} for removing the assigned resource
#' * \code{\link[DSI]{datashield.assign.resource}} for the underlying DataSHIELD function
#'
OMOPCDMDatabase$set("public", "assignResource", function(symbol) {
  # Assign the database resource to the specified symbol in the DataSHIELD environment
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
#' from the DataSHIELD environment. It is commonly used after completing database operations
#' to clean up the environment and free resources. The function silently handles any errors
#' or warnings that may occur during resource removal.
#'
#' @return No return value, called for side effect of removing the resource.
#'
#' @examples
#' \dontrun{
#' # Create and use database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # ... perform operations ...
#' 
#' # Clean up by removing resource
#' db$removeResource()
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-assignResource}} for assigning resources
#' * \code{\link[DSI]{datashield.rm}} for the underlying DataSHIELD function
#'
OMOPCDMDatabase$set("public", "removeResource", function() {
  # Attempt to remove the resource and suppress any errors/warnings
  tryCatch(
    {
      DSI::datashield.rm(self$connections, self$resourceSymbol)
    },
    # Hide DataSHIELD's error/warning messages as they may be ambiguous in this case
    error = function(error) {},
    warning = function(warning) {}
  )
})

#' @title Generate a Unique Resource Symbol for an OMOP CDM Database Resource
#' @name generateResourceSymbol
#'
#' @description 
#' This internal function generates a unique symbol name for a connection resource to an 
#' OMOP CDM database. The generated symbol includes a random alphanumeric string to prevent 
#' naming conflicts in the DataSHIELD environment. The symbol follows the format 
#' "dsO.XXXX" where XXXX is a random 4-character string.
#'
#' @param resource A character string representing the resource for which the symbol 
#' is generated. While currently unused, this parameter is maintained for potential 
#' future customization of symbol generation based on resource characteristics.
#'
#' @return A character string representing the unique symbol for the resource, in the 
#' format "dsO.XXXX" where XXXX is a random alphanumeric string.
#'
#' @details
#' The function:
#' 1. Generates a random 4-character string using numbers, lowercase and uppercase letters
#' 2. Prepends "dsO." to create the final symbol
#' 3. Returns the generated symbol
#'
#' The "dsO." prefix helps identify OMOP database symbols in the DataSHIELD environment.
#'
#' @keywords internal
#'
generateResourceSymbol <- function(resource) {
  # Generate random 4-character string from alphanumeric characters
  randomString <- paste0(sample(c(0:9, letters, LETTERS), 4, replace = TRUE), collapse = "")
  
  # Create final symbol with prefix
  resourceSymbol <- paste0("dsO.", randomString)
  
  return(resourceSymbol)
}
