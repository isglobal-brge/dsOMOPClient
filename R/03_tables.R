#' @title Get Tables Catalog from OMOP CDM Database
#' @name OMOPCDMDatabase-tables
#'
#' @description Retrieves a comprehensive list of available tables in an OMOP Common 
#' Data Model (CDM) database. This function provides an overview of the database 
#' structure by returning all accessible table names, which is useful for exploring 
#' the database schema and planning data retrieval operations.
#'
#' @details The function performs the following steps:
#' 1. Assigns the database resource to the DataSHIELD environment
#' 2. Retrieves the table catalog using a server-side function
#' 3. Cleans up by removing the resource from the environment
#' 4. Handles any errors that may occur during the process
#'
#' The table catalog is retrieved in a privacy-preserving way through DataSHIELD's
#' aggregate functions.
#'
#' @return A character vector containing the names of all available tables in the 
#' OMOP CDM database. Common tables include:
#' * person - Demographics and person information
#' * condition_occurrence - Clinical conditions and diagnoses
#' * measurement - Laboratory tests and clinical measurements
#' * observation - Clinical observations
#' * drug_exposure - Medication records
#' * procedure_occurrence - Medical procedures
#' * visit_occurrence - Healthcare visits
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Get list of available tables
#' available_tables <- db$tables()
#' 
#' # Print the tables
#' print(available_tables)
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-get}} for retrieving data from specific tables
#' * \code{\link{OMOPCDMDatabase-assignResource}} for resource management details
#'
OMOPCDMDatabase$set("public", "tables", function() {
  # Step 1: Assign the database resource to the DataSHIELD environment
  self$assignResource(self$resourceSymbol)
  
  # Step 2: Retrieve table catalog through DataSHIELD's aggregate function
  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getTableCatalogDS(", self$resourceSymbol, ")"),
    # Step 3: Clean up by removing the resource, whether successful or not
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
