#' @title Get Concepts Catalog from OMOP CDM Database Table
#' @name OMOPCDMDatabase-concepts
#'
#' @description Retrieves a comprehensive catalog of standardized concepts (concept IDs and 
#' their corresponding names) from a specified table in an OMOP Common Data Model (CDM) 
#' database. This function helps explore and understand the semantic meaning of coded data 
#' within OMOP CDM tables.
#'
#' @details The function performs the following steps:
#' 1. Assigns the database resource to the DataSHIELD environment
#' 2. Retrieves the concepts catalog using a server-side function
#' 3. Cleans up by removing the resource from the environment
#' 4. Handles any errors that may occur during the process
#'
#' The concepts catalog is retrieved in a privacy-preserving way through DataSHIELD's
#' aggregate functions.
#'
#' @param table A character string specifying the name of the table for which to retrieve 
#'              the concepts catalog (e.g., "condition_occurrence", "measurement")
#'
#' @return A data frame with two columns:
#' * concept_id: Numeric identifier for each standardized concept
#' * concept_name: Character string containing the human-readable name/description of the concept
#'
#' Common concept types include:
#' * Conditions/diagnoses (in condition_occurrence)
#' * Medications (in drug_exposure)
#' * Laboratory tests (in measurement)
#' * Clinical observations (in observation)
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Get concepts from condition_occurrence table
#' condition_concepts <- db$concepts("condition_occurrence")
#' 
#' # Get concepts from measurement table
#' measurement_concepts <- db$concepts("measurement")
#' 
#' # Print first few concepts
#' head(condition_concepts)
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-get}} for retrieving table data using concept IDs
#' * \code{\link{OMOPCDMDatabase-tables}} for listing available tables
#'
OMOPCDMDatabase$set("public", "concepts", function(table) {
  # Step 1: Assign the database resource to the DataSHIELD environment
  self$assignResource(self$resourceSymbol)
  
  # Step 2: Retrieve concepts catalog through DataSHIELD's aggregate function
  # Constructs and executes the server-side function call with the table parameter
  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getConceptCatalogDS(", self$resourceSymbol, ", table = '", table, "')"),
    # Step 3: Clean up by removing the resource, whether successful or not
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
