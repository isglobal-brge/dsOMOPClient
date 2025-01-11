#' @title Get Columns Catalog from OMOP CDM Database Table
#' @name OMOPCDMDatabase-columns
#'
#' @description Retrieves a comprehensive list of available columns for a specified table 
#' in an OMOP Common Data Model (CDM) database. This function helps explore the structure 
#' of individual tables by providing information about their column names and organization.
#'
#' @details The function performs the following steps:
#' 1. Assigns the database resource to the DataSHIELD environment
#' 2. Retrieves the column catalog using a server-side function
#' 3. Cleans up by removing the resource from the environment
#' 4. Handles any errors that may occur during the process
#'
#' The column catalog is retrieved in a privacy-preserving way through DataSHIELD's
#' aggregate functions.
#'
#' @param table A character string specifying the name of the table for which to retrieve 
#'              the columns catalog (e.g., "person", "condition_occurrence")
#' @param dropNA A logical indicating whether to exclude columns that contain only NA values 
#'              (default: FALSE). Setting this to TRUE can help identify meaningful columns.
#'
#' @return A character vector containing the names of all available columns in the specified 
#' table. Common columns across tables include:
#' * person_id - Unique identifier for each person
#' * visit_occurrence_id - Identifier for healthcare visits
#' * start_date/end_date - Temporal information
#' * concept_id - References to standardized concepts
#'
#' @examples
#' \dontrun{
#' # Create database connection
#' db <- ds.omop(connections, "omop_database")
#' 
#' # Get all columns from person table
#' person_columns <- db$columns("person")
#' 
#' # Get non-empty columns from measurement table
#' measurement_columns <- db$columns("measurement", dropNA = TRUE)
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase-tables}} for getting available tables
#' * \code{\link{OMOPCDMDatabase-get}} for retrieving table data
#'
OMOPCDMDatabase$set("public", "columns", function(table, dropNA = FALSE) {
  # Step 1: Assign the database resource to the DataSHIELD environment
  self$assignResource(self$resourceSymbol)
  
  # Step 2: Retrieve column catalog through DataSHIELD's aggregate function
  # Constructs and executes the server-side function call with provided parameters
  DSI::datashield.aggregate(
    self$connections,
    expr = paste0("getColumnCatalogDS(", self$resourceSymbol, 
                 ", table = '", table, 
                 "', dropNA = ", dropNA, ")"),
    # Step 3: Clean up by removing the resource, whether successful or not
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})
