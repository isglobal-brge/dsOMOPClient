#' @title Get Table from OMOP CDM Database
#' @name OMOPCDMDatabase-get
#'
#' @description Retrieves and assigns a specified table from an OMOP Common Data Model (CDM) database 
#' to the DataSHIELD environment. This function provides flexible filtering and transformation options 
#' to customize the retrieved data while maintaining privacy controls.
#'
#' @details
#' The function supports multiple ways to filter and transform the data:
#' * Concept ID filtering for tables with concept-based data
#' * Column selection to retrieve only needed variables
#' * Person-level filtering based on IDs from other tables
#' * Optional reshaping of longitudinal data to wide format
#' * Removal of empty columns
#'
#' The function handles the database resource management automatically by:
#' 1. Assigning the database resource
#' 2. Executing the table retrieval
#' 3. Cleaning up the resource afterward
#'
#' @param table A string specifying the name of the table to retrieve (e.g., "person", "condition_occurrence")
#' @param symbol A string specifying the symbol for the table assignment in the DataSHIELD environment.
#'              If NULL, defaults to the table name.
#' @param conceptFilter A numeric vector of concept IDs to filter the table by. Useful for tables like
#'                     condition_occurrence, measurement, or observation that are organized by concepts.
#' @param columnFilter A character vector specifying which columns to keep in the output. Reduces memory
#'                    usage by selecting only needed variables.
#' @param personFilter A string specifying a table symbol in the environment from which to obtain person IDs
#'                    for filtering. Uses the unique values of person_id from that table.
#' @param mergeColumn A string specifying the column name for merging operations (defaults to "person_id").
#'                   This column will be used when joining with other tables.
#' @param dropNA A logical indicating whether to remove columns containing only NA values (default: FALSE)
#' @param wideLongitudinal A logical indicating whether to reshape longitudinal data to wide format
#'                        with numerically suffixed columns (default: FALSE)
#'
#' @return No return value. The function assigns the retrieved table to the specified symbol
#'         in the DataSHIELD environment.
#'
#' @examples
#' \dontrun{
#' # Basic table retrieval
#' db$get("person", "person_table")
#'
#' # Get conditions for specific concepts with column filtering
#' db$get("condition_occurrence", 
#'        "diabetes_conditions",
#'        conceptFilter = c(201826, 201254),  # diabetes-related concepts
#'        columnFilter = c("person_id", "condition_start_date"))
#'
#' # Get measurements for patients in a cohort
#' db$get("measurement",
#'        "cohort_measurements", 
#'        personFilter = "my_cohort",
#'        wideLongitudinal = TRUE)
#'
#' # Get demographics with empty columns removed
#' db$get("person",
#'        "demographics",
#'        columnFilter = c("year_of_birth", "gender_concept_id", "race_concept_id"),
#'        dropNA = TRUE)
#' }
#'
#' @seealso 
#' * \code{\link{OMOPCDMDatabase}} for the main class documentation
#' * \code{\link{getTableCall}} for details on the underlying call construction
#'
OMOPCDMDatabase$set("public", "get", function(table,
                                             symbol = NULL,
                                             conceptFilter = NULL,
                                             columnFilter = NULL,
                                             personFilter = NULL,
                                             mergeColumn = NULL,
                                             dropNA = FALSE,
                                             wideLongitudinal = FALSE) {
  # Use table name as default symbol if none provided
  if (is.null(symbol)) {
    symbol <- table
  }

  # Step 1: Assign the database resource for the operation
  self$assignResource(self$resourceSymbol)
  
  # Step 2: Construct the table retrieval call
  call <- getTableCall(self$resourceSymbol, table, conceptFilter, columnFilter, 
                      personFilter, mergeColumn, dropNA, wideLongitudinal)
  
  # Step 3: Execute the call and ensure resource cleanup
  DSI::datashield.assign.expr(
    self$connections,
    symbol,
    call,
    # Clean up resource regardless of success or failure
    success = function(server, error) self$removeResource(),
    error = function(server, error) self$removeResource()
  )
})


#' @title Construct DataSHIELD Server Call for Table Retrieval
#' @name getTableCall
#'
#' @description Constructs the R expression that will be evaluated on the DataSHIELD server
#' to retrieve and process an OMOP CDM table. This internal function handles the complex
#' logic of building a valid call string incorporating all specified filtering and 
#' transformation options.
#'
#' @details
#' The function builds the call in stages:
#' 1. Starts with the base function call including resource and table name
#' 2. Adds concept filtering if specified
#' 3. Adds column filtering if specified
#' 4. Adds person-level filtering if specified
#' 5. Adds merge column specification if provided
#' 6. Adds optional flags for NA handling and longitudinal data reshaping
#'
#' Input validation is performed to ensure vector parameters are properly formatted.
#'
#' @param resource Character string identifying the database resource in the server environment
#' @param table Character string naming the OMOP CDM table to retrieve
#' @param conceptFilter Numeric vector of concept IDs for filtering
#' @param columnFilter Character vector of column names to select
#' @param personFilter Character string naming a table to use for person ID filtering
#' @param mergeColumn Character string specifying the merge key column
#' @param dropNA Logical flag for dropping empty columns
#' @param wideLongitudinal Logical flag for reshaping longitudinal data
#'
#' @return A character string containing the complete R expression to be evaluated on the server
#'
#' @keywords internal
#'
getTableCall <- function(resource,
                        table,
                        conceptFilter = NULL,
                        columnFilter = NULL,
                        personFilter = NULL,
                        mergeColumn = NULL,
                        dropNA = FALSE,
                        wideLongitudinal = FALSE) {
  # Start with base function call
  call <- paste0("getOMOPCDMTableDS(", resource, ", table = '", table, "'")

  # Add concept filtering if specified
  if (!is.null(conceptFilter)) {
    if (!is.vector(conceptFilter)) {
      stop("conceptFilter must be a vector of concept IDs.")
    }
    call <- paste0(call, ", conceptFilter = c(", paste(conceptFilter, collapse = ", "), ")")
  }

  # Add column filtering if specified
  if (!is.null(columnFilter)) {
    if (!is.vector(columnFilter)) {
      stop("columnFilter must be a vector of column names.")
    }
    call <- paste0(call, ", columnFilter = c('", paste(columnFilter, collapse = "', '"), "')")
  }

  # Add person filtering if specified
  if (!is.null(personFilter)) {
    call <- paste0(call, ", personFilter = ", personFilter)
  }

  # Add merge column if specified
  if (!is.null(mergeColumn)) {
    call <- paste0(call, ", mergeColumn = '", mergeColumn, "'")
  }

  # Add optional flags
  if (dropNA) {
    call <- paste0(call, ", dropNA = TRUE")
  }
  if (wideLongitudinal) {
    call <- paste0(call, ", wideLongitudinal = TRUE")
  }

  # Close the function call
  call <- paste0(call, ")")

  return(call)
}
